#' ---
#'
#' Floor speeches - predict whether the speech was made by a Republican
#' or Democrat
#'
#' ---

library(tidyverse)
library(tidytext)
library(tidymodels)
library(SnowballC)

load('data/congressional-floor-speeches/wilkerson-casas-2017.RData')

## Step 1: Tidy the data and compute the predictors ------------

# find the word stems that you want to include in the model
stems_to_keep <- d |>
  mutate(speech = str_replace_all(speech,
                                  pattern = 'Mr. Speaker, I rise today ',
                                  replacement = '')) |>
  unnest_tokens(input = 'speech',
                output = 'word') |>
  # remove the stop words and numerals
  anti_join(get_stopwords()) |>
  filter(str_detect(word, '[0-9]', negate = TRUE)) |>
  # convert to word stems
  mutate(word_stem = wordStem(word)) |>
  count(word_stem) |>
  # remove the very rarest words (20 uses, for the sake of my RAM)
  filter(n > 20) |>
  pull(word_stem)

tidy_speeches <- d |>
  # create a unique ID for each speech
  mutate(id = 1:nrow(d)) |>
  # remove the preamble
  mutate(speech = str_replace_all(speech,
                                  pattern = 'Mr. Speaker, I rise today ',
                                  replacement = '')) |>
  unnest_tokens(input = 'speech',
                output = 'word') |>
  # just keep the words stems we identified before
  mutate(word_stem = wordStem(word)) |>
  filter(word_stem %in% stems_to_keep) |>
  filter(word_stem != '') |>
  # count up the words
  count(id, word_stem) |>
  # compute term frequency
  bind_tf_idf(document = 'id',
              term = 'word_stem',
              n = 'n') |>
  # pivot so each term frequency is a column vector
  select(id, word_stem, tf) |>
  pivot_wider(id_cols = 'id',
              names_from = 'word_stem',
              values_from = 'tf',
              values_fill = 0)

# add the party labels back in
tidy_speeches <- d |>
  mutate(id = 1:nrow(d)) |>
  select(id, party) |>
  left_join(tidy_speeches, by = 'id') |>
  mutate(party = factor(party))


# split into training and test sets
speech_split <- initial_split(data = tidy_speeches,
                              prop = 0.8)

train <- training(speech_split)
test <- testing(speech_split)


## Step 2: Fit a LASSO model ---------------------------

model1 <- logistic_reg(penalty = 0.01, mixture = 1) |>
  set_engine('glmnet') |>
  fit(formula = party ~ .,
      data = train |>
        select(-id) )

# look at the coefficients
tidy(model1)

# visualize the coefficients
model1 |>
  tidy() |>
  filter(abs(estimate) > 50) |>
  ggplot(mapping = aes(x=estimate, y=reorder(term, -estimate))) +
  geom_col() +
  labs(x = 'Coefficient Estimate',
       caption = 'Positive is More Republican',
       y = 'Word Stem')


## Step 3: Check out-of-sample prediction ----------------

# does it do a good job at prediction though?

# in-sample fit
train |>
  select(party) |>
  bind_cols(predict(model1, train)) |>
  accuracy(truth = party, estimate = .pred_class)

# out-of-sample fit
test |>
  select(party) |>
  bind_cols(predict(model1, test)) |>
  accuracy(truth = party, estimate = .pred_class)


## Alternative Step 3: V-fold Cross-validation ---------------------------

# one way to not be fooled by the in-sample accuracy of your model
# is through *cross-validation*, where we repeatedly sample observations
# to be in the test set until each one has been in the test set once.
# then we assess the accuracy of predictions on those held out observations,
# to get an *in-sample* measure of *out-of-sample* fit.
# ...it's weird.

# first, assign each observation in train to a "fold"
folds <- vfold_cv(train |>
                    select(-id),
                  v = 10)
folds

# now, fit the model on each training fold, assessing performance
# on the out-of-sample fold
model_specification <-
  logistic_reg(penalty = 0.01, mixture = 1) |>
  set_engine('glmnet')

cv_fit <- workflow() |>
  add_model(model_specification) |>
  add_formula(party ~ .) |>
  fit_resamples(folds)

collect_metrics(cv_fit)

## Step 4: Tuning the hyperparameters ----------------------

# in each of the LASSO models we fit above, we sort of arbitrarily set the penalty
# parameter to 0.01. Why 0.01? I dunno. It seemed to do good.
# But we can do better than that, by *tuning* that penalty parameter to optimize
# cross-validation accuracy.


# first, create the model specification, with tune() as a placeholder for the
# penalty parameter
model_specification <-
  logistic_reg(penalty = tune(), mixture = 1) |>
  set_engine('glmnet')

# next, create a set of penalty parameters that we want to try.
# fortunately, tidymodels has some convenient functions here,
# like penalty(), which gives a range of sensible parameters to try
penalty_grid <- grid_regular(penalty(), levels = 30)
penalty_grid

# NOTE: This is what that line of code is doing above. Making a tibble with a range of sensible values to try:
# penalty_grid <- tibble(
#   penalty = 10^seq(-10,0,length.out = 30)
# )
# penalty_grid

# now for the heart of the tuning procedure, tune_grid()
model_workflow <- workflow() |>
  add_model(model_specification) |>
  add_formula(party ~ .)

tune_rs <- tune_grid(
  model_workflow,
  folds,
  grid = penalty_grid
)

collect_metrics(tune_rs)

autoplot(tune_rs) +
  labs(
    title = "Lasso model performance across regularization penalties",
    subtitle = "Performance metrics can be used to identity the best penalty"
  )

# looks like 0.01 was pretty good, but something smaller would be even better!
collect_metrics(tune_rs) |>
  filter(.metric == 'accuracy') |>
  arrange(-mean)

best_penalty <- tune_rs |>
  select_best(metric = 'accuracy') |>
  pull(penalty)

# let's fit that model
model2 <- logistic_reg(penalty = best_penalty, mixture = 1) |>
  set_engine('glmnet') |>
  fit(formula = party ~ .,
      data = train |>
        select(-id) )

# look at the coefficients
tidy(model2)

model2 |> tidy() |> filter(estimate != 0) |> nrow()

# visualize the coefficients
model2 |>
  tidy() |>
  filter(abs(estimate) > 100) |>
  ggplot(mapping = aes(x=estimate, y=reorder(term, -estimate))) +
  geom_col() +
  labs(x = 'Coefficient Estimate',
       caption = 'Positive is More Republican',
       y = 'Word Stem')

# in-sample fit
train |>
  select(party) |>
  bind_cols(predict(model2, train)) |>
  accuracy(truth = party, estimate = .pred_class)

# out-of-sample fit
test |>
  select(party) |>
  bind_cols(predict(model2, test)) |>
  accuracy(truth = party, estimate = .pred_class)


## Try a random forest ------------------------

rf_model <- rand_forest(mode = 'classification',
                        mtry = 3,
                        trees = 1001,
                        min_n = 20)

model3 <- rf_model |>
  fit(formula = party ~ .,
      data = train |>
        select(-id))

# in-sample fit
train |>
  select(party) |>
  bind_cols(predict(model3, train)) |>
  accuracy(truth = party, estimate = .pred_class)

# out-of-sample fit
test |>
  select(party) |>
  bind_cols(predict(model3, test)) |>
  accuracy(truth = party, estimate = .pred_class)

# not so great. Let's tune the hyperparameters
rf_specification <- rand_forest(mode = 'classification',
                                mtry = tune(),
                                trees = 1001,
                                min_n = tune())


rf_grid <- grid_regular(mtry(range = c(1, 5)),
                        min_n(range = c(2, 40)),
                        levels = 5)
rf_grid

rf_workflow <- workflow() |>
  add_model(rf_specification) |>
  add_formula(party ~ .)

rf_tune <- tune_grid(
  rf_workflow,
  folds,
  grid = rf_grid
)

collect_metrics(rf_tune)





