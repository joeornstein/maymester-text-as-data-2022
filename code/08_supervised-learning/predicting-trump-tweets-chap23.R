# predicting Trump vs. staff tweets (chapter 23)


library(tidyverse)
library(tidytext)
library(tidymodels)


# training set from David Robinson
load(url("http://varianceexplained.org/files/trump_tweets_df.rda"))


# clean up
tweets <- trump_tweets_df |>
  select(.id = id,
         .source = statusSource,
         .text = text,
         .created = created) |>
  extract(.source, '.source', "Twitter for (.*?)<") |>
  filter(.source %in% c('iPhone', 'Android')) |>
  mutate(.source = factor(.source))

# (notice that I'm putting a dot in front of all these
# column names, on the off chance that words like "source"
# or "id" appear in the corpus)

library(lubridate)

tweets |>
  count(.source, hour = hour(with_tz(.created, "EST"))) |>
  mutate(percent = n / sum(n)) |>
  ggplot(aes(hour, percent, color = .source)) +
  geom_line() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "") +
  theme_minimal()



# pick the words to keep as predictors
words_to_keep <- tweets |>
  unnest_tokens(input = '.text',
                output = 'word') |>
  count(word) |>
  # remove numerals, URLs
  filter(str_detect(word, '.co|.com|.net|.edu|.gov|http', negate = TRUE)) |>
  filter(str_detect(word, '[0-9]', negate = TRUE)) |>
  # remove rare words
  filter(n > 2) |>
  pull(word)

# tokenize
tidy_tweets <- tweets |>
  unnest_tokens(input = '.text',
                output = 'word') |>
  filter(word %in% words_to_keep) |>
  count(.id, word) |>
  bind_tf_idf(term = 'word',
              document = '.id',
              n = 'n') |>
  select(.id, word, tf) |>
  pivot_wider(id_cols = '.id',
              names_from = 'word',
              values_from = 'tf',
              values_fill = 0)

tidy_tweets <- tweets |>
  select(.id, .source, .created) |>
  right_join(tidy_tweets, by = '.id')


# split into train and test sets
tweet_split <- initial_split(tidy_tweets,
                             prop = 0.8)

train <- training(tweet_split)
test <- testing(tweet_split)

# fit three logistic regression models:
# underfit, overfit, and regularized

# underfit model
model1 <- logistic_reg() |>
  fit(formula = .source ~ crooked + dumb + emails +
        crowds + hillary + winning + weak,
      data = train)

tidy(model1)

# in-sample fit
train |>
  bind_cols(predict(model1, train)) |>
  accuracy(truth = .source, estimate = .pred_class)


# out-of-sample fit
test |>
  bind_cols(predict(model1, test)) |>
  accuracy(truth = .source, estimate = .pred_class)

# overfit
model2 <- logistic_reg() |>
  fit(formula = .source ~ .,
      data = train |>
        select(-.id, -.created))

tidy(model2)

# in-sample fit
train |>
  bind_cols(predict(model2, train)) |>
  accuracy(truth = .source, estimate = .pred_class)

# out-of-sample fit
test |>
  bind_cols(predict(model2, test)) |>
  accuracy(truth = .source, estimate = .pred_class)

# does no better than the underfit model

# fit a regularized model (LASSO)
model3 <- logistic_reg(penalty = 0.01, mixture = 1) |>
  set_engine('glmnet') |>
  fit(formula = .source ~ .,
      data = train |>
        select(-.id, -.created))


tidy(model3)


# in-sample fit
train |>
  bind_cols(predict(model3, train)) |>
  accuracy(truth = .source, estimate = .pred_class)

# out-of-sample fit
test |>
  bind_cols(predict(model3, test)) |>
  accuracy(truth = .source, estimate = .pred_class)


# cross-validation:

# first, assign each observation in train to a "fold"
folds <- vfold_cv(train |>
                    select(-.id, -.created),
                  v = 10)
folds

# now, fit the model on each training fold, assessing performance
# on the out-of-sample fold
lasso_specification <-
  logistic_reg(penalty = 0.01, mixture = 1) |>
  set_engine('glmnet')

lasso_cv <- workflow() |>
  add_model(lasso_specification) |>
  add_formula(.source ~ .) |>
  fit_resamples(folds)

collect_metrics(lasso_cv)

rf_specification <-
  rand_forest(mode = 'classification', mtry = 5, min_n = 10, trees = 1001) |>
  set_engine('ranger')

rf_cv <- workflow() |>
  add_model(rf_specification) |>
  add_formula(.source ~ .) |>
  fit_resamples(folds)

collect_metrics(rf_cv)

## Tune the hyperparameters through cross validation -----

rf_specification <- rand_forest(mode = 'classification',
                                mtry = tune(),
                                trees = 1001,
                                min_n = 10)


rf_grid <- grid_regular(mtry(range = c(5, 100)),
                        levels = 5)
rf_grid

rf_workflow <- workflow() |>
  add_model(rf_specification) |>
  add_formula(.source ~ .)

rf_tune <- tune_grid(
  rf_workflow,
  folds,
  grid = rf_grid
)

collect_metrics(rf_tune)

autoplot(rf_tune) +
  labs(
    title = "Random forest performance across regularization penalties",
  )










## tesing naive Bayes -----------------

library(naivebayes)
nb <- naive_bayes(
  formula = .source ~ .,
  data = train |>
    select(-.id, -.created),
  Laplace = 0.1)

summary(nb)

# in-sample fit
train |>
  bind_cols(.pred_class = predict(nb, train)) |>
  accuracy(truth = .source, estimate = .pred_class)

# out-of-sample fit
test |>
  bind_cols(.pred_class = predict(nb, test)) |>
  accuracy(truth = .source, estimate = .pred_class)


# Further Reading:
# http://varianceexplained.org/r/trump-tweets/