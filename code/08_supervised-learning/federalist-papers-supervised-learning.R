#' ---
#'
#' Train a supervised learning model on the Federalist Paper
#' authorship problem
#'
#' ---

library(tidyverse)
library(tidytext)
library(tidymodels)

## Step 1: Load the Federalist papers and record the term frequency for a bunnch of words -----------------

words_to_keep <- c('the', 'upon', 'whilst') |>
  union(get_stopwords()$word)

tidy_federalist <- corpus::federalist |>
  # remove the preamble
  mutate(text = str_replace_all(text,
                                'To the People of the State of New York',
                                '')) |>
  # tokenize to the word level
  unnest_tokens(input = 'text',
                output = 'word') |>
  # just keep a subset of words
  filter(word %in% words_to_keep) |>
  mutate(word = factor(word)) |>
  # count up the number of each word
  count(name, author, word,
        .drop = FALSE) |>
  # compute the tf
  bind_tf_idf(term = 'word',
              document = 'name',
              n = 'n') |>
  select(name, author, word, tf) |>
  pivot_wider(names_from = 'word',
              values_from = 'tf',
              values_fill = 0)

# split the known texts into train and test sets
set.seed(42)

# training set is a random 80% where we know the authorship
train <- tidy_federalist |>
  filter(author %in% c('Hamilton', 'Madison')) |>
  # tidymodels wants the label to be a factor
  mutate(author = factor(author)) |>
  slice_sample(prop = 0.8)

# test set is the rest of the labeled data
test <- tidy_federalist |>
  filter(author %in% c('Hamilton', 'Madison')) |>
  mutate(author = factor(author)) |>
  anti_join(train)

# disputed set
disputed <- tidy_federalist |>
  filter(is.na(author))


## Step 2: Fit a one-variable logistic regression ---------------

# how well does this variable predict authorship in the training set?
ggplot(data = train,
       mapping = aes(x=upon, y=as.numeric(author == 'Hamilton'))) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)


# fit a logistic regression
model1 <- logistic_reg() |>
  fit(formula = author ~ upon,
      data = train)

tidy(model1)

# in-sample fit
train |>
  select(name, author) |>
  bind_cols(predict(model1, train)) |>
  accuracy(truth = author, estimate = .pred_class)

## Step 3: Take the model we trained and see how good a job it does on the test set --------------------

# out-of-sample fit
test |>
  select(name, upon, author) |>
  bind_cols(predict(model1, test)) |>
  arrange(-upon) |>
  accuracy(truth = author, estimate = .pred_class)

## Step 3: Let's try it again, but with some more predictors ---------

model2 <- logistic_reg() |>
  fit(formula = author ~ upon + the + whilst,
      data = train)

tidy(model2)

# in-sample fit
train |>
  select(name, author) |>
  bind_cols(predict(model2, train)) |>
  accuracy(truth = author, estimate = .pred_class)

# out-of-sample fit
test |>
  select(name, author) |>
  bind_cols(predict(model2, test)) |>
  accuracy(truth = author, estimate = .pred_class)

## Step 4: Try it with ALL THE WORDS ----------------------

# remove the names of the papers
# (if we tell the model the paper names, then it will
# *definitely* overfit)
train <- train |>
  select(-name)

# fit a logistic regression
model3 <- logistic_reg() |>
  fit(formula = author ~ .,
      data = train)

tidy(model3)


# in-sample fit
train |>
  select(author) |>
  bind_cols(predict(model3, train)) |>
  accuracy(truth = author, estimate = .pred_class)

# out-of-sample fit
test |>
  select(name, author) |>
  bind_cols(predict(model3, test)) |>
  accuracy(truth = author, estimate = .pred_class)

# this model is *way* overfit

disputed <- disputed |>
  mutate(predicted_value_1 = predict(model1, disputed)$.pred_class,
         predicted_value_2 = predict(model2, disputed)$.pred_class,
         predicted_value_3 = predict(model3, disputed)$.pred_class,
  )

table(disputed$predicted_value_1)
table(disputed$predicted_value_2)
table(disputed$predicted_value_3)

## Step 5: Regularization --------------------------

# Supervised machine learning, in a nutshell, is about trying to hit that
# sweet spot, a model that's complex enough to get low error in the training set
# but not so complex that it overfits to the training set and does terrible on the test set.

# The way we automate that process is *regularization*, adding a term to the model's
# objective function that keeps it from getting too complex.


# LASSO is a regularized linear model
model4 <- logistic_reg(penalty = 0.01, mixture = 1) |>
  set_engine('glmnet') |>
  fit(formula = author ~ .,
      data = train)

tidy(model4)

# in-sample fit
train |>
  select(author) |>
  bind_cols(predict(model4, train)) |>
  accuracy(truth = author, estimate = .pred_class)

# out-of-sample fit
test |>
  select(name, author) |>
  bind_cols(predict(model4, test)) |>
  accuracy(truth = author, estimate = .pred_class)
