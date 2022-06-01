#' ---
#'
#' Train a supervised learning model on the Federalist Paper
#' authorship problem
#'
#' ---

library(tidyverse)
library(tidytext)
library(tidymodels)

## Step 1: Load the Federalist papers -----------------

fed <- corpus::federalist |>
  filter(author %in% c('Madison', 'Hamilton')) |>
  mutate(author = factor(author))


# split the known texts into train and test sets
set.seed(42)

# training set is a random 80% where we know the authorship
train <- fed |>
  slice_sample(prop = 0.8)

# test set is the rest of the labeled data
test <- fed |>
  anti_join(train)

# disputed set
disputed <- corpus::federalist |>
  filter(is.na(author))

## Step 2: Define the features of the data ---------------------

# get the frequency of the word upon
train <- train |>
  # remove the preamble
  mutate(text = str_replace_all(text,
                                'To the People of the State of New York',
                                '')) |>
  # tokenize to the word level
  unnest_tokens(input = 'text',
                output = 'word') |>
  # count up the number of each word
  count(name, author, word) |>
  # compute the tf
  bind_tf_idf(term = 'word',
              document = 'name',
              n = 'n') |>
  # just keep the word "upon"
  filter(word %in% c('the', 'upon', 'whilst')) |>
  select(name, author, word, tf) |>
  pivot_wider(names_from = 'word',
              values_from = 'tf',
              values_fill = 0)


# how well does this variable predict authorship in the training set?
ggplot(data = train,
       mapping = aes(x=upon, y=as.numeric(author == 'Hamilton'))) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)


# fit a logistic regression
model1 <- logistic_reg() |>
  fit(formula = hamilton ~ upon_tf,
      data = train |>
        mutate(hamilton = factor(hamilton)))

# in-sample fit
train <- train |>
  mutate(predicted_value = predict(model1, train))

## Step 3: Take the model we trained and see how good a job it does on the test set --------------------

# get the frequency of the word upon
test <- test |>
  # recode the paper names as factor (so they don't get dropped later)
  mutate(name = factor(name)) |>
  # remove the preamble
  mutate(text = str_replace_all(text,
                                'To the People of the State of New York',
                                '')) |>
  # tokenize to the word level
  unnest_tokens(input = 'text',
                output = 'word') |>
  # count up the number of each word
  count(name, word) |>
  # compute the tf
  bind_tf_idf(term = 'word',
              document = 'name',
              n = 'n') |>
  # just keep the word "upon"
  filter(word == 'upon') |>
  select(name, upon_tf = tf) |>
  # merge that back with the original dataset
  right_join(test, by = 'name') |>
  # if there were any papers with *no* upons, replace that missing value with 0
  mutate(upon_tf = replace_na(upon_tf, 0))

test <- test |>
  mutate(predicted_value = predict(model1, test))











