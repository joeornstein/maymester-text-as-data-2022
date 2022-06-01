#' ---
#'
#' Let's demonstrate regularized regression on the Federalist Paper
#' authorship prediction problem
#'
#' ---

library(tidyverse)
library(tidytext)
library(tidymodels)

# training set is all the undisputed Madison and Hamilton papers
train <- corpus::federalist |>
  filter(author != 'Jay') |>
  mutate(hamilton = as.numeric(author == 'Hamilton'))
table(train$hamilton, train$author)

# the test set consists of all our disputed papers
test <- corpus::federalist |>
  filter(is.na(author))

## Model 1: Just the tf of upon ------------------------

# tokenize the test and train sets
tf <- train |>
  mutate(name = factor(name)) |>
  # remove the salutation
  mutate(text = str_replace_all(text,
                                pattern = 'To the People of the State of New York:',
                                replacement = '')) |>
  # tokenize to words
  unnest_tokens(input = 'text',
                output = 'word') |>
  count(name, word) |>
  bind_tf_idf(term = 'word',
              document = 'name',
              n = 'n') |>
  filter(word == 'upon') |>
  select(name, upon_tf = tf)

train <- left_join(train, tf, by = 'name') |>
  # replace those NAs (no upons found) with zeros
  mutate(upon_tf = replace_na(upon_tf, 0))

ggplot(data = train,
       mapping = aes(x=upon_tf, y=hamilton)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'glm', method.args = list(family = "binomial"))

model1 <- logistic_reg()


tidy_test <- test |>
  # remove the salutation
  mutate(text = str_replace_all(text,
                                pattern = 'To the People of the State of New York:',
                                replacement = '')) |>
  # tokenize to words
  unnest_tokens(input = 'text',
                output = 'word')



