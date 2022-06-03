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
  filter(str_detect(word, '.com|.net|.edu|.gov|http', negate = TRUE)) |>
  filter(str_detect(word, '[0-9]', negate = TRUE)) |>
  # remove rare words
  filter(n > 1) |>
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
  left_join(tidy_tweets, by = '.id')


# fit a model
lasso_model <- logistic_reg(penalty = 0.01, mixture = 1) |>
  set_engine('glmnet') |>
  fit(formula = .source ~ .,
      data = tidy_tweets |>
        select(-.id, -.created))


tidy(lasso_model)


# cross-validation:

# first, assign each observation in train to a "fold"
folds <- vfold_cv(tidy_tweets |>
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







# Further Reading:
# http://varianceexplained.org/r/trump-tweets/