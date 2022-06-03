#' ---
#'
#' Explore and model the Sentiment140 dataset
#'
#' ---

library(tidyverse)
library(tidytext)
library(tidymodels)
library(SnowballC)

d <- read_csv('data/raw/Sentiment140/training.1600000.processed.noemoticon.csv',
              col_names = c('target', 'ids', 'date', 'flag', 'user', 'text'))

d <- d |>
  mutate(.id = 1:nrow(d)) |>
  select(.id, .target = target, text) |>
  mutate(.target = factor(.target)) |>
  slice_sample(n = 1e5)

# pick which word stems to keep
stems_to_keep <- d |>
  unnest_tokens(input = 'text',
                output = 'word') |>
  anti_join(get_stopwords()) |>
  # remove numerals, URLs
  filter(str_detect(word, '.com|.net|.edu|.gov|http', negate = TRUE)) |>
  filter(str_detect(word, '[0-9]', negate = TRUE)) |>
  # convert to word stems
  mutate(word_stem = wordStem(word)) |>
  count(word_stem) |>
  # remove the extremely rare words (used less than 200 times in 1.6 million tweets)
  filter(n >= 20) |>
  pull(word_stem)

tidy_tweets <- d |>
  unnest_tokens(input = 'text',
                output = 'word') |>
  mutate(word_stem = wordStem(word)) |>
  filter(word_stem %in% stems_to_keep) |>
  filter(!(word_stem %in% c('', '__'))) |>
  # count up the words
  count(.id, word_stem) |>
  # get the term frequency
  bind_tf_idf(term = 'word_stem',
              document = '.id',
              n = 'n') |>
  # pivot so each term frequency is a column vector
  select(.id, word_stem, tf) |>
  pivot_wider(id_cols = '.id',
              names_from = 'word_stem',
              values_from = 'tf',
              values_fill = 0,
              names_repair = 'unique')

# put the labels back in
tidy_tweets <- d |>
  select(.id, .target) |>
  right_join(tidy_tweets, by = '.id')


# split the data into train and test sets
tweet_split <- initial_split(tidy_tweets,
                             prop = 0.8)

train <- training(tweet_split)
test <- testing(tweet_split)

## Step 2: Fit a model --------------------------------

model1 <- logistic_reg(penalty = 0.001, mixture = 1) |>
  fit(.target ~ .,
      data = train |>
        select(-.id))


