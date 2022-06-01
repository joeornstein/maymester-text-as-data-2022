#' ---
#'
#' Explore and model the Sentiment140 dataset
#'
#' ---

library(tidyverse)
library(tidytext)

d <- read_csv('data/raw/Sentiment140/training.1600000.processed.noemoticon.csv',
              col_names = c('target', 'ids', 'date', 'flag', 'user', 'text')) |>
  mutate(id = 1:nrow(d)) |>
  select(id, target, text)

tidy_tweets <- d |>
  # remnove this unruly character
  mutate(text = str_replace_all(text, '\\p{WHITE_SPACE}', '')) |>
  # nice wrapper function for working with tweets, stripping URLs and preserving handles and hashtags
  unnest_tweets(input = 'text',
                output = 'word',
                strip_url = TRUE) |>
  anti_join(get_stopwords())
  # remove numerals, URLs

