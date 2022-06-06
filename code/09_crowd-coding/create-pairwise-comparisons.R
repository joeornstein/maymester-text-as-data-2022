#' ---
#'
#' Create pairwise comparisons from Supreme Court tweets dataset
#'
#' ---


library(tidyverse)

# load the tweets
tweets <- read_csv('data/tweets/supreme-court-tweets.csv') |>
  mutate(id = 1:nrow(tweets))


# set a random seed and get 30 random tweets
set.seed(42)

random_set <- tweets |>
  slice_sample(n = 30) |>
  select(id, text)

# pair them up
all_pairs <- random_set$id |>
  # get all unique combinations
  combn(m = 2) |>
  # transpose
  t() |>
  # convert to tibble
  as_tibble() |>
  # set column names
  set_names(c('tweet1', 'tweet2')) |>
  # merge the text
  left_join(
    random_set |>
      select(tweet1 = id,
             text1 = text)
  ) |>
  left_join(
    random_set |>
      select(tweet2 = id,
             text2 = text)
  )

# randomize the row order so it's not boring
all_pairs <- all_pairs |>
  mutate(x = rnorm(n = nrow(all_pairs))) |>
  arrange(x) |>
  select(-x)

write_csv(all_pairs, file = 'data/tweets/scotus_tweets_all_pairs.csv')













