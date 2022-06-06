#' ---
#'
#' Take the pairwise ratings we produced and convert to a
#' continuous measure of negativity
#'
#' ---

library(tidyverse)

d <- read_csv('data/tweets/Tweets Crowd Coding - scotus_tweets_all_pairs.csv')

ids <- c(d$tweet1, d$tweet2) |> unique()

d <- d |>
  filter(!is.na(most_negative)) |>
  mutate(tweet1 = factor(tweet1, levels = ids),
         tweet2 = factor(tweet2, levels = ids))

library(BradleyTerry2)

d <- d |>
  mutate(win1 = if_else(most_negative == 1, 1, 0),
         win2 = if_else(most_negative == 2, 1, 0))

bt_model <- BTm(outcome = cbind(win1, win2),
                player1 = tweet1,
                player2 = tweet2,
                formula = ~tweet,
                id = 'tweet',
                data = d)

bt_model |>
  tidy() |>
  arrange(estimate)
