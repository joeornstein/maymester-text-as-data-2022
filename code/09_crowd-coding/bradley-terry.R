#' ---
#'
#' Take the pairwise ratings we produced and convert to a
#' continuous measure of negativity
#'
#' ---

library(tidyverse)
library(broom)

d <- read_csv('data/tweets/Tweets Crowd Coding - scotus_tweets_all_pairs.csv')

ids <- c(d$tweet1, d$tweet2) |> unique()

tweets <- read_csv('data/tweets/supreme-court-tweets.csv') |>
  mutate(tweet_id = 1:n()) |>
  filter(tweet_id %in% ids) |>
  mutate(tweet_id = factor(tweet_id))


## Simple average ------------------------

# One measure is the just percent of times a tweet was coded as the
# "most negative". More negative tweets will be coded as more negative
# more often.

tweets <- d |>
  # identify which tweet won the contest
  mutate(winner = if_else(most_negative == 1,
                          tweet1,
                          tweet2)) |>
  # recode as a factor so we don't drop any levels when we count
  mutate(winner = factor(winner, levels = ids)) |>
  # count up how many times each tweet won the "Most Negative" contest
  count(winner, .drop = FALSE) |>
  # then their win rate is the number of wins divided by 29
  mutate(avg_negativity = n / (nrow(tweets) - 1)) |>
  select(tweet_id = winner, avg_negativity) |>
  # join with the tweets dataframe
  right_join(tweets, by = 'tweet_id')


## Bradley Terry approach ----------------------

# Another measure: assume that each tweet has some latent
# negativity score (alpha), and the odds that tweet i
# gets labeled as more negative than tweet j is alpha_i / alpha_j

# find the set of alpha values that maximizes the likelihood of the
# pairwise ratings we observed. This is the Bradley Terry estimator

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

# get the coefficients
bt_model |>
  tidy() |>
  arrange(estimate)
