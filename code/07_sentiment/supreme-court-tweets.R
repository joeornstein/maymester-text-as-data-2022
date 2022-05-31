#' ---
#'
#' title: Classify the sentiment of 945 tweets about the Supreme Court
#'
#' ---

library(tidyverse)
library(tidytext)

## Step 1: Load and tidy the tweets ---------------------------

tweets <- read_csv('data/tweets/supreme-court-tweets.csv')

# create a sentiment score equal to the sum of the expert codes
tweets <- tweets |>
  mutate(sentiment_score = expert1 + expert2 + expert3)

# For those zero sentiment scores, was it mostly that everyone agreed
# on a sentiment score, or there was total disagreement?
tweets |>
  filter(sentiment_score == 0) |>
  filter(expert1 < 0 | expert2 < 0 | expert3 < 0)
