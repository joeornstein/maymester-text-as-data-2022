#' ---
#' Pull tweets from the Twitter API
#' and conduct a sentiment analysis
#'
#' author: Joe Ornstein
#' date: 2022-05-19
#' ---

library(tidyverse)
library(tidytext)
library(rtweet)

# Step 1: Create a token -------------------------

# this is where you input your API keys
# (i.e. giant password strings)
# from the Twitter developer dashboard to prove to Twitter
# that you have permission to be snooping around

create_token(
  app = "Maymester",
  consumer_key = read_file('data/twitter-keys/api-key.txt'),
  consumer_secret = read_file('data/twitter-keys/api-key-secret.txt'),
  access_token = read_file('data/twitter-keys/access-token.txt'),
  access_secret = read_file('data/twitter-keys/access-token-secret.txt')
)

## Step 2: Download some tweets ----------------------

tweets <- search_tweets(
  "inflation", n = 200, include_rts = FALSE
)

# save your tweets if you want
save(tweets, file = 'data/raw/tweets.RData')


## Step 3: Sentiment Analysis --------------------------

tokenized_tweets <- tweets |>
  # create a unique identifier
  mutate(ID = 1:nrow(tweets)) |>
  # tokenize
  unnest_tokens(input = 'text',
                output = 'word')

sentiment_scores <- tokenized_tweets |>
  # join with the sentiment lexicon
  inner_join(get_sentiments('bing')) |>
  # count positive and negative words
  group_by(ID) |>
  summarize(positive_words = sum(sentiment == 'positive'),
            negative_words = sum(sentiment == 'negative'),
            tweet_sentiment = (positive_words - negative_words) / (positive_words + negative_words))

# merge sentiment scores back with the original tweets
tweets <- tweets |>
  mutate(ID = 1:nrow(tweets)) |>
  select(ID, text) |>
  left_join(sentiment_scores, by = 'ID')



