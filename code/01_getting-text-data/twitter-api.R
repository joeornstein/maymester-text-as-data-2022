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
  app = "",
  consumer_key = "",
  consumer_secret = "",
  access_token = "",
  access_secret = ""
)

## Step 2: Download some tweets ----------------------

tweets <- search_tweets(
  "", n = 200, include_rts = FALSE
)

# save your tweets if you want
# save(tweets, file = 'data/raw/tweets.RData')