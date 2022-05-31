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
  mutate(sentiment_score = expert1 + expert2 + expert3) |>
  # recode tweet_id as a factor so we don't lose them when we count words
  mutate(tweet_id = factor(tweet_id))

# For those zero sentiment scores, was it mostly that everyone agreed
# on a sentiment score, or there was total disagreement?
tweets |>
  filter(sentiment_score == 0) |>
  filter(expert1 < 0 | expert2 < 0 | expert3 < 0)

# did we get better at coding over time?
tweets |>
  mutate(index = 1:nrow(tweets)) |>
  mutate(everyone_agreed = as.numeric(expert1 == expert2,
                                      expert2 == expert3)) |>
  mutate(early_tweet = as.numeric(index <= 100)) |>
  filter(!is.na(everyone_agreed)) |>
  group_by(early_tweet) |>
  summarize(pct_agreement = mean(everyone_agreed))


## Step 2: Tokenize and merge with a sentiment lexicon -----------

tidy_tweets <- tweets |>
  unnest_tokens(input = 'text',
                output = 'word')


# merge with sentiment lexicon
tidy_tweets <- tidy_tweets |>
  inner_join(get_sentiments('bing'), by = 'word') |>
  # take out the inappropriate words
  filter(!(word %in% c('trump', 'supreme', 'like')))

# to create a sentiment score, just count up positive and negative
# sentiments
tidy_tweets <- tidy_tweets |>
  group_by(tweet_id, .drop = FALSE) |>
  summarize(num_positive = sum(sentiment == 'positive'),
            num_negative = sum(sentiment == 'negative'),
            bing_score = (num_positive - num_negative) / (num_positive + num_negative)) |>
  mutate(bing_score = if_else(is.nan(bing_score), 0, bing_score))

# what's the distribution of sentiment?
ggplot(data = tidy_tweets,
       mapping = aes(x = bing_score)) +
  geom_histogram(color = 'black')

## Step 3: Validation, validation, validation -----------

# merge the scores back in with the original dataset
tweets <- tweets |>
  left_join(tidy_tweets, by = 'tweet_id')

# plot the two measures
ggplot(data = tweets,
       mapping = aes(x=sentiment_score,
                     y=bing_score)) +
  geom_jitter(alpha = 0.5) +
  labs(x='Hand-Coded Sentiment Score',
       y='Dictionary Score')

cor(tweets$sentiment_score, tweets$bing_score,
    use = 'pairwise.complete')

# accuracy measure
tweets |>
  filter(sentiment_score %in% c(3, -3)) |>
  count((sentiment_score == 3 & bing_score > 0) |
          sentiment_score == -3 & bing_score < 0) |>
  pivot_wider(names_from = `(sentiment_score == 3 & bing_score > 0) | ...`,
              values_from = 'n') |>
  mutate(pct_correct = `TRUE` / (`TRUE` + `FALSE`))



