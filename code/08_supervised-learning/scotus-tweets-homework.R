#' ---
#'
#' Predict sentiment of the SCOTUS tweets from a model trained on Sentiment140
#'
#' ---

library(tidyverse)
library(tidytext)
library(tidymodels)
library(SnowballC)


# load and tidy the Sentiment 140 dataset
d <- read_csv('data/raw/Sentiment140/training.1600000.processed.noemoticon.csv',
              col_names = c('target', 'ids', 'date', 'flag', 'user', 'text'))

d <- d |>
  mutate(.id = 1:nrow(d)) |>
  select(.id, .target = target, text) |>
  mutate(.target = factor(.target)) |>
  slice_sample(n = 1e5)

# load and tidy the SCOTUS tweets
scotus_tweets <- read_csv('data/tweets/supreme-court-tweets.csv')

# create a sentiment score equal to the sum of the expert codes
scotus_tweets <- scotus_tweets |>
  mutate(.sentiment_score = expert1 + expert2 + expert3) |>
  # recode tweet_id as a factor so we don't lose them when we count words
  mutate(.id = factor(tweet_id)) |>
  select(.id, .sentiment_score, text)

# pick which word stems to keep
stems_to_keep <- scotus_tweets |>
  unnest_tokens(input = 'text',
                output = 'word') |>
  anti_join(get_stopwords()) |>
  # remove numerals, URLs
  filter(str_detect(word, '.com|.net|.edu|.gov|http', negate = TRUE)) |>
  filter(str_detect(word, '[0-9]', negate = TRUE)) |>
  # convert to word stems
  mutate(word_stem = wordStem(word)) |>
  count(word_stem) |>
  # remove the extremely rare words
  filter(n >= 2) |>
  filter(word_stem != '') |>
  pull(word_stem)


# tokenize and pivot
tidy_scotus_tweets <- scotus_tweets |>
  unnest_tokens(input = 'text',
                output = 'word') |>
  mutate(word_stem = wordStem(word)) |>
  filter(word_stem %in% stems_to_keep) |>
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

# merge the sentiment scores back in
scotus_tweets <- scotus_tweets |>
  select(.id, .sentiment_score) |>
  left_join(tidy_scotus_tweets, by = '.id')


# preprocess sentiment140 like we preprocessed the SCOTUS tweets
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

# NOTE: I ran this over the weekend, and the final model object is over 2GB.
model1 <- logistic_reg(penalty = 0.005, mixture = 1) |>
  fit(.target ~ .,
      data = train |>
        select(-.id))

# how many nonzero coefficients?
model1 |>
  tidy() |>
  filter(estimate != 0) |>
  nrow()

# what are the largest estimated coefficients?
model1 |>
  tidy() |>
  slice_max(abs(estimate), n = 20)

# in-sample fit
train |>
  bind_cols(predict(model1, train)) |>
  accuracy(truth = .target, estimate = .pred_class)


# out-of-sample fit
test |>
  bind_cols(predict(model1, test)) |>
  accuracy(truth = .target, estimate = .pred_class)

test |>
  bind_cols(predict(model1, test)) |>
  conf_mat(truth = .target, estimate = .pred_class) |>
  autoplot(type = 'heatmap')

ggsave(filename = 'course-website/img/setiment140-test-heatmap.png', width = 8, height = 5)


## Step 3: Predict the sentiment of the 945 Supreme Court tweets with this model ----------------------------------

# notice that there are about 2,000 predictors missing from the test set. The term
# frequency for all those variables should be zero.

missing_stems <- setdiff( names(train), names(scotus_tweets) )

# bind 1936 columns of zeroes
scotus_tweets <- scotus_tweets |>
  bind_cols(
    matrix(0,
           nrow = nrow(scotus_tweets),
           ncol = length(missing_stems),
           dimnames = list(NULL, missing_stems))
  )

# add predictions
scotus_tweets <- scotus_tweets |>
  bind_cols(predict(model1, scotus_tweets, type = 'prob'))

ggplot(data = scotus_tweets,
       mapping = aes(x=.sentiment_score, y=.pred_4)) +
  geom_jitter(alpha = 0.5) +
  theme_minimal() +
  labs(x = 'Expert Sentiment Score',
       y = 'LASSO Probability Positive')

ggsave(filename = 'course-website/img/sentiment140-scotus-tweets.png',
       width = 8,
       height = 5)

scotus_tweets |>
  filter(.sentiment_score == 3 | .sentiment_score == -3) |>
  count( (.sentiment_score == 3 & .pred_4 >= 0.5) |
           (.sentiment_score == -3 & .pred_4 < 0.5) )
