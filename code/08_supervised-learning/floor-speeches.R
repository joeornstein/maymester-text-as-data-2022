#' ---
#'
#' Floor speeches - predict whether the speech was made by a Republican
#' or Democrat
#'
#' ---

library(tidyverse)
library(tidytext)
library(tidymodels)
library(SnowballC)

load('data/congressional-floor-speeches/wilkerson-casas-2017.RData')

## Step 1: Tidy the data and compute the predictors ------------

# find the word stems that you want to include in the model
stems_to_keep <- d |>
  mutate(speech = str_replace_all(speech,
                                  pattern = 'Mr. Speaker, I rise today ',
                                  replacement = '')) |>
  unnest_tokens(input = 'speech',
                output = 'word') |>
  # remove the stop words and numerals
  anti_join(get_stopwords()) |>
  filter(str_detect(word, '[0-9]', negate = TRUE)) |>
  # convert to word stems
  mutate(word_stem = wordStem(word)) |>
  count(word_stem) |>
  # remove the very rarest words (20 uses, for the sake of my RAM)
  filter(n > 20) |>
  pull(word_stem)

tidy_speeches <- d |>
  # create a unique ID for each speech
  mutate(id = 1:nrow(d)) |>
  # remove the preamble
  mutate(speech = str_replace_all(speech,
                                  pattern = 'Mr. Speaker, I rise today ',
                                  replacement = '')) |>
  unnest_tokens(input = 'speech',
                output = 'word') |>
  # just keep the words stems we identified before
  mutate(word_stem = wordStem(word)) |>
  filter(word_stem %in% stems_to_keep) |>
  filter(word_stem != '') |>
  # count up the words
  count(id, word_stem) |>
  # compute term frequency
  bind_tf_idf(document = 'id',
              term = 'word_stem',
              n = 'n') |>
  # pivot so each term frequency is a column vector
  select(id, word_stem, tf) |>
  pivot_wider(id_cols = 'id',
              names_from = 'word_stem',
              values_from = 'tf',
              values_fill = 0)

# add the party labels back in
tidy_speeches <- d |>
  mutate(id = 1:nrow(d)) |>
  select(id, party) |>
  left_join(tidy_speeches, by = 'id') |>
  mutate(party = factor(party))


# split into training and test sets
speech_split <- initial_split(data = tidy_speeches,
                              prop = 0.8)

train <- training(speech_split)
test <- testing(speech_split)


## Step 2: Fit a LASSO model ---------------------------

model1 <- logistic_reg(penalty = 0.01, mixture = 1) |>
  set_engine('glmnet') |>
  fit(formula = party ~ .,
      data = train |>
        select(-id) )

# look at the coefficients
tidy(model1)

# visualize the coefficients
model1 |>
  tidy() |>
  filter(abs(estimate) > 50) |>
  ggplot(mapping = aes(x=estimate, y=reorder(term, -estimate))) +
  geom_col() +
  labs(x = 'Coefficient Estimate',
       caption = 'Positive is More Republican',
       y = 'Word Stem')


## Check out-of-sample prediction ----------------

# does it do a good job at prediction though?

# in-sample fit
train |>
  select(party) |>
  bind_cols(predict(model1, train)) |>
  accuracy(truth = party, estimate = .pred_class)

# out-of-sample fit
test |>
  select(party) |>
  bind_cols(predict(model1, test)) |>
  accuracy(truth = party, estimate = .pred_class)





