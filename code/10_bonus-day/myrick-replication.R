#' ---
#'
#' title: Replicate Myrick (2021)
#'
#' ---

library(tidyverse)
library(tidytext)
library(tidymodels)

## Load speeches dataset --------------------

all_speeches <- read_csv('data/raw/myrick-2021/Data/external_threat_textdata_study1_speechesclean.csv')

# Keep just the speeches about Iran from 1975 - 1983
iran_speeches <- all_speeches |>
  filter(iso3 == 'IRN',
         session_startyr >= 1975,
         session_startyr <= 1983)

rm(all_speeches)

iran_speeches |>
  count(iso3, session_startyr)

# get the words we're going to keep
vocab <- read_csv('data/raw/myrick-2021/Data/external_threat_textdata_study1_fixedvocab.csv')

# keep the 3000 most common words
words_to_keep <- vocab |>
  slice_max(count, n = 3000) |>
  pull(word)


## Create a function to compute the classification accuracy -------------

get_classification_accuracy <- function(df, .country, .year){

  # print start time
  print(paste('Fitting', .country, .year, 'model at', Sys.time()))

  # subset the data
  df <- df |>
    filter(iso3 == .country,
           session_startyr == .year)

  # tokenize and create term frequency columns
  tidy_speeches <- df |>
    unnest_tokens(input = 'speech',
                  output = 'word') |>
    filter(word %in% words_to_keep) |>
    mutate(word = factor(word)) |>
    count(party, speech_id, word) |>
    bind_tf_idf(term = 'word',
                document = 'speech_id',
                n = 'n') |>
    select(.party = party, .speech_id = speech_id, word, tf) |>
    pivot_wider(id_cols = c('.party', '.speech_id'),
                names_from = 'word',
                values_from = 'tf',
                values_fill = 0) |>
    mutate(.party = factor(.party))

  # split into cross-validation folds
  folds <- vfold_cv(tidy_speeches |>
                      select(-.speech_id),
                    v = 10)
  folds

  # now, fit the model on each training fold, assessing performance
  # on the out-of-sample fold
  model_specification <-
    logistic_reg(penalty = 0.01, mixture = 1) |>
    set_engine('glmnet')

  cv_fit <- workflow() |>
    add_model(model_specification) |>
    add_formula(.party ~ .) |>
    fit_resamples(folds)

  # print end time
  print(paste('Completed', .country, .year, 'model at', Sys.time()))

  collect_metrics(cv_fit) |>
    filter(.metric == 'accuracy') |>
    pull(mean)

}

iran1975_accuracy <- get_classification_accuracy(df = iran_speeches,
                                                 .country = 'IRN',
                                                 .year = 1975)



# now for homework do that but for all the other years