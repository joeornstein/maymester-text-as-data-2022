#' -----
#'
#' title: Text as Outcome
#'
#'
#' ---

library(tidyverse)
library(tidytext)
library(tidymodels)
library(topicmodels)

## Step 1: splitting the data into a set for measurement, and a set for estimation --------------

d <- read_csv('data/egami-2018/Experiment2.csv')

set.seed(42)

d_split <- initial_split(data = d,
                         prop = 0.5,
                         strata = 'treat')

measurement_set <- training(d_split)

estimation_set <- testing(d_split)


## Step 2: Create the g function -------------------


measurement_set |>
  select(-treat) |>
  View()

# Fit a Latent Dirichlet Allocation


# tokenize and create a document-term matrix
tidy_responses <- measurement_set |>
  mutate(id = 1:nrow(measurement_set)) |>
  unnest_tokens(input = 'text',
                output = 'word') |>
  anti_join(get_stopwords()) |>
  filter(!is.na(word)) |>
  count(id, word)

responses_dtm <- cast_dtm(data = tidy_responses,
                          document = 'id',
                          term = 'word',
                          value = 'n')

responses_dtm

responses_lda <- LDA(responses_dtm, k = 2, control = list(seed = 42))

# look at the topic-level probability vectors
our_topics <- tidy(responses_lda, matrix = 'beta')

our_topics |>
  # get each word's average beta across topics
  group_by(term) |>
  mutate(average_beta = mean(beta)) |>
  ungroup() |>
  # compare beta in that topic with the average beta
  mutate(delta = beta - average_beta) |>
  # get the words with the largest difference in each topic
  group_by(topic) |>
  slice_max(delta, n = 15) |>
  # plot it
  ggplot(mapping = aes(x=delta, y=reorder(term, delta))) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~topic, scales = 'free') +
  labs(x = 'Term Probability Compared to Average',
       y = 'Term')

lda_documents <- tidy(responses_lda, matrix = 'gamma')

lda_documents

# no matter how many clusters we try, LDA cannot distinguish between
# documents