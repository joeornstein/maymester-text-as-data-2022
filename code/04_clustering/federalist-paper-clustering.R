#' ---
#'
#' title: Cluster the Federalist papers using k-means to get a sense of the set of topics
#'
#' ---

library(tidyverse)
library(tidytext)

## step 1: load the Federalist papers, tokenize to the word level -------

tidy_federalist <- corpus::federalist |>
  mutate(text = str_replace_all(text,
                                pattern = 'To the People of the State of New York:',
                                replacement = '')) |>
  unnest_tokens(input = 'text',
                output = 'word') |>
  anti_join(get_stopwords()) |>
  count(name, word) |>
  # remove the numerals
  filter(str_detect(word, '[0-9]', negate = TRUE)) |>
  # turn the counts into percentages
  group_by(name) |>
  mutate(word_share = n / sum(n))


# convert to a document-term matrix
federalist_dtm <- cast_dtm(data = tidy_federalist,
                           document = 'name',
                           term = 'word',
                           value = 'word_share')

federalist_dtm


## step 2: k-means clustering -------------------------

km <- kmeans(x = federalist_dtm,
             centers = 10,
             nstart = 100)

km$cluster

# merge this back with the original federalist paper corpus
cluster_assignments <- tibble(name = names(km$cluster),
                              cluster = km$cluster)


df <- corpus::federalist |>
  select(name, title, author, text) |>
  left_join(cluster_assignments, by = 'name')

## Step 3: Try to find words that distinguish one cluster from another ------------------------

cluster_of_interest <- 8

distinctive_words <- tidy_federalist |>
  left_join(cluster_assignments, by = 'name') |>
  mutate(cluster_of_interest = if_else(cluster == cluster_of_interest,
                                       'yes', 'no')) |>
  # count the words in each cluster
  group_by(cluster_of_interest, word) |>
  summarize(n = sum(n)) |>
  pivot_wider(names_from = 'cluster_of_interest',
              values_from = 'n',
              values_fill = 0) |>
  # compute word shares
  mutate(yes = yes / sum(yes),
         no = no / sum(no)) |>
  mutate(delta = yes - no) |>
  arrange(-delta)


# now with center vectors

centers <- km$centers

# take the vector for one topic and subtract the average vector for all the other topics
cluster_of_interest <- 10

v1 <- centers[cluster_of_interest,]
v2 <- colMeans(centers[-cluster_of_interest,])

# subtract the two and sort
distinctive_words <- sort(v1 - v2)

# ten most distinctive words from that cluster
distinctive_words[8490:8500]

# next step: dive into the texts and see if this would be a good cluster label



