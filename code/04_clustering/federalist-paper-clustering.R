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

## Step 3: Try to find distinctive words ------------------------

centers <- km$centers

# take the vector for one topic and subtract the average vector for all the other topics
v1 <- centers[4,]
v2 <- colMeans(centers[c(1:3, 5:10),])

# subtract the two and sort
distinctive_words <- sort(v1 - v2)

# ten most distinctive words from that cluster
distinctive_words[8490:8500]

# next step: dive into the texts and see if this would be a good cluster label



