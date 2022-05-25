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
  # compute tf and tf-idf
  bind_tf_idf(term = 'word',
              document = 'name',
              n = 'n') |>
  # filter by frequency. remove super rare and super common words
  filter(n > 1, idf != 0)


# convert to a document-term matrix
federalist_dtm <- cast_dtm(data = tidy_federalist,
                           document = 'name',
                           term = 'word',
                           value = 'tf')

federalist_dtm


## step 2: k-means clustering -------------------------

km <- kmeans(x = federalist_dtm,
             centers = 7,
             nstart = 100)

km$cluster

# merge this back with the original federalist paper corpus
cluster_assignments <- tibble(name = names(km$cluster),
                              cluster = km$cluster)


df <- corpus::federalist |>
  select(name, title, author, text) |>
  left_join(cluster_assignments, by = 'name')

count(df, cluster)

## Step 3: Try to find words that distinguish one cluster from another ------------------------

get_top_words <- function(tidy_federalist, cluster_of_interest){
  tidy_federalist |>
    left_join(cluster_assignments, by = 'name') |>
    mutate(in_cluster = if_else(cluster == cluster_of_interest,
                                'within_cluster', 'outside_cluster')) |>
    # count the words in each cluster
    group_by(in_cluster, word) |>
    summarize(n = sum(n)) |>
    pivot_wider(names_from = 'in_cluster',
                values_from = 'n',
                values_fill = 0) |>
    # compute word shares
    mutate(within_cluster = within_cluster / sum(within_cluster),
           outside_cluster = outside_cluster / sum(outside_cluster)) |>
    mutate(delta = within_cluster - outside_cluster) |>
    arrange(-delta) |>
    head(10) |>
    pull(word)
}


get_top_words(tidy_federalist, 5)

map(1:7, ~get_top_words(tidy_federalist, .x))
