#' ---
#'
#' title: Press release clustering
#'
#' ---

library(tidyverse)
library(tidytext)
library(SnowballC)

set.seed(42) # set a random seed so that we get the same results from run to run

## Step 1: Load in the 558 press releases -----------------------

files_in_folder <- list.files('data/press-releases/Lautenberg/')

# create an empty dataframe to fill up
df <- tibble(
  id = 1:length(files_in_folder),
  date = rep(NA, length(files_in_folder)),
  text = rep(NA, length(files_in_folder))
  )

# loop through the text files and pull the text
for(i in 1:length(files_in_folder)){

  # pull the date
  df$date[i] <- files_in_folder[i] |>
    str_sub(1, 9)

  # pull the text
  df$text[i] <- read_file(
    paste0('data/press-releases/Lautenberg/',
           files_in_folder[i])
  )

}

# save the completed dataset
save(df, file = 'data/press-releases/lautenberg.RData')


## Step 2: Tidy up the text -------------------------------

tidy_press_releases <- df |>
  mutate(text = str_replace_all(text,
                                pattern = '     Senator Frank R  Lautenberg                                                                                                                      Press Release        of        Senator Lautenberg                                                                                ',
                                replacement = '')) |>
  # tokenize to the word level
  unnest_tokens(input = 'text',
                output = 'word') |>
  # remove stop words
  anti_join(get_stopwords()) |>
  # remove numerals
  filter(str_detect(word, '[0-9]', negate = TRUE)) |>
  # create word stems
  mutate(word_stem = wordStem(word)) |>
  # count up bag of word stems
  count(id, word_stem) |>
  # # remove the infrequent word stems
  # filter(n > 2) |>
  # compute tf-idf
  bind_tf_idf(term = 'word_stem',
              document = 'id',
              n = 'n') |>
  filter(!is.na(tf_idf))

# create document-term matrix
lautenberg_dtm <- cast_dtm(data = tidy_press_releases,
                           document = 'id',
                           term = 'word_stem',
                           value = 'tf')
lautenberg_dtm


## Step 3: K-means clustering -------------------------

km <- kmeans(x = lautenberg_dtm,
             centers = 4,
             nstart = 100)

table(km$cluster)


# function to find the words that are most overrepresented in the cluster mean for a given cluster
get_top_words <- function(centers, cluster_of_interest, n = 10){
  (centers[cluster_of_interest,] - colMeans(centers[-cluster_of_interest,])) |>
    sort(decreasing = TRUE) |>
    head(n)
}

# cluster 1 (security)
get_top_words(km$centers, 1)

# cluster 2 (legislation / senate business)
get_top_words(km$centers, 2)

# cluster 3 ("partisan taunting")
get_top_words(km$centers, 3)

# cluster 4 (credit claiming for New Jersey projects)
get_top_words(km$centers, 4)


## Step 4: Remerge with original dataset and see if our topic labels make sense ---------

cluster_assignments <- tibble(id = km$cluster |> names() |> as.numeric(),
                              cluster = km$cluster)

df <- df |>
  left_join(cluster_assignments,
            by = 'id')

# this one should be about security
df |>
  filter(cluster == 1) |>
  slice_sample(n = 1) |>
  pull(text)

# this one should be about legislation / Senate business
df |>
  filter(cluster == 2) |>
  slice_sample(n = 1) |>
  pull(text)

# this one should be "partisan taunting"
df |>
  filter(cluster == 3) |>
  slice_sample(n = 1) |>
  pull(text)


# this one should be credit claiming / New Jersey related
df |>
  filter(cluster == 4) |>
  slice_sample(n = 1) |>
  pull(text)


## Step 4: Let's try that again, but using average word embeddings to represent our documents ------------------

library(textdata)

# get the glove embedding vectors
glove <- embedding_glove6b(dimensions = 100)

# convert to a matrix (it will make the computation easier)
glove_tokens <- glove$token

glove <- glove |>
  select(-token) |>
  as.matrix()

rownames(glove) <- glove_tokens

glove[100:120,1:3]


# get all the press releases and count up the words for each
tidy_press_releases <- df |>
  # get rid of the earlier cluster assignments
  select(-cluster, -date) |>
  mutate(text = str_replace_all(text,
                                pattern = '     Senator Frank R  Lautenberg                                                                                                                      Press Release        of        Senator Lautenberg                                                                                ',
                                replacement = '')) |>
  # tokenize to the word level
  unnest_tokens(input = 'text',
                output = 'word') |>
  # remove stop words
  anti_join(get_stopwords()) |>
  # remove numerals
  filter(str_detect(word, '[0-9]', negate = TRUE)) |>
  # remove the words that aren't in the glove lexicon
  filter(word %in% glove_tokens)

# matrix called document embeddings
document_embeddings <- matrix(nrow = 558,
                              ncol = 100)

for(i in 1:558){

  list_of_words <- tidy_press_releases |>
    filter(id == i) |>
    pull(word)

  document_embeddings[i,] <- colMeans(glove[list_of_words,])

}

# now run k-means on the document embedding vectors
km <- kmeans(x = document_embeddings,
             centers = 20,
             nstart = 100)

table(km$cluster)


# what do the top words in those clusters look like?

# merge the cluster assignments back with the documents
cluster_assignments <- tibble(id = 1:558,
                              cluster = km$cluster)

df <- df |>
  # remove the earlier cluster assignment from bag of words
  select(-cluster) |>
  left_join(cluster_assignments,
            by = 'id')

table(df$cluster)


get_top_words <- function(tidy_press_releases, cluster_of_interest){
  tidy_press_releases |>
    count(id, word) |>
    left_join(cluster_assignments, by = 'id') |>
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

get_top_words(tidy_press_releases, 1)
get_top_words(tidy_press_releases, 2)
get_top_words(tidy_press_releases, 3)
get_top_words(tidy_press_releases, 4)
get_top_words(tidy_press_releases, 5)
get_top_words(tidy_press_releases, 6)




