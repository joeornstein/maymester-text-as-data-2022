#' ---
#' If we have three clusters of words, can k-means on the word
#' embeddings correctly identify the clusters?
#' ---

library(tidyverse)
library(textdata)

## First, get the word embeddings (see 03_word-embeddings/word-embeddings.R) --------------------
options(timeout=5*60)

glove <- embedding_glove6b(dimensions = 100)

# convert this to a matrix (makes things *so* much faster)
vocab <- glove$token

glove <- glove |>
  select(d1:d100) |>
  as.matrix()

rownames(glove) <- vocab

## Next, create three clusters of words ----------------------------

library(readxl)
words <- read_xlsx('data/word-embeddings/three-clusters.xlsx')

# clean up the words (lower case, remove spaces)
words <- words |>
  mutate(word = str_trim(word),
         word = str_to_lower(word))


