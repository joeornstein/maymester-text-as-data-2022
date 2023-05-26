#' ---
#'
#' title: Explore word embeddings
#' author: Joe Ornstein
#' date: 2022-05-24
#' version: 0.1
#'
#' ---

library(tidyverse)
library(tidytext)
library(textdata) # to download pre-trained word embeddings

# load some useful functions I'm hiding in another script
source('code/03_word-embeddings/useful-functions.R')


## 1. Download the pre-trained GloVe word embeddings ----------

# tell R to give you at least 5 minutes to download a big file
options(timeout=5*60)

glove <- embedding_glove6b(dimensions = 100)

# convert this to a matrix (makes things *so* much faster)
vocab <- glove$token

glove <- glove |>
  select(d1:d100) |>
  as.matrix()

rownames(glove) <- vocab

## 2. Explore the embedding space -------------------------

# we can't visualize 100 dimensions, but we can find the words
# with the highest cosine similarity

# what words are close to "democracy"?
sim2(x = glove,
     y = glove['democracy', , drop = FALSE],
     method = 'cosine',
     norm = 'l2')[,1] |>
  sort(decreasing = TRUE) |>
  head(15)

cosine_similarity(glove['democracy',],
                  glove['freedom',])


# take a moment to try a few!