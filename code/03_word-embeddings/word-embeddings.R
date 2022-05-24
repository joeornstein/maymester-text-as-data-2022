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

glove <- embedding_glove6b(dimensions = 100)
glove

# right now it's a matrix, but we can put it into a tidy dataframe like so:
tidy_glove <- glove |>
  pivot_longer(contains("d"),
               names_to = "dimension")


## 2. Explore the embedding space -------------------------

# we can't visualize 100 dimensions, but we can find the words
# with the highest cosine similarity

# what words are close to "bill"?
nearest_neighbors(df = tidy_glove,
                  token = 'can')

# take a moment to try a few!

christopher_nn <- nearest_neighbors(df = tidy_glove,
                                    token = 'christopher')

tabitha_nn <- nearest_neighbors(df = tidy_glove,
                                    token = 'tabitha')
