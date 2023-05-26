#' ---
#' Can we create our own custom word embeddings using the
#' Federalist Paper corpus?
#' A proof of concept using the `text2vec` package
#' author: Joe Ornstein
#' date: 2023-05-25
#' ---

# useful vignette here: https://cran.r-project.org/web/packages/text2vec/vignettes/glove.html

library(tidyverse)
library(text2vec)
library(rvest)

# load some useful functions I'm hiding in another script
source('code/03_word-embeddings/useful-functions.R')


## Step 1: Scrape the Federalist Paper corpus from Project Gutenberg ---

# read the raw HTML
page <- read_html('https://www.gutenberg.org/cache/epub/18/pg18-images.html')

# get all the chapters
paragraphs <- html_elements(page, '.chapter')

# get just the text from the element we want
text <- html_text2(paragraphs)

d <- tibble(text)

# get rid of the slightly different version of Federalist 70
d <- d |>
  filter(str_detect(text, 'slightly different version', negate = TRUE))

# strip punctuation and convert to lower case
d$text <- d$text |>
  str_to_lower() |>
  str_replace_all("[[:punct:]]", "") |>
  str_replace_all('\r|\n', '')

# keep only the text between "to the people of the state of new york" and "publius"
# thanks to: https://stackoverflow.com/questions/39086400/extracting-a-string-between-other-two-strings-in-r
d$text <- d$text |>
  str_extract("to the people of the state of new york (.+) publius") |>
  str_replace_all('to the people of the state of new york ', '') |>
  str_replace_all(' publius', '')


## Step 2: Create the "vocabulary" object ----------------

# Create iterator over tokens
tokens <- space_tokenizer(d$text)
# Create vocabulary. Terms will be unigrams (simple words).
it = itoken(tokens)
vocab <- create_vocabulary(it)

# only keep terms that appear at least five times
# it will be tough to get a good representation of a word we don't see a lot
vocab <- prune_vocabulary(vocab, term_count_min = 5)


## Step 3: Create the term-co-occurence matrix (TCM) --------

# Use our filtered vocabulary
vectorizer <- vocab_vectorizer(vocab)
# use window of 10 for context words
tcm <- create_tcm(it, vectorizer, skip_grams_window = 10)

# tcm is now a 3095 x 3095 matrix

## Step 4: Fit the model ----------------------

# now we use the GloVe algorithm to assign each word in the vocabulary
# a vector representation, such that words closer together are more likely
# to co-occur in the corpus.

dim <- 100
glove <- GlobalVectors$new(rank = dim, x_max = 10)
wv_main <- glove$fit_transform(tcm, n_iter = 2000, convergence_tol = 0.0001, n_threads = 8)

# extract the fitted vectors
wv_context <- glove$components
word_vectors <- wv_main + t(wv_context)

colnames(word_vectors) <- paste0('dimension', 1:dim)

word_vectors['democracy',]
word_vectors['freedom',]

cosine_similarity(word_vectors['democracy',],
                  word_vectors['freedom',])

cosine_similarity(word_vectors['athens',],
                  word_vectors['greece',])

cosine_similarity(word_vectors['slaves',],
                  word_vectors['property',])

cosine_similarity(word_vectors['slaves',],
                  word_vectors['people',])


# find the "nearest neighbors" of various words in the vocabulary
sim2(x = word_vectors,
     y = word_vectors['athens', , drop = FALSE],
     method = 'cosine',
     norm = 'l2')[,1] |>
  sort(decreasing = TRUE) |>
  head(15)
