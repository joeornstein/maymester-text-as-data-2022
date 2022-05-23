#' ---
#'
#' title: Tidy the Federalist Papers and predict authorship of the disputed Federalist No. 18
#' date: 2022-05-23
#'
#' ---

library(tidyverse)
library(tidytext)
library(corpus)

## Load the data ----------------------

d <- federalist

tidy_federalist <- d |>
  mutate(text = str_replace_all(text,
                                pattern = 'To the People of the State of New York:',
                                replacement = '')) |>
  unnest_tokens(input = 'text',
                output = 'word')


## word cloud some of these papers ----------------

library(wordcloud2)

# All the Federalist Papers
tidy_federalist |>
  anti_join(get_stopwords()) |>
  count(word) |>
  filter(str_detect(word, '[0-9]', negate = TRUE)) |>
  wordcloud2()

tidy_federalist |>
  anti_join(get_stopwords()) |>
  filter(author == 'Hamilton') |>
  count(word) |>
  filter(str_detect(word, '[0-9]', negate = TRUE)) |>
  wordcloud2()

tidy_federalist |>
  anti_join(get_stopwords()) |>
  filter(author == 'Madison') |>
  count(word) |>
  filter(str_detect(word, '[0-9]', negate = TRUE)) |>
  wordcloud2()


## Keep only interesting stop words -----------------------

federalist_interesting_words <- tidy_federalist |>
  filter(word %in% c('upon', 'although', 'always', 'commonly',
                     'consequently', 'considerable', 'heretofore',
                     'whilst'))

# create a word cloud of these "interesting" words for each author
federalist_interesting_words |>
  filter(author == 'Hamilton') |>
  count(word) |>
  wordcloud2()

federalist_interesting_words |>
  filter(author == 'Madison') |>
  count(word) |>
  wordcloud2()

# how do these bags of words compare to a disputed text?
federalist_interesting_words |>
  filter(name == 'Federalist No. 18') |>
  count(word) |>
  wordcloud2()

# demo wordcloud() function
hamilton_bag_of_words <- federalist_interesting_words |>
  filter(author == 'Hamilton') |>
  count(word)
wordcloud(words = hamilton_bag_of_words$word,
          freq = hamilton_bag_of_words$n)


## Model the bag of words ------------------------

# we think Fed 18 was written by Madison, but how *sure* are we?

# get the frequencies of the interesting words in each author's corpus
bags_of_words <- federalist_interesting_words |>
  filter(author %in% c('Hamilton', 'Madison', 'Jay')) |>
  count(author, word) |>
  # fill in the missing words with zeroes (thanks: https://stackoverflow.com/questions/22523131/dplyr-summarise-equivalent-of-drop-false-to-keep-groups-with-zero-length-in)
  complete(author, word, fill = list(n = 0)) |>
  # sort the words in alphabetical order
  arrange(author, word)

# pull the vectors for Hamilton, Madison, and Jay
hamilton_vector <- bags_of_words |>
  filter(author == 'Hamilton') |>
  pull(n)

madison_vector <- bags_of_words |>
  filter(author == 'Madison') |>
  pull(n)

jay_vector <- bags_of_words |>
  filter(author == 'Jay') |>
  pull(n)

# now get the vector for Federalist No. 18
fed18_vector <- c(0, 1, 0, 0, 1, 0, 1, 1)

# multinomial likelihood for each one
dmultinom(x = fed18_vector,
          prob = hamilton_vector)

dmultinom(x = fed18_vector,
          prob = madison_vector)

dmultinom(x = fed18_vector,
          prob = jay_vector)


# Laplace smoothing (aka being generous to Jay)
hamilton_likelihood <- dmultinom(x = fed18_vector,
                                 prob = hamilton_vector + 0.1)

madison_likelihood <- dmultinom(x = fed18_vector,
                                prob = madison_vector + 0.1)

jay_likelihood <- dmultinom(x = fed18_vector,
                            prob = jay_vector + 0.1)

hamilton_likelihood
madison_likelihood
jay_likelihood

madison_likelihood / hamilton_likelihood
madison_likelihood / jay_likelihood


## Another way to think about the problem: the VECTOR SPACE MODEL ---------------------


# cosine similarity

# start with two dimensions, upon and whilst
x1 <- c(20, 0)
x2 <- c(0, 10)

cosine_similarity <- function(x1, x2){
  sum(x1*x2) / sqrt(sum(x1^2)) / sqrt(sum(x2^2))
}

cosine_similarity(x1, x2)

# two vectors with an angle of 0
x1 <- c(20, 20)
x2 <- c(40, 40)

# should give us a cosine similarity of 1
cosine_similarity(x1, x2)


# get the cosine similarity for Federalist 18 with all the authors
cosine_similarity(fed18_vector, hamilton_vector)
cosine_similarity(fed18_vector, madison_vector)
cosine_similarity(fed18_vector, jay_vector)





