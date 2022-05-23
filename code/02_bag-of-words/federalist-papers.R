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
  # remove the salutation
  mutate(text = str_replace_all(text,
                                pattern = 'To the People of the State of New York:',
                                replacement = '')) |>
  # tokenize to words
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

interesting_words <- c('although', 'always', 'commonly', 'consequently',
                       'considerable', 'heretofore', 'upon', 'whilst')

federalist_interesting_words <- tidy_federalist |>
  filter(word %in% interesting_words) |>
  # convert these to factors so count() doesn't drop the zero counts later
  mutate(author = factor(author),
         word = factor(word))

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
  count(author, word, .drop = FALSE) |>
  # sort the words in alphabetical order
  arrange(author, word)

# pull the vectors for Hamilton, Madison, and Jay
hamilton_vector <- bags_of_words |>
  filter(author == 'Hamilton') |>
  pull(n) |>
  # add names to make the vector more readable
  set_names(interesting_words)

madison_vector <- bags_of_words |>
  filter(author == 'Madison') |>
  pull(n) |>
  # add names to make the vector more readable
  set_names(interesting_words)

jay_vector <- bags_of_words |>
  filter(author == 'Jay') |>
  pull |>
  # add names to make the vector more readable
  set_names(interesting_words)

# now get the vector for Federalist No. 18
fed18_vector <- federalist_interesting_words |>
  filter(name == 'Federalist No. 18') |>
  count(word, .drop = FALSE) |>
  pull(n) |>
  # add names to make the vector more readable
  set_names(interesting_words)


# multinomial likelihood for each one
dmultinom(x = fed18_vector,
          prob = hamilton_vector)

dmultinom(x = fed18_vector,
          prob = madison_vector)

dmultinom(x = fed18_vector,
          prob = jay_vector)

# the computation above makes a very strong assumption about Jay:
# because he never uses the word "whilst" in any of his Federalist papers,
# we assume that he would *never* *ever* use the word whilst in another paper.
# We can do better by regularizing our estimates, adding a small positive number to each vector,
# to include the possibility that Jay might someday use the word whilst.

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

# likelihood ratios
madison_likelihood / hamilton_likelihood
madison_likelihood / jay_likelihood


## Another way to think about the problem: the VECTOR SPACE MODEL ---------------------


# define cosine similarity
cosine_similarity <- function(x1, x2){
  sum(x1*x2) / sqrt(sum(x1^2)) / sqrt(sum(x2^2))
}

# start with two dimensions, upon and whilst
x1 <- c(20, 0)
x2 <- c(0, 10)

# this should give us a similarity score of 0, because they're completely orthogonal (non-overlapping)
cosine_similarity(x1, x2)

# now two vectors with an angle of 0
x1 <- c(20, 20)
x2 <- c(40, 40)

# should give us a cosine similarity of 1
cosine_similarity(x1, x2)


# get the cosine similarity for Federalist 18 with all the authors
cosine_similarity(fed18_vector, hamilton_vector)
cosine_similarity(fed18_vector, madison_vector)
cosine_similarity(fed18_vector, jay_vector)



## Validation test: does our procedure accurately classify an undisputed Hamilton / Madison paper? ---------------


# Federalist No. 30 is a Hamilton paper, and 45 is Madison. Let's pretend we didn't know that:
federalist_interesting_words <- tidy_federalist |>
  mutate(author = if_else(name %in% c('Federalist No. 30',
                                      'Federalist No. 45'),
                          'Unknown', author)) |>
  filter(word %in% interesting_words) |>
  mutate(author = factor(author),
         word = factor(word))



bags_of_words <- federalist_interesting_words |>
  filter(author %in% c('Hamilton', 'Madison', 'Jay')) |>
  count(author, word, .drop = FALSE) |>
  # sort the words in alphabetical order
  arrange(author, word)

# pull the vectors for Hamilton, Madison, and Jay
hamilton_vector <- bags_of_words |>
  filter(author == 'Hamilton') |>
  pull(n) |>
  # add names to make the vector more readable
  set_names(interesting_words)

madison_vector <- bags_of_words |>
  filter(author == 'Madison') |>
  pull(n) |>
  # add names to make the vector more readable
  set_names(interesting_words)

jay_vector <- bags_of_words |>
  filter(author == 'Jay') |>
  pull(n) |>
  # add names to make the vector more readable
  set_names(interesting_words)

# here's the vector for Federalist 30 (Hamilton)
fed30_vector <- federalist_interesting_words |>
  filter(name == 'Federalist No. 30') |>
  count(word, .drop = FALSE) |>
  pull(n) |>
  # add names to make the vector more readable
  set_names(interesting_words)

fed30_vector

# and for Federalist 45 (Madison)
fed45_vector <- federalist_interesting_words |>
  filter(name == 'Federalist No. 45') |>
  count(word, .drop = FALSE) |>
  pull(n) |>
  # add names to make the vector more readable
  set_names(interesting_words)

fed45_vector


# Likelihood Ratio (Federalist 30)
dmultinom(x = fed30_vector, prob = hamilton_vector + 0.1) /
  dmultinom(x = fed30_vector, prob = madison_vector + 0.1)

# Cosine similarities (Federalist 30)
cosine_similarity(fed30_vector, hamilton_vector)
cosine_similarity(fed30_vector, madison_vector)

# Likelihood Ratio (Federalist 45)
dmultinom(x = fed45_vector, prob = madison_vector + 0.1) /
  dmultinom(x = fed45_vector, prob = hamilton_vector + 0.1)
# according to our model, it would be mind-bogglingly unlikely for Hamilton
# to get through a paper without the word upon.

# Cosine Similarity (Federalist 45)
cosine_similarity(fed45_vector, hamilton_vector)
cosine_similarity(fed45_vector, madison_vector)

# Note, however, that this test sometimes performs poorly distinguishing between Madison and Jay
# for instance, Fed 10 (the "mischiefs of faction" paper)
fed10_vector <- federalist_interesting_words |>
  filter(name == 'Federalist No. 10') |>
  count(word, .drop = FALSE) |>
  pull(n) |>
  # add names to make the vector more readable
  set_names(interesting_words)

fed10_vector

dmultinom(x = fed10_vector, prob = jay_vector + 0.1) /
  dmultinom(x = fed10_vector, prob = madison_vector + 0.1)

cosine_similarity(fed10_vector, jay_vector)
cosine_similarity(fed10_vector, madison_vector)

# both our methods think that this paper looks like a Jay paper,
# probably because of all the "always". This is a case where, failing
# a crucial validation test, we should go back and refine our method,
# including a set of words that does a good job distinguishing between
# Madison and Jay in the same way "upon" and "whilst" do a good job distinguishing
# between Hamilton and Madison.