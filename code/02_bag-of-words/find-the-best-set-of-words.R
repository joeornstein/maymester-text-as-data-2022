
library(tidyverse)
library(tidytext)

# Let's look at the Mosteller & Wallace (1964) list and try to determine which discriminate best between authors)
mw1964_words <- c("a", "all", "also", "an", "and", "any", "are", "as", "at", "be", "been", "but", "by", "can", "do", "down",
                  "even", "every", "for", "from", "had", "has", "have", "her", "his", "if", "in", "into", "is", "it", "its",
                  "may", "more", "must", "my", "no", "not", "now", "of", "on", "one", "only", "or", "our", "shall", "should",
                  "so", "some", "such", "than", "that", "the", "their", "then", "there", "things", "this", "to", "up", "upon",
                  "was", "were", "what", "when", "which", "while", "whilst", "who", "will", "with", "would", "your")

tidy_federalist <- corpus::federalist |>
  # remove the preamble
  mutate(text = str_replace_all(text,
                                pattern = 'To the People of the State of New York:',
                                replacement = '')) |>
  # tokenize to the word level
  unnest_tokens(input = 'text',
                output = 'word') |>
  filter(word %in% mw1964_words) |>
  mutate(author = factor(author),
         word = factor(word))

# create a dataframe where each column is an author and each row is a frequency for each word
frequency_table <- tidy_federalist |>
  filter(author %in% c('Hamilton', 'Jay', 'Madison')) |>
  count(author, word, .drop = FALSE) |>
  pivot_wider(names_from = 'author',
              values_from = 'n') |>
  # normalize by each author's number of words
  mutate(Hamilton = Hamilton / sum(Hamilton),
         Jay = Jay / sum(Jay),
         Madison = Madison / sum(Madison)) |>
  mutate(Hamilton_Jay = Hamilton / Jay,
         Hamilton_Madison = Hamilton / Madison,
         Jay_Madison = Jay / Madison) |>
  # just keep the words that one author uses 3 times more often than another
  filter(Hamilton_Jay > 3 |
           Hamilton_Jay < 0.33333 |
           Hamilton_Madison > 3 |
           Hamilton_Madison < 0.33333 |
           Jay_Madison > 3 |
           Jay_Madison < 0.33333)


# our new set of interesting words
interesting_words <- frequency_table$word


## Validation test ---------------------------------


# 42 (Madison), # 33 (Hamilton), 4 for Jay

bags_of_words <- tidy_federalist |>
  # remove the ones we're validating on
  filter(!(name %in% c('Federalist No. 42', 'Federalist No. 33',
                       'Federalist No. 4'))) |>
  # keep only the 10 words we found before
  filter(word %in% interesting_words) |>
  # sort the words in alphabetical order
  arrange(author, word) |>
  # recode word as a factor
  mutate(word = factor(word)) |>
  filter(author %in% c('Hamilton', 'Madison', 'Jay')) |>
  count(author, word, .drop = FALSE)


# create vectors for each author and paper
hamilton <- bags_of_words |>
  filter(author == 'Hamilton') |>
  pull(n) |>
  set_names(interesting_words)
hamilton

madison <- bags_of_words |>
  filter(author == 'Madison') |>
  pull(n) |>
  set_names(interesting_words)

jay <- bags_of_words |>
  filter(author == 'Jay') |>
  pull(n) |>
  set_names(interesting_words)

fed42 <- tidy_federalist |>
  filter(name == 'Federalist No. 42') |>
  # count words
  count(word, .drop = FALSE) |>
  # keep only the 10 words we found before
  filter(word %in% interesting_words) |>
  pull(n) |>
  set_names(interesting_words)


fed33 <- tidy_federalist |>
  filter(name == 'Federalist No. 33') |>
  # count words
  count(word, .drop = FALSE) |>
  # keep only the 10 words we found before
  filter(word %in% interesting_words) |>
  pull(n) |>
  set_names(interesting_words)

fed4 <- tidy_federalist |>
  filter(name == 'Federalist No. 4') |>
  # count words
  count(word, .drop = FALSE) |>
  # keep only the 10 words we found before
  filter(word %in% interesting_words) |>
  pull(n) |>
  set_names(interesting_words)


## Likelihood Ratios and Cosine Similarity ------------------

# define cosine similarity
cosine_similarity <- function(x1, x2){
  sum(x1*x2) / sqrt(sum(x1^2)) / sqrt(sum(x2^2))
}

# Federalist No 4
cosine_similarity(hamilton, fed4)
cosine_similarity(madison, fed4)
cosine_similarity(jay, fed4)

# Federalist No 33
cosine_similarity(hamilton, fed33)
cosine_similarity(madison, fed33)
cosine_similarity(jay, fed33)

# Federalist No 42
cosine_similarity(hamilton, fed42)
cosine_similarity(madison, fed42)
cosine_similarity(jay, fed42)


# likelihood ratios

dmultinom(x = fed4,
          prob = jay + 0.1)
dmultinom(x = fed4,
          prob = hamilton + 0.1)
dmultinom(x = fed4,
          prob = madison + 0.1)

# 7 billion times more likely than Hamilton
dmultinom(x = fed4,
          prob = jay + 0.1) /
  dmultinom(x = fed4,
            prob = hamilton + 0.1)

# 8 million times more likely than Madison
dmultinom(x = fed4,
          prob = jay + 0.1) /
  dmultinom(x = fed4,
            prob = madison + 0.1)



# 33
dmultinom(x = fed33,
          prob = hamilton + 0.1) /
  dmultinom(x = fed33,
            prob = jay + 0.1)

dmultinom(x = fed33,
          prob = hamilton + 0.1) /
  dmultinom(x = fed33,
            prob = madison + 0.1)

# 42
dmultinom(x = fed42,
          prob = madison + 0.1) /
  dmultinom(x = fed42,
            prob = jay + 0.1)

dmultinom(x = fed42,
          prob = madison + 0.1) /
  dmultinom(x = fed42,
            prob = hamilton + 0.1)
