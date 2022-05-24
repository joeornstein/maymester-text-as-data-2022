# Demonstrate TF-IDF (term frequency - inverse document frequency)

library(tidyverse)
library(tidytext)

# load the federalist papers and tokenize
tidy_federalist <- corpus::federalist |>
  # we don't care about author for this application, so just keep name and text
  select(name, text) |>
  # strip the preamble
  mutate(text = str_replace_all(text,
                                pattern = 'To the People of the State of New York:',
                                replacement = '')) |>
  # tokenize to word level
  unnest_tokens(input = 'text',
                output = 'word') |>
  # count up all the words
  count(name, word) |>
  # compute tf-idf
  bind_tf_idf(term = 'word',
              document = 'name',
              n = n)
