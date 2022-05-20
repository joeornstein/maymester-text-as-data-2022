#' ---
#'
#' title: Bag of words representation of State of the Union speeches over time
#'
#' ---


library(tidyverse)
library(tidytext)
library(sotu)
library(SnowballC)

## 1. Tokenize to the sentence level ------------------------

df <- sotu_meta |>
  mutate(text = sotu_text)

sotu_sentences <- df |>
  unnest_tokens(output = 'sentence',
                input = 'text',
                token = 'sentences')

sotu_sentences$sentence_id <- 1:nrow(sotu_sentences)

## 2. Identify the sentences that use the word 'manufacturing' ---------

sotu_sentences <- sotu_sentences |>
  filter(str_detect(sentence, 'manufactur'))


## 3. Tokenize to the word level -------------------------

sotu_words <- sotu_sentences |>
  unnest_tokens(input = 'sentence',
                output = 'word') |>
  anti_join(get_stopwords()) |>
  mutate(word_stem = wordStem(word))


# look at the frequency distribution of these words so far
sotu_words |>
  filter(word_stem != 'manufactur') |>
  count(word_stem) |>
  ggplot(mapping = aes(x=n)) +
  geom_histogram(color = 'black')

# let's remove the words that only appear once
sotu_words <- sotu_words |>
  group_by(word_stem) |>
  filter(n() > 1) |>
  ungroup()


## 4. Word cloud -------------------------
library(wordcloud2)

sotu_words |>
  count(word_stem) |>
  wordcloud2()



