#' ---
#' title: Webscraping with the rvest package
#' author: Joe Ornstein
#' date: 2022-05-08
#' version: 0.1
#' ---

# load packages
library(tidyverse)
library(tidytext)
library(rvest) # for harvesting text data from the web

## 1. Read HTML from page containing cable news transcript ---------------

page <- read_html('https://www.foxnews.com/transcript/tucker-the-us-is-looking-at-a-grim-economic-picture')

## 2. Keep only the text in the main body ------------

text <- page |>
  # get the paragraphs (p)
  html_elements('p') |>
  # convert to text
  html_text2()

# looks like the transcript is in the fourth paragraph element
# let's take it and convert it to a tokenized dataframe
d <- tibble(transcript = text[4]) |>
  unnest_tokens(output = 'word',
                input = 'transcript')

# create a word cloud
library(wordcloud2)

# word cloud of all the sentiment-laden words
d |>
  anti_join(get_stopwords()) |>
  inner_join(get_sentiments('bing')) |>
  filter(!(word %in% c('right', 'like', 'well', 'trump'))) |>
  count(word) |>
  wordcloud2()