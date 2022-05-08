#' ---
#' title: Webscraping with the rvest package
#' author: Joe Ornstein
#' date: 2022-05-08
#' version: 0.1
#' ---

# load packages
library(tidyverse)
library(rvest) # for harvesting text data from the web

## 1. Read HTML from page containing cable news transcript ---------------

page <- read_html('https://www.foxnews.com/transcript/tucker-the-us-is-looking-at-a-grim-economic-picture')

text <- page |>
  html_elements('body') |>
  html_text2(preserve_nbsp = FALSE)

d <- tibble(text)

library(tidytext)

d2 <- d |>
  unnest_tokens(output = 'word',
                input = 'text')
