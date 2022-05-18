# Introduction to webscraping


library(tidyverse)
library(rvest)
library(tidytext)

# read the raw HTML from Hannity's transcript page
page <- read_html('https://www.foxnews.com/transcript/hannity-biden-admin-playing-blame-game')


# get all the "paragraphs"
paragraphs <- html_elements(page, 'p')

# get just the text from the element we want
text <- html_text2(paragraphs[[4]])

text

# now using the SelectorGadget
text <- read_html('https://www.foxnews.com/transcript/hannity-biden-admin-playing-blame-game') |>
  html_elements('.speakable:nth-child(6)') |>
  html_text2()

text


## Step 2: Tidy the text and count the times he says "inflation" ---------------

# put the text into a dataframe (aka tibble)
d <- tibble(text)

# split it up into words (tokenization)
tidy_hannity <- unnest_tokens(tbl = d,
                              output = 'word',
                              input = 'text')

# count number of times the word "inflation" was used in the transcript
tidy_hannity |>
  filter(word == 'inflation') |>
  nrow()

# another route; count each word, then filter the ones you want
tidy_hannity |>
  count(word) |>
  filter(word %in% c('inflation', 'price', 'prices', 'shortage'))

# this time using regular expressions, to catch plurals
inflation_count <- tidy_hannity |>
  count(word) |>
  filter(str_detect(word, 'inflatio|^pric|^shortage'))


# write to a csv
write_csv(inflation_count, file = 'data/inflation-project/hannity_may17.csv')

