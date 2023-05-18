
# let's try to scrape the bill summary of Senate Bill 25 from the congress.gov site
library(tidyverse)
library(rvest)

# read the html
page <- read_html("https://www.congress.gov/bill/118th-congress/senate-bill/25")

# get the #bill-summary elements from that page
paragraphs <- html_elements(page, "#bill-summary")

# convert it to text
text <- html_text2(paragraphs)
text

## turns out this doesn't work except on Brandon's home network????
## because the federal government prefers you working through the API


# https://www.dataquest.io/blog/r-api-tutorial/

# Workflow:


# Step 1: Get your API key (good practice not to hard code it in the script)
api_key <- read_file('congress-api-key.txt')

# Step 2: Get the JSON file from the API using the URL from https://gpo.congress.gov/#/bill/bill_summaries
library(httr)
d <- GET(url = paste0('https://api.congress.gov/v3/bill/118/s/25/summaries?api_key=', api_key))

# Step 3: Take the content and convert it from unicode to an R character object
library(jsonlite)
# take the content attribute
content <- d$content |>
  # convert from unicode to English characters
  rawToChar() |>
  # convert the JSON format to an R object
  fromJSON()

content$summaries$text

# Step 4: convert that from html to text, removing all the tags
library(rvest)
bill_summary <- content$summaries$text |>
  read_html() |>
  html_text2()
