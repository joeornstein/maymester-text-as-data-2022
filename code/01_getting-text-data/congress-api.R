
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




# Okay, let's put all those steps into a function, so we can call it with one line.
get_bill_summary <- function(congress, bill_type, bill_number,
                             api_key = read_file('congress-api-key.txt')){

  # Step 1: Get the JSON file from the API using the URL from https://gpo.congress.gov/#/bill/bill_summaries
  library(httr)
  d <- GET(url = paste0(
    'https://api.congress.gov/v3/bill/',
    congress, '/', bill_type, '/', bill_number, '/summaries?api_key=', api_key))

  # Step 2: Take the content and convert it from unicode to an R character object
  library(jsonlite)
  # take the content attribute
  content <- d$content |>
    # convert from unicode to English characters
    rawToChar() |>
    # convert the JSON format to an R object
    fromJSON()

  # keep only the most recent bill summary
  most_recent_summary <- content$summaries$text[length(content$summaries$text)]

  # Step 4: convert that from html to text, removing all the tags
  library(rvest)
  bill_summary <- most_recent_summary |>
    read_html() |>
    html_text2()

  return(bill_summary)

}

# 2023 Assault Weapons Ban
get_bill_summary(congress = 118, bill_type = 's', bill_number = 25)

# 1994 Assault Weapons Ban
get_bill_summary(congress = 103, bill_type = 'hr', bill_number = 4296)



# now that we have this function, let's get the bill summaries from every major piece of
# gun control legislation over the past two decades.
library(readxl)
congress <- read_xlsx('data/congress/gun_control_legislation.xlsx')

# create a new column for bill summaries
congress$summary <- ''

# loop through each row in the dataset and get the bill summary
for(i in 1:nrow(congress)){

  print(i)
  congress$summary[i] <- get_bill_summary(congress = congress$congress[i],
                                          bill_type = congress$bill_type[i],
                                          bill_number = congress$bill_number[i])

}







