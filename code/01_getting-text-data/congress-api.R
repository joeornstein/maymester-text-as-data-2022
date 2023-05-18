# https://www.dataquest.io/blog/r-api-tutorial/

api_key <- ''

library(httr)
d <- GET(url = paste0('https://api.congress.gov/v3/bill/118/s/25/summaries?api_key=', api_key))

library(jsonlite)
content <- d$content |>
  rawToChar() |>
  fromJSON()

content$summaries$text

# convert that from html to text
library(rvest)
content$summaries$text |>
  read_html() |>
  html_text2() |>
  cat()
