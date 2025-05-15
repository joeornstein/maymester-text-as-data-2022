## Get Raw Text of Proposed Rules from Federal Register Bulk Data Repository

# data saved in raw/FR-2025


## Step 1: Get the dates and document numbers of the proposed rules -------------------

library(tidyverse)
library(httr)

# get metadata on proposed rules from 2025
response <- GET('https://www.federalregister.gov/api/v1/documents.json?fields[]=publication_date&fields[]=document_number&fields[]=raw_text_url&fields[]=title&per_page=1000&conditions[publication_date][year]=2025&conditions[type][]=PRORULE')

# convert the response into an R list
content <- response$content |>
  rawToChar() |>
  jsonlite::fromJSON()

df <- content$results

## Step 2: For each document, find its entry in the bulk data download ----

library(lubridate)

df$publication_date[1]
df$document_number[1]

# get the month folder
month <- month(df$publication_date[1])

# read that xml file
content <- xml2::read_xml(paste0('raw/FR-2025/0', month, '/FR-',
                                 df$publication_date[1], '.xml'))

xml2::xml_text(content)

