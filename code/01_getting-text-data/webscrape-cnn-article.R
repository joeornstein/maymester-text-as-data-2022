
library(tidyverse)
library(rvest)

# three steps to scrape a web page

# 1. Read the HTML from the page
page <- read_html('https://www.state.gov/reports/2022-trafficking-in-persons-report/')

# 2. Get the "elements" on that page that you want

# get only the elements associated with the 'p' (paragraph) tag
paragraphs <- html_elements(page, '#paragraph-10e7fe7b-e92d-fd23-31a3-2acaa6d22137 , .inline-placeholder')

# 3. From the HTML elements, extract the raw text
text <- html_text2(paragraphs)

text
