# scrape the Federalist Paper Number 10


library(tidyverse)
library(rvest)


page <- read_html('https://billofrightsinstitute.org/primary-sources/federalist-no-10')

text <- page |>
  html_elements('.RichText_richtext__3G1SE') |>
  html_text() |>
  # remove everything before "Federalist 10 Full Text"
  str_remove("(?s).*Federalist 10 Full Text\n")
