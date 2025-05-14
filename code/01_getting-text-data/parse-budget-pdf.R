library(pdftools)

pdf <- pdf_text('data/budget-pdfs/Chattahoochee.2021.pdf')
page <- pdf[24] # page with the Capital Assets spreadsheet

# split into lines
lines <- str_split_1(page, '\n')
lines[9:15]

# just extract the spreadsheet
sheet <- page |>
  str_extract("(?s)(?<=as follows:\\n\\n).*?(?=\\n\\n\\n\\n\\n)")

library(readr)
df <- read_tsv(sheet)
