# do our tools tokenize Chinese characters out-of-the-box?

library(tidyverse)
library(tidytext)
library(pdftools)

# read the pdf
doc <- pdf_text('data/img/chinese-document.pdf')
doc <- paste0(doc, collapse = '\n')

doc <- tibble(doc)

# tokenize
tokenized_doc <- doc |>
  unnest_tokens(input = 'doc',
                output = 'word')


