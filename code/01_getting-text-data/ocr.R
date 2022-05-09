#' ---
#' title: Optical Character Recognition (OCR) Tutorial
#' author: Joe Ornstein
#' date: 2022-05-08
#' version: 0.1
#' ---

# load packages
library(tidyverse)
library(tidytext)
library(tesseract)


## 1. Test the function ocr() to read an image file and detect text strings ------

# the ocr() function reads all the text it sees into a character object
text <- ocr(image = 'data/img/testocr.png')
text

# the ocr_data() function puts each word into a dataframe, along with:
# - the model's confidence that it got the right word
# - a 'bounding box' describing where the word was found in the image
text_data <- ocr_data(image = 'data/img/testocr.png')
text_data

## 2. Convert to tidy tokenized dataframe -----------------------------
d <- tibble(text)
d <- unnest_tokens(d,
                   input = 'text',
                   output = 'word')


## 3. Take a PDF, convert it to png, and call ocr() ---------------------

pngfile <- pdftools::pdf_convert('data/img/Brochure_TAG_Project-Sheets_100-PRINCE.pdf', dpi = 600)

text <- tesseract::ocr(pngfile)

text2 <- pdftools::pdf_ocr_data('data/img/Brochure_TAG_Project-Sheets_100-PRINCE.pdf')

# Garbage. Let's see if we can just capture the text in the white box
library(magick)

brochure <- image_read_pdf('data/img/Brochure_TAG_Project-Sheets_100-PRINCE.pdf', pages = 1, density = 600)

# The syntax to crop an image is is "width x height + left offset + top offset"
brochure_cropped <- image_crop(brochure, '3700 x 4200 + 0 + 2200')
brochure_cropped

brochure_text <- ocr(brochure_cropped)

brochure_tidied <- tibble(brochure_text) |>
  unnest_tokens(input = 'brochure_text',
                output = 'word')
