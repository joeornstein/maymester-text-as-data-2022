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


## 1. Use the function ocr() to read an image file and detect text strings ------
text <- ocr(image = 'data/img/testocr.png')
text

## 2. Convert to tidy tokenized dataframe -----------------------------
d <- tibble(text)
d <- unnest_tokens(d,
                   input = 'text',
                   output = 'word')


## 3. Take a PDF, convert it to png, and call ocr() ---------------------

pngfile <- pdftools::pdf_convert('data/img/Brochure_TAG_Project-Sheets_100-PRINCE.pdf', dpi = 600)

text <- tesseract::ocr(pngfile)

text2 <- pdftools::pdf_ocr_data('data/img/Brochure_TAG_Project-Sheets_100-PRINCE.pdf')

# garbage. let's see if we can just capture the text in the white box
library(magick)

brochure <- image_read_pdf('data/img/Brochure_TAG_Project-Sheets_100-PRINCE.pdf', pages = 1, density = 600)

# syntax is width x height + left offset + top offset
brochure_cropped <- image_crop(brochure, '3700 x 4200 + 0 + 2200')
brochure_cropped

brochure_text <- ocr(brochure_cropped)

brochure_tidied <- tibble(brochure_text) |>
  unnest_tokens(input = 'brochure_text',
                output = 'word')
