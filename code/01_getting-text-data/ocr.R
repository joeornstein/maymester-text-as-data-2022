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
library(pdftools)
library(magick)


## 1. Test the function ocr() to read an image file and detect text strings ------

# the ocr() function reads all the text it sees into a character object
text <- ocr(image = 'data/img/testocr.png')
text

# OCR the fax
fax <- ocr(image = 'data/img/ocrscan_1.png')

## 2. Try with some really terrible ones ------------------


otago_express <- ocr(image = 'data/img/old_newspaper.jpeg')

hamilton <- ocr(image = 'data/img/hamilton.png')

# may be running into trouble with the three columns of text. let's try
# cropping it first
hamilton_image <- image_read(path = 'data/img/hamilton.png')

# The syntax to crop an image is is "width x height + left offset + top offset"
hamilton_crop <- image_crop(hamilton_image, '470 x 800 + 60 + 0')
hamilton_crop

hamilton_text <- ocr(image = hamilton_crop)
hamilton_text


## 3. Try a pdf with multiple columns ------------------------

text <- pdftools::pdf_ocr_data('data/img/SOJ.pdf')
# this approach doesn't recognize the two columns.

# 1. convert to a png (just the third page)
pdf_convert(pdf = 'data/img/SOJ.pdf',
                     format = 'png', dpi = 600,
                     pages = 3,
                     filenames = 'data/img/SOJ_page3.png')
# 2. read it in with magick
page3 <- image_read('data/img/SOJ_page3.png')
page3
# 2. Crop it into two images (syntax is ?width x height + left offset + top offset")
page3_left <- image_crop(page3, '2550 x 4300 + 0 + 1000')
page3_left

page3_right <- image_crop(page3, '2550 x 5000 + 2550 + 1000')
page3_right

# 3. OCR the text for each side
text_left <- ocr(page3_left)
text_right <- ocr(page3_right)

# 4. Paste together
text <- paste(text_left, text_right)

text



# 5. Word cloud that page
library(tidytext)
library(wordcloud2)

tibble(text) |>
  unnest_tokens(input = 'text',
                output = 'word') |>
  anti_join(get_stopwords()) |>
  count(word) |>
  rename(freq = n) |>
  wordcloud2()