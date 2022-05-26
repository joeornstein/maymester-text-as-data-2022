#' ---
#'
#' title: Press release clustering
#'
#' ---

library(tidyverse)
library(tidytext)

## Step 1: Load in the 558 press releases -----------------------

files_in_folder <- list.files('data/press-releases/Lautenberg/')

# create an empty dataframe to fill up
df <- tibble(
  id = 1:length(files_in_folder),
  date = rep(NA, length(files_in_folder)),
  text = rep(NA, length(files_in_folder))
  )

# loop through the text files and pull the text
for(i in 1:length(files_in_folder)){

  # pull the date
  df$date[i] <- files_in_folder[i] |>
    str_sub(1, 9)

  # pull the text
  df$text[i] <- read_file(
    paste0('data/press-releases/Lautenberg/',
           files_in_folder[i])
  )

}

## Step 2: Tidy up the text -------------------------------

df <- df |>
  mutate(text = str_replace_all(text,
                                pattern = '     Senator Frank R  Lautenberg                                                                                                                      Press Release        of        Senator Lautenberg                                                                                ',
                                replacement = ''))




