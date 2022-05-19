# Plot the inflation-intensity for each cable news program


library(tidyverse)
library(rvest)
library(tidytext)

## Step 1: Scrape The Text -------------------------------

df <- read_csv('data/inflation-project/cable-news-urls.csv')

# loop through the URLs, scraping the transcript text for each one
for(i in 1:nrow(df)){

  df$text[i] <- read_html(df$url[i]) |>
    html_elements(df$selector[i]) |>
    html_text2()

  print(paste0('Completed URL ', i))

}


## Step 2: Tokenize and count words ----------------------------

tidy_transcripts <- df |>
  unnest_tokens(input = 'text',
                output = 'word')

# how many words per episode?
tidy_transcripts |>
  count(program, network, date)

# some are much wordier than others!


# count inflation words as a percent of total words said
inflation_counts <- tidy_transcripts |>
  group_by(program, network, date) |>
  summarize(total_words = n(),
            inflation_words = sum(str_detect(word, 'inflatio|^pric|^shortage'))) |>
  mutate(pct_inflation = inflation_words / total_words * 100) |>
  ungroup()


# plot the results
p <- inflation_counts |>
  mutate(program_label = paste0(program, ' (', date, ')')) |>
  ggplot(mapping = aes(x = pct_inflation,
                       y = reorder(program_label, pct_inflation),
                       fill = network)) +
  geom_col() +
  theme_minimal() +
  labs(x = 'Percent of Words About Inflation',
       y = 'Program',
       fill = 'Network')

p
