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
            inflation_words = sum(str_detect(word, 'inflatio|^pric|^shortage')),
            trump_mentions = sum(word == 'trump'),
            ukraine_words = sum(str_detect(word, '^ukrai|^escal^|^nucl|^zelens|war|^russi')),
            extremism_mentions = sum(str_detect(word, '^extremi'))) |>
  mutate(pct_inflation = inflation_words / total_words * 100,
         pct_trump = trump_mentions / total_words * 100,
         pct_ukraine = ukraine_words / total_words * 100,
         pct_extremism = extremism_mentions / total_words * 100) |>
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

p2 <- inflation_counts |>
  mutate(program_label = paste0(program, ' (', date, ')')) |>
  ggplot(mapping = aes(x = pct_trump,
                       y = reorder(program_label, pct_trump),
                       fill = network)) +
  geom_col() +
  theme_minimal() +
  labs(x = 'Trump Mentions (% of Words)',
       y = 'Program',
       fill = 'Network')

p2

p3 <- inflation_counts |>
  mutate(program_label = paste0(program, ' (', date, ')')) |>
  ggplot(mapping = aes(x = pct_ukraine,
                       y = reorder(program_label, pct_ukraine),
                       fill = network)) +
  geom_col() +
  theme_minimal() +
  labs(x = 'Words About Ukraine War (% of total)',
       y = 'Program',
       fill = 'Network')

p3

p4 <- inflation_counts |>
  mutate(program_label = paste0(program, ' (', date, ')')) |>
  ggplot(mapping = aes(x = pct_extremism,
                       y = reorder(program_label, pct_extremism),
                       fill = network)) +
  geom_col() +
  theme_minimal() +
  labs(x = 'Extremism Mentions (% of total)',
       y = 'Program',
       fill = 'Network')

p4


## Step 3: Make a word cloud -------------------------------------

library(wordcloud2)

tidy_transcripts |>
  filter(program == 'Hannity') |>
  # remove the stop words
  anti_join(get_stopwords()) |>
  group_by(word) |>
  summarize(freq = n()) |>
  arrange(-freq) |>
  wordcloud2()

tidy_transcripts |>
  filter(program == 'Hannity') |>
  # keep only the words that expressed emotion
  inner_join(get_sentiments('bing')) |>
  # filter out some of the filler words, like like
  filter(!(word %in% c('trump', 'like', 'well', 'right'))) |>
  group_by(word) |>
  summarize(freq = n()) |>
  arrange(-freq) |>
  wordcloud2()


