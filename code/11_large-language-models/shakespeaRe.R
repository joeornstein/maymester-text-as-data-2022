

## Let's try to classify the works of Shakespeare into comedies and tragedies


library(bardr)
library(tidyverse)
library(tidytext)


df <- bardr::all_works_df


## One approach: a bag-of-words style sentiment analysis.
## count up all the positive vs. negative words, and
## the ones with the most negative words are the tragedies

tokenized_shakespeare <- df |>
  filter(genre %in% c('Tragedy', 'Comedy')) |>
  unnest_tokens(input = 'content', output = 'word') |>
  # keep only the positive and negative sentiment words
  inner_join(get_sentiments('bing'))

# compute sentiment as the share of positive words
shakespeare_sentiment <- tokenized_shakespeare |>
  count(name, genre, sentiment) |>
  # pivot wider so we have a column for positive words and a column for negative words
  pivot_wider(names_from = 'sentiment',
              values_from = 'n') |>
  mutate(sentiment_score = positive / (negative + positive) * 100)



## Step 2: Topic model of the Shakespeare plays -----------------------

# our first step is creating a document-level that's more than just a single line
# for ease, let's split it into 100 lines per document
# on average they're about 2400 lines per play, so 24 documents per play

# just keep the comedies and tragedies
df <- df |>
  filter(genre %in% c('Comedy', 'Tragedy'))

for(play in unique(df$name)){

  print(play)

  this_play <- df |>
    filter(name == play)

  # create a chunk variable
  this_play$line_number <- 1:nrow(this_play)
  this_play$chunk_number <- this_play$line_number %/% 100

  this_play <- this_play |>
    select(name, genre, chunk_number, content) |>
    group_by(name, genre, chunk_number) |>
    summarize(content = paste(content, collapse = ' '))


  # then, bind it onto a new dataset called "chunked_plays"
  if(play == unique(df$name)[1]){
    chunked_plays <- this_play
  } else{
    chunked_plays <- bind_rows(chunked_plays, this_play)
  }

}

# create a unique id for each chunk
chunked_plays <- chunked_plays |>
  ungroup() |>
  mutate(id = 1:n())

# can we replace those box characters with apostrophes?
chunked_plays$content[12]

chunked_plays <- chunked_plays |>
  mutate(content = str_replace_all(content, '\032', '\''))



# to fit an LDA model, we need to first convert this into a document-term matrix

library(words)

tokenized_chunked_plays <- chunked_plays |>
  unnest_tokens(input = 'content', output = 'word') |>
  anti_join(get_stopwords(source = 'smart')) |>
  # just keep scrabble words
  inner_join(words::words, by = 'word') |>
  count(id, word) |>
  # remove rare words
  filter(n > 2)

chunked_dtm <- cast_dtm(data = tokenized_chunked_plays,
                        document = 'id',
                        term = 'word',
                        value = 'n')


# fit the LDA
library(topicmodels)

shakespeare_lda <- LDA(chunked_dtm, k = 30)

# use the tidy() function from tidytext to extract the beta vector
shakespeare_topics <- tidy(shakespeare_lda, matrix = 'beta')

shakespeare_topics |>
  # get each word's average beta across topics
  group_by(term) |>
  mutate(average_beta = mean(beta)) |>
  ungroup() |>
  # compare beta in that topic with the average beta
  mutate(delta = beta - average_beta) |>
  # get the words with the largest difference in each topic
  group_by(topic) |>
  slice_max(delta, n = 15) |>
  # plot it
  ggplot(mapping = aes(x=delta, y=reorder(term, delta))) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~topic, scales = 'free') +
  labs(x = 'Term Probability Compared to Average',
       y = 'Term')






## Approach 3: Ask GPT-3 what the topics/themes are in each document --------------------


# try a random sample of 100
set.seed(42)
shakespeare_sample <- chunked_plays |>
  slice_sample(n = 100)


library(tidyverse)
library(text2data)
api_key <- read_file('openai-api-key.txt')

custom_complete_prompt <- function(prompt, max_tokens = 1, temperature = 0){

  # first, create an openai Python object
  openai <- reticulate::import("openai")

  # input the api key
  openai$api_key = api_key

  # ask the API for a completion
  response <- openai$Completion$create(engine = 'text-davinci-003', prompt = prompt,
                                       logprobs = as.integer(1),
                                       max_tokens = as.integer(max_tokens),
                                       temperature = as.integer(temperature))

  # get the text
  return(response$choices[[1]]$text)
}

# zero-shot prompt
custom_complete_prompt(prompt = paste0('Describe the themes explored in this passage from a Shakespeare play, separated by commas.\n---\n',
                                       shakespeare_sample$content[1], '\n---\nThemes:'),
                       max_tokens = 100, temperature = 0)




























