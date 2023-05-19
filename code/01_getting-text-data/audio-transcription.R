
# if you're running Windows, you will likely need to install Rtools
# https://cran.rstudio.com/bin/windows/Rtools/
# remotes::install_github('bnosac/audio.whisper')
library(audio.whisper)

model <- whisper('tiny')

transcript <- predict(model, 'data/audio/jfk.wav',
                      lang = 'en', n_threads = 4)

transcript$data$text

# for more: https://github.com/bnosac/audio.whisper


# this package needs a 16-bit wav file, so if your audio file is something else,
# need to convert it first:
library(av)
av_audio_convert("data/audio/Sophia 1_GMT20230328-204657_Recording.m4a",
                 output = "data/audio/sophia.wav", format = "wav", sample_rate = 16000)

transcript <- predict(model, 'data/audio/sophia.wav',
                      lang = 'en', n_threads = 4)

d <- transcript$data


# suppose I want to measure sentiment of the interview over time.
# let's tokenize each of the interview segments.

library(tidytext)
tidy_interview <- d |>
  unnest_tokens(input = 'text',
                output = 'word')

# perform a basic sentiment analysis
library(tidyverse)

sentiment_analysis <- tidy_interview |>
  inner_join(get_sentiments(lexicon = 'bing')) |>
  # remove some words that aren't actually conveying sentiment
  filter(!word %in% c('like', 'well')) |>
  # count up the number of positive and negative words by segment
  count(segment, sentiment) |>
  # create a separate column for positive and negative words
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) |>
  # measure sentiment as the percent of positive words in each segment
  mutate(sentiment_score = positive / (positive + negative) * 100)


ggplot(data = sentiment_analysis,
       mapping = aes(x= segment, y=sentiment_score)) +
  geom_point() +
  geom_line()




