

2 + 2


my_number <- 2 + 3


my_number + 5


# objects can be characters as well. just surround with quotation marks
my_sentence <- 'Hello I am a sentence.'

# here's two different ways to deal with the problem of including a single quotation mark in a character
legislator_name <- "O'Connor"
legislator_name2 <- 'O\'Connor'


# if objects are the "nouns", then functions are the "verbs".


# how many characters were in my sentence?
nchar(my_sentence)

# another way to do function syntax is with the pipe.
my_sentence |> nchar()


# the useful thing about pipes is that you can chain multiple function calls together
my_sentence |>
  toupper() |>
  tolower() |>
  nchar()


# vectors are a type of object that contain a list of objects
my_sentences <- c('I am a sentence.', 'I am also a sentence.')
my_sentences[1]
my_sentences[2]
my_sentences[3]

# helpfully, a lot of R functions are *vectorized* (will perform the function on each element in the vector)
nchar(my_sentences)


# packages

library(tidyverse)

# which of my sentences contains the word "also"?
str_detect(my_sentences, 'also')


