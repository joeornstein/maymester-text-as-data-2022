


# to install the package
# devtools::install_github('joeornstein/text2data')

library(tidyverse)
library(text2data)

# let's read in our API key like this
# get the API key from https://platform.openai.com/account/api-keys
api_key <- read_file('openai-api-key.txt')

# IF YOU ARE A MAC USER, YOU MAY NEED THE FOLLOWING SETUP STEPS AS WELL
# reticulate::conda_create('myenv')
# THEN RESTART R
# reticulate::use_condaenv('myenv')

# then setup the OpenAI Python module like this
# text2data::setup_openai()


complete_prompt('My favorite food is', max_tokens = 30,
                openai_api_key = api_key)

complete_prompt('My favorite food is', max_tokens = 1,
                openai_api_key = api_key)



complete_prompt('Decide whether the sentiment of the following statment is Positive or Negative\n\n"Help I am stuck in a well."\n\nSentiment:',
                max_tokens = 1,
                openai_api_key = api_key)



## package is broken today, so we're just going to go through the python directly:

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

custom_complete_prompt(prompt = 'My favorite food is')

custom_complete_prompt(prompt = 'My favorite food is', max_tokens = 100)



## Armed with our new function, we can try to apply to datasets of interest........















