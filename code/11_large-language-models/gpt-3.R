


# to install the package
# devtools::install_github('joeornstein/text2data')

library(tidyverse)
library(text2data)

# let's read in our API key like this
# get the API key from https://platform.openai.com/account/api-keys
api_key <- read_file('openai-api-key.txt')

# then setup the OpenAI Python module like this
text2data::setup_openai()


complete_prompt('My favorite food is', max_tokens = 30,
                openai_api_key = api_key)

complete_prompt('My favorite food is', max_tokens = 1,
                openai_api_key = api_key)



complete_prompt('Decide whether the sentiment of the following statment is Positive or Negative\n\n"Help I am stuck in a well."\n\nSentiment:',
                max_tokens = 1,
                openai_api_key = api_key)




