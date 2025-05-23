## Get text embeddings from OpenAI


# devtools::install_github('joeornstein/fuzzylink')

library(fuzzylink)

# copy-paste your API key here:
openai_api_key('<MY API KEY>', install = TRUE)

# get the text embeddings for a bunch of animals and their babies
animals <- c('cat', 'kitten', 'dog', 'puppy', 'horse', 'foal',
             'duck', 'duckling', 'moose', 'calf')

animal_embeddings <- get_embeddings(animals)
animal_embeddings

# extract some rows from that matrix
cat_vector <- animal_embeddings['cat',]
dog_vector <- animal_embeddings['dog',]
kitten_vector <- animal_embeddings['kitten',]
duckling_vector <- animal_embeddings['duckling',]
puppy_vector <- animal_embeddings['puppy',]

# cosine similarity = dot product divided by length
# nice thing about OpenAI embeddings is that they're always
# length 1, so we just take the dot product
sum(cat_vector * dog_vector)
sum(cat_vector * kitten_vector)
sum(cat_vector * puppy_vector)
sum(kitten_vector * duckling_vector)
sum(cat_vector * duckling_vector)

## matrix multiplication is just a series of dot products
cosine_similarities <- animal_embeddings %*% t(animal_embeddings)

