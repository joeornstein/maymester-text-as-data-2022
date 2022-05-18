# R refresher
library(tidyverse)


# saving information as an object
text <- 'This is a bit of text.'

# here's a function
tolower(text)

# here's that same function call with a pipe
text |> tolower()

# useful for iterative function calls
text |> tolower() |> toupper()

