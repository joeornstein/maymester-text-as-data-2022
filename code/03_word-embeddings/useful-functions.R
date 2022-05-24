
# function to find nearest neighbors from a tidy word embedding dataframe
# credit: https://smltar.com/embeddings.html#glove
library(widyr)
nearest_neighbors <- function(df, token) {
  df |>
    rename(item1 = token) |>
    widely(
      ~ {
        y <- .[rep(token, nrow(.)), ]
        res <- rowSums(. * y) /
          (sqrt(rowSums(. ^ 2)) * sqrt(sum(.[token, ] ^ 2)))
        matrix(res, ncol = 1, dimnames = list(x = names(res)))
      },
      sort = TRUE,
      maximum_size = NULL
    )(item1, dimension, value) %>%
    select(-item2)
}
