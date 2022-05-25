#' ---
#' K-means tutorial
#' ---

library(tidyverse)

# create a random set of data
df <- tibble(
  x = c(1,2,1,2,4,2, 2.5, 8, 6, 5,4, 7, 9, 6.7, 8, 9),
  y = c(2,1,2,2.5, 1,4.4, 1, 3, 10, 10, 7, 9, 6, 7, 8, 10)
)

ggplot(data = df,
       mapping = aes(x=x, y=y)) +
  geom_point(alpha = 0.7)

# kmeans is included in the base stats package
km <- kmeans(x = df,
             centers = 4)

km

# visualize the cluster assignments
df |>
  mutate(cluster_assignment = km$cluster) |>
  ggplot(mapping = aes(x=x, y=y, color = factor(cluster_assignment))) +
  geom_point() +
  theme_minimal()

