# test the Smith-Waterman alignment algorithm

library(tidyverse)
library(text.alignment)

# load the bill texts
d <- read_csv('data/raw/PolicyIdeasReplication/AJPSPPACA.csv')

# bill texts from table 1
index <- 1

bill <- d$BillText[index]
law <- d$LawText[index]


sw <- smith_waterman(bill, law)

sw

# note that Wilkerson et al have a slightly different "affine" penalty for gaps, but the vanilla Smith-Waterman results are pretty close.


smith_waterman('hello, my name is Joe!!!', 'hello, my name is Joe!')

