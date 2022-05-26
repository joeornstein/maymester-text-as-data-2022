# look at Jansa et al data

library(tidyverse)
library(readstata13)

d <- read.dta13('data/raw/jansa-2019/Copy_Paste_Legislate_Succeed_Analysis_Dataset.dta')


# note: the dataset doesn't include the texts, and I don't think it even contains enough observations to replicate Table 2