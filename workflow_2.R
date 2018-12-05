# Add in other data sets --------------------------------------------------
library(tidyverse)

# The data sets are large; read in the largest first and use gc() to release RAM
principals <- read_tsv("title_principals.tsv")
name <- read_tsv("name_basics.tsv")
akas <- read_tsv("title_akas.tsv")

the_unparsed <- map_dfr(list(principals, name, akas), problems)
