# Packages ----------------------------------------------------------------
library(tidyverse)


# Read in name data set ---------------------------------------------------
name <- read_tsv("name_basics.tsv")
name_example <- name %>% slice(1:100) # use this tibble to think about how to break name dataset
rm(name)


# Seperate into multiple data frames --------------------------------------
name_prof <- name_example %>% 
  select(nconst, primaryName, primaryProfession) %>% 
  rename(name = primaryName, profession = primaryProfession) %>%  # rename columns
  separate_rows(profession, sep = ",")
  
