# Packages ----------------------------------------------------------------
library(tidyverse)


# Read in name data set ---------------------------------------------------
name <- read_tsv("name_basics.tsv")
name_example <- name %>% slice(1:100) # example data set


# Divide and tidy ---------------------------------------------------------
name_profession <- name %>% 
  select(nconst, primaryName, primaryProfession) %>% 
  rename(name = primaryName, profession = primaryProfession) %>% 
  separate_rows(profession, sep = ",")


name_known <- name %>% 
  select(nconst, primaryName, knownForTitles) %>% 
  rename(name = primaryName, known = knownForTitles)


name_age <- name %>% 
  select(nconst, primaryName, birthYear, deathYear) %>% 
  rename(name = primaryName, birth = birthYear, death = deathYear)


rm(name) 