# Packages ----------------------------------------------------------------
library(tidyverse)


# Read in name data set ---------------------------------------------------
name <- read_tsv("name_basics.tsv")
name_example <- name %>% slice(1:100) # example data set


# Divide and tidy ---------------------------------------------------------
name_profession <- name %>% 
  select(nconst, primaryName, primaryProfession) %>% 
  rename(name = primaryName, profession = primaryProfession) %>% 
  filter(!is.na(profession)) %>% # Removed NAs
  # NA is different from char string "NA"
  separate_rows(profession, sep = ",") 


name_known <- name %>% 
  select(nconst, primaryName, knownForTitles) %>% 
  rename(name = primaryName, known = knownForTitles) 
  # this line to check if there's NAs in known col - if so, remove them
  # separate_rows(known, sep = ",") # this will expand the rectangle a lot


# Only containts information of professionals who are ceased
name_age <- name %>% 
  select(nconst, primaryName, birthYear, deathYear) %>% 
  rename(name = primaryName, birth = birthYear, death = deathYear) %>% 
  mutate(birth = as.integer(birth), death = as.integer(death), 
         age = death - birth) %>% 
  filter(!is.na(birth), !is.na(death), !is.na(age))
  

rm(name) 


# Exploration -------------------------------------------------------------
prof_count <- name_profession %>% count(profession) %>% arrange(desc(n))
prof_count %>% ggplot(aes(x = n, y = reorder(profession, n))) +
  geom_point()


name_age_arrange <- name_age %>%
  arrange(desc(age)) %>% 
  {.[1,4] <- 1865; .[1,5] <- 53; . } %>% # if without . at the very end, it won't save the entire tibble
  # Corrected data value after some research
  # name_age %>%  {.[1,4] <- 1865; .} is equivalent to 
  # name_age %T>% {.[1,4] <- 1865} magrittr tree operator
  filter(age > 0) %>% # remove negative age and age 0
  arrange(age)

name_age_arrange %>% ggplot(aes(x = age)) +
  geom_bar()
