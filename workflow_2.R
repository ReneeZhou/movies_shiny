# Add in other data sets --------------------------------------------------
library(tidyverse)


# Read in data set --------------------------------------------------------
principals <- read_tsv("title_principals.tsv")


# Divide principals data set according to category column -----------------
# Have a look at the categories
principals %>% distinct(category)
self <- principals %>% filter(category == "self")
director <- principals %>% filter(category == "director")
cinematographer <- principals %>% filter(category == "cinematographer")
producer <- principals %>% filter(category == "producer")
editor <- principals %>% filter(category == "editor")
actor <- principals %>% filter(category == "actor")
actress <- principals %>% filter(category == "actress")
writer <- principals %>% filter(category == "writer")
production_designer <- principals %>% filter(category == "production_designer")
archive_footage <- principals %>% filter(category == "archive_footage")
archive_sound <- principals %>% filter(category == "archive_sound")