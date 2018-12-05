# Add in other data sets --------------------------------------------------
library(tidyverse)


# Read in data set --------------------------------------------------------
principals <- read_tsv("title_principals.tsv")


# Divide principals data set according to category column -----------------
# Have a look at the categories
category <- principals %>% distinct(category)


# Division
self <- principals %>% filter(category == "self")
director <- principals %>% filter(category == "director")
cinematographer <- principals %>% filter(category == "cinematographer")
composer <- principals %>% filter(category == "composer")
producer <- principals %>% filter(category == "producer")
editor <- principals %>% filter(category == "editor")
actor <- principals %>% filter(category == "actor")
actress <- principals %>% filter(category == "actress")
writer <- principals %>% filter(category == "writer")
production_designer <- principals %>% filter(category == "production_designer")
archive_footage <- principals %>% filter(category == "archive_footage")
archive_sound <- principals %>% filter(category == "archive_sound")


# Remove the original, save and clean up the workspace
rm(principals)
save(self, file = "self")
save(director, file = "director")
save(cinematographer, file = "cinematographer")
save(composer, file = "composer")
save(producer, file = "producer")
save(editor, file = "editor")
save(actor, file = "actor")
save(actress, file = "actress")
save(writer, file = "writer")
save(production_designer, file = "production_designer")
save(archive_footage, file = "archive_footage")
save(archive_sound, file = "archive_sound")
