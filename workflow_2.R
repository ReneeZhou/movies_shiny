# Packages ----------------------------------------------------------------
library(tidyverse)


# Read in principals data set ---------------------------------------------
principals <- read_tsv("title_principals.tsv")


# Divide principals data set according to category column -----------------
category_var <- unique(principals$category)


# # Looping over each category to save as different files -----------------
for (i in 1:length(category_var)) {
  obj <- principals %>% filter(category == category_var[i])
  nam <- category_var[i]
  saveRDS(obj, file = paste0(nam, ".rds"))
  rm(obj, nam, i)
}


# Release memory ----------------------------------------------------------
rm(principals)


# Read in name data set ---------------------------------------------------
name <- read_tsv("name_basics.tsv")
name_example <- name %>% slice(1:100) # use this tibble to think about how to break name dataset
rm(name)
