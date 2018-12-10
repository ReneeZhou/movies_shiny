# Packages ----------------------------------------------------------------
library(tidyverse)
library(htmlwidgets)


# Read in data set --------------------------------------------------------
principals <- read_tsv("title_principals.tsv")


# Divide principals data set according to category column -----------------
category_var <- unique(principals$category)


# Looping over category, and save it as a local file
for (i in 1:length(category_var)) {
  obj <- principals %>% filter(category == category_var[i])
  nam <- category_var[i]
  saveRDS(obj, file = paste0(nam, ".rds"))
  rm(obj, nam, i)
}


# Remove the original data set to release memory
rm(principals)
