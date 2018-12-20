# Packages ----------------------------------------------------------------
library(tidyverse)


# Read in principals data set ---------------------------------------------
principals <- read_tsv("title_principals.tsv")


# Divide principals data set according to category column -----------------
category_var <- unique(principals$category)
write_rds(category_var, path = "category_var.rds", compress = "gz") # saving for later use


# # Looping over each category to save as different files -----------------
for (i in 1:length(category_var)) {
  obj <- principals %>% filter(category == category_var[i])
  nam <- category_var[i]
  write_rds(obj, path = paste0("raw_", nam, ".rds"), compress = "gz")
  rm(obj, nam, i)
}


# Release memory ----------------------------------------------------------
rm(principals)


# Loop to read in files
# Basic cleaning - remove columns contain no extra info
for (i in 1:length(category_var)) { 
  # can change length to load in a few files only 
  # use 11:12 as the smallest files
  nam <- category_var[i]
  obj <- read_rds(paste0("raw_", nam, ".rds"))
  
  if (length(unique(obj$job)) == 1) {
    if (unique(obj$job) == "\\N") {
      if (length(unique(obj$characters)) == 1) {
        if (unique(obj$characters) == "\\N") {
          obj <- obj %>% 
            select(-job, -characters, -category)
        }
      } else {
        obj <- obj %>% 
          select(-job, -category)
      }
    }
  } else if (length(unique(obj$characters)) == 1) {
      if (unique(obj$characters) == "\\N") {
        obj <- obj %>% 
          select(-characters, -category)
    }
  } else {
    obj <- obj %>% 
      select(-category)
  }
  
  # Assign the namce accordingly
  assign(nam, obj)
  # Overwrite files
  write_rds(obj, path = paste0("clean_", nam, ".rds"), compress = "gz")
  # Remove redundant objs
  rm(i, nam, obj)
}
