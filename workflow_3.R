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


name_age <- name_age %>%
  arrange(desc(age)) %>% 
  {.[1,4] <- 1865; .[1,5] <- 53; . } %>% # if without . at the very end, it won't save the entire tibble
  # Corrected data value after some research
  # name_age %>%  {.[1,4] <- 1865; .} is equivalent to 
  # name_age %T>% {.[1,4] <- 1865} magrittr tree operator
  filter(age > 0) %>% # remove negative age and age 0
  arrange(age)

  
# Save data sets as local files
write_rds(name_age, path = "name_age.rds", compress = "gz")
write_rds(name_known, path = "name_known_for_title.rds", compress = "gz")
write_rds(name_profession, path = "name_profession.rds", compress = "gz")
write_rds(name_example, path = "name_test.rds", compress = "gz")


rm(name) 


# Exploration -------------------------------------------------------------
# Distribution of different prfessions
prof_count <- name_profession %>% 
  count(profession) %>% 
  arrange(desc(n)) %>% 
  mutate(profession = str_to_title(str_replace_all(profession, "_", " "))) 
  # Clean the label beforehand


prof_count %>% ggplot(aes(x = n, y = reorder(profession, n))) +
  geom_point() +
  labs(title = "Number of Professions", x = "Number", y = "Professions")


# Distribution of age of all professions
name_age %>% ggplot(aes(x = age)) +
  geom_bar() +
  labs(title = "Age Distribution of All Professions", x = "Age", y = "Number")
  # In a bell shape


# Age distribution of different professions
age_profession <- name_profession %>% 
  left_join(name_age, by = c("nconst", "name")) %>% 
  filter(!is.na(age)) %>% 
  mutate(profession = str_to_title(str_replace_all(profession, "_", " ")))
  
age_by_profession %>% 
  ggplot(aes(x = age)) +
  geom_histogram() +
  facet_wrap(~ profession, scales = "free_y") +
  labs(title = "Age Distribution by Profession", x = "Age", y = "Number")


# Heatmap to display age distribution
# Production designer as an example
age_profession <- name_profession %>% 
  left_join(name_age, by = c("nconst", "name")) %>% 
  filter(!is.na(age)) %>% 
  mutate(profession = str_to_title(str_replace_all(profession, "_", " ")),
         mid = (birth + death)/2)


heatmap_production_designer <- age_profession %>% 
  filter(profession == "Production Designer") %>% 
  ggplot(aes(x = mid, y = age, fill = ..density..)) +
  stat_density_2d(geom = "tile", contour = FALSE) +
  scale_fill_viridis_c(option = "E") +
  labs(title = "Production Designer Age Distribution", 
       x = "Median of Birth & Death", 
       y = "Age")
 
# Save ggplot as a local object
ggsave(plot = heatmap_production_designer, filename = "heatmap_production_designer.png", 
       path = "~/movies_shiny/plot/")


# For loop for creating heatmaps for each profession
for (i in 1:ceiling(length(unique(age_profession$profession))/4)) {
  var <- levels(factor(age_profession$profession))
  ind <- c((4*i-3):(4*i))
  
  gg <- age_profession %>% 
    filter(profession %in% var[ind]) %>% 
    ggplot(aes(x = age, y = mid, ymin = birth, ymax = death)) +
    geom_linerange(position = "jitter", alpha = 0.1) +
    geom_point(color = "blue", alpha = 0.2) +
    facet_wrap(~profession, scales = "free", ncol = 2) +
    labs(title = "Age Lenth Distribution by Profession", x = "Age", y = "Year") +
    coord_flip()
  
  print(gg)
  
  rm(i, var, ind, gg)
}

