# Packages ----------------------------------------------------------------
library(tidyverse)


# Read in Datasets --------------------------------------------------------
basics <- read_tsv("title_basics.tsv")
ratings <- read_tsv("title_ratings.tsv")
crew <- read_tsv("title_crew.tsv")


# Save & examine parsing failures
unparsed_bit <- map_dfr(list(basics, ratings), problems)


# Tidy Data ---------------------------------------------------------------

# Join 2 datasets together
# Keep only movie titletype
# Tidy up genres
# Keep only numbers in tconst col
# Remove unnecessary columns, endYear is only for TV series
movies_0 <- basics %>% 
  inner_join(ratings, by = "tconst") %>% 
  mutate(runtimeMinutes = as.numeric(runtimeMinutes)) %>% 
  filter(titleType == "movie") %>% 
  separate_rows(genres, sep = ",") %>% 
  select(-titleType, -endYear) 


# Give logical values to genre cols, and spread
# Replace NAs with 0s 
movies <- movies_0 %>% 
  mutate(genre_boolean = rep(1, nrow(movies_0))) %>% 
  spread(genres, genre_boolean, fill = 0, convert = TRUE)


# Change column names
colnames(movies)[9] <- "noGenre"
colnames(movies)[5] <- "release"


# Exploring data ----------------------------------------------------------
# Creating a dataset for exploration with movies uner 210 minutes
movies_exp <- movies %>%
  select(-originalTitle, -isAdult) %>% 
  filter(runtimeMinutes <= 210)
    

  # Most common movie length
plot_runtime <- movies_exp %>% 
  ggplot(aes(x = runtimeMinutes)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(0, 210, 10)) +
  scale_y_continuous(breaks = c(seq(0, 6000, 500), 12500)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3)) +
  labs(title = "Frequent Runtime in Minutes", x = "Runtime", y = "Count") 


  # Movies released per year
plot_release <- movies_exp %>% 
  ggplot(aes(x = release, y = 1)) +
  geom_col() +
  scale_x_continuous(breaks = c(c(1894, 1897, 1906, 1908, 1904, 1899), 
                                #label the 6 years with movie release on the left end
                                seq(1910, 2015, 5))) +
  scale_y_continuous(breaks = seq(0, 5200, 500)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Movie Release per Year", x = "Year", y = "Count") 


  # Most popular genre (col 9:37)
plot_genre <- movies_exp %>% 
  summarise_at(vars(noGenre:Western), funs(sum)) %>% 
  gather(Genre, Count) %>% 
  ggplot(aes(x = reorder(Genre, Count), y = Count)) +
  geom_point(size = 2) +
  scale_y_continuous(breaks = c(seq(0, 20000, 5000), 45000, 77000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Frequency by Genre", x = "Genre", y = "Count") 
  

  # Relationship between numVotes & averageRating
plot_vote_rating <- movies_exp %>% 
  ggplot(aes(x = averageRating, y = numVotes)) +
  geom_point(position = "jitter", colour = "orange",
             size = 2, alpha = 0.15) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(0, 10, 2),
                     labels = seq(0, 10, 2)) +
  scale_y_continuous(breaks = c(seq(0, 1000000, 250000), 1500000, 2000000),
                     labels = c(0, paste0(c(seq(250, 1000, 250), 1500, 2000), "K"))) +
  labs(title = "Number of Votes vs Average Rating", x = "Average Rating", y = "Number of Votes")


# Relationship between numVotes(>=500k) & averageRating
plot_vote_rating_over500k <- movies_exp %>% 
  filter(numVotes >= 500000) %>% 
  ggplot(aes(x = averageRating, y = numVotes)) +
  geom_point(position = "jitter", color = "orange",
             size = 2, alpha = 0.65) +
  scale_x_continuous(breaks = seq(6, 10, 0.5),
                     limits = c(6.5, 9.5)) +
  scale_y_continuous(breaks = seq(500000, 2000000,500000), 
                     labels = c(paste0(seq(500, 2000, 500), "K"))) +
  geom_smooth(se = FALSE) +
  labs(title = "Number of Votes (over 500K) vs Average Rating", x = "Average Rating", y = "Number of Votes")


# Relationship between ratings and director/writers
  # Tidy crew data set
movies_crew <- movies_exp %>% 
  select(tconst, averageRating:numVotes) %>% 
  inner_join(crew, by = "tconst") %>% 
  separate_rows(directors, sep = ",") %>% 
  separate_rows(writers, sep = ",") %>% 
  mutate(directors = str_replace_all(directors, fixed("\\N"), NA_character_), 
         writers = str_replace_all(writers, fixed("\\N"), NA_character_))

  
# Same director/writer tibble
same_director_writer <- movies_crew %>% 
  count(directors, writers, sort = TRUE) %>% 
  # Add a col to indicate if director & writer are identical
  mutate(same = directors == writers) %>% 
  filter(directors != "NA", writers != "NA")
  

plot_same <- same_director_writer %>% 
  ggplot(aes(x = reorder(directors, n), y = n, 
             color = same, group = 1)) +
  geom_point() +
  geom_line() +
  geom_smooth(se = FALSE, color = "grey") +
  scale_color_discrete(guide = guide_legend(reverse = TRUE)) +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(), 
        legend.background = element_blank(), 
        legend.position = "bottom") +
  labs(title = "Distribution", x = "ID", y = "Count",
       color = "Same") 


# Draw a plot to find out the distribution above
same100 <- same_director_writer %>% slice(1:100)
  
plot_temp <- same100 %>% 
  ggplot(aes(x = reorder(directors, n), y = n, 
             color = same, group = 1)) +
  geom_point(size = 3) +
  geom_line() +
  geom_smooth(se = FALSE, color = "grey") +
  scale_color_discrete(guide = guide_legend(reverse = TRUE)) +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(), 
        legend.background = element_blank(), 
        legend.position = "bottom") +
  labs(title = "Distribution", x = "ID", y = "Count",
       color = "Same") 


# Free up work environment ------------------------------------------------
rm(unparsed_bit, basics, crew, ratings, movies_0, same100,
   plot_genre, plot_release, plot_runtime, plot_same,
   plot_temp, plot_vote_rating, plot_vote_rating_over500k)


# Save objects for other .R files -----------------------------------------
save(movies, movies_crew, file = "objs.rda")
