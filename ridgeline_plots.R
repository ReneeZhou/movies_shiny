library(tidyverse)
library(ggridges)
library(viridis)


# Genre vs 4 continuous x vars --------------------------------------------
test %>% ggplot(aes(x = release, y = reorder(genre, desc(genre)), fill = genre)) +
  geom_density_ridges(rel_min_height = 0.01) +
  scale_fill_viridis(discrete = TRUE, option = "D")
  
  
test %>% ggplot(aes(x = runtime, y = reorder(genre, desc(genre)), fill = genre)) +
  geom_density_ridges(rel_min_height = 0.01) +
  scale_fill_viridis(discrete = TRUE, option = "D")


test %>% ggplot(aes(x = numberOfVotes, y = reorder(genre, desc(genre)), fill = genre)) +
  geom_density_ridges(rel_min_height = 0.01) +
  scale_fill_viridis(discrete = TRUE, option = "D")


test %>% ggplot(aes(x = rating, y = reorder(genre, desc(genre)), fill = genre)) +
  geom_density_ridges(rel_min_height = 0.01) +
  scale_fill_viridis(discrete = TRUE, option = "D")
 



# Different color scales --------------------------------------------------
test %>% ggplot(aes(x = rating, y = reorder(genre, rating), fill = genre)) +
  geom_density_ridges_gradient(rel_min_height = 0.01) +
  scale_fill_viridis(discrete = TRUE, option = "E")



# Color scale within ..x.. ------------------------------------------------
test %>% 
  ggplot(aes(x = numberOfVotes, y = reorder(genre, desc(genre)), fill = factor(..x..))) +
  geom_density_ridges_gradient(rel_min_height = 0.01) +
  scale_fill_viridis(discrete = TRUE, guide = FALSE, option = "E")

test %>% 
  ggplot(aes(x = runtime, y = reorder(genre, desc(genre)), fill = factor(..x..))) +
  geom_density_ridges_gradient(rel_min_height = 0.01) +
  scale_fill_viridis(discrete = TRUE, guide = FALSE, option = "E")


# *****
# The below are equivelant but if changing guide = TRUE to the 1st one will be a disaster
# since we have too many levels from factor(..x..)
# And here fill is discrete since we factorise ..x..
test %>% 
  ggplot(aes(x = rating, y = reorder(genre, desc(genre)), fill = factor(..x..))) +
  geom_density_ridges_gradient(rel_min_height = 0.01) +
  scale_fill_viridis(discrete = TRUE, guide = FALSE, option = "E")

# Here fill is continuous & we can able legend since fill will be a continuos scale
test %>% 
  ggplot(aes(x = rating, y = reorder(genre, desc(genre)), fill = ..x..)) +
  geom_density_ridges_gradient(rel_min_height = 0.01) +
  scale_fill_viridis(discrete = FALSE, option = "E")
# *****


test %>% 
  ggplot(aes(x = release, y = reorder(genre, desc(genre)), fill = factor(..x..))) +
  geom_density_ridges_gradient(rel_min_height = 0.01) +
  scale_fill_viridis(discrete = TRUE, guide = FALSE, option = "E")




# With quantile lines -----------------------------------------------------
test %>% 
  ggplot(aes(x = rating, y = reorder(genre, desc(genre)), fill = factor(..quantile..))) +
  scale_fill_viridis(discrete = TRUE, option = "E", name = "Quartiles", 
                     labels = c("0%-2.5%", "2.5%-97.5%", "97.5%-100%"), alpha = 0.5) + # guide = "none"/FALSE to disable legend
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, 
                      quantile_lines = FALSE, quantiles = c(0.025, 0.975), 
                      rel_min_height = 0.01, 
                      jittered_points = TRUE, position = "raincloud", point_alpha = 0.8, 
                      point_shape = "|",  point_size = 5, size = 3)


# Using ecdf (empirical cumulative density function): alpha in (scale_)fill doesn't work properly
test %>% 
  ggplot(aes(x = rating, y = reorder(genre, desc(genre)), fill = ..ecdf..)) +
  scale_fill_viridis(discrete = FALSE, option = "E", name = "Quartiles") + # guide = "none"/FALSE to disable legend
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, 
                      quantile_lines = TRUE, 
                      jittered_points = TRUE, 
                      position = position_points_jitter(width = 1, height = 0), 
                      point_alpha = 0.5, point_size = 2,
                      rel_min_height = 0.01)


# Using ecdf (empirical cumulative density function)
test %>% 
  ggplot(aes(x = rating, y = reorder(genre, desc(genre)), fill = 0.5 - abs(0.5 - ..ecdf..))) +
  scale_fill_viridis(discrete = FALSE, option = "E", name = "Quartiles") + # guide = "none"/FALSE to disable legend
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, 
                      quantile_lines = FALSE,  
                      rel_min_height = 0.01, 
                      jittered_points = TRUE, position = "points_sina")

