# Tidy Tuesday Week 11
# 2020_03_09_submission.R
# 3 - 9 - 2020

# libraries
library(tidyverse)
library(LaCroixColoR)
library(here)


# import data
raw_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')

# glimpse
glimpse(raw_bechdel)
glimpse(movies)

hist(raw_bechdel$year)
hist(movies$year)

movies_df <- movies %>%
  select(clean_test, genre) %>%
  # break genre up
  separate(genre, into = c("genre_1","genre_2","genre_3"), sep = ",") %>%
  mutate(genre_1 = str_trim(genre_1, side = "both"),
         genre_2 = str_trim(genre_2, side = "both"),
         genre_3 = str_trim(genre_3, side = "both")) %>%
  pivot_longer(cols = c(genre_1, genre_2, genre_3)) %>%
  group_by(value, clean_test) %>%
  # calculate number of levels per genre
  summarise(total = n()) %>%
  replace_na(list(total = 0)) %>%
  # remove the NA genre column
  na.omit()

# create plot!
movies_df %>%
  ggplot(aes(x = value, y = total, fill = 
               factor(clean_test, levels = c("nowomen","notalk","men","dubious","ok")) )) +
  geom_col() +
  coord_polar() +
  theme_classic() +
  labs(title = "Passing the Bechdel Test by Genre", fill = "Bechdel Rating") +
  scale_fill_manual(values = lacroix_palette("Pamplemousse",type = "discrete")) +
  ylab("Total Movies") +
  xlab("Genre") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  ggsave(here("plots","2020_03_09.png"), height = 6, width = 6)
  
  


  
