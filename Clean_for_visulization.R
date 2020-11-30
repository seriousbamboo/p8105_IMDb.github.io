### Include libraries
library(tidyverse)
library(stringr)
library(xml2)
library(rvest)
### Import data
imdb_raw = read_csv("./movie_metadata.csv")

### Get the year month date:

get_ymd <- function(url_raw)
{
  url = str_replace(url_raw,"\\?ref","releaseinfo?ref")
  swm_html = read_html(url)
  ymd = 
    swm_html %>% 
    html_nodes(css = ".release-date-item:nth-child(1) .release-date-item__date") %>% 
    html_text()
  return(ymd)
}

#imdb = 
#  imdb_raw %>% 
#  mutate(ymd = map(movie_imdb_link, get_ymd)) %>% 
#  unnest(ymd)

### Cleaning
imdb_clean = 
  imdb_raw %>%
  drop_na(movie_title, gross) %>% 
  mutate(movie_title = str_replace(movie_title,"\\?$","")) %>% 
  separate(genres,
           sep = "\\|",
           into = c("g1","g2","g3","g4","g5","g6","g7","g8")) %>% 
  pivot_longer(g1:g8,
               names_to = "dummy",
               values_to = "genres") %>%
  select(-dummy) %>% 
  drop_na(genres) %>% 
  separate(plot_keywords, 
           sep = "\\|",
           into = c("g1","g2","g3","g4","g5","g6","g7","g8")) %>% 
  pivot_longer(g1:g8,
               names_to = "dummy",
               values_to = "plot_keyword") %>%
  select(-dummy) %>% 
  drop_na(plot_keyword)  
imdb_clean %>% 
  write_csv(.,"./imdb_explore_clean.csv")

### Get the date of onboarding:




