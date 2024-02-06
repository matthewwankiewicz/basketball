### prop scraping
library(rvest)
library(httr)
library(janitor)

prop_data = read_csv("nba_props.csv")

player_props <- prop_data %>%
  select(-c(grade, money, ...1)) %>% 
  pivot_wider(
    names_from = prop,    # Use the 'prop' column to define new column names
    values_from = value,  # Use the 'value' column for the new columns' values
    names_prefix = ""     # No prefix added to the names, adjust as needed
  ) %>% 
  rename("pts" = points, 
         "reb" = rebounds,
         "ast" = assists,
         "fg3m" = `3fgm`)


## case where using api
player_props <- read_csv("nba_player_props.csv")

player_props <- player_props %>% 
  separate(description, sep = " - ", into = c("full_name", "prop")) %>% 
  filter(prop %in% c("Points", "Assists", "Rebounds", "Made Threes")) %>% 
  select(full_name, "stat_name" = prop, "stat_line" = handicap) %>% 
  mutate(stat_name = 
           case_when(stat_name == "Points" ~ "pts",
                     stat_name == "Assists" ~ "ast",
                     stat_name == "Rebounds" ~ "reb",
                     stat_name == "Made Threes" ~ "fg3m"))

player_props <- player_props %>% 
  distinct(full_name, stat_name, .keep_all = T) %>%
  pivot_wider(names_from = stat_name,
              values_from = stat_line,
              values_fill = list(stat_line = NA))
