### prop scraping
library(rvest)
library(httr)

prop_data = read_csv("/Users/matthew/Downloads/nba-player-props-rotowire.csv")

prop_data %>% select(1:7) %>% 
  row_to_names(1) %>% 
  mutate_at(.vars = colnames(.)[4:7], as.numeric) %>% 
  janitor::clean_names() %>% 
  rename("fg3m" = "x3pt",
        "Opponent" = "opp") -> player_props

player_props[player_props == "Kelly Oubre"] <- "Kelly Oubre Jr."
player_props[player_props == "Marcus Morris"] <- "Marcus Morris Sr."
