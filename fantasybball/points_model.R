#### create model to predict fantasy points #####
library(tidyverse)
library(lme4)

## load in data
data <- fread("fantasybball/fantasybball/daily_app_data.csv")
games <- fread("fantasybball/fantasybball/games2024.csv") %>% 
  clean_names() %>% 
  filter(grepl('vs.', matchup)) %>% 
  separate(matchup, into = c("home", "away"), sep = " vs. ") %>% 
  data.table()


## merge in opponent team into gamelogs
data <- data %>% 
  merge(games[, .(game_id, home, away)]) %>% 
  mutate(opponent_team = ifelse(home == team_abbreviation, away, home))


### have players in positional clusters but also opponent team clusters
##### - different teams will have different levels of defense

## filter for players with ten or more games with 15+ mins
data <- data %>%
  group_by(player_name) %>% 
  filter(sum(min >= 15, na.rm=T) >= 1) %>%
  ungroup() %>% 
  filter(!is.na(position))


### impute negatives to 0
data <- data %>%
  mutate(fan_pts = ifelse(fan_pts < 0, 0, fan_pts))


### create model
model <- glmer(fan_pts ~ (1|player_name) + (1|opponent_team:position), 
              data = data %>% filter(game_date>= Sys.Date()-25&position!=""&!is.na(position)), family = poisson())

sjPlot::tab_model(model)


## pull out intercepts for each player and team
intercepts <- ranef(model)

intercepts$`opponent_team:position` %>% View

player_intercepts <- intercepts$player_name %>% 
  rownames_to_column() %>% 
  rename("player_name" = rowname)

## filter for free agents
free_agents <- data %>% filter(is_free_agent==T) %>% pull(player_name) %>% unique()


## filter intercept for free agents
player_intercepts %>% 
  filter(player_name%in% free_agents) %>% 
  arrange(desc(`(Intercept)`))



#### NEXT:
### - get player's future schedules and make predictions
## get most recent team and position
position_data <- data %>% 
  group_by(player_name) %>% 
  filter(game_date == max(game_date)) %>% 
  select(player_name, position, "team" = team_abbreviation, is_free_agent)


## read in schedule
schedule <- fread("~/Documents/basketball/fantasybball/filtered_schedule.csv")

## select date, home, away
nba_schedule <- schedule %>% 
  select("game_date" = GameDate, "opponent_team" = "Visitor",
         "team" = "Home")

## flip home and away rbind
nba_schedule_final <- rbind(nba_schedule, 
                            nba_schedule %>% 
                              rename("team" = "opponent_team",
                                     "opponent_team" = "team"))

matchups <- nba_schedule_final %>% 
  filter(game_date==Sys.Date() & player_name %in% player_intercepts$player_name) %>% 
  merge(position_data, by = "team") %>% 
  filter(player_name %in% player_intercepts$player_name)

matchups$estimate <- predict(model, newdata = matchups, type = "response", allow.new.levels=T)


