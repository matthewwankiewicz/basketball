#### create model to predict fantasy points #####
library(tidyverse)
library(lme4)
library(data.table)
library(janitor)

## load in data
data <- fread("fantasybball/fantasybball/daily_app_data.csv")
games <- fread("fantasybball/fantasybball/games2024.csv") %>% 
  clean_names() %>% 
  filter(grepl('vs.', matchup)) %>% 
  separate(matchup, into = c("home", "away"), sep = " vs. ") %>% 
  data.table() 

data <- data %>% 
  merge(games[, .(game_id, home, away)]) %>% 
  mutate(opponent_team = ifelse(home == team_abbreviation, away, home)) %>%
  group_by(player_name) %>% 
  filter(sum(min >= 15, na.rm=T) >= 1) %>%
  ungroup() %>% 
  filter(!is.na(position)) %>% 
  mutate(ppm = fan_pts / min) %>%
  mutate(fan_pts = pmax(fan_pts, 0), 
         ppm = pmax(ppm, 0))


data$scaled_min <- scale(data$rolling_min)

### create model
model <- glmer(fan_pts ~ (1|opponent_team:position) + (1|player_name), 
              data = data %>% filter(game_date>= Sys.Date()-15&position!=""&!is.na(position)), 
              family = poisson())

sjPlot::tab_model(model)

## save model
write_rds(model, "fantasybball/fantasy_points_model.rds")

## pull out intercepts for each player and team
intercepts <- ranef(model)

player_intercepts <- intercepts$player_name %>% 
  rownames_to_column() %>% 
  rename("player_name" = rowname)

## filter for free agents
free_agents <- data %>% filter(is_free_agent==T) %>% pull(player_name) %>% unique()


## filter intercept for free agents
player_intercepts %>% 
  filter(player_name%in% free_agents) %>% 
  arrange(desc(`(Intercept)`))


### get most recent ppm for each player
ppm_stats <- data %>% 
  group_by(player_name) %>% 
  filter(game_date == max(game_date)) %>% 
  select(player_name, rolling_min)


#### NEXT:
### - get player's future schedules and make predictions
## get most recent team and position
position_data <- fread("~/Documents/basketball/fantasybball/fantasybball/player_info.csv")

position_data <- position_data %>% 
  merge(ppm_stats, by = "player_name")


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
  filter(game_date>=Sys.Date() & game_date <= "2025-01-05") %>% 
  merge(position_data, by.x = "team", by.y = "pro_team", allow.cartesian = T) %>% 
  filter(player_name %in% player_intercepts$player_name)

matchups$estimate <- predict(model, newdata = matchups, type = "response")


matchups[injury_status!='OUT', .(sum(estimate)), fantasy_team][order(-V1)]

matchups %>% filter(game_date==Sys.Date()) %>% 
  group_by(player_name, is_free_agent) %>% 
  summarise(proj_pts = sum(estimate),
            rolling_min = rolling_min) %>%
  unique() %>% 
  filter(is_free_agent==T) %>% 
  ggplot(aes(rolling_min, proj_pts)) +
  geom_point()


matchups %>% filter(game_date==Sys.Date() & injury_status!='OUT') %>% View


### use pull weekly schedule function to help set up report
schedule <- pull_weekly_schedule(schedule, position_data, start = "2024-12-30", end = "2025-01-05",
                     teams = c("Team Wankiewicz"))

## get estimates
schedule$estimate <- predict(model, newdata = schedule, type = "response")


### get minutes and points estimates
schedule_mins_final <- calculate_weekly_totals(schedule, "rolling_min")

schedule_pts_final <- calculate_weekly_totals(schedule, "estimate")


### put into report