library(zoo)
library(tidyverse)

read_csv('/users/matthew/Downloads/DKSalaries.csv') %>% 
  select(Instructions) %>% 
  slice(-(1:5)) -> nba_lineups

separate(nba_lineups, Instructions, into = paste0("Column", 1:10), sep = ",", extra = "merge") %>% 
  janitor::row_to_names(1) %>% 
  select(1:9) %>% 
  filter(`Roster Position` != 'CPT') -> nba_lineups

### filter for last month and get player averages
gamelogs_ref %>%
  arrange(player_name, desc(game_date)) %>% 
  filter(player_name %in% nba_lineups$Name) %>%
  group_by(player_name) %>%
  filter(game_date == max(game_date),
         rolling_mins >= 15) %>%
  ungroup() -> nba_l30_avgs

nba_lineups %>% 
  left_join(nba_l30_avgs,
            by = c("Name" = "player_name")) %>% 
  mutate(Salary = as.numeric(Salary)) -> nba_lineups


getOpponentTeam <- function(gameInfo, teamAbbrev) {
  # Split the Game Info string to get the part with team abbreviations
  teamsInfo <- unlist(strsplit(gameInfo, " "))[1]
  
  # Split the team info on '@' to get the individual team abbreviations
  teams <- unlist(strsplit(teamsInfo, "@"))
  
  # Identify the opponent team by excluding the team in TeamAbbrev
  opponentTeam <- teams[teams != teamAbbrev]
  
  return(opponentTeam)
}

# Example usage with your dataset
# Assuming your dataframe is named df
nba_lineups$Opponent <- mapply(getOpponentTeam, nba_lineups$`Game Info`, nba_lineups$TeamAbbrev)


### create defense rankings ----
nba_lineups %>% 
  select(Name, Position) -> position_keys
  

nba_lineups %>% 
  separate_rows(Position, sep = "/") %>%
  rename(PlayerPosition = Position) -> nba_lineups

gamelogs %>%
  filter(min > 10) %>% 
  separate_rows(pos, sep = ,) %>% 
  group_by(game_date, opp_team, pos) %>% 
  summarise(dfs_points = mean(dfs_points),
            mins = mean(min),
            ppm = dfs_points/mins,
            pts = mean(pts),
            ast = mean(ast),
            reb = mean(reb),
            fg3m = mean(fg3m)) %>%
  arrange(game_date) %>%
  group_by(opp_team, pos) %>%
  mutate(
    rolling_dfs = rollapply(dfs_points, 5, mean, fill = NA, align = "right"),
    rolling_pts = rollapply(pts, 5, mean, fill = NA, align = "right"),
    rolling_ast = rollapply(ast, 5, mean, fill = NA, align = "right"),
    rolling_reb = rollapply(reb, 5, mean, fill = NA, align = "right"),
    rolling_fg3m = rollapply(fg3m, 5, mean, fill = NA, align = "right"),
    rolling_mins = rollapply(mins, 5, mean, fill = NA, align = "right")  # Replace 'mins' with your actual minutes column
  ) %>%
  filter(game_date == max(game_date, na.rm = TRUE)) %>%
  ungroup() %>%
  rename("team" = opp_team) %>%
  group_by(pos) %>% 
  mutate(
    rolling_dfs = as.vector(scale(rolling_dfs, center = TRUE, scale = TRUE)),
    rolling_mins = as.vector(scale(rolling_mins, center = TRUE, scale = TRUE)),
    rolling_pts = as.vector(scale(rolling_pts, center = TRUE, scale = TRUE)),
    rolling_ast = as.vector(scale(rolling_ast, center = TRUE, scale = TRUE)),
    rolling_reb = as.vector(scale(rolling_reb, center = TRUE, scale = TRUE)),
    rolling_fg3m = as.vector(scale(rolling_fg3m, center = TRUE, scale = TRUE))
  ) %>%
  select(team, pos, "opp_rolling_dfs" = rolling_dfs, "opp_rolling_mins" = rolling_mins, "opp_rolling_pts" = rolling_pts,
         "opp_rolling_ast" =  rolling_ast, "opp_rolling_reb" = rolling_reb, 
         "opp_rolling_fg3m" = rolling_fg3m) %>%
  drop_na() -> team_def_rankings

# 2. Merge with Defense Rankings
merged_data <- nba_lineups %>%
  left_join(team_def_rankings, by = c("Opponent" = "team", "PlayerPosition" = "pos"))

merged_data %>% 
  left_join(player_props %>% 
              select(player, "pts_line" = pts),
            by = c('Name' = 'player')) -> merged_data


#### take lowest value (some players have multiple positions - base results on toughest matchup)
final_data <- merged_data %>% group_by(Name) %>% filter(game_date == max(game_date)) %>% slice(1)


## make predictions
final_data$est_dfs <- predict(dfs_model, newdata = final_data %>% rename("rolling_value" = opp_rolling_dfs,
                                                                         "stat_max_l5" = rolling_max_dfs,
                                                                         "stat_min_l5" = rolling_min_dfs))

final_data$est_pts <- predict(pts_model, newdata = final_data %>% rename("rolling_value" = opp_rolling_pts,
                                                                         "stat_max_l5" = rolling_max_pts,
                                                                         "stat_min_l5" = rolling_min_pts))

final_data$est_ast <- predict(ast_model, newdata = final_data %>% rename("rolling_value" = opp_rolling_ast,
                                                                         "stat_max_l5" = rolling_max_ast,
                                                                         "stat_min_l5" = rolling_min_ast))

final_data$est_reb <- predict(reb_model, newdata = final_data %>% rename("rolling_value" = opp_rolling_reb,
                                                                         "stat_max_l5" = rolling_max_reb,
                                                                         "stat_min_l5" = rolling_min_reb))

final_data$est_fg3m <- predict(fg3m_model, newdata = final_data %>% rename("rolling_value" = opp_rolling_fg3m,
                                                                           "stat_max_l5" = rolling_max_fg3m,
                                                                           "stat_min_l5" = rolling_min_fg3m))


write_csv(final_data, "dk_lines.csv")



# 
# 
# 
# 
# #### owners box method ------
# ob_lineup <- read_csv("/Users/matthew/Downloads/$750-Layup-[NO-RAKE]-Player-Slate (1).csv") %>% 
#   select(Instructions) %>% 
#   slice(-(1:5))
# 
# separate(ob_lineup, Instructions, into = paste0("Column", 1:14), sep = ",", extra = "merge") %>% 
#   janitor::row_to_names(1) %>% 
#   select(1:14) -> ob_lineup
# 
# 
# gamelogs %>% 
#   filter(player_name %in% nba_lineups$Name & game_date >= Sys.Date()-30) %>%
#   group_by(player_name) %>% 
#   summarise(avg_pts = mean(ob_points),
#             mins = mean(min),
#             ppm = avg_pts/mins,
#             sd_pts = sd(ob_points),
#             player_rate = abs(avg_pts/sd_pts)) -> nba_l30_avgs
# 
# ob_lineup %>% 
#   left_join(nba_l30_avgs,
#             by = c("Name" = "player_name")) %>% 
#   mutate(Salary = as.numeric(Salary)) -> ob_lineup
# 
# gamelogs %>% 
#   filter(min >= 10) %>% 
#   separate_rows(pos, sep = ',') %>% 
#   group_by(opp_team, pos) %>% 
#   summarise(pts = mean(pts),
#             ast = mean(ast),
#             reb = mean(reb),
#             stl = mean(stl),
#             blk = mean(blk),
#             ob_points = mean(ob_points)) %>% 
#   rename("team" = opp_team) %>% 
#   mutate_if(is.numeric, function(x){round(x, 1)}) -> team_def_rankings
# 
# ob_lineup[ob_lineup == "NY"] <- "NYK"
# 
# merged_data <- ob_lineup %>%
#   left_join(team_def_rankings, by = c("Opponent" = "team", "Roster Position" = "pos"))
# 
# 
# merged_data %>% 
#   mutate(ob_pos = ifelse(`Roster Position` %in% c("C", "PF", "SF"), "FC", "G")) -> merged_data
# 
# merged_data %>% 
#   separate_rows(Position, sep = "/") %>% 
#   filter(Position != "SFLEX") -> merged_data
# 
# 
# write_csv(merged_data, "owners_box.csv")



