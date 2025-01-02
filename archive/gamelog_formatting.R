## script to collect position data for nba players
### libraries ####
library(tidyverse)
library(janitor)
library(rvest)
library(zoo)
library(data.table)

### Working on gamelogs ####

library(tidyverse)
library(rvest)
library(janitor)


gamelogs <- read_csv("fantasybball/gamelogs2024.csv")
gamelogs$GAME_ID <- ifelse(nchar(gamelogs$GAME_ID) == 8, paste0("00", gamelogs$GAME_ID),
                           ifelse(nchar(gamelogs$GAME_ID) == 9, paste0("0", gamelogs$GAME_ID),
                                  gamelogs$GAME_ID))
nba_games <- read_csv("fantasybball/games2024.csv")
positions <- read_csv("fantasybball/nba_active_player_positions.csv") %>% 
  clean_names()

opp_data <- nba_games %>%
  select(-starts_with("opp_")) %>%
  rename_at(.vars = c(9:28), ~ paste0("opp_", .))


nba_games_opponent <- nba_games %>%
  select(-starts_with("opp_")) %>%  # Remove existing opp_ columns if any
  rename_with(~ paste("opp_", ., sep = ""),
              -c(GAME_ID, MATCHUP, TEAM_ID, TEAM_NAME, TEAM_ABBREVIATION)) %>% 
  select(-c(opp_SEASON_ID, TEAM_NAME, opp_GAME_DATE,
            opp_WL, MATCHUP, TEAM_ID))

# Merge the original data with the opponent's data based on GAME_ID and different team names
merged_nba_games <- merge(
  nba_games,
  nba_games_opponent,
  by = c("GAME_ID"),
  suffixes = c("_team", "_opp"),
  incomparables = NA,  # Handle non-matching teams by inserting NA values
  allow.cartesian = TRUE  # Allow the creation of a Cartesian product
)

# Filter rows where team names are different
nba_games <- merged_nba_games %>% 
  filter(TEAM_ABBREVIATION_team != TEAM_ABBREVIATION_opp) %>% 
  data.table::data.table()



nba_teams <- nba_games %>% 
  # filter(SEASON_ID == 22024,
  #        TEAM_ID > 100) %>% 
  pull(TEAM_ID) %>% unique()

## merge gamelogs with nba games
gamelogs <- gamelogs %>% 
  merge(nba_games %>% unique(),
        by = c("TEAM_ID", "GAME_ID")
)


## remove ".x" from stats, change column with ".y" to "team_[COLNAME}"]
gamelogs <- gamelogs %>% 
  rename_at(vars(contains(".x")), ~ str_replace_all(., "\\.x$", "")) %>%
  rename_at(vars(contains(".y")), ~ paste0("team_", str_replace_all(., "\\.y$", "")))


gamelogs %>% 
  clean_names() %>%
  rename("team_to" = "tov") %>%
  filter(game_date >= as.Date("2023-10-22"),
         team_id %in% nba_teams) %>%
  select(game_date, game_id, matchup, player_name, player_id,
         min, fgm, fga, fg_pct, fg3m, fg3a, fg3_pct, ftm, fta, ft_pct,
         oreb, dreb, reb, ast, stl, blk, to, pf, pts, plus_minus,
         team_min, team_fgm, team_reb, opp_reb, team_fta,
         team_to, team_fga) %>%
  filter(!is.na(min)) %>% 
  mutate(fan_pts = 2*fgm - fga + ftm - fta + fg3m + 2*reb + 2*ast +
           3*stl + 3*blk - 2*to + pts,
         ftr = fta/fga,
         team_min = team_min/5,
         min = parse_number(min),
         ast_rate = (100*ast)/(((min/(team_min /5))*team_fgm)-fgm),
         reb_rate = (100*reb*team_min)/(min*(team_reb+opp_reb)),
         usage_percentage = ((fga + 0.44 * fta + to) * (team_min)) / (min * (team_fga + 0.44 * team_fta + team_to)),
         ts_percentage = pts/(2*(fga+0.44*fta)),
         double_double = ifelse(pts > 10 & (ast > 10 | reb > 10 | blk > 10 | stl > 10) |
                                  ast > 10 & (reb > 10 | blk > 10 | stl > 10) |
                                  reb > 10 & (blk > 10 | stl > 10) |
                                  blk > 10 & stl > 10, 1, 0),
         triple_double = ifelse(pts > 10 & ast > 10 & (reb > 10 | blk > 10 | stl > 10) |
                                  pts > 10 & reb > 10 & (ast > 10 | blk > 10 | stl > 10) |
                                  pts > 10 & blk > 10 & (ast > 10 | reb > 10 | stl > 10) |
                                  pts > 10 & stl > 10 & (ast > 10 | reb > 10 | blk > 10) |
                                  ast > 10 & reb > 10 & (pts > 10 | blk > 10 | stl > 10) |
                                  ast > 10 & blk > 10 & (pts > 10 | reb > 10 | stl > 10) |
                                  ast > 10 & stl > 10 & (pts > 10 | reb > 10 | blk > 10) |
                                  reb > 10 & blk > 10 & (pts > 10 | ast > 10 | stl > 10) |
                                  reb > 10 & stl > 10 & (pts > 10 | ast > 10 | blk > 10) |
                                  blk > 10 & stl > 10 & (pts > 10 | ast > 10 | reb > 10), 1, 0),
         ppm = fan_pts/min,
         fg3_rate = fg3a/fga) %>% 
  separate(matchup, into = c("team1", "team2"),
           sep = " vs. | @ ") -> gamelogs


## add in positions
gamelogs <- gamelogs %>% 
  merge(positions, by = "player_name")


gamelogs %>%
  group_by(player_name) %>%
  arrange(game_date) %>%
  mutate(
    rolling_pts = rollapply(pts, 4, mean, fill = NA, align = "right"),
    rolling_mins = rollapply(min, 4, mean, fill = NA, align = "right"),
    rolling_ast = rollapply(ast, 4, mean, fill = NA, align = "right"),
    rolling_reb = rollapply(reb, 4, mean, fill = NA, align = "right"),
    rolling_fg3m = rollapply(fg3m, 4, mean, fill = NA, align = "right"),
    rolling_dfs = rollapply(fan_pts, 4, mean, fill = NA, align = "right"), 
    rolling_usg = rollapply(usage_percentage, 4, mean, fill = NA, align = "right"),
    ppm = rolling_pts / rolling_mins,
    sd_pts = rollapply(fan_pts, width = 4, FUN = sd, align = "right", fill = NA),
    player_rate = abs(rolling_pts / sd_pts),
    rolling_max_pts = rollapply(pts, 5, max, fill = NA, align = "right"),
    rolling_max_ast = rollapply(ast, 5, max, fill = NA, align = "right"),
    rolling_max_fg3m = rollapply(fg3m, 5, max, fill = NA, align = "right"),
    rolling_max_reb = rollapply(reb, 5, max, fill = NA, align = "right"),
    rolling_max_dfs = rollapply(fan_pts, 5, max, fill = NA, align = "right"),
    rolling_min_dfs = rollapply(fan_pts, 5, min, fill = NA, align = "right"),
    rolling_min_pts = rollapply(pts, 5, min, fill = NA, align = "right"),
    rolling_min_ast = rollapply(ast, 5, min, fill = NA, align = "right"),
    rolling_min_reb = rollapply(reb, 5, min, fill = NA, align = "right"),
    rolling_min_fg3m = rollapply(fg3m, 5, min, fill = NA, align = "right"),
    rolling_sd_dfs = rollapply(fan_pts, 5, sd, fill = NA, align = "right"),
    rolling_sd_pts = rollapply(pts, 5, sd, fill = NA, align = "right"),
    rolling_sd_ast = rollapply(ast, 5, sd, fill = NA, align = "right"),
    rolling_sd_reb = rollapply(reb, 5, sd, fill = NA, align = "right"),
    rolling_sd_fg3m = rollapply(fg3m, 5, sd, fill = NA, align = "right"),
    rolling_fg3_rate = rollapply(fg3_rate, 4, mean, fill = NA, alight = "right"),
    rolling_max_fg3_rate = rollapply(fg3_rate, 5, max, fill = NA, alight = "right"),
    rolling_min_fg3_rate = rollapply(fg3_rate, 5, min, fill = NA, alight = "right")
  ) %>%
  select(game_date, player_name, player_id, rolling_mins, min, "pos" = position,
         pts, ast, reb, fg3m, ftr, reb_rate, ast_rate, fg3_rate, usage_percentage, 
         rolling_pts, rolling_usg, rolling_ast,
         rolling_reb, rolling_fg3m, rolling_dfs, fan_pts,
         rolling_max_pts, rolling_min_pts, rolling_max_ast, rolling_min_ast,
         rolling_max_dfs, rolling_min_dfs, rolling_max_reb, rolling_min_reb,
         rolling_max_fg3m, rolling_min_fg3m,
         rolling_sd_dfs, rolling_sd_pts, rolling_sd_ast, rolling_sd_reb,
         rolling_sd_fg3m, rolling_fg3_rate, rolling_max_fg3_rate, rolling_min_fg3_rate) %>% 
  group_by(game_date, player_name) %>% 
  slice(1) %>% 
  arrange(desc(game_date), player_name) %>% 
  group_by(player_name) %>% 
  mutate(current_pts = lead(rolling_pts, 1),
         current_usg = lead(rolling_usg, 1),
         current_ast = lead(rolling_ast, 1),
         current_reb = lead(rolling_reb, 1),
         current_3s = lead(rolling_fg3m, 1),
         current_dfs = lead(rolling_dfs, 1),
         current_mins = lead(rolling_mins, 1),
         current_fg3_rate = lead(rolling_fg3_rate, 1),
         prev_game_date = lead(game_date, 1),
         ppm = current_pts/current_mins,
         days_rest = as.numeric(game_date - prev_game_date) - 1) %>% 
  ungroup() -> gamelogs_ref



gamelogs_ref %>% 
  fwrite("fantasybball/daily_app_data.csv")
