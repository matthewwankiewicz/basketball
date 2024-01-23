## script to collect position data for nba players
### libraries ####
library(tidyverse)
library(janitor)
library(rvest)

link <- "https://www.fantasypros.com/nba/rankings/overall-points-espn.php"
page <- read_html(link)

page %>% 
  html_table() %>% 
  .[[1]] %>% 
  clean_names() %>% 
  select(player) -> data

data %>% 
  separate(player, sep = "\\(", into = c("player", "team_pos")) %>% 
  mutate(team_pos = sub("\\).*", "", team_pos)) %>% 
  separate(team_pos, sep = "-", into = c("team", "pos")) %>% 
  mutate(player = sub(" Jr.", "", player)) %>% 
  mutate(player = str_squish(player),
         team = str_squish(team),
         pos = str_squish(pos)) -> player_data

player_data[player_data == "Jabari Smith"] <- "Jabari Smith Jr."
player_data[player_data == "Michael Porter"] <- "Michael Porter Jr."
player_data[player_data == "Jaren Jackson"] <- "Jaren Jackson Jr."
player_data[player_data == "Kelly Oubre"] <- "Kelly Oubre Jr."
player_data[player_data == "Gary Trent"] <- "Gary Trent Jr."
player_data[player_data == "Tim Hardaway"] <- "Tim Hardaway Jr."
player_data[player_data == "Dennis Smith"] <- "Dennis Smith Jr."
player_data[player_data == "Larry Nance"] <- "Larry Nance Jr."
player_data[player_data == "Jaime Jaquez"] <- "Jaime Jaquez Jr."
player_data[player_data == "Kevin Porter"] <- "Kevin Porter Jr."
player_data[player_data == "Russell Westbrook III"] <- "Russell Westbrook"

### Working on gamelogs ####

library(tidyverse)
library(rvest)
library(janitor)


gamelogs <- read_csv("gamelogs2023.csv")
gamelogs$GAME_ID <- ifelse(nchar(gamelogs$GAME_ID) == 8, paste0("00", gamelogs$GAME_ID),
                           ifelse(nchar(gamelogs$GAME_ID) == 9, paste0("0", gamelogs$GAME_ID),
                                  gamelogs$GAME_ID))
nba_games <- read_csv("games2023.csv")


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
  suffixes = c("", "_opp"),
  incomparables = NA,  # Handle non-matching teams by inserting NA values
  allow.cartesian = TRUE  # Allow the creation of a Cartesian product
)

# Filter rows where team names are different
nba_games <- merged_nba_games %>% 
  filter(TEAM_ABBREVIATION != TEAM_ABBREVIATION_opp)



nba_teams <- nba_games %>% 
  filter(SEASON_ID == 22023,
         TEAM_ID > 100) %>% 
  pull(TEAM_ID) %>% unique()

gamelogs %>% 
  merge(nba_games %>% 
          filter(WL == 'W') %>% 
          select(GAME_ID, GAME_DATE, MATCHUP, "TEAM_MIN" = MIN,
                 "TEAM_FGM" = FGM, "TEAM_REB" = REB, opp_REB,
                 "TEAM_FGA" = FGA, "TEAM_FTA" = FTA, "TEAM_TO" = TOV),
        by = "GAME_ID") -> gamelogs

gamelogs %>% 
  clean_names() %>% 
  filter(game_date >= as.Date("2023-10-24"),
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
         dfs_points = pts + 0.5*fg3m + 1.25*reb + 1.5*ast +
           2*stl + 2*blk - 0.5*to + 1.5*double_double + 3*triple_double,
         ob_points = pts + 1.2*reb + 1.5*ast +
           2.5*stl + 3*blk - 1*to,
         ppm = dfs_points/min) %>% 
  separate(matchup, into = c("team1", "team2"),
           sep = " vs. | @ ") -> gamelogs



merge(gamelogs, 
      player_data,
      by.x = "player_name",
      by.y = "player",
      all.x = T) -> gamelogs

## add in opponent
gamelogs %>% 
  mutate(opp_team = ifelse(team == team1, team2, team1)) %>% 
  select(-c(team1, team2)) %>% 
  distinct(player_name, game_date, game_id,
           .keep_all = T) -> gamelogs

gamelogs %>% 
  write_csv("fantasybball/gamelogs2023.csv")

gamelogs %>% 
  separate_rows(pos, sep = ",") %>% 
  group_by(player_name, game_date) %>% 
  slice(1) %>% 
  write_csv("thedaily/gamelogs2023.csv")


gamelogs %>%
  group_by(player_name) %>%
  arrange(game_date) %>%
  mutate(
    rolling_pts = rollapply(pts, 4, mean, fill = NA, align = "right"),
    rolling_mins = rollapply(min, 4, mean, fill = NA, align = "right"),
    rolling_ast = rollapply(ast, 4, mean, fill = NA, align = "right"),
    rolling_reb = rollapply(reb, 4, mean, fill = NA, align = "right"),
    rolling_fg3m = rollapply(fg3m, 4, mean, fill = NA, align = "right"),
    rolling_dfs = rollapply(dfs_points, 4, mean, fill = NA, align = "right"), 
    rolling_usg = rollapply(usage_percentage, 4, mean, fill = NA, align = "right"),
    ppm = rolling_pts / rolling_mins,
    sd_pts = rollapply(dfs_points, width = 4, FUN = sd, align = "right", fill = NA),
    player_rate = abs(rolling_pts / sd_pts),
    rolling_max_pts = rollapply(pts, 5, max, fill = NA, align = "right"),
    rolling_max_ast = rollapply(ast, 5, max, fill = NA, align = "right"),
    rolling_max_fg3m = rollapply(fg3m, 5, max, fill = NA, align = "right"),
    rolling_max_reb = rollapply(reb, 5, max, fill = NA, align = "right"),
    rolling_max_dfs = rollapply(dfs_points, 5, max, fill = NA, align = "right"),
    rolling_min_dfs = rollapply(dfs_points, 5, min, fill = NA, align = "right"),
    rolling_min_pts = rollapply(pts, 5, min, fill = NA, align = "right"),
    rolling_min_ast = rollapply(ast, 5, min, fill = NA, align = "right"),
    rolling_min_reb = rollapply(reb, 5, min, fill = NA, align = "right"),
    rolling_min_fg3m = rollapply(fg3m, 5, min, fill = NA, align = "right"),
    rolling_sd_dfs = rollapply(dfs_points, 5, sd, fill = NA, align = "right"),
    rolling_sd_pts = rollapply(pts, 5, sd, fill = NA, align = "right"),
    rolling_sd_ast = rollapply(ast, 5, sd, fill = NA, align = "right"),
    rolling_sd_reb = rollapply(reb, 5, sd, fill = NA, align = "right"),
    rolling_sd_fg3m = rollapply(fg3m, 5, sd, fill = NA, align = "right")
  ) %>%
  select(game_date, player_name, pos, opp_team, rolling_mins, min,
         pts, ast, reb, fg3m, usage_percentage,
         rolling_pts, rolling_usg, rolling_ast,
         rolling_reb, rolling_fg3m, rolling_dfs, dfs_points,
         rolling_max_pts, rolling_min_pts, rolling_max_ast, rolling_min_ast,
         rolling_max_dfs, rolling_min_dfs, rolling_max_reb, rolling_min_reb,
         rolling_max_fg3m, rolling_min_fg3m,
         rolling_sd_dfs, rolling_sd_pts, rolling_sd_ast, rolling_sd_reb,
         rolling_sd_fg3m) %>% 
  separate_rows(pos, sep = ",") %>% 
  group_by(game_date, player_name) %>% 
  slice(1) %>% 
  arrange(desc(game_date), player_name) %>% 
  group_by(player_name) %>% 
  mutate(current_pts = lead(rolling_pts, 1),
         current_usg = lead(rolling_usg, 1),
         current_ast = lead(rolling_ast, 1),
         current_reb = lead(rolling_reb, 1),
         current_3s = lead(rolling_fg3m, 1),
         current_dfs = lead(rolling_dfs, 1)) %>% 
  ungroup() -> gamelogs_ref
