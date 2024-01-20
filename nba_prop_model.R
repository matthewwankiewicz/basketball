create_matchup_table <- \(player){
  player_table <- gamelogs %>%
    filter(player_name == player) %>%
    group_by(player_name) %>%
    arrange(game_date) %>%
    mutate(
      rolling_pts = rollapply(pts, 4, mean, fill = NA, align = "right"),
      rolling_mins = rollapply(min, 4, mean, fill = NA, align = "right"),
      rolling_ast = rollapply(ast, 4, mean, fill = NA, align = "right"),
      rolling_reb = rollapply(reb, 4, mean, fill = NA, align = "right"),
      rolling_3s = rollapply(fg3m, 4, mean, fill = NA, align = "right"),
      rolling_dfs = rollapply(dfs_points, 4, mean, fill = NA, align = "right"), 
      rolling_usg = rollapply(usage_percentage, 4, mean, fill = NA, align = "right"),
      ppm = rolling_pts / rolling_mins,
      sd_pts = rollapply(dfs_points, width = 4, FUN = sd, align = "right", fill = NA),
      player_rate = abs(rolling_pts / sd_pts)
    ) %>%
    filter(player_name == player) %>% 
    select(game_date, player_name, pos, opp_team, rolling_mins, pts, ast, reb, fg3m,
           rolling_pts, rolling_usg, rolling_ast,
           rolling_reb, rolling_3s, rolling_dfs, dfs_points) %>% 
    separate_rows(pos, sep = ",") %>% 
    group_by(game_date, player_name) %>% 
    slice(1)
  
  
  team_table <- gamelogs %>%
    filter(min > 15) %>%
    separate_rows(pos, sep = ",") %>%
    group_by(game_date, opp_team, pos) %>%
    summarise(dfs_points = mean(dfs_points),
              mins = mean(min),
              ppm = dfs_points/mins,
              pts = mean(pts),
              ast = mean(ast),
              reb = mean(reb),
              fg3m = mean(fg3m),
              .groups = 'drop') %>%
    arrange(game_date) %>% 
    group_by(opp_team, pos) %>%
    mutate(
      rolling_dfs = rollapply(dfs_points, 5, mean, fill = NA, align = "right"),
      rolling_pts = as.vector(rollapply(pts, 5, mean, fill = NA, align = "right")),
      rolling_ast = rollapply(ast, 5, mean, fill = NA, align = "right"),
      rolling_reb = rollapply(reb, 5, mean, fill = NA, align = "right"),
      rolling_fg3m = rollapply(fg3m, 5, mean, fill = NA, align = "right"),
      rolling_mins = rollapply(mins, 5, mean, fill = NA, align = "right")) %>%
    ungroup() %>%
    group_by(opp_team) %>%
    mutate(
      scaled_rolling_pts = scale(rolling_pts),
      scaled_rolling_ast = scale(rolling_ast),
      scaled_rolling_reb = scale(rolling_reb),
      scaled_rolling_fg3m = scale(rolling_fg3m),
      scaled_rolling_mins = scale(rolling_mins),
      scaled_rolling_dfs = scale(rolling_dfs)
    ) %>%
    select(game_date, opp_team, pos, scaled_rolling_pts, scaled_rolling_ast, scaled_rolling_reb,
           scaled_rolling_fg3m, scaled_rolling_mins, scaled_rolling_dfs)
  
  
  
  ## merge tables
  
  player_table %>% 
    left_join(team_table,
              by = c("pos", "game_date", "opp_team")) %>% 
    arrange(desc(game_date)) -> player_table
  
  player_table$current_pts <- lead(player_table$rolling_pts, 1)
  player_table$current_usg <- lead(player_table$rolling_usg, 1)
  player_table$current_ast <- lead(player_table$rolling_ast, 1)
  player_table$current_reb <- lead(player_table$rolling_reb, 1)
  player_table$current_3s <- lead(player_table$rolling_3s, 1)
  player_table$current_dfs <- lead(player_table$rolling_dfs, 1)
  
  return(player_table)
  
}

totals <- list()
player_list <- unique(gamelogs$player_name)
for(i in 1:length(player_list)){
  player = player_list[i]

  player_matchups <- create_matchup_table(player)
  
  totals[[i]] <- player_matchups
}


totals_full <- do.call(rbind, totals)

totals_full %>% 
  arrange(player_name, desc(game_date)) %>% 
  group_by(player_name) %>% 
  mutate(prev_game_date = lead(game_date, 1),
         days_rest = as.numeric(game_date - prev_game_date) - 1) -> totals_full

find_best_matcups <- \(table){
  table %>% 
    filter(rolling_pts >= 1) %>% 
    select(Name, Opponent, "rolling_value" = rolling_pts) %>% 
    left_join(player_props %>% 
                 select(player, "stat" = pts), 
               by = c("Name" = "player")) %>% 
    mutate(stat_name = "pts") -> pts
  
  table %>% 
    filter(rolling_ast >= 1) %>% 
    select(Name, Opponent, "rolling_value" = rolling_ast) %>% 
    left_join(player_props %>% 
                 select(player, "stat" = ast), 
               by = c("Name" = "player")) %>% 
    mutate(stat_name = "ast") -> ast
  
  table %>% 
    filter(rolling_reb >= 1) %>% 
    select(Name, Opponent, "rolling_value" = rolling_reb) %>% 
    left_join(player_props %>% 
                 select(player, "stat" = reb), 
               by = c("Name" = "player")) %>% 
    mutate(stat_name = "reb") -> reb

  table %>% 
    filter(rolling_fg3m >= 1) %>% 
    select(Name, Opponent, "rolling_value" = rolling_fg3m) %>% 
    left_join(player_props %>% 
                 select(player, "stat" = fg3m), 
               by = c("Name" = "player")) %>% 
    mutate(stat_name = "fg3m") -> fg3m
  
  rbind(pts, ast, reb, fg3m) %>% 
    drop_na() %>% 
    return()
}


find_best_matcups(final_data) -> best_matchups




find_best_matchups <- function(table, gamelogs, player_props) {
  # Define the get_hit_rate function
  get_hit_rate <- function(player, stat, prop_line, gamelogs) {
    hits <- gamelogs %>%
      filter(player_name == player) %>%
      arrange(desc(game_date)) %>%
      select(!!sym(stat)) %>%
      mutate(hit = ifelse(!!sym(stat) > prop_line, 1, 0)) %>%
      slice(1:10) %>%
      pull(hit)
    
    mean(hits, na.rm = TRUE)
  }
  
  # Process each stat category and combine
  map_dfr(c("pts", "ast", "reb", "fg3m"), ~{
    stat_name <- .x
    table %>%
      filter(!!sym(paste0("rolling_", stat_name)) >= 1) %>%
      select(Name, Opponent, "rolling_value" = !!sym(paste0("rolling_", stat_name))) %>%
      left_join(player_props %>% select(player, "stat" = !!sym(stat_name)), by = c("Name" = "player")) %>%
      mutate(stat_name = stat_name,
             hit_rate = map2_dbl(Name, stat, ~get_hit_rate(.x, stat_name, .y, gamelogs)),
             stat_average = map_dbl(Name, ~mean(gamelogs %>% 
                                                  filter(player_name == .x) %>% 
                                                  select(!!sym(stat_name)) %>%
                                                  pull(!!sym(stat_name)), na.rm = TRUE)),
             stat_min_l5 = map_dbl(Name, ~min(gamelogs %>% 
                                                  filter(player_name == .x) %>% 
                                                  select(!!sym(stat_name)) %>%
                                                  pull(!!sym(stat_name)), na.rm = TRUE)),
             stat_max_l5 = map_dbl(Name, ~max(gamelogs %>% 
                                                  filter(player_name == .x) %>% 
                                                  select(!!sym(stat_name)) %>%
                                                  pull(!!sym(stat_name)), na.rm = TRUE)))
  }) %>% 
    drop_na() -> result
  
  return(result)
}

find_best_matchups(final_data, gamelogs, player_props) %>% view



### player prop models --------
totals_full$scaled_rolling_pts <- as.numeric(totals_full$scaled_rolling_pts)
totals_full$scaled_rolling_ast <- as.numeric(totals_full$scaled_rolling_ast)
totals_full$scaled_rolling_reb <- as.numeric(totals_full$scaled_rolling_reb)
totals_full$scaled_rolling_fg3m <- as.numeric(totals_full$scaled_rolling_fg3m)
totals_full$scaled_rolling_dfs <- as.numeric(totals_full$scaled_rolling_dfs)


totals2 <- totals_full %>% 
  drop_na(current_pts, current_ast, current_3s, current_reb, current_usg, scaled_rolling_pts,
          current_dfs) %>% 
  filter(days_rest < 10)

lm(pts ~ current_pts+rolling_value+current_usg+days_rest, data = totals2 %>% filter(current_usg < 1) %>% 
     rename("rolling_value" = scaled_rolling_pts)) -> pts_model
lm(ast ~ current_ast + rolling_value + current_usg + days_rest, data = totals2 %>% filter(current_usg < 1) %>% 
     rename("rolling_value" = scaled_rolling_ast)) -> ast_model
lm(reb ~ current_reb + rolling_value + current_usg + days_rest, data = totals2 %>% filter(current_usg < 1) %>% 
     rename("rolling_value" = scaled_rolling_reb)) -> reb_model
lm(fg3m ~ current_3s + rolling_value + current_usg + days_rest, data = totals2 %>% filter(current_usg < 1) %>% 
     rename("rolling_value" = scaled_rolling_fg3m)) -> fg3m_model
lm(dfs_points ~ current_dfs + rolling_value + current_usg + days_rest, data = totals2 %>% filter(current_usg < 1) %>% 
     rename("rolling_value" = scaled_rolling_dfs)) -> dfs_model




get_rolling_stats <- \(player){

gamelogs %>%
  filter(player_name == player) %>%
  group_by(player_name) %>%
  arrange(game_date) %>%
  mutate(
    rolling_pts = rollapply(pts, 4, mean, fill = NA, align = "right"),
    rolling_mins = rollapply(min, 4, mean, fill = NA, align = "right"),
    rolling_ast = rollapply(ast, 4, mean, fill = NA, align = "right"),
    rolling_reb = rollapply(reb, 4, mean, fill = NA, align = "right"),
    rolling_3s = rollapply(fg3m, 4, mean, fill = NA, align = "right"),
    rolling_usg = rollapply(usage_percentage, 4, mean, fill = NA, align = "right"),
    ppm = rolling_pts / rolling_mins,
    sd_pts = rollapply(dfs_points, width = 4, FUN = sd, align = "right", fill = NA),
    player_rate = abs(rolling_pts / sd_pts)
  ) %>%
  filter(game_date == max(game_date)) %>% 
  select(player_name, rolling_pts, rolling_ast, rolling_reb, rolling_3s, rolling_usg)
}




find_best_matchups <- function(table, gamelogs, player_props) {
  # Define the get_hit_rate function
  get_hit_rate <- function(player, stat, prop_line, gamelogs) {
    hits <- gamelogs %>%
      filter(player_name == player) %>%
      arrange(desc(game_date)) %>%
      select(!!sym(stat)) %>%
      mutate(hit = ifelse(!!sym(stat) > prop_line, 1, 0)) %>%
      slice(1:10) %>%
      pull(hit)
    
    mean(hits, na.rm = TRUE)
  }
  
  # Process each stat category and combine
  map_dfr(c("pts", "ast", "reb", "fg3m"), ~{
    stat_name <- .x
    table %>%
      filter(!!sym(paste0("rolling_", stat_name)) >= 1) %>%
      select(Name, Opponent, "rolling_value" = !!sym(paste0("rolling_", stat_name))) %>%
      left_join(player_props %>% select(player, "stat" = !!sym(stat_name)), by = c("Name" = "player")) %>%
      mutate(stat_name = stat_name,
             hit_rate = map2_dbl(Name, stat, ~get_hit_rate(.x, stat_name, .y, gamelogs)),
             stat_average = map_dbl(Name, ~mean(gamelogs %>% 
                                                  filter(player_name == .x) %>% 
                                                  select(!!sym(stat_name)) %>%
                                                  pull(!!sym(stat_name)), na.rm = TRUE)),
             stat_min_l5 = map_dbl(Name, ~min(gamelogs %>% 
                                                filter(player_name == .x) %>% 
                                                select(!!sym(stat_name)) %>%
                                                pull(!!sym(stat_name)), na.rm = TRUE)),
             stat_max_l5 = map_dbl(Name, ~max(gamelogs %>% 
                                                filter(player_name == .x) %>% 
                                                select(!!sym(stat_name)) %>%
                                                pull(!!sym(stat_name)), na.rm = TRUE)))
  }) %>% 
    drop_na() %>% 
    group_by(Name) %>% 
    slice(1) -> result

  
  # Incorporate the get_rolling_stats function
  rolling_stats <- map_dfr(result$Name, ~get_rolling_stats(.x))
  final_result <- left_join(result, rolling_stats, by = c("Name" = "player_name"))
  
  
  final_result %>% 
    rename("current_pts" = rolling_pts,
           "current_ast" = rolling_ast,
           "current_reb" = rolling_reb,
           "current_3s" = rolling_3s,
           "current_usg" = rolling_usg) -> final_result
  
  return(final_result)
}

find_matchups <- function(table, gamelogs, player_props) {
  # Define the get_hit_rate function
  get_hit_rate <- function(player, stat, prop_line, gamelogs) {
    hits <- gamelogs %>%
      filter(player_name == player) %>%
      arrange(desc(game_date)) %>%
      select(!!sym(stat)) %>%
      mutate(hit = ifelse(!!sym(stat) > prop_line, 1, 0)) %>%
      slice(1:10) %>%
      pull(hit)
    
    mean(hits, na.rm = TRUE)
  }
  
  # Process each stat category and combine
  map_dfr(c("pts", "ast", "reb", "fg3m"), ~{
    stat_name <- .x
    table %>%
      select(Name, Opponent, days_rest, "rolling_value" = !!sym(paste0("rolling_", stat_name))) %>%
      left_join(player_props %>% select(player, "stat" = !!sym(stat_name)), by = c("Name" = "player")) %>%
      mutate(stat_name = stat_name,
             hit_rate = map2_dbl(Name, stat, ~get_hit_rate(.x, stat_name, .y, gamelogs)),
             stat_average = map_dbl(Name, ~mean(gamelogs %>% 
                                                  filter(player_name == .x) %>% 
                                                  select(!!sym(stat_name)) %>%
                                                  pull(!!sym(stat_name)), na.rm = TRUE)),
             stat_min_l5 = map_dbl(Name, ~min(gamelogs %>% 
                                                filter(player_name == .x) %>% 
                                                select(!!sym(stat_name)) %>%
                                                pull(!!sym(stat_name)), na.rm = TRUE)),
             stat_max_l5 = map_dbl(Name, ~max(gamelogs %>% 
                                                filter(player_name == .x) %>% 
                                                select(!!sym(stat_name)) %>%
                                                pull(!!sym(stat_name)), na.rm = TRUE)))
  }) %>% 
    drop_na() %>% 
    group_by(Name) %>% 
    slice(1) -> result
  
  
  # Incorporate the get_rolling_stats function
  rolling_stats <- map_dfr(result$Name, ~get_rolling_stats(.x))
  final_result <- left_join(result, rolling_stats, by = c("Name" = "player_name"))
  
  
  final_result %>% 
    rename("current_pts" = rolling_pts,
           "current_ast" = rolling_ast,
           "current_reb" = rolling_reb,
           "current_3s" = rolling_3s,
           "current_usg" = rolling_usg) -> final_result
  
  return(final_result)
}

# 
# find_best_matchups(table = final_data, gamelogs = gamelogs, player_props = player_props) -> best_players
# 
# find_worst_matchups(final_data, gamelogs, player_props) -> worst_players
# 
# 
# full_players <- rbind(best_players, worst_players)

full_players <- find_matchups(final_data, gamelogs, player_props)

make_predictions <- \(table){
  table %>% 
    filter(stat_name == "pts") -> pts_tab
  
  table %>% 
    filter(stat_name == "ast") -> ast_tab
  
  table %>% 
    filter(stat_name == "reb") -> reb_tab
  
  table %>% 
    filter(stat_name == "fg3m") -> fg3m_tab
  
  pts_tab$predicted_stat <- predict(pts_model,
                                    newdata = pts_tab)
  
  ast_tab$predicted_stat <- predict(ast_model,
                                    newdata = ast_tab)
  
  reb_tab$predicted_stat <- predict(reb_model,
                                    newdata = reb_tab)
  
  fg3m_tab$predicted_stat <- predict(fg3m_model,
                                    newdata = fg3m_tab)
  
  return(rbind(pts_tab, ast_tab, reb_tab, fg3m_tab))
}


## create google sheets connection

library(googlesheets4)
# gs4_auth()
# 
sheet = "https://docs.google.com/spreadsheets/d/16OW02KsaDxia-e74rRbPZA-ujuA6ZytPulWgVqFEJEw/edit#gid=0"
# 
# googlesheets4::write_sheet(ss = sheet, sheet = 1, bet_data)


## for future updates:

gs4_auth()

## pull from google sheet
bet_results <- googlesheets4::read_sheet(sheet, sheet = 1)

# Format the date columns if they're not already in Date format
bet_results$bet_date <- as.Date(bet_results$bet_date)
gamelogs$game_date <- as.Date(gamelogs$game_date)

# Joining the dataframes on player names and dates
joined_data <- merge(bet_results, gamelogs, by.x = c("Name", "bet_date"), by.y = c("player_name", "game_date"),
                     all.x = T)

# Function to determine the result
determine_result <- function(row) {
  stat_name <- row['stat_name']
  predicted_stat <- row['predicted_stat']
  actual_stat <- 0
  
  if (stat_name == 'pts') {
    actual_stat <- row['pts']
  } else if (stat_name == 'ast') {
    actual_stat <- row['ast']
  } else if (stat_name == 'reb') {
    actual_stat <- row['reb']
  }
  else {
    actual_stat <- row['fg3m']
  }
  
  return(as.numeric(actual_stat >= predicted_stat))
}

# Apply the function to each row
joined_data$over <- apply(joined_data, 1, determine_result)

## mutate line for est_over/est_under, then correct
joined_data %>% 
  mutate("over_predicted" = ifelse(predicted_stat > stat, 1, 0),
         correct = ifelse(over == over_predicted, 1, 0)) -> joined_data

# Extract the updated bet_data with the result column
updated_bet_data <- joined_data[, c(names(bet_results))]

current_props <- make_predictions(full_players) %>% 
  filter(floor(predicted_stat) > stat | ceiling(predicted_stat) < stat) %>% 
  mutate(bet_date = Sys.Date()) %>% 
  rename("opponent_rolling_rating_scaled" = rolling_value)


bind_rows(updated_bet_data, current_props) %>%
  distinct(.keep_all = TRUE) %>% 
  left_join(bind_rows(bet_results, current_props) %>% 
              distinct(Name, Opponent, stat, stat_name, 
                       hit_rate, .keep_all = T),keep = F) %>%
  filter(floor(predicted_stat) > stat | ceiling(predicted_stat < stat)) %>% 
  mutate("over_predicted" = ifelse(predicted_stat > stat, 1, 0),
         correct = ifelse(over == over_predicted, 1, 0)) %>% 
  arrange(bet_date) -> bet_data


bet_data %>% 
  relocate(bet_date, Name, Opponent, stat_name, stat, predicted_stat) %>% 
  distinct(bet_date, Name, Opponent, stat_name, stat, .keep_all = T)-> bet_data

googlesheets4::write_sheet(ss = sheet, sheet = 1, bet_data)





### script to find players who have gone over their line 5+ of last 10
## read hit rates
hit_rates_sheet <- googlesheets4::read_sheet(sheet, sheet = 2)


player_props

full_stats <- list()
for(i in 1:length(player_props$player)){
  name <- player_props$player[i]
  
  gamelogs %>% filter(player_name == name) %>% 
    arrange(desc(game_date)) %>% slice(1:10) -> player_l10
  
  player_props %>% 
    filter(player == name) -> prop_lines
  
  pts_line <- prop_lines %>% pull(pts)
  ast_line <- prop_lines %>% pull(ast)
  reb_line <- prop_lines %>% pull(reb)
  fg3m_line <- prop_lines %>% pull(fg3m)
  
  player_l10 %>% 
    mutate(pts_hit = ifelse(pts > pts_line, 1, 0),
           ast_hit = ifelse(ast > ast_line, 1, 0),
           reb_hit = ifelse(reb > reb_line, 1, 0),
           fg3m_hit = ifelse(fg3m > fg3m_line, 1, 0)) -> player_l10
  
  pts_rate <- mean(player_l10$pts_hit, na.rm = TRUE)
  ast_rate <- mean(player_l10$ast_hit, na.rm = TRUE)
  reb_rate <- mean(player_l10$reb_hit, na.rm = TRUE)
  fg3m_rate <- mean(player_l10$fg3m_hit, na.rm = TRUE)
  
  # Initialize results as NULL for each player
  pts_res <- NULL
  ast_res <- NULL
  reb_res <- NULL
  fg3m_res <- NULL
  
  if(!is.na(pts_rate) && pts_rate != 0.5){
    pts_res <- tibble("player" = name, "stat_name" = "points", "hit_rate" = pts_rate,
                      "line" = pts_line)
  }
  
  if(!is.na(ast_rate) && ast_rate != 0.5){
    ast_res <- tibble("player" = name, "stat_name" = "assists", "hit_rate" = ast_rate,
                      "line" = ast_line)
  }
  
  if(!is.na(reb_rate) && reb_rate != 0.5){
    reb_res <- tibble("player" = name, "stat_name" = "rebounds", "hit_rate" = reb_rate,
                      "line" = reb_line)
  }
  
  if(!is.na(fg3m_rate) && fg3m_rate != 0.5){
    fg3m_res <- tibble("player" = name, "stat_name" = "fg3m", "hit_rate" = fg3m_rate,
                       "line" = fg3m_line)
  }
  
  res <- list(pts_res, ast_res, reb_res, fg3m_res) %>% 
    purrr::compact() %>% 
    dplyr::bind_rows()
  
  full_stats[[i]] <- res
  
}


do.call(rbind, full_stats) %>% 
  mutate(bet_date = Sys.Date()) %>% 
  relocate(bet_date)-> hit_rates


rbind(hit_rates_sheet, hit_rates) %>% 
  arrange(bet_date, desc(hit_rate)) %>% 
  unique() -> hit_rates_final

googlesheets4::write_sheet(ss = sheet, sheet = 2, hit_rates_final)




