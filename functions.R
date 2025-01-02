### processing functions

## create matchup table --------
create_matchup_table <- \(player){
  player_table <- gamelogs %>%
    mutate(fg3_rate = fg3a/fga) %>% 
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
      rolling_sd_fg3m = rollapply(fg3m, 5, sd, fill = NA, align = "right"),
      rolling_fg3_rate = rollapply(fg3_rate, 4, mean, fill = NA, alight = "right")
    ) %>%
    filter(player_name == player) %>% 
    select(game_date, player_name, pos, opp_team, rolling_mins, min, pts, ast, reb, fg3m,
           rolling_pts, rolling_usg, rolling_ast,
           rolling_reb, rolling_3s, rolling_dfs, dfs_points,
           rolling_max_pts, rolling_min_pts, rolling_max_ast, rolling_min_ast,
           rolling_max_dfs, rolling_min_dfs, rolling_max_reb, rolling_min_reb,
           rolling_max_fg3m, rolling_min_fg3m,
           rolling_sd_dfs, rolling_sd_pts, rolling_sd_ast, rolling_sd_reb,
           rolling_sd_fg3m, rolling_fg3_rate) %>% 
    separate_rows(pos, sep = ",") %>% 
    group_by(game_date, player_name) %>% 
    slice(1)
  
  
  team_table <- gamelogs %>%
    mutate(fg3_rate = fg3a/fga) %>% 
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
              fg3_rate = mean(fg3_rate),
              .groups = 'drop') %>%
    arrange(game_date) %>% 
    group_by(opp_team, pos) %>%
    mutate(
      rolling_dfs = rollapply(dfs_points, 5, mean, fill = NA, align = "right"),
      rolling_pts = as.vector(rollapply(pts, 5, mean, fill = NA, align = "right")),
      rolling_ast = rollapply(ast, 5, mean, fill = NA, align = "right"),
      rolling_reb = rollapply(reb, 5, mean, fill = NA, align = "right"),
      rolling_fg3m = rollapply(fg3m, 5, mean, fill = NA, align = "right"),
      rolling_mins = rollapply(mins, 5, mean, fill = NA, align = "right"),
      rolling_fg3_rate = rollapply(fg3_rate, 5, mean, fill = NA, align = "right")) %>%
    ungroup() %>%
    group_by(opp_team) %>%
    mutate(
      scaled_rolling_pts = scale(rolling_pts),
      scaled_rolling_ast = scale(rolling_ast),
      scaled_rolling_reb = scale(rolling_reb),
      scaled_rolling_fg3m = scale(rolling_fg3m),
      scaled_rolling_mins = scale(rolling_mins),
      scaled_rolling_dfs = scale(rolling_dfs),
      scaled_rolling_fg3_rate = scale(rolling_fg3_rate)
    ) %>%
    select(game_date, opp_team, pos, scaled_rolling_pts, scaled_rolling_ast, scaled_rolling_reb,
           scaled_rolling_fg3m, scaled_rolling_mins, scaled_rolling_dfs, scaled_rolling_fg3_rate)
  
  
  
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
  player_table$current_mins <- lead(player_table$rolling_mins, 1)
  
  return(player_table)
  
}

### find best matchups -----
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



### get rolling stats -------
get_rolling_stats <- \(player){
  
  gamelogs_ref %>%
    filter(player_name == player) %>%
    group_by(player_name) %>%
    arrange(game_date) %>%
    mutate(
      rolling_pts = rollapply(pts, 4, mean, fill = NA, align = "right"),
      rolling_ast = rollapply(ast, 4, mean, fill = NA, align = "right"),
      rolling_reb = rollapply(reb, 4, mean, fill = NA, align = "right"),
      rolling_3s = rollapply(fg3m, 4, mean, fill = NA, align = "right"),
      rolling_mins = rollapply(min, 4, mean, fill = NA, align = "right"),
      rolling_dfs = rollapply(dfs_points, 4, mean, fill = NA, align = "right"),
      rolling_usg = rollapply(usage_percentage, 4, mean, fill = NA, align = "right"),
      rolling_sd_dfs = rollapply(dfs_points, 5, sd, fill = NA, align = "right"),
      rolling_sd_pts = rollapply(pts, 5, sd, fill = NA, align = "right"),
      rolling_sd_ast = rollapply(ast, 5, sd, fill = NA, align = "right"),
      rolling_sd_reb = rollapply(reb, 5, sd, fill = NA, align = "right"),
      rolling_sd_fg3m = rollapply(fg3m, 5, sd, fill = NA, align = "right"),
      ppm = rolling_pts / rolling_mins,
      sd_pts = rollapply(dfs_points, width = 4, FUN = sd, align = "right", fill = NA),
      player_rate = abs(rolling_pts / sd_pts),
      rolling_fg3_rate = rollapply(fg3_rate, 4, mean, fill = NA, alight = "right")
    ) %>%
    filter(game_date == max(game_date)) %>% 
    ungroup() %>% 
    select(player_name, rolling_pts, rolling_ast, rolling_reb, rolling_3s, 
           rolling_usg, rolling_mins, rolling_sd_dfs, rolling_sd_pts,
           rolling_sd_fg3m, rolling_sd_reb, rolling_sd_ast,
           rolling_dfs, rolling_fg3_rate)
}


### find matchups -------
find_matchups <- function(table, gamelogs, player_props) {
  # Define the get_hit_rate function
  get_hit_rate <- function(player, stat, prop_line, gamelogs) {
    hits <- gamelogs_ref %>%
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
      ungroup() %>%
      select(Name, Opponent, "rolling_value" = !!sym(paste0("opp_rolling_", stat_name))) %>%
      left_join(player_props %>% select(full_name, "stat_line" = !!sym(stat_name)), by = c("Name" = "full_name")) %>%
      group_by(Name) %>%
      mutate(stat_name = stat_name,
             hit_rate = map2_dbl(Name, stat_line, ~get_hit_rate(.x, stat_name, .y, gamelogs)),
             stat_average = map_dbl(Name, ~mean(gamelogs_ref %>%
                                                  filter(player_name == .x) %>%
                                                  ungroup() %>% 
                                                  select(!!sym(stat_name)) %>%
                                                  pull(!!sym(stat_name)), na.rm = TRUE)),
             stat_min_l5 = map_dbl(Name, ~min(gamelogs_ref %>%
                                                filter(player_name == .x) %>%
                                                ungroup() %>% 
                                                select(!!sym(stat_name)) %>%
                                                pull(!!sym(stat_name)), na.rm = TRUE)),
             stat_max_l5 = map_dbl(Name, ~max(gamelogs_ref %>%
                                                filter(player_name == .x) %>%
                                                ungroup() %>%
                                                select(!!sym(stat_name)) %>%
                                                pull(!!sym(stat_name)), na.rm = TRUE)))
  }) %>%
    drop_na() -> result
  
  result <- result %>% 
    left_join(table %>% select(Name, current_mins, days_rest),
              by = c("Name"))
  
  # Incorporate the get_rolling_stats function
  rolling_stats <- map_dfr(unique(result$Name), ~get_rolling_stats(.x)) %>% 
    ungroup()
  final_result <- left_join(result, rolling_stats, by = c("Name" = "player_name")) %>% 
    ungroup()
  
  
  final_result %>%
    rename("current_pts" = rolling_pts,
           "current_ast" = rolling_ast,
           "current_reb" = rolling_reb,
           "current_3s" = rolling_3s,
           "current_usg" = rolling_usg,
           "current_dfs" = rolling_dfs) -> final_result
  
  return(final_result)
}


## make predictions
make_predictions <- \(table){
  table$est_mins <- predict(mins_model,
                            newdata = table)
  
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

#### get schedule #####
# data <- fread("fantasybball/fantasybball/daily_app_data.csv") # formatted gamelogs file
# schedule <- fread("~/Documents/basketball/fantasybball/nba_final_schedule.csv") # formatted schedule

## create function to pull player's schedule for a given week
get_schedule <- function(player, start_date = Sys.Date()-7, end_date = Sys.Date(),
                         game_data = data, full_schedule = schedule){
  ## get each player's most recent team + position
  position_data <- game_data %>% 
    slice_max(game_date, by = player_name) %>% 
    select(player_name, position, "team" = team_abbreviation, is_free_agent) %>% 
    filter(player_name == player)
  
  ## pull the player's team
  player_team <- position_data$team
  
  ## filter schedule for that team return
  team_schedule <- full_schedule %>% 
    filter(game_date >= start_date & game_date <= end_date &
             team == player_team)
  
  ## output the schedule
  return(team_schedule)
}

#get_schedule("Scottie Barnes")

#### without function -----------
analyze_performance_without_player <- function(player) {
  # pull player's team
  gamelogs %>% filter(player_name == !!player) %>% 
    arrange(desc(game_date)) %>% 
    pull(team) %>% .[1] ->> team
  
  # Identify the games where the specific player participated
  games_with_player <- gamelogs %>%
    filter(team == team, player_name == !!player) %>%
    select(game_id) %>%
    distinct()
  
  # Filter the dataset for games where the specific player did not participate
  teammates_stats_without_player <- gamelogs %>%
    filter(team == !!team, !game_id %in% games_with_player$game_id)
  
  # Analyze the statistics of these players
  teammates_summary <- teammates_stats_without_player %>%
    filter(team == !!team) %>% 
    group_by(player_name) %>%
    summarise(min = mean(min),
              pts = mean(pts),
              ast = mean(ast),
              reb = mean(reb),
              dfs_points = mean(dfs_points),
              ppm = dfs_points/min,
              games = n())
  
  return(teammates_summary)
}

# Usage example
# Replace 'c("stat1", "stat2", ...)' with the actual stat columns you're interested in.
# analyze_performance_without_player(
#   "Joel Embiid"
# )



#### simulate stats -------

simulate_stat <- \(player, opponent_rolling_stat, stat_name, stat_line, date_filter = Sys.Date(), stat_output = "prob"){
  ## filter for player's game logs
  player_logs <- gamelogs_ref %>% 
    filter(player_name == player, 
           game_date < date_filter) %>% 
    arrange(desc(game_date)) ## arange game dates to have most recent list first
  
  player_pos <- player_logs$pos[1]
  
  
  ## slice for last 3, 5, and 10 games
  last3_slice <- player_logs %>% 
    slice(1:3)
  
  
  last5_slice <- player_logs %>% 
    slice(1:5)
  
  
  
  last10_slice <- player_logs %>% 
    slice(1:7)
  
  
  
  ## get averages & sd for 3, 5, 10 game log
  last3_avg <- last3_slice[[stat_name]] %>% mean()
  last5_avg <- last5_slice[[stat_name]] %>% mean()
  last10_avg <- last10_slice[[stat_name]] %>% mean()
  
  last3_sd <- last3_slice[[stat_name]] %>% sd()
  last5_sd <- last5_slice[[stat_name]] %>% sd()
  last10_sd <- last10_slice[[stat_name]] %>% sd()
  
  
  
  ## run the simulations
  simmed <- c()
  simmed2 <- c()
  simmed3 <- c()
  set.seed(21)
  for(i in 1:10000){
    simmed[i] <- qnorm(runif(1), mean = last3_avg, sd = last3_sd) +
      qnorm(runif(1), mean = last3_avg, sd = last3_sd)*team_adj
    simmed2[i] <- qnorm(runif(1), mean = last5_avg, sd = last5_sd) +
      qnorm(runif(1), mean = last5_avg, sd = last5_sd)*team_adj
    simmed3[i] <- qnorm(runif(1), mean = last10_avg, sd = last10_sd) +
      qnorm(runif(1), mean = last10_avg, sd = last10_sd)*team_adj
  }
  combined <-  simmed
  
  ## return estimated point
  if(stat_output != "prob"){
    return(mean(combined))
  } else{
    return((sum(combined > stat_line))/10000) 
  }
}


### pull weekly schedules --------

pull_weekly_schedule <- function(schedule, positions, start, end, teams = NULL){
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
    filter(game_date>=start & game_date <= end) %>% 
    merge(positions, by.x = "team", by.y = "pro_team", allow.cartesian = T)
  
  if(!is.null(teams)){
    matchups %>% 
      filter(fantasy_team %in% teams) %>% 
      return()
  }
  
  else{
    return(matchups)
  }
  
}

## example
# pull_weekly_schedule(schedule, position_data, start = Sys.Date(), end = "2025-01-05",
#                      teams = c("Team Wankiewicz"))


### get weekly stat function -----------
### returns a table with a column for each team and row for each date, followed
### by the total (will only be used for points and minutes now)
calculate_weekly_totals <- function(data, value_col) {
  # Convert value_col to symbol for tidy evaluation
  value_col_sym <- rlang::sym(value_col)
  
  # Calculate weekly totals
  weekly_totals <- data %>% 
    group_by(game_date, fantasy_team) %>% 
    summarise(total = sum(!!value_col_sym, na.rm=T)) %>% 
    pivot_wider(
      names_from = fantasy_team,
      values_from = total
    ) %>% 
    mutate(game_date = as.character(game_date))
  
  # Calculate grand totals
  total_projected <- data %>% 
    group_by(fantasy_team) %>% 
    summarise(total = sum(!!value_col_sym, na.rm=T)) %>% 
    pivot_wider(
      names_from = fantasy_team,
      values_from = total
    ) %>% 
    mutate(game_date = "Total")
  
  # Combine results
  final_output <- rbind(weekly_totals, total_projected)
  
  return(final_output)
}

## example
#calculate_weekly_totals(schedule, "rolling_min")