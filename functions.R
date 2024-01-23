### processing functions

## create matchup table --------
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
    filter(player_name == player) %>% 
    select(game_date, player_name, pos, opp_team, rolling_mins, min, pts, ast, reb, fg3m,
           rolling_pts, rolling_usg, rolling_ast,
           rolling_reb, rolling_3s, rolling_dfs, dfs_points,
           rolling_max_pts, rolling_min_pts, rolling_max_ast, rolling_min_ast,
           rolling_max_dfs, rolling_min_dfs, rolling_max_reb, rolling_min_reb,
           rolling_max_fg3m, rolling_min_fg3m,
           rolling_sd_dfs, rolling_sd_pts, rolling_sd_ast, rolling_sd_reb,
           rolling_sd_fg3m) %>% 
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
      player_rate = abs(rolling_pts / sd_pts)
    ) %>%
    filter(game_date == max(game_date)) %>% 
    ungroup() %>% 
    select(player_name, rolling_pts, rolling_ast, rolling_reb, rolling_3s, 
           rolling_usg, rolling_mins, rolling_sd_dfs, rolling_sd_pts,
           rolling_sd_fg3m, rolling_sd_reb, rolling_sd_ast,
           rolling_dfs)
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
      left_join(player_props %>% select(player, "stat_line" = !!sym(stat_name)), by = c("Name" = "player")) %>%
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

