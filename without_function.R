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
analyze_performance_without_player(
  "Joel Embiid"
)
