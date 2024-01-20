match_res <- list()
for(i in 1:length(nba_lineups$Name)){
  player_name <- nba_lineups$Name[i]
  match_res[[i]] <- create_matchup_table(player_name) %>% 
    arrange(desc(game_date)) %>% 
    ungroup() %>% 
    slice(1)
}

match_res.full <- do.call(rbind, match_res)



match_res.full %>% 
  unique() -> match_res.full


match_res.full %>% 
  rename("rolling_value" = rolling_dfs) -> match_res.full

match_res.full %>% 
  left_join(final_data %>% select(Name, days_rest),
            by = c("player_name" = "Name")) -> match_res.full


match_res.full$est_dfs <- predict(dfs_model, newdata = match_res.full)

