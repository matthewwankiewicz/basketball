#### model building ------------

### run through player list and collect matchups
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

totals_full %>% 
  left_join(gamelogs_ref %>% select(player_name, pos) %>%
              group_by(player_name) %>% slice(1) %>% ungroup) %>% drop_na(pos) -> totals_full

### player prop models --------
totals_full$scaled_rolling_pts <- as.numeric(totals_full$scaled_rolling_pts)
totals_full$scaled_rolling_ast <- as.numeric(totals_full$scaled_rolling_ast)
totals_full$scaled_rolling_reb <- as.numeric(totals_full$scaled_rolling_reb)
totals_full$scaled_rolling_fg3m <- as.numeric(totals_full$scaled_rolling_fg3m)
totals_full$scaled_rolling_dfs <- as.numeric(totals_full$scaled_rolling_dfs)

## remove NAs, filter out players who were injured for long periods of time
totals2 <- totals_full %>% 
  drop_na(current_pts, current_ast, current_3s, current_reb, current_usg, scaled_rolling_pts,
          current_dfs) %>% 
  filter(days_rest < 10 & current_usg < 1 & min >=20)


### creating training and testing split
set.seed(21)
splitIndex <- createDataPartition(totals2$dfs_points, p = .80, list = FALSE, times = 1)
trainData <- totals2[splitIndex,]
testData <- totals2[-splitIndex,]

### dfs model----
lm(dfs_points ~ current_dfs + rolling_value:rolling_sd_dfs + current_usg + stat_max_l5 +
     stat_min_l5 + current_dfs*rolling_value + rolling_mins, data = trainData %>% 
     rename("rolling_value" = scaled_rolling_dfs,
            "stat_max_l5" = rolling_max_dfs,
            "stat_min_l5" = rolling_min_dfs)) -> dfs_model
summary(dfs_model)

testData$est_dfs <- predict(dfs_model, newdata = testData %>% rename("rolling_value" = scaled_rolling_dfs,
                                                                     "stat_max_l5" = rolling_max_dfs,
                                                                     "stat_min_l5" = rolling_min_dfs))

mean((testData$dfs_points - testData$est_dfs)^2)
plot(testData$dfs_points, testData$est_dfs)
cor(testData$dfs_points, testData$est_dfs)

## pts model----
lm(pts ~current_usg + current_pts + stat_max_l5 + rolling_mins +
     stat_min_l5 + current_pts:rolling_value, data = trainData %>% 
     rename("rolling_value" = scaled_rolling_pts,
            "stat_max_l5" = rolling_max_pts,
            "stat_min_l5" = rolling_min_pts)) -> pts_model
summary(pts_model)

testData$est_pts <- predict(pts_model, newdata = testData %>% rename("rolling_value" = scaled_rolling_pts,
                                                                     "stat_max_l5" = rolling_max_pts,
                                                                     "stat_min_l5" = rolling_min_pts))

mean((testData$pts - testData$est_pts)^2)
plot(testData$pts, testData$est_pts)
cor(testData$pts, testData$est_pts)

### ast model----
lm(ast ~ current_ast + rolling_value + current_usg + stat_max_l5 + rolling_mins +
     stat_min_l5 + current_ast*rolling_value, data = trainData %>% 
     rename("rolling_value" = scaled_rolling_ast,
            "stat_max_l5" = rolling_max_ast,
            "stat_min_l5" = rolling_min_ast)) -> ast_model
summary(ast_model)

testData$est_ast <- predict(ast_model, newdata = testData %>% rename("rolling_value" = scaled_rolling_ast,
                                                                     "stat_max_l5" = rolling_max_ast,
                                                                     "stat_min_l5" = rolling_min_ast))

mean((testData$ast - testData$est_ast)^2)
plot(testData$ast, testData$est_ast)
cor(testData$ast, testData$est_ast)

### reb model----
lm(reb ~ current_reb + rolling_value:rolling_sd_reb + stat_max_l5 +
     stat_min_l5 + rolling_mins, data = trainData %>% 
     rename("rolling_value" = scaled_rolling_reb,
            "stat_max_l5" = rolling_max_reb,
            "stat_min_l5" = rolling_min_reb)) -> reb_model
summary(reb_model)

testData$est_reb <- predict(reb_model, newdata = testData %>% rename("rolling_value" = scaled_rolling_reb,
                                                                     "stat_max_l5" = rolling_max_reb,
                                                                     "stat_min_l5" = rolling_min_reb))

mean((testData$reb - testData$est_reb)^2)
plot(testData$reb, testData$est_reb)
cor(testData$reb, testData$est_reb)

### fg3m
glm(fg3m ~ current_3s + current_usg + stat_max_l5 + rolling_mins +
     stat_min_l5 + current_3s*rolling_value, data = trainData %>% 
     rename("rolling_value" = scaled_rolling_fg3m,
            "stat_max_l5" = rolling_max_fg3m,
            "stat_min_l5" = rolling_min_fg3m)) -> fg3m_model
summary(fg3m_model)

testData$est_fg3m <- predict(fg3m_model, newdata = testData %>% rename("rolling_value" = scaled_rolling_fg3m,
                                                                       "stat_max_l5" = rolling_max_fg3m,
                                                                       "stat_min_l5" = rolling_min_fg3m))

mean((testData$fg3m - testData$est_fg3m)^2)
plot(testData$fg3m, testData$est_fg3m)
cor(testData$fg3m, testData$est_fg3m)












# 
# find_best_matchups(table = final_data, gamelogs = gamelogs, player_props = player_props) -> best_players
# 
# find_worst_matchups(final_data, gamelogs, player_props) -> worst_players
# 
# 
# full_players <- rbind(best_players, worst_players)

full_players <- find_matchups(final_data, gamelogs_ref, player_props)




## update google sheet ----

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
joined_data <- merge(bet_results, gamelogs_ref %>% 
                       select(pts, reb, ast, fg3m, player_name, game_date), by.x = c("Name", "bet_date"), by.y = c("player_name", "game_date"),
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
  mutate("over_predicted" = ifelse(predicted_stat > stat_line, 1, 0),
         correct = ifelse(over == over_predicted, 1, 0)) %>% 
  drop_na(correct) -> joined_data

# Extract the updated bet_data with the result column
updated_bet_data <- joined_data[, c(names(bet_results), "over", "over_predicted",
                                          "correct")]

current_props <- make_predictions(full_players) %>%
  filter(floor(predicted_stat) > stat_line | ceiling(predicted_stat) < stat_line) %>% 
  mutate(bet_date = Sys.Date()) %>% 
  rename("opponent_rolling_rating_scaled" = rolling_value)


bind_rows(updated_bet_data, current_props) %>%
  distinct(.keep_all = TRUE) %>% 
  left_join(bind_rows(bet_results, current_props) %>% 
              distinct(Name, Opponent, stat_line, stat_name, 
                       hit_rate, .keep_all = T),keep = F) %>%
  filter(floor(predicted_stat) > stat_line | ceiling(predicted_stat < stat_line)) %>% 
  mutate("over_predicted" = ifelse(predicted_stat > stat_line, 1, 0),
         correct = ifelse(over == over_predicted, 1, 0)) %>% 
  arrange(bet_date) -> bet_data


bet_data %>% 
  relocate(bet_date, Name, Opponent, stat_name, stat_line, predicted_stat) %>% 
  distinct(bet_date, Name, Opponent, stat_name, stat_line, .keep_all = T) %>% 
  select(bet_date, Name, Opponent, stat_name, stat_line, predicted_stat, opponent_rolling_rating_scaled,
         hit_rate, stat_average, stat_min_l5, stat_max_l5,
         over, over_predicted, correct)-> bet_data

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




