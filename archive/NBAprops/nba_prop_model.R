#### model building ------------
library(caret)
library(tidyverse)
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

totals_full <- totals_full %>%
  mutate(ppm = current_dfs/current_mins)

### player prop models --------
totals_full$scaled_rolling_pts <- as.numeric(totals_full$scaled_rolling_pts)
totals_full$scaled_rolling_ast <- as.numeric(totals_full$scaled_rolling_ast)
totals_full$scaled_rolling_reb <- as.numeric(totals_full$scaled_rolling_reb)
totals_full$scaled_rolling_fg3m <- as.numeric(totals_full$scaled_rolling_fg3m)
totals_full$scaled_rolling_dfs <- as.numeric(totals_full$scaled_rolling_dfs)

## remove NAs, filter out players who were injured for long periods of time
totals2 <- totals_full %>%
  drop_na(current_pts, current_ast, current_3s, current_reb, current_usg, scaled_rolling_pts,
          current_dfs, rolling_fg3_rate) %>%
  filter(days_rest < 10 & current_usg < 1)


### creating training and testing split
set.seed(21)
splitIndex <- createDataPartition(totals2$dfs_points, p = .80, list = FALSE, times = 1)
trainData <- totals2[splitIndex,]
testData <- totals2[-splitIndex,]

trainData$est_mins <- predict(mins_model, newdata = trainData)
testData$est_mins <- predict(mins_model, newdata = testData)

# 
# ### dfs model----
# lm(dfs_points ~ current_dfs + rolling_value:rolling_sd_dfs + current_usg + stat_max_l5 +
#      stat_min_l5 + current_dfs*rolling_value + rolling_mins + rolling_mins:current_usg +
#      ppm:rolling_mins + ppm, data = trainData %>%
#      rename("rolling_value" = scaled_rolling_dfs,
#             "stat_max_l5" = rolling_max_dfs,
#             "stat_min_l5" = rolling_min_dfs)) -> dfs_model
# summary(dfs_model)
# 
# testData$est_dfs <- predict(dfs_model, newdata = testData %>% rename("rolling_value" = scaled_rolling_dfs,
#                                                                      "stat_max_l5" = rolling_max_dfs,
#                                                                      "stat_min_l5" = rolling_min_dfs))
# 
# mean((testData$dfs_points - testData$est_dfs)^2)
# plot(testData$dfs_points, testData$est_dfs)
# cor(testData$dfs_points, testData$est_dfs)
# 
# ## pts model----
# lm(pts ~current_usg + current_pts + stat_max_l5 + rolling_mins +
#      stat_min_l5 + current_pts:rolling_value + current_mins, data = trainData %>%
#      rename("rolling_value" = scaled_rolling_pts,
#             "stat_max_l5" = rolling_max_pts,
#             "stat_min_l5" = rolling_min_pts)) -> pts_model
# summary(pts_model)
# 
# testData$est_pts <- predict(pts_model, newdata = testData %>% rename("rolling_value" = scaled_rolling_pts,
#                                                                      "stat_max_l5" = rolling_max_pts,
#                                                                      "stat_min_l5" = rolling_min_pts))
# 
# mean((testData$pts - testData$est_pts)^2)
# plot(testData$pts, testData$est_pts)
# cor(testData$pts, testData$est_pts)
# 
# ### ast model----
# lm(ast ~ current_ast + rolling_value + current_usg + stat_max_l5 + rolling_mins +
#      stat_min_l5 + current_ast*rolling_value + est_mins, data = trainData %>%
#      rename("rolling_value" = scaled_rolling_ast,
#             "stat_max_l5" = rolling_max_ast,
#             "stat_min_l5" = rolling_min_ast)) -> ast_model
# summary(ast_model)
# 
# testData$est_ast <- predict(ast_model, newdata = testData %>% rename("rolling_value" = scaled_rolling_ast,
#                                                                      "stat_max_l5" = rolling_max_ast,
#                                                                      "stat_min_l5" = rolling_min_ast))
# 
# mean((testData$ast - testData$est_ast)^2)
# plot(testData$ast, testData$est_ast)
# cor(testData$ast, testData$est_ast)
# 
# ### reb model----
# lm(reb ~ current_reb + rolling_value:rolling_sd_reb + stat_max_l5 +
#      stat_min_l5 + rolling_mins + est_mins, data = trainData %>%
#      rename("rolling_value" = scaled_rolling_reb,
#             "stat_max_l5" = rolling_max_reb,
#             "stat_min_l5" = rolling_min_reb)) -> reb_model
# summary(reb_model)
# 
# testData$est_reb <- predict(reb_model, newdata = testData %>% rename("rolling_value" = scaled_rolling_reb,
#                                                                      "stat_max_l5" = rolling_max_reb,
#                                                                      "stat_min_l5" = rolling_min_reb))
# 
# mean((testData$reb - testData$est_reb)^2)
# plot(testData$reb, testData$est_reb)
# cor(testData$reb, testData$est_reb)
# 
### fg3m
glm(fg3m ~ current_3s + current_usg + stat_max_l5 + rolling_mins +
     stat_min_l5 + current_3s*rolling_value + est_mins + rolling_fg3_rate, data = trainData %>%
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
# ## minutes model
# mins_model <- lm(min ~ current_mins*days_rest + current_usg - 1,
#                  data = trainData %>% filter(current_mins > 15))
# 
# 
# summary(mins_model)
# 
# testData$est_mins <- predict(mins_model, newdata = testData)
# plot(testData$est_mins,testData$min)
# cor(testData$est_mins, testData$min)
# mean((testData$est_mins-testData$min)^2)




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

#gs4_auth()

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
  data.frame() %>% 
  mutate("over_predicted" = ifelse(predicted_stat > stat_line, 1, 0),
         correct = ifelse(over == over_predicted, 1, 0)) %>%
  arrange(bet_date) -> bet_data


bet_data %>% 
  relocate(bet_date, Name, Opponent, stat_name, stat_line, predicted_stat) %>% 
  distinct(bet_date, Name, Opponent, stat_name, stat_line, .keep_all = T) %>% 
  select(bet_date, Name, Opponent, stat_name, stat_line, predicted_stat, opponent_rolling_rating_scaled,
         hit_rate, stat_average, stat_min_l5, stat_max_l5,
         over, over_predicted, correct)-> bet_data

googlesheets4::sheet_append(ss = sheet, sheet = 1, bet_data)





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



### logistic regression model
# sheet_log_results.model <- sheet_log_results %>% 
#   merge(bet_data %>% select(Name, bet_date, stat_name, predicted_stat),
#         by = c("Name", "bet_date", "stat_name")) %>% 
#   drop_na(over)
# 
# # Class frequencies
# class_0_count <- 458
# class_1_count <- 290
# 
# # Total number of instances
# total_count <- class_0_count + class_1_count
# 
# # Weights (inversely proportional to class frequencies)
# weight_0 <- total_count / class_0_count
# weight_1 <- total_count / class_1_count
# 
# # Normalize the weights so that the smallest weight is 1
# min_weight <- min(weight_0, weight_1)
# weight_0 <- weight_0 / min_weight
# weight_1 <- weight_1 / min_weight
# 
# # Show the weights
# weight_0
# weight_1
# 
# 
# set.seed(21)
# splitIndex <- createDataPartition(sheet_log_results.model$over, p = .80, list = FALSE, times = 1)
# sheet_log_results.model <- as.data.frame(sheet_log_results.model)
# trainData <- sheet_log_results.model[splitIndex,]
# testData <- sheet_log_results.model[-splitIndex,]
# weights <- ifelse(trainData$over == 0, weight_0, weight_1)
# 
player_prop_model <- glm(over ~ .^2,
         data = trainData %>% select(-c(Name, bet_date, Opponent, est_prob,
                                        correct_log, log_pred)), family = binomial())

summary(player_prop_model)
# 
# step(player_prop_model) -> final_model
# 
# testData$est_prob <- predict(final_model, type = "response",
#                              newdata = testData)
# 
# testData %>%
#   mutate(log_pred = ifelse(est_prob > 0.5, 1, 0),
#          correct_log = ifelse(log_pred == over, 1, 0)) %>% 
#   pull(correct_log) %>% mean(na.rm = T)
  
## read yesterday's bets from sheet
logistic_regression_results <- googlesheets4::read_sheet(ss = sheet, 
                                          sheet = 3) %>% 
  filter(bet_date == Sys.Date()-1) %>% 
  select(-c(pts, ast, reb, fg3m))

### modify determine result function to look if a player went over or not
determine_result.log <- function(row) {
  stat_name <- row['stat_name']
  predicted_stat <- row['stat_line']
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


## merge with gamelogs to find results
joined_data_log <- merge(logistic_regression_results, gamelogs_ref %>% 
                       select(pts, reb, ast, fg3m, player_name, game_date), by.x = c("Name", "bet_date"), by.y = c("player_name", "game_date"),
                     all.x = T)


joined_data_log$over <- apply(joined_data_log, 1, determine_result.log)

joined_data_log %>% drop_na(over) %>% 
  distinct(bet_date, Name, Opponent, stat_name,
           .keep_all = T) -> joined_data_log

joined_data_log %>% 
  mutate(log_pred = ifelse(est_prob > 0.5, 1, 0),
         correct_log = ifelse(log_pred == over, 1, 0)) %>% 
  select(bet_date, Name, Opponent, stat_name, stat_line, est_prob, opponent_rolling_rating_scaled,
         hit_rate, stat_average, stat_min_l5, stat_max_l5,
         over, log_pred, correct_log) -> logistic_results



bet_data$est_prob <- predict(final_model, type = "response",
                             newdata = bet_data)

bet_data %>% 
  filter(bet_date == Sys.Date()) %>% 
  mutate(log_pred = ifelse(est_prob > 0.5, 1, 0),
         correct_log = ifelse(log_pred == over, 1, 0)) %>% 
  select(bet_date, Name, Opponent, stat_name, stat_line, est_prob, opponent_rolling_rating_scaled,
         hit_rate, stat_average, stat_min_l5, stat_max_l5,
         over, log_pred, correct_log) -> logistic_results_present



sheet_log_results <- googlesheets4::read_sheet(ss = sheet, sheet = 3) 

logistic_results_upload <- bind_rows(sheet_log_results, logistic_results_present) %>% 
  distinct(bet_date, Name, stat_name, Opponent, stat_line, .keep_all = T) %>% 
  mutate(log_pred = ifelse(est_prob > 0.5, 1, 0),
         correct_log = ifelse(log_pred == over, 1, 0)) %>% 
  select(-c(pts, reb, ast, fg3m)) %>% 
  merge(gamelogs_ref %>% 
            select(pts, reb, ast, fg3m, player_name, game_date), by.x = c("Name", "bet_date"), by.y = c("player_name", "game_date"),
                    all.x = T)


logistic_results_upload$over <- apply(logistic_results_upload, 1, determine_result.log)

logistic_results_upload <- logistic_results_upload %>% 
  mutate(log_pred = ifelse(est_prob > 0.5, 1, 0),
         correct_log = ifelse(log_pred == over, 1, 0))

googlesheets4::write_sheet(ss = sheet, sheet = 3, logistic_results_upload %>% arrange(bet_date))


logistic_results_upload <- logistic_results_upload %>% 
  mutate(log_pred = ifelse(est_prob > 0.5, 1, 0),
         correct_log = ifelse(log_pred == over, 1, 0),
         matchup_rating = ifelse(opponent_rolling_rating_scaled > 0, "good", "bad"))

logistic_results_upload %>%
  filter(opponent_rolling_rating_scaled > 1, 
         hit_rate > 0.5) %>% 
  mutate(perc = case_when(
    est_prob > 0.95 ~ ">.9",
    est_prob > .8 ~ ">.8",
    est_prob > 0.7 ~ ">.7",
    est_prob > 0.6 ~ ">0.6",
    est_prob > 0.5 ~ ">0.5",
    est_prob < 0.1 ~ "<.2",
    .default = "else"
  )) %>%
  group_by(bet_date) %>% 
  summarise(accuracy = mean(correct_log, na.rm = T),
            bets = n(),
            avg_pred = mean(est_prob, na.rm = T)) %>% 
  mutate(roll = cummean(accuracy))








#### glmer

new.mod <- lme4::glmer(over ~ (1|stat_name) + stat_line + 
                         hit_rate + stat_average + stat_min_l5:hit_rate,
                       family = binomial(), data = trainData)

ranef(new.mod)

testData$est_prob <- predict(final_model, newdata = testData,
                             type = "response")


logistic_results_upload %>% 
  filter(log_pred == 1,
         bet_date >= Sys.Date()-2) %>% 
  mutate(log_pred = ifelse(est_prob > 0.5, 1, 0),
         correct_log = ifelse(over == log_pred, 1, 0)) %>% pull(correct_log) %>% mean(na.rm=T)


pROC::roc(logistic_results$log_pred, logistic_results$over) %>% plot

confusionMatrix(as.factor(logistic_results$log_pred), as.factor(logistic_results$over))


### xg boost model
# Assuming your data is in a data frame called 'data'
# Convert factors to dummy variables
data_dummy <- model.matrix(~ . - 1, data = sheet_log_results.model)

# Split the data into training and testing sets
set.seed(42)
train_idx <- sample(1:nrow(data_dummy), 0.7 * nrow(data_dummy))
train_data <- data_dummy[train_idx, ]
test_data <- data_dummy[-train_idx, ]

# Separate features and target
train_label <- train_data$over
train_matrix <- xgb.DMatrix(data = as.matrix(train_data[, -which(names(train_data) == "over")]), label = train_label)

test_label <- test_data$over
test_matrix <- xgb.DMatrix(data = as.matrix(test_data[, -which(names(test_data) == "over")]), label = test_label)

# Parameters for the XGBoost model
params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eval_metric = "logloss",
  eta = 0.3,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 1,
  colsample_bytree = 1
)

# Train the model
xgb_model <- xgboost(
  params = params,
  data = train_matrix,
  nrounds = 100,
  watchlist = list(eval = test_matrix, train = train_matrix),
  verbose = 0
)

# Predictions
preds <- predict(xgb_model, test_matrix)
preds_class <- ifelse(preds > 0.5, 1, 0)

# Evaluate the model
library(caret)
confusionMatrix(factor(preds_class), factor(test_label))



logistic_results_upload %>% 
  filter(est_prob > 0.8,
         opponent_rolling_rating_scaled  -0.5) %>% 
  group_by(bet_date) %>% 
  summarise(n = n(),
            acc = mean(correct_log, na.rm = T))


logistic_results_upload %>% 
  select(-c(pts, ast, reb, fg3m)) %>% 
  merge(totals_full,
        by.x = c("bet_date", "Name"),
        by.y = c("game_date", "player_name")) -> player_stat_lines


player_stat_lines <- player_stat_lines %>% 
  select(-c(log_pred, correct_log, est_prob))

player_stat_lines <- player_stat_lines %>% 
  mutate(scaled_rolling_mins = as.vector(scaled_rolling_mins),
         scaled_rolling_fg3_rate = as.vector(scaled_rolling_fg3_rate))

write_csv(player_stat_lines, "monte_carlo_gpt.csv")




### monte carlo sims ####

# monte_carlo_start <- bet_data %>%
#   rowwise() %>%
#   mutate(simulation_result = simulate_stat(Name, Opponent, stat_name, stat_line,
#                                            date_filter = bet_date))

### modify determine result function to look if a player went over or not
determine_result.monte <- function(row) {
  stat_name <- row['stat_name']
  predicted_stat <- row['stat_line']
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

monte_carlo_upload <- monte_carlo_start %>% 
  left_join(gamelogs %>% select(player_name, game_date, 
                                pts, ast, reb, fg3m),
            by = c("Name" = "player_name", "bet_date" = "game_date"))


monte_carlo_upload$over <- apply(monte_carlo_upload, 1, determine_result.monte)

monte_carlo_upload <- monte_carlo_upload %>% 
  select(bet_date, Name, Opponent, stat_name, stat_line, opponent_rolling_rating_scaled,
         hit_rate, over, simulation_result, stat_average, pts, reb, ast, fg3m)

write_sheet(monte_carlo_upload, ss = sheet, sheet = 4)

## pull current sheet
monte_carlo_current = read_sheet(ss = sheet, sheet = 4)

monte_carlo_current <- monte_carlo_current %>% 
  rowwise() %>% 
  mutate(simulation_result = simulate_stat(Name, opponent_rolling_rating_scaled, stat_name, stat_line,
                                           date_filter = bet_date))

# merge with gamelogs and calculate results
monte_carlo_current <- monte_carlo_current %>% select(-c(pts, ast, reb, fg3m)) %>% 
  left_join(gamelogs %>% select(player_name, game_date, 
                                pts, ast, reb, fg3m),
            by = c("Name" = "player_name", "bet_date" = "game_date"))


monte_carlo_current$over <- apply(monte_carlo_current, 1, determine_result.monte)


monte_carlo_current <- monte_carlo_current %>%
  mutate(monte_pred = ifelse(simulation_result > 0.5, 1, 0),
         monte_correct = ifelse(monte_pred == over, 1, 0))


## create matchup list for today
full_players <- find_matchups(final_data, gamelogs_ref, player_props)

## make predictions
current_props <- full_players %>% 
  rename("opponent_rolling_rating_scaled" = rolling_value) %>% 
  mutate(bet_date = Sys.Date()) %>% 
  select(bet_date, Name, Opponent, stat_name, stat_line, opponent_rolling_rating_scaled,
         hit_rate, stat_average) %>% 
  rowwise() %>% 
  mutate(simulation_result = simulate_stat(Name, opponent_rolling_rating_scaled, stat_name, stat_line,
                                           date_filter = bet_date),
         monte_pred = ifelse(simulation_result > 0.5, 1, 0))


## bind rows with previous and current tables
monte_carlo_upload <- bind_rows(monte_carlo_current, current_props)

## upload to google sheet
write_sheet(monte_carlo_upload, ss = sheet, sheet = 4)


monte_carlo_current %>% 
  mutate(monte_pred = ifelse(simulation_result > 0.5, 1, 0),
         monte_correct = ifelse(monte_pred == over, 1, 0)) %>%
  filter(simulation_result > 0.9) %>% 
  group_by(bet_date) %>% 
  summarise(acc = mean(monte_correct, na.rm = T),
            n = n()) %>% 
  mutate(overall_average = cummean(acc),
         total_bets = cumsum(n)) %>% view


monte_carlo_current %>% 
  filter(stat_name == "fg3m") %>% 
  mutate(diff = stat_line - fg3m) %>% 
  group_by(bet_date) %>% 
  summarise(mea = mean(diff, na.rm = T)) %>% 
  mutate(rol = cummean(mea)) %>% view



monte_carlo_current %>%
  filter(simulation_result > 0.9,
         bet_date == Sys.Date()-1) %>%
  mutate(bet = ifelse(monte_pred == 1, "over", "under")) %>% 
  select(Name, Opponent, stat_name, stat_line, bet,
         "Last 10 Hit Rate" = hit_rate, simulation_result) %>% view
