str(gamelogs)

library(dplyr)
library(caret)

# Assuming your dataset is named 'basketball_data'

# Calculate player averages for numeric variables
player_averages <- gamelogs %>%
  filter(min > 15) %>% 
  select(-ftr) %>% 
  group_by(player_name) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Merge player averages with the original dataset
merged_data <- gamelogs %>%
  select(player_name, dfs_points, player_id) %>% 
  left_join(player_averages, by = c("player_name", "player_id"))

set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(merged_data$dfs_points.x, p = 0.8, list = FALSE)
train_data <- merged_data[trainIndex, ]
test_data <- merged_data[-trainIndex, ]

# Create a formula with all numeric variables (excluding dfs_points)
numeric_vars <- names(select_if(player_averages, is.numeric))
formula <- as.formula(paste("dfs_points.x ~", paste(numeric_vars[!numeric_vars %in% "dfs_points"], collapse = " + ")))

# Train the model on the training data
model <- lm(formula, data = train_data)

predicted_dfs_points <- predict(model, newdata = test_data)

# Evaluate the model
accuracy <- sqrt(mean((test_data$dfs_points - predicted_dfs_points)^2))
print(paste("Root Mean Squared Error (RMSE):", accuracy))

