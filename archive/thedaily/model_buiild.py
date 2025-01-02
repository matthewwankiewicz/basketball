#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jan  1 16:25:21 2024

@author: matthew
"""

import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.metrics import mean_squared_error, r2_score

# Load the dataset
file_path = 'thedaily/gamelogs2023.csv'  # Replace with your file path
data = pd.read_csv(file_path)

# Initial data preprocessing and feature engineering

# Removing in-game and post-game statistics and other non-required columns
cols_to_exclude = ['min', 'fgm', 'fga', 'fg_pct', 'fg3m', 'fg3a', 'fg3_pct', 'ftm', 'fta', 'ft_pct', 
                   'oreb', 'dreb', 'reb', 'ast', 'stl', 'blk', 'to', 'pf', 'pts', 'plus_minus', 
                   'player_id', 'game_id', 'game_date', 'ast_rate', 'reb_rate', 'usage_percentage', 
                   'ts_percentage', 'double_double', 'triple_double', 'team_min', 'team_fgm', 'team_reb', 
                   'opp_reb', 'team_fta', 'team_to', 'team_fga', 'fan_pts', 'ftr', 'team', 'pos', 'opp_team']

data_reduced = data.drop(columns=cols_to_exclude)

# Calculating historical averages for each player's 'dfs_points'
player_historical_avg = data.groupby('player_name')['dfs_points'].mean().reset_index()
player_historical_avg.rename(columns={'dfs_points': 'player_avg_dfs_points'}, inplace=True)

# Calculating the average 'dfs_points' allowed by each opponent team
opp_team_impact = data.groupby('opp_team')['dfs_points'].mean().reset_index()
opp_team_impact.rename(columns={'dfs_points': 'opp_team_avg_allowed_dfs_points'}, inplace=True)

# Merging these averages back into the main dataset
data_with_new_features = data.merge(player_historical_avg, on='player_name')
data_with_new_features = data_with_new_features.merge(opp_team_impact, on='opp_team')

# Final data preparation
data_final_new = data_with_new_features.drop(columns=cols_to_exclude)

# Splitting the data into features (X) and target (y)
y_new = data_final_new['dfs_points']
X_new = data_final_new.drop(columns=['dfs_points', 'player_name'])  # Exclude 'player_name'

# Splitting the dataset into training and testing sets
X_train_new, X_test_new, y_train_new, y_test_new = train_test_split(X_new, y_new, test_size=0.2, random_state=42)

# Training and evaluating models with the new features

# Linear Regression
lr_model_new = LinearRegression()
lr_model_new.fit(X_train_new, y_train_new)
y_pred_lr_new = lr_model_new.predict(X_test_new)
mse_lr_new = mean_squared_error(y_test_new, y_pred_lr_new)
r2_lr_new = r2_score(y_test_new, y_pred_lr_new)

# Gradient Boosting
gb_model_new = GradientBoostingRegressor(random_state=42)
gb_model_new.fit(X_train_new, y_train_new)
y_pred_gb_new = gb_model_new.predict(X_test_new)
mse_gb_new = mean_squared_error(y_test_new, y_pred_gb_new)
r2_gb_new = r2_score(y_test_new, y_pred_gb_new)

# Results
print("Linear Regression - MSE:", mse_lr_new, "R2:", r2_lr_new)
print("Gradient Boosting - MSE:", mse_gb_new, "R2:", r2_gb_new)