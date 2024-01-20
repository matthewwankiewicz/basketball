#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Dec 31 18:51:08 2023

@author: matthew
"""

import pandas as pd
import pulp

# Positions
positions = ['PG', 'SG', 'SF', 'PF', 'C']

# Load the data
dk_lines = pd.read_csv("dk_lines.csv")

# Remove rows with NaN in relevant columns
dk_lines = dk_lines.dropna(subset=['avg_pts', 'ppm'])

# Load your historical data
# This should include the same features as in dk_lines and the target variable (e.g., DFS points)
historical_data = pd.read_csv('thedaily/gamelogs2023.csv')

# Calculate historical averages for each player's 'dfs_points'
player_historical_avg = historical_data.groupby(['player_name', 'pos'])['dfs_points'].mean().reset_index()
player_historical_avg.rename(columns={'dfs_points': 'player_avg_dfs_points'}, inplace=True)

# Calculate the average 'dfs_points' allowed by each opponent team
opp_team_impact = historical_data.groupby(['opp_team', 'pos'])['dfs_points'].mean().reset_index()
opp_team_impact.rename(columns={'dfs_points': 'opp_team_avg_allowed_dfs_points'}, inplace=True)

# Merge player historical averages
dk_lines = dk_lines.merge(player_historical_avg, left_on='Name', right_on='player_name', how='left')

# Merge opponent team impact # Extract opponent team abbreviation
dk_lines = dk_lines.merge(opp_team_impact, left_on=['Opponent', 'pos'], 
                          right_on=['opp_team','pos'], how='left')

dk_lines = dk_lines.dropna()

# Preprocess dk_lines to match the model's feature set
# Ensure that dk_lines has the same features as your model's input
dk_lines_features = dk_lines[['player_avg_dfs_points', 'opp_team_avg_allowed_dfs_points']]  # Include all necessary features


# Make predictions using the trained model
dk_lines['model_projections'] = gb_model_new.predict(dk_lines_features)

dk_lines.loc[dk_lines['Name'] == "Larry Nance Jr.", 'PlayerPosition'] = 'PF'
dk_lines.loc[dk_lines['Name'] == "Cason Wallace", 'PlayerPosition'] = 'PG'
dk_lines.loc[dk_lines['Name'] == "Cole Anthony", 'PlayerPosition'] = 'PG'
dk_lines.loc[dk_lines['Name'] == "Marcus Smart", 'PlayerPosition'] = 'PG'
dk_lines.loc[dk_lines['Name'] == "Talen Horton-Tucker", 'PlayerPosition'] = 'SF'
dk_lines.loc[dk_lines['Name'] == "Klay Thompson", 'PlayerPosition'] = 'SG'
dk_lines.loc[dk_lines['Name'] == "Larry Nance Jr.", 'PlayerPosition'] = 'PF'
dk_lines.loc[dk_lines['Name'] == "Bogdan Bogdanovic", 'PlayerPosition'] = 'SG'
dk_lines.loc[dk_lines['Name'] == "Grayson Allen", 'PlayerPosition'] = 'SG'
dk_lines.loc[dk_lines['Name'] == "Duncan Robinson", 'PlayerPosition'] = 'SG'
dk_lines.loc[dk_lines['Name'] == "Norman Powell", 'PlayerPosition'] = 'SG'
dk_lines.loc[dk_lines['Name'] == "Ochai Agbaji", 'PlayerPosition'] = 'SG'
dk_lines.loc[dk_lines['Name'] == "Kris Dunn", 'PlayerPosition'] = 'SG'
dk_lines.loc[dk_lines['Name'] == "Patrick Williams", 'PlayerPosition'] = 'PF'
dk_lines.loc[dk_lines['Name'] == "Nickeil Alexander-Walker", 'PlayerPosition'] = 'SG'


# Assign weights to the variables
weight_dfs_points = 2  # Higher weight for dfs_points
weight_ppm = 1     # Higher weight for ppm
weight_avg_pts = 2  # Lower weight for avg_pts
weight_mins = 1  # Lower weight for mins
weight_proj = 0
weight_rate = 1

# Calculate the weighted composite score
dk_lines['composite_score'] = (
    (dk_lines['opp_team_avg_allowed_dfs_points'] * weight_dfs_points) + 
    (dk_lines['ppm'] * weight_ppm) +
    (dk_lines['avg_pts'] * weight_avg_pts) + 
    (dk_lines['mins'] * weight_mins) +
    (dk_lines['model_projections'] * weight_proj) +
    (dk_lines['player_rate'] * weight_rate)
) / (weight_dfs_points + weight_ppm + weight_avg_pts + weight_mins + weight_proj + weight_rate)

dk_lines[dk_lines['composite_score'] > 0]

## remove teams that are not in slate
team = ['GSW', 'DET', 'TOR', 'SAC', 'MEM', 'LAL']

dk_lines = dk_lines[dk_lines['TeamAbbrev'].isin(team)]


dk_lines = dk_lines[dk_lines["Name"] != "Mitchell Robinson"]
dk_lines = dk_lines[dk_lines["Name"] != "Draymond Green"]
dk_lines = dk_lines[dk_lines["Name"] != "Talen Horton-Tucker"]
dk_lines = dk_lines[dk_lines["Name"] != "De'Andre Hunter"]
dk_lines = dk_lines[dk_lines["Name"] != "Darius Garland"]



def optimize_lineup(dk_lines_data, budget, n_players, team_val, pos_restriction, captain=False, weight_avg_pts=0.5, weight_comp_score=0.5):
    # Create a pulp linear programming model
    model = pulp.LpProblem("Optimize NBA Lineup", pulp.LpMaximize)

    # Create a decision variable for each player
    player_vars = pulp.LpVariable.dicts("Players", dk_lines_data['Name'], 0, 1, cat='Integer')

    # Create captain decision variable if captain is True
    captain_vars = {}
    if captain:
        captain_vars = pulp.LpVariable.dicts("Captain", dk_lines_data['Name'], 0, 1, cat='Integer')

    # Objective function: Maximize weighted sum of total avg_pts and total composite_score
    total_avg_pts = pulp.lpSum(player_vars[player] * dk_lines_data.loc[dk_lines_data['Name'] == player, 'avg_pts'].values[0] 
                               for player in dk_lines_data['Name'])
    total_comp_score = pulp.lpSum(player_vars[player] * dk_lines_data.loc[dk_lines_data['Name'] == player, 'composite_score'].values[0] 
                                  for player in dk_lines_data['Name'])

    if captain:
        total_avg_pts += 0.5 * pulp.lpSum(captain_vars[player] * dk_lines_data.loc[dk_lines_data['Name'] == player, 'avg_pts'].values[0] 
                                          for player in dk_lines_data['Name'])
        total_comp_score += 0.5 * pulp.lpSum(captain_vars[player] * dk_lines_data.loc[dk_lines_data['Name'] == player, 'composite_score'].values[0] 
                                            for player in dk_lines_data['Name'])

    model += weight_avg_pts * total_avg_pts + weight_comp_score * total_comp_score

    # Constraint: Total salary must not exceed the budget
    total_salary = pulp.lpSum(player_vars[player] * dk_lines_data.loc[dk_lines_data['Name'] == player, 'Salary'].values[0] 
                              for player in dk_lines_data['Name'])
    if captain:
        total_salary += 0.5 * pulp.lpSum(captain_vars[player] * dk_lines_data.loc[dk_lines_data['Name'] == player, 'Salary'].values[0] 
                                         for player in dk_lines_data['Name'])
    model += total_salary <= budget

    # Constraint for captain
    if captain:
        model += pulp.lpSum(captain_vars[player] for player in dk_lines_data['Name']) == 1

    # Position constraints
    if pos_restriction:
        for pos in ['PG', 'SG', 'SF', 'PF', 'C']:  # Assuming these are the positions
            eligible_players = dk_lines_data[dk_lines_data['PlayerPosition'].str.contains(pos)]['Name']
            model += pulp.lpSum(player_vars[player] for player in eligible_players) >= 1  # At least one player from each position
            model += pulp.lpSum(player_vars[player] for player in eligible_players) <= 2  # At most two players from each position

    # Team constraint: No more than team_val players from a single team
    for team in dk_lines_data['TeamAbbrev'].unique():
        team_players = dk_lines_data[dk_lines_data['TeamAbbrev'] == team]['Name']
        model += pulp.lpSum(player_vars[player] for player in team_players) <= team_val

    # Constraint: Exactly n_players must be selected
    model += pulp.lpSum(player_vars[player] for player in dk_lines_data['Name']) == n_players

    # Solve the model
    status = model.solve()
    if status != pulp.LpStatusOptimal:
        return "No optimal solution found"

    # Retrieve the optimal lineup and captain if applicable
    optimal_lineup = [player for player in dk_lines_data['Name'] if player_vars[player].value() == 1]
    optimal_players = dk_lines_data[dk_lines_data['Name'].isin(optimal_lineup)]

    if captain:
        optimal_captain = [player for player in dk_lines_data['Name'] if captain_vars[player].value() == 1]
        optimal_players['IsCaptain'] = optimal_players['Name'].isin(optimal_captain)
        optimal_players_selected = optimal_players[['Name', 'TeamAbbrev', 'PlayerPosition', 'model_projections', 'Salary', 'IsCaptain', 'avg_pts']]
    else:
        optimal_players_selected = optimal_players[['Name', 'TeamAbbrev', 'PlayerPosition', 'model_projections', 'Salary', 'avg_pts']]

    return optimal_players_selected


# Example usage
optimal_lineup = optimize_lineup(dk_lines, 50000, n_players = 8, team_val = 3, pos_restriction = True, captain = False)
optimal_lineup


