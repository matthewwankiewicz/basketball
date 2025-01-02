#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jan  1 19:33:38 2024

@author: matthew
"""

import pandas as pd
import pulp

dk_lines = pd.read_csv("nhl_lineups.csv")

teams = ['ANH', 'TOR']

dk_lines = dk_lines[dk_lines['TeamAbbrev'].isin(teams)]

dk_lines = dk_lines[dk_lines['Name'] != "Martin Jones"]
dk_lines = dk_lines[dk_lines['Name'] != "Lukas Dostal"]
dk_lines = dk_lines[dk_lines['Name'] != "Darcy Kuemper"]
dk_lines = dk_lines[dk_lines['Name'] != "John Gibson"]
dk_lines = dk_lines[dk_lines['Name'] != "Vitek Vanecek"]
dk_lines = dk_lines[dk_lines['Name'] != "Dougie Hamilton"]

def optimize_nhl_lineup(budget):
    # Create a pulp linear programming model
    model = pulp.LpProblem("Optimize NHL Lineup", pulp.LpMaximize)

    # Create a decision variable for each player
    player_vars = pulp.LpVariable.dicts("Players", dk_lines['Name'], 0, 1, cat='Integer')

    # Objective function: Maximize total composite score
    model += pulp.lpSum(player_vars[player] * dk_lines.loc[dk_lines['Name'] == player, 'AvgPointsPerGame'].values[0] 
                        for player in dk_lines['Name'])

    # Constraint: Total salary must not exceed the budget
    model += pulp.lpSum(player_vars[player] * dk_lines.loc[dk_lines['Name'] == player, 'Salary'].values[0] 
                        for player in dk_lines['Name']) <= budget

    # Position constraints for NHL
    # Goalies (G)
    goalies = dk_lines[dk_lines['Position'] == 'G']['Name']
    model += pulp.lpSum(player_vars[player] for player in goalies) == 1

    # Defensemen (D)
    defensemen = dk_lines[dk_lines['Position'] == 'D']['Name']
    model += pulp.lpSum(player_vars[player] for player in defensemen) >= 2

    # Centers (C)
    centers = dk_lines[dk_lines['Position'] == 'C']['Name']
    model += pulp.lpSum(player_vars[player] for player in centers) >= 2

    # Wingers (LW/RW)
    wingers = dk_lines[dk_lines['Position'].isin(['LW', 'RW'])]['Name']
    model += pulp.lpSum(player_vars[player] for player in wingers) >= 2

    # Adjust total players constraint for NHL lineup (adjust the number as needed)
    model += pulp.lpSum(player_vars[player] for player in dk_lines['Name']) == 9
    
    for team in dk_lines['TeamAbbrev'].unique():
        team_players = dk_lines[dk_lines['TeamAbbrev'] == team]['Name']
        model += pulp.lpSum(player_vars[player] for player in team_players) <= 3

    # Solve the model
    status = model.solve()
    if status != pulp.LpStatusOptimal:
        return "No optimal solution found"

    # Retrieve the optimal lineup
    optimal_lineup = [player for player in dk_lines['Name'] if player_vars[player].value() == 1]
    optimal_players = dk_lines[dk_lines['Name'].isin(optimal_lineup)]

    # Select only the specified columns
    return optimal_players[['Name', 'TeamAbbrev', 'Position', 'AvgPointsPerGame', 'Salary']]

# Example usage
optimal_nhl_lineup = optimize_nhl_lineup(50000)
print(optimal_nhl_lineup)
