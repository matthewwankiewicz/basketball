#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Oct 20 19:36:00 2024

@author: matthew
"""
import pandas as pd
from nba_api.stats.endpoints import commonplayerinfo
from nba_api.stats.static import players

# Function to get player position using their player_id
def get_player_position(player_id):
    try:
        player_info = commonplayerinfo.CommonPlayerInfo(player_id=player_id)
        position = player_info.get_dict()['resultSets'][0]['rowSet'][0][15]  # Column index for position
        return position
    except:
        return "N/A"  # If the player info isn't available

# Get the list of all NBA players
all_players = players.get_players()

# List to store player data
player_data = []

# Loop through all players and get position data
for player in all_players:
    player_id = player['id']
    player_name = player['full_name']
    position = get_player_position(player_id)
    player_data.append({'Player Name': player_name, 'Position': position})

# Convert to DataFrame
df = pd.DataFrame(player_data)

# Save to CSV
df.to_csv('fantasybball/nba_player_positions.csv', index=False)


