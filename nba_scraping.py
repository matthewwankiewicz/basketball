#reticulate::use_python("/Users/matthew/anaconda3/bin/python3")
import time as time
import pandas as pd
import numpy as np
import os
import nba_api

from nba_api.stats.endpoints import leaguegamefinder
from nba_api.stats.endpoints import boxscoretraditionalv2
from nba_api.stats.endpoints import leaguedashteamstats

# Check if the CSV file already exists
if os.path.exists("gamelogs2023.csv"):
    existing_data = pd.read_csv("gamelogs2023.csv")
    existing_game_ids = existing_data['GAME_ID'].unique()
else:
    existing_game_ids = []

gamefinder = leaguegamefinder.LeagueGameFinder()

games = gamefinder.get_data_frames()[0]
games['GAME_DATE'] = pd.to_datetime(games['GAME_DATE'], format='%Y-%m-%d')


filtered_games = games[(games['SEASON_ID'] == "22023") & (games['GAME_DATE'] >= "2023-10-24")]

game_ids = filtered_games['GAME_ID'].unique().tolist()

existing_game_ids_str = [str(game_id) for game_id in existing_game_ids]

# Filter out game_ids that are not in the existing CSV file and add "00" to the beginning
game_ids_no_zeros = [game_id.lstrip("0") for game_id in game_ids]

# Filter out game_ids that are already in existing_game_ids (without leading zeros)
filtered_game_ids = [game_id for game_id in game_ids_no_zeros if game_id not in str(existing_game_ids)]

# Add back the "00" prefix to the filtered game_ids
filtered_game_ids_with_prefix = ["00" + game_id for game_id in filtered_game_ids]

# filter out strings longer than 10 char
filtered_game_ids_with_prefix = [game_id for game_id in filtered_game_ids_with_prefix if len(game_id) == 10]

boxscores = []

for game_id in filtered_game_ids_with_prefix:
    player_stats_data = boxscoretraditionalv2.BoxScoreTraditionalV2(game_id=game_id, timeout=50)
    time.sleep(0.1)
    df = player_stats_data.player_stats.get_data_frame()
    df['GAME_ID'] = game_id
    print("Downloading game logs for GAME_ID:", game_id)
    boxscores.append(df)

dfs = pd.concat(boxscores)

# Append the new data to the existing CSV file
total = pd.concat([existing_data, dfs])


total.to_csv("gamelogs2023.csv", index=False)

filtered_games.to_csv("games2023.csv", index=False)
quit
