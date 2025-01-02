import pandas as pd
import numpy as np
from typing import Optional
import logging
from datetime import datetime

class NBADataProcessor:
    def __init__(self, output_dir: str = '.'):
        """
        Initialize the NBA data processor.
        
        Args:
            output_dir (str): Directory containing the data files
        """
        self.output_dir = output_dir
        self.positions_file = f"{output_dir}/nba_active_player_positions.csv"
        self.gamelogs_file = f"{output_dir}/gamelogs2024.csv"
        self.games_file = f"{output_dir}/games2024.csv"
        self.output_file = f"{output_dir}/daily_app_data.csv"

    def calculate_rolling_stats(self, df: pd.DataFrame, column: str, window: int = 4, 
                              stats: list = ['mean', 'max', 'min', 'std']) -> pd.DataFrame:
        """Calculate rolling statistics for a given column."""
        stats_dict = {
            'mean': lambda x: x.rolling(window=window, min_periods=1).mean(),
            'max': lambda x: x.rolling(window=window, min_periods=1).max(),
            'min': lambda x: x.rolling(window=window, min_periods=1).min(),
            'std': lambda x: x.rolling(window=window, min_periods=1).std()
        }
        
        for stat in stats:
            if stat in stats_dict:
                df[f'rolling_{stat}_{column}'] = df.groupby('player_name')[column].transform(stats_dict[stat])
        
        return df

    def process_data(self):
        """Process NBA game data and calculate statistics."""
        try:
            # Read data files
            gamelogs = pd.read_csv(self.gamelogs_file)
            nba_games = pd.read_csv(self.games_file)
            positions = pd.read_csv(self.positions_file)
            
            # Fix GAME_ID format
            gamelogs['GAME_ID'] = gamelogs['GAME_ID'].astype(str).apply(
                lambda x: '00' + x if len(x) == 8 else ('0' + x if len(x) == 9 else x)
            )

            # Process opponent data
            opp_cols = [col for col in nba_games.columns if not col.startswith('opp_')]
            nba_games_opponent = nba_games.copy()
            for col in opp_cols:
                if col not in ['GAME_ID', 'MATCHUP', 'TEAM_ID', 'TEAM_NAME', 'TEAM_ABBREVIATION']:
                    nba_games_opponent[f'opp_{col}'] = nba_games_opponent[col]

            # Merge games data
            merged_nba_games = pd.merge(
                nba_games,
                nba_games_opponent,
                on='GAME_ID',
                suffixes=('_team', '_opp')
            )
            
            # Filter different team matchups
            nba_games = merged_nba_games[
                merged_nba_games['TEAM_ABBREVIATION_team'] != merged_nba_games['TEAM_ABBREVIATION_opp']
            ]

            # Merge gamelogs with games data
            gamelogs = pd.merge(gamelogs, nba_games, on=['TEAM_ID', 'GAME_ID'])

            # Clean column names
            gamelogs.columns = [col.replace('.x', '') if '.x' in col 
                              else f'team_{col.replace(".y", "")}' if '.y' in col 
                              else col for col in gamelogs.columns]

            # Calculate basic statistics
            gamelogs['MIN'] = pd.to_numeric(gamelogs['MIN'].str.replace(':', '.'), errors='coerce')
            gamelogs['fan_pts'] = (2 * gamelogs['FGM'] - gamelogs['FGA'] + 
                                 gamelogs['FTM'] - gamelogs['FTA'] + 
                                 gamelogs['FG3M'] + 2 * gamelogs['REB'] + 
                                 2 * gamelogs['AST'] + 3 * gamelogs['STL'] + 
                                 3 * gamelogs['BLK'] - 2 * gamelogs['TOV'] + 
                                 gamelogs['PTS'])
            
            gamelogs['ftr'] = gamelogs['FTA'] / gamelogs['FGA']
            gamelogs['team_min'] = gamelogs['team_MIN'] / 5
            gamelogs['ast_rate'] = (100 * gamelogs['AST']) / (
                ((gamelogs['MIN']/(gamelogs['team_MIN']/5)) * gamelogs['team_FGM']) - gamelogs['FGM']
            )
            gamelogs['reb_rate'] = (100 * gamelogs['REB'] * gamelogs['team_MIN']) / (
                gamelogs['MIN'] * (gamelogs['team_REB'] + gamelogs['opp_REB'])
            )
            gamelogs['usage_percentage'] = ((gamelogs['FGA'] + 0.44 * gamelogs['FTA'] + gamelogs['TOV']) * 
                                          gamelogs['team_MIN']) / (gamelogs['MIN'] * (gamelogs['team_FGA'] + 
                                          0.44 * gamelogs['team_FTA'] + gamelogs['team_TOV']))
            gamelogs['ts_percentage'] = gamelogs['PTS'] / (2 * (gamelogs['FGA'] + 0.44 * gamelogs['FTA']))
            gamelogs['fg3_rate'] = gamelogs['FG3A'] / gamelogs['FGA']

            # Calculate double-doubles and triple-doubles
            stats = ['PTS', 'AST', 'REB', 'BLK', 'STL']
            gamelogs['double_double'] = 0
            gamelogs['triple_double'] = 0
            
            for i in range(len(stats)):
                for j in range(i+1, len(stats)):
                    mask = (gamelogs[stats[i]] > 10) & (gamelogs[stats[j]] > 10)
                    gamelogs.loc[mask, 'double_double'] = 1
                    
                    for k in range(j+1, len(stats)):
                        mask = (gamelogs[stats[i]] > 10) & (gamelogs[stats[j]] > 10) & (gamelogs[stats[k]] > 10)
                        gamelogs.loc[mask, 'triple_double'] = 1

            # Calculate rolling statistics
            gamelogs = gamelogs.sort_values(['player_name', 'GAME_DATE'])
            for stat in ['PTS', 'MIN', 'AST', 'REB', 'FG3M', 'fan_pts', 'usage_percentage', 'fg3_rate']:
                gamelogs = self.calculate_rolling_stats(gamelogs, stat)

            # Calculate current stats (next game stats)
            gamelogs['prev_game_date'] = gamelogs.groupby('PLAYER_NAME')['GAME_DATE'].shift(-1)
            gamelogs['days_rest'] = (pd.to_datetime(gamelogs['GAME_DATE']) - 
                                   pd.to_datetime(gamelogs['prev_game_date'])).dt.days - 1

            # Select and rename columns for final output
            columns_mapping = {
                'GAME_DATE': 'game_date',
                'PLAYER_NAME': 'player_name',
                'PLAYER_ID': 'player_id',
                'MIN': 'min',
                'position': 'pos'
                # Add other columns as needed
            }
            
            gamelogs_ref = gamelogs.rename(columns=columns_mapping)
            
            # Save processed data
            gamelogs_ref.to_csv(self.output_file, index=False)
            logging.info(f"Successfully saved processed data to {self.output_file}")
            
            return gamelogs_ref

        except Exception as e:
            logging.error(f"Error processing data: {str(e)}")
            return None

def main():
    processor = NBADataProcessor(output_dir="fantasybball")
    processor.process_data()

if __name__ == "__main__":
    main()