#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import janitor
import unidecode 
import time
import pandas as pd
import numpy as np
import os
import logging
from datetime import datetime, timedelta
import shutil
from typing import List, Optional, Dict, Tuple
from nba_api.stats.endpoints import (
    leaguegamefinder,
    boxscoretraditionalv2,
    commonplayerinfo,
    leaguedashteamstats
)
from nba_api.stats.static import players

class NBADataCollector:
    def __init__(self, season: str, output_dir: str = '.', espn_league_id: int = 9844077):
        """
        Initialize the NBA data collector.
        
        Args:
            season (str): Season in format 'YYYY-YY'
            output_dir (str): Directory to save output files
            espn_league_id (int): ESPN Fantasy Basketball league ID
        """
        self.season = season
        self.output_dir = output_dir
        self.espn_league_id = espn_league_id
        self.gamelogs_file = os.path.join(output_dir, f"gamelogs{season[:4]}.csv")
        self.games_file = os.path.join(output_dir, f"games{season[:4]}.csv")
        self.processed_file = os.path.join(output_dir, "daily_app_data.csv")
        self.roster_file = os.path.join(output_dir, "player_info.csv")
        
        # Create necessary directories
        os.makedirs(output_dir, exist_ok=True)
        os.makedirs(os.path.join(output_dir, 'backups'), exist_ok=True)
        os.makedirs(os.path.join(output_dir, 'logs'), exist_ok=True)
        
        # Setup logging
        self._setup_logging()

    def _setup_logging(self):
        """Set up logging with daily rotation."""
        log_file = os.path.join(
            self.output_dir,
            'logs',
            f'nba_collection_{datetime.now().strftime("%Y%m%d")}.log'
        )
        
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(levelname)s - %(message)s',
            handlers=[
                logging.FileHandler(log_file),
                logging.StreamHandler()
            ]
        )

    def create_backup(self):
        """Create a backup of existing data files."""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        backup_dir = os.path.join(self.output_dir, 'backups', timestamp)
        os.makedirs(backup_dir, exist_ok=True)

        for file in [self.gamelogs_file, self.games_file]:
            if os.path.exists(file):
                backup_file = os.path.join(backup_dir, os.path.basename(file))
                shutil.copy2(file, backup_file)
                logging.info(f"Created backup: {backup_file}")

    def get_league_players(self) -> pd.DataFrame:
        """Get all players from the league including their positions and roster status."""
        try:
            logging.info("Collecting league players data")
            from espn_api.basketball import League
            
            league = League(league_id=self.espn_league_id, year=2025)
            players = []
            
            # Get players from all teams
            for team in league.teams:
                for player in team.roster:
                    players.append({
                        'player_name': player.name,
                        'fantasy_team': team.team_name,
                        'pro_team': player.proTeam,
                        'position': player.position,
                        'injured': player.injured,
                        'injury_status': player.injuryStatus if hasattr(player, 'injuryStatus') else None,
                        'is_free_agent': False
                    })
            
            # Get free agents
            free_agents = league.free_agents(size = 1000)
            for player in free_agents:
                players.append({
                    'player_name': player.name,
                    'fantasy_team': 'Free Agent',
                    'pro_team': player.proTeam,
                    'position': player.position,
                    'injured': player.injured,
                    'injury_status': player.injuryStatus if hasattr(player, 'injuryStatus') else None,
                    'is_free_agent': True
                })
            
            df = pd.DataFrame(players)
            logging.info(f"Collected data for {len(df)} players ({df['is_free_agent'].sum()} free agents)")
            logging.info("Saving player info...")
            df.to_csv(self.roster_file, index=False)
            return df
            
        except Exception as e:
            logging.error(f"Error collecting league players: {str(e)}")
            logging.error("Error details: ", exc_info=True)
            return pd.DataFrame()

    def get_recent_games(self) -> Tuple[pd.DataFrame, List[str]]:
        """Get recent games data and IDs."""
        try:
            gamefinder = leaguegamefinder.LeagueGameFinder(
                season_nullable=self.season,
                league_id_nullable='00'
            )
            games_df = gamefinder.get_data_frames()[0]
            game_ids = games_df['GAME_ID'].astype(str).unique().tolist()
            return games_df, game_ids
        except Exception as e:
            logging.error(f"Error getting recent games: {str(e)}")
            return pd.DataFrame(), []

    def collect_game_data(self) -> bool:
        """Collect recent game data."""
        try:
            logging.info("Starting game data collection...")
            
            # Load existing data if available
            existing_data = None
            if os.path.exists(self.gamelogs_file):
                existing_data = pd.read_csv(self.gamelogs_file)
                logging.info(f"Loaded {len(existing_data)} existing records")
                existing_ids = existing_data['GAME_ID'].astype('str')
            
            # Get recent games
            games_df, recent_game_ids = self.get_recent_games()
            
            if not recent_game_ids:
                logging.info("No recent games found")
                return True

            # Filter out existing games
            # Before filtering
            
            logging.info(f"Sample existing IDs: {list(existing_ids)[:5]}")
            logging.info(f"Sample recent IDs: {recent_game_ids[:5]}")

            # Make sure both are in consistent format (10 digits with leading zeros)
            existing_ids = set(existing_data['GAME_ID'].astype(str).apply(
                lambda x: '00' + x if len(x) == 8 else ('0' + x if len(x) == 9 else x)
            )) if existing_data is not None else set()
            
            recent_game_ids = [('00' + gid if len(gid) == 8 else ('0' + gid if len(gid) == 9 else gid)) 
                               for gid in recent_game_ids]
            
            # After formatting
            logging.info(f"Sample formatted existing IDs: {list(existing_ids)[:5]}")
            logging.info(f"Sample formatted recent IDs: {recent_game_ids[:5]}")
            
            # Now filter
            new_game_ids = [gid for gid in recent_game_ids if gid not in existing_ids]
            logging.info(f"Found {len(new_game_ids)} new games out of {len(recent_game_ids)} total games")

            if not new_game_ids:
                logging.info("No new games to collect")
                return True

            # Collect new game data
            boxscores = []
            for game_id in new_game_ids:
                try:
                    logging.info(f"Collecting game {game_id}")
                    player_stats = boxscoretraditionalv2.BoxScoreTraditionalV2(
                        game_id=game_id,
                        timeout=50
                    )
                    df = player_stats.player_stats.get_data_frame()
                    df['GAME_ID'] = game_id
                    boxscores.append(df)
                    time.sleep(0.6)
                except Exception as e:
                    logging.error(f"Error collecting game {game_id}: {str(e)}")

            if not boxscores:
                logging.error("No new data collected")
                return False

            # Process and save new data
            new_data = pd.concat(boxscores, ignore_index=True)
            final_data = pd.concat([existing_data, new_data], ignore_index=True) if existing_data is not None else new_data
            final_data.to_csv(self.gamelogs_file, index=False)
            
            # Update games file
            games_df.to_csv(self.games_file, index=False)
            
            logging.info(f"Successfully collected {len(new_data)} new records")
            return True

        except Exception as e:
            logging.error(f"Error collecting game data: {str(e)}")
            return False

    def process_data(self) -> Optional[pd.DataFrame]:
        """Process collected data and calculate statistics."""
        try:
            logging.info("Starting data processing...")
            
            # Read data files
            gamelogs = pd.read_csv(self.gamelogs_file)
            nba_games = pd.read_csv(self.games_file)
            
            logging.info('Successfully read in files')
            
            # Log initial data size
            initial_size = len(gamelogs)
            logging.info(f"Initial number of records: {initial_size}")
            
            # Remove duplicates
            gamelogs = gamelogs.drop_duplicates(
                subset=['GAME_ID', 'PLAYER_NAME', 'TEAM_ID'],
                keep='last'  # Keep the last occurrence of any duplicate
            )
            
            # Log how many duplicates were removed
            final_size = len(gamelogs)
            removed_count = initial_size - final_size
            logging.info(f"Removed {removed_count} duplicate records")
            logging.info(f"Final number of records: {final_size}")
            
            # Fix GAME_ID format and ensure consistent data types
            gamelogs['GAME_ID'] = gamelogs['GAME_ID'].astype(str)
            nba_games['GAME_ID'] = nba_games['GAME_ID'].astype(str)
            gamelogs['TEAM_ID'] = gamelogs['TEAM_ID'].astype(str)
            nba_games['TEAM_ID'] = nba_games['TEAM_ID'].astype(str)

            # Format GAME_ID to ensure consistent length
            gamelogs['GAME_ID'] = gamelogs['GAME_ID'].apply(
                lambda x: '00' + x if len(x) == 8 else ('0' + x if len(x) == 9 else x)
            )
            nba_games['GAME_ID'] = nba_games['GAME_ID'].apply(
                lambda x: '00' + x if len(x) == 8 else ('0' + x if len(x) == 9 else x)
            )

            # Merge gamelogs with games data
            logging.info("Merging gamelogs with games data...")
            gamelogs = pd.merge(
                gamelogs,
                nba_games[['GAME_ID', 'TEAM_ID', 'TEAM_NAME', 'GAME_DATE']],
                on=['GAME_ID', 'TEAM_ID'],
                how='left'
            )

            logging.info("Calculating statistical metrics...")
            # Convert MIN to numeric, handling time format
            gamelogs['MIN'] = gamelogs['MIN'].astype(str).apply(
                lambda x: float(x.split(':')[0]) + float(x.split(':')[1])/60 if ':' in str(x) else float(x)
            )

            # Calculate fantasy points
            gamelogs['fan_pts'] = (2 * gamelogs['FGM'] - gamelogs['FGA'] +
                                 gamelogs['FTM'] - gamelogs['FTA'] +
                                 gamelogs['FG3M'] + 2 * gamelogs['REB'] +
                                 2 * gamelogs['AST'] + 3 * gamelogs['STL'] +
                                 3 * gamelogs['BLK'] - 2 * gamelogs['TO'] +
                                 gamelogs['PTS'])

            # Calculate advanced stats with error handling
            gamelogs['ftr'] = gamelogs.apply(lambda row: row['FTA'] / row['FGA'] if row['FGA'] != 0 else 0, axis=1)
            gamelogs['fg3_rate'] = gamelogs.apply(lambda row: row['FG3A'] / row['FGA'] if row['FGA'] != 0 else 0, axis=1)
            gamelogs['ts_percentage'] = gamelogs.apply(
                lambda row: row['PTS'] / (2 * (row['FGA'] + 0.44 * row['FTA'])) 
                if (row['FGA'] + 0.44 * row['FTA']) != 0 else 0, 
                axis=1
            )

            # Ensure PLAYER_NAME is string type
            gamelogs['PLAYER_NAME'] = gamelogs['PLAYER_NAME'].astype(str)

            # Convert date column to datetime
            gamelogs['GAME_DATE'] = pd.to_datetime(gamelogs['GAME_DATE'])

            logging.info("Calculating rolling statistics")
            # Group by player and calculate rolling stats
            stats_to_roll = ['PTS', 'MIN', 'AST', 'REB', 'FG3M', 'fan_pts']
            gamelogs = gamelogs.sort_values(['PLAYER_NAME', 'GAME_DATE'])
            
            for stat in stats_to_roll:
                gamelogs[f'rolling_{stat.lower()}'] = gamelogs.groupby('PLAYER_NAME')[stat].transform(
                    lambda x: x.rolling(window=4, min_periods=1).mean()
                )
                gamelogs[f'rolling_max_{stat.lower()}'] = gamelogs.groupby('PLAYER_NAME')[stat].transform(
                    lambda x: x.rolling(window=5, min_periods=1).max()
                )
                gamelogs[f'rolling_min_{stat.lower()}'] = gamelogs.groupby('PLAYER_NAME')[stat].transform(
                    lambda x: x.rolling(window=5, min_periods=1).min()
                )
                gamelogs[f'rolling_sd_{stat.lower()}'] = gamelogs.groupby('PLAYER_NAME')[stat].transform(
                    lambda x: x.rolling(window=5, min_periods=1).std()
                )

            logging.info("Adding league player data")
            # Add league player data
            league_players = self.get_league_players()
            
            
            gamelogs['PLAYER_NAME'] = gamelogs['PLAYER_NAME'].apply(lambda x: unidecode.unidecode(str(x)))
            league_players['player_name'] = league_players['player_name'].apply(lambda x: unidecode.unidecode(str(x)))

            
            if not league_players.empty:
                gamelogs = pd.merge(
                    gamelogs,
                    league_players,
                    left_on='PLAYER_NAME',
                    right_on='player_name',
                    how='left'
                )
                
            # Drop the redundant column
            gamelogs = gamelogs.drop('player_name', axis=1)
            
            # Clean column names
            gamelogs = janitor.clean_names(gamelogs)
            

            # Save processed data
            logging.info("Saving processed data")
            gamelogs.to_csv(self.processed_file, index=False)
            logging.info(f"Successfully saved processed data to {self.processed_file}")
            
            return gamelogs

        except Exception as e:
            logging.error(f"Error processing data: {str(e)}")
            logging.error(f"Error details: ", exc_info=True)
            return None

    def run_daily_collection(self):
        """Run the complete daily collection process."""
        try:
            logging.info(f"Starting daily collection for {datetime.now().strftime('%Y-%m-%d')}")
            
            # Create backup
            self.create_backup()
            
            # Collect game data
            if not self.collect_game_data():
                logging.error("Game data collection failed")
                return
            
            # Process data
            if self.process_data() is None:
                logging.error("Data processing failed")
                return
            
            logging.info("Daily collection completed successfully")
            
        except Exception as e:
            logging.error(f"Error in daily collection: {str(e)}")

def main():
    collector = NBADataCollector(
        season="2024-25", 
        output_dir="fantasybball",
        espn_league_id=9844077
    )
    collector.run_daily_collection()

#if __name__ == "__main__":
#    main()


    

    
    
    