#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Oct 20 22:31:43 2024

@author: matthew
"""

from espn_api.basketball import League

## get league
league = League(league_id=9844077, year=2024)

## save list of free agents
free_agents = league.free_agents(size = 200)

## convert to dataframe
df = pd.DataFrame(free_agents, columns=['player_name'])


df.to_csv('free_agents.csv')
