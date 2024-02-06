#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb  1 23:52:44 2024

@author: matthew
"""


import numpy as np
import pandas as pd
import requests
from bs4 import BeautifulSoup
import json


def action_scrape(sport=None, propnames=None):
    fulldf = pd.DataFrame()
    ## need header for access to site
    headers = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/98.0.4758.102 Safari/537.36',
    'Cache-Control': 'no-cache'}
    for propname in propnames:
        urlbuild = 'https://www.actionnetwork.com/'+sport+'/props/'
        url = urlbuild + propname
        r = requests.get(url, headers=headers)
        soup = BeautifulSoup(r.content, 'html.parser')
        ## get json
        parsed = soup.find_all('script')
        jstext = parsed[-1].string
        ## load entire json into pandas, drop unnecessary columns
        data = json.loads(jstext)
        #print( json.dumps(data, indent=2) )
        df_js = pd.json_normalize(data)

        ## get o/u corresponding value -- smaller corresponds to over, larger corresponds to under according to nba
        ## checker:
        #print(df_js.filter(regex='props.pageProps.initialMarketConfig.market.rules.options'))
        cols = df_js.filter(regex='props.pageProps.initialMarketConfig.market.rules.options').columns.tolist()
        ## remove prefix string, keep only the number, and save it to unique list to avoid dupes
        try:
            nums = []
            for i in range(len(cols)):
                nums.append(int(cols[i].replace('props.pageProps.initialMarketConfig.market.rules.options.', '')[0:2].replace('.','')))
                nums = np.unique(nums).tolist()
        except: pass

        ## keep relevant columns now that we have o/u numbers, expand the json
        keep = 'props.pageProps.initialMarketConfig.market.'
        cols_to_keep = [keep+'books',keep+'teams',keep+'players']
        df = df_js[cols_to_keep]
        df = df.copy()
        df.rename(columns={cols_to_keep[0]:'books', cols_to_keep[1]:'teams', cols_to_keep[2]:'players'}, inplace=True)

        ## create odds df
        odds = pd.json_normalize(df['books'][0], record_path='odds', meta='book_id')
        odds = odds[odds.columns.drop(list(odds.filter(regex='deeplink')))]
        # create o/u to be obvious -- its hidden in option type id -- smaller num corresponds to over, larger to under
        try:
            odds['ou'] = np.where(odds['option_type_id']==np.min(nums), 'over', 'under')
        except: pass

        ## get book names
        book_ids = odds.book_id.unique()
        bookdict= {}
        for i in book_ids:
            try:
                book = df_js['props.pageProps.bookMap.{}.display_name'.format(i)].values[0]
                if i not in bookdict:
                    bookdict[i] = book
            except: pass
        ## exception for other book id
        bookdict[15] = 'Best Odds'

        teams = pd.json_normalize(df['teams'][0])
        players = pd.json_normalize(df['players'][0])
        books = pd.DataFrame(bookdict.items(), columns=['book_id', 'book_name'])

        odds1 = odds.merge(books, on='book_id')
        odds2 = odds1.merge(teams[['id','display_name']], left_on='team_id', right_on='id')
        odds3 = odds2.merge(players[['id','full_name']], left_on='player_id', right_on='id')
        df = odds3[odds3.columns.drop(list(odds.filter(regex='id')))]
        df = df.drop(['id_x','id_y'], axis=1)
        df['prop'] = propname
        df = df.sort_values(by=['prop','full_name','book_name','is_best'], ascending=[True,True,True,False])
        fulldf = pd.concat([fulldf, df], ignore_index=True)
    return fulldf

## get action data for NBA props
nba_propnames = ['points', 'rebounds', 'assists', '3fgm']
name_dict = {'points':''}
action = action_scrape(sport='nba', propnames=nba_propnames)



filtered_action = action[(action['book_name'] == "Best Odds") & (action['ou'] == "over")][['full_name', 'prop', 'value', 'money', 'grade']].dropna(subset=['grade'])

filtered_action.to_csv("nba_props.csv")

