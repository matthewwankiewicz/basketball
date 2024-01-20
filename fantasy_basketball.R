library(rvest)
library(tidyverse)
library(data.table)
library(caret)
library(car)

### loop through last 5 years
stats_list <- list()
for(i in 2018:2023){
   link <- paste0("https://www.basketball-reference.com/leagues/NBA_", i, "_totals.html")
   read_html(link) %>% html_table() %>% .[[1]] -> raw_table
   
   raw_table %>% 
     janitor::clean_names() %>% 
     mutate_all(as.character) %>% 
     mutate_at(vars(-c(player, pos, tm)), parse_number) %>% 
     drop_na(rk) %>% 
     select(-rk) %>% 
     mutate_all(as.character) %>% 
     mutate_at(vars(-c("tm", "player", "pos")), parse_number) %>%
     group_by(player) %>% 
     mutate(apps = n()) %>%
     mutate(has_multiple_apps = ifelse(apps > 1, TRUE, FALSE),
            season = i-1) -> nba_stats
   
   
   link <- paste0("https://www.basketball-reference.com/leagues/NBA_", i, "_advanced.html")
   read_html(link) %>% html_table() %>% .[[1]] -> raw_table_adv
   
   raw_table_adv %>% 
     janitor::clean_names() %>% 
     mutate_all(as.character) %>% 
     mutate_at(vars(-c(player, pos, tm)), parse_number) %>% 
     drop_na(rk) %>% 
     select(-rk) %>% 
     mutate_all(as.character) %>% 
     mutate_at(vars(-c("tm", "player", "pos")), parse_number) %>% 
     mutate(season = i-1) -> nba_adv_stats
   
   
   nba_total_stats <- merge(nba_stats,
                            nba_adv_stats) %>% 
     filter(!has_multiple_apps | tm == "tot") %>% 
     ungroup() %>% 
     mutate(fantasy_pts = 2*fg - fga + ft - fta + x3p + 2*trb +
              2*ast + 3*stl + 3*blk - 2*tov + pts,
            season = i-1) %>% 
     select(-c(has_multiple_apps, apps, x, x_2)) %>%
     arrange(desc(fantasy_pts)) -> stats_list[[i]]
}


Filter(Negate(is.null), stats_list) -> stats_list


total_stats <- do.call(rbind, stats_list)
total_stats <- data.table::data.table(total_stats) %>% 
  drop_na()


total_stats %>% 
  arrange(player, desc(season)) %>% 
  group_by(player) %>% 
  mutate(next_fpts = lag(fantasy_pts)) %>% 
  ungroup() %>% data.table::data.table() %>% 
  drop_na(next_fpts) %>% 
  filter(g > 40) -> total_stats_pred


lm(next_fpts ~ . - player - tm - season,
   data = total_stats_pred) -> fantasy_mod

fan_mod <- step(fantasy_mod)

test <- total_stats[season == 2022]
test$pred <- predict(fan_mod, newdata = test)



vif(fan_mod)


fan_mod_vif <- lm(next_fpts ~ age + g + fg_percent + x3p + drb + stl + tov + 
                    pf + ts_percent + f_tr + usg_percent + dws + dbpm,
                  data  = total_stats_pred)


test$pred_vif <- predict(fan_mod_vif, newdata = test)


vif(fan_mod_vif)

test %>% 
  select(player, pos, pred_vif) -> fantasy_proj_total


total_stats %>% 
  group_by(pos, season) %>% 
  summarise(avg_pts = mean(fantasy_pts)) %>% view()









total_stats_pg <- do.call(rbind, stats_list)
total_stats_pg <- data.table::data.table(total_stats_pg) %>% 
  mutate(fppg = fantasy_pts/g) %>% 
  drop_na()


total_stats_pg %>% 
  arrange(player, desc(season)) %>% 
  group_by(player) %>% 
  mutate(next_fpts = lag(fppg)) %>% 
  ungroup() %>% data.table::data.table() %>% 
  drop_na(next_fpts) %>% 
  filter(g > 40) -> total_stats_pred_pg


lm(next_fpts ~ . - player - tm - season - fppg,
   data = total_stats_pred_pg) -> fantasy_mod

fan_mod_pg <- step(fantasy_mod)

test_pg <- total_stats_pg[season == 2022]
test_pg$pred <- predict(fan_mod_pg, newdata = test_pg)



vif(fan_mod_pg)


fan_mod_vif_pg <- lm(next_fpts ~ age + g + drb + ast + stl + per + ts_percent + 
                       f_tr + blk_percent + usg_percent + ws_48 + vorp,
                  data  = total_stats_pred_pg)


test_pg$pred_vif <- predict(fan_mod_vif_pg, newdata = test_pg)


vif(fan_mod_vif)


test_pg %>% 
  select(player, pos, "pred_per_game" = pred_vif) %>% 
  merge(fantasy_proj_total %>% rename("pred_total" = pred_vif),
        by = c("player", "pos")) %>% 
  mutate(total_rank = rank(-pred_total),
         pg_rank = rank(-pred_per_game),
         adj_rank = (total_rank + pg_rank)/2) %>% 
  select(player, pos, adj_rank) %>% 
  mutate(adj_rank = rank(adj_rank)) %>% 
  arrange(adj_rank) -> final_ranking


plot(fan_mod_vif_pg)

write_csv(final_ranking, "rankings.csv")





