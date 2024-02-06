#### monte carlo sims

simulate_stat <- \(player, opponent_rolling_stat, stat_name, stat_line, date_filter = Sys.Date(), stat_output = "prob"){
  ## filter for player's game logs
  player_logs <- gamelogs_ref %>% 
    filter(player_name == player, 
           game_date < date_filter) %>% 
    arrange(desc(game_date)) ## arange game dates to have most recent list first
  
  player_pos <- player_logs$pos[1]
  
  
  ## slice for last 3, 5, and 10 games
  last3_slice <- player_logs %>% 
    slice(1:3)
  
  
  last5_slice <- player_logs %>% 
    slice(1:5)
  

  
  last10_slice <- player_logs %>% 
    slice(1:7)
  

  
  ## get averages & sd for 3, 5, 10 game log
  last3_avg <- last3_slice[[stat_name]] %>% mean()
  last5_avg <- last5_slice[[stat_name]] %>% mean()
  last10_avg <- last10_slice[[stat_name]] %>% mean()
  
  last3_sd <- last3_slice[[stat_name]] %>% sd()
  last5_sd <- last5_slice[[stat_name]] %>% sd()
  last10_sd <- last10_slice[[stat_name]] %>% sd()
  
    
  
  ## run the simulations
  simmed <- c()
  simmed2 <- c()
  simmed3 <- c()
  set.seed(21)
  for(i in 1:10000){
    simmed[i] <- qnorm(runif(1), mean = last3_avg, sd = last3_sd) +
      qnorm(runif(1), mean = last3_avg, sd = last3_sd)*team_adj
    simmed2[i] <- qnorm(runif(1), mean = last5_avg, sd = last5_sd) +
      qnorm(runif(1), mean = last5_avg, sd = last5_sd)*team_adj
    simmed3[i] <- qnorm(runif(1), mean = last10_avg, sd = last10_sd) +
      qnorm(runif(1), mean = last10_avg, sd = last10_sd)*team_adj
  }
  combined <-  simmed
  
  ## return estimated point
  if(stat_output != "prob"){
    return(mean(combined))
  } else{
    return((sum(combined > stat_line))/10000) 
  }
}





monte_carlo_current.copy <- monte_carlo_current %>% 
  rowwise() %>% 
  mutate(simulation_result = simulate_stat(Name, opponent_rolling_rating_scaled, stat_line = stat_line, 
                                           stat_name = stat_name, date_filter = bet_date))



monte_carlo_current.copy %>%
  mutate(monte_pred = ifelse(simulation_result > 0.5, 1, 0),
         monte_correct = ifelse(monte_pred == over, 1, 0)) %>% 
  drop_na(monte_correct) %>% filter(stat_name == "ast", stat_line > 3) %>% 
  summarise(acc = mean(monte_correct),
            n = n()) %>% 
  mutate(roll = cummean(acc))


## no scaling acc: 56%
## half scale: 59.9%
## quarter scale: 64.4
## 8th scale: 67.5%
## 16th scale: 68.45%
## 32nd scale: 68.7
## 64th scale: 68.6