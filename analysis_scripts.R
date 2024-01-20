##### Scripts for finding stats for plots/tables #####
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(magick)

## Opponent stats against ####
## find stats vs team by position
gamelogs %>% 
  filter(min >= 25) %>% 
  separate_rows(pos, sep = ',') %>% 
  group_by(opp_team, pos) %>% 
  summarise(fan_pts = mean(pts)) %>% 
  view()


## get stat x and stat y and plot
gamelogs %>% 
  group_by(player_name) %>% 
  summarise(min = mean(min),
            fan_pts = sum(fan_pts),
            ftr = mean(ftr, na.rm = T),
            reb_rate = mean(reb_rate, na.rm = T),
            ast_rate = mean(ast_rate, na.rm = T)) %>% 
  filter(min >= 25) -> rate_stats
  
rate_stats %>%
  ggplot(aes(fan_pts, ftr)) +
  geom_point(aes(size = ast_rate)) +
  geom_text_repel(
    aes(label = player_name),
    box.padding = 0.5,
    point.padding = 0.5,
    force = 10,
    segment.color = "grey",
    data = rate_stats %>% filter(reb_rate >= 12.5),
    max.overlaps = 30  # Adjust max.overlaps to allow for more labels
  )


result <- gamelogs %>%
  group_by(player_name) %>%
  arrange(player_name, game_date) %>%
  mutate(rolling_avg_fan_pts = zoo::rollapply(fan_pts, width = 2, FUN = mean, align = "right", fill = NA)) %>%
  filter(!is.na(rolling_avg_fan_pts)) %>% 
  summarise(max_gain = last(rolling_avg_fan_pts) - first(rolling_avg_fan_pts))

# Filter for players with the biggest gains
top_gainers <- result %>%
  top_n(10, wt = max_gain)  # Adjust the number (10) based on your preference

# Filter gamelogs for the last 10 games for each top gainer
recent_games <- gamelogs %>%
  filter(player_name %in% top_gainers$player_name) %>%
  group_by(player_name) %>%
  slice_tail(n = recent_games_to_plot) %>%
  mutate(game_sequence = row_number())  # Create a variable for the game sequence

# Plot the rolling averages for the last 10 games for the biggest gainers
recent_games %>%
  ggplot(aes(x = game_sequence, y = fan_pts, color = player_name)) +
  geom_line(aes(y = zoo::rollapply(fan_pts, width = n, FUN = mean, align = "right", fill = NA, na.rm = TRUE)),
            size = 1, linetype = "dashed", show.legend = T) +
  labs(title = "Rolling Average of fan_pts for Top Gainers (Last 10 Games)",
       x = "Game Sequence",
       y = "fan_pts") +
  theme_minimal() +
  theme(legend.position = "top")


#### find player's rate of scoring 40+ fantasy points ####

gamelogs %>% 
  mutate(over_40 = ifelse(fan_pts>=40, 1, 0)) %>% 
  group_by(player_name) %>% 
  summarise(min = mean(min),
            gg_rate = mean(over_40)) %>% 
  filter(min >= 25)

### find average points scored per position ####

generate_report <- function(player) {
  player_data <- gamelogs %>%
    filter(player_name == player)
  
  # Separate rows for each position
  player_data <- player_data %>%
    separate_rows(pos, sep = ",")
  
  report <- gamelogs %>%
    separate_rows(pos, sep = ',') %>%
    group_by(player_name) %>%
    summarise(
      FanPts_per_game = mean(fan_pts, na.rm = TRUE),
      PTS_per_game = mean(pts, na.rm = TRUE),
      REB_per_game = mean(reb, na.rm = TRUE),
      AST_per_game = mean(ast, na.rm = TRUE)
    ) %>% 
    filter(player_name == player)
  
  report_percentiles <- gamelogs %>%
    group_by(player_name) %>%
    summarise(
      FanPts_per_game = mean(fan_pts, na.rm = TRUE),
      PTS_per_game = mean(pts, na.rm = TRUE),
      REB_per_game = mean(reb, na.rm = TRUE),
      AST_per_game = mean(ast, na.rm = TRUE)) %>% 
    group_by(player_name) %>% 
    group_by(FanPts_percentile = ntile(FanPts_per_game, 100),
             PTS_percentile = ntile(PTS_per_game, 100),
             REB_percentile = ntile(REB_per_game, 100),
             AST_percentile = ntile(AST_per_game, 100)) %>% 
    select(player_name, ends_with("percentile")) %>% 
    filter(player_name == player)
  
  # Calculate average points scored by position
  average_by_position <- gamelogs %>%
    separate_rows(pos, sep = ",") %>% 
    filter(min >= 20) %>% 
    group_by(pos) %>%
    summarise(average_points_by_position = mean(fan_pts, na.rm = TRUE))
  
  # Create a graph showing fantasy points scored per game by date for the specific player and average by position
  plot <- ggplot(player_data, aes(x = game_date, y = fan_pts, color = pos)) +
    geom_line(colour = "green") +
    geom_hline(data = average_by_position %>% filter(pos %in% player_data$pos),
               aes(yintercept = average_points_by_position, color = pos),
               linetype = "dashed", size = 1) +
    labs(title = paste("Fantasy Points Scored per Game by Date for", player),
         subtitle = "Dashed lines represent average points scored by position",
         x = "Game Date",
         y = "Fantasy Points") +
    theme_minimal() +
    theme(legend.position = "top")
  
  # Convert the report table to a data frame
  report_table <- as.data.frame(report)
  
  # Create headshot link
  player_id <- player_data %>% 
    select(player_id) %>% pull() %>% .[1]
  
  headshot_link <- paste("https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/", player_id, ".png", sep = "")
  
  # Read the headshot image
  player_headshot <- magick::image_read(headshot_link)
  
  # Convert the image to a raster
  player_headshot_raster <- magick::image_raster(player_headshot)
  
  # Combine the plot and the headshot using cowplot
  plot_with_headshot <- ggdraw() +
    draw_plot(plot, width = 0.8) +
    draw_image(player_headshot, x =.8, y = 0.5, width = 0.25, height = 0.5)

  
  # Display the combined plot and the table side by side
  grid.arrange(ggplotGrob(plot_with_headshot), tableGrob(report_table),
               tableGrob(report_percentiles), ncol = 1)
}


generate_report("Karl-Anthony Towns")


### Correlation stats ####

# Assuming your gamelogs data is already loaded
gamelogs_2 <- gamelogs %>% drop_na(reb_rate, ast_rate, ftr) %>% 
  filter(min >= 20)

# Calculate correlation matrix
correlation_matrix <- cor(gamelogs_2[, c("fan_pts", "pts", "reb", "ast", "stl", "blk", "to", "fg_pct", "fg3_pct", "ft_pct",
                                       "fga", "ftr", "reb_rate", "ast_rate", "usage_percentage",
                                       "ts_percentage")])

# Print the correlation matrix
print(correlation_matrix)

# Visualize the correlation matrix using a heatmap
library(corrplot)
corrplot(correlation_matrix, method = "color")

# You can also get a sorted list of correlations with Fan Points
correlation_with_fan_pts <- cor(gamelogs_2[, c("fan_pts", "pts", "reb", "ast", "stl", "blk", "to", "fg_pct", "fg3_pct", "ft_pct",
                                       "fga", "ftr", "reb_rate", "ast_rate", "usage_percentage",
                                       "ts_percentage")])["fan_pts", ]
sorted_correlations <- sort(correlation_with_fan_pts, decreasing = TRUE)

# Print the sorted correlations
print(sorted_correlations)


#### find rankings of players by position ####

gamelogs %>% 
  separate_rows(pos, sep = ",") %>%
  group_by(player_name, pos) %>% 
  summarise(fan_pts = mean(fan_pts, na.rm = TRUE)) %>% 
  group_by(pos) %>% 
  mutate(pos_rank = dense_rank(desc(fan_pts))) %>% view()


### find players whose fantasy points have increased the most 
#### over the course of the season using rolling average ####

# Assuming your gamelogs data is already loaded
gamelogs_2 <- gamelogs %>% drop_na(reb_rate, ast_rate, ftr) %>% 
  filter(min >= 20)

# Calculate rolling average of fantasy points scored

gamelogs_2 <- gamelogs_2 %>% 
  arrange(player_name, game_date) %>%
  group_by(player_name) %>% 
  mutate(pts_rolling_avg = zoo::rollmean(pts, 4, fill = NA, align = "right", na.rm = TRUE),
         fan_pts_rolling_avg = zoo::rollmean(fan_pts, 4, fill = NA, align = "right", na.rm = TRUE),
         reb_rolling_avg = zoo::rollmean(reb, 4, fill = NA, align = "right", na.rm = TRUE),
         ast_rolling_avg = zoo::rollmean(ast, 4, fill = NA, align = "right", na.rm = TRUE),
         stl_rolling_avg = zoo::rollmean(stl, 4, fill = NA, align = "right", na.rm = TRUE),
         blk_rolling_avg = zoo::rollmean(blk, 4, fill = NA, align = "right", na.rm = TRUE),
         to_rolling_avg = zoo::rollmean(to, 4, fill = NA, align = "right", na.rm = TRUE),
         ts_percent_rolling_avg = zoo::rollmean(ts_percentage, 4, fill = NA, align = "right", na.rm = TRUE),
         usage_percentage_rolling_avg = zoo::rollmean(usage_percentage, 4, fill = NA, align = "right", na.rm = TRUE),
         ast_rate_rolling_avg = zoo::rollmean(ast_rate, 4, fill = NA, align = "right", na.rm = TRUE),
         reb_rate_rolling_avg = zoo::rollmean(reb_rate, 4, fill = NA, align = "right", na.rm = TRUE),
         ftr_rolling_avg = zoo::rollmean(ftr, 4, fill = NA, align = "right", na.rm = TRUE))

# Find the player with the highest increase in fantasy points

gamelogs_2 %>% 
  group_by(player_name) %>% 
  drop_na(pts_rolling_avg) %>% 
  summarise(fan_pts_diff = last(usage_percentage_rolling_avg) - first(usage_percentage_rolling_avg)) %>% 
  arrange(desc(fan_pts_diff)) %>% 
  head(20) %>% 
  view()


### find percentile rankings of player's stats by position ####

# Assuming your gamelogs data is already loaded

gamelogs_2 <- gamelogs %>% drop_na(reb_rate, ast_rate, ftr) %>% 
  filter(min >= 20)

# Calculate percentile rankings of player stats by position

gamelogs_2 %>% 
  separate_rows(pos, sep = ",") %>%
  group_by(player_name, pos) %>% 
  summarise(avg_pts = mean(pts, na.rm = T),
            avg_fan_pts = mean(fan_pts, na.rm = T),
            avg_reb = mean(reb, na.rm = T),
            avg_ast = mean(ast, na.rm = T),
            avg_stl = mean(stl, na.rm = T),
            avg_blk = mean(blk, na.rm = T),
            avg_to = mean(to, na.rm = T),
            avg_ts_percent = mean(ts_percentage, na.rm = T),
            avg_usage_percent = mean(usage_percentage, na.rm = T),
            avg_ast_rate = mean(ast_rate, na.rm = T),
            avg_reb_rate = mean(reb_rate, na.rm = T),
            avg_ftr = mean(ftr, na.rm = T)) %>%
  group_by(pos) %>%
  mutate(pts_percentile = ntile(avg_pts, 100),
            fan_pts_percentile = ntile(avg_fan_pts, 100),
            reb_percentile = ntile(avg_reb, 100),
            ast_percentile = ntile(avg_ast, 100),
            stl_percentile = ntile(avg_stl, 100),
            blk_percentile = ntile(avg_blk, 100),
            to_percentile = ntile(avg_to, 100),
            ts_percent_percentile = ntile(avg_ts_percent, 100),
            usage_percent_percentile = ntile(avg_usage_percent, 100),
            ast_rate_percentile = ntile(avg_ast_rate, 100),
            reb_rate_percentile = ntile(avg_reb_rate, 100),
            ftr_percentile = ntile(avg_ftr, 100)) -> x

            
    
#### find players whose fantasy points have increased ####

# Assuming your gamelogs data is already loaded

gamelogs_2 <- gamelogs %>% drop_na(reb_rate, ast_rate, ftr) %>% 
  filter(min >= 20)

# Calculate rolling average of fantasy points scored

gamelogs_2 <- gamelogs_2 %>% 
  arrange(player_name, game_date) %>%
  group_by(player_name) %>% 
  mutate(pts_rolling_avg = zoo::rollmean(pts, 4, fill = NA, align = "right", na.rm = TRUE),
         fan_pts_rolling_avg = zoo::rollmean(fan_pts, 4, fill = NA, align = "right", na.rm = TRUE),
         reb_rolling_avg = zoo::rollmean(reb, 4, fill = NA, align = "right", na.rm = TRUE),
         ast_rolling_avg = zoo::rollmean(ast, 4, fill = NA, align = "right", na.rm = TRUE),
         stl_rolling_avg = zoo::rollmean(stl, 4, fill = NA, align = "right", na.rm = TRUE),
         blk_rolling_avg = zoo::rollmean(blk, 4, fill = NA, align = "right", na.rm = TRUE),
         to_rolling_avg = zoo::rollmean(to, 4, fill = NA, align = "right", na.rm = TRUE),
         ts_percent_rolling_avg = zoo::rollmean(ts_percentage, 4, fill = NA, align = "right", na.rm = TRUE),
         usage_percentage_rolling_avg = zoo::rollmean(usage_percentage, 4, fill = NA, align = "right", na.rm = TRUE),
         ast_rate_rolling_avg = zoo::rollmean(ast_rate, 4, fill = NA, align = "right", na.rm = TRUE),
         reb_rate_rolling_avg = zoo::rollmean(reb_rate, 4, fill = NA, align = "right", na.rm = TRUE),
         ftr_rolling_avg = zoo::rollmean(ftr, 4, fill = NA, align = "right", na.rm = TRUE))

# Find players whose fantasy points have increased

gamelogs_2 %>% 
  group_by(player_name) %>% 
  drop_na(pts_rolling_avg) %>% 
  summarise(fan_pts_diff = last(fan_pts_rolling_avg) - first(fan_pts_rolling_avg),
            has_increased = ifelse(fan_pts_diff > 0,
                                   1, 0),
            current_points = last(fan_pts_rolling_avg)) %>% 
  filter(has_increased == T) %>% 
  view()
