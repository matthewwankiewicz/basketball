library(shiny)
library(ggplot2)
library(cowplot)
library(tidyverse)
library(tidyr)

# load game logs file
gamelogs <- read_csv("gamelogs2023.csv")

# Get unique player names
unique_players <- unique(gamelogs$player_name)

# Shiny App
ui <- fluidPage(
  titlePanel("NBA Player Report"),
  sidebarLayout(
    sidebarPanel(
      selectInput("player", "Select Player", choices = unique_players),
      actionButton("submit", "Generate Report")
    ),
    mainPanel(
      plotOutput("playerPlot"),
      tableOutput("reportTable"),
      # Add percentile table
      tableOutput("percentileTable")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$submit, {
    player <- input$player
    if (nchar(player) > 0) {
      # Extract player data
      player_data <- gamelogs %>%
        filter(player_name == player)
      
      # Separate rows for each position
      player_data <- player_data %>%
        separate_rows(pos, sep = ",")
      
      # Summary statistics for the player
      report <- gamelogs %>%
        separate_rows(pos, sep = ',') %>%
        group_by(player_name) %>%
        summarise(
          FanPts_per_game = mean(fan_pts, na.rm = TRUE),
          PTS_per_game = mean(pts, na.rm = TRUE),
          REB_per_game = mean(reb, na.rm = TRUE),
          AST_per_game = mean(ast, na.rm = TRUE),
          FTR = mean(ftr, na.rm = TRUE),
          REB_rate = mean(reb_rate, na.rm = TRUE),
          AST_rate = mean(ast_rate, na.rm = TRUE)) %>% 
        filter(player_name == player)
      
      # Percentiles for the player
      report_percentiles <- gamelogs %>%
        group_by(player_name) %>%
        summarise(
          FanPts_per_game = mean(fan_pts, na.rm = TRUE),
          PTS_per_game = mean(pts, na.rm = TRUE),
          REB_per_game = mean(reb, na.rm = TRUE),
          AST_per_game = mean(ast, na.rm = TRUE),
          FT_rate = mean(ftr, na.rm = TRUE),
          REB_rate = mean(reb_rate, na.rm = TRUE),
          AST_rate = mean(ast_rate, na.rm = TRUE),
          USG_pct = mean(usage_percentage, na.rm = T)) %>% 
        group_by(player_name) %>% 
        group_by(FanPts_percentile = ntile(FanPts_per_game, 100),
                 PTS_percentile = ntile(PTS_per_game, 100),
                 REB_percentile = ntile(REB_per_game, 100),
                 AST_percentile = ntile(AST_per_game, 100),
                 FTR_percentile = ntile(FT_rate, 100),
                 REB_rate_percentile = ntile(REB_rate, 100),
                 AST_rate_percentile = ntile(AST_rate, 100),
                 USG_rate_percentile = ntile(USG_pct, 100)) %>% 
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
      
      # Convert the player's headshot
      player_id <- player_data %>% 
        select(player_id) %>% pull() %>% .[1]
      
      headshot_link <- paste("https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/", player_id, ".png", sep = "")
      player_headshot <- magick::image_read(headshot_link)
      
      # Render the plot
      output$playerPlot <- renderPlot({
        plot_with_headshot <- ggdraw() +
          draw_plot(plot, width = 0.8) +
          draw_image(player_headshot, x = 0.8, y = 0.5, width = 0.25, height = 0.5)
        print(plot_with_headshot)
      })
      
      # Render the report table
      output$reportTable <- renderTable({
        report_table
      })
      
      # Render the percentile table
      output$percentileTable <- renderTable({
        # Include percentile table from the report_percentiles data frame
        report_percentiles
      })
    }
  })
}

shinyApp(ui, server)
