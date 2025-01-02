# app.R
library(shiny)
library(dplyr)
library(lubridate)
library(tidyverse)
library(DT)
library(plotly)
library(stringdist)

# Function to standardize player names
standardize_name <- function(name) {
  name %>%
    str_to_lower() %>%
    str_replace_all("[.'\\-]", "") %>%
    str_replace_all("jr$|iii$|ii$", "") %>%
    str_trim()
}

# UI Definition
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  titlePanel("NBA Player Performance Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("window_size",
                   "Analysis Window (games):",
                   value = 5,
                   min = 2,
                   max = 20),
      
      selectInput("metric",
                  "Select Metric:",
                  choices = c("Minutes" = "mins",
                              "Points" = "pts",
                              "Usage %" = "usg",
                              "Assists" = "ast",
                              "Rebounds" = "reb",
                              "3-Pointers" = "fg3m",
                              "Fantasy Points" = "dfs")),
      
      selectInput("position",
                  "Filter by Position:",
                  choices = c("All", "Guard", "Forward", "Center", 
                              "Guard-Forward", "Forward-Center"),
                  selected = "All"),
      
      checkboxInput("free_agents_only",
                    "Show Free Agents Only",
                    value = TRUE),
      
      numericInput("min_games",
                   "Minimum Games Played:",
                   value = 3,
                   min = 1,
                   max = 20),
      
      numericInput("top_n",
                   "Number of Players to Show:",
                   value = 25,
                   min = 5,
                   max = 50),
      
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Performance Changes",
                 fluidRow(
                   column(12,
                          tags$div(
                            style = "display: flex; justify-content: space-between; align-items: center;",
                            tags$div(
                              style = "flex-grow: 1;",
                              textOutput("player_count")
                            ),
                            downloadButton("download_data", "Download Data")
                          ),
                          tags$hr()
                   )
                 ),
                 fluidRow(
                   column(6,
                          h4("Top Risers"),
                          DTOutput("risers_table")),
                   column(6,
                          h4("Top Fallers"),
                          DTOutput("fallers_table"))
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          h4("Trend Analysis"),
                          plotlyOutput("trend_plot", height = "600px"))
                 )),
        
        tabPanel("Player Details",
                 fluidRow(
                   column(12,
                          h4("Player Statistics"),
                          DTOutput("player_stats"))
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          h4("Performance Distribution"),
                          plotlyOutput("distribution_plot"))
                 )),
        
        tabPanel("Volatility Analysis",
                 fluidRow(
                   column(12,
                          h4("Player Volatility Rankings"),
                          DTOutput("volatility_table"))
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          h4("Volatility vs Performance"),
                          plotlyOutput("volatility_plot"))
                 ))
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Load data
  load_data <- reactive({
    df <- read_csv("fantasybball/daily_app_data.csv") %>%
      filter(game_date >= "2024-10-22")
    
    free_agents <- read_csv("fantasybball/free_agents.csv") %>%
      mutate(
        player_name = sub("Player\\((.*?)\\)", "\\1", player_name),
        std_name = standardize_name(player_name)
      )
    
    df_processed <- df %>%
      mutate(std_name = standardize_name(player_name))
    
    matches <- lapply(unique(df_processed$std_name), function(name) {
      distances <- stringdist::stringsim(name, free_agents$std_name, method = "jw")
      is_match <- max(distances) >= 0.85
      data.frame(std_name = name, is_free_agent = is_match)
    }) %>% bind_rows()
    
    df_processed %>%
      left_join(matches, by = "std_name") %>%
      select(-std_name)
  })
  
  # Filtered data
  data <- reactive({
    df <- load_data()
    
    if (input$position != "All") {
      df <- df %>% filter(pos == input$position)
    }
    
    if (input$free_agents_only) {
      df <- df %>% filter(is_free_agent)
    }
    
    df %>%
      group_by(player_name) %>%
      filter(n() >= input$min_games) %>%
      ungroup()
  })
  
  # Display player count
  output$player_count <- renderText({
    data <- data()
    player_count <- length(unique(data$player_name))
    status <- if(input$free_agents_only) "free agent" else "total"
    sprintf("Analyzing %d %s players", player_count, status)
  })
  
  # Calculate changes
  changes <- reactive({
    req(data())
    
    data() %>%
      arrange(player_name, game_date) %>%
      group_by(player_name) %>%
      mutate(
        current_value = get(paste0("rolling_", input$metric)),
        prev_value = lag(current_value, input$window_size),
        change = current_value - prev_value
      ) %>%
      filter(!is.na(change)) %>%
      ungroup()
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("player_analysis_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      download_data <- changes() %>%
        group_by(player_name) %>%
        arrange(desc(game_date)) %>%
        slice(1) %>%
        ungroup() %>%
        select(player_name, pos, game_date, change, current_value, is_free_agent)
      
      write.csv(download_data, file, row.names = FALSE)
    }
  )
  
  # Risers table
  output$risers_table <- renderDT({
    req(changes())
    
    changes() %>%
      group_by(player_name) %>%
      arrange(desc(game_date)) %>%
      slice(1) %>%
      ungroup() %>%
      arrange(desc(change)) %>%
      slice_head(n = input$top_n) %>%
      filter(change > 0) %>%
      select(Player = player_name,
             Position = pos,
             Date = game_date,
             Change = change,
             Current = current_value) %>%
      datatable(options = list(pageLength = 10),
                rownames = FALSE) %>%
      formatRound(columns = c("Change", "Current"), digits = 2)
  })
  
  # Fallers table
  output$fallers_table <- renderDT({
    req(changes())
    
    changes() %>%
      group_by(player_name) %>%
      arrange(desc(game_date)) %>%
      slice(1) %>%
      ungroup() %>%
      arrange(change) %>%
      slice_head(n = input$top_n) %>%
      filter(change < 0) %>%
      select(Player = player_name,
             Position = pos,
             Date = game_date,
             Change = change,
             Current = current_value) %>%
      datatable(options = list(pageLength = 10),
                rownames = FALSE) %>%
      formatRound(columns = c("Change", "Current"), digits = 2)
  })
  
  # Trend plot
  output$trend_plot <- renderPlotly({
    req(changes())
    
    top_changers <- changes() %>%
      group_by(player_name) %>%
      arrange(desc(game_date)) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(abs_change = abs(change)) %>%
      arrange(desc(abs_change)) %>%
      slice_head(n = 10) %>%
      pull(player_name)
    
    plot_data <- changes() %>%
      filter(player_name %in% top_changers)
    
    plot_ly(plot_data,
            x = ~game_date,
            y = ~current_value,
            color = ~player_name,
            type = 'scatter',
            mode = 'lines+markers',
            hovertemplate = paste(
              "Player: %{text}<br>",
              "Date: %{x}<br>",
              "Value: %{y:.1f}<br>",
              "<extra></extra>"
            ),
            text = ~player_name) %>%
      layout(title = "Trend Analysis (Top 10 Changers)",
             xaxis = list(title = "Date"),
             yaxis = list(title = input$metric),
             showlegend = TRUE)
  })
  
  # Player stats table
  output$player_stats <- renderDT({
    req(data())
    
    data() %>%
      group_by(player_name) %>%
      summarise(
        Position = first(pos),
        Games = n(),
        `Avg Minutes` = mean(rolling_mins, na.rm = TRUE),
        `Avg Points` = mean(rolling_pts, na.rm = TRUE),
        `Avg Fantasy` = mean(rolling_dfs, na.rm = TRUE),
        `Max Fantasy` = max(rolling_max_dfs, na.rm = TRUE),
        `Min Fantasy` = min(rolling_min_dfs, na.rm = TRUE)
      ) %>%
      arrange(desc(`Avg Fantasy`)) %>%
      datatable(options = list(pageLength = 25),
                rownames = FALSE) %>%
      formatRound(columns = c("Avg Minutes", "Avg Points", "Avg Fantasy",
                              "Max Fantasy", "Min Fantasy"),
                  digits = 1)
  })
  
  # Volatility table
  output$volatility_table <- renderDT({
    req(data())
    
    data() %>%
      group_by(player_name) %>%
      summarise(
        Position = first(pos),
        Games = n(),
        `Avg Points` = mean(rolling_pts, na.rm = TRUE),
        `Points SD` = mean(rolling_sd_pts, na.rm = TRUE),
        `Avg Fantasy` = mean(rolling_dfs, na.rm = TRUE),
        `Fantasy SD` = mean(rolling_sd_dfs, na.rm = TRUE)
      ) %>%
      arrange(desc(`Fantasy SD`)) %>%
      datatable(options = list(pageLength = 25),
                rownames = FALSE) %>%
      formatRound(columns = c("Avg Points", "Points SD",
                              "Avg Fantasy", "Fantasy SD"),
                  digits = 1)
  })
  
  # Volatility plot
  output$volatility_plot <- renderPlotly({
    req(data())
    
    plot_data <- data() %>%
      group_by(player_name) %>%
      summarise(
        position = first(pos),
        avg_value = mean(get(paste0("rolling_", input$metric)), na.rm = TRUE),
        volatility = mean(get(paste0("rolling_sd_", input$metric)), na.rm = TRUE)
      ) %>%
      filter(!is.na(volatility), !is.na(avg_value))
    
    plot_ly(plot_data,
            x = ~avg_value,
            y = ~volatility,
            color = ~position,
            type = 'scatter',
            mode = 'markers',
            text = ~player_name,
            hovertemplate = paste(
              "Player: %{text}<br>",
              "Average: %{x:.1f}<br>",
              "Volatility: %{y:.1f}<br>",
              "<extra></extra>"
            )) %>%
      layout(title = "Performance vs Volatility",
             xaxis = list(title = paste("Average", input$metric)),
             yaxis = list(title = "Volatility (Standard Deviation)"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)# app.R
library(shiny)
library(dplyr)
library(lubridate)
library(tidyverse)
library(DT)
library(plotly)
library(stringdist)

# Function to standardize player names
standardize_name <- function(name) {
  name %>%
    str_to_lower() %>%
    str_replace_all("[.'\\-]", "") %>%
    str_replace_all("jr$|iii$|ii$", "") %>%
    str_trim()
}

# UI Definition
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  titlePanel("NBA Player Performance Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("window_size",
                   "Analysis Window (games):",
                   value = 5,
                   min = 2,
                   max = 20),
      
      selectInput("metric",
                  "Select Metric:",
                  choices = c("Minutes" = "mins",
                              "Points" = "pts",
                              "Usage %" = "usg",
                              "Assists" = "ast",
                              "Rebounds" = "reb",
                              "3-Pointers" = "fg3m",
                              "Fantasy Points" = "dfs")),
      
      selectInput("position",
                  "Filter by Position:",
                  choices = c("All", "Guard", "Forward", "Center", 
                              "Guard-Forward", "Forward-Center"),
                  selected = "All"),
      
      checkboxInput("free_agents_only",
                    "Show Free Agents Only",
                    value = TRUE),
      
      numericInput("min_games",
                   "Minimum Games Played:",
                   value = 3,
                   min = 1,
                   max = 20),
      
      numericInput("top_n",
                   "Number of Players to Show:",
                   value = 25,
                   min = 5,
                   max = 50),
      
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Performance Changes",
                 fluidRow(
                   column(12,
                          tags$div(
                            style = "display: flex; justify-content: space-between; align-items: center;",
                            tags$div(
                              style = "flex-grow: 1;",
                              textOutput("player_count")
                            ),
                            downloadButton("download_data", "Download Data")
                          ),
                          tags$hr()
                   )
                 ),
                 fluidRow(
                   column(6,
                          h4("Top Risers"),
                          DTOutput("risers_table")),
                   column(6,
                          h4("Top Fallers"),
                          DTOutput("fallers_table"))
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          h4("Trend Analysis"),
                          plotlyOutput("trend_plot", height = "600px"))
                 )),
        
        tabPanel("Player Details",
                 fluidRow(
                   column(12,
                          h4("Player Statistics"),
                          DTOutput("player_stats"))
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          h4("Performance Distribution"),
                          plotlyOutput("distribution_plot"))
                 )),
        
        tabPanel("Volatility Analysis",
                 fluidRow(
                   column(12,
                          h4("Player Volatility Rankings"),
                          DTOutput("volatility_table"))
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          h4("Volatility vs Performance"),
                          plotlyOutput("volatility_plot"))
                 ))
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Load data
  load_data <- reactive({
    df <- read_csv("fantasybball/daily_app_data.csv") %>%
      filter(game_date >= "2024-10-22")
    
    free_agents <- read_csv("fantasybball/free_agents.csv") %>%
      mutate(
        player_name = sub("Player\\((.*?)\\)", "\\1", player_name),
        std_name = standardize_name(player_name)
      )
    
    df_processed <- df %>%
      mutate(std_name = standardize_name(player_name))
    
    matches <- lapply(unique(df_processed$std_name), function(name) {
      distances <- stringdist::stringsim(name, free_agents$std_name, method = "jw")
      is_match <- max(distances) >= 0.85
      data.frame(std_name = name, is_free_agent = is_match)
    }) %>% bind_rows()
    
    df_processed %>%
      left_join(matches, by = "std_name") %>%
      select(-std_name)
  })
  
  # Filtered data
  data <- reactive({
    df <- load_data()
    
    if (input$position != "All") {
      df <- df %>% filter(pos == input$position)
    }
    
    if (input$free_agents_only) {
      df <- df %>% filter(is_free_agent)
    }
    
    df %>%
      group_by(player_name) %>%
      filter(n() >= input$min_games) %>%
      ungroup()
  })
  
  # Display player count
  output$player_count <- renderText({
    data <- data()
    player_count <- length(unique(data$player_name))
    status <- if(input$free_agents_only) "free agent" else "total"
    sprintf("Analyzing %d %s players", player_count, status)
  })
  
  # Calculate changes
  changes <- reactive({
    req(data())
    
    data() %>%
      arrange(player_name, game_date) %>%
      group_by(player_name) %>%
      mutate(
        current_value = get(paste0("rolling_", input$metric)),
        prev_value = lag(current_value, input$window_size),
        change = current_value - prev_value
      ) %>%
      filter(!is.na(change)) %>%
      ungroup()
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("player_analysis_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      download_data <- changes() %>%
        group_by(player_name) %>%
        arrange(desc(game_date)) %>%
        slice(1) %>%
        ungroup() %>%
        select(player_name, pos, game_date, change, current_value, is_free_agent)
      
      write.csv(download_data, file, row.names = FALSE)
    }
  )
  
  # Risers table
  output$risers_table <- renderDT({
    req(changes())
    
    changes() %>%
      group_by(player_name) %>%
      arrange(desc(game_date)) %>%
      slice(1) %>%
      ungroup() %>%
      arrange(desc(change)) %>%
      slice_head(n = input$top_n) %>%
      filter(change > 0) %>%
      select(Player = player_name,
             Position = pos,
             Date = game_date,
             Change = change,
             Current = current_value) %>%
      datatable(options = list(pageLength = 10),
                rownames = FALSE) %>%
      formatRound(columns = c("Change", "Current"), digits = 2)
  })
  
  # Fallers table
  output$fallers_table <- renderDT({
    req(changes())
    
    changes() %>%
      group_by(player_name) %>%
      arrange(desc(game_date)) %>%
      slice(1) %>%
      ungroup() %>%
      arrange(change) %>%
      slice_head(n = input$top_n) %>%
      filter(change < 0) %>%
      select(Player = player_name,
             Position = pos,
             Date = game_date,
             Change = change,
             Current = current_value) %>%
      datatable(options = list(pageLength = 10),
                rownames = FALSE) %>%
      formatRound(columns = c("Change", "Current"), digits = 2)
  })
  
  # Trend plot
  output$trend_plot <- renderPlotly({
    req(changes())
    
    top_changers <- changes() %>%
      group_by(player_name) %>%
      arrange(desc(game_date)) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(abs_change = abs(change)) %>%
      arrange(desc(abs_change)) %>%
      slice_head(n = 10) %>%
      pull(player_name)
    
    plot_data <- changes() %>%
      filter(player_name %in% top_changers)
    
    plot_ly(plot_data,
            x = ~game_date,
            y = ~current_value,
            color = ~player_name,
            type = 'scatter',
            mode = 'lines+markers',
            hovertemplate = paste(
              "Player: %{text}<br>",
              "Date: %{x}<br>",
              "Value: %{y:.1f}<br>",
              "<extra></extra>"
            ),
            text = ~player_name) %>%
      layout(title = "Trend Analysis (Top 10 Changers)",
             xaxis = list(title = "Date"),
             yaxis = list(title = input$metric),
             showlegend = TRUE)
  })
  
  # Player stats table
  output$player_stats <- renderDT({
    req(data())
    
    data() %>%
      group_by(player_name) %>%
      summarise(
        Position = first(pos),
        Games = n(),
        `Avg Minutes` = mean(rolling_mins, na.rm = TRUE),
        `Avg Points` = mean(rolling_pts, na.rm = TRUE),
        `Avg Fantasy` = mean(rolling_dfs, na.rm = TRUE),
        `Max Fantasy` = max(rolling_max_dfs, na.rm = TRUE),
        `Min Fantasy` = min(rolling_min_dfs, na.rm = TRUE)
      ) %>%
      arrange(desc(`Avg Fantasy`)) %>%
      datatable(options = list(pageLength = 25),
                rownames = FALSE) %>%
      formatRound(columns = c("Avg Minutes", "Avg Points", "Avg Fantasy",
                              "Max Fantasy", "Min Fantasy"),
                  digits = 1)
  })
  
  # Volatility table
  output$volatility_table <- renderDT({
    req(data())
    
    data() %>%
      group_by(player_name) %>%
      summarise(
        Position = first(pos),
        Games = n(),
        `Avg Points` = mean(rolling_pts, na.rm = TRUE),
        `Points SD` = mean(rolling_sd_pts, na.rm = TRUE),
        `Avg Fantasy` = mean(rolling_dfs, na.rm = TRUE),
        `Fantasy SD` = mean(rolling_sd_dfs, na.rm = TRUE)
      ) %>%
      arrange(desc(`Fantasy SD`)) %>%
      datatable(options = list(pageLength = 25),
                rownames = FALSE) %>%
      formatRound(columns = c("Avg Points", "Points SD",
                              "Avg Fantasy", "Fantasy SD"),
                  digits = 1)
  })
  
  # Volatility plot
  output$volatility_plot <- renderPlotly({
    req(data())
    
    plot_data <- data() %>%
      group_by(player_name) %>%
      summarise(
        position = first(pos),
        avg_value = mean(get(paste0("rolling_", input$metric)), na.rm = TRUE),
        volatility = mean(get(paste0("rolling_sd_", input$metric)), na.rm = TRUE)
      ) %>%
      filter(!is.na(volatility), !is.na(avg_value))
    
    plot_ly(plot_data,
            x = ~avg_value,
            y = ~volatility,
            color = ~position,
            type = 'scatter',
            mode = 'markers',
            text = ~player_name,
            hovertemplate = paste(
              "Player: %{text}<br>",
              "Average: %{x:.1f}<br>",
              "Volatility: %{y:.1f}<br>",
              "<extra></extra>"
            )) %>%
      layout(title = "Performance vs Volatility",
             xaxis = list(title = paste("Average", input$metric)),
             yaxis = list(title = "Volatility (Standard Deviation)"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)