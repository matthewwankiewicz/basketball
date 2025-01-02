library(tidyverse)
library(shiny)
library(DT)

## read in gamelogs file
gamelogs <- read_csv("gamelogs2023.csv")

# Define UI for application that draws a histogram
ui <- navbarPage("The Daily",
    ## tab for player averages ----
    tabPanel("Player Averages",
      dateRangeInput("date_range", "Select Date Range",
                    start = "2023-10-24"),
      selectInput("team", "Select Team", choices = unique(gamelogs$team),
                  multiple = T, selected = unique(gamelogs$team)),
      DT::dataTableOutput("player_averages")
    ),
    ## tab for defence vs pos ----
    tabPanel("Defence vs Position",
             dateRangeInput("date_range_matchups", "Select Date Range",
                            start = "2023-10-24"),
             selectInput("position", "Select a position",
                         choices = c("PG", "SG", "SF",
                                     "PF", "C")),
             DT::dataTableOutput("opp_matchups")
    ),
    
    ## tab for player game logs ----
    tabPanel("Player Game Logs",
             selectInput("player", "Select a player",
                         choices = unique(gamelogs$player_name)),
             DT::dataTableOutput("player_logs")
    )
    
    ## tab for players with best matchups ----
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    ## output for player averages ----
    output$player_averages <- DT::renderDataTable({
      gamelogs %>% 
        filter((game_date >= input$date_range[1] & game_date <= input$date_range[2]) &
                team %in% input$team) %>% 
        select(player_name, team, min, pts, ast, reb, blk, stl, dfs_points) %>% 
        group_by(player_name, team) %>% 
        summarise_all(.funs = mean) %>% 
        mutate_at(.vars = c("pts", "reb", "ast", "min", "blk", "stl"), function(x){round(x, 1)}) %>% 
        mutate(dfs_points = round(dfs_points, 2)) %>% 
        datatable(rownames = F, 
                  options = list(pageLength = 100)
                  )
    })
    
    ## output for opponent matchups ----
    output$opp_matchups <- renderDataTable({
      ## filter for players >= 10 mins
      gamelogs %>% 
        filter(min >= 10 & 
                 (game_date >= input$date_range_matchups[1] & 
                    game_date <= input$date_range_matchups[2])
        ) %>% 
        separate_rows(pos, sep = ',') %>% 
        group_by(opp_team, pos) %>% 
        summarise(pts = mean(pts),
                  ast = mean(ast),
                  reb = mean(reb),
                  stl = mean(stl),
                  blk = mean(blk),
                  dfs_points = mean(dfs_points)) %>% 
        filter(pos == input$position) %>% 
        rename("team" = opp_team) %>% 
        mutate_if(is.numeric, function(x){round(x, 1)}) %>% 
        select(-pos) %>% 
        datatable(options = list(pageLength = 30),
                  rownames = F)
    })
    
    ## output for game logs ----
    output$player_logs <- renderDataTable({
      gamelogs %>% 
        filter(player_name == input$player) %>% 
        select(player_name, game_date, min, pts, reb, ast, blk, stl,
               usage_percentage, reb_rate, ast_rate, dfs_points) %>% 
        mutate_at(.vars = c("usage_percentage", "reb_rate",
                            "ast_rate"), function(x){round(x, 3)}) %>% 
        arrange(desc(game_date)) %>% 
        datatable(options = list(pageLength = 82),
                  rownames = F)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
