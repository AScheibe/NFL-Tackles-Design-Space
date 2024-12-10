library(shiny)
library(dplyr)
library(plotly)

# Shiny App UI
ui <- fluidPage(
  titlePanel("Interactive Green Bay Packers Play Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("week_filter", "Select Week", choices = NULL),
      selectInput("formation_filter", "Select Offensive Formation", choices = NULL),
      selectInput("play_filter", "Select Play", choices = NULL),
      textOutput("opponent_text"),
      textOutput("game_scores"),
      textOutput("current_game_clock"),
      textOutput("play_details") # Added text output for play details
    ),
    mainPanel(
      plotlyOutput("interactive_play")
    )
  )
)

server <- function(input, output, session) {
  # Populate week filter
  observe({
    updateSelectInput(session, "week_filter", choices = unique(gb_plays$week))
  })
  
  # Update formation and opponent
  observeEvent(input$week_filter, {
    req(input$week_filter)
    
    week_data <- gb_plays %>% filter(week == input$week_filter)
    updateSelectInput(session, "formation_filter", choices = unique(week_data$offenseFormation))
    output$opponent_text <- renderText({
      paste("Opponent:", unique(week_data$possessionTeam))
    })
  })
  
  # Update play filter
  observeEvent(input$formation_filter, {
    req(input$week_filter, input$formation_filter)
    
    play_data <- gb_plays %>%
      filter(
        week == input$week_filter,
        offenseFormation == input$formation_filter
      ) %>%
      mutate(
        play_label = paste(
          "Game:", paste(possessionTeam, "vs", defensiveTeam), 
          "| Play ID:", playId
        )
      )
    
    updateSelectInput(
      session, 
      "play_filter", 
      choices = setNames(play_data$playId, play_data$play_label)
    )
  })
  
  # Render visualization
  output$interactive_play <- renderPlotly({
    req(input$play_filter)
    
    selected_play <- as.numeric(input$play_filter)
    play_tracking <- gb_tracking %>% filter(playId == selected_play)
    
    # Ball tracking
    ball_tracking <- play_tracking %>%
      filter(displayName == "football") %>%
      mutate(
        role = as.character("Ball"),  # Explicitly set as character
        color = "black"
      )
    
    # Player tracking
    player_tracking <- play_tracking %>%
      filter(displayName != "football") %>%
      mutate(
        role = as.character(ifelse(club == "GB", "Green Bay Defense", "Opponent Offense")),  # Explicitly set as character
        color = ifelse(club == "GB", "darkgreen", "red")
      )
    
    # Combine data
    combined_data <- bind_rows(
      player_tracking %>% select(frameId, x, y, displayName, role, color, gameClock),
      ball_tracking %>% select(frameId, x, y, displayName, role, color, gameClock)
    )
    
    output$current_game_clock <- renderText({
      paste("Game Clock:", unique(combined_data$gameClock))
    })
    
    # Add play details
    play_details <- gb_plays %>%
      filter(playId == selected_play) %>%
      summarise(
        quarter = first(quarter),
        down = first(down),
        yards_to_go = first(yardsToGo),
        home_score = first(preSnapHomeScore),
        visitor_score = first(preSnapVisitorScore),
        play_desc = first(playDescription)
      )
    
    output$play_details <- renderText({
      paste0(
        "Quarter: ", play_details$quarter, "\n",
        "Down: ", play_details$down, " | Yards to Go: ", play_details$yards_to_go, "\n",
        "Home Score: ", play_details$home_score, " | Visitor Score: ", play_details$visitor_score, "\n",
        "Play Description: ", play_details$play_desc
      )
    })
    
    # Define field layout with end zones and yard lines
    field_shapes <- list(
      list(type = "rect", x0 = 0, x1 = 10, y0 = 0, y1 = 53.3, fillcolor = "yellow", line = list(width = 0)),
      list(type = "rect", x0 = 110, x1 = 120, y0 = 0, y1 = 53.3, fillcolor = "yellow", line = list(width = 0))
    )
    yard_lines <- lapply(seq(10, 110, by = 10), function(x) {
      list(type = "line", x0 = x, x1 = x, y0 = 0, y1 = 53.3, line = list(color = "white", width = 1, dash = "dash"))
    })
    field_shapes <- c(field_shapes, yard_lines)
    
    plot_ly(
      data = combined_data,
      x = ~x,
      y = ~y,
      frame = ~frameId,
      type = "scatter",
      mode = "markers",
      marker = list(size = 8, opacity = 0.8),
      color = ~color,
      colors = c("black", "green", "red"),
      text = ~paste("Name:", displayName, "<br>Role:", role, "<br>Clock:", gameClock),
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste("Interactive Visualization - Play ID:", selected_play),
        xaxis = list(title = "Field Length (yards)", range = c(0, 120), tickvals = seq(0, 120, by = 10)),
        yaxis = list(title = "Field Width (yards)", range = c(0, 53.3)),
        plot_bgcolor = "darkgreen",
        paper_bgcolor = "white",
        shapes = field_shapes,
        showlegend = FALSE
      ) %>%
      animation_opts(frame = 100, transition = 0, redraw = FALSE) %>%
      animation_slider(currentvalue = list(prefix = "Frame: "))
  })
}

shinyApp(ui, server)
