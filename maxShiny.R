library(shiny)
library(plotly)
library(dplyr)
library(lubridate)

ui <- fluidPage(
  titlePanel("Interactive NFL Play Animation"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("game", "Select Game:", choices = unique(games$gameId)),
      uiOutput("play_selector"),  # Dynamic UI for play selection
      h3("Play Description"),
      verbatimTextOutput("play_description"),
      br(),
      h3("Play Metrics"),
      tableOutput("play_metrics")
    ),
    mainPanel(
      plotlyOutput("interactive_play", height = "600px"),
      br(),
      h3("Additional Play Details"),
      dataTableOutput("play_table")
    )
  )
)


server <- function(input, output, session) {
  
  # Dynamic UI for selecting plays based on selected game
  output$play_selector <- renderUI({
    req(input$game)
    plays_in_game <- plays %>% filter(gameId == as.numeric(input$game))
    selectInput("play", "Select Play:", choices = plays_in_game$playId)
  })
  
  # Display play description
  output$play_description <- renderText({
    req(input$game, input$play)
    play_details <- plays %>%
      filter(gameId == as.numeric(input$game), playId == as.numeric(input$play))
    
    if (nrow(play_details) == 0) return("No play description available.")
    play_details$playDescription
  })
  
  # Display useful play metrics
  output$play_metrics <- renderTable({
    req(input$game, input$play)
    
    # Join plays and games to include team abbreviations
    play_details <- plays %>%
      filter(gameId == as.numeric(input$game), playId == as.numeric(input$play)) %>%
      left_join(games, by = "gameId")  # Join with games data
    
    if (nrow(play_details) == 0) return(NULL)
    
    data.frame(
      Metric = c("Offensive Team", "Defensive Team", "Score", "Ball Carrier", 
                 "Quarter", "Down", "Yards to Go", "Game Clock"),
      Value = c(
        play_details$possessionTeam,
        play_details$defensiveTeam,
        paste(play_details$homeTeamAbbr, play_details$preSnapHomeScore, "-", play_details$visitorTeamAbbr, play_details$preSnapVisitorScore),
        play_details$ballCarrierDisplayName,
        play_details$quarter,
        play_details$down,
        play_details$yardsToGo,
        gsub("^0+", "", sub(":00$", "", play_details$gameClock))  # Format Game Clock
      )
    )
  }, rownames = FALSE)
  
  # Render Interactive Play Visualization
  output$interactive_play <- renderPlotly({
    req(input$game, input$play)
    
    # Filter tracking data for the selected play
    play_tracking <- tracking %>%
      filter(gameId == as.numeric(input$game), playId == as.numeric(input$play))
    
    # Get play details for line of scrimmage and yards to go
    play_details <- plays %>%
      filter(gameId == as.numeric(input$game), playId == as.numeric(input$play)) %>%
      select(absoluteYardlineNumber, yardsToGo)
    
    if (nrow(play_details) == 0) return(NULL)
    
    # Initialize line_of_scrimmage
    line_of_scrimmage <- play_details$absoluteYardlineNumber
    
    # Adjust for plays moving left
    play_direction <- tracking %>%
      filter(gameId == as.numeric(input$game), playId == as.numeric(input$play)) %>%
      slice(1) %>%
      pull(playDirection)
    
    if (play_direction == "left") {
      # Flip the field if playDirection is "left"
      line_of_scrimmage <- 120 - line_of_scrimmage
    }
    
    # Calculate first_down_marker by adding yardsToGo to line_of_scrimmage
    first_down_marker <- line_of_scrimmage + play_details$yardsToGo
    
    # Ensure the lines remain within the playable field range (10–110)
    line_of_scrimmage <- pmax(10, pmin(line_of_scrimmage, 110))
    first_down_marker <- pmax(10, pmin(first_down_marker, 110))
    
    player_tracking <- play_tracking %>%
      filter(!is.na(nflId)) %>%  # Players have nflId
      left_join(plays %>% select(gameId, playId, possessionTeam, defensiveTeam), by = c("gameId", "playId")) %>%
      mutate(
        role = case_when(
          club == defensiveTeam ~ "Defense",
          club == possessionTeam ~ "Offense",
          TRUE ~ "Other"
        ),
        color = case_when(
          role == "Defense" ~ "red",
          role == "Offense" ~ "blue",
          TRUE ~ "gray"
        ),
        team = club  # Add team for tooltips
      )
    
    ball_tracking <- play_tracking %>%
      filter(is.na(nflId)) %>%  # Ball has no nflId
      mutate(role = "Ball", color = "black", team = "Ball")  # Include team as "Ball"
    
    # Combine data
    combined_data <- bind_rows(
      player_tracking %>% select(frameId, x, y, displayName, role, color, team, time),
      ball_tracking %>% select(frameId, x, y, displayName, role, color, team, time)
    )
    
    combined_data <- combined_data %>%
      mutate(
        time = case_when(
          !is.na(time) & grepl("^\\d{2}:\\d{2}:\\d{2}$", time) ~ format(hms(time), "%M:%S"),  # Properly formatted time
          TRUE ~ NA_character_  # Set invalid or missing times to NA
        )
      )
    
    # Define field layout with the blue and yellow lines
    field_shapes <- list(
      list(type = "rect", x0 = 0, x1 = 10, y0 = 0, y1 = 53.3, fillcolor = "yellow", line = list(width = 0)),
      list(type = "rect", x0 = 110, x1 = 120, y0 = 0, y1 = 53.3, fillcolor = "yellow", line = list(width = 0)),
      # Yellow first down marker
      list(
        type = "line",
        x0 = first_down_marker, x1 = first_down_marker,
        y0 = 0, y1 = 53.3,
        line = list(color = "yellow", width = 2, dash = "dash")
      ),
      # Blue line of scrimmage
      list(
        type = "line",
        x0 = line_of_scrimmage, x1 = line_of_scrimmage,
        y0 = 0, y1 = 53.3,
        line = list(color = "blue", width = 2, dash = "dash")
      )
    )
    
    # Add yard lines
    yard_lines <- lapply(seq(10, 110, by = 10), function(x) {
      list(type = "line", x0 = x, x1 = x, y0 = 0, y1 = 53.3, line = list(color = "white", width = 1, dash = "dash"))
    })
    field_shapes <- c(field_shapes, yard_lines)
    
    # Create interactive plotly visualization
    plot_ly(
      data = combined_data,
      x = ~x,
      y = ~y,
      frame = ~frameId,
      type = "scatter",
      mode = "markers",
      marker = list(size = 8, opacity = 0.8),
      color = ~color,  
      colors = c("black", "blue", "red"),
      text = ~paste("Name:", displayName, "<br>Role:", role, "<br>Team:", team),  # Include team
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste("Interactive Visualization - Game ID:", input$game, "| Play ID:", input$play),
        xaxis = list(title = "Field Length (yards)", range = c(0, 120)),
        yaxis = list(title = "Field Width (yards)", range = c(0, 53.3)),
        plot_bgcolor = "darkgreen",
        paper_bgcolor = "white",
        shapes = field_shapes,
        showlegend = FALSE
      ) %>%
      animation_opts(frame = 100, transition = 0, redraw = FALSE)
  })
  
  
  # Render Additional Play Details Table
  output$play_table <- renderDataTable({
    req(input$game, input$play)
    play_details <- plays %>%
      filter(gameId == as.numeric(input$game), playId == as.numeric(input$play))
    
    if (nrow(play_details) == 0) return(NULL)
    
    # Combine relevant data
    play_details %>%
      select(playDescription, possessionTeam, defensiveTeam, preSnapHomeScore, preSnapVisitorScore,
             quarter, down, yardsToGo, gameClock, ballCarrierDisplayName, playResult, offenseFormation, defendersInTheBox) %>%
      rename(
        "Play Description" = playDescription,
        "Offensive Team" = possessionTeam,
        "Defensive Team" = defensiveTeam,
        "Home Score" = preSnapHomeScore,
        "Visitor Score" = preSnapVisitorScore,
        "Quarter" = quarter,
        "Down" = down,
        "Yards to Go" = yardsToGo,
        "Game Clock" = gameClock,
        "Ball Carrier" = ballCarrierDisplayName,
        "Yards Gained" = playResult,
        "Offensive Formation" = offenseFormation,
        "Number of Defenders in the Box" = defendersInTheBox
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
