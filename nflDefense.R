library(shiny)
library(tidyverse)
library(plotly)
library(lubridate)
library(here)
library(nflreadr)
library(DT)

# Load data
data <- read.csv("data/working_data.csv")

games <- readRDS("data/games.rds")
plays <- readRDS("data/plays.rds")
players <- readRDS("data/players.rds")
tackles <- readRDS("data/tackles.rds")
tracking <- readRDS("data/tracking.rds")
tracking_runs <- readRDS("data/tracking_runs.rds")
frames <- readRDS("data/frames.rds")
pbp22 <- readRDS("data/pbp22.rds")
rb_bc_plays <- readRDS("data/rb_bc_plays.rds")
runs <- readRDS("data/runs.rds")

# Filter and group data
data_grouped <- data %>%
  filter(!is.na(expectedPointsAdded) & 
           !is.na(defensiveTeam) & 
           !is.na(defendersInTheBox) & 
           !is.na(playId) & 
           !is.na(gameId) & 
           !is.na(passProbability) & 
           !is.na(dis)) %>%
  group_by(gameId, playId, nflId) %>%
  summarize(
    avg_distance = mean(dis, na.rm = TRUE),
    avg_pass_prob = mean(passProbability, na.rm = TRUE)
  )

bar_data <- data %>%
  filter(!is.na(expectedPointsAdded) & 
           !is.na(defensiveTeam) & 
           !is.na(defendersInTheBox) & 
           !is.na(playId) & 
           !is.na(gameId) & 
           !is.na(passProbability) & 
           !is.na(dis)) %>%
  group_by(defendersInTheBox) %>%
  summarize(
    avg_pass_prob = mean(passProbability, na.rm = TRUE)
  )

epa_data <- data %>%
  filter(!is.na(expectedPointsAdded) & 
           !is.na(defendersInTheBox)) %>%
  group_by(defendersInTheBox) %>%
  summarize(
    avg_epa = mean(expectedPointsAdded, na.rm = TRUE)
  )

# UI
ui <- navbarPage(
  "Defensive Space",
  tabPanel(
    "Overview",
    sidebarLayout(
      sidebarPanel(
        helpText("Explore the overall performance and defensive metrics.")
      ),
      mainPanel(
        h3("Welcome to the Defensive Space Dashboard"),
        p("Use the tabs above to navigate through different visualizations.")
      )
    )
  ),
  tabPanel(
    "Play Visualization",
    sidebarLayout(
      sidebarPanel(
        selectInput("week", "Select Week:", choices = sort(unique(games$week))),
        uiOutput("matchup_selector"),  # Dynamic UI for matchup selection
        uiOutput("defensive_team_selector"),  # New dynamic UI for defensive team selection
        uiOutput("quarter_selector"),  # New dynamic UI for quarter selection
        uiOutput("play_selector"),    # Dynamic UI for play selection
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
  ),
  tabPanel(
    "Defensive Box Dashboard",
    sidebarLayout(
      sidebarPanel(
        helpText("Explore defensive metrics and their relationships.")
      ),
      mainPanel(
        fluidRow(
          column(6, plotlyOutput("smoothedPlot")),
          column(6, plotlyOutput("barGraph"))
        ),
        fluidRow(
          column(12, plotlyOutput("epaGraph"))
        )
      )
    )
  ),
  tabPanel(
    "Further Analysis with EPA",
    sidebarLayout(
      sidebarPanel(
        helpText("Select the analysis to view:"),
        selectInput(
          inputId = "analysis_type",
          label = "Choose Analysis Type:",
          choices = c(
            "Average DIR by Team" = "avg_dir",
            "Defensive Impact Rate by Team (Run vs. Pass)" = "run_pass_dir",
            "Negative EPA Rate (Faceted by Formation)" = "negative_epa_rate"
          ),
          selected = "avg_dir"
        )
      ),
      mainPanel(
        uiOutput("dynamic_plot_or_table")  # Conditional rendering output
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Smoothed line graph with filled area
  output$smoothedPlot <- renderPlotly({
    p <- ggplot(data_grouped, aes(x = avg_distance, y = avg_pass_prob)) +
      geom_smooth(
        method = "loess",
        se = FALSE,
        color = "black",
        span = 0.3,
        size = 1
      ) +
      geom_area(
        stat = "smooth",
        method = "loess",
        fill = "red",
        span = 0.3,
        alpha = 0.5
      ) +
      labs(
        title = "Player Distance Traveled by Pass Probability",
        x = "Average Distance",
        y = "Pass Probability"
      ) +
      theme_minimal() +
      theme(
        text = element_text(family = "Arial", color = "#495057"),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)
      )
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Bar graph for average players in box and pass probability
  output$barGraph <- renderPlotly({
    p <- ggplot(bar_data, aes(x = factor(defendersInTheBox), y = avg_pass_prob)) +
      geom_col(
        fill = "orange",
        alpha = 0.5
      ) +
      labs(
        title = "Pass Probability vs Defenders in Box",
        x = "Defenders in Box",
        y = "Average Pass Probability"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        text = element_text(family = "Arial", color = "#495057"),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)
      )
    
    ggplotly(p)
  })
  
  # EPA graph
  output$epaGraph <- renderPlotly({
    p <- ggplot(epa_data, aes(x = factor(defendersInTheBox), y = avg_epa)) +
      geom_col(
        fill = "steelblue",
        alpha = 0.7
      ) +
      labs(
        title = "Expected Points Added (EPA) vs Defenders in Box",
        x = "Defenders in Box",
        y = "Average EPA"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        text = element_text(family = "Arial", color = "#495057"),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)
      )
    
    ggplotly(p)
  })
  
  # Negative EPA Rate Calculation
  negative_epa_rate <- plays %>%
    mutate(negative_epa = ifelse(expectedPointsAdded < 0, 1, 0)) %>%
    group_by(defendersInTheBox, offenseFormation) %>%
    summarize(
      success_rate = mean(negative_epa, na.rm = TRUE),
      total_plays = n(),
      .groups = "drop"
    ) %>%
    filter(offenseFormation != "NA" & !is.na(defendersInTheBox)) %>% # Filter out NAs
    mutate(
      defendersInTheBox = factor(defendersInTheBox, levels = sort(unique(defendersInTheBox))),
      offenseFormation = factor(offenseFormation, levels = sort(unique(offenseFormation)))
    )
  
  # Create a list of `plotly` plots for each offensive formation
  formation_plots <- negative_epa_rate %>%
    split(.$offenseFormation) %>%
    lapply(function(data) {
      plot_ly(
        data,
        x = ~defendersInTheBox,
        y = ~success_rate,
        type = "bar",
        name = unique(data$offenseFormation),  # Use formation name for the trace
        hoverinfo = "text",  # Ensure text only shows in hover
        text = ~paste(
          "Defenders in Box:", defendersInTheBox, "<br>",
          "Offensive Formation:", offenseFormation, "<br>",
          "Success Rate:", round(success_rate * 100, 1), "%", "<br>",
          "Total Plays:", total_plays
        )
      ) %>%
        layout(
          xaxis = list(
            title = "Defenders in the Box",
            tickmode = "array",  # Ensure all values are represented
            tickvals = sort(unique(as.numeric(data$defendersInTheBox)))  # Explicit tick values
          ),
          yaxis = list(
            title = "Success Rate (Negative EPA)",
            tickformat = ".0%"  # Format y-axis as percentage
          ),
          showlegend = FALSE  # Hide individual legends for clarity
        )
    })
  
  # Combine the individual formation plots into a single faceted layout
  output$negative_epa_rate_plot <- renderPlotly({
    subplot(
      formation_plots,
      nrows = 2,  # Adjust the number of rows based on the number of formations
      titleX = TRUE,
      titleY = TRUE,
      shareX = TRUE,
      shareY = TRUE
    ) %>%
      layout(
        title = "Success Rate (Negative EPA) by Defenders in Box and Offensive Formation",
        margin = list(t = 50),
        showlegend = TRUE  # Show a combined legend
      )
  })
  
  # Render Data Table for Success Rate
  output$negative_epa_rate_table <- renderDT({
    negative_epa_rate %>%
      arrange(offenseFormation, defendersInTheBox) %>%  # Sort by offensive formation and defenders in box
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),  # Paginated and scrollable table
        rownames = FALSE,
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: left; font-size: 16px;",
          "Data Table: Success Rate and Total Plays by Defenders in Box and Offensive Formation"
        )
      )
  })
  
  # Dynamic UI for selecting matchups based on week
  output$matchup_selector <- renderUI({
    req(input$week)
    matchups_in_week <- games %>%
      filter(week == input$week) %>%
      mutate(matchup = paste(homeTeamAbbr, "vs", visitorTeamAbbr))  # Create matchup string
    selectInput("matchup", "Select Matchup:", choices = unique(matchups_in_week$matchup))
  })
  
  # New dynamic UI for selecting the defensive team
  output$defensive_team_selector <- renderUI({
    req(input$matchup)
    teams <- strsplit(input$matchup, " vs ")[[1]]  # Split matchup into home and away teams
    selectInput("defensive_team", "Select Defensive Team:", choices = teams)
  })
  
  # Quarter selection
  output$quarter_selector <- renderUI({
    req(input$matchup, input$defensive_team)
    
    # Filter selected game
    selected_game <- games %>%
      filter(week == input$week) %>%
      mutate(matchup = paste(homeTeamAbbr, "vs", visitorTeamAbbr)) %>%
      filter(matchup == input$matchup) %>%
      pull(gameId)
    
    # Filter plays for the selected game and defensive team
    available_quarters <- plays %>%
      filter(gameId == selected_game, defensiveTeam == input$defensive_team) %>%
      pull(quarter) %>%
      unique() %>%
      sort()
    
    selectInput("quarter", "Select Quarter:", choices = c("All", available_quarters))
  })
  
  # Dynamic UI for selecting plays based on the selected matchup and defensive team
  output$play_selector <- renderUI({
    req(input$matchup, input$defensive_team)
    
    # Filter selected game
    selected_game <- games %>%
      filter(week == input$week) %>%
      mutate(matchup = paste(homeTeamAbbr, "vs", visitorTeamAbbr)) %>%
      filter(matchup == input$matchup) %>%
      pull(gameId)
    
    # Filter plays in the game and defensive team
    plays_in_game <- plays %>%
      filter(gameId == selected_game, defensiveTeam == input$defensive_team)
    
    # Apply quarter filter if selected
    if (!is.null(input$quarter) && input$quarter != "All") {
      plays_in_game <- plays_in_game %>%
        filter(quarter == as.numeric(input$quarter))
    }
    
    # Sort plays by game clock
    plays_in_game <- plays_in_game %>%
      arrange(desc(as.numeric(gameClock))) 
    
    selectInput("play", "Select Play:", choices = plays_in_game$playId)
  })
  
  # Display play description
  output$play_description <- renderText({
    req(input$play, input$matchup)
    
    # Determine the selected game
    selected_game <- games %>%
      filter(week == input$week) %>%
      mutate(matchup = paste(homeTeamAbbr, "vs", visitorTeamAbbr)) %>%
      filter(matchup == input$matchup) %>%
      pull(gameId)
    
    # Filter play details using both gameId and playId
    play_details <- plays %>%
      filter(gameId == selected_game, playId == as.numeric(input$play)) %>%
      distinct(playDescription)
    
    if (nrow(play_details) == 0) return("No play description available.")
    
    play_details$playDescription[1]
  })
  
  # Display useful play metrics
  output$play_metrics <- renderTable({
    req(input$week, input$matchup, input$play)
    
    selected_game <- games %>%
      filter(week == input$week) %>%
      mutate(matchup = paste(homeTeamAbbr, "vs", visitorTeamAbbr)) %>%
      filter(matchup == input$matchup) %>%
      pull(gameId)
    
    play_details <- plays %>%
      filter(gameId == selected_game, playId == as.numeric(input$play)) %>%
      left_join(games, by = "gameId")
    
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
        gsub("^0+", "", sub(":00$", "", play_details$gameClock))
      )
    )
  }, rownames = FALSE)
  
  # Replace references to input$game with selected_game
  output$interactive_play <- renderPlotly({
    req(input$week, input$matchup, input$play)
    
    # Determine the selected game based on week and matchup
    selected_game <- games %>%
      filter(week == input$week) %>%
      mutate(matchup = paste(homeTeamAbbr, "vs", visitorTeamAbbr)) %>%
      filter(matchup == input$matchup) %>%
      pull(gameId)
    
    # Filter tracking data for the selected play
    play_tracking <- tracking %>%
      filter(gameId == selected_game, playId == as.numeric(input$play))
    
    # Get play details for line of scrimmage and yards to go
    play_details <- plays %>%
      filter(gameId == selected_game, playId == as.numeric(input$play)) %>%
      select(absoluteYardlineNumber, yardsToGo)
    
    if (nrow(play_details) == 0) return(NULL)
    
    # Initialize line_of_scrimmage
    line_of_scrimmage <- play_details$absoluteYardlineNumber
    
    # Adjust for plays moving left
    play_direction <- tracking %>%
      filter(gameId == as.numeric(selected_game), playId == as.numeric(input$play)) %>%
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
    
    # Prepare tracking data for visualization
    player_tracking <- play_tracking %>%
      filter(!is.na(nflId)) %>%
      left_join(players %>% select(nflId, position), by = "nflId") %>% # Add player positions 
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
        team = club
      )
    
    ball_tracking <- play_tracking %>%
      filter(is.na(nflId)) %>%
      mutate(role = NA, color = "black", team = NA, position = NA)
    
    # Combine data
    combined_data <- bind_rows(
      player_tracking %>% select(frameId, x, y, displayName, role, color, team, position, time),
      ball_tracking %>% select(frameId, x, y, displayName, role, color, team, position, time)
    )
    
    # Define field layout with the blue and yellow lines
    field_shapes <- list(
      list(type = "rect", x0 = 0, x1 = 10, y0 = 0, y1 = 53.3, fillcolor = "yellow"),
      list(type = "rect", x0 = 110, x1 = 120, y0 = 0, y1 = 53.3, fillcolor = "yellow"),
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
      ),
      # Out-of-bounds area below the field
      list(
        type = "rect",
        x0 = 0,
        x1 = 120,
        y0 = -10,
        y1 = 0,
        fillcolor = "#2a5631"
      ),
      # Out-of-bounds area above the field
      list(
        type = "rect",
        x0 = 0,
        x1 = 120,
        y0 = 53.3,
        y1 = 60,
        fillcolor = "#2a5631"
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
      marker = list(size = 8, opacity = 0.8, cliponaxis = FALSE),
      color = ~color,
      colors = c("black", "blue", "red"),
      text = ~paste(
        "Name:", displayName, 
        "<br>Role:", role, 
        "<br>Position:", position, 
        "<br>Team:", team, 
        "<br>Yardline:", round(x, 1)
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste("Game ID:", selected_game, "| Play ID:", input$play),
        xaxis = list(title = "Field Length (yards)", range = c(0, 120)),
        yaxis = list(title = "Field Width (yards)", range = c(-3, 56.6)),
        plot_bgcolor = "#3c914a",
        paper_bgcolor = "white",
        shapes = field_shapes,
        margin = list(l = 50, r = 50, b = 50, t = 50),
        showlegend = FALSE
      ) %>%
      animation_opts(frame = 100, transition = 0, redraw = FALSE)
  })
  
  # Render Additional Play Details Table
  output$play_table <- renderDataTable({
    req(input$week, input$matchup, input$play)
    
    selected_game <- games %>%
      filter(week == input$week) %>%
      mutate(matchup = paste(homeTeamAbbr, "vs", visitorTeamAbbr)) %>%
      filter(matchup == input$matchup) %>%
      pull(gameId)
    
    play_details <- plays %>%
      filter(gameId == selected_game, playId == as.numeric(input$play))
    
    if (nrow(play_details) == 0) return(NULL)
    
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
  
  # Calculate Average DIR by Team
  team_defensive_impact <- tackles %>%
    inner_join(plays, by = c("gameId", "playId")) %>%
    inner_join(pbp22 %>% 
                 mutate(gameId = as.double(old_game_id), 
                        playId = as.double(play_id)), 
               by = c("gameId", "playId")) %>%
    filter(!is.na(expectedPointsAdded)) %>%
    mutate(
      negative_epa = ifelse(expectedPointsAdded < 0, 1, 0)
    ) %>%
    group_by(defensiveTeam) %>%
    summarize(
      total_negative_epa = sum(negative_epa, na.rm = TRUE),
      total_plays = n(),
      avg_DIR = total_negative_epa / total_plays,
      .groups = "drop"
    )
  
  # Render Average DIR Plot
  output$avg_dir_plot <- renderPlotly({
    avg_dir <- ggplot(team_defensive_impact, aes(x = reorder(defensiveTeam, avg_DIR), y = avg_DIR, fill = avg_DIR)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        title = "Average Defensive Impact Rate by Team",
        x = "Team",
        y = "Average Defensive Impact Rate (DIR)",
        fill = "DIR"
      ) +
      scale_fill_gradient(low = "blue", high = "red", name = "DIR") +
      theme_minimal()
    
    # Convert to interactive plotly
    ggplotly(avg_dir, tooltip = c("x", "y", "fill"))
  })
  
  # Render Data Table for Average DIR
  output$avg_dir_table <- renderDT({
    team_defensive_impact %>%
      arrange(desc(avg_DIR)) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: left; font-size: 16px;",
          "Data Table: Average Defensive Impact Rate (DIR) by Team"
        )
      )
  })
  
  # Defensive Impact Rate by Team (Run vs. Pass)
  output$run_pass_dir_plot <- renderPlotly({
    # Set factor levels for defensiveTeam based on the Average DIR order
    team_defensive_impact_run_pass <- tackles %>%
      inner_join(plays, by = c("gameId", "playId")) %>%
      inner_join(pbp22 %>% 
                   mutate(gameId = as.double(old_game_id), 
                          playId = as.double(play_id)), 
                 by = c("gameId", "playId")) %>%
      filter(!is.na(expectedPointsAdded), play_type != "no_play") %>%
      mutate(
        negative_epa = ifelse(expectedPointsAdded < 0, 1, 0)
      ) %>%
      group_by(defensiveTeam, play_type) %>%
      summarize(
        total_negative_epa = sum(negative_epa, na.rm = TRUE),
        total_plays = n(),
        avg_DIR = total_negative_epa / total_plays,
        .groups = "drop"
      ) %>%
      mutate(defensiveTeam = factor(defensiveTeam, levels = team_defensive_impact$defensiveTeam))  # Match factor levels
    
    # Create the ggplot
    run_pass_dir <- ggplot(team_defensive_impact_run_pass, aes(x = defensiveTeam, y = avg_DIR, fill = play_type)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      labs(
        title = "Defensive Impact Rate by Team (Run vs. Pass)",
        x = "Team",
        y = "Average Defensive Impact Rate (DIR)",
        fill = "Play Type"
      ) +
      theme_minimal()
    
    # Convert to interactive plotly
    ggplotly(run_pass_dir, tooltip = c("x", "y", "fill"))
  })
  
  # Render Data Table for Run vs. Pass DIR
  output$run_pass_dir_table <- renderDT({
    tackles %>%
      inner_join(plays, by = c("gameId", "playId")) %>%
      inner_join(pbp22 %>%
                   mutate(gameId = as.double(old_game_id), 
                          playId = as.double(play_id)), 
                 by = c("gameId", "playId")) %>%
      filter(!is.na(expectedPointsAdded), play_type != "no_play") %>%
      mutate(
        negative_epa = ifelse(expectedPointsAdded < 0, 1, 0)
      ) %>%
      group_by(defensiveTeam, play_type) %>%
      summarize(
        total_negative_epa = sum(negative_epa, na.rm = TRUE),
        total_plays = n(),
        avg_DIR = total_negative_epa / total_plays,
        .groups = "drop"
      ) %>%
      mutate(defensiveTeam = factor(defensiveTeam, levels = team_defensive_impact$defensiveTeam)) %>%
      arrange(desc(avg_DIR)) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: left; font-size: 16px;",
          "Data Table: Defensive Impact Rate (Run vs. Pass) by Team"
        )
      )
  })
  
  # Dynamic UI with Explanatory Notes
  output$dynamic_plot_or_table <- renderUI({
    req(input$analysis_type)  # Ensure a selection is made
    
    if (input$analysis_type == "avg_dir") {
      tagList(
        tags$p(
          "Note: The Average Defensive Impact Rate (DIR) is calculated as the percentage of plays 
        where the defensive team contributed to a negative Expected Points Added (EPA). 
        A higher DIR indicates a more impactful defensive performance."
        ),
        plotlyOutput("avg_dir_plot", height = "600px"),
        DTOutput("avg_dir_table")
      )
    } else if (input$analysis_type == "run_pass_dir") {
      tagList(
        tags$p(
          "Note: The Defensive Impact Rate (DIR) by Play Type (Run vs. Pass) breaks down the average DIR 
        separately for run and pass plays. It helps assess a team's defensive strength against different play types."
        ),
        plotlyOutput("run_pass_dir_plot", height = "600px"),
        DTOutput("run_pass_dir_table")
      )
    } else if (input$analysis_type == "negative_epa_rate") {
      tagList(
        tags$p(
          "Note: The Negative EPA Rate by Defenders in the Box and Offensive Formation shows the success rate 
        of plays where the defense caused a negative EPA. It highlights the relationship between defensive 
        alignment and offensive formations."
        ),
        plotlyOutput("negative_epa_rate_plot", height = "600px"),
        DTOutput("negative_epa_rate_table")
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)