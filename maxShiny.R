library(shiny)
library(tidyverse)
library(plotly)
library(lubridate)
library(here)
library(nflreadr)
library(DT)

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

# Summarize data for plotting, grouping only by player
rb_summary_all <- rb_bc_plays %>%
  left_join(plays %>% select(gameId, playId, playResult, possessionTeam, defensiveTeam), 
            by = c("gameId", "playId")) %>%
  group_by(ballCarrierDisplayName, possessionTeam, gameId, playId) %>%  # Group by unique plays
  summarize(
    total_run_plays = n_distinct(playId),  # Count unique play IDs
    avg_yards_gained = mean(playResult, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ballCarrierDisplayName, possessionTeam) %>%  # Group to summarize per player
  summarize(
    total_run_plays = sum(total_run_plays),
    avg_yards_gained = mean(avg_yards_gained, na.rm = TRUE),
    .groups = "drop"
  )

# Team colors
team_colors_main <- c("ARI" = "#97233F", "ATL" = "#a71930", "BAL" = "#241773", "BUF" = "#00338D", "CAR" = "#0085CA",
                      "CHI" = "#0B162A", "CIN" = "#fb4f14", "CLE" = "#311D00", "DAL" = "#869397", "DEN" = "#FB4F14",
                      "DET" = "#0076b6", "GB" = "#203731", "HOU" = "#03202f", "IND" = "#002C5F", "JAX" = "#006778", 
                      "KC" = "#E31837", "LAC" = "#0080C6", "LA" = "#003594", "MIA" = "#008E97", "MIN" = "#4F2683", 
                      "NE" = "#002244", "NO" = "#D3BC8D", "NYG" = "#0B2265", "NYJ" = "#125740", "LV" = "#000000", 
                      "PHI" = "#004C54", "PIT" = "#FFB612", "SF" = "#AA0000", "SEA" = "#002244", "TB" = "#D50A0A", 
                      "TEN" = "#0C2340", "WAS" = "#5A1414")

team_colors_outer <- c("ARI" = "#FFB612", "ATL" = "#000000", "BAL" = "#000000", "BUF" = "#C60C30", "CAR" = "#101820",
                       "CHI" = "#c83803", "CIN" = "#000000", "CLE" = "#ff3c00", "DAL" = "#003594", "DEN" = "#002244",
                       "DET" = "#B0B7BC", "GB" = "#FFB612", "HOU" = "#A71930", "IND" = "#A2AAAD", "JAX" = "#9F792C", 
                       "KC" = "#FFB81C", "LAC" = "#FFC20E", "LA" = "#ffa300", "MIA" = "#FC4C02", "MIN" = "#FFC62F", 
                       "NE" = "#C60C30", "NO" = "#101820", "NYG" = "#a71930", "NYJ" = "#000000", "LV" = "#A5ACAF", 
                       "PHI" = "#A5ACAF", "PIT" = "#101820", "SF" = "#B3995D", "SEA" = "#69BE28", "TB" = "#34302B", 
                       "TEN" = "#4B92DB", "WAS" = "#FFB612")

# Join rb_bc_plays with plays to bring in necessary columns
rb_data <- rb_bc_plays %>%
  left_join(
    plays %>% 
      select(gameId, playId, expectedPointsAdded, defendersInTheBox, playResult, offenseFormation, defensiveTeam), 
    by = c("gameId", "playId")
  ) %>%
  left_join(
    tracking %>% 
      select(gameId, playId, nflId, dis), 
    by = c("gameId", "playId")
  ) %>%
  filter(
    !is.na(expectedPointsAdded),
    !is.na(defendersInTheBox),
    !is.na(dis)
  )

# UI
ui <- navbarPage(
  "Defensive Space - RB Run Analysis",
  tabPanel(
    "Play Visualization",
    sidebarLayout(
      sidebarPanel(
        selectInput("week", "Select Week:", choices = sort(unique(games$week))),
        uiOutput("matchup_selector"),  # Dynamic UI for matchup selection
        uiOutput("defensive_team_selector"),  # Dynamic UI for defensive team selection
        uiOutput("quarter_selector"),  # Dynamic UI for quarter selection
        uiOutput("play_selector"),    # Dynamic UI for play selection
        h3("Play Description"),
        verbatimTextOutput("play_description"),  # Only one description section
        br(),
        h3("Play Metrics"),
        tableOutput("play_metrics")  # Only one play_metrics table
      ),
      mainPanel(
        h3("Welcome to the Defensive Space Dashboard!"),
        p("Please use the  tabs above to navigate through different visualizations."),
        HTML("<b>Authors:</b> Max Lake, Alexander Scheibe, Yoel Nasi Kazado, Urmi Mehta, Joseph Ho, Miguel Marcial"),
        plotlyOutput("interactive_play", height = "600px"),
        br(),
        h3("Defender Pressure Analysis"),  # Main play details, separate from metrics
        plotlyOutput("defender_pressure_plot", height = "400px")  # Use this for additional detailed data if needed
      )
    )
  ),
  tabPanel(
    "RB Performance",
    sidebarLayout(
      sidebarPanel(
        selectInput("team", "Select Team (RB's Team):", 
                    choices = c("All", sort(unique(rb_summary_all$possessionTeam))), 
                    selected = "All"),
        selectInput("player", "Select Running Back:", choices = NULL, 
                    multiple = TRUE, selectize = TRUE),
        selectInput("opponent", "Select Opponent Team:", 
                    choices = c("All", sort(unique(plays$defensiveTeam))), 
                    selected = "All"),
        selectInput("formation", "Select Offensive Formation:", 
                    choices = c("All", sort(unique(rb_data$offenseFormation))), 
                    selected = "All"),
        selectInput("defenders", "Select Defenders in the Box:", 
                    choices = c("All", sort(unique(rb_data$defendersInTheBox))), 
                    selected = "All"),
        actionButton("add_player", "Add Selected Players"),
        br(), br(),
        actionButton("reset_button", "Reset Selections", icon = icon("Redo"))
      ),
      mainPanel(
        p("This scatter plot shows the performance of running backs (RBs) based on their total number of run plays (x-axis) 
        and average yards gained per play (y-axis). Use the filters on the left to select specific teams, players, opponents, 
        formations, and defenders in the box. Hover over the points to view detailed player stats. Players with a higher 
        average yards gained and more total plays are generally more efficient."),
        plotlyOutput("rb_performance_plot"),
        br(),
        DTOutput("rb_summary_table")
      )
    )
  ),
  tabPanel(
    "Defensive Box Dashboard",
    sidebarLayout(
      sidebarPanel(
        helpText("Explore defensive metrics specifically for RB run plays."),
        selectInput(
          "team_selection", "Select Team:",
          choices = c("All", sort(unique(plays$defensiveTeam))),
          selected = "All"
        )
      ),
      mainPanel(
        fluidRow(
          column(12, p("This chart shows the average Expected Points Added (EPA) for running back plays based on the 
                       number of defenders in the box. Lower EPA indicates stronger defensive performance.")),
          column(6, plotlyOutput("epaGraph"))
        ),
        fluidRow(
          column(12, p("This chart displays the percentage of plays resulting in negative EPA, grouped by offensive 
                       formation and defenders in the box. A higher rate indicates more defensive success.")),
          column(6, plotlyOutput("negative_epa_rate_plot"))
        ),
        fluidRow(
          column(12, p("This graph shows the tackling success rate, calculated as solo tackles divided by total tackles, 
                       based on defensive alignment and offensive formation.")),
          column(6, plotlyOutput("tackling_success_plot"))
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Dynamic UI for selecting matchups based on week
  output$matchup_selector <- renderUI({
    req(input$week)
    matchups_in_week <- games %>%
      filter(week == input$week) %>%
      mutate(matchup = paste(homeTeamAbbr, "vs", visitorTeamAbbr))
    selectInput("matchup", "Select Matchup:", choices = unique(matchups_in_week$matchup))
  })
  
  # Dynamic UI for defensive team
  output$defensive_team_selector <- renderUI({
    req(input$matchup)
    teams <- strsplit(input$matchup, " vs ")[[1]]
    selectInput("defensive_team", "Select Defensive Team:", choices = teams)
  })
  
  # Dynamic UI for quarters
  output$quarter_selector <- renderUI({
    req(input$matchup, input$defensive_team)
    selected_game <- games %>%
      filter(week == input$week, paste(homeTeamAbbr, "vs", visitorTeamAbbr) == input$matchup) %>%
      pull(gameId)
    available_quarters <- plays %>%
      filter(gameId == selected_game, defensiveTeam == input$defensive_team) %>%
      pull(quarter) %>%
      unique()
    selectInput("quarter", "Select Quarter:", choices = c("All", sort(available_quarters)))
  })
  
  # Dynamic UI for plays
  output$play_selector <- renderUI({
    req(input$matchup, input$defensive_team, input$quarter)
    selected_game <- games %>%
      filter(week == input$week, paste(homeTeamAbbr, "vs", visitorTeamAbbr) == input$matchup) %>%
      pull(gameId)
    filtered_plays <- plays %>%
      filter(gameId == selected_game, defensiveTeam == input$defensive_team)
    if (input$quarter != "All") {
      filtered_plays <- filtered_plays %>% filter(quarter == as.numeric(input$quarter))
    }
    selectInput("play", "Select Play:", choices = filtered_plays$playId)
  })
  
  # Display play description
  output$play_description <- renderText({
    req(input$play)
    play_details <- plays %>%
      filter(playId == as.numeric(input$play)) %>%
      pull(playDescription)
    if (length(play_details) == 0) return("No description available.")
    play_details
  })
  
  # Display play metrics
  output$play_metrics <- renderTable({
    req(input$play)
    
    play_details <- plays %>%
      filter(playId == as.numeric(input$play)) %>%
      distinct(playId, .keep_all = TRUE) %>%  # Ensure only unique rows
      select(
        possessionTeam, defensiveTeam, preSnapHomeScore, preSnapVisitorScore,
        ballCarrierDisplayName, quarter, down, yardsToGo, gameClock, offensiveFormation, defendersInTheBox, playResult
      )
    
    if (nrow(play_details) == 0) return(NULL)
    
    data.frame(
      Metric = c("Offensive Team", "Defensive Team", "Score", "Ball Carrier", 
                 "Quarter", "Down", "Yards to Go", "Game Clock", "Offensive Formation", "Number of Defenders in the Box",
                 "Play Result"),
      Value = c(
        play_details$possessionTeam,
        play_details$defensiveTeam,
        paste(play_details$preSnapHomeScore, "-", play_details$preSnapVisitorScore),
        play_details$ballCarrierDisplayName,
        play_details$quarter,
        play_details$down,
        play_details$yardsToGo,
        gsub("^0+", "", sub(":00$", "", play_details$gameClock)),
        play_details$offensiveFormation,
        play_details$defendersInTheBox,
        play_details$playResult
      )
    )
  })
  
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
    yard_lines <- lapply(seq(20, 100, by = 10), function(x) {
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
  
  # Display play description
  output$play_description <- renderText({
    req(input$week, input$matchup, input$play)
    
    selected_game <- games %>%
      filter(week == input$week) %>%
      mutate(matchup = paste(homeTeamAbbr, "vs", visitorTeamAbbr)) %>%
      filter(matchup == input$matchup) %>%
      pull(gameId)
    
    play_details <- plays %>%
      filter(gameId == selected_game, playId == as.numeric(input$play)) %>%
      pull(playDescription)
    if (length(play_details) == 0) return("No description available.")
    play_details
  })
  
  # Display play metrics
  output$play_metrics <- renderTable({
    req(input$week, input$matchup, input$play, input$quarter)
    
    selected_game <- games %>%
      filter(week == input$week) %>%
      mutate(matchup = paste(homeTeamAbbr, "vs", visitorTeamAbbr)) %>%
      filter(matchup == input$matchup) %>%
      pull(gameId)
    
    play_details <- plays %>%
      filter(gameId == selected_game, playId == as.numeric(input$play)) %>%
      {if (input$quarter != "All") filter(., quarter == as.numeric(input$quarter)) else .} %>%
      distinct(playId, .keep_all = TRUE) %>%  # Ensure unique rows
      select(
        possessionTeam, defensiveTeam, preSnapHomeScore, preSnapVisitorScore,
        ballCarrierDisplayName, quarter, down, yardsToGo, gameClock
      )
    
    if (nrow(play_details) == 0) return(NULL)
    
    data.frame(
      Metric = c("Offensive Team", "Defensive Team", "Score", "Ball Carrier", 
                 "Quarter", "Down", "Yards to Go", "Game Clock"),
      Value = c(
        play_details$possessionTeam,
        play_details$defensiveTeam,
        paste(play_details$preSnapHomeScore, "-", play_details$preSnapVisitorScore),
        play_details$ballCarrierDisplayName,
        play_details$quarter,
        play_details$down,
        play_details$yardsToGo,
        gsub("^0+", "", sub(":00$", "", play_details$gameClock))
      )
    )
  })
  
  
  # Reactive data for filtering players by team
  filtered_players <- reactive({
    rb_data_filtered <- rb_bc_plays %>%
      left_join(plays %>% 
                  select(gameId, playId, playResult, possessionTeam, defensiveTeam, 
                         offenseFormation, defendersInTheBox), 
                by = c("gameId", "playId")) %>%
      group_by(ballCarrierDisplayName, possessionTeam, defensiveTeam, gameId, playId, 
               offenseFormation, defendersInTheBox) %>%
      summarize(
        total_run_plays = n_distinct(playId),  # Count unique runs
        avg_yards_gained = mean(playResult, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Apply filters
    if (input$team != "All") {
      rb_data_filtered <- rb_data_filtered %>% filter(possessionTeam == input$team)
    }
    if (input$opponent != "All") {
      rb_data_filtered <- rb_data_filtered %>% filter(defensiveTeam == input$opponent)
    }
    if (input$formation != "All") {
      rb_data_filtered <- rb_data_filtered %>% filter(offenseFormation == input$formation)
    }
    if (input$defenders != "All") {
      rb_data_filtered <- rb_data_filtered %>% filter(defendersInTheBox == as.numeric(input$defenders))
    }
    
    return(rb_data_filtered %>% 
             group_by(ballCarrierDisplayName, possessionTeam) %>% 
             summarize(
               total_run_plays = sum(total_run_plays),
               avg_yards_gained = mean(avg_yards_gained, na.rm = TRUE),
               .groups = "drop"
             ))
    
  })
  
  # Update player selector based on team
  observe({
    available_players <- sort(unique(filtered_players()$ballCarrierDisplayName))
    updateSelectInput(session, "player", choices = available_players)
  })
  
  # ReactiveValues to store selected players
  selected_players <- reactiveValues(list = character(0))
  
  observeEvent(input$add_player, {
    req(input$player)  # Ensure input$player is not NULL
    selected_players$list <- unique(c(selected_players$list, input$player))
  })
  
  # Reactive value for clicked player
  clicked_player <- reactiveVal(NULL)
  
  # Update reactive value on plot click
  observeEvent(event_data("plotly_click", source = "rb_plot"), {
    click_info <- event_data("plotly_click", source = "rb_plot")
    if (!is.null(click_info)) {
      clicked_player(click_info$key)  # Use the player name as a key
    }
  })
  
  observeEvent(input$reset_button, {
    selected_players$list <- character(0)
    clicked_player(NULL)
    
    updateSelectInput(session, "team", selected = "All")
    updateSelectInput(session, "opponent", selected = "All")
    updateSelectInput(session, "formation", selected = "All")
    updateSelectInput(session, "defenders", selected = "All")
    updateSelectInput(session, "player", choices = NULL, selected = NULL)
  })
  
  
  # Update the player performance scatter plot
  output$rb_performance_plot <- renderPlotly({
    rb_data <- filtered_players()
    
    # Filter data based on selected players and clicked player
    if (length(selected_players$list) > 0) {
      rb_data <- rb_data %>% filter(ballCarrierDisplayName %in% selected_players$list)
    } else if (!is.null(clicked_player())) {
      rb_data <- rb_data %>% filter(ballCarrierDisplayName == clicked_player())
    }
    
    # Add dynamic colors
    rb_data <- rb_data %>%
      mutate(
        fill_color = team_colors_main[possessionTeam],
        outline_color = team_colors_outer[possessionTeam]
      )
    
    # Render Plotly scatter plot
    plot_ly(
      data = rb_data,
      x = ~total_run_plays,
      y = ~avg_yards_gained,
      text = ~paste(
        "Player:", ballCarrierDisplayName,
        "<br>Team:", possessionTeam,
        "<br>Total Runs:", total_run_plays,
        "<br>Avg Yards:", round(avg_yards_gained, 2)
      ),
      hoverinfo = "text",
      marker = list(
        color = ~fill_color,
        line = list(color = ~outline_color, width = 2),
        size = 10
      )
    ) %>%
      layout(
        title = "RB Performance: Total Runs vs Average Yards Gained",
        xaxis = list(title = "Total Run Plays"),
        yaxis = list(title = "Average Yards Gained")
      )
  })
  
  # Update the summary table based on clicked player
  output$rb_summary_table <- renderDT({
    data <- filtered_players()
    
    # Apply selected players filter
    if (length(selected_players$list) > 0) {
      data <- data %>% filter(ballCarrierDisplayName %in% selected_players$list)
    } else if (!is.null(clicked_player())) {
      data <- data %>% filter(ballCarrierDisplayName == clicked_player())
    }
    
    # Round numeric values to 3 significant figures
    data <- data %>%
      mutate(
        total_run_plays = signif(total_run_plays, 3),
        avg_yards_gained = signif(avg_yards_gained, 3)
      )
    
    # Render the datatable
    datatable(
      data %>% 
        select(
          ballCarrierDisplayName, possessionTeam, total_run_plays, avg_yards_gained) %>%
        arrange(desc(total_run_plays)),
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  epa_data <- rb_data %>%
    group_by(defensiveTeam, defendersInTheBox) %>%
    summarize(
      avg_epa = mean(expectedPointsAdded, na.rm = TRUE),
      total_plays = n(),
      .groups = "drop"
    )
  
  filtered_epa_data <- reactive({
    if (input$team_selection == "All") {
      epa_data %>%
        group_by(defendersInTheBox) %>%
        summarize(
          avg_epa = mean(avg_epa, na.rm = TRUE),
          total_plays = sum(total_plays),
          .groups = "drop"
        )
    } else {
      epa_data %>%
        filter(defensiveTeam == input$team_selection)
    }
  })
  
  # EPA graph
  output$epaGraph <- renderPlotly({
    p <- ggplot(filtered_epa_data(), aes(x = factor(defendersInTheBox), y = avg_epa)) +
      geom_col(fill = "steelblue", alpha = 0.7) +
      labs(
        title = "Average EPA vs Defenders in Box (RB Run Plays)",
        x = "Number of Defenders in the Box",
        y = "Average EPA"
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  # Precompute Negative EPA Rate
  negative_epa_rate <- rb_data %>%
    mutate(negative_epa = ifelse(expectedPointsAdded < 0, 1, 0)) %>%
    group_by(defensiveTeam, defendersInTheBox, offenseFormation) %>%
    summarize(
      success_rate = mean(negative_epa, na.rm = TRUE),  # Proportion of negative EPA
      total_plays = n(),
      .groups = "drop"
    )
  
  # Reactive Filter for Negative EPA
  filtered_negative_epa <- reactive({
    if (input$team_selection == "All") {
      negative_epa_rate %>%
        group_by(defendersInTheBox, offenseFormation) %>%
        summarize(
          success_rate = sum(success_rate * total_plays, na.rm = TRUE) / sum(total_plays),  # Weighted average
          total_plays = sum(total_plays),
          .groups = "drop"
        )
    } else {
      negative_epa_rate %>% filter(defensiveTeam == input$team_selection)
    }
  })
  
  # Negative EPA Rate Plot by Offensive Formation
  output$negative_epa_rate_plot <- renderPlotly({
    p <- ggplot(filtered_negative_epa(), aes(x = factor(defendersInTheBox), y = success_rate, fill = offenseFormation)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = "Negative EPA Rate by Defenders in Box and Formation",
        x = "Number of Defenders in the Box",
        y = "Negative EPA Rate",
        fill = "Offensive Formation"
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  
  tracking_closest_defenders <- reactive({
    req(input$play)  # Ensure a play is selected
  
    
    # Get ball carrier details
    play_details <- plays %>%
      filter(playId == as.numeric(input$play)) %>%
      slice(1)
    
    # Filter tracking data for the selected play
    play_tracking <- tracking %>%
      filter(playId == as.numeric(input$play), gameId == as.numeric(play_details$gameId))
    
    ball_carrier_id <- play_details$ballCarrierId
    defensive_team <- play_details$defensiveTeam
    
    # Validate that a ball carrier exists
    if (is.na(ball_carrier_id)) stop("No ball carrier ID found for the selected play.")
    
    # Get ball carrier positions
    ball_carrier <- play_tracking %>%
      filter(nflId == ball_carrier_id) %>%
      select(frameId, x, y)
    
    # Get defenders' positions
    defenders <- play_tracking %>%
      filter(club == defensive_team, !is.na(nflId)) %>%
      select(frameId, nflId, x, y, displayName)
    
    # Calculate distances between ball carrier and each defender
    closest_defenders <- ball_carrier %>%
      inner_join(defenders, by = "frameId") %>%
      mutate(distance = sqrt((x.x - x.y)^2 + (y.x - y.y)^2)) %>%
      group_by(frameId) %>%
      slice_min(order_by = distance, n = 3) %>%
      ungroup()
    
    return(closest_defenders)
  })
  
  output$defender_pressure_plot <- renderPlotly({
    req(tracking_closest_defenders())
    
    closest_defenders <- tracking_closest_defenders()
    
    p <- ggplot(closest_defenders, aes(x = frameId, y = distance, color = displayName)) +
      geom_line(size = 1) +
      labs(
        title = "Defender Distance to Ball Carrier (RB Run Plays)",
        x = "Frame ID",
        y = "Distance (yards)",
        color = "Defender"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$defensive_summary_table <- renderDT({
    epa_data %>%
      arrange(avg_epa) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        caption = htmltools::tags$caption(
          "Table: Average EPA and Total Plays by Defenders in the Box"
        )
      )
  })
  
  
  pbp22_clean <- pbp22 %>%
    mutate(
      gameId = as.double(old_game_id),  # Match games format
      playId = as.double(play_id)       # Match plays format
    )
  
  # Join pbp22 with plays to add defendersInTheBox
  pbp22_rb <- pbp22_clean %>%
    inner_join(
      plays %>% select(gameId, playId, defendersInTheBox, offenseFormation, defensiveTeam), 
      by = c("gameId", "playId")
    ) %>%
    inner_join(rb_bc_plays, by = c("gameId", "playId"))  # Focus on RB ball carriers
  
  # Precompute Tackling Success Rates
  tackle_success <- pbp22_rb %>%
    group_by(defensiveTeam, defendersInTheBox, offenseFormation) %>%
    summarize(
      total_tackles = sum(solo_tackle, na.rm = TRUE) + sum(assist_tackle, na.rm = TRUE),
      solo_tackles = sum(solo_tackle, na.rm = TRUE),
      tackle_success_rate = ifelse(total_tackles > 0, solo_tackles / total_tackles, NA),
      .groups = "drop"
    )
  
  # Reactive Filter for Tackling Success
  filtered_tackle_success <- reactive({
    if (input$team_selection == "All") {
      tackle_success %>%
        group_by(defendersInTheBox, offenseFormation) %>%
        summarize(
          total_tackles = sum(total_tackles),
          solo_tackles = sum(solo_tackles),
          tackle_success_rate = ifelse(total_tackles > 0, solo_tackles / total_tackles, NA),
          .groups = "drop"
        )
    } else {
      tackle_success %>% filter(defensiveTeam == input$team_selection)
    }
  })
  
  # Plot the tackle success rates
  output$tackling_success_plot <- renderPlotly({
    p <- ggplot(filtered_tackle_success(), aes(x = factor(defendersInTheBox), y = tackle_success_rate, fill = offenseFormation)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = "Tackling Success Rate by Defenders in Box and Offensive Formation",
        x = "Number of Defenders in the Box",
        y = "Tackle Success Rate",
        fill = "Offensive Formation"
      ) +
      theme_minimal()
    ggplotly(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
