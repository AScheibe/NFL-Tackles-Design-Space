library(shiny)
library(tidyverse)
library(plotly)

# Load data
data <- read.csv("data/working_data.csv")

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
    avg_pass_prob = mean(passProbability, na.rm = TRUE),
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

# New grouped data for EPA visualization
epa_data <- data %>%
  filter(!is.na(expectedPointsAdded) & 
           !is.na(defendersInTheBox)) %>%
  group_by(defendersInTheBox) %>%
  summarize(
    avg_epa = mean(expectedPointsAdded, na.rm = TRUE)
  )

# UI
ui <- fluidPage(
  tabsetPanel(
    # Tab 1: Placeholder for other visualizations
    tabPanel(
      "Overview",
      sidebarLayout(
        sidebarPanel(
          helpText("Overview of player performance data.")
        ),
        mainPanel(
          h3("This is the Overview tab. Add content here.")
        )
      )
    ),
    # Tab 2: Distance vs Pass Probability
    tabPanel(
      "Defensive Box Dashboard",
      sidebarLayout(
        sidebarPanel(
          helpText("Hover over the graphs to view metrics.")
        ),
        mainPanel(
          plotlyOutput("smoothedPlot"),
          plotlyOutput("barGraph"), # Add bar graph output
          plotlyOutput("epaGraph"), # Add new EPA graph output
      )
    )
    ),
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
      ) + # Smoothed line
      geom_area(
        stat = "smooth",
        method = "loess",
        fill = "red",
        span = 0.3,
        alpha = 0.5
      ) + # Filled area under smoothed line
      labs(
        title = "Player Distance Traveled by Pass Probability",
        x = "Pass Probability",
        y = "Avg Distance Traveled"
      ) +
      theme_minimal()
    
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
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5) # Force labels to be centered under bars
      )
    
    ggplotly(p)
  })
  
  # New EPA graph
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
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5) # Center x-axis labels
      )
    
    ggplotly(p)
  })
}

# Run the app
shinyApp(ui = ui, server = server)

