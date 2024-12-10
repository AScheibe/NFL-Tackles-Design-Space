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
        helpText("View and analyze frame by frame play data.")
      ),
      mainPanel(
        #TODO: MAX ADD CONTENT
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
}

# Run the app
shinyApp(ui = ui, server = server)
