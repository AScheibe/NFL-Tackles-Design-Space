library(tidyverse)
library(shiny)
library(plotly)
library(DT)

# Load the necessary data
data <- read_csv("data/working_data.csv")

# Define field dimensions
field_length <- 120  # yards including end zones
field_width <- 53.3  # yards

# Create the heatmap plot with football field dimensions and yard markers
tackle_heatmap <- ggplot(data, aes(x = avg_x, y = avg_y)) +
  geom_bin2d(bins = 60) +
  scale_fill_gradient(low = "blue", high = "red") +
  scale_x_continuous(breaks = seq(0, field_length, by = 10), limits = c(0, field_length), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, field_width), expand = c(0, 0)) +
  labs(title = "Tackle Position Heatmap", x = "Yard Line", y = "Field Width (Yards)") +
  theme_minimal() +
  theme(
    aspect.ratio = field_width / field_length,
    panel.grid.major.x = element_line(color = "gray", linetype = "dashed")
  )

# Define UI
ui <- fluidPage(
  titlePanel("NFL Tackle Analysis"),
  tabsetPanel(
    tabPanel("Tackle Heatmap",
             plotlyOutput("heatmapPlot", height = "800px"),
             DTOutput("heatmapDataTable")
    ),
    tabPanel("Player Density by Yard Line",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("yardLineRange", "Yard Line Range:", min = 0, max = field_length, value = c(0, field_length))
               ),
               mainPanel(
                 plotlyOutput("densityPlot")
               )
             )
    ),
    tabPanel("Average Tackle Distance",
             sidebarLayout(
               sidebarPanel(
                 selectInput("playerID", "Select Player:", choices = unique(data$player_id)),
                 sliderInput("timeRange", "Time Range (Seconds):", min = 0, max = 60, value = c(0, 60))
               ),
               mainPanel(
                 plotlyOutput("distancePlot")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Heatmap Plot
  output$heatmapPlot <- renderPlotly({
    ggplotly(tackle_heatmap, source = "heatmap_click")
  })
  
  # Update table based on clicks on heatmap
  observeEvent(event_data("plotly_click", source = "heatmap_click"), {
    click_data <- event_data("plotly_click", source = "heatmap_click")
    if (!is.null(click_data)) {
      x_clicked <- click_data$x
      y_clicked <- click_data$y
      clicked_data <- data %>%
        filter(abs(avg_x - x_clicked) < 1, abs(avg_y - y_clicked) < 1)
      
      output$heatmapDataTable <- renderDT({
        datatable(clicked_data)
      })
    }
  })
  
  # Density Plot by Yard Line
  output$densityPlot <- renderPlotly({
    filtered_data <- data %>% filter(avg_x >= input$yardLineRange[1], avg_x <= input$yardLineRange[2])
    density_plot <- ggplot(filtered_data, aes(x = avg_x)) +
      geom_density(fill = "green", alpha = 0.5) +
      labs(title = "Player Density by Yard Line", x = "Yard Line", y = "Density")
    ggplotly(density_plot)
  })
  
  # Average Tackle Distance Plot
  output$distancePlot <- renderPlotly({
    filtered_data <- data %>%
      filter(player_id == input$playerID, time >= input$timeRange[1], time <= input$timeRange[2])
    avg_distance_plot <- ggplot(filtered_data, aes(x = time, y = distance)) +
      geom_line(color = "purple") +
      labs(title = "Average Tackle Distance Over Time", x = "Time (Seconds)", y = "Distance (Yards)")
    ggplotly(avg_distance_plot)
  })
}

# Run the application
shinyApp(ui = ui, server = server)