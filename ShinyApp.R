library(tidyverse)
library(shiny)
library(plotly)
library(DT)
library(bslib)
library(jpeg)
library(png)

# Load the necessary data
data <- read_csv("data/working_data.csv")
heatmap_data = data %>% select(week, season, displayName, offenseFormation, defensiveTeam, playDescription, ballCarrierDisplayName, x, y, gameId, playId)

print(heatmap_data)
# Football field image
field_image <- readJPEG("football-field.jpg")

# Define field dimensions
field_length <- 120  # yards including end zones
field_width <- 53.3  # yards

# Create the heatmap plot with football field dimensions and yard markers
tackle_heatmap <- ggplot(heatmap_data, aes(x = x, y = y)) +
  
  # Add the football field image as background
  annotation_raster(field_image, xmin = -3, xmax = 123, ymin = 0, ymax = 53.3, interpolate = TRUE) +
  
  geom_bin2d(bins = 60, alpha = 0.6) +  
  scale_fill_gradient(low = "blue", high = "red") +
  
  # Set x-axis with center field and end zones
  scale_x_continuous(breaks = seq(0, 120, by = 10), limits = c(0, 120), expand = c(0, 0),
                     labels = function(x) ifelse(x == 0 | x == 120, "End Zone", x)) +
  scale_y_continuous(limits = c(0, field_width), expand = c(0, 0)) +
  
  labs(title = "Tackle Position Heatmap on Football Field", x = "Yard Line", y = "Field Width (Yards)") +
  theme_minimal() +
  theme(
    aspect.ratio = field_width / field_length,
    panel.grid.major = element_blank() # Remove grid lines since we have football field image
  )

# Define UI with bslib 
ui <- fluidPage(
  theme = bs_theme(
    version = 4,
    bootswatch = "cerulean", # Can change theme if desired
    primary = "#007bff", # Primary color 
    font_scale = 1.2
  ),
  
  titlePanel("NFL Tackle Analysis"),
  tabsetPanel(
    tabPanel("Tackle Heatmap",
             plotlyOutput("heatmapPlot", width = "70vw", height = "55vh"),
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
                 selectInput("playerID", "Select Player:", choices = unique(data$displayName), selected = NULL),
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
      clicked_data <- heatmap_data %>%
        filter(abs(x - x_clicked) < 1, abs(y - y_clicked) < 1)
      
      output$heatmapDataTable <- renderDT({
        datatable(clicked_data)
      })
    }
  })
  
  # Density Plot by Yard Line
  output$densityPlot <- renderPlotly({
    filtered_data <- data %>% filter(x >= input$yardLineRange[1], x <= input$yardLineRange[2])
    density_plot <- ggplot(filtered_data, aes(x = x)) +
      geom_density(fill = "#006700", alpha = 0.6) +
      labs(title = "Player Density by Yard Line", x = "Yard Line", y = "Density") +
      theme_minimal() 
    ggplotly(density_plot)
  })
  
  
  # Average Tackle Distance Plot
  output$distancePlot <- renderPlotly({
    # Check if the necessary columns are available in the data
    req("player_id" %in% colnames(data), "time" %in% colnames(data), "distance" %in% colnames(data))
    
    # Filter data for the selected player and time range
    filtered_data <- data %>%
      filter(displayName == input$playerID, time >= input$timeRange[1], time <= input$timeRange[2])
    
    # Check if filtered data is empty and handle it
    if (nrow(filtered_data) == 0) {
      showNotification("No data available for the selected player and time range.", type = "warning")
      return(NULL)
    }
    
    # Create the average tackle distance plot
    avg_distance_plot <- ggplot(filtered_data, aes(x = time, y = distance)) +
      geom_line(color = "purple") +
      labs(title = "Average Tackle Distance Over Time", x = "Time (Seconds)", y = "Distance (Yards)") +
      theme_minimal()
    
    ggplotly(avg_distance_plot)
  })
}

# Run the application
shinyApp(ui = ui, server = server)