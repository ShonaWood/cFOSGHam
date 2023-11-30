library(tidyverse)
library(shiny)
library(plotly)

# Filtering the cdf dataset to remove 'amb' and then sample 10 random IDs (reiterate untill all ID have been checked)
random_IDs <- cdf %>%
  filter(ID != "amb") %>%
  distinct(ID) %>%
  sample_n(10) %>%
  pull(ID)

# Subset cdf for the randomly selected IDs
cdf_random <- cdf %>%
  filter(ID %in% random_IDs)

# Function to create a plot for a single ID
create_plot_for_id <- function(id) {
  data_for_id <- subset(cdf_random, ID == id)
  
  plot <- ggplot(data_for_id, aes(x = datetime, y = t_bat, color = ID)) +
    geom_point(alpha = 0.5, size = 1) +
    geom_line(data = data_for_id[!is.na(data_for_id$ap_t),], aes(y = ap_t), size = 0.5) +
    geom_line(aes(y = tc), alpha = 0.5, size = 0.5, color = 'gray50') +
    theme_minimal() +
    labs(title = paste("Temperature Readings for", id, "over Time"),
         x = "Datetime",
         y = "Temperature",
         color = "Bat ID")
  
  return(ggplotly(plot))
}

# Define the UI
ui <- fluidPage(
  titlePanel("Temperature Readings for Random IDs over Time"),
  mainPanel(
    plotlyOutput("plot"),
    actionButton("next_button", "Next")
  )
)

# Define the server logic
server <- function(input, output, session) {
  plot_index <- reactiveVal(1)
  output$plot <- renderPlotly({
    create_plot_for_id(random_IDs[plot_index()])
  })
  observeEvent(input$next_button, {
    if (plot_index() < length(random_IDs)) {
      plot_index(plot_index() + 1)
    } else {
      plot_index(1)
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)
