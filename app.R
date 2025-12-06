library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
source("lib.R")

options(shiny.autoreload = TRUE)

sediment_data <- read_sediment_data()
station_list <- sediment_data %>%
  group_by(region, subregion, country, station_name, station_latitude, station_longitude, station_type, waterbody_type) %>%
  summarize(
    determinands = n_distinct(determinand),
    measurements = n(),
    years = n_distinct(monitoring_year),
    min_year = min(monitoring_year, na.rm = TRUE),
    max_year = max(monitoring_year, na.rm = TRUE),
    interval = as.integer(max(monitoring_year, na.rm = TRUE) - min(monitoring_year, na.rm = TRUE) + 1)
  ) %>% 
  ungroup() %>% 
  arrange(region, subregion, country, station_name)

ui <- fluidPage(
  titlePanel("2025 OSPAR CEMP assessment"),
  navset_pill(
    nav_panel("Explore stations",
      fluidRow(
        column(4, selectInput("subregion_filter", "Subregion:", choices = c("All", unique(station_list$subregion)), selected = "All")),
        column(4, selectInput("country_filter", "Country:", choices = c("All", unique(station_list$country)), selected = "All"))
      ),
      DT::dataTableOutput("station_list")
    )
  )
)

server <- function(input, output) {
 filtered_data <- reactive({
    data <- station_list
    if (input$subregion_filter != "All") {
      data <- data %>% filter(station_list$subregion == input$subregion_filter)
    }
    if (input$country_filter != "All") {
      data <- data %>% filter(country == input$country_filter)
    }
    data %>%
      select(subregion, country, station_name, station_type, waterbody_type, years, min_year, max_year)
  })
  output$station_list <- DT::renderDataTable(DT::datatable(filtered_data(), options = list(pageLength = 15)))
}

shinyApp(ui = ui, server = server)
