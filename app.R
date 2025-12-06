library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(tidyr)
source("lib.R")

options(shiny.autoreload = TRUE)

# data loading

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
  , .groups = "drop") %>% 
  arrange(region, subregion, country, station_name)

determinand_group_total_per_station <- sediment_data %>% 
  group_by(subregion, country, waterbody_type, station_type, station_name, sample_date, determinand_group) %>% 
  summarize(concentration = sum(concentration)) %>% 
  group_by(subregion, country, waterbody_type, station_type, station_name, determinand_group) %>% 
  summarize(concentration = mean(concentration))

determinand_group_total_per_station_wide <- determinand_group_total_per_station %>%
  pivot_wider(id_cols = c(subregion, country, waterbody_type, station_type, station_name), names_from = determinand_group, values_from = concentration)

# UI

ui <- page_fluid(
  titlePanel("2025 OSPAR CEMP assessment"),
  navset_pill(
    nav_panel(
      "Station explorer",
      p("Explore measurement time series by station. Select a station to inspect the data."),
      fluidRow(
        column(4, selectInput("subregion_filter", "Subregion:", choices = c("All", unique(station_list$subregion)), selected = "All")),
        column(4, selectInput("country_filter", "Country:", choices = c("All", unique(station_list$country)), selected = "All")),
        column(4, checkboxInput("log_transform", "Log transform concentrations", FALSE))
      ),
      DT::dataTableOutput("station_list"),
      plotOutput("station_plot")
    ),
    nav_panel(
      "Determinand group concentrations",
      fluidRow(
        column(4, selectInput("determinand_group_select_1", "Determinand group 1:", choices = unique(sediment_data$determinand_group), selected = "Metals")),
        column(4, selectInput("determinand_group_select_2", "Determinand group 2:", choices = unique(sediment_data$determinand_group), selected = "Polychlorinated biphenyls"))
      ),
      plotOutput("determinand_group_plot")
    )
  )
)

# server

server <- function(input, output) {
  filtered_data <- reactive({
    data <- station_list
    if (input$subregion_filter != "All") {
      data <- data %>% filter(subregion == input$subregion_filter)
    }
    if (input$country_filter != "All") {
      data <- data %>% filter(country == input$country_filter)
    }
    data %>%
      select(subregion, country, station_name, station_type, waterbody_type, years, min_year, max_year)
  })
  
  output$station_list <- DT::renderDataTable(
    filtered_data() %>% 
      DT::datatable(selection = "single", rownames = FALSE, options = list(pageLength = 10, searching = FALSE))
  )
  
  output$station_plot <- renderPlot({
    req(input$station_list_rows_selected)
    
    selected_station <- filtered_data()$station_name[input$station_list_rows_selected]
    
    station_data <- sediment_data %>% 
      filter(station_name == selected_station)
    
    station_data_determinand_group <- station_data %>% 
      group_by(sample_date, determinand_group) %>% 
      summarize(concentration = sum(concentration), .groups = "drop")
    
    trans <- "identity"
    if (input$log_transform) {
      trans <- "log10"
    }
    suppressWarnings({
      ggplot() +
        geom_point(data = station_data_determinand_group, aes(sample_date, concentration, color = determinand_group), shape = 21, size = 2.5) +
        geom_smooth(data = station_data_determinand_group, aes(sample_date, concentration, color = determinand_group), lwd = 0.7, alpha = 0.2) +
        scale_y_continuous(trans = trans) +
        theme_minimal()
    })
  })

  output$determinand_group_plot <- renderPlot({
    ggplot() +
      geom_point(data = determinand_group_total_per_station_wide, aes_string(x = as.name(input$determinand_group_select_1), y = as.name(input$determinand_group_select_2), col = "subregion", shape = "country"), size = 2.5) +
      scale_x_continuous(trans = "log10") +
      scale_y_continuous(trans = "log10") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
