library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(tidyr)
library(sf)
library(leaflet)
source("lib.R")

options(shiny.autoreload = TRUE)

# UI

ui <- page_fluid(
  titlePanel("2025 OSPAR CEMP assessment"),
  p("OSPAR is the mechanism by which 15 Governments and the European Union cooperate to protect the marine environment of the North-East Atlantic.
  OSPAR's Coordinated Environmental Monitoring Programme (CEMP) aims to deliver comparable data from across the OSPAR Maritime Area.
  This application visualizes data collected under the CEMP on sediment contaminants in the North-East Atlantic. The dataset is hosted by ICES and publicly available.
  The source code for this dashboard is available from https://github.com/pieterprovoost/ospar-cemp.", style = "margin-bottom: 20px;"),
  navset_pill(
    nav_panel(
      "Station timeseries",
      p("This tab lists all stations with sediment data, along with some statistics on the number of years with data and the period covered. The table can be filtered by subregion or country. Select a station in the table to inspect timeseries for the different contaminant groups. While the dataset contains concentrations for individual contaminants, concentrations have been summed per contaminant group to keep the visualization comprehensible. The plotted values can be optionally log transformed.", style = "margin-top: 20px;"),
      fluidRow(
        column(4, selectInput("subregion_filter", "Subregion:", choices = NULL)),
        column(4, selectInput("country_filter_1", "Country:", choices = NULL)),
        column(4, checkboxInput("log_transform", "Log transform concentrations", FALSE))
      ),
      DT::dataTableOutput("station_list"),
      plotOutput("station_plot")
    ),
    nav_panel(
      "Compare contaminant groups",
      p("This tab visualizes the average concentration of different contaminants for each station. Click the bubbles to get more information for a specific station.", style = "margin-top: 20px;"),
      fluidRow(
        column(4, selectInput("determinand_group_select_1", "Contaminant group 1:", choices = NULL)),
        column(4, selectInput("determinand_group_select_2", "Contaminant group 2:", choices = NULL))
      ),
      plotOutput("determinand_group_plot")
    ),
    nav_panel(
      "Station map",
      p("This tab visualizes the average concentration of different contaminants for each station on a map.", style = "margin-top: 20px;"),
      fluidRow(
        column(4, selectInput("determinand_group_select_3", "Contaminant group:", choices = NULL))
      ),
      leafletOutput("determinand_map_plot", height = "600px")
    ),
    nav_panel(
      "Compare countries",
      p("This tab compares contaminant concentrations between two countries. To assess whether concentrations for different groups differ between countries, Mann-Whitney U tests are performed for every contaminant group.", style = "margin-top: 20px;"),
      fluidRow(
        column(4, selectInput("country_filter_2", "Country:", choices = NULL)),
        column(4, selectInput("country_filter_3", "Country:", choices = NULL))
      ),
      plotOutput("country_plot"),
      verbatimTextOutput("country_results")
    )
  )
)

# server

server <- function(input, output, session) {

  sediment_data <- reactive({
    tryCatch(
      {
        read_sediment_data()
      },
      error = function(e) {
        showNotification("Could not load sediment data. Please check that the data file exists.", type = "error", duration = NULL)
        return(NULL)
      }
    )
  })

  station_list <- reactive({
    req(sediment_data())
    sediment_data() %>%
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
    })

  determinand_group_total_per_station <- reactive({
    req(sediment_data())
    sediment_data() %>% 
      group_by(subregion, country, waterbody_type, station_type, station_name, station_longitude, station_latitude, sample_date, determinand_group) %>% 
      summarize(concentration = sum(concentration)) %>% 
      group_by(subregion, country, waterbody_type, station_type, station_name, station_longitude, station_latitude, determinand_group) %>% 
      summarize(concentration = mean(concentration))
  })

  determinand_group_total_per_station_wide <- reactive({
    req(determinand_group_total_per_station())
    determinand_group_total_per_station() %>%
      pivot_wider(id_cols = c(subregion, country, waterbody_type, station_type, station_name, station_longitude, station_latitude), names_from = determinand_group, values_from = concentration)
  })

  filtered_data <- reactive({
    req(station_list())
    data <- station_list()
    if (input$subregion_filter != "All") {
      data <- data %>% filter(subregion == input$subregion_filter)
    }
    if (input$country_filter_1 != "All") {
      data <- data %>% filter(country == input$country_filter_1)
    }
    data %>%
      select(subregion, country, station_name, station_type, waterbody_type, years, min_year, max_year)
  })
  
  observe({
    req(sediment_data())
    subregions <- c("All", unique(sediment_data()$subregion))
    countries <- c("All", unique(sediment_data()$country))
    countries_noall <- c("All", unique(sediment_data()$country))
    determinand_groups <- unique(sediment_data()$determinand_group)
    updateSelectInput(session, "subregion_filter", choices = subregions, selected = "All")
    updateSelectInput(session, "country_filter_1", choices = countries, selected = "All")
    updateSelectInput(session, "country_filter_2", choices = countries_noall, selected = "France")
    updateSelectInput(session, "country_filter_3", choices = countries_noall, selected = "Germany")
    updateSelectInput(session, "determinand_group_select_1", choices = determinand_groups, selected = determinand_groups[1])
    updateSelectInput(session, "determinand_group_select_2", choices = determinand_groups, selected = if(length(determinand_groups) > 1) determinand_groups[2] else determinand_groups[1])
    updateSelectInput(session, "determinand_group_select_3", choices = determinand_groups, selected = determinand_groups[1])
  })

  output$station_list <- DT::renderDataTable(
    filtered_data() %>% 
      DT::datatable(selection = "single", rownames = FALSE, options = list(pageLength = 10, searching = FALSE))
  )
  
  output$station_plot <- renderPlot({
    req(input$station_list_rows_selected)
    
    selected_station <- filtered_data()$station_name[input$station_list_rows_selected]
    
    station_data <- sediment_data() %>% 
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
    req(input$determinand_group_select_1)
    req(input$determinand_group_select_2)
    ggplot() +
      geom_point(data = determinand_group_total_per_station_wide(), aes_string(x = as.name(input$determinand_group_select_1), y = as.name(input$determinand_group_select_2), col = "subregion", shape = "country"), size = 2.5) +
      scale_x_continuous(trans = "log10") +
      scale_y_continuous(trans = "log10") +
      scale_shape_manual(values = c(15:19, 0:14)) + 
      theme_minimal()
  })

  output$determinand_map_plot <- renderLeaflet({
    req(input$determinand_group_select_3)
    data <- determinand_group_total_per_station_wide()
    data$concentration <- data[[input$determinand_group_select_3]]
    data$size <- data[[input$determinand_group_select_3]] / max(data[[input$determinand_group_select_3]], na.rm = TRUE)
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~station_longitude,
        lat = ~station_latitude,
        radius = ~sqrt(size) * 10,
        fillOpacity = 0.1,
        stroke = TRUE,
        weight = 1,
        popup = ~paste0("<b>", station_name, "</b><br>Subregion: ", subregion, "<br>Country: ", country, "<br>Average concentration: ", signif(concentration, 4), " ", default_units)
      ) %>%
      setView(lng = -2.5, lat = 50, zoom = 4)
  })

  output$country_plot <- renderPlot({
    data <- determinand_group_total_per_station() %>% 
      filter(country %in% c(input$country_filter_2, input$country_filter_3))

    ggplot() +
      geom_violin(data = data, aes(country, concentration)) +
      geom_jitter(data = data, aes(country, concentration), height = 0, width = 0.2) +
      facet_wrap(~determinand_group, scales = "free") +
      theme_minimal()
  })

  output$country_results <- renderText({
    data <- determinand_group_total_per_station() %>% 
      filter(country %in% c(input$country_filter_2, input$country_filter_3))
    
    det_groups <- unique(data$determinand_group)
    
    results <- sapply(det_groups, function(group) {
      group_data <- data %>% filter(determinand_group == group)
      
      country1_data <- group_data %>% filter(country == input$country_filter_2) %>% pull(concentration)
      country2_data <- group_data %>% filter(country == input$country_filter_3) %>% pull(concentration)
      
      if (length(country1_data) > 0 && length(country2_data) > 0) {
        test <- wilcox.test(country1_data, country2_data)
        sprintf("%s: p-value = %.5f (n1=%d, n2=%d)", group, test$p.value, length(country1_data), length(country2_data))
      } else {
        sprintf("%s: Not enough data", group)
      }
    })
    paste(c(sprintf("Mann-Whitney U results (%s vs %s):\n", input$country_filter_2, input$country_filter_3), results), collapse = "\n")
  })

}

shinyApp(ui = ui, server = server)
