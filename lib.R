library(dplyr)

default_units <- "mg/kg"
data_folder <- "data"
data_file <- "sediment_data.csv"
data_path <- file.path(data_folder, data_file)

read_sediment_data <- function() {
  read.csv(data_path) %>% 
    mutate(sample_date = lubridate::ymd(sample_date)) %>% 
    # convert all to mg/kg
    mutate(
      concentration = case_when(
        unit == "ug/kg" ~ concentration / 1000,
        TRUE ~ concentration
      ),
      unit = case_when(
        unit == "ug/kg" ~ "mg/kg",
        TRUE ~ unit
      )
    ) %>% 
    # remove impossible values
    filter(concentration < 1000000)
}
