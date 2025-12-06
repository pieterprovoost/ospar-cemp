library(dplyr)

read_sediment_data <- function(sediment_data_path = "data/sediment_data.csv") {
  read.csv(sediment_data_path) %>% 
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
