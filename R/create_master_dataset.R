library(readr)
library(purrr)
library(janitor)
library(dplyr)
library(stringr)

csv_files <- str_subset(list.files('data/raw_data'), 'of_2.csv$') # get all .csv files

# read all .csv files
trade_data <- purrr::map_df(paste0('data/raw_data/', csv_files), 
                            function(data_filepath) {
                              output <-
                                read_csv(data_filepath) %>% 
                                janitor::clean_names() %>%
                                select(cn8, port_name, country, year, month, value, net_mass_kg, eu_non_eu) %>%
                                mutate(commodity_code = str_extract(cn8, '^[:digit:]*'),
                                       description = str_replace(cn8, '^[:digit:]*\\s*', '')) %>%
                                select(-cn8) %>%
                                filter(nchar(commodity_code) == 8)# remove EU data as no port information from EU countries
                              
                              return(output)
                          
                            })



trade_data <-
    
  trade_data %>%
    group_by(commodity_code, 
             description,
             port_name,
             country,
             year,
             eu_non_eu) %>%
    summarise(value = sum(value, na.rm = TRUE),
              net_mass_kg = sum(net_mass_kg, na.rm = TRUE)) %>%
  ungroup()


trade_data <-
  trade_data %>%
  mutate(port_name = trimws(str_replace_all(port_name, 'Airp$', 'Airport')),
         port_name = recode(port_name,
                            "London Heathrow" = "London Heathrow Airport",
                            "London Stansted" = "London Stansted Airport",
                            "London Gatwick" = "London Gatwick Airport",
                            "Robin Hood Airport" = "Robin Hood Doncaster Sheffield",
                            "East Mids Airport" = "East Midlands Airport",
                            "Newport Gwent" = "Newport, Gwent",
                            "Belfast Int Airport" = "Belfast International Airport",#
                            "Humberside Airport"= "Humberside International Airport",
                            "Liverpool Lennon Air" = "Liverpool John Lennon Airport"))

# scraped from HMRC: 
# https://www.gov.uk/government/publications/uk-trade-tariff-freight-location-codes/freight-location-codes
port_type <- read_csv('data/raw_data/port_data.csv') %>% janitor::clean_names()

port_type <- 
  port_type %>%
  mutate(port = recode(port,
                       "Bournemouth (Hurn) Airport" = "Bournemouth Airport",
                       "Cardiff (Wales) Airport" = "Cardiff Airport"))
if (length(setdiff(trade_data$port_name, append(port_type$port,c("Inland Clearance", "PoC Unknown", "Not Collected")))) > 0) stop("Trade data port_name is missing from port_type data object. Check that the spellings of the port names match")


trade_data <- 
  trade_data %>%
  left_join(port_type %>% select(port, port_type), 
            by = c('port_name' = 'port'))

# remove 'year' column (not relevant to current analysis)
trade_data <- 
  trade_data %>%
  group_by(commodity_code,
           description, 
           country,
           port_type,
           eu_non_eu) %>%
  summarise(value = sum(value, na.rm = TRUE),
            mass = sum(net_mass_kg, na.rm = TRUE)) %>%
  ungroup()



# Removing CN8 codes with no port data. 
# See R/eda.R for justification.

# Create vector with commodity codes with no port data
no_port_data_codes <-
  trade_data %>%
  distinct(commodity_code, port_type) %>%
  group_by(commodity_code) %>%
  mutate(cc_n = n()) %>%
  filter(cc_n == 1 & is.na(port_type)) %>%
  pull(commodity_code)

# remove commodity codes with no port data from master dataset
trade_data <-
  trade_data %>%
  filter(!commodity_code %in% no_port_data_codes)



calc_port_type_vals <- function(type_of_port) {
  
  # handling na through an if statement (changing the filter)
  if (!is.na(type_of_port)) {
    
    port_value <- 
      trade_data %>%
      filter(port_type == type_of_port) %>%
      select(commodity_code, country, value, mass) %>%
      group_by(commodity_code, country) %>%
      mutate("total_{type_of_port}_value" := sum(value, na.rm = TRUE),
             "total_{type_of_port}_mass" := sum(mass, na.rm = TRUE)) %>%
      select(-c(mass,value))
    
  } else {
    
    port_value <- 
      trade_data %>%
      filter(is.na(port_type)) %>%
      select(commodity_code, country, value, mass) %>%
      group_by(commodity_code, country) %>%
      mutate("total_{type_of_port}_value" := sum(value, na.rm = TRUE),
             "total_{type_of_port}_mass" := sum(mass, na.rm = TRUE)) %>%
      select(-c(mass,value))
    
  }
  
  
  
  return(port_value)

}

port_type_values_ls <- list()
all_port_type_values <- 
  
  purrr::map_df(unique(trade_data$port_type), function(x) {
  
  port_type_values_ls <- append(port_type_values_ls, calc_port_type_vals(x))
  
  return(port_type_values_ls)
  
}) 



# this merges rows of the same commodity type and country together
all_port_type_values <- 
  all_port_type_values %>%
  group_by(commodity_code, country) %>%
  summarise(across(starts_with('total_'), ~sum(.x, na.rm = TRUE)))



# join in Sea/Rail/NA totals to calculate the % by each transport mode
trade_data <- 
  trade_data %>%
  left_join(all_port_type_values, by = c("commodity_code",
                                         "country")) %>%
  group_by(commodity_code, country) %>%
  mutate(value_total = sum(value, na.rm = TRUE),
         mass_total = sum(mass, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(across(matches('^total_.*_value$'), ~ .x / value_total, .names = "{.col}_perc"),  # divides any columns with total_ & _value by the total_value
         across(matches('^total_.*_mass$'), ~ .x / mass_total, .names = "{.col}_perc")) # divides any columns with total_ & _mass by the total_mass

write_rds(trade_data, "data/master_data/master_data_set.rds")





