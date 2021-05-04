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
                                select(cn8, port_name, country, year, month, value, net_mass_kg) %>%
                                mutate(commodity_code = str_extract(cn8, '^[:digit:]*'),
                                       description = str_replace(cn8, '^[:digit:]*\\s*', '')) %>%
                                select(-cn8) %>%
                                filter(nchar(commodity_code) == 8)
                              
                              return(output)
                          
                            })



trade_data <-
    
  trade_data %>%
    group_by(commodity_code, 
             description,
             port_name,
             country,
             year) %>%
    summarise(value = sum(value, na.rm = TRUE),
              net_mass_kg = sum(net_mass_kg, na.rm = TRUE))


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
                            "Belfast Int Airport" = "Belfast International Airport"))

port_type <- read_csv('data/raw_data/port_data.csv') %>% janitor::clean_names()

port_type <- 
  port_type %>%
  mutate(port = recode(port,
                       "Bournemouth (Hurn) Airport" = "Bournemouth Airport",
                       "Cardiff (Wales) Airport" = "Cardiff Airport"))


trade_data <- 
  trade_data %>%
  left_join(port_type %>% select(port, port_type), 
            by = c('port_name' = 'port'))

trade_data <- 
  trade_data %>%
  group_by(commodity_code,
           description, 
           port_name,
           country,
           port_type) %>%
  summarise(value = sum(value, na.rm = TRUE),
            mass = sum(net_mass_kg, na.rm = TRUE))








trade_data %>%
  group_by(commodity_code,
           description,
           port_name,
           country) %>%
  mutate(mass_trans_perc = mass/sum(mass, na.rm = TRUE),
         value_trans_perc = value/sum(value, na.rm = TRUE)) -> foo




  
  
  