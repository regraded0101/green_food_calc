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

# add type as Road for EU countries (assumption of the model)
trade_data <-
  trade_data %>%
  mutate(port_type = case_when(eu_non_eu == "EU" & is.na(port_type) ~ "Road/Sea_EC",
                               T ~ port_type))

# create a function to calculate the co2 emmissions based on country and port_type

calculate_co2 <- function(import_country, commodity) {
  
  likely_transport_type <- 
    trade_data %>%
    filter(country == import_country,
           description == commodity) %>%
    slice_max(value) %>%
    pull(port_type)
  
  if (is.na(likely_transport_type)) {
    return(as.numeric(NA))
  }
  
  # for EU countries, need to combine road and EC co2
  else if (likely_transport_type == "Road/Sea_EC") {
    
    # get the road distance and multiply by the road co2 factor
    
    likely_distance_road <- 
      sea_air_road_distances %>%
      filter(country ==  import_country,
             type == "Road") %>%
      pull(distance)
    
    likely_emmission_road <- likely_distance_road * (emmission_factor %>% filter(type=="Road") %>% pull(kg_co2e))
    
    
    # get the EC distance and mutliply by EC emmission factor
    
    likely_distance_EC <-
      sea_air_road_distances %>%
      filter(country ==  import_country,
             type == "Sea_EC") %>%
      pull(distance)
    
    likely_emmission_EC <- likely_distance_EC * (emmission_factor %>% filter(type=="Sea_EC") %>% pull(kg_co2e))
    
    # sum the values to get total emmissions  
    likely_emmission_road_total <- likely_emmission_road + likely_emmission_EC
    
    return(likely_emmission_road_total)
    
    } else {
    likely_distance <-
      sea_air_road_distances %>%
      filter(country ==  import_country,
             type == likely_transport_type) %>%
      pull(distance)
    
    likely_emmission_factor <- 
      emmission_factor %>%
      filter(type == likely_transport_type) %>%
      pull(kg_co2e)
    
    co2_emmissions <- likely_distance * likely_emmission_factor
    
    return(co2_emmissions)
    }
}
# add the co2 emmissions for each country & product
trade_data <- 
  trade_data %>% 
  filter(country != "Cyprus") %>% # Have to remove Cyprus as we have no route planning data for them
  rowwise() %>%
  mutate(co2_emmissions = calculate_co2(import_country = country, commodity = description))

# add column to give the likely mode of transport
trade_data <- 
  trade_data %>%
  group_by(country, description) %>%
  arrange(desc(value)) %>%
  mutate(likely_transport_type = head(port_type,1))

# Remove the 'excluding' text from the descriptions of the products (makes it hard for user to read)

trade_data <- 
  trade_data %>%
  mutate(description = str_remove(description, "\\s*\\([^\\)]+\\)"))

write_rds(trade_data, "data/master_data/master_data_set.rds")

# Clean Emmissions Data ----

# clean up emmission factor data 
emmission_factor <- 
  emmission_factor %>%
  janitor::clean_names() %>%
  mutate(activity_type = paste0(activity, ' ', type)) %>%
  select(-c(size, activity, type)) %>%
  mutate(activity_type = recode(activity_type, 
                                "Freight flights International, to/from non-UK" = "Air",
                                "Cargo ship General cargo" = "Sea",
                                "Cargo ship RoRo-Ferry" = "Sea_EC",
                                "HGV (all diesel) All HGVs" = "Road")) %>%
  rename(type = activity_type)

write_rds(emmission_factor, "data/master_data/emmissions_factors_master.rds")

# Create Emmissions Master Data ----
eu_countries <- trade_data %>% filter(eu_non_eu == "EU") %>% distinct(country) %>% pull()

get_emmisions <- function(import_country, commodity) {
  
  # if EU country is selected then give transport type as "Road"
  if (import_country %in% eu_countries) {
    transport_type <- 
      trade_data %>%
      filter(description == commodity,
             country == import_country) %>%
      mutate(port_type = replace_na(port_type,"Road/Sea_EC"), 
             value_share = value/value_total,
             mass_share = mass/mass_total) %>%
      select(port_type, value_share, mass_share) %>%
      rename(type = port_type) %>%
      separate_rows(type, sep = "/")
    
    # get transport distance from country and transport type
    transport_distance <-
      sea_air_road_distances %>%
      filter(country == import_country,
             type %in% pull(transport_type, type)) %>%
      select(distance, type)
    
    # get transport emmission factor
    transport_emmission <-
      emmission_factor %>%
      filter(type %in% pull(transport_type, type)) %>%
      select(kg_co2e, type)
    
    # combine emmissions for Road and English Channel and remove the duplicated row
    emmissions_type <-
      tibble(transport_type) %>%
      left_join(transport_distance, by = "type") %>%
      left_join(transport_emmission, by = "type") %>%
      mutate(emmissions_per_tonne = distance*kg_co2e) %>%
      select(-c(distance, kg_co2e)) %>%
      mutate(emmissions_per_tonne = sum(emmissions_per_tonne),
             country = import_country,
             product = commodity) %>%
      mutate(type = case_when(type == "Road" ~ "Road/Sea_EC",
                              type == "Sea_EC" ~ "Road/Sea_EC")) %>%
      distinct()
    
    
    
  } else {  
    # get transport type from country and commodity
    transport_type <- 
      trade_data %>%
      filter(description == commodity,
             country == import_country) %>%
      mutate(value_share = value/value_total,
             mass_share = mass/mass_total) %>%
      select(port_type, value_share, mass_share) %>%
      rename(type = port_type)
    
    
    # get transport distance from country and transport type
    transport_distance <-
      sea_air_road_distances %>%
      filter(country == import_country,
             type %in% pull(transport_type, type)) %>%
      select(distance, type)
    
    # get transport emmission factor
    transport_emmission <-
      emmission_factor %>%
      filter(type %in% pull(transport_type, type)) %>%
      select(kg_co2e, type)
    
    
    emmissions_type <- 
      tibble(transport_type) %>%
      left_join(transport_distance, by = "type") %>%
      left_join(transport_emmission, by = "type") %>%
      mutate(emmissions_per_tonne = distance*kg_co2e,
             country = import_country,
             product = commodity) %>%
      select(-c(distance, kg_co2e))
    
  }
  
  return(emmissions_type)

}




countries <- 
  trade_data %>%
  distinct(country) %>%
  pull(country)

commodities <-
  trade_data %>%
  distinct(description) %>%
  pull(description)

get_emmisions(commodity = 'Fresh or chilled peas "Pisum sativum", shelled or unshelled')

trade_data %>%
  filter(description == 'Seed potatoes') %>%
  distinct(country)


commodity_country_emmissions <- data.frame()
i <- 0
#Run time ~ 20 mins
for (unit_commodity in commodities) {
  com_countries <- 
    trade_data %>%
    filter(description == unit_commodity) %>%
    distinct(country) %>%
    pull(country)
  
  i <- i + 1
  print(paste0(i, " of ", length(commodities)))

  for (country in com_countries) {

    df <- get_emmisions(import_country = country,
                        commodity = unit_commodity)
    
    commodity_country_emmissions <- rbind(commodity_country_emmissions, df)

  }
}

# Known Issue with Cyprus -> no road map probably because its an Island so Google will struggle
write_rds(commodity_country_emmissions, "data/master_data/emmissions_data_master.rds")


