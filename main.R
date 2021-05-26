sea_air_road_distances <- read_rds("data/geo_data/tidy_data/sea_air_road_distances.rds")
trade_data <- read_rds("data/master_data/master_data_set.rds")


emmission_factor <- read_csv("data/raw_data/emmissions_factors.csv")
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


eu_countries <- trade_data %>% filter(eu_non_eu == "EU") %>% distinct(country) %>% pull()

get_emmisions <- function(import_country, commodity) {
  
  # if EU country is selected then give transport type as "Road"
  if (import_country %in% eu_countries) {
    transport_type <- 
      trade_data %>%
      filter(description == commodity,
             country == import_country) %>%
      mutate(port_type = replace_na(port_type,"Road"), 
             value_share = value/value_total,
             mass_share = mass/mass_total) %>%
      select(port_type, value_share, mass_share) %>%
      rename(type = port_type)
    
    
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
    }
  
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
    mutate(emmissions_per_tonne = distance*kg_co2e) %>%
    select(-c(distance, kg_co2e))
  

  return(emmissions_type)
  
    
  
}

get_emmisions(import_country = "Australia",
              commodity = 'Fresh or chilled peas "Pisum sativum", shelled or unshelled')


