
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

get_emmis_rank <- function(commodity, import_country) {
  
  return(
    commodity_country_emmissions %>%
      filter(product == commodity) %>%
      group_by(product) %>%
      mutate(rank = percent_rank(emmissions_per_tonne)) %>%
      arrange(desc(emmissions_per_tonne)) %>%
      filter(country == import_country) %>%
      pull(rank)
  )
  
}

get_better_emmis <- function(commodity, import_country) {
  
  subset_df <- 
    commodity_country_emmissions %>%
    filter(product == commodity) %>%
    group_by(product) %>%
    mutate(rank = percent_rank(emmissions_per_tonne)) %>%
    arrange(desc(emmissions_per_tonne)) %>%
    ungroup()
  
  rank_idx <- 
    subset_df %>%
    filter(country == import_country) %>%
    pull(rank)
  
  better_emmissions <- 
    subset_df %>%
    filter(rank < rank_idx) %>%
    arrange(rank) %>%
    select(country)
  
  return(better_emmissions)
  
}

get_worse_emmis <- function(commodity, import_country) {
  
  subset_df <- 
    commodity_country_emmissions %>%
    filter(product == commodity) %>%
    group_by(product) %>%
    mutate(rank = percent_rank(emmissions_per_tonne)) %>%
    arrange(desc(emmissions_per_tonne)) %>%
    ungroup()
  
  rank_idx <- 
    subset_df %>%
    filter(country == import_country) %>%
    pull(rank)
  
  worse_emmissions <- 
    subset_df %>%
    filter(rank > rank_idx) %>%
    arrange(desc(rank)) %>%
    select(country)
  
  return(worse_emmissions)
  
}










