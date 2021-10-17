source("dependencies.R")
 
 
 sea_air_road_distances <- read_rds("data/geo_data/tidy_data/sea_air_road_distances.rds")
 sea_air_road_paths <- read_rds("data/geo_data/tidy_data/sea_air_road_paths.rds")
 trade_data <- read_rds("data/master_data/master_data_set.rds")
 world_map <- readOGR(dsn = "data/shape_files", layer = "wb_world_map_clean") %>% broom::tidy(region = "FORMAL_EN")
 
world_map <- 
  world_map %>% 
  mutate(id = trimws(str_remove_all(id, "Kingdom of|Republic|Republic of")))
 
 
 
eu_countries <- trade_data %>% filter(eu_non_eu == "EU") %>% distinct(country) %>% pull()
 
emmission_factor <- read_rds("data/master_data/emmissions_factors_master.rds")
commodity_country_emmissions <- read_rds("data/master_data/emmissions_data_master.rds")
 
 
source("R/functions.R")
 
 
 
 
 
 
 
