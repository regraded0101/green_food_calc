# set up google API key - Step written in console to avoid writing in script (for privacy)
library(ggmap)
library(rgdal)
library(broom)
library(rgeos)
library(tidyr)
library(plotly)
library(geosphere)
library(tibble)
# create port location master ----
# DO NOT RUN - Linked to Google's API and can charge to run a lot
#location_data <- 
#  trade_data %>%
#  distinct(country) %>%
#  bind_rows(port_type %>%
#              distinct(port) %>% 
#              mutate(port = paste0(port, ', United Kingdom')) %>%
#              rename(country = port)) %>%
#  mutate_geocode(country)

#write_csv(location_data, 'data/geo_data/country_and_port_locations.csv')

# alter location data where Google's Api could not find them or they are 
# obviously incorrect e.g.in USA for UK ports. For unknown uk ports, the lat/long have
# been replaced with London's. These ports include: Furnace; All Other Airports
location_data <- read_csv('data/geo_data/country_and_port_locations.csv')

location_data <- 
  location_data %>%
  rows_update(tibble(country = c("Thamesport, United Kingdom",
                                 "Occ Palest Terr",
                                 "Furnace, United Kingdom",
                                 "Bowling, United Kingdom",
                                 "All Other Airports, United Kingdom",
                                 "Pembroke Epu, United Kingdom",
                                 "Southend (Corporation Jetty), United Kingdom",
                                 "Peel, United Kingdom",
                                 "Georgia",
                                 "Turks & Caicos",
                                 "Jordan",
                                 "Br Virgin Is"),
                     lat = c(51.431285,
                             31.9474,
                             51.509865,
                             53.7812,
                             51.509865,
                             51.6746,
                             51.530133,
                             54.2225,
                             42.3154,
                             21.6940,
                             30.5852,
                             18.4335),
                     lon = c(0.68690500,
                             35.2272,
                             -0.118092,
                             1.7373,
                             -0.118092,
                             -4.9129,
                             0.730685,
                             -4.6985,
                             43.3569,
                             71.7979,
                             36.2384,
                             64.6333)),
              
                     by = "country")

port_type_join <- 
  port_type %>%
  mutate(port = paste0(port, ", United Kingdom")) %>%
  select(port, port_type)


# add column to flag UK ports and type of UK port
location_data <-
  location_data %>%
  mutate(uk_port = case_when(str_detect(country, 'United Kingdom$') ~ TRUE,
                             TRUE ~ FALSE)) %>%
  left_join(port_type_join, 
            by = c("country" = "port"))





# create airports data ----
airports_data <-
  tibble(x = location_data %>% filter(uk_port & port_type == 'Air') %>% pull(country)) %>%  # create row names with UK Airports
  add_column(!!!set_names(location_data %>% filter(!uk_port) %>% pull(country) %>% as.list())) %>% # create column names with all non-UK ports
  mutate(across(!matches("^x$"), function(x){return(NA)})) %>% # turn every column, except the first, into NA
  pivot_longer(cols = matches("[^^x$]")) %>% # turn from wide to long df
  select(-value) %>% # remove NA column 'value'
  rename(uk_port = x,
         intl_port = name)

# bring in all ports lat/long data
airports_data  <- 
  airports_data %>%
  left_join(location_data %>% filter(uk_port) %>% select(-c(uk_port, port_type)), 
            by = c("uk_port" = "country")) %>%
  rename(lon_x = lon,
         lat_x = lat) %>%
  left_join(location_data %>% filter(!uk_port) %>% select(-c(uk_port, port_type)), 
            by = c("intl_port" = "country")) %>%
  rename(lon_y = lon,
         lat_y = lat)

# calculate Haversine Great Circle Distance
airports_data <- 
  airports_data  %>%
  rowwise() %>%
  mutate(dist = distHaversine(cbind(lon_x, lat_x), cbind(lon_y, lat_y)))





















# shapefile created in QGIS with panama canal and suez canal removed
world_shp <- rgdal::readOGR(dsn = "data/shape_files", layer = "world_map_suez_pan")
world_df <- broom::tidy(world_shp)

ggplot() +
  geom_polygon(data = world_df, aes(x = long, y = lat, group = group), fill = 'lightgrey', col = 'black') +
  geom_point(data = location_data, aes(x = lon, y = lat, text = country))

ggplotly(tooltip = 'text')


uk_ports <- 
  location_data %>%
  slice(144:nrow(location_data))

uk_shp <- rgdal::readOGR(dsn = "data/shape_files", layer = "uk_countries")
uk_df <- broom::tidy(uk_shp)
uk_df$long <- uk_df$long/1e5
uk_df$lat <- uk_df$lat/1e4

ggplot() +
  geom_polygon(data = uk_df, aes(x = long, y = lat, group = group), fill = 'white', alpha = 0.4) +
  geom_point(data = uk_ports, aes(x = lon, y = lat), col = 'red')

ggplotly()
