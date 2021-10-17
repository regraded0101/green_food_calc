# set up google API key - Step written in console to avoid writing in script (for privacy)

# create port location master ----
# DO NOT RUN - Linked to Google's API and can charge to run a lot
#register_google()
# 
# location_data <- 
#    trade_data %>%
#    distinct(country, eu_non_eu) %>%
#    bind_rows(port_type %>%
#                distinct(port) %>% 
#                mutate(port = paste0(port, ', United Kingdom')) %>%
#                rename(country = port)) %>%
#    mutate_geocode(country)
# 
# 
# location_data <- 
#   location_data %>%
#   rows_update(tibble(country = c("Thamesport, United Kingdom",
#                                  "Occ Palest Terr",
#                                  "Furnace, United Kingdom",
#                                  "Bowling, United Kingdom",
#                                  "All Other Airports, United Kingdom",
#                                  "Pembroke Epu, United Kingdom",
#                                  "Southend (Corporation Jetty), United Kingdom",
#                                  "Peel, United Kingdom",
#                                  "Georgia",
#                                  "Turks & Caicos",
#                                  "Jordan",
#                                  "Br Virgin Is"),
#                      lat = c(51.431285,
#                              31.9474,
#                              51.509865,
#                              53.7812,
#                              51.509865,
#                              51.6746,
#                              51.530133,
#                              54.2225,
#                              42.3154,
#                              21.6940,
#                              30.5852,
#                              18.4335),
#                      lon = c(0.68690500,
#                              35.2272,
#                              -0.118092,
#                              1.7373,
#                              -0.118092,
#                              -4.9129,
#                              0.730685,
#                              -4.6985,
#                              43.3569,
#                              71.7979,
#                              36.2384,
#                              64.6333)),
#               
#               by = "country")
# 
# 
# write_csv(location_data, 'data/geo_data/country_and_port_locations.csv')

# alter location data where Google's Api could not find them or they are 
# obviously incorrect e.g.in USA for UK ports. For unknown uk ports, the lat/long have
# been replaced with London's. These ports include: Furnace; All Other Airports
location_data <- read_csv('data/geo_data/country_and_port_locations.csv')

# Air Distances----
port_type <- read_csv("data/raw_data/port_data.csv") %>% clean_names()

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



# create airports data
airports_data <-
  tibble(x = "UK") %>%  # create row with "UK"
  add_column(!!!set_names(location_data %>% filter(!uk_port) %>% pull(country) %>% as.list())) %>% # create column names with all non-UK ports
  mutate(across(!matches("^x$"), function(x){return(NA)})) %>% # turn every column, except the first, into NA
  pivot_longer(cols = matches("[^^x$]")) %>% # turn from wide to long df
  select(-value) %>% # remove NA column 'value'
  rename(uk_port = x,
         intl_port = name)



# bring in all ports lat/long data
airports_data  <- 
  airports_data %>%
  mutate(lon = -1.309042,
         lat =  52.421042)%>%
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

#write_rds( airports_data, "data/geo_data/tidy_data/air_distance.rds")





# Sea Distances----

# shapefile created in QGIS with panama canal and suez canal removed
#world_shp <- rgdal::readOGR(dsn = "app/data/shape_files", layer = "world_map_suez_pan_v7")
world_df <- broom::tidy(world_shp)


#Create an empty raster
new = raster(ncol=360*3, nrow= 180*3)
projection(new) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


r <- rasterize(world_shp, new)


#Replace value with 1, 99999 to the point where ship can go and cannot
values(r)[is.na(values(r))] <- 1
values(r)[values(r)>1] <- 999999
plot(r)

#Prepare transition object
p <- transition(r, function(x){1/mean(x)}, 8)
p <- geoCorrection(p)



#Self defined distance calculator, iput two pairs of longitude and latitude to get the shortest distance
DistanceCalculator <- function(port1, port2){
  path <- shortestPath(x = p, origin = port1, goal = port2, output = "SpatialLines")
  #plot(r)
  #lines(path)
  return(path)
  #return(SpatialLinesLengths(path ,longlat=TRUE)*0.539957)
}

non_uk_eu_ports <-
  location_data %>% 
  filter((!uk_port) & (eu_non_eu != "EU")) %>% 
  select(lon, lat) %>%
  as.matrix()


tic()
ports_ls <- vector("list", nrow(non_uk_eu_ports)) # speed up for loop by setting list length before loop
#port_df <- data.frame(port_uk = character(), port_intl = character(), distance = numeric(), stringsAsFactors = FALSE)
port_df <- as.data.frame(matrix(ncol = 3, nrow = nrow(non_uk_eu_ports)))
colnames(port_df) <- c("port_uk", "port_intl", "distance")


for (j in 1:nrow(non_uk_eu_ports)) {
  ports_ls[[j]]  <- DistanceCalculator(non_uk_eu_ports[j,], c(-1.309042, 52.421042)) # set fixed UK lat/long
  port_df[[j, "port_uk"]] <- "UK"
  port_df[[j, "port_intl"]] <- location_data[(!location_data$uk_port) & (location_data$eu_non_eu != "EU"),]$country[j]
  port_df[[j, "distance"]] <- SpatialLinesLengths(ports_ls[[j]], longlat = TRUE)
}
toc('Run Time: ')

#write_rds(port_df,"data/geo_data/tidy_data/sea_distance.rds")


# Air Paths----

airports_path_ls <- vector("list", nrow(airports_data))
# create points for flight paths
for (i in 1:nrow(airports_data)) {
  
  UK_coord <- airports_data[i, c("lon_x", "lat_x")]
  intl_coord <- airports_data[i, c("lon_y", "lat_y")]
  airports_path_ls[[i]] <- mutate(as.data.frame(gcIntermediate(UK_coord, intl_coord)), group = as.character(airports_data[i, 'intl_port']))
  
}

airports_path_df <- do.call(rbind, airports_path_ls)

# Sea Paths----
# create blank dataframe
port_lines <- data.frame(x = numeric(), y = numeric())

get_port_lines <- function(i) {
  
  out <- as.data.frame(coordinates(ports_ls[[i]])) # get coordinates of the SpatialLines
  out$group <- as.character(port_df[i, 'port_intl'])
  colnames(out) <- c("lon", "lat", "group")
  
  return(out)
}

# iterate over all SpatialLines
port_lines_ls <- list()
for (l in 1:length(ports_ls)) {
  
  port_lines_ls[[l]] <- get_port_lines(l)
  
  
}
# Combine into dataframe
port_lines_df <- do.call(rbind, port_lines_ls)


#Together Paths----
sea_air_paths <- 
  rbind(mutate(airports_path_df, type = "Air"),
        mutate(port_lines_df, type = "Sea"))
# Create conversion table from group number to country name
#paths_group_conver_tbl <- as.data.frame(cbind(airports_data[['intl_port']], seq(1,170)))
#colnames(paths_group_conver_tbl) <- c("country", "group")
#write_rds(paths_group_conver_tbl, "data/geo_data/tidy_data/paths_group_convert_tbl.rds")
#sea_air_paths <- 
#  sea_air_paths %>%
#  left_join(paths_group_conver_tbl, by = "group")

#Road Paths----
# Get paths and distances from Google's API
# Create paths from country to Calais, France -> lorries will probably go to somewhere close to this
# and catch a ferry to UK. Will also include a short ferry hop
eu_ports <- 
  location_data %>%
  filter((!uk_port) & (eu_non_eu == "EU") & (country != "Cyprus")) %>% # remove Cyprus as the route does not exist

  select(country) 

road_path_ls <- vector("list", nrow(eu_ports))
road_distances <- data.frame(distance = numeric(), country = character(), stringsAsFactors=FALSE)
for (k in 1:nrow(eu_ports)) {
  
  country_name <- eu_ports[[k,1]]
  
  # direct routes from Ireland to Belfast
  if (country_name == "Irish Republic") {
    
    route <- 
      ggmap::route(from = as.character(country_name), 
                   to = "Belfast, United Kingdom", 
                   mode = "driving", 
                   structure = "route")
    
    
  } else {
  # create path from EU country to Calais, France
    route <- 
      ggmap::route(from = as.character(country_name), 
                   to = "Calais, France", 
                   mode = "driving", 
                   structure = "route")
  }
  
  # add name of port
  route <- mutate(route, country = country_name)
  
  # calculate the distance of the route
  road_dist <- sum(route$km, na.rm = TRUE)
  # add to dataframe
  road_distances[[k, 'distance']] <- road_dist
  road_distances[[k, 'country']] <- country_name
      
  # add to path road list
  road_path_ls[[k]] <- route

}

# turn into a dataframe
road_path_df <- do.call(rbind, road_path_ls)

# alter and add columns to map to sea_air_paths
road_path_df <-
  road_path_df %>% 
  ungroup() %>%
  mutate(type = "Road") %>%
  rename(group= country) %>%
  select(lon, lat, group, type) 

# add the crossing from Calais to Dover for each country
road_path_df <- 
  road_path_df %>% 
  group_by(group) %>% 
  do({ road_path_df <- . 
  last_row           <- road_path_df %>% slice(n())
  last_row$lat  <- 51.1279
  last_row$lon <- 1.3134
  last_row$type <- "Sea_EC"
  road_path_df  <- bind_rows(road_path_df,last_row)
  }) %>%
  ungroup()



# Together ----
sea_air_road_paths <- sea_air_paths %>%
  rbind(road_path_df) %>%
  mutate(type = recode(type, 
                       "Road" = "Road/Sea_EC",
                       "Sea_EC" = "Road/Sea_EC")) 


write_rds(sea_air_paths, "data/geo_data/tidy_data/sea_air_paths.rds")
write_rds(sea_air_road_paths, "data/geo_data/tidy_data/sea_air_road_paths.rds")


sea_air_road_paths <- read_rds("data/geo_data/tidy_data/sea_air_road_paths.rds")
# Together Distances----
## Combine the air, sea and road distances into 1 dataframe
## Step 1. Normalise the distances -> airports_data in m, port_df in km, road_distances in km
airports_data <- airports_data %>% mutate(distance = dist/1000)

## Step 2. Normalise the number of columns and their names
airports_data <- 
  airports_data %>% 
  select(intl_port, dist) %>%
  mutate(type = "Air") %>%
  rename(distance = dist,
         country = intl_port)

port_df <- 
  port_df %>%
  select(port_intl, distance) %>%
  mutate(type = "Sea") %>%
  rename(country = port_intl)

road_distances <- 
  road_distances %>% 
  select(country, distance) %>%
  mutate(type = "Road")

### add in  the distance from Calais to Dover (from distHaversine formula)
ec_distances <- tibble(country = road_distances$country,
                       distance = distHaversine(c(1.8587,50.9513), c(1.3134, 51.1279)) /1000,
                       type = "Sea_EC")

## Step 3. Combine into a single dataframe

sea_air_road_distances <- 
  bind_rows(airports_data,
            port_df,
            road_distances,
            ec_distances)

## Step 4. Write to .RDS
write_rds(sea_air_road_distances, "data/geo_data/tidy_data/sea_air_road_distances.rds")

