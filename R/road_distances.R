# https://unece.org/traffic-census-2005
roads_shp <- readOGR("data/shape_files/ROAD/Layers", "Final_network2005")
#roads_shp <- SpatialLines
plot(roads_shp)

library(sf)
library(fasterize)
p_snap <- st_as_sf(roads_shp)

ggmap::route(from = "London, United Kingdom", to = "Birmingham, United Kingdom", mode = "driving", structure = "route") -> test

ggplot() +
  geom_polygon(world_df_plot, mapping = aes(x = long, y = lat, group = group), fill = 'lightgrey') +
  geom_path(test,mapping = aes(x = lon, y = lat),  colour = "red",
            size = 1.5, alpha = .5)

test <- read_rds('data/geo_data/tidy_data/sea_air_paths.rds')
