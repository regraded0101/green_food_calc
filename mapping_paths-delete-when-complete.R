sp_countries <- 
  commodity_country_emmissions %>%
  filter(product == "Seed potatoes") %>%
  select(country, type)

sea_air_road_paths %>% filter(group %in% pull(sp_countries, country),
                              type %in% pull(sp_countries, type)) -> test

sp_countries %>%
  left_join(sea_air_road_paths, 
            by = c("country" = "group", "type")) -> test

world_map %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "#181818", col = "#808080") +
  #geom_polygon(data = world_map %>% )
  theme(panel.background = element_rect(fill = "black"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
        ) 
]