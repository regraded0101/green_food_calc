library(ggmap)
library(gmapsdistance)
library(rgdal)
library(broom)
library(rgeos)
library(tidyr)
library(plotly)
library(geosphere)
library(tibble)
library(tictoc)
library(rgdal)
library(raster)
library(gdistance)
library(readr)
library(purrr)
library(janitor)
library(dplyr)
library(stringr)
library(shiny)
library(shinydashboard)




sea_air_road_distances <- read_rds("data/geo_data/tidy_data/sea_air_road_distances.rds")
sea_air_road_paths <- read_rds("data/geo_data/tidy_data/sea_air_road_paths.rds")
trade_data <- read_rds("data/master_data/trade_data_master.rds")


emmission_factor <- read_rds("data/master_data/emmissions_factors_master.rds")
commodity_country_emmissions <- read_rds("data/master_data/emmissions_data_master.rds")

source("R/functions.R")






