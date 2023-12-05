
################################################################################
# Reading city boundaries for Texas 
# National Center for Environmental Economics
# Last edited: 10/23/25
################################################################################

# Load Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  tidyverse,
  sf, # vector data operations
  raster, # raster data operations
  exactextractr, # fast raster data extraction for polygons
  maps, # to get county boundary data
  data.table, # data wrangling
  dplyr, # data wrangling
  lubridate, # Date object handling
  tmap, # for map creation
  modelsummary, # regression table generation
  future.apply, # parallel computation
  cowplot, # for bivariate mapping
  rgdal, # required for cdlTools
  prism, # download PRISM data
  stringr, # string manipulation
  magrittr,
  tidycensus,
  mapview,
  patchwork, #for combining plots
  readxl,
  janitor,
  fuzzyjoin,
  zipcodeR
)


#set directory

mypath <- paste0("C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/Heat waves inmate mortality")

setwd(mypath)
getwd()


################################################################################
## Load Data
################################################################################

# data source: https://gis-txdot.opendata.arcgis.com/datasets/TXDOT::txdot-city-boundaries/about

st_layers("Data/Facility locations/TxDOT_City_Boundaries.gdb")

tx_city_boundaries <- st_read("Data/Facility locations/TxDOT_City_Boundaries.gdb",
                              layer = "Cities") %>%
  clean_names()

st_as_sf(tx_city_boundaries)

tx_city_boundaries <- tx_city_boundaries %>%
  rename(city = city_nm) %>%
  mutate_at("city", .funs=toupper)


ggplot(tx_city_boundaries) + geom_sf()

saveRDS(tx_city_boundaries, "Data/Facility locations/TxDOT_City_Boundaries.rds")

tx_city_boundaries <- readRDS("Data/Facility locations/TxDOT_City_Boundaries.rds") %>%
  st_as_sf()

texas_zc <- reverse_zipcode(cocf_tx_2012$zip)
