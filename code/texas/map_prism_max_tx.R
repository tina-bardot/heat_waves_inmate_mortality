################################################################################
# Plot PRISM data
# National Center for Environmental Economics
# Last edited: 11/08/2023
################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  arrow,
  tidyverse,
  data.table, # data wrangling
  dplyr, # data wrangling
  lubridate, # Date object handling
  prism, # download PRISM data
  stringr, # string manipulation
  patchwork, #for combining plots
  readxl,
  janitor,
  zipcodeR,
  sf
)

#set directory

mypath <- paste0("C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/Heat waves inmate mortality")

setwd(mypath)
getwd()


################################################################################
## Load PRISM data : Daily temp data
################################################################################

# Choose a day : July 23, 2018

hot_day <- read_feather('Data/Raw/Climate data/prism/maxt/2018/maxt_2018-07-23.feather') %>%
  dplyr::select(c("city", "max_t"))

# Load city data

texas_cities <- readRDS("Data/Intermediate/TX/TxDOT_City_Boundaries.rds") %>% 
  st_as_sf()

hot_day <- left_join(texas_cities, hot_day, by = "city")

tx_boundaries <- tigris::block_groups("TX") %>%
  mutate(ID=str_pad(GEOID, 12, pad="0")) %>%
  st_transform(crs = 4326)

tx_boundaries <- tigris::counties("TX") %>%
  st_transform(crs = 4326)

lcr_vio_CA <- left_join(CA_bg, lcr_vio_CA)

st_as_sf(lcr_vio_CA) 

# Plot it 

ggplot() +
  geom_sf(data = hot_day, aes(fill = max_t, geometry = SHAPE), color = NA) +
  scale_fill_distiller(name = "max temp", palette = "YlOrRd",
                       direction = 1) +
  geom_sf(data = tx_boundaries, fill = NA, color = scales::alpha("black", alpha = 0.3)) +
  coord_sf() +
   theme_void() 

ggsave("Output/maps/tx_072318_heat.png")

  # theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
  #       legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
  #       legend.box.background = element_blank()) 
  # 

prism <- read_feather('Data/Intermediate/Climate data/prism/maxt/tx_max_temp.feather')

