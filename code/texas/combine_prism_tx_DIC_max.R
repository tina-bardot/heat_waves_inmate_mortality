################################################################################
# Texas DIC and PRISM 
# National Center for Environmental Economics
# Last edited: 11/08/2023
################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
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
  sf,
  arrow
)

#set directory

mypath <- paste0("C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/Heat waves inmate mortality")

setwd(mypath)
getwd()

################################################################################
## Subset PRISM data to cities in TX with DIC
################################################################################

# Load data: get all cities that include deaths OR have facilities

tx_dic <- readRDS("Data/Intermediate/TX/tx_DIC_clean.rds") %>%
  distinct(city) 

# Jails

coj_tx <- readRDS("Data/Intermediate/TX/coj_tx_city.rds") %>%
  distinct(city) 

# State and federal prisons

cocf_tx <- readRDS("Data/Intermediate/TX/cocf_tx_city.rds") %>%
  distinct(city) 

# Combine to get a full city dataset

tx_fac <- full_join(tx_dic, coj_tx) %>%
  full_join(cocf_tx)

# Read PRISM temperature data

prism <- read_feather('Data/Intermediate/Climate data/prism/maxt/tx_max_temp.feather') %>%
  mutate(city = gsub("THE COLONY", "TENNESSEE C", city)) %>%
  filter(city %in% tx_fac$city) # filter to cities which exist in other datasets

rm(cocf_tx, coj_tx, tx_dic)

tx_temps <- prism %>%
  select(c("city", "max_t", "date"))

saveRDS(tx_temps, "Data/Intermediate/TX/tx_fac_city_temps.rds")

################################################################################
## Load Texas Deaths in Custody data
################################################################################

tx_dic <- readRDS("Data/Intermediate/TX/tx_DIC_clean.rds")

# Change format to match PRISM date format

tx_dic$death_ymd <- as.Date(tx_dic$death_ymd, format="%m/%d/%Y") 
tx_dic$death_ymd <- format(tx_dic$death_ymd, format="%Y-%m-%d")

################################################################################
## Count DIC per day around max temps
################################################################################

# Left join to combine the DIC data to the temperature data, count number of deaths
# per city per day

tx_temp_dic <- left_join(tx_temps, tx_dic, by = join_by("date" == "death_ymd", "city" == "city"), relationship = "many-to-many") %>%
  group_by(date, city) %>%
  mutate(death_count_total = n()) %>% 
  distinct(date, city, .keep_all = TRUE) %>%
  mutate(death_count_total = case_when(
   is.na(record_id) ~ 0,
   TRUE ~ death_count_total
  ))

saveRDS(tx_temp_dic, "Data/Intermediate/TX/tx_temp_dic.rds")


