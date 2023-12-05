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

# Load data

tx_dic <- readRDS("Data/Intermediate/TX/tx_DIC_clean.rds") %>%
  distinct(city) 

coj_tx <- readRDS("Data/Intermediate/TX/coj_tx_city.rds") %>%
  distinct(city) 

cocf_tx <- readRDS("Data/Intermediate/TX/cocf_tx_city.rds") %>%
  distinct(city) 

tx_fac <- full_join(tx_dic, coj_tx) %>%
  full_join(cocf_tx)

prism <- read_feather('Data/Intermediate/Climate data/prism/maxt/tx_max_temp.feather') %>%
  mutate(city = gsub("THE COLONY", "TENNESSEE C", city)) %>%
  filter(city %in% tx_fac$city)

rm(cocf_tx, coj_tx, tx_dic)

tx_temps <- prism %>%
  select(c("city", "max_t", "date"))

saveRDS(tx_temps, "Data/Intermediate/TX/tx_fac_city_temps.rds")

################################################################################
## Load tx DIC data
################################################################################

tx_dic <- readRDS("Data/Intermediate/TX/tx_DIC_clean.rds")

tx_dic$death_ymd <- as.Date(tx_dic$death_ymd, format="%m/%d/%Y") 
tx_dic$death_ymd <- format(tx_dic$death_ymd, format="%Y-%m-%d")


################################################################################
## Extract temp data from Tx temps 
################################################################################


tx_dic_temp <- full_join(tx_dic, tx_temps, by = join_by("city" == "city", "death_ymd" == "date"))

tx_dic_nomatch <- anti_join(tx_dic, tx_temps, by = join_by("city" == "city", "death_ymd" == "date"))

saveRDS(tx_dic_temp, "Data/Intermediate/TX/tx_DIC_max_t.rds")

################################################################################
## Count DIC per day around max temps
################################################################################

tx_temp_dic <- left_join(tx_temps, tx_dic, by = join_by("date" == "death_ymd", "city" == "city"), relationship = "many-to-many") %>%
  group_by(date, city) %>%
  mutate(death_count_total = n()) %>% 
  distinct(date, city, .keep_all = TRUE) %>%
  mutate(death_count_total = case_when(
   is.na(record_id) ~ 0,
   TRUE ~ death_count_total
  ))


