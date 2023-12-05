
################################################################################
# Data cleaning for TX
# National Center for Environmental Economics
# Last edited: 9/6/25
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
  readxl
)


#set directory

mypath <- paste0("C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/Heat waves inmate mortality")

setwd(mypath)
getwd()

################################################################################
## Load Data
################################################################################

## Texas data

tx_DIC_raw <- read.csv("Data/Raw/State DIC/TX_custodial-deaths.csv")

tx_DIC <- tx_DIC_raw %>% 
  dplyr::select(record_id, age_at_time_of_death, sex, race, 
                death_date, death_date_and_time, death_location_city, 
                death_location_county, death_from_pre_existing_medical_condition, 
                manner_of_death, medical_cause_of_death, type_of_custody, contains("agency_")) %>%
  rename(age = age_at_time_of_death) %>%
  rename(death_ymd = death_date) %>%
  filter(age > 12)
  
tx_DIC$death_ymd <- format(as.Date(tx_DIC$death_ymd, format = '%m/%d/%Y'), format = '%m/%d/%Y')
tx_DIC$death_md <- format(as.Date(tx_DIC$death_ymd, format = '%m/%d/%Y'), format = '%m/%d')
tx_DIC$death_m <- format(as.Date(tx_DIC$death_ymd,format='%m/%d/%Y'), format = '%m') 
tx_DIC$death_y <- format(as.Date(tx_DIC$death_ymd,format='%m/%d/%Y'), format = '%y') 

# summary(tx_DIC)

tx_DIC <- tx_DIC %>%
  rename(city = death_location_city) %>%
  rename(county = death_location_county) %>%
  rename(custody = type_of_custody) %>%
  mutate(custody = ifelse(custody == 'JAIL - COUNTY' | custody == "JAIL - MUNICIPAL", 
                          "JAIL", custody)) %>%
  filter(custody != "POLICE CUSTODY (PRE-BOOKING)") %>% ## Filter out observations associated with pre-booking
  dplyr::select(-contains("agency_")) %>%
  mutate(city = gsub("COLONY", "C", city)) %>% # Fixing all of the misspellings in the dataset
  mutate(city = gsub(", TEXAS", "", city)) %>%
  mutate(city = gsub(", TX.", "", city)) %>%
  mutate(city = gsub(", TX", "", city)) %>%
  mutate(city = gsub(" TX", "", city)) %>%
  mutate(city = gsub(", LA", "", city)) %>%
#  mutate(city = gsub(".", "", city)) %>%
  mutate(city = gsub("[[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]]", "", city)) %>%
  mutate(city = case_when(
    city == "TENNESSEE" | city == "TENNESSE C" | city == "TENNESSEE CQ" | city == "TENESSEE C" | 
    city == "TENENSSEE C" | city == "TENNEESEE C" | city == "TENNESSEE C OLONY" | city == "TENNESSEE COLON" | 
      city == "TENNESSEE COLONG"  | city == "TENNESSEE CPLONY" | city == "TENNESSES C" | city == "TENNESSESS C"
    ~ "TENNESSEE C",
    city == "DICKENSON" ~ "DICKINSON", 
    city == "HUNSTVILLE" | city == "HUSTVILLE" | city == "HUTNSVILLE" | city == "HUTSVILLE" ~ "HUNTSVILLE", 
    city == "GLAVESTON" | city == "GAVLESTON" | city == "GALVES" ~ "GALVESTON",
    city == "DALLS" ~ "DALLAS",
    city == "FT. WORTH" ~ "FORT WORTH", 
    city == "FT. STOCKTON" ~ "FORT STOCKTON",
    city == "GATEVILLE" | city == "GATESVILLED" ~ "GATESVILLE", 
    city == "AMARLLO" | city == "AMARILLA" | city == "AMARILO" ~ "AMARILLO", 
    city == "ARANSAS PASS" ~ "ARKANSAS PASS",
    city == "BEAMONT" | city == "BEAUMONTH" | city == "BEAUMON" ~ "BEAUMONT", 
    city == "BEEVILE" | city == "BEEVILLE-8982" | city == "BEEVILLE TEXAS" | city == "BEEWILLE" ~ "BEEVILLE",
    city == "COLORADO" ~ "COLORADO CITY",
    city == "CUERRO" | city == "CURERO" ~ "CUERO", 
    city == "EDINBERG" | city == "EDINBURGH" ~ "EDINBERG",
    city == "GROESBUCK" ~ "GROESBECK",
    city == "KENNEDY" ~ "KENEDY",
    city == "LAGRANGE" ~ "LA GRANGE",
    city == "LUBBCOK" | city == "LUBBOC" ~ "LUBBOCK",
    city == "MT. PLEASANT" ~ "MOUNT PLEASANT",
    city == "ODESS" ~ "ODESSA",
    city == "PALASTINE" | city == "PA;ESTINE" ~ "PALESTINE",
    city == "RAMONDVILLE" ~ "RAYMONDVILLE",
    city == "RICHMON" | city == "RICHMONED" ~ "RICHMOND",
    city == "RIO GRNADE CITY"~ "RIO GRANDE CITY",
    city == "ROSESHARON" ~ "ROSHARON",
    city == "SUGARLAND" ~ "SUGAR LAND",
    city == "TEXAS" ~ "TEXAS CITY",
    city == "WAXAHAXHIE" ~ "WAXAHACHIE",
    city == "WICHITA" ~ "WICHITA FALLS",
    TRUE ~ city
  ))  %>%
  group_by(city) %>%
  mutate(county = na_if(county, "")) %>%
  tidyr::fill(county) %>%
  mutate(county = case_when(
    county == "VAN_ZANDT" ~ "VAN ZANDT",
    county == "TOM_GREEN" ~ "TOM GREEN",
    county == "LIVE_OAK" ~ "LIVE OAK",
    county == "DE WITT" ~ "DEWITT",
    county == "DALLAM" ~ "DALLAS",
    city == "BROWNFIELD"  ~ "TERRY",
    city == "CUERO"  ~ "DEWITT",
    city == "DALHART" ~ "HARTLEY",
    city == "BRIDGEPORT" ~ "WISE",
    city == "BONHAM" ~ "FANNIN",
    city == "OVERTON"  ~ "RUSK",
    city == "BARTLETT"  ~ "WILLIAMSON",
    city == "SAN DIEGO"  ~ "DUVAL",
    city == "MINERAL WELLS" ~ "PALO PINTO",
    TRUE ~ county
  ))  %>%
  ungroup() %>%
  mutate_all(trimws) 


## Now perform some mutations

tx_DIC <- tx_DIC %>%
  group_by(city) %>%
  mutate(city_DIC = n())%>%
  ungroup() %>%
  group_by(city, death_y) %>%
  mutate(city_year_DIC = n()) %>%
  ungroup() %>%
  mutate(age = as.numeric(age)) %>%
  mutate(race = case_when(
    race == "" ~ NA, 
    TRUE ~ race
  ))  %>%
  mutate(age_group = case_when( #creating age groups
    age < 18 ~ "<18",
    age >= 18 & age <= 24 ~ "18-24",
    age >=25 & age <=30 ~ "25-30",
    age >=31 & age <=35 ~ "31-35",
    age >=36 & age <=40 ~ "36-40",
    age >=41 & age <=45 ~ "41-45",
    age >=46 & age <=50 ~ "46-50",
    age >=51 & age <=55 ~ "51-55",
    age >=56 & age <=60 ~ "56-60",
    age >=61 & age <=65 ~ "61-65",
    age >=66 & age <=70 ~ "66-70",
    age >=71 & age <=75 ~ "71-75",
    age >=76 ~ "76+"
  ))


saveRDS(tx_DIC, file = "Data/Intermediate/TX/tx_DIC_clean.rds") # save data for future use

## Data summary and visualization

death_md_his <- ggplot(tx_DIC, aes(x = death_m)) + geom_histogram(stat = "count")

ggplot(tx_DIC,aes(x=death_m, group = county, fill=death_location_county))+
  geom_histogram(stat = "count") +
  theme_bw()

ggsave("Output/tx_DIC_mo_his.png")


ggplot(tx_DIC, aes(x = age)) + geom_histogram(stat = "count")

ggsave("Output/tx_DIC_age_his.png")


tx_dic_cities <- distinct(tx_DIC, city)

 

