
################################################################################
# Creating City-level State and Federal Correctional Facilities in Texas 
# National Center for Environmental Economics
# Last edited: 10/31/25
################################################################################

# Load Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  tidyverse,
  grid,
  gridExtra, #for creating tables
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
  tidytab,
  epiDisplay,
  fuzzyjoin,
  zoo
)


#set directory

mypath <- paste0("C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/Heat waves inmate mortality")

setwd(mypath)
getwd()

plot_path <- "Output/"


################################################################################
## Join to BJS Census of Correctional Facilities: city, county
################################################################################

cocf_tx_2019 <- readRDS("Data/Intermediate/COCF/cocf_2019_clean.rds") %>%
  filter(state == "TX")  %>%
  mutate(city = gsub("COLONY", "C", city),
         city = gsub("CORP CHRISTI", "CORPUS CHRISTI", city),
         city = gsub("WHT SETTLEMT", "WHITE SETTLEMENT", city),
         city = gsub("SUGARLAND", "SUGAR LAND", city),
         county = case_when(city == "HUTCHINS" ~ "DALLAS",
                            city == "SUGAR LAND" ~ "FORT BEND",
                            TRUE ~ county)) %>%
  dplyr::select(c("city", "county", "state", "ad_m_pop",
                  "ad_f_pop", "tot_pop", "year")) %>%
  mutate(tot_pop = as.numeric(tot_pop),
         ad_m_pop = as.numeric(ad_m_pop),
         ad_f_pop = as.numeric(ad_f_pop))


cocf_tx_2012 <- readRDS("Data/Intermediate/COCF/cocf_2012_clean.rds")  %>%
  filter(state == "TX")  %>%
  mutate(city = gsub("COLONY", "C", city)) %>%
  mutate(city = gsub("CORP CHRISTI", "CORPUS CHRISTI", city)) %>%
  dplyr::select(c("city", "county", "state", "ad_m_pop",
                  "ad_f_pop", "tot_pop", "year")) %>%
  mutate(tot_pop = as.numeric(tot_pop),
         ad_m_pop = as.numeric(ad_m_pop),
         ad_f_pop = as.numeric(ad_f_pop))

cocf_tx_2005 <- readRDS("Data/Intermediate/COCF/cocf_2005_clean.rds") %>%
  filter(state == "TX")  %>%
  mutate(city = gsub("COLONY", "C", city),
         city = gsub("CORP CHRISTI", "CORPUS CHRISTI", city),
         county = case_when(city == "HUTCHINS" ~ "DALLAS",
                            TRUE ~ county),
         city = gsub("FT WORTH", "FORT WORTH", city)) %>%
  dplyr::select(c("city", "county", "state", "ad_m_pop",
                  "ad_f_pop", "tot_pop", "year")) %>%
  mutate(tot_pop = as.numeric(tot_pop),
         ad_m_pop = as.numeric(ad_m_pop),
         ad_f_pop = as.numeric(ad_f_pop))

cocf_tx <- bind_rows(cocf_tx_2005, cocf_tx_2012, cocf_tx_2019) %>%
  group_by(city) %>%
  fill(county, .direction = "updown") %>%
  group_by(city, county, year)  %>%
  mutate(city_pop = sum(tot_pop),
         city_pop_f = sum(ad_f_pop),
         city_pop_m = sum(ad_m_pop)) %>%
  mutate(facilities = n()) 

cocf_tx_city <- cocf_tx %>%
  distinct(city, .keep_all = TRUE) %>%
  ungroup() %>%
  group_by(city) %>%
  fill(county, .direction = "updown")

saveRDS(cocf_tx_city, "Data/Intermediate/TX/cocf_tx_city.rds")

# Now we have a data frame that has city, county by year population data

cocf_tx_city <- readRDS("Data/Intermediate/TX/cocf_tx_city.rds") %>%
  group_by(city, county) %>%
  complete(year = 2005:2019) %>%
  mutate(city_pop = na.approx(city_pop, na.rm = FALSE)) %>%
  mutate(city_pop = format(round(city_pop, 0))) %>%
  fill(state, .direction = "up")

saveRDS(cocf_tx_city, "Data/Intermediate/TX/cocf_tx_city_fill.rds")

  

