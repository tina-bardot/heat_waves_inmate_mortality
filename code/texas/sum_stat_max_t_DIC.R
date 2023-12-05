################################################################################
# Summary statistics for DIC and hot days in Texas
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
## Load DIC data with temperature data
################################################################################

tx_dic_temp <- readRDS("Data/Intermediate/TX/tx_DIC_max_t.rds") %>%
  mutate(month = format(as.Date(death_ymd), format="%m")) %>%
  mutate(season = case_when(
    month == "12" | month == "01" | month == "02" ~ "winter",
    month == "04" | month == "05" | month == "03" ~ "spring", 
    month == "06" | month == "07" | month == "08" ~ "summer",
    month == "09" | month == "10" | month == "11" ~ "fall"
  )) %>%
  mutate(death = case_when(
    is.na(record_id) ~ FALSE, 
    TRUE ~ TRUE
  )) %>%
  group_by(city, custody, death_ymd) %>%
  mutate(death_count = n())  

# Now we have a variable that indicates whether someone has died on a given day

# What we want to do next is to verify whether the temperature is linked to the 
# death variable being true

summary(glm(death ~ max_t, family = binomial(link = "probit"), data = tx_dic_temp))

summary(glm(death_count ~ max_t + custody, family = poisson(link = "identity"), data = tx_dic_temp), control = "month")


## What if we limit to the summer months? 

tx_dic_summer <- tx_dic_temp %>%
  filter(season == "summer")


summary(glm(death_count ~ max_t + custody, family = poisson(link = "identity"), data = tx_dic_summer), control = "month")

## Looking at heat waves specifically, identifying days when max_t > 80th percentile

tx_dic_temp %>% 
  ungroup() %>%
  summarise(percent80 = quantile(max_t, probs = .80, na.rm = TRUE))

# percent80
#   1      33.8

# Now to create a variable that identifies whether a day is a heat wave

tx_dic_temp <- tx_dic_temp %>%
  mutate(heat_wave = case_when(
    max_t > 33.8 ~ TRUE,
    TRUE ~ FALSE
  ))

summary(glm(death ~ heat_wave, family = binomial(link = "probit"), data = tx_dic_temp))

summary(glm(death_count ~ heat_wave + custody, family = poisson(link = "identity"), data = tx_dic_temp), control = "season")


tx_dic_summer <- tx_dic_temp %>%
  filter(season == "summer")

summary(glm(death_count ~ heat_wave + custody, family = poisson(link = "identity"), data = tx_dic_summer), control = c("city", "month"))


