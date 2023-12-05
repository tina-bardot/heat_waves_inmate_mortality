################################################################################
# Summary statistics for TX COCF 
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
  furniture
)


#set directory

mypath <- paste0("C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/Heat waves inmate mortality")

setwd(mypath)
getwd()

###############################################################################
# Load data : Texas city-level jail populations data
###############################################################################

cocf_tx_city <- readRDS("cocf_tx_city.rds")


cocf_tx_city <- cocf_tx_city %>%
  group_by(city, county) %>%
  mutate(fac_const = case_when(
    max(facilities) != min(facilities) ~ FALSE,
    TRUE ~TRUE
  ))


###############################################################################

# Summary stats of all cities with recorded facilities

sum_cocf <- cocf_tx_city %>%
  group_by(city, county) %>%
  summarize(N = n(),
            sd = sd(city_pop),
            max_fac = max(facilities),
            min_fac = min(facilities),
            max_pop = max(city_pop),
            min_pop = min(city_pop),
            mean_pop = mean(city_pop, na.rm = TRUE)) %>%
  mutate(diff_pop = max_pop-min_pop,
         diff_w = diff_pop/mean_pop)


# Plot it

ggplot(sum_cocf, aes(N))+
  geom_histogram(stat = "count")

ggsave("Output/COCF_TX_obs.png")

ggplot(sum_cocf, aes(mean_pop))+
  geom_histogram()

ggsave("Output/COCF_TX_mean_pop.png")


# Look at cities with highest average population

sub_cocf_sum <- sum_cocf %>%
  arrange(desc(mean_pop)) %>%
  head(10)

# Look at the highest weighted differences between max and min pop observations

sub_cocf_sum <- sum_cocf %>%
  arrange(desc(diff_w)) %>%
  head(10)

# Make a table to export

grid.newpage()

grid.draw(tableGrob(sub_cocf_sum,
                    rows = rownames(sub_cocf_sum)))

# Save table

grid.newpage()
png("Output/summary_table_tx.png",
    width = 600, height = 300)
grid.draw(tableGrob(sub_cocf_sum,
                    rows = rownames(sub_cocf_sum)))
dev.off()


################################################################################
## Summary of cities with based on facilities observations across datasets 
################################################################################

# First, summary of cities with non-constant facility observations

cocf_tx_sub <- cocf_tx_city %>%
   filter(fac_const == FALSE)
 
sum_cocf <- cocf_tx_sub %>%
   group_by(city, county) %>%
   summarize(N = n(),
             sd = sd(city_pop),
             min_pop = min(city_pop),
             max_fac = max(facilities),
             min_fac = min(facilities),
             max_pop = max(city_pop),
             mean_pop = mean(city_pop, na.rm = TRUE)) %>%
   mutate(diff_pop = max_pop-min_pop,
          diff_w = diff_pop/mean_pop)

 # Plot it: histogram of distribution
 
 
 ggplot(sum_cocf, aes(diff_w))+
   geom_histogram(stat = "bin", bins = 20) +
   labs(title = "Texas Correctional Facilities populations",
        caption = "Distribution of weighted difference between
max and min values for facility populations for
cities with different facility counts over time, N = 86")
 

 ## Find statistical distributions for non-constant facilities according to categories
 
 # First, pivot wide data.frame to summarize by city
 
 sum_cocf <- cocf_tx_sub %>%
   wide(v.names = c("facilities", "city_pop"),
        timevar = "year",
        id = "city") %>%
   dplyr::select(c("city", "county") | contains(c("facilities", "city_pop.")))
 
 # Subset cities where the number of facilities is decreasing
 
 fac_dec <- sum_cocf %>%
   filter(facilities.2005 >= facilities.2012 & facilities.2012 >= facilities.2019)
 
 # Now that you have a list of cities, semi-join with original dataset to perform
 # summary stats
 
 cocf_tx_sub <- semi_join(cocf_tx_city, fac_dec, by = c("city", "county"))
 
 # Plot population trends

 ggplot(cocf_tx_sub, aes(facilities, city_pop)) +
   geom_point()
 
 # Correlation test
 
 cor.test(cocf_tx_sub$facilities, cocf_tx_sub$city_pop)
 
 # Do the same for cities where number of facilities is increasing

 fac_inc <- sum_cocf %>%
   filter(facilities.2005 <= facilities.2012 & facilities.2012 <= facilities.2019)
 
 cocf_tx_sub <- semi_join(cocf_tx_city, fac_inc, by = c("city", "county"))
 
 ggplot(cocf_tx_sub, aes(facilities, city_pop)) +
   geom_point()
 
 cor.test(cocf_tx_sub$facilities, cocf_tx_sub$city_pop)
 
 # Finally, perform test for cities where the number of facilities does not follow
 # a linear pattern, i.e. where 2012 has the greatest number of facilities
 
 fac_weird <- sum_cocf %>%
   filter(facilities.2012 > facilities.2019 & facilities.2012 > facilities.2005)
 
 cocf_tx_sub <- semi_join(cocf_tx_city, fac_weird, by = c("city", "county"))
 
 ggplot(cocf_tx_sub, aes(facilities, city_pop)) +
   geom_point(aes(colour = factor(year)))
 
 cor.test(cocf_tx_sub$facilities, cocf_tx_sub$city_pop)
 
 ## Now, summary statistics on cities that do have constant facility observations
 
 cocf_tx_sub <- cocf_tx_city %>%
   filter(fac_const == TRUE)
 
 cor.test(cocf_tx_sub$facilities, cocf_tx_sub$city_pop)
 
 sum_cocf <- cocf_tx_city %>%
   group_by(city, county) %>%
   summarize(N = n(),
             facilities = max(facilities),
             sd_pop = sd(city_pop),
             min_pop = min(city_pop),
             max_pop = max(city_pop),
             mean_pop = mean(city_pop, na.rm = TRUE)) %>%
   mutate(diff_pop = max_pop-min_pop,
          diff_w = diff_pop/mean_pop)

 ggplot(sum_cocf, aes(diff_w))+
   geom_histogram(stat = "bin", bins = 20) +
   labs(title = "Texas Correctional Facilities populations",
        caption = "Distribution of weighted difference between
 max and min values for facility populations for
 cities with constant facility counts over time, N = 194")
 
 ggsave("Output/COCF_TX_sum_w_diff_fac_same.png")

 # Look at the cities with highest weighted difference in population counts
 
 sub_cocf_sum <- sum_cocf %>%
   arrange(desc(diff_w)) %>%
   head(10)
 
 grid.draw(tableGrob(sub_cocf_sum,
                     rows = rownames(sub_cocf_sum)))
 
 png("Output/summary_table_tx.png",
     width = 800, height = 300)
 grid.draw(tableGrob(sub_cocf_sum,
                     rows = rownames(sub_cocf_sum)))
