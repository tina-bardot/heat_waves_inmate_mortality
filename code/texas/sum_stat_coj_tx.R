################################################################################
# Summary statistics for TX coj 
# National Center for Environmental Economics
# Last edited: 11/14/25
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

coj_tx_city <- readRDS("Data/Intermediate/TX/coj_tx_city.rds")

coj_tx_city <- coj_tx_city %>%
  group_by(city, county) %>%
  mutate(fac_const = case_when(
    max(facilities) != min(facilities) ~ FALSE,
    TRUE ~TRUE
  ))


###############################################################################

# Summary stats of all cities with recorded facilities

sum_coj <- coj_tx_city %>%
  group_by(city, county) %>%
  summarize(N = n(),
            sd = sd(city_pop),
            max_fac = max(facilities),
            min_fac = min(facilities),
            max_pop = max(city_pop),
            min_pop = min(city_pop),
            mean_pop = mean(city_pop, na.rm = TRUE)) %>%
  mutate(diff_fac = max_fac-min_fac,
         diff_pop = max_pop-min_pop,
         diff_w = diff_pop/mean_pop)
 
# Plot it

ggplot(sum_coj, aes(N))+
  geom_histogram(stat = "count") +
  labs(title = "Number of observations of Texas Cities with Jails",
       caption = "Jails by city, represented in 2006, 2013, 2019, N = 278")

ggsave("Output/coj_TX_obs.png")

ggplot(sum_coj, aes(diff_fac))+
  geom_histogram(stat = "count") +
  labs(title = "Difference in facility observations by city",
       caption = "Jails by city, represented in 2006, 2013, 2019, N = 278")

ggplot(sum_coj, aes(mean_pop))+
  geom_histogram(bins = 45) +
  labs(title = "Distribution of average jail populations by city",
       caption = "N = 278")

ggsave("Output/coj_TX_mean_pop.png")

# Look at cities with highest average population

sub_coj_sum <- sum_coj %>%
  arrange(desc(mean_pop)) %>%
  head(10)

# Cities with the smallest max population (oddities)

sub_coj_sum <- sum_coj %>%
  arrange(max_pop) %>%
  head(10)

grid.newpage()

png("Output/jail_small_max_pop.png",
    width = 800, height = 300)
grid.draw(tableGrob(sub_coj_sum,
                    rows = rownames(sub_coj_sum)))
dev.off()

# Cities with the largest max population

sub_coj_sum <- sum_coj %>%
  arrange(desc(max_pop)) %>%
  head(10)

grid.newpage()

png("Output/tx_jail_big_max_pop.png",
    width = 800, height = 300)
grid.draw(tableGrob(sub_coj_sum,
                    rows = rownames(sub_coj_sum)))
dev.off()

# Look at the highest weighted differences between max and min pop observations

sub_coj_sum <- sum_coj %>%
  arrange(desc(diff_w)) %>%
  head(10)

# Make a table to export

grid.newpage()

grid.draw(tableGrob(sub_coj_sum,
                    rows = rownames(sub_coj_sum)))

# Save table

grid.newpage()
png("Output/summary_table_tx.png",
    width = 850, height = 300)

grid.draw(tableGrob(sub_coj_sum,
                    rows = rownames(sub_coj_sum)))
dev.off()


################################################################################
## Summary of cities with based on facilities observations across datasets 
################################################################################

# First, summary of cities with non-constant facility observations

coj_tx_sub <- coj_tx_city %>%
  filter(fac_const == FALSE)

sum_coj <- coj_tx_sub %>%
  group_by(city, county) %>%
  summarize(N = n(),
            sd = sd(city_pop),
            max_fac = max(facilities),
            min_fac = min(facilities),
            min_pop = min(city_pop),
            max_pop = max(city_pop),
            mean_pop = mean(city_pop, na.rm = TRUE)) %>%
  mutate(diff_pop = max_pop-min_pop,
         diff_w = diff_pop/mean_pop)

# Plot it: histogram of distribution

ggplot(sum_coj, aes(diff_w))+
  geom_histogram(stat = "bin", bins = 15) +
  labs(title = "Texas Correctional Facilities populations",
       caption = "Distribution of weighted difference between
max and min values for facility populations for
cities with different facility counts over time, N = 15")

## Find statistical distributions for non-constant facilities according to categories

# First, pivot wide data.frame to summarize by city

sum_coj <- coj_tx_sub %>%
  wide(v.names = c("facilities", "city_pop"),
       timevar = "year",
       id = "city") %>%
  dplyr::select(c("city", "county") | contains(c("facilities", "city_pop.")))

# Save table

grid.newpage()
png("Output/tx_jails_diff_fac_count.png",
    width = 850, height = 300)

grid.draw(tableGrob(sum_coj,
                    rows = rownames(sum_coj)))
dev.off()

# Correlation test

cor.test(coj_tx_sub$facilities, coj_tx_sub$city_pop)

# t = 4.5452, df = 42, p-value = 4.592e-05
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.3343097 0.7442154
# sample estimates:
#   cor : 0.5742007 

# Subset cities where the number of facilities is decreasing

fac_dec <- sum_coj %>%
  filter(facilities.2006 >= facilities.2013 & facilities.2013 >= facilities.2019)

# N = 1

# Now that you have a list of cities, semi-join with original dataset to perform
# summary stats

coj_tx_sub <- semi_join(coj_tx_city, fac_dec, by = c("city", "county"))

# Plot population trends

ggplot(coj_tx_sub, aes(facilities, city_pop)) +
  geom_point()

# Correlation test

cor.test(coj_tx_sub$facilities, coj_tx_sub$city_pop)

# Do the same for cities where number of facilities is increasing

fac_inc <- sum_coj %>%
  filter(facilities.2006 <= facilities.2013 & facilities.2013 <= facilities.2019)

# N = 9

coj_tx_sub <- semi_join(coj_tx_city, fac_inc, by = c("city", "county"))

ggplot(coj_tx_sub, aes(facilities, city_pop)) +
  geom_point()

cor.test(coj_tx_sub$facilities, coj_tx_sub$city_pop)

# Finally, perform test for cities where the number of facilities does not follow
# a linear pattern, i.e. where 2013 has the greatest number of facilities

fac_weird <- sum_coj %>%
  filter(facilities.2013 > facilities.2019 & facilities.2013 > facilities.2006)

coj_tx_sub <- semi_join(coj_tx_city, fac_weird, by = c("city", "county"))

ggplot(coj_tx_sub, aes(facilities, city_pop)) +
  geom_point(aes(colour = factor(year)))

cor.test(coj_tx_sub$facilities, coj_tx_sub$city_pop)

## Now, summary statistics on cities that do have constant facility observations

coj_tx_sub <- coj_tx_city %>%
  filter(fac_const == TRUE)

cor.test(coj_tx_sub$facilities, coj_tx_sub$city_pop)

# data:  coj_tx_sub$facilities and coj_tx_sub$city_pop
# t = 32.464, df = 641, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.7574067 0.8161085
# sample estimates:
#   cor 
# 0.7885471 

sum_coj <- coj_tx_sub %>%
  group_by(city, county) %>%
  summarize(N = n(),
            facilities = max(facilities),
            sd_pop = sd(city_pop),
            min_pop = min(city_pop),
            max_pop = max(city_pop),
            mean_pop = mean(city_pop, na.rm = TRUE)) %>%
  mutate(diff_pop = max_pop-min_pop,
         diff_w = diff_pop/mean_pop)

# N = 278

ggplot(sum_coj, aes(diff_w))+
  geom_histogram(stat = "bin", bins = 20) +
  labs(title = "Texas Jails populations",
       caption = "Distribution of weighted difference between
 max and min values for facility populations for
 cities with constant facility counts over time, N = 263")

ggsave("Output/coj_TX_sum_w_diff_fac_same.png")

# Look at the cities with highest weighted difference in population counts

sub_coj_sum <- sum_coj %>%
  arrange(desc(diff_w)) %>%
  head(10)

grid.draw(tableGrob(sub_coj_sum,
                    rows = rownames(sub_coj_sum)))

grid.newpage()

png("Output/tx_jail_wgt_diff.png",
    width = 800, height = 300)
grid.draw(tableGrob(sub_coj_sum,
                    rows = rownames(sub_coj_sum)))

dev.off()




