###############################################################################
###########################    Get PRISM Weather    ###########################
###############################################################################

## install packages if they are not on the machine
## examples and info available here: https://docs.ropensci.org/prism/
# library("devtools")
# devtools::install_github("ropensci/prism")
# devtools::install_github("UrbanInstitute/urbnmapr")

##########################
#################  library
##########################

mypath <- paste0("C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/Heat waves inmate mortality/Data")

setwd(mypath)
getwd()

## clear worksace
rm(list = ls())
gc()


## this function will check if a package is installed, and if not, install it
# list.of.packages <- c('magrittr','tidyverse','dplyr','data.table','lubridate',
#                       'arrow',
#                       "prism",
#                       'sf', 'urbnmapr',
#                       'raster', 'exactextractr',
#                       'archive')
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
# lapply(list.of.packages, library, character.only = TRUE)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  magrittr,
  tidyverse,
  dplyr,
  data.table,
  lubridate,
  arrow,
  prism,
  sf, 
  urbnmapr,
  raster, 
  exactextractr,
  archive
)

##########################
###################  parts
##########################

options(timeout = max(1000, getOption("timeout")))

## open a prism raster and reproject so that the huc12s are already in the correct projection
# if (!dir.exists('store/prism_temporary')) {dir.create('store/prism_temporary/', recursive = T)}
# options(prism.path = "store/prism_temporary")
# get_prism_dailys(type = "ppt", minDate = "1999-12-31", maxDate = "1999-12-31", keepZip = F)
# dir    = 'store/prism_temporary'
# subdir = list.files(dir)
# path   = list.files(dir, full.names = T)
# raster = raster(paste(path, "/", subdir, ".bil", sep = ""))
# polys =
#   readRDS('Facility locations/TxDOT_City_Boundaries.rds') %>%
#   st_as_sf() %>%
#   st_transform(st_crs(raster))
# unlink(dir, recursive = T)

## export to skip computation time above
polys %>% saveRDS('Climate data/store/TxDOT_City_Boundaries_with_PRISM.rds')

## check projections
# plot(polys)
# plot(raster, add = T)

## read from above
polys = 
  readRDS('Data/Raw/Climate data/store/TxDOT_City_Boundaries_with_PRISM.rds') # %>% 
 # dplyr::select(huc_12, geometry)

##########################
#################  execute
##########################

for (DATE in as.character(seq(ymd("2010-01-31"), ymd("2010-12-31"), "days"))){
  
  # ## test date parameter
  # DATE = "1999-12-02"
  
  ## create temporary directory if it doesn't yet exist
  if (!dir.exists(paste0('Climate data/store/prism_temporary/', DATE))) {dir.create(paste0('Climate data/store/prism_temporary/', DATE), recursive = T)}
  dir = paste0('Climate data/store/prism_temporary/', DATE)
  
  ## set destination for PRISM raw .bil files
  options(prism.path = dir) 
  get_prism_dailys(type = "tmax", minDate = DATE, maxDate = DATE, keepZip = F) 
  
  ## paths
  subdir = list.files(dir)
  path   = list.files(dir, full.names = T)
  file   = paste(path, "/", subdir, ".bil", sep = "")
  
  ## create directory if it doesn't yet exist
  if (!dir.exists(paste0('Climate data/prism/maxt/', year(DATE)))) {dir.create(paste0('Climate data/prism/maxt/', year(DATE)), recursive = T)}
  
  ## extract
  polys %>% 
    mutate(max_t = exact_extract(raster(file), polys, 'mean')) %>% 
    st_drop_geometry() %>% 
    write_feather(paste0('Climate data/prism/maxt/', year(DATE), '/maxt_', DATE, '.feather'))
  
  ## delete prism file directory
  unlink(dir, recursive = T)
  
  ## clean house
  gc()
}

## Now, I want to create a dataset that just has the geometry and the dates for each year

for (DATE in as.character(seq(ymd("2005-01-01"), ymd("2023-10-03"), "days"))){
  
  # ## test date parameter
  # DATE = "1999-12-02"
  
  ## create temporary directory if it doesn't yet exist
  if (!dir.exists(paste0('Climate data/store/prism_temporary/', DATE))) {dir.create(paste0('Climate data/store/prism_temporary/', DATE), recursive = T)}
  dir = paste0('Climate data/store/prism_temporary/', DATE)
  
  ## set destination for PRISM raw .bil files
  options(prism.path = dir) 
  get_prism_dailys(type = "tmax", minDate = DATE, maxDate = DATE, keepZip = F) 
  
  ## paths
  subdir = list.files(dir)
  path   = list.files(dir, full.names = T)
  file   = paste(path, "/", subdir, ".bil", sep = "")
  
  ## create directory if it doesn't yet exist
  if (!dir.exists(paste0('Climate data/prism/maxt/', year(DATE)))) {dir.create(paste0('Climate data/prism/maxt/', year(DATE)), recursive = T)}
  
  ## extract
  polys %>% 
    mutate(max_t = exact_extract(raster(file), polys, 'mean')) %>% 
    st_drop_geometry() %>% 
    write_feather(paste0('Climate data/prism/maxt/', year(DATE), '/maxt_', DATE, '.feather'))
  
  ## delete prism file directory
  unlink(dir, recursive = T)
  
  ## clean house
  gc()
}
## end of script, have a great day!

prism <- read_feather('Climate data/prism/maxt/2005/maxt_2005-01-01.feather')

## Now we a dataframe for the temperature at each location for each of the cities, by day by year


for (DATE in as.character(seq(ymd("2005-01-01"), ymd("2023-10-03"), "days"))) {
 
  ## Where are the files
  dir = paste0('Data/Climate data/prism/maxt/', year(DATE))
  
  ## What is the file
  file   = read_feather(paste0(dir, '/maxt_', DATE, '.feather'))
  
  colnames(file)[15] <- paste0("max_t")
  
  mutate(file, date = (paste0(year(DATE),"_",month(DATE),"_",day(DATE))))
  
  write_feather(file, paste0('Data/Climate data/prism/maxt/', year(DATE), '/maxt_', DATE, '.feather'))
  
}


