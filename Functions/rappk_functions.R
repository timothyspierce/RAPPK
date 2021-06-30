library(tidycensus)
library(tidyverse)
library(dplyr)

#This function will generate an entire table of results from the ACS census data at the county level. 
#You can change county to county subdivision.

rapp_table <- function(varcode, year){
  get_acs(geography = "county subdivision",
          state = 51,
          county = 157,
          table = varcode,
          year = year,
          output = "wide",
          geometry = TRUE)}

#This will pull specific variables/lines out of a table at the county subdivision level
rapp_var <- function(varcode){
  get_acs(geography = "county subdivision",
          state = 51,
          county = 157,
          variables = varcode,
          year = 2019,
          geometry = TRUE,
          output = "wide")}

#This function will generate a map of results from the variable called into question.
#This map will be separated at the county subdivision level.
rapp_map <- function(varcode){
  get_acs(geography = "county subdivision",
          state = 51,
          county = 157,
          variables = varcode,
          year = 2019,
          geometry = TRUE) %>% 
    ggplot() + geom_sf(aes(fill = estimate))}


