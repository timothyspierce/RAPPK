##### Necessary Libraries ######



#Decennial is a dead end, this is here for posterity



##########################################




library(tidyverse)
library(tidycensus)
library(ggplot2)
library(sp)
library(rgdal)
library(tigris)
library(raster) 
library(maptools)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(tmap)
library(tmaptools)
library(maps)
library(leaflet)
library(viridis)
library(mapview)
library(RColorBrewer)
library(stringr)
library(osmdata)
library(osrm)
library(sf)
library(ggpubr)
library(ggmap)
library(gridExtra)
library(xlsx)
library(ipumsr)
devtools::install_github("rCarto/osrm")
options(tigris_use_cache = TRUE)
options(tigris_class="sf")

#####################################################################

acs_poptotal <- c(poptotal = "B02001_001") #this is acs only
poptotal <- c(poptotal = "P001001")

########## Necessary functions for working all variables ############



#Gets the variables for Rappahannock as a whole county rather than subcounty
#Adds a percent column
get_rapp_all <- function(varcode, summary_var, year = 2010){
  get_decennial(geography = "county",
          state = 51,
          county = 157,
          variables = varcode,
          summary_var = summary_var,
          year = year,
          geometry = TRUE,
          keep_geo_vars = TRUE,
          cache = TRUE)   }


#Same as above but wide and no percent column
get_rapp_all_wide <- function(varcode, summary_var, year = 2010){
  get_decennial(geography = "county",
                state = 51,
                county = 157,
                variables = varcode,
                summary_var = summary_var,
                year = year,
                geometry = TRUE,
                keep_geo_vars = TRUE,
                cache = TRUE)   }

#2019 census data
get_rapp_acs_all <- function(varcode, summary_var, year = 2019){
  get_acs(geography = "county",
          state = 51,
          county = 157,
          variables = varcode,
          summary_var = summary_var,
          year = year,
          geometry = TRUE,
          keep_geo_vars = TRUE,
          cache = TRUE) %>%
    mutate(percent = (estimate/summary_est)*100) }



#Function for getting variables by county subdivison and makes a pct column
get_rapp <-  function(varcode, summary_var, year = 2010){
  get_decennial(geography = "county subdivision",
          state = 51,
          county = 157,
          variables = varcode,
          summary_var = summary_var,
          year = year,
          geometry = TRUE,
          keep_geo_vars = TRUE,
          cache = TRUE) %>%
    subset(select = -c(NAME.y)) }
  


#Same as above but wide and no percent column
get_rapp_wide <- function(varcode, summary_var, year = 2010){
            get_decennial(geography = "county subdivision",
                            state = 51,
                            county = 157,
                            variables = varcode,
                            summary_var = summary_var,
                            year = year,
                            geometry = TRUE,
                            keep_geo_vars = TRUE,
                            output = "wide",
                            cache = TRUE) %>%
  subset(select = -c(NAME.y))  }



get_rapp_acs <- function(varcode, summary_var, year = 2019){
  get_acs(geography = "county subdivision",
          state = 51,
          county = 157,
          variables = varcode,
          summary_var = summary_var,
          year = year,
          geometry = TRUE,
          keep_geo_vars = TRUE,
          cache = TRUE) %>%
    mutate(percent = (estimate/sum(summary_est))*100) %>%
    subset(select = -c(NAME.y)) }



#######################################################

#varcode for population by race
race_var <- (c(poptotal = "P001001",
            white = "P003002",
            black = "P003003",
            first_nations = "P003004",
            asian = "P003005",
            oceania = "P003006",
            other = "P003007",
            mixed = "P003008",
            hisplatino = "P004003"))


######################## Total Population from 2000 to 2019 ##########################################

  
  rapp_pop_2019_wide <- get_rapp_acs_all(acs_poptotal, acs_poptotal) %>%
  subset(select = c(GEOID, geometry, NAME.y, variable, estimate, summary_est)) %>%
  rename(c(NAME = NAME.y, value = estimate, summary_value...poptotal = summary_est)) 
  
rapp_pop_2019_wide$NAME <- str_replace(rapp_pop_2019_wide$NAME, ", Virginia", "")


rapp_pop_2010_wide <- get_rapp_all(poptotal, poptotal) %>% 
  subset(select = c(GEOID, NAME.y, variable, value, summary_value...poptotal, geometry)) %>%
  rename(c(NAME = NAME.y)) 

rapp_pop_2010_wide$NAME <- str_replace(rapp_pop_2010_wide$NAME, ", Virginia", "")


rapp_pop_2000_wide <- get_rapp_all(poptotal, poptotal, 2000) %>% 
  subset(select = c(GEOID, NAME, variable, value, summary_value...poptotal, geometry))

rapp_pop_2000_wide$NAME <- str_replace(rapp_pop_2000_wide$NAME, ", Virginia", "") 



time_series <- rbind(rapp_pop_2019_wide, rapp_pop_2010_wide, rapp_pop_2000_wide) %>% 
  add_column(percent = NA) %>%
  add_column(year = c("2019", "2010", "2000"))

ggplot(time_series, aes(x = year, y = value)) +
  geom_line()



########################## Total Population from 2010 to 2019 by Subdivision

#Getting Rapp's 2019 ACS population data
rapp_pop_2019 <- get_rapp_acs(acs_poptotal, acs_poptotal) %>%
  subset(select = c(GEOID, NAME.x, variable, estimate, summary_est, percent, geometry)) %>%
  rename(c(NAME = NAME.x, value = estimate, summary_value...poptotal = summary_est))


#getting Rapp's 2010 Decennial population data
rapp_pop_2010 <- get_rapp(poptotal, poptotal) %>%
  mutate(percent = (value/sum(summary_value...poptotal))*100) %>%
  subset(select = c(GEOID, NAME.x, variable, value, summary_value...poptotal, percent, geometry)) %>%
  rename(c(NAME = NAME.x))


#This section is not possible because apparantly "county subdivison" wasn't a thing in the decennial pre-2010
#getting Rapp's 2000 Decennial population data
# rapp_pop_2000 <- get_rapp(poptotal, poptotal, 2000) %>%
#   mutate(percent = (value/sum(summary_value...poptotal))*100) %>%
#   subset(select = c(GEOID, NAME.x, variable, value, summary_value...poptotal, percent, geometry)) %>%
#   rename(c(NAME = NAME.x))

# combining the Two population gets and adds a column so that their year is delineated for analysis
rapp_pop_time <- rbind(rapp_pop_2019,
                       rapp_pop_2010) %>%
  add_column(year = c("2019", "2019", "2019", "2019", "2019",
                      "2010", "2010", "2010", "2010", "2010"))


ggplot(rapp_pop_time, aes(x = year, y = value, color = NAME)) +
  geom_line()



###################################Districts with Rappahannock as the baseline#################################################


rapp_pop_2019_wide <- rapp_pop_2019_wide %>% 
  add_column(percent = 100) %>%
  subset(select = c(GEOID, NAME, variable, value, summary_value...poptotal, percent, geometry))
rapp_pop_2019_wide$NAME <- str_replace(rapp_pop_2019_wide$NAME, ", Virginia", "")

rapp_pop_2010_wide <- rapp_pop_2010_wide %>% 
  add_column(percent = 100)%>%
  subset(select = c(GEOID, NAME, variable, value, summary_value...poptotal, percent, geometry))
rapp_pop_2010_wide$NAME <- str_replace(rapp_pop_2010_wide$NAME, ", Virginia", "")

time_and_baseline <- rbind(rapp_pop_2019_wide, rapp_pop_2019, rapp_pop_2010_wide, rapp_pop_2010) %>%
  add_column(year = c("2019", "2019", "2019", "2019", "2019", "2019", 
                      "2010", "2010", "2010", "2010", "2010", "2010"))

ggplot(time_and_baseline, aes(x = year, y = value, fill = NAME)) +
  geom_col(position = "dodge")

ggplot(time_and_baseline, aes(x = year, y = percent, fill = NAME)) +
  geom_col(position = "dodge")

ggplot(time_and_baseline, aes(x = year, y = value, color = NAME)) +
  geom_line()


################################################### 



age_vars <- c("DP1_C43", "DP1_C44")

get_rapp_all(age_vars, "DP1_C43")

age_vars <- c("65_74_total" = "DP1_C26", "65_74_percent" =	"DP1_C27", "75_84_total" =	"DP1_C28", "75_84_percent" = "DP1_C29")

age_vars <- c("65_and_older_total" = "SF1DP1049", "65_and_older_percent" = "SF1DP1050")


over65 <- get_rapp_all_wide(age_vars, "SF1DP1001")


