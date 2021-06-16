## Rappahannock County Racial breakdown. You know, in theory
#All intended libraries for this plan
library(tidyverse)
library(tidycensus)
library(ggplot2)
library(ggmap)
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
devtools::install_github("rCarto/osrm")
options(tigris_use_cache = TRUE)
options(tigris_class="sf")
census_api_key("f307140366c2d8fd4688eda86d5e085362d7ec44", install = TRUE)

###################################################################################
# subcountynames <- c("Hampton District", "Jackson District", "Piedmont District", "Stonewall-Hawthorne District", "Wakefield District")

#Base functions
rapp_table <- function(varcode, year){
  data.frame(get_acs(geography = "county",
                     state = 51,
                     county = 157,
                     table = varcode,
                     year = year))}

#This will pull specific variables/lines out of a table at the county subdivision level
rapp_var <- function(varcode){
  get_acs(geography = "county subdivision",
          state = 51,
          county = 157,
          variables = varcode,
          year = 2018,
          geometry = TRUE)}

#This function will generate a map of results from the variable called into question.
#This map will be separated at the county subdivision level.
rapp_map <- function(varcode){
  get_acs(geography = "county subdivision",
          state = 51,
          county = 157,
          variables = varcode,
          year = 2018,
          geometry = TRUE,
          keep_geo_vars = TRUE) %>%
   
    ggplot() + geom_sf(aes(fill = estimate)) + coord_sf(datum = NA)}


####################################################################################################

#selects and names race variables
renamerace <-c(white = "B02001_002",
  black = "B02001_003",
  first_nations = "B02001_004",
  asian = "B02001_005",
  oceania = "B02001_006",
  other = "B02001_007",
  mixed_total = "B02001_008")

#same as above but without all the white people messing with the rest of the demographic data
renamerace_sanswhite <-c (black = "B02001_003",
                 first_nations = "B02001_004",
                 asian = "B02001_005",
                 oceania = "B02001_006",
                 other = "B02001_007",
                 mixed_total = "B02001_008")

#Get race by county subdivision
race_county_sub <- rapp_var(renamerace) %>% rename(DISTRICT = NAME) 

#Maps race population by county. Rappahannock County? More like Ranch Dressing County
rapp_race_map <- rapp_map(renamerace) + facet_wrap(~variable)
rapp_race_map_sanswhite <- rapp_map(renamerace_sanswhite) + facet_wrap(~variable)

rapp_race_map
rapp_race_map_sanswhite


#Create column visual for racial composition by county subdivision
#Fixes some labelling problems with the magic of stringr
race_county_sub <- mutate(race_county_sub, DISTRICT = str_replace_all(race_county_sub$DISTRICT, ", Rappahannock County, Virginia", "")) 

race_county_sub %>%
ggplot(aes(x = variable, y = estimate)) +
  geom_col(na.rm = TRUE, show.legend = TRUE) +
  facet_wrap(~DISTRICT)

######################################################################
#selects and names language ability
renamelang <- c(pct_polyglot  = "S0601_C01_024",
                pct_polyglot_eng_well = "S0601_C01_025",
                pct_polyglot_eng_poor = "S0601_C01_026")

#runs language ability variable through "get acs" functions
lang_county_sub <- rapp_var(renamelang) %>% rename(DISTRICT = NAME)
#Fixes some labelling problems with the magic of stringr
lang_county_sub <-  mutate(lang_county_sub, DISTRICT = str_replace_all(lang_county_sub$DISTRICT, ", Rappahannock County, Virginia", ""))



rapp_lang_map <- rapp_map(renamelang) + facet_wrap(~variable)

lang_county_sub %>% ggplot(aes(x = variable, y = estimate,)) +
  geom_col(na.rm = TRUE, show.legend = TRUE) +
  facet_wrap(~DISTRICT)

###################################################################################


#So this should've worked but it didn't, I'm going to play with it later because DP0 tables are awful

rapptract <- get_acs(geography = "tract", state = 51,
                     county = 157,
                     variables = (c(pop_total="DP05_0033",
                                    race_single_total = "DP05_0034",
                                    race_multi_total = "DP05_0035",
                                    race_single_white = "DP05_0037",
                                    race_single_black = "DP05_0038",
                                    race_single_firstnations ="DP05_0039",
                                    race_single_asian = "DP05_0044",
                                    race_single_oceania ="DP05_0052",
                                    race_single_other = "DP05_0057",
                                    race_hisp_latin_total = "DP05_0070",
                                    race_hisp_latin_anyrace = "DP05_0071",
                                    race_not_hisp_total = "DP05_0076",
                                    race_not_hisp_white = "DP05_077",
                                    race_not_hisp_black = "DP05_0078",
                                    race_not_hisp_firstnations = "DP05_0079",
                                    race_not_hisp_asian = "DP05_0080",
                                    race_not_hisp_oceania ="DP05_0081",
                                    race_not_hisp_other = "DP05_0082",
                                    race_not_hisp_mixed = "DP05_0083",
                     )),
                     summary_var = "DP05_0033",
                     output = "wide",
                     cache = TRUE,
                     geometry = TRUE,
                     keep_geo_vars = TRUE)

