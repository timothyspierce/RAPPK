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


########################################################################################
#I had the ACS codes for these from another R file so I did this for fun.


homevaluesvector = c(home_values_10kless = "B25075_002",
home_values_10_14k = "B25075_003",
home_values_15_19k = "B25075_004",
home_values_20_24k = "B25075_005",
home_values_25_29k = "B25075_006",
home_values_30_34k = "B25075_007",
home_values_35_39k = "B25075_008",
home_values_40_49k = "B25075_009",
home_values_50_59k = "B25075_010",
home_values_60_69k = "B25075_011",
home_values_70_79k = "B25075_012",
home_values_80_89k = "B25075_013",
home_values_90_99k = "B25075_014",
home_values_100_124k = "B25075_015",
home_values_125_149k = "B25075_016",
home_values_150_174k = "B25075_017",
home_values_175_199k = "B25075_018",
home_values_200_249k = "B25075_019",
home_values_250_299k = "B25075_020",
home_values_300_399k = "B25075_021",
home_values_400_499k = "B25075_022",
home_values_500_749k = "B25075_023",
home_values_750_999k = "B25075_024",
home_values_1_1.4m = "B25075_025",
home_values_1.5_1.9m = "B25075_026",
home_values_2mmore = "B25075_027")

housing <- get_acs(geography = "county subdivision",
        state = 51,
        county = 157,
        variables = homevaluesvector,
        summary_var = "B25075_001",
        year = 2019,
        geometry = TRUE,
        keep_geo_vars = TRUE) %>%
          rename(DISTRICT = NAME.y) 


#So I did this by percentage of houses in that subcounty at certain values.
#I removed any variables that resulted in 0%

housing <- mutate(housing, DISTRICT = str_replace_all(housing$DISTRICT, ", Rappahannock County, Virginia", ""))
housingpercentage <- housing %>% mutate(pct_estimate = (estimate/summary_est)*100)
housingspecific <- housingpercentage %>% filter(pct_estimate != 0)


#So removing all those 0s gives us a very interesting picture so we get to see where homes of certain values exclusively exist.

ggplot(housingpercentage) + geom_sf(aes(fill = pct_estimate)) + coord_sf(datum = NA) + facet_wrap(~variable)
ggplot(housingspecific) + geom_sf(aes(fill = pct_estimate)) + coord_sf(datum = NA) + facet_wrap(~variable)
