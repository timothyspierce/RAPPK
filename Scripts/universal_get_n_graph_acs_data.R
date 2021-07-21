##### Necessary Libraries ######
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
library(scales)
library(fpp2)
devtools::install_github("rCarto/osrm")
options(tigris_use_cache = TRUE)
options(tigris_class="sf")

#####################################################################

#Rappk Coordinates, Latitude = 38.6762° Longitude = -78.1564

#Generalized vector to get the total population for any function
pop_total <- c(poptotal = "B02001_001")

########## Necessary functions for working all variables ############



#Gets the states for Rappahannock as a whole county rather than subcounty
rapp_all <- function(varcode, summary_var, year = 2019){
  get_acs(geography = "county",
          state = 51,
          county = 157,
          variables = varcode,
          summary_var = summary_var,
          year = year,
          geometry = TRUE,
          keep_geo_vars = TRUE,
          cache = TRUE) %>%
    mutate(percent = (estimate/sum(summary_est))*100) %>%
    subset(select = -c(COUNTYNS))}

#Function for getting variables and makes a pct column
rapp_var <- function(varcode, summary_var, year = 2019){
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
    subset(select = -c(COUSUBFP, COUSUBNS))}


#Function for making a standard bar graph
bar_graph <- function(dataset){
  ggplot(dataset, aes(x = variable, y = estimate, fill = NAME.x)) +
    geom_col(position = "dodge") +
    facet_wrap(~variable)}

#Function for making a standard line graph
#Okay It's not that standard, I'm working on it
#line_graph <- function(dataset){
 # ggplot(dataset, aes(x = NAME, y = estimate, color = NAME)) +
  #  geom_point() +
   # facet_wrap(~variable)}


## Experimental function that should be able to take whatever variable list you get from the above functions
#And turn them into bar graphs, provided you fo everything else on the page
bar_graph_and_medianline <- function(dataset){
  ggplot(dataset, aes(x = NAME.x, y = estimate, fill = NAME)) +
    geom_col(position = "dodge") + 
    geom_hline(aes(yintercept = median(estimate)), color = "black", size = 1.5, alpha = 0.25) +
    labs(yintercept = "Median") +
    facet_wrap(~variable)}


#The neat thing about these functions, with the much appreciated help from Christina Prisbe
#Is the horizontial line thing! Now my primary issue is what to do about scaling.
#Nevermind the scaling problem, I fixed it. Sort of.
bar_graph_and_meanline <- function(dataset){
  ggplot(dataset, aes(x = NAME.x, y = estimate, fill = NAME.x)) +
    geom_col(position = "dodge") + 
    geom_hline(aes(yintercept = mean(estimate)), color = "black", size = 1.5, alpha = 0.25) +
    labs(yintercept = "Mean") +
    facet_wrap(~variable)}



#I'm working on it
#time_shift_bar_graph <- ggarrange



get_map(location = c(lon = -78.1564, lat = 38.6762), maptype = c("hybrid"))

###############################################################
################### For Means of travel #########################


#Means of travel vector for the function
means_vars <- c( travelmeans_all = "B08101_001",
                 travelmeans_alone = "B08101_009",
                 travelmeans_carpool = "B08101_017",
                 travelmeans_publicnotaxi= "B08101_025",
                 travelmeans_walked = "B08101_033",
                 travelmeans_other = "B08101_041",
                 travelmeans_athome = "B08101_049")
travelmeans_all = "B08101_001"

#Doing the ACS call, adding a row for the total
#Doing some housecleaning with some columns
travel_means <- rapp_var(means_vars, travelmeans_all) %>%
  add_row(rapp_all(means_vars, travelmeans_all)) %>%
  subset(select = -c(NAME.y))

#Here's your variable. Plug it into one of the graphing functions and see what happens
travel_means

##########################################################################
######################## For Commuting times #############################


#Commute time vector for function
commute_vars <- c(travel_less_5 = "B08303_002",
                  travel_5_9 = "B08303_003",
                  travel_10_14 = "B08303_004",
                  travel_15_19 = "B08303_005",
                  travel_20_24 = "B08303_006",
                  travel_25_29 = "B08303_007",
                  travel_30_34 = "B08303_008",
                  travel_35_39 = "B08303_009",
                  travel_40_44 = "B08303_010",
                  travel_45_59 = "B08303_011",
                  travel_60_89 = "B08303_012",
                  travel_90_plus = "B08303_013")
travel_all = "B08303_001"

#For when I factor them
commutevector <- c("travel_less_5",
                   "travel_5_9",
                   "travel_10_14",
                   "travel_15_19",
                   "travel_20_24",
                   "travel_25_29",
                   "travel_30_34",
                   "travel_35_39",
                   "travel_40_44",
                   "travel_45_59",
                   "travel_60_89",
                   "travel_90_plus")


#This is the part where I factor them for time hierarchy
factor(commutevector, order = TRUE, levels = c(commutevector))


#Doing the ACS call, adding a row for all of Rappahannock
#Doing some housecleaning with some columns
commute_time <- rapp_var(commute_vars, travel_all) %>% 
  add_row(rapp_all(commute_vars, travel_all)) %>%
  subset(select = -c(NAME.y))


commute_time$variable <- factor(as.factor(commute_time$variable), levels = commutevector)


#Here's your variable. Plug it into one of the graphing functions and see what happens
commute_time


#######################################################################
###################### Getting Race Demographics ###################

#selects and names race variables
race_vars <- c(white = "B02001_002",
               black = "B02001_003",
               first_nations = "B02001_004",
               asian = "B02001_005",
               oceania = "B02001_006",
               other = "B02001_007",
               mixed_total = "B02001_008")

#Doing the ACS call, adding a row for all of Rappahannock
#Doing some housecleaning with some columns
#Getting rid of some non-entity values
race_demo <- rapp_var(race_vars, pop_total) %>% 
  add_row(rapp_all(race_vars, pop_total)) %>%
  subset(select = -c(NAME.y)) %>%
  filter(percent != 0)

#Here's your variable. Plug it into one of the graphing functions and see what happens
race_demo

######################################################################
####################### For Housing Values ###########################

#Selects and names housing values
homevalue_var = c(home_values_10kless = "B25075_002",
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
homes_all = "B25075_001"

#I used this to set the levels for the variables when I factor them later.
#Thank goodness i figured out how to factor them
housingvector = c("home_values_10kless",
                  "home_values_10_14k",
                  "home_values_15_19k",
                  "home_values_20_24k",
                  "home_values_25_29k",
                  "home_values_30_34k",
                  "home_values_35_39k",
                  "home_values_40_49k",
                  "home_values_50_59k",
                  "home_values_60_69k",
                  "home_values_70_79k",
                  "home_values_80_89k",
                  "home_values_90_99k",
                  "home_values_100_124k",
                  "home_values_125_149k",
                  "home_values_150_174k",
                  "home_values_175_199k",
                  "home_values_200_249k",
                  "home_values_250_299k",
                  "home_values_300_399k",
                  "home_values_400_499k",
                  "home_values_500_749k",
                  "home_values_750_999k",
                  "home_values_1_1.4m",
                  "home_values_1.5_1.9m",
                  "home_values_2mmore")

#Factoring these values for useage!
factor(housingvector, order = TRUE, levels = c(housingvector))


#Doing the ACS call, adding a row for all of Rappahannock
#Doing some housecleaning with some columns
#Getting rid of some non-entity values
housing_values <- rapp_var(homevalue_var, homes_all) %>% 
  add_row(rapp_all(homevalue_var, homes_all)) %>%
  subset(select = -c(NAME.y)) %>%
  filter(percent != 0)

#This is the part where I factor them for time hierarchy
housing_values$variable <- factor(as.factor(housing_values$variable), levels = housingvector)

#Here's your variable. Plug it into one of the graphing functions and see what happens
housing_values

#####################################################################
################## For Median Household Income ######################


#hhtype_total = "B11001_001",

medianincome_var <- c(
med_hh_income_less10k = "B19001_002",
med_hh_income_10_14k = "B19001_003",
med_hh_income_15_19k = "B19001_004",
med_hh_income_20_24k = "B19001_005",
med_hh_income_25_29k = "B19001_006",
med_hh_income_30_34k = "B19001_007",
med_hh_income_35_39k = "B19001_008",
med_hh_income_40_44k = "B19001_009",
med_hh_income_45_49k = "B19001_010",
med_hh_income_50_59k = "B19001_011",
med_hh_income_60_74k = "B19001_012",
med_hh_income_75_99k = "B19001_013",
med_hh_income_100_124k = "B19001_014",
med_hh_income_125_149k = "B19001_015",
med_hh_income_150_199k = "B19001_016",
med_hh_income_200kmore = "B19001_017")
median_income_all = "B19001_001"


medianincomevector <- c(
                     "med_hh_income_less10k",
                     "med_hh_income_10_14k",
                     "med_hh_income_15_19k",
                     "med_hh_income_20_24k",
                     "med_hh_income_25_29k",
                     "med_hh_income_30_34k",
                     "med_hh_income_35_39k",
                     "med_hh_income_40_44k",
                     "med_hh_income_45_49k",
                     "med_hh_income_50_59k",
                     "med_hh_income_60_74k",
                     "med_hh_income_75_99k",
                     "med_hh_income_100_124k",
                     "med_hh_income_125_149k",
                     "med_hh_income_150_199k",
                     "med_hh_income_200kmore")


#Factoring these values for useage!
factor(medianincomevector, order = TRUE, levels = c(medianincomevector))


#Doing the ACS call, adding a row for all of Rappahannock
#Doing some housecleaning with some columns
#Getting rid of some non-entity values
median_income <- rapp_var(medianincome_var, median_income_all) %>% 
  add_row(rapp_all(medianincome_var, median_income_all)) %>%
  subset(select = -c(NAME.y)) %>%
  filter(percent != 0)


#Factoring so that the variables have a hierarchy
median_income$variable <- factor(as.factor(median_income$variable), levels = medianincomevector)


#Here's your variable. Plug it into one of the graphing functions and see what happens
median_income

#########################################################################

population <- rapp_all(pop_total, pop_total) %>%
  add_row(rapp_var(pop_total, pop_total)) %>%
  add_row(rapp_all(pop_total, pop_total, year = 2018)) %>%
  add_row(rapp_var(pop_total, pop_total, year = 2018)) %>%
  add_row(rapp_all(pop_total, pop_total, year = 2017)) %>%
  add_row(rapp_var(pop_total, pop_total, year = 2017)) %>%
  add_row(rapp_all(pop_total, pop_total, year = 2016)) %>%
  add_row(rapp_var(pop_total, pop_total, year = 2016)) %>%
  add_row(rapp_all(pop_total, pop_total, year = 2015)) %>%
  add_row(rapp_var(pop_total, pop_total, year = 2015)) %>%
  #There's some compatibility issues with the geometry that makes combining geography pre-2014 a headache
  # add_row(rapp_all(pop_total, pop_total, year = 2014)) %>%
  # add_row(rapp_var(pop_total, pop_total, year = 2014)) %>%
  #add_row(rapp_all(pop_total, pop_total, year = 2013)) %>%
  #add_row(rapp_var(pop_total, pop_total, year = 2013)) %>%
  #So it looks like there are 9 more columns in the dataframe pre 2013
  #So this little experiment only goes so far
  #add_row(rapp_var(pop_total, pop_total, year = 2012)) %>%
  subset(select = -c(NAME.y)) %>%
  rename(NAME = NAME.x) %>%
  add_column(year = c("2019", "2019", "2019", "2019", "2019", "2019",
                      "2018", "2018", "2018", "2018", "2018", "2018",
                      "2017", "2017", "2017", "2017", "2017", "2017",
                      "2016", "2016", "2016", "2016", "2016", "2016",
                      "2015", "2015", "2015", "2015", "2015", "2015"))

ggplot(population, aes(x = year, y = estimate, group = NAME, color = NAME)) +
  geom_line()



population_districts <- population %>% filter(NAME != "Rappahannock")
tm_shape(population_districts) +
  tm_borders() +
  tm_fill("percent") +
  tm_facets(by = "year") +
  tm_text("NAME")

  


#####################################################################
############ Ignore this, this is stuff I'm playing around with for our purposes#####


ggplottheme <- theme(plot.title=element_text(size=20, 
                                             face="bold", 
                                             family="Monotype",
                                             color="blue",
                                             hjust=0.5,
                                             lineheight=1.2),  # title
                     plot.subtitle=element_text(size=15, 
                                                family="Monotype",
                                                face="bold",
                                                hjust=0.5),  # subtitle
                     plot.caption=element_text(size=15),  # caption
                     axis.title.x=element_text(vjust=10,  
                                               size=15),  # X axis title
                     axis.title.y=element_text(size=15),  # Y axis title
                     axis.text.x=element_text(size=10, 
                                              angle = 90,
                                              vjust=.5),  # X axis text
                     axis.text.y=element_text(size=10))  # Y axis text



ggplot(aes(x = variable, y = estimate, fill = variable)) +
  geom_col(position = "dodge") +
  
  facet_wrap(~NAME)
 



ggplot(travel_means, aes(x = estimate)) +
  geom_histogram() +
  #ggtitle("", subtitle ="") +
  #xlab("") +
  #ylab("") +
  facet_wrap(~NAME)

ggplot(travel_means, aes(x = variable, y = estimate, fill = variable)) +
  geom_col(position = "dodge") +

  #ggtitle("", subtitle ="") +
  #xlab("") +
  #ylab("") +
  geom_hline(aes(yintercept= 50.1, linetype = "Median"), color= "black", size = 1.5, alpha = 0.25) +
  facet_wrap(~NAME)

