############# Necessary Libraries ##############

####### 

################################################


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



#New function for getting migration movement!
#variables include "MOVEDOUT", "MOVEDIN" and "MOVEDNET"
#Table results will 2 names and GEOIDs.
#the first is the reference and the second is where people moved to or from 
#From 2006-2010 and 2011-2015 we can cross flow data with demographics
#using the (breakdown = "") argument and (breakdown_labels = TRUE)
#However there are limitations you can find more here: https://www.census.gov/data/developers/data-sets/acs-migration-flows.html



rappflow2010 <- get_flows(geography = "county",
                      state = 51,
                      variable = "MOVEDNET",
                      breakdown = "AGE",
                      breakdown_labels = TRUE,
                      county = 157,
                      year = 2010)

rappflow2015 <- get_flows(geography = "county",
                          state = 51,
                          variable = "MOVEDNET",
                          breakdown = "AGE",
                          breakdown_labels = TRUE,
                          county = 157,
                          year = 2015)

rappflow2010 <- rappflow2010 %>% filter(variable == "MOVEDNET") %>% filter(AGE != "00")
rappflow2015 <- rappflow2015 %>% filter(variable == "MOVEDNET") %>% filter(AGE != "00")


distribution2010 <- data.frame(AGE = rappflow2010$AGE, estimate = rappflow2010$estimate) 

ggplot(distribution2010, aes(x = AGE, y = estimate)) +
  geom_col()


distribution2015 <- data.frame(AGE = rappflow2015$AGE, estimate = rappflow2015$estimate)


rappflow_older <- rappflow %>% slice(1369:1824) %>% filter(variable == "MOVEDNET")
rappflow_younger <- rappflow %>% slice(1:1369) %>% filter(variable == "MOVEDNET") %>% filter(AGE != "00") %>% group_by(AGE)


movednet <- rappflow_younger$estimate 
rappflow$estimate 

ggplot(rappflow_younger, aes(x = AGE, y = estimate, fill = AGE_label)) +
  geom_col() +
  ylim(-20,20)



indexes <-  str_which(rappflow$AGE_label, "60 to 64 years|65 to 69 years|70 to 74 years|75 year and over")


ggplot(rappflow,

ggplot(rappflow_older, aes(x = AGE_label, y = estimate, fill = AGE_label)) +
geom_col(position = "dodge")

levels(rappflow$AGE_label)
str(rappflow$AGE)

