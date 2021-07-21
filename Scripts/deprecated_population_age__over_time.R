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
    subset(select = -c(COUNTYNS))
}

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

###########################################################################

age_var <- c(under18 = "S0101_C01_022",
             age18_24 = "S0101_C01_023",
             age25_29 = "S0101_C01_007",
             age30_34 = "S0101_C01_008",
             age35_39 = "S0101_C01_009",
             age40_44 = "S0101_C01_010",
             age45_49 = "S0101_C01_011",
             age50_54 = "S0101_C01_012",
             age55_59 = "S0101_C01_013",
             age60_64 = "S0101_C01_014",
             age65_older = "S0101_C01_030")

age_vector <- c("under18",
                             "age18_29",
                             "age30_64",
                             "age65_older")

pop_total <- c(pop_total = "S0101_C01_001")


##### Age 2019

age <- rapp_all(age_var, pop_total) %>%
  subset(select = -c(NAME.y, STATEFP, COUNTYFP, AFFGEOID, LSAD, ALAND, AWATER)) %>%
  rename(NAME = NAME.x)

adult <- filter(age, variable == "age30_34" | variable == "age35_39" | variable == "age40_44" | variable == "age45_49" | variable == "age50_54" | variable == "age55_59" | variable == "age60_64") %>%
  summarise(GEOID, NAME, variable = "age30_64", estimate = sum(estimate), moe = sum(moe), summary_est, summary_moe, percent = sum(percent)) %>%
  slice(1)  

youngadult <- filter(age, variable == "age18_24" | variable == "age25_29") %>% 
  summarise(GEOID, NAME, variable = "age18_29", estimate = sum(estimate), moe = sum(moe), summary_est, summary_moe, percent = sum(percent)) %>%
  slice(1)  

age <- age %>% filter(variable != "age18_24" & variable != "age25_29" & variable != "age30_34" & variable != "age35_39" & variable != "age40_44" & variable != "age45_49" & variable != "age50_54" & variable != "age55_59" & variable != "age60_64")

age <- age %>% rbind(youngadult) %>% rbind(adult)

age$variable <- factor(age_vector, order = TRUE, levels = c(age_vector))

age <- mutate(age, percent = (estimate/summary_est) * 100)

age2019 <- cbind(age, year = "2019")

##### Age 2018

age <- rapp_all(age_var, pop_total, 2018) %>%
  subset(select = -c(NAME.y, STATEFP, COUNTYFP, AFFGEOID, LSAD, ALAND, AWATER)) %>%
  rename(NAME = NAME.x)

adult <- filter(age, variable == "age30_34" | variable == "age35_39" | variable == "age40_44" | variable == "age45_49" | variable == "age50_54" | variable == "age55_59" | variable == "age60_64") %>%
  summarise(GEOID, NAME, variable = "age30_64", estimate = sum(estimate), moe = sum(moe), summary_est, summary_moe, percent = sum(percent)) %>%
  slice(1)  

youngadult <- filter(age, variable == "age18_24" | variable == "age25_29") %>% 
  summarise(GEOID, NAME, variable = "age18_29", estimate = sum(estimate), moe = sum(moe), summary_est, summary_moe, percent = sum(percent)) %>%
  slice(1)  

age <- age %>% filter(variable != "age18_24" & variable != "age25_29" & variable != "age30_34" & variable != "age35_39" & variable != "age40_44" & variable != "age45_49" & variable != "age50_54" & variable != "age55_59" & variable != "age60_64")

age <- age %>% rbind(youngadult) %>% rbind(adult)

age$variable <- factor(age_vector, order = TRUE, levels = c(age_vector))

age <- mutate(age, percent = (estimate/summary_est) * 100)

age2018 <- cbind(age, year = "2018")

##### Age 2017

age <- rapp_all(age_var, pop_total, 2017) %>%
  subset(select = -c(NAME.y, STATEFP, COUNTYFP, AFFGEOID, LSAD, ALAND, AWATER)) %>%
  rename(NAME = NAME.x)

adult <- filter(age, variable == "age30_34" | variable == "age35_39" | variable == "age40_44" | variable == "age45_49" | variable == "age50_54" | variable == "age55_59" | variable == "age60_64") %>%
  summarise( GEOID, NAME, variable = "age30_64", estimate = sum(estimate), moe = sum(moe), summary_est, summary_moe, percent = sum(percent)) %>%
  slice(1)  

youngadult <- filter(age, variable == "age18_24" | variable == "age25_29") %>% 
  summarise( GEOID, NAME, variable = "age18_29", estimate = sum(estimate), moe = sum(moe), summary_est, summary_moe, percent = sum(percent)) %>%
  slice(1)  

age <- age %>% filter(variable != "age18_24" & variable != "age25_29" & variable != "age30_34" & variable != "age35_39" & variable != "age40_44" & variable != "age45_49" & variable != "age50_54" & variable != "age55_59" & variable != "age60_64")

age <- age %>% rbind(youngadult) %>% rbind(adult)

age$variable <- factor(age_vector, order = TRUE, levels = c(age_vector))

age <- mutate(age, percent = (estimate/summary_est) * 100)

age2017 <- cbind(age, year = "2017")
##############################################################################3










##### Age 2016

age <- rapp_all(age_var, pop_total, 2016) %>%
  subset(select = -c(NAME.y, STATEFP, COUNTYFP, AFFGEOID, LSAD, ALAND, AWATER)) %>%
  rename(NAME = NAME.x)

adult <- filter(age, variable == "age30_34" | variable == "age35_39" | variable == "age40_44" | variable == "age45_49" | variable == "age50_54" | variable == "age55_59" | variable == "age60_64") %>%
  summarise(GEOID, NAME, variable = "age30_64", estimate = sum(estimate), moe = sum(moe), summary_est, summary_moe, percent = sum(percent)) %>%
  slice(1)  

youngadult <- filter(age, variable == "age18_24" | variable == "age25_29") %>% 
  summarise(GEOID, NAME, variable = "age18_29", estimate = sum(estimate), moe = sum(moe), summary_est, summary_moe, percent = sum(percent)) %>%
  slice(1)  

age <- age %>% filter(variable != "age18_24" & variable != "age25_29" & variable != "age30_34" & variable != "age35_39" & variable != "age40_44" & variable != "age45_49" & variable != "age50_54" & variable != "age55_59" & variable != "age60_64")

age <- age %>% rbind(youngadult) %>% rbind(adult)

age$variable <- factor(age_vector, order = TRUE, levels = c(age_vector))

age <- mutate(age, percentage = (estimate/summary_est) * 100)

age2016 <- cbind(age, year = "2016")

##### Age 2015

age <- rapp_all(age_var, pop_total, 2015) %>%
  subset(select = -c(NAME.y)) %>%
  rename(NAME = NAME.x)

adult <- filter(age, variable == "age30_34" | variable == "age35_39" | variable == "age40_44" | variable == "age45_49" | variable == "age50_54" | variable == "age55_59" | variable == "age60_64") %>%
  summarise(STATEFP, COUNTYFP, AFFGEOID, GEOID, NAME, LSAD, ALAND, AWATER, variable = "age30_64", estimate = sum(estimate), moe = sum(moe), summary_est, summary_moe, percent = sum(percent)) %>%
  slice(1)  

youngadult <- filter(age, variable == "age18_24" | variable == "age25_29") %>% 
  summarise(STATEFP, COUNTYFP, AFFGEOID, GEOID, NAME, LSAD, ALAND, AWATER, variable = "age18_29", estimate = sum(estimate), moe = sum(moe), summary_est, summary_moe, percent = sum(percent)) %>%
  slice(1)  

age <- age %>% filter(variable != "age18_24" & variable != "age25_29" & variable != "age30_34" & variable != "age35_39" & variable != "age40_44" & variable != "age45_49" & variable != "age50_54" & variable != "age55_59" & variable != "age60_64")

age <- age %>% rbind(youngadult) %>% rbind(adult)

age$variable <- factor(age_vector, order = TRUE, levels = c(age_vector))

age <- mutate(age, percent = (estimate/summary_est) * 100)

age2015 <- cbind(age, year = "2015")

##### Age 2014

age <- rapp_all(age_var, pop_total, 2014) %>%
  subset(select = -c(NAME.y)) %>%
  rename(NAME = NAME.x)

adult <- filter(age, variable == "age30_34" | variable == "age35_39" | variable == "age40_44" | variable == "age45_49" | variable == "age50_54" | variable == "age55_59" | variable == "age60_64") %>%
  summarise(STATEFP, COUNTYFP, AFFGEOID, GEOID, NAME, LSAD, ALAND, AWATER, variable = "age30_64", estimate = sum(estimate), moe = sum(moe), summary_est, summary_moe, percent = sum(percent)) %>%
  slice(1)  

youngadult <- filter(age, variable == "age18_24" | variable == "age25_29") %>% 
  summarise(STATEFP, COUNTYFP, AFFGEOID, GEOID, NAME, LSAD, ALAND, AWATER, variable = "age18_29", estimate = sum(estimate), moe = sum(moe), summary_est, summary_moe, percent = sum(percent)) %>%
  slice(1)  

age <- age %>% filter(variable != "age18_24" & variable != "age25_29" & variable != "age30_34" & variable != "age35_39" & variable != "age40_44" & variable != "age45_49" & variable != "age50_54" & variable != "age55_59" & variable != "age60_64")

age <- age %>% rbind(youngadult) %>% rbind(adult)

age$variable <- factor(age_vector, order = TRUE, levels = c(age_vector))

age <- mutate(age, percent = (estimate/summary_est) * 100)

age2014 <- cbind(age, year = "2014")

##### Age 2013

age <- rapp_all(age_var, pop_total, 2013) %>%
  subset(select = -c(NAME.y)) %>%
  rename(NAME = NAME.x)

adult <- filter(age, variable == "age30_34" | variable == "age35_39" | variable == "age40_44" | variable == "age45_49" | variable == "age50_54" | variable == "age55_59" | variable == "age60_64") %>%
  summarise(STATEFP, COUNTYFP, AFFGEOID, GEOID, NAME, LSAD, ALAND, AWATER, variable = "age30_64", estimate = sum(estimate), moe = sum(moe), summary_est, summary_moe, percent = sum(percent)) %>%
  slice(1)  

youngadult <- filter(age, variable == "age18_24" | variable == "age25_29") %>% 
  summarise(STATEFP, COUNTYFP, AFFGEOID, GEOID, NAME, LSAD, ALAND, AWATER, variable = "age18_29", estimate = sum(estimate), moe = sum(moe), summary_est, summary_moe, percent = sum(percent)) %>%
  slice(1)  

age <- age %>% filter(variable != "age18_24" & variable != "age25_29" & variable != "age30_34" & variable != "age35_39" & variable != "age40_44" & variable != "age45_49" & variable != "age50_54" & variable != "age55_59" & variable != "age60_64")

age <- age %>% rbind(youngadult) %>% rbind(adult)

age$variable <- factor(age_vector, order = TRUE, levels = c(age_vector))

age <- mutate(age, percent = (estimate/summary_est) * 100)

age2013 <- cbind(age, year = "2013")

##### Age 2012

age <- rapp_all(age_var, pop_total, 2012) %>%
  subset(select = -c(NAME.y)) %>%
  rename(NAME = NAME.x)

adult <- filter(age, variable == "age30_34" | variable == "age35_39" | variable == "age40_44" | variable == "age45_49" | variable == "age50_54" | variable == "age55_59" | variable == "age60_64") %>%
  summarise(STATEFP, COUNTYFP, GEOID, NAME, LSAD, ALAND, AWATER, variable = "age30_64", estimate = sum(estimate), moe = sum(moe), summary_est, summary_moe, percent = sum(percent)) %>%
  slice(1)  

youngadult <- filter(age, variable == "age18_24" | variable == "age25_29") %>% 
  summarise(STATEFP, COUNTYFP,  GEOID, NAME, LSAD, ALAND, AWATER, variable = "age18_29", estimate = sum(estimate), moe = sum(moe), summary_est, summary_moe, percent = sum(percent)) %>%
  slice(1)  

age <- age %>% filter(variable != "age18_24" & variable != "age25_29" & variable != "age30_34" & variable != "age35_39" & variable != "age40_44" & variable != "age45_49" & variable != "age50_54" & variable != "age55_59" & variable != "age60_64")

age <- age %>% rbind(youngadult) %>% rbind(adult)

age$variable <- factor(age_vector, order = TRUE, levels = c(age_vector))

age <- mutate(age, percent = (estimate/summary_est) * 100)

age2012 <- cbind(age, year = "2012")

##### Age 2011

age <- rapp_all(age_var, pop_total, 2011) %>%
  subset(select = -c(NAME.y)) %>%
  rename(NAME = NAME.x)

adult <- filter(age, variable == "age30_34" | variable == "age35_39" | variable == "age40_44" | variable == "age45_49" | variable == "age50_54" | variable == "age55_59" | variable == "age60_64") %>%
  summarise(STATEFP, COUNTYFP, GEOID, NAME, LSAD, ALAND, AWATER, variable = "age30_64", estimate = sum(estimate), moe = sum(moe), summary_est, summary_moe, percent = sum(percent)) %>%
  slice(1)  

youngadult <- filter(age, variable == "age18_24" | variable == "age25_29") %>% 
  summarise(STATEFP, COUNTYFP, GEOID, NAME, LSAD, ALAND, AWATER, variable = "age18_29", estimate = sum(estimate), moe = sum(moe), summary_est, summary_moe, percent = sum(percent)) %>%
  slice(1)  

age <- age %>% filter(variable != "age18_24" & variable != "age25_29" & variable != "age30_34" & variable != "age35_39" & variable != "age40_44" & variable != "age45_49" & variable != "age50_54" & variable != "age55_59" & variable != "age60_64")

age <- age %>% rbind(youngadult) %>% rbind(adult)

age$variable <- factor(age_vector, order = TRUE, levels = c(age_vector))

age <- mutate(age, percent = (estimate/summary_est) * 100)

age2011 <- cbind(age, year = "2011")

##### Age 2010

age <- rapp_all(age_var, pop_total, 2010) %>%
  subset(select = -c(NAME.y)) %>%
  rename(NAME = NAME.x)

adult <- filter(age, variable == "age30_34" | variable == "age35_39" | variable == "age40_44" | variable == "age45_49" | variable == "age50_54" | variable == "age55_59" | variable == "age60_64") %>%
  summarise(STATEFP, COUNTYFP, GEOID, NAME, LSAD, ALAND, AWATER, variable = "age30_64", estimate = sum(estimate), moe = sum(moe), summary_est, summary_moe, percent = sum(percent)) %>%
  slice(1)  

youngadult <- filter(age, variable == "age18_24" | variable == "age25_29") %>% 
  summarise(STATEFP, COUNTYFP, GEOID, NAME, LSAD, ALAND, AWATER, variable = "age18_29", estimate = sum(estimate), moe = sum(moe), summary_est, summary_moe, percent = sum(percent)) %>%
  slice(1)  

age <- age %>% filter(variable != "age18_24" & variable != "age25_29" & variable != "age30_34" & variable != "age35_39" & variable != "age40_44" & variable != "age45_49" & variable != "age50_54" & variable != "age55_59" & variable != "age60_64")

age <- age %>% rbind(youngadult) %>% rbind(adult)

age$variable <- factor(age_vector, order = TRUE, levels = c(age_vector))

age <- mutate(age, percent = (estimate/summary_est) * 100)

age2010 <- cbind(age, year = "2010")

#############################################################################


age2017_2019 <- age2019 %>% rbind(age2018) %>% rbind(age2017)
age2016_2013 <- age2016 %>% rbind(age2015) %>% rbind(age2014) %>% rbind(age2013) 



ggplot(age2019) +
    geom_sf(aes(fill = percent)) +
   facet_wrap(~variable)






ggplot(age2017_2019, aes(x = year, y = percent, group = variable, color = variable)) +
                            geom_line(aes(size = "estimate")) +
  ylim(0,100)



ggplot(age2016_2013, aes(x = year, y = estimate, group = variable, color = variable)) +
  geom_line() +
  plot_theme +
  ylim(0,100)












