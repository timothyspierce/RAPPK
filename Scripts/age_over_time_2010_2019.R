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

library(ggtext)
library(ggpubr)
library(cowplot)
library(ggthemes)
devtools::install_github("rCarto/osrm")
options(tigris_use_cache = TRUE)
options(tigris_class="sf")

#####################################################################

#Rappk Coordinates, Latitude = 38.6762° Longitude = -78.1564

#Generalized vector to get the total population for any function
pop_total <- c(poptotal = "B02001_001")

########## Necessary functions for working all variables ############
plot_theme <- theme(plot.title = element_text(hjust = 0.5),
                    axis.text=element_text(size=12),
                    legend.text = element_text(size=12),
                    axis.title.x=element_text(size =13),
                    axis.title.y=element_text(size =13),
                    panel.background = element_blank())



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
          cache = TRUE,
          output = "wide")}

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
          cache = TRUE,
          output = "wide") }

########################################################

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

age_var_pre2017 <- c(under5 = "S0101_C01_002",
                     age5_14 = "S0101_C01_020",
                     age15_17 = "S0101_C01_021",
                     age18_24 = "S0101_C01_022",
                     age25_29 = "S0101_C01_007",
                     age30_34 = "S0101_C01_008",
                     age35_39 = "S0101_C01_009",
                     age40_44 = "S0101_C01_010",
                     age45_49 = "S0101_C01_011",
                     age50_54 = "S0101_C01_012",
                     age55_59 = "S0101_C01_013",
                     age60_64 = "S0101_C01_014",
                     age65_older = "S0101_C01_028")
  
age_vector <- c("under18",
                "age18_29",
                "age30_64",
                "age65_older")

pop_total <- c(pop_total = "S0101_C01_001")


rappage2019 <- rapp_all(age_var, pop_total, 2019) %>% 
  subset(select = c(GEOID, NAME.x, under18E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2019")

rappage2018 <- rapp_all(age_var, pop_total, 2018) %>% 
  subset(select = c(GEOID, NAME.x, under18E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2018")

rappage2017 <- rapp_all(age_var, pop_total, 2017) %>% 
  subset(select = c(GEOID, NAME.x, under18E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2017")

rappage2016 <- rapp_all(age_var_pre2017, pop_total, 2016) %>% 
  subset(select = c(GEOID, NAME.x, under5E, age5_14E, age15_17E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2016")

rappage2015 <- rapp_all(age_var_pre2017, pop_total, 2015) %>% 
  subset(select = c(GEOID, NAME.x, under5E, age5_14E, age15_17E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2015")

rappage2014 <- rapp_all(age_var_pre2017, pop_total, 2014) %>% 
  subset(select = c(GEOID, NAME.x, under5E, age5_14E, age15_17E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2014") %>% 
  st_zm(drop=TRUE, what = "ZM")

rappage2013 <- rapp_all(age_var_pre2017, pop_total, 2013) %>% 
  subset(select = c(GEOID, NAME.x, under5E, age5_14E, age15_17E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2013")

rappage2012 <- rapp_all(age_var_pre2017, pop_total, 2012) %>% 
  subset(select = c(GEOID, NAME.x, under5E, age5_14E, age15_17E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2012")

rappage2011 <- rapp_all(age_var_pre2017, pop_total, 2011) %>% 
  subset(select = c(GEOID, NAME.x, under5E, age5_14E, age15_17E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2011")

rappage2010 <- rapp_all(age_var_pre2017, pop_total, 2010) %>% 
  subset(select = c(GEOID, NAME.x, under5E, age5_14E, age15_17E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2010")


rappage2017_2019 <- rappage2019 %>% rbind(rappage2018) %>% rbind(rappage2017)
rappageage2017_2019combined <- rappage2017_2019 %>%
  mutate(under18 = rappage2017_2019$under18E) %>%
  mutate(age18_29 = (rappage2017_2019$age18_24E + rappage2017_2019$age25_29E)) %>% 
  mutate(age30_64 = (rappage2017_2019$age30_34E + rappage2017_2019$age35_39E + rappage2017_2019$age40_44E + rappage2017_2019$age45_49E + rappage2017_2019$age50_54E + rappage2017_2019$age55_59E + rappage2017_2019$age60_64E)) %>%
  mutate(age65_older = rappage2017_2019$age65_olderE) %>%
  subset(select = -c(under18E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE))  %>%
  rename(NAME = NAME.x) %>%
  pivot_longer(cols = c(under18, age18_29, age30_64, age65_older), names_to = "ages", values_to = "estimate")

rappageage2017_2019 <- mutate(rappageage2017_2019combined, percent = (rappageage2017_2019combined$estimate/rappageage2017_2019combined$summary_est)*100)





rappage2010_2016 <- rappage2016 %>% rbind(rappage2015) %>% rbind(rappage2014) %>% rbind(rappage2013) %>% rbind(rappage2012) %>% rbind(rappage2011) %>% rbind(rappage2010)

rappageage2010_2016combined <- rappage2010_2016 %>%
  mutate(under18 = (rappage2010_2016$under5E + rappage2010_2016$age5_14E + rappage2010_2016$age15_17E)) %>%
  mutate(age18_29 = (rappage2010_2016$age18_24E + rappage2010_2016$age25_29E)) %>% 
  mutate(age30_64 = (rappage2010_2016$age30_34E + rappage2010_2016$age35_39E + rappage2010_2016$age40_44E + rappage2010_2016$age45_49E + rappage2010_2016$age50_54E + rappage2010_2016$age55_59E + rappage2010_2016$age60_64E)) %>%
  mutate(age65_older = rappage2010_2016$age65_olderE) %>%
  subset(select = -c(under5E, age5_14E, age15_17E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE))  %>%
  rename(NAME = NAME.x) %>%
  pivot_longer(cols = c(under18, age18_29, age30_64, age65_older), names_to = "ages", values_to = "percent")

rappageage2010_2016 <- mutate(rappageage2010_2016combined, estimate = (rappageage2010_2016combined$percent/100) * rappageage2010_2016combined$summary_est)

rappage_timeseries <- rappageage2017_2019 %>% rbind(rappageage2010_2016) 

rappage_timeseries$ages <- factor(rappage_timeseries$ages, levels = age_vector)

saveRDS(rappage_timeseries, "shiny_app/data/rapp_age_time_series.Rda")

ggplot(rappage_timeseries, aes(x = year, y = percent, group = ages, color = ages)) +
  geom_line(aes(size = estimate)) +
  labs(title = "Age of Population from 2010 to 2019", color = "Age Categories", size = "Total Population") +
  xlab("Years") +
  ylab("Percent of the Population") +
  scale_color_viridis_d(
    labels = c("under18" = "Under 18", 
               "age18_29" = "18 to 29", 
               "age30_64" = "30 to 64", 
               "age65_older" = "65 and Older")) +
  plot_theme
  
  
  
  
  
  #####################################################################################

age2019 <- rapp_var(age_var, pop_total, 2019) %>% 
  subset(select = c(GEOID, NAME.x, under18E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2019")
age2018 <- rapp_var(age_var, pop_total, 2018) %>% 
  subset(select = c(GEOID, NAME.x, under18E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2018")
age2017 <- rapp_var(age_var, pop_total, 2017) %>% 
  subset(select = c(GEOID, NAME.x, under18E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2017")

age2017_2019 <- age2019 %>% rbind(age2018) %>% rbind(age2017)


age2017_2019combined <- age2017_2019 %>%
  mutate(under18 = age2017_2019$under18E) %>%
  mutate(age18_29 = (age2017_2019$age18_24E + age2017_2019$age25_29E)) %>% 
  mutate(age30_64 = (age2017_2019$age30_34E + age2017_2019$age35_39E + age2017_2019$age40_44E + age2017_2019$age45_49E + age2017_2019$age50_54E + age2017_2019$age55_59E + age2017_2019$age60_64E)) %>%
  mutate(age65_older = age2017_2019$age65_olderE) %>%
  subset(select = -c(under18E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE))  %>%
  rename(NAME = NAME.x) %>%
  pivot_longer(cols = c(under18, age18_29, age30_64, age65_older), names_to = "ages", values_to = "estimate")


age2017_2019 <- age2017_2019combined %>% mutate(percent = (age2017_2019combined$estimate/age2017_2019combined$summary_est)*100)



age2017_2019$ages <- factor(age2017_2019$ages, levels = age_vector)



age2016 <- rapp_var(age_var_pre2017, pop_total, 2016) %>% 
  subset(select = c(GEOID, NAME.x, under5E, age5_14E, age15_17E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2016")
age2015 <- rapp_var(age_var_pre2017, pop_total, 2015) %>% 
  subset(select = c(GEOID, NAME.x, under5E, age5_14E, age15_17E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2015")
age2014 <- rapp_var(age_var_pre2017, pop_total, 2014) %>% 
  st_zm(drop = TRUE, what = "ZM") %>%
  subset(select = c(GEOID, NAME.x, under5E, age5_14E, age15_17E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2014")
age2013 <- rapp_var(age_var_pre2017, pop_total, 2013) %>% 
  subset(select = c(GEOID, NAME.x, under5E, age5_14E, age15_17E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2013")
age2012 <- rapp_var(age_var_pre2017, pop_total, 2012) %>% 
  subset(select = c(GEOID, NAME.x, under5E, age5_14E, age15_17E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2012")
age2011 <- rapp_var(age_var_pre2017, pop_total, 2011) %>% 
  subset(select = c(GEOID, NAME.x, under5E, age5_14E, age15_17E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2011")
age2010 <- rapp_var(age_var_pre2017, pop_total, 2010) %>% 
  subset(select = c(GEOID, NAME.x, under5E, age5_14E, age15_17E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2010")

age2010_2016 <- age2016 %>% 
  rbind(age2015) %>% 
  rbind(age2014) %>% 
  rbind(age2013) %>% 
  rbind(age2012) %>% 
  rbind(age2011) %>% 
  rbind(age2010)


age2010_2016combined <- age2010_2016 %>%
  mutate(under18 = (age2010_2016$under5E + age2010_2016$age5_14E + age2010_2016$age15_17E)) %>%
  mutate(age18_29 = (age2010_2016$age18_24E + age2010_2016$age25_29E)) %>% 
  mutate(age30_64 = (age2010_2016$age30_34E + age2010_2016$age35_39E + age2010_2016$age40_44E + age2010_2016$age45_49E + age2010_2016$age50_54E + age2010_2016$age55_59E + age2010_2016$age60_64E)) %>%
  mutate(age65_older = age2010_2016$age65_olderE) %>%
  subset(select = -c(under5E, age5_14E, age15_17E, age18_24E, age25_29E, age30_34E, age35_39E, age40_44E, age45_49E, age50_54E, age55_59E, age60_64E, age65_olderE))  %>%
  rename(NAME = NAME.x)


age2010_2016 <- age2010_2016combined %>% 
  pivot_longer(cols = c(under18, age18_29, age30_64, age65_older), names_to = "ages", values_to = "percent")

age2010_2016 <- age2010_2016 %>% mutate(estimate = (age2010_2016$percent/100)*age2010_2016$summary_est)


age2010_2016$ages <- factor(age2010_2016$ages, levels = age_vector)




agetimeseries <- age2017_2019 %>% rbind(age2010_2016) %>% st_as_sf()


ggplot(agetimeseries, aes(x = year, y = percent, color = ages, group = ages)) +
  geom_line(aes(size = estimate)) +
  labs(title = "Age of Population by District from 2010-2019", color = "Age Categories", size = "Total Population") +
  xlab("Years") +
  ylab("Percentage of Population") +
  scale_color_viridis_d(
    labels = c("under18" = "Under 18", 
               "age18_29" = "18 to 29", 
               "age30_64" = "30 to 64", 
               "age65_older" = "65 and Older")) +
  facet_wrap(~NAME) +
  plot_theme

ggplot(agetimeseries, aes(x = year, y = estimate, color = ages, group = ages)) +
  geom_line(aes(size = percent)) +
  labs(title = "Age of Population by District from 2010-2019", color = "Age Categories", size = "Percent of Population") +
  xlab("Years") +
  ylab("Total Population") +
  scale_color_viridis_d(
    labels = c("under18" = "Under 18", 
               "age18_29" = "18 to 29", 
               "age30_64" = "30 to 64", 
               "age65_older" = "65 and Older")) +
  facet_wrap(~NAME) +
  plot_theme



# 
# tm_shape(agetimeseries) +
#   tm_border() +
#   tm_fill("percent") 