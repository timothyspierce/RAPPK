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


plot_theme <- theme(plot.title = element_text(hjust = 0.5),
                    axis.text=element_text(size=12),
                    legend.text = element_text(size=12),
                    axis.title.x=element_text(size =13),
                    axis.title.y=element_text(size =13),
                    panel.background = element_blank())



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


##############################################################################



race_var <- c(white = "B02001_002",
               black = "B02001_003",
               first_nations = "B02001_004",
               asian = "B02001_005",
               oceania = "B02001_006",
               other = "B02001_007",
               mixed_total = "B02001_008")
pop_total <- c(poptotal = "B02001_001")


#### For Rappahannock in total. By district is the next section


race2019 <- rapp_all(race_var, pop_total, 2019) %>% 
  subset(select = c(GEOID, NAME.x, whiteE, blackE, first_nationsE, asianE, oceaniaE, otherE, mixed_totalE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2019")

race2018 <- rapp_all(race_var, pop_total, 2018) %>% 
  subset(select = c(GEOID, NAME.x, whiteE, blackE, first_nationsE, asianE, oceaniaE, otherE, mixed_totalE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2018")

race2017 <- rapp_all(race_var, pop_total, 2017) %>% 
  subset(select = c(GEOID, NAME.x, whiteE, blackE, first_nationsE, asianE, oceaniaE, otherE, mixed_totalE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2017")

race2016 <- rapp_all(race_var, pop_total, 2016) %>% 
  subset(select = c(GEOID, NAME.x, whiteE, blackE, first_nationsE, asianE, oceaniaE, otherE, mixed_totalE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2016")

race2015 <- rapp_all(race_var, pop_total, 2015) %>% 
  subset(select = c(GEOID, NAME.x, whiteE, blackE, first_nationsE, asianE, oceaniaE, otherE, mixed_totalE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2015")

race2014 <- rapp_all(race_var, pop_total, 2014) %>% 
  subset(select = c(GEOID, NAME.x, whiteE, blackE, first_nationsE, asianE, oceaniaE, otherE, mixed_totalE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2014") %>% 
  st_zm(drop=TRUE, what = "ZM")

race2013 <- rapp_all(race_var, pop_total, 2013) %>% 
  subset(select = c(GEOID, NAME.x, whiteE, blackE, first_nationsE, asianE, oceaniaE, otherE, mixed_totalE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2013")

race2012 <- rapp_all(race_var, pop_total, 2012) %>% 
  subset(select = c(GEOID, NAME.x, whiteE, blackE, first_nationsE, asianE, oceaniaE, otherE, mixed_totalE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2012")

race2011 <- rapp_all(race_var, pop_total, 2011) %>% 
  subset(select = c(GEOID, NAME.x, whiteE, blackE, first_nationsE, asianE, oceaniaE, otherE, mixed_totalE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2011")

race2010 <- rapp_all(race_var, pop_total, 2010) %>% 
  subset(select = c(GEOID, NAME.x, whiteE, blackE, first_nationsE, asianE, oceaniaE, otherE, mixed_totalE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2010")

race_time_series <- race2019 %>%
  rbind(race2018) %>%
  rbind(race2017) %>%
  rbind(race2016) %>%
  rbind(race2015) %>%
  rbind(race2014) %>%
  rbind(race2013) %>%
  rbind(race2013) %>%
  rbind(race2012) %>%
  rbind(race2011) %>%
  rbind(race2010) %>%
  rename(NAME = NAME.x) %>%
  rename(White = whiteE, Black =  blackE, "First Nations" = first_nationsE, Asian = asianE, "Pacific Islander" = oceaniaE, Other = otherE, Mixed = mixed_totalE) %>%
  pivot_longer(cols = c(White, Black, "First Nations", Asian, "Pacific Islander", Other, Mixed), names_to = "race", values_to = "estimate")

race_time_series <-  race_time_series %>% mutate(Percent = ( (race_time_series$estimate/race_time_series$summary_est) * 100))


ggplot(race_time_series, aes(x = year, y = estimate, color = race, group = race)) +
  geom_line(aes(size = Percent)) +
  labs(title = "Racial Demographics 2010-2019", color = "Race", size = "Percent of Population") +
  xlab("Years") +
  ylab("Total Population") +
  scale_color_viridis_d() +
  plot_theme

ggplot(race_time_series, aes(x = year, y = Percent, color = race, group = race)) +
  geom_line(aes(size = estimate)) +
  labs(title = "Racial Demographics 2010-2019", color = "Race", size = "Total Population") +
  xlab("Years") +
  ylab("Percent of Population") +
  scale_color_viridis_d() +
  plot_theme


ggplot(race_time_series, aes(x = year, y = Percent, fill = race, group = race)) +
  geom_col(position = "fill") +
  labs(title = "Racial Demographics 2010-2019", fill = "Race") +
  xlab("Years") +
  ylab("Percent of Population") +
  scale_fill_viridis_d() +
  plot_theme 
  


ggplot(race_time_series, aes(x = year, y = Percent, fill = race, group = race)) +
  geom_col(position = "dodge") +
  labs(title = "Racial Demographics 2010-2019", fill = "Race") +
  xlab("Years") +
  ylab("Percent of Population") +
  scale_fill_viridis_d() +
  plot_theme 


ggplot(race_time_series, aes(x = year, y = estimate, fill = race, group = race)) +
  geom_col(position = "fill") +
  labs(title = "Racial Demographics 2010-2019", fill = "Race") +
  xlab("Years") +
  ylab("Percent of Population") +
  scale_fill_viridis_d() +
  plot_theme 


ggplot(race_time_series, aes(x = year, y = estimate, fill = race, group = race)) +
  geom_col(position = "dodge") +
  labs(title = "Racial Demographics 2010-2019", fill = "Race") +
  xlab("Years") +
  ylab("Total Population") +
  scale_fill_viridis_d() +
  plot_theme 





#################### By District


racedistrict2019 <- rapp_var(race_var, pop_total, 2019) %>% 
  subset(select = c(GEOID, NAME.x, whiteE, blackE, first_nationsE, asianE, oceaniaE, otherE, mixed_totalE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2019")

racedistrict2018 <- rapp_var(race_var, pop_total, 2018) %>% 
  subset(select = c(GEOID, NAME.x, whiteE, blackE, first_nationsE, asianE, oceaniaE, otherE, mixed_totalE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2018")

racedistrict2017 <- rapp_var(race_var, pop_total, 2017) %>% 
  subset(select = c(GEOID, NAME.x, whiteE, blackE, first_nationsE, asianE, oceaniaE, otherE, mixed_totalE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2017")

racedistrict2016 <- rapp_var(race_var, pop_total, 2016) %>% 
  subset(select = c(GEOID, NAME.x, whiteE, blackE, first_nationsE, asianE, oceaniaE, otherE, mixed_totalE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2016")

racedistrict2015 <- rapp_var(race_var, pop_total, 2015) %>% 
  subset(select = c(GEOID, NAME.x, whiteE, blackE, first_nationsE, asianE, oceaniaE, otherE, mixed_totalE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2015")

racedistrict2014 <- rapp_var(race_var, pop_total, 2014) %>% 
  subset(select = c(GEOID, NAME.x, whiteE, blackE, first_nationsE, asianE, oceaniaE, otherE, mixed_totalE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2014") %>% 
  st_zm(drop=TRUE, what = "ZM")

racedistrict2013 <- rapp_var(race_var, pop_total, 2013) %>% 
  subset(select = c(GEOID, NAME.x, whiteE, blackE, first_nationsE, asianE, oceaniaE, otherE, mixed_totalE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2013")

racedistrict2012 <- rapp_var(race_var, pop_total, 2012) %>% 
  subset(select = c(GEOID, NAME.x, whiteE, blackE, first_nationsE, asianE, oceaniaE, otherE, mixed_totalE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2012")

racedistrict2011 <- rapp_var(race_var, pop_total, 2011) %>% 
  subset(select = c(GEOID, NAME.x, whiteE, blackE, first_nationsE, asianE, oceaniaE, otherE, mixed_totalE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2011")

racedistrict2010 <- rapp_var(race_var, pop_total, 2010) %>% 
  subset(select = c(GEOID, NAME.x, whiteE, blackE, first_nationsE, asianE, oceaniaE, otherE, mixed_totalE, summary_est, summary_moe, geometry)) %>%
  add_column(year = "2010")

race_district <- racedistrict2019 %>%
  rbind(racedistrict2018) %>%
  rbind(racedistrict2017) %>%
  rbind(racedistrict2016) %>%
  rbind(racedistrict2015) %>%
  rbind(racedistrict2014) %>%
  rbind(racedistrict2013) %>%
  rbind(racedistrict2013) %>%
  rbind(racedistrict2012) %>%
  rbind(racedistrict2011) %>%
  rbind(racedistrict2010) %>%
  rename(NAME = NAME.x) %>%
  rename(White = whiteE, Black =  blackE, "First Nations" = first_nationsE, Asian = asianE, "Pacific Islander" = oceaniaE, Other = otherE, Mixed = mixed_totalE) %>%
  pivot_longer(cols = c(White, Black, "First Nations", Asian, "Pacific Islander", Other, Mixed), names_to = "race", values_to = "estimate")
race_district <-  race_district %>% mutate(Percent = ( (race_district$estimate/race_district$summary_est) * 100))

race_district %>% filter(year == "2013")


ggplot(race_district, aes(x = year, y = estimate, color = race, group = race)) +
  geom_line(aes(size = Percent)) +
  labs(title = "Racial Demographics 2010-2019", color = "Race", size = "Percent of Population") +
  xlab("Years") +
  ylab("Total Population") +
  scale_color_viridis_d() +
  plot_theme +
  facet_wrap(~NAME)



ggplot(race_district, aes(x = year, y = Percent, color = race, group = race)) +
  geom_line(aes(size = estimate)) +
  labs(title = "Racial Demographics 2010-2019", color = "Race", size = "Total Population") +
  xlab("Years") +
  ylab("Percent of Population") +
  scale_color_viridis_d() +
  plot_theme +
  facet_wrap(~NAME)


ggplot(race_district, aes(x = year, y = Percent, fill = race, group = race)) +
  geom_col(position = "fill") +
  labs(title = "Racial Demographics 2010-2019", fill = "Race") +
  xlab("Years") +
  ylab("Percent of Population") +
  scale_fill_viridis_d() +
  plot_theme +
  facet_wrap(~NAME)

ggplot(race_district, aes(x = year, y = Percent, fill = race, group = race)) +
  geom_col(position = "dodge") +
  labs(title = "Racial Demographics 2010-2019", fill = "Race") +
  xlab("Years") +
  ylab("Percent of Population") +
  scale_fill_viridis_d() +
  plot_theme +
  facet_wrap(~NAME)


ggplot(race_district, aes(x = year, y = estimate, fill = race, group = race)) +
  geom_col(position = "fill") +
  labs(title = "Racial Demographics 2010-2019", fill = "Race") +
  xlab("Years") +
  ylab("Percent of Population") +
  scale_fill_viridis_d() +
  plot_theme +
  facet_wrap(~NAME)

ggplot(race_district, aes(x = year, y = estimate, fill = race, group = race)) +
  geom_col(position = "dodge") +
  labs(title = "Racial Demographics 2010-2019", fill = "Race") +
  xlab("Years") +
  ylab("Total Population") +
  scale_fill_viridis_d() +
  plot_theme +
  facet_wrap(~NAME)












race_demo <- rapp_all(race_vars, pop_total) %>%
  add_row(rapp_var(race_vars, pop_total)) %>%
  add_row(rapp_all(race_vars, pop_total, year = 2018)) %>%
  add_row(rapp_var(race_vars, pop_total, year = 2018)) %>%
  add_row(rapp_all(race_vars, pop_total, year = 2017)) %>%
  add_row(rapp_var(race_vars, pop_total, year = 2017)) %>%
  add_row(rapp_all(race_vars, pop_total, year = 2016)) %>%
  add_row(rapp_var(race_vars, pop_total, year = 2016)) %>%
  add_row(rapp_all(race_vars, pop_total, year = 2015)) %>%
  add_row(rapp_var(race_vars, pop_total, year = 2015)) %>%
  subset(select = -c(STATEFP, COUNTYFP, AFFGEOID, LSAD, NAEE.y, ALAND, AWATER, percent)) %>%
  rename(NAEE = NAME.x) %>%
  mutate(percent = (estimate/summary_est)*100) %>%
  add_column(year = c("2019", "2019", "2019", "2019", "2019", "2019", "2019",
                      "2019", "2019", "2019", "2019", "2019", "2019", "2019",
                      "2019", "2019", "2019", "2019", "2019", "2019", "2019",
                      "2019", "2019", "2019", "2019", "2019", "2019", "2019",
                      "2019", "2019", "2019", "2019", "2019", "2019", "2019",
                      "2019", "2019", "2019", "2019", "2019", "2019", "2019",
                      "2018", "2018", "2018", "2018", "2018", "2018", "2018",
                      "2018", "2018", "2018", "2018", "2018", "2018", "2018",
                      "2018", "2018", "2018", "2018", "2018", "2018", "2018",
                      "2018", "2018", "2018", "2018", "2018", "2018", "2018",
                      "2018", "2018", "2018", "2018", "2018", "2018", "2018",
                      "2018", "2018", "2018", "2018", "2018", "2018", "2018",
                      "2017", "2017", "2017", "2017", "2017", "2017", "2017",
                      "2017", "2017", "2017", "2017", "2017", "2017", "2017",
                      "2017", "2017", "2017", "2017", "2017", "2017", "2017",
                      "2017", "2017", "2017", "2017", "2017", "2017", "2017",
                      "2017", "2017", "2017", "2017", "2017", "2017", "2017",
                      "2017", "2017", "2017", "2017", "2017", "2017", "2017",
                      "2016", "2016", "2016", "2016", "2016", "2016", "2016",
                      "2016", "2016", "2016", "2016", "2016", "2016", "2016",
                      "2016", "2016", "2016", "2016", "2016", "2016", "2016",
                      "2016", "2016", "2016", "2016", "2016", "2016", "2016",
                      "2016", "2016", "2016", "2016", "2016", "2016", "2016",
                      "2016", "2016", "2016", "2016", "2016", "2016", "2016",
                      "2015", "2015", "2015", "2015", "2015", "2015", "2015",
                      "2015", "2015", "2015", "2015", "2015", "2015", "2015",
                      "2015", "2015", "2015", "2015", "2015", "2015", "2015",
                      "2015", "2015", "2015", "2015", "2015", "2015", "2015",
                      "2015", "2015", "2015", "2015", "2015", "2015", "2015",
                      "2015", "2015", "2015", "2015", "2015", "2015", "2015"))


#Well, it shows that the majority of Rappahannock is white.
#So white that it'd be more statistically significant to put everyone else in "other"
#And that's just sorta insulting
ggplot(race_demo, aes(x = NAEE, y = percent, fill = variable)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~year) +
  plot_theme

ggplot(race_demo, aes(x = year, y = percent, color = variable, group = variable)) +
  geom_line(aes(size=estimate)) +
  facet_wrap(~NAEE)  +
  plot_theme




