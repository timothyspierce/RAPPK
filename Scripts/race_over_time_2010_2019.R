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

rapp_table <- function(varcode, year){
  get_acs(geography = "county subdivision",
          state = 51,
          county = 157,
          table = varcode,
          output = "wide",
          year = year)}

get_race <- function(year){
  rapp_table("B02001", 2019) -> race_table
  race_table <- mutate(race_table, 'White' = (B02001_002E / B02001_001E))
  race_table <- mutate(race_table, 'non-White' = (B02001_003E + B02001_004E +B02001_005E+B02001_006E+B02001_007E+B02001_008E)/B02001_001E)  %>%  
  select(NAME, White, 'non-White') %>% 
  pivot_longer(cols = c("White", "non-White"),
               names_to = "Race",
               values_to = "Percent") %>% 
    mutate("Year" = year)
}

race <- rbind(
  get_race(2019),
  get_race(2018),
  get_race(2017),
  get_race(2016),
  get_race(2015),
  get_race(2014),
  get_race(2013),
  get_race(2012),
  get_race(2011),
  get_race(2010))

ggplot(race, aes(x = Year, y = Percent, group = NAME, fill = Race)) + geom_col()

edu2019 <- edu2019 %>% mutate(NAME = str_sub(NAME, end = -32 ))
edu2019$EduLevel <- factor(edu2019$EduLevel, c("Above Bachelors", "Bachelors Degree", "Some College", "HS Diploma or GED", "Less Than High School"))
edu2019 <- edu2019 %>% mutate(Percent = Percent*100)
saveRDS(edu2019, "shiny_app/data/edu2019.Rds")
readRDS("shiny_app/data/edu2019.Rds")
ggplot(edu2019, aes(x = NAME, y = Percent, group = EduLevel, fill = EduLevel)) + 
  geom_col() + scale_fill_viridis_d() +
  ggtitle("Education Levels by District")  + xlab("District")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        axis.title.x=element_text(size =13),
        axis.title.y=element_text(size =13),
        panel.background = element_blank()) 




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

race_time_series <- race_time_series %>% filter(race != "White")

View(race_time_series)

saveRDS(race_time_series, file = "shiny_app/data/race_time_series.Rds")

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
race_district <- race_district %>% filter(race != "White")

View(race_district)

saveRDS(race_district, file = "shiny_app/data/race_district.Rds")

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





