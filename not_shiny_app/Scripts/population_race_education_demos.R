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


#Just gonna slap this at the end of every ggplot for consistency
plot_theme <- theme(plot.title = element_text(hjust = 0.5),
                    axis.text=element_text(size=12),
                    legend.text = element_text(size=12),
                    axis.title.x=element_text(size =13),
                    axis.title.y=element_text(size =13)) 

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


population2015_2019 <- rapp_all(pop_total, pop_total) %>%
  add_row(rapp_var(pop_total, pop_total)) %>%
  add_row(rapp_all(pop_total, pop_total, year = 2018)) %>%
  add_row(rapp_var(pop_total, pop_total, year = 2018)) %>%
  add_row(rapp_all(pop_total, pop_total, year = 2017)) %>%
  add_row(rapp_var(pop_total, pop_total, year = 2017)) %>%
  add_row(rapp_all(pop_total, pop_total, year = 2016)) %>%
  add_row(rapp_var(pop_total, pop_total, year = 2016)) %>%
  add_row(rapp_all(pop_total, pop_total, year = 2015)) %>%
  add_row(rapp_var(pop_total, pop_total, year = 2015)) %>%
  subset(select = -c(STATEFP, COUNTYFP, AFFGEOID, LSAD, ALAND, AWATER)) %>%
  subset(select = -c(NAME.y)) %>%
  rename(NAME = NAME.x) %>%
  add_column(year = c("2019", "2019", "2019", "2019", "2019", "2019",
                      "2018", "2018", "2018", "2018", "2018", "2018",
                      "2017", "2017", "2017", "2017", "2017", "2017",
                      "2016", "2016", "2016", "2016", "2016", "2016",
                      "2015", "2015", "2015", "2015", "2015", "2015"))

 population2013_2014 <- rapp_all(pop_total, pop_total, year = 2014) %>%
   add_row(rapp_var(pop_total, pop_total, year = 2014)) %>%
   st_zm(drop = TRUE, what = "ZM") %>%
   add_row(rapp_all(pop_total, pop_total, year = 2013)) %>%
   add_row(rapp_var(pop_total, pop_total, year = 2013)) %>%
 subset(select = c(GEOID, NAME.x, variable, estimate, moe, summary_est, summary_moe, percent, geometry)) %>%
   rename(NAME = NAME.x) %>%
   add_column(year = c("2014", "2014", "2014", "2014", "2014", "2014",
                       "2013", "2013", "2013", "2013", "2013", "2013"))
 
 
   population2011_2012 <- rapp_all(pop_total, pop_total, year = 2012) %>%
   add_row(rapp_var(pop_total, pop_total, year = 2012) %>% subset(select = -c(CNECTAFP, NECTAFP, NCTADVFP))) %>%
   add_row(rapp_all(pop_total, pop_total, year = 2011)) %>%
   add_row(rapp_var(pop_total, pop_total, year = 2011) %>% subset(select = -c(CNECTAFP, NECTAFP, NCTADVFP))) %>%
   subset(select = c(GEOID, NAME.x, variable, estimate, moe, summary_est, summary_moe, percent, geometry)) %>%
     rename(NAME = NAME.x) %>%
     add_column(year = c("2012", "2012", "2012", "2012", "2012", "2012",
                         "2011", "2011", "2011", "2011", "2011", "2011"))
   
   
population2010 <-   get_acs(geography = "county",
                            state = 51,
                            county = 157,
                            variables = pop_total,
                            summary_var = pop_total,
                            year = 2010,
                            geometry = TRUE,
                            keep_geo_vars = TRUE,
                            cache = TRUE) %>%
  mutate(percent = (estimate/sum(summary_est))*100) %>%
  subset(select = c(GEOID, NAME.x, variable, estimate, moe, summary_est, summary_moe, percent, geometry)) %>%
   add_row( get_acs(geography = "county subdivision",
                    state = 51,
                    county = 157,
                    variables = pop_total,
                    summary_var = pop_total,
                    year = 2010,
                    geometry = TRUE,
                    keep_geo_vars = TRUE,
                    cache = TRUE) %>%
              mutate(percent = (estimate/sum(summary_est))*100) %>%
              subset(select = c(GEOID, NAME.x, variable, estimate, moe, summary_est, summary_moe, percent, geometry))) %>%
   
   rename(NAME = NAME.x) %>%
   add_column(year = c(
                       "2010", "2010", "2010", "2010", "2010", "2010"))
 

population2010_2019 <- population2015_2019 %>%
  rbind(population2013_2014) %>%
  rbind(population2011_2012) %>%
  rbind(population2010)
  
population2010_2019 #<- filter(population2010_2019, NAME != "Rappahannock") 
 

ggplot(population2010_2019, aes(x = year, y = percent, group = NAME, color = NAME)) +
  geom_line()
  
  
ggplot(population2010_2019, aes(x = year, y = estimate, group = NAME, color = NAME)) +
  geom_line(aes(size = "Percent of Population" <- percent)) +
  ggtitle(label = "Estimated Total Population 2010-2019")

ggplot(population2010_2019 %>% filter(NAME != "Rappahannock"), aes(x = year, y = estimate, group = NAME, color = NAME)) +
  geom_line(aes(size = "Percent of Population" <- percent)) +
  ggtitle(label = "Estimated Total Population 2010-2019")



ggplot(population2010_2019 %>% filter(NAME != "Rappahannock"), aes(x = year, y = estimate, group = NAME, color = NAME)) +
  geom_line(aes(size = "Percent of Population" <- percent)) +
  ggtitle(label = "Estimated Total Population 2010-2019")

  ggplot(population2010_2019 %>% filter(NAME != "Rappahannock"), aes(x = year, y = percent, group = NAME, color = NAME)) +
           geom_line(aes(size = "Percent of Population" <- estimate)) +
           ggtitle(label = "Estimated Total Population 2010-2019")

population_districts <- population2010_2019 %>% filter(NAME != "Rappahannock")
tm_shape(population_districts) +
  tm_borders() +
  tm_fill("percent") +
  tm_facets(by = "year") +
  tm_text("NAME") +
  tm_layout(title = "Estimated percentage of Population 2010-2019")

ggplot(population2010_2019 %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = percent)) +
  coord_sf(datum = NA) +
  facet_wrap(~year) +
  plot_theme +
  ggtitle(label = "Estimated Percentage of Population by district 2010-2019")

# 
# ##############################################################################
# race_vars <- c(white = "B02001_002",
#                black = "B02001_003",
#                first_nations = "B02001_004",
#                asian = "B02001_005",
#                oceania = "B02001_006",
#                other = "B02001_007",
#                mixed_total = "B02001_008")
# 
# 
# race_demo <- rapp_all(race_vars, pop_total) %>%
#   add_row(rapp_var(race_vars, pop_total)) %>%
#   add_row(rapp_all(race_vars, pop_total, year = 2018)) %>%
#   add_row(rapp_var(race_vars, pop_total, year = 2018)) %>%
#   add_row(rapp_all(race_vars, pop_total, year = 2017)) %>%
#   add_row(rapp_var(race_vars, pop_total, year = 2017)) %>%
#   add_row(rapp_all(race_vars, pop_total, year = 2016)) %>%
#   add_row(rapp_var(race_vars, pop_total, year = 2016)) %>%
#   add_row(rapp_all(race_vars, pop_total, year = 2015)) %>%
#   add_row(rapp_var(race_vars, pop_total, year = 2015)) %>%
#   subset(select = -c(STATEFP, COUNTYFP, AFFGEOID, LSAD, NAME.y, ALAND, AWATER, percent)) %>%
#   rename(NAME = NAME.x) %>%
#   mutate(percent = (estimate/summary_est)*100) %>%
#   add_column(year = c("2019", "2019", "2019", "2019", "2019", "2019", "2019",
#                       "2019", "2019", "2019", "2019", "2019", "2019", "2019",
#                       "2019", "2019", "2019", "2019", "2019", "2019", "2019",
#                       "2019", "2019", "2019", "2019", "2019", "2019", "2019",
#                       "2019", "2019", "2019", "2019", "2019", "2019", "2019",
#                       "2019", "2019", "2019", "2019", "2019", "2019", "2019",
#                       "2018", "2018", "2018", "2018", "2018", "2018", "2018",
#                       "2018", "2018", "2018", "2018", "2018", "2018", "2018",
#                       "2018", "2018", "2018", "2018", "2018", "2018", "2018",
#                       "2018", "2018", "2018", "2018", "2018", "2018", "2018",
#                       "2018", "2018", "2018", "2018", "2018", "2018", "2018",
#                       "2018", "2018", "2018", "2018", "2018", "2018", "2018",
#                       "2017", "2017", "2017", "2017", "2017", "2017", "2017",
#                       "2017", "2017", "2017", "2017", "2017", "2017", "2017",
#                       "2017", "2017", "2017", "2017", "2017", "2017", "2017",
#                       "2017", "2017", "2017", "2017", "2017", "2017", "2017",
#                       "2017", "2017", "2017", "2017", "2017", "2017", "2017",
#                       "2017", "2017", "2017", "2017", "2017", "2017", "2017",
#                       "2016", "2016", "2016", "2016", "2016", "2016", "2016",
#                       "2016", "2016", "2016", "2016", "2016", "2016", "2016",
#                       "2016", "2016", "2016", "2016", "2016", "2016", "2016",
#                       "2016", "2016", "2016", "2016", "2016", "2016", "2016",
#                       "2016", "2016", "2016", "2016", "2016", "2016", "2016",
#                       "2016", "2016", "2016", "2016", "2016", "2016", "2016",
#                       "2015", "2015", "2015", "2015", "2015", "2015", "2015",
#                       "2015", "2015", "2015", "2015", "2015", "2015", "2015",
#                       "2015", "2015", "2015", "2015", "2015", "2015", "2015",
#                       "2015", "2015", "2015", "2015", "2015", "2015", "2015",
#                       "2015", "2015", "2015", "2015", "2015", "2015", "2015",
#                       "2015", "2015", "2015", "2015", "2015", "2015", "2015"))
# 
# 
# #Well, it shows that the majority of Rappahannock is white.
# #So white that it'd be more statistically significant to put everyone else in "other"
# #And that's just sorta insulting
# ggplot(race_demo, aes(x = NAME, y = percent, fill = variable)) +
#   geom_col() +
#   coord_flip() +
#   facet_wrap(~year) +
#   plot_theme
# 
# ggplot(race_demo, aes(x = year, y = percent, color = variable, group = variable)) +
#   geom_line(aes(size=estimate)) +
#   facet_wrap(~NAME)  +
#   plot_theme
# 
# minority <- filter(race_demo, variable != "white") 
# rapprace <- filter(minority, NAME == "Rappahannock") %>% filter(year == "2019") 
# wakerace <- filter(minority, NAME == "Wakefield")  %>% filter(year == "2019")
# piedrace <- filter(minority, NAME == "Piedmont") %>% filter(year == "2019")
# swrace <-  filter(minority, NAME == "Stonewall-Hawthorne") %>% filter(year == "2019")
# jackrace <- filter(minority, NAME == "Jackson") %>% filter(year == "2019")
# hamrace <- filter(minority, NAME == "Hampton") %>% filter(year == "2019")
# 
# #Just going to combine all the non-white people into "Minority" for simplicity's sake. Surely someone will find it useful
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #Time to break it up by year
# race_demo2019 <- race_demo %>% filter(year == "2019")
# race_demo2018 <- race_demo %>% filter(year == "2018")
# race_demo2017 <- race_demo %>% filter(year == "2017")
# race_demo2016 <- race_demo %>% filter(year == "2016")
# race_demo2015 <- race_demo %>% filter(year == "2015")
# 
# ggplot(race_demo, aes(x = NAME, y))
# 
# 

########################################################################
#Population over 25 or older with these educational credentials

pop25orolder = "B15003_001"

edu_var <- c(hsdiploma = "B15003_017",
             ged = "B15003_018" ,
             somecollege1 = "B15003_019",
             somecollege2 = "B15003_020",
             associates = "B15003_021",
             bachelors = "B15003_022",
             masters = "B15003_023",
             professionaldegree = "B15003_024",
             phd = "B15003_025"
             )





education$variable <- factor(eduvector, order = TRUE, levels = c(eduvector))

phd <- rapp_map("B15003_025") + labs(title = "PhD")
masters <- rapp_map("B15003_023") + labs(title = "Masters")
bachelors <- rapp_map("B15003_022") + labs(title = "Bachelors")
associates <- rapp_map("B15003_021") + labs(title = "Associates")
somecollege <-rapp_map("B15003_019") + labs(title = "Some College")
diploma <- rapp_map("B15003_017") + labs(title = "HS Diploma")


#####################################################################


median_income_dollars <- c(median_income_dollars = "S1901_C01_012")
mean_income_dollars <- c(mean_income_dollars = "S1901_C01_013")



mediandollars2019 <- rapp_var(median_income_dollars, median_income_dollars) %>%
  subset(select = c(GEOID, NAME.x, variable, estimate, moe, summary_est, summary_moe, percent, geometry)) %>%
  rename(NAME = NAME.x) %>%
  add_column(year = "2019")
mediandollars2018 <- rapp_var(median_income_dollars, median_income_dollars, 2018)  %>%
  subset(select = c(GEOID, NAME.x, variable, estimate, moe, summary_est, summary_moe, percent, geometry)) %>%
  rename(NAME = NAME.x) %>%
  add_column(year = "2018")
mediandollars2017 <- rapp_var(median_income_dollars, median_income_dollars, 2017)  %>%
  subset(select = c(GEOID, NAME.x, variable, estimate, moe, summary_est, summary_moe, percent, geometry)) %>%
  rename(NAME = NAME.x) %>%
  add_column(year = "2017")
mediandollars2016 <- rapp_var(median_income_dollars, median_income_dollars, 2016)  %>%
  subset(select = c(GEOID, NAME.x, variable, estimate, moe, summary_est, summary_moe, percent, geometry)) %>%
  rename(NAME = NAME.x) %>%
  add_column(year = "2016")
mediandollars2015 <- rapp_var(median_income_dollars, median_income_dollars, 2015)  %>%
  subset(select = c(GEOID, NAME.x, variable, estimate, moe, summary_est, summary_moe, percent, geometry)) %>%
  rename(NAME = NAME.x) %>%
  add_column(year = "2015")
mediandollars2014 <- rapp_var(median_income_dollars, median_income_dollars, 2014)  %>%
  subset(select = c(GEOID, NAME.x, variable, estimate, moe, summary_est, summary_moe, percent, geometry)) %>%
  rename(NAME = NAME.x) %>%
  add_column(year = "2014") %>%
  st_zm(drop = TRUE, what = "ZM")
mediandollars2013 <- rapp_var(median_income_dollars, median_income_dollars, 2013)  %>%
  subset(select = c(GEOID, NAME.x, variable, estimate, moe, summary_est, summary_moe, percent, geometry)) %>%
  rename(NAME = NAME.x) %>%
  add_column(year = "2013")
mediandollars2012 <- rapp_var(median_income_dollars, median_income_dollars, 2012)  %>%
  subset(select = c(GEOID, NAME.x, variable, estimate, moe, summary_est, summary_moe, percent, geometry)) %>%
  rename(NAME = NAME.x) %>%
  add_column(year = "2012")
mediandollars2011 <- rapp_var(median_income_dollars, median_income_dollars, 2011)  %>%
  subset(select = c(GEOID, NAME.x, variable, estimate, moe, summary_est, summary_moe, percent, geometry)) %>%
  rename(NAME = NAME.x) %>%
  add_column(year = "2011")
mediandollars2010 <-  get_acs(geography = "county subdivision",
                              state = 51,
                              county = 157,
                              variables = median_income_dollars,
                              summary_var = median_income_dollars,
                              year = 2010,
                              geometry = TRUE,
                              keep_geo_vars = TRUE,
                              cache = TRUE) %>%
  mutate(percent = (estimate/sum(summary_est))*100)  %>%
  subset(select = c(GEOID, NAME.x, variable, estimate, moe, summary_est, summary_moe, percent, geometry)) %>%
  rename(NAME = NAME.x) %>%
  add_column(year = "2010")



mediandollars2010_2019 <- mediandollars2019 %>% 
  rbind(mediandollars2018) %>% 
  rbind(mediandollars2017) %>% 
  rbind(mediandollars2016) %>% 
  rbind(mediandollars2015) %>% 
  rbind(mediandollars2014) %>% 
  rbind(mediandollars2013) %>% 
  rbind(mediandollars2012) %>% 
  rbind(mediandollars2011) %>% 
  rbind(mediandollars2010) 

ggplot(mediandollars2010_2019, aes(x = year, y = estimate, color = NAME, group = NAME)) +
         geom_line()

tm_shape(mediandollars2010_2019) +
  tm_borders() +
  tm_fill("estimate") +
  tm_text("NAME") +
  tm_facets("year")

ggplot(mediandollars2010_2019) +
  geom_sf(aes(fill = estimate)) +
coord_sf(datum = NA) +
  facet_wrap(~year)
