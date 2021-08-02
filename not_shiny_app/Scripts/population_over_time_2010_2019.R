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

# 
# ggplot(population2010_2019, aes(x = year, y = percent, group = NAME, color = NAME)) +
#   geom_line()

# 
# ggplot(population2010_2019 %>% filter(NAME == "Rappahannock"), aes(x = year, y = estimate, group = variable)) +
#   geom_line(aes(size = percent)) +
#   ggtitle(label = "Estimated Total Population 2010-2019") +
#   ylab("Total Population") +
#   ylim(range = c(0,10000)) +
#   guides(size = FALSE) +
#   scale_color_viridis() +
#   plot_theme
# 

ggplot(population2010_2019 %>% filter(NAME != "Rappahannock"), aes(x = year, y = estimate, group = NAME, color = NAME)) +
  geom_line(aes(size = percent)) +
  ggtitle(label = "Estimated Total Population by District 2010-2019") +
  ylab("Total Population") +
  labs(size = "Percent of Population") +
  scale_color_viridis_d(name = "District") +
  plot_theme

ggplot(population2010_2019 %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = percent)) +
  coord_sf(datum = NA) +
  facet_wrap(~year) +
  plot_theme +
  labs(fill = "Percent") +
  geom_sf_label(aes(label = NAME), label.size = 0.005) +
  xlab("") +
  ylab("") +
 # scale_color_viridis_c() +
  ggtitle(label = "Estimated Percentage of Population by district 2010-2019")



ggplot(population2010_2019 %>% filter(NAME != "Rappahannock"), aes(x = year, y = percent, group = NAME, color = NAME)) +
  geom_line(aes(size = estimate)) +
  ggtitle(label = "Percentage of Population per District 2010-2019") +
  ylab("Percentage of Population") +
  labs(size = "Total Population") +
  scale_color_viridis_d(name = "District") +
  plot_theme










population_districts <- population2010_2019 %>% filter(NAME != "Rappahannock")
tm_shape(population_districts) +
  tm_borders() +
  tm_fill("percent") +
  tm_facets(by = "year") +
  tm_text("NAME") +
  tm_layout(title = "Estimated percentage of Population 2010-2019")



