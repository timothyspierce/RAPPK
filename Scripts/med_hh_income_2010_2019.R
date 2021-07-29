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
library(spData)
library(spDataLarge)
library(GISTools)

devtools::install_github("rCarto/osrm")
options(tigris_use_cache = TRUE)
options(tigris_class="sf")

#################################################



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
    mutate(percent = (estimate/summary_est)*100) %>%
    subset(select = -c(COUNTYNS))}


get_rapp <- function(varcode, summary_var, year = 2019){
  get_acs(geography = "county",
          state = 51,
          county = 157,
          variables = varcode,
          summary_var = summary_var,
          year = year,
          geometry = TRUE,
          keep_geo_vars = TRUE,
          cache = TRUE) %>%
    mutate(percent = (estimate/summary_est)*100) %>%
    st_transform(crs = "WGS84")} #Converts the dataframe into a coord system most commonly used


rapp_wide <- function(varcode, summary_var, year = 2019){
  get_acs(geography = "county",
          state = 51,
          county = 157,
          variables = varcode,
          summary_var = summary_var,
          year = year,
          geometry = TRUE,
          keep_geo_vars = TRUE,
          cache = TRUE,
          output = "wide") %>%
    st_transform(crs = "WGS84") #Converts the dataframe into a coord system most commonly used
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
    mutate(percent = (estimate/summary_est)*100) %>%
    subset(select = -c(NAME.y))}

#################################################


get_district_acs_wide <- function(varcode, summary_var, year = 2019){
  get_acs(geography = "county subdivision",
          state = 51,
          county = 157,
          variables = varcode,
          summary_var = summary_var,
          geometry = TRUE,
          keep_geo_vars = TRUE,
          year = year,
          cache = TRUE,
          output = "wide") %>%
    st_transform(crs = "WGS84") %>%
    subset(select = -c(NAME.y))}



medianincome_wide <- get_district_acs_wide(medianincome_var, median_income_all)


################################################# Same as above but better


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

incomevector <- c("Under $25,000" = "medianunder25k", "$25,000 to $50,000" = "median25to50k", "$50,000 to $100,000" = "median50to100k", "Over $100,000" = "medianover100k")


#For every year I summon forth the median income, combine them into succinct brackets, and give them a percentage value


medianincome_wide <- get_district_acs_wide(medianincome_var, median_income_all)

medianincome2019 <-  medianincome_wide %>%
  mutate(medianunder25k = (((medianincome_wide$med_hh_income_less10kE + medianincome_wide$med_hh_income_10_14kE + medianincome_wide$med_hh_income_15_19kE + medianincome_wide$med_hh_income_20_24kE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(median25to50k = (((medianincome_wide$med_hh_income_25_29kE + medianincome_wide$med_hh_income_30_34kE + medianincome_wide$med_hh_income_35_39kE + medianincome_wide$med_hh_income_40_44kE + medianincome_wide$med_hh_income_45_49kE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(median50to100k = (((medianincome_wide$med_hh_income_50_59kE + medianincome_wide$med_hh_income_60_74kE + medianincome_wide$med_hh_income_75_99kE)/ sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(medianover100k = (((medianincome_wide$med_hh_income_100_124kE + medianincome_wide$med_hh_income_125_149kE + medianincome_wide$med_hh_income_150_199kE + med_hh_income_200kmoreE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  subset(select = c(GEOID, NAME.x, medianunder25k, median25to50k, median50to100k, medianover100k, summary_est, summary_moe, geometry)) %>%
  cbind(year = "2019")




medianincome_wide <- get_district_acs_wide(medianincome_var, median_income_all, 2018)

medianincome2018 <-  medianincome_wide %>%
  mutate(medianunder25k = (((medianincome_wide$med_hh_income_less10kE + medianincome_wide$med_hh_income_10_14kE + medianincome_wide$med_hh_income_15_19kE + medianincome_wide$med_hh_income_20_24kE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(median25to50k = (((medianincome_wide$med_hh_income_25_29kE + medianincome_wide$med_hh_income_30_34kE + medianincome_wide$med_hh_income_35_39kE + medianincome_wide$med_hh_income_40_44kE + medianincome_wide$med_hh_income_45_49kE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(median50to100k = (((medianincome_wide$med_hh_income_50_59kE + medianincome_wide$med_hh_income_60_74kE + medianincome_wide$med_hh_income_75_99kE)/ sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(medianover100k = (((medianincome_wide$med_hh_income_100_124kE + medianincome_wide$med_hh_income_125_149kE + medianincome_wide$med_hh_income_150_199kE + med_hh_income_200kmoreE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  subset(select = c(GEOID, NAME.x, medianunder25k, median25to50k, median50to100k, medianover100k, summary_est, summary_moe, geometry)) %>%
  cbind(year = "2018")




medianincome_wide <- get_district_acs_wide(medianincome_var, median_income_all, 2017)

medianincome2017 <-  medianincome_wide %>%
  mutate(medianunder25k = (((medianincome_wide$med_hh_income_less10kE + medianincome_wide$med_hh_income_10_14kE + medianincome_wide$med_hh_income_15_19kE + medianincome_wide$med_hh_income_20_24kE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(median25to50k = (((medianincome_wide$med_hh_income_25_29kE + medianincome_wide$med_hh_income_30_34kE + medianincome_wide$med_hh_income_35_39kE + medianincome_wide$med_hh_income_40_44kE + medianincome_wide$med_hh_income_45_49kE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(median50to100k = (((medianincome_wide$med_hh_income_50_59kE + medianincome_wide$med_hh_income_60_74kE + medianincome_wide$med_hh_income_75_99kE)/ sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(medianover100k = (((medianincome_wide$med_hh_income_100_124kE + medianincome_wide$med_hh_income_125_149kE + medianincome_wide$med_hh_income_150_199kE + med_hh_income_200kmoreE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  subset(select = c(GEOID, NAME.x, medianunder25k, median25to50k, median50to100k, medianover100k, summary_est, summary_moe, geometry)) %>%
  cbind(year = "2017")



medianincome_wide <- get_district_acs_wide(medianincome_var, median_income_all, 2016)

medianincome2016 <-  medianincome_wide %>%
  mutate(medianunder25k = (((medianincome_wide$med_hh_income_less10kE + medianincome_wide$med_hh_income_10_14kE + medianincome_wide$med_hh_income_15_19kE + medianincome_wide$med_hh_income_20_24kE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(median25to50k = (((medianincome_wide$med_hh_income_25_29kE + medianincome_wide$med_hh_income_30_34kE + medianincome_wide$med_hh_income_35_39kE + medianincome_wide$med_hh_income_40_44kE + medianincome_wide$med_hh_income_45_49kE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(median50to100k = (((medianincome_wide$med_hh_income_50_59kE + medianincome_wide$med_hh_income_60_74kE + medianincome_wide$med_hh_income_75_99kE)/ sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(medianover100k = (((medianincome_wide$med_hh_income_100_124kE + medianincome_wide$med_hh_income_125_149kE + medianincome_wide$med_hh_income_150_199kE + med_hh_income_200kmoreE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  subset(select = c(GEOID, NAME.x, medianunder25k, median25to50k, median50to100k, medianover100k, summary_est, summary_moe, geometry)) %>%
  cbind(year = "2016")



medianincome_wide <- get_district_acs_wide(medianincome_var, median_income_all, 2015)

medianincome2015 <-  medianincome_wide %>%
  mutate(medianunder25k = (((medianincome_wide$med_hh_income_less10kE + medianincome_wide$med_hh_income_10_14kE + medianincome_wide$med_hh_income_15_19kE + medianincome_wide$med_hh_income_20_24kE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(median25to50k = (((medianincome_wide$med_hh_income_25_29kE + medianincome_wide$med_hh_income_30_34kE + medianincome_wide$med_hh_income_35_39kE + medianincome_wide$med_hh_income_40_44kE + medianincome_wide$med_hh_income_45_49kE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(median50to100k = (((medianincome_wide$med_hh_income_50_59kE + medianincome_wide$med_hh_income_60_74kE + medianincome_wide$med_hh_income_75_99kE)/ sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(medianover100k = (((medianincome_wide$med_hh_income_100_124kE + medianincome_wide$med_hh_income_125_149kE + medianincome_wide$med_hh_income_150_199kE + med_hh_income_200kmoreE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  subset(select = c(GEOID, NAME.x, medianunder25k, median25to50k, median50to100k, medianover100k, summary_est, summary_moe, geometry)) %>%
  cbind(year = "2015")



medianincome_wide <- get_district_acs_wide(medianincome_var, median_income_all, 2014)

medianincome2014 <-  medianincome_wide %>%
  mutate(medianunder25k = (((medianincome_wide$med_hh_income_less10kE + medianincome_wide$med_hh_income_10_14kE + medianincome_wide$med_hh_income_15_19kE + medianincome_wide$med_hh_income_20_24kE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(median25to50k = (((medianincome_wide$med_hh_income_25_29kE + medianincome_wide$med_hh_income_30_34kE + medianincome_wide$med_hh_income_35_39kE + medianincome_wide$med_hh_income_40_44kE + medianincome_wide$med_hh_income_45_49kE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(median50to100k = (((medianincome_wide$med_hh_income_50_59kE + medianincome_wide$med_hh_income_60_74kE + medianincome_wide$med_hh_income_75_99kE)/ sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(medianover100k = (((medianincome_wide$med_hh_income_100_124kE + medianincome_wide$med_hh_income_125_149kE + medianincome_wide$med_hh_income_150_199kE + med_hh_income_200kmoreE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  subset(select = c(GEOID, NAME.x, medianunder25k, median25to50k, median50to100k, medianover100k, summary_est, summary_moe, geometry)) %>%
  cbind(year = "2014")

# 2014's shapefile is incompatible with all the other years so I have to drop the z-axis from its dataframe
medianincome2014 <- st_zm(medianincome2014, drop=TRUE, what = "ZM")



medianincome_wide <- get_district_acs_wide(medianincome_var, median_income_all, 2013)

medianincome2013 <-  medianincome_wide %>%
  mutate(medianunder25k = (((medianincome_wide$med_hh_income_less10kE + medianincome_wide$med_hh_income_10_14kE + medianincome_wide$med_hh_income_15_19kE + medianincome_wide$med_hh_income_20_24kE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(median25to50k = (((medianincome_wide$med_hh_income_25_29kE + medianincome_wide$med_hh_income_30_34kE + medianincome_wide$med_hh_income_35_39kE + medianincome_wide$med_hh_income_40_44kE + medianincome_wide$med_hh_income_45_49kE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(median50to100k = (((medianincome_wide$med_hh_income_50_59kE + medianincome_wide$med_hh_income_60_74kE + medianincome_wide$med_hh_income_75_99kE)/ sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(medianover100k = (((medianincome_wide$med_hh_income_100_124kE + medianincome_wide$med_hh_income_125_149kE + medianincome_wide$med_hh_income_150_199kE + med_hh_income_200kmoreE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  subset(select = c(GEOID, NAME.x, medianunder25k, median25to50k, median50to100k, medianover100k, summary_est, summary_moe, geometry)) %>%
  cbind(year = "2013")




medianincome_wide <- get_district_acs_wide(medianincome_var, median_income_all, 2012)

medianincome2012 <-  medianincome_wide %>%
  mutate(medianunder25k = (((medianincome_wide$med_hh_income_less10kE + medianincome_wide$med_hh_income_10_14kE + medianincome_wide$med_hh_income_15_19kE + medianincome_wide$med_hh_income_20_24kE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(median25to50k = (((medianincome_wide$med_hh_income_25_29kE + medianincome_wide$med_hh_income_30_34kE + medianincome_wide$med_hh_income_35_39kE + medianincome_wide$med_hh_income_40_44kE + medianincome_wide$med_hh_income_45_49kE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(median50to100k = (((medianincome_wide$med_hh_income_50_59kE + medianincome_wide$med_hh_income_60_74kE + medianincome_wide$med_hh_income_75_99kE)/ sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(medianover100k = (((medianincome_wide$med_hh_income_100_124kE + medianincome_wide$med_hh_income_125_149kE + medianincome_wide$med_hh_income_150_199kE + med_hh_income_200kmoreE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  subset(select = c(GEOID, NAME.x, medianunder25k, median25to50k, median50to100k, medianover100k, summary_est, summary_moe, geometry)) %>%
  cbind(year = "2012")




medianincome_wide <- get_district_acs_wide(medianincome_var, median_income_all, 2011)

medianincome2011 <-  medianincome_wide %>%
  mutate(medianunder25k = (((medianincome_wide$med_hh_income_less10kE + medianincome_wide$med_hh_income_10_14kE + medianincome_wide$med_hh_income_15_19kE + medianincome_wide$med_hh_income_20_24kE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(median25to50k = (((medianincome_wide$med_hh_income_25_29kE + medianincome_wide$med_hh_income_30_34kE + medianincome_wide$med_hh_income_35_39kE + medianincome_wide$med_hh_income_40_44kE + medianincome_wide$med_hh_income_45_49kE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(median50to100k = (((medianincome_wide$med_hh_income_50_59kE + medianincome_wide$med_hh_income_60_74kE + medianincome_wide$med_hh_income_75_99kE)/ sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(medianover100k = (((medianincome_wide$med_hh_income_100_124kE + medianincome_wide$med_hh_income_125_149kE + medianincome_wide$med_hh_income_150_199kE + med_hh_income_200kmoreE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  subset(select = c(GEOID, NAME.x, medianunder25k, median25to50k, median50to100k, medianover100k, summary_est, summary_moe, geometry)) %>%
  cbind(year = "2011")




medianincome_wide <- get_district_acs_wide(medianincome_var, median_income_all, 2010)

medianincome2010 <-  medianincome_wide %>%
  mutate(medianunder25k = (((medianincome_wide$med_hh_income_less10kE + medianincome_wide$med_hh_income_10_14kE + medianincome_wide$med_hh_income_15_19kE + medianincome_wide$med_hh_income_20_24kE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(median25to50k = (((medianincome_wide$med_hh_income_25_29kE + medianincome_wide$med_hh_income_30_34kE + medianincome_wide$med_hh_income_35_39kE + medianincome_wide$med_hh_income_40_44kE + medianincome_wide$med_hh_income_45_49kE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(median50to100k = (((medianincome_wide$med_hh_income_50_59kE + medianincome_wide$med_hh_income_60_74kE + medianincome_wide$med_hh_income_75_99kE)/ sum(medianincome_wide$summary_est)) * 100)) %>%
  mutate(medianover100k = (((medianincome_wide$med_hh_income_100_124kE + medianincome_wide$med_hh_income_125_149kE + medianincome_wide$med_hh_income_150_199kE + med_hh_income_200kmoreE) / sum(medianincome_wide$summary_est)) * 100)) %>%
  subset(select = c(GEOID, NAME.x, medianunder25k, median25to50k, median50to100k, medianover100k, summary_est, summary_moe, geometry)) %>%
  cbind(year = "2010")



#The only justification for these separations are incompatible geography
medianincome2010_2019 <- medianincome2019 %>% 
  rbind(medianincome2018) %>% 
  rbind(medianincome2017) %>% 
  rbind(medianincome2016) %>% 
  rbind(medianincome2015) %>%
  rbind(medianincome2014) %>%
  rbind(medianincome2013) %>% 
  rbind(medianincome2012) %>%
  rbind(medianincome2011) %>% 
  rbind(medianincome2010)


#Converting the above wide format into a long format so that all the variables in a singular column and the estimates can be their own column

income2010_2019 <- medianincome2010_2019 %>%
  pivot_longer(cols = c(medianunder25k, median25to50k, median50to100k, medianover100k), names_to = "incomebracket", values_to = "percent")
income2010_2019$incomebracket <- factor(income2010_2019$incomebracket, levels = incomevector)
income2010_2019 <- income2010_2019 %>% mutate(estimate = ((income2010_2019$percent / 100) * income2010_2019$summary_est) )


income2010_2019 <- st_as_sf(income2010_2019)



ggplot(income2010_2019, aes(x = incomebracket, y = percent, fill = NAME.x, group = NAME.x)) +
  geom_col(position = "dodge") +
  ggtitle("Median Household Income (In US Dollars) by District  2010-2019") +
  xlab("Income Bracket") +
  ylab("Percentage of Population") +
  facet_wrap(~year) +
  scale_fill_viridis_d(name = "District") +
  coord_flip() +
  scale_x_discrete(labels = c("Under $25,000", "$25,000 to $50,000" , "$50,000 to $100,000" , "Over $100,000" ))
  plot_theme
  
  ggplot(income2010_2019, aes(x = incomebracket, y = estimate, fill = NAME.x, group = NAME.x)) +
    geom_col(position = "dodge") +
    ggtitle("Median Household Income (In US Dollars) by District 2010-2019") +
    xlab("Income Bracket") +
    ylab("Total Population") +
  facet_wrap(~year) +
    scale_fill_viridis_d(name = "District") +
    coord_flip() +
    scale_x_discrete(labels = c("Under $25,000", "$25,000 to $50,000" , "$50,000 to $100,000" , "Over $100,000" )) +
    plot_theme
  
  ggplot(income2010_2019, aes(x = year, y = estimate, fill = incomebracket, group = incomebracket)) +
    geom_col(position = "dodge") +
    ggtitle("Median Household Income (In US Dollars) 2010-2019") +
    xlab("Income Bracket") +
    ylab("Total Population") +
    scale_fill_viridis_d(name = "Income Bracket", labels = c("Under $25,000", "$25,000 to $50,000" , "$50,000 to $100,000" , "Over $100,000" )) +
    plot_theme
  
  
  ggplot(income2010_2019, aes(x = year, y = estimate, group = incomebracket, color = incomebracket,)) +
geom_line(aes(size = percent)) +
  ggtitle(label = "Median Household Income (In US Dollars) by District 2010-2019") +
  ylab("Total Population") +
    labs(size ="Percent of Population") +
  scale_color_viridis_d(name = "Income Brackets", labels = c("Under $25,000", "$25,000 to $50,000" , "$50,000 to $100,000" , "Over $100,000" )) +
    facet_wrap(~NAME.x) +
    plot_theme
 
  
  ggplot(income2010_2019, aes(x = year, y = percent, group = incomebracket, color = incomebracket,)) +
    geom_line(aes(size = estimate)) +
    ggtitle(label = "Median Household Income (In US Dollars) by District 2010-2019") +
    ylab("Percentage of Population") +
    labs(size = "Total Population") +
    scale_color_viridis_d(name = "Income Brackets", labels = c("Under $25,000", "$25,000 to $50,000" , "$50,000 to $100,000" , "Over $100,000" )) +
    facet_wrap(~NAME.x) +
    plot_theme
  
    

####################### Median Income in raw Dollars #######################


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
  geom_line(aes(size = 3)) +
  labs(title = "Median Household Income by District (In US Dollars) 2010-2019") +
  xlab("Income in Dollars") +
  guides(size = FALSE) +
  ylim(30000,120000) +
  scale_color_viridis_d(name = "District") +
  plot_theme
  
  

ggplot(mediandollars2010_2019) +
  geom_sf(aes(fill = estimate)) +
  ggtitle("Median Household Income by District (In US Dollars) 2010-2019")
  geom_sf_label(aes(label = NAME)) +
  xlab("") +
  ylab("") +
  coord_sf(datum = NA) +
  facet_wrap(~year)

