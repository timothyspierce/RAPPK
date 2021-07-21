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



#########################################################################

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



# #ACS call for median income
# median_income <- get_rapp(medianincome_var, median_income_all)


############################# 2019


#ACS call for median income wide so that the housing income levels are separate. This is so they can be grouped separately
median_income <- rapp_wide(medianincome_var, median_income_all)

#Grouping income income levels
income_grouped <- median_income %>% 
  mutate(medianunder10k = median_income$med_hh_income_less10kE) %>%
  mutate(median10to24k = median_income$med_hh_income_10_14kE + median_income$med_hh_income_15_19kE + median_income$med_hh_income_20_24kE) %>%
  mutate(median25to49k = median_income$med_hh_income_25_29kE + median_income$med_hh_income_30_34kE + median_income$med_hh_income_35_39kE + median_income$med_hh_income_40_44kE + median_income$med_hh_income_45_49kE) %>%
  mutate(median50to74k = median_income$med_hh_income_50_59kE + median_income$med_hh_income_60_74kE) %>%
  mutate(median75to99k = median_income$med_hh_income_75_99kE) %>%
  mutate(median100to150k = median_income$med_hh_income_100_124kE + median_income$med_hh_income_125_149kE) %>%
  mutate(median150to200k = median_income$med_hh_income_150_199kE) %>%
  mutate(medianover200k = med_hh_income_200kmoreE)


#grouping income levels but giving them a percentage when I'm done
income_grouped_pct <- median_income %>%
  mutate(medianunder10k = ((median_income$med_hh_income_less10kE)/ summary_est * 100)) %>%
  mutate(median10to24k = ((median_income$med_hh_income_10_14kE + median_income$med_hh_income_15_19kE + median_income$med_hh_income_20_24kE)/summary_est * 100)) %>%
  mutate(median25to49k = ((median_income$med_hh_income_25_29kE + median_income$med_hh_income_30_34kE + median_income$med_hh_income_35_39kE + median_income$med_hh_income_40_44kE + median_income$med_hh_income_45_49kE)/ summary_est * 100)) %>%
  mutate(median50to74k = ((median_income$med_hh_income_50_59kE + median_income$med_hh_income_60_74kE)/ summary_est * 100)) %>%
  mutate(median75to99k = ((median_income$med_hh_income_75_99kE)/ summary_est * 100)) %>%
  mutate(median100to150k = ((median_income$med_hh_income_100_124kE + median_income$med_hh_income_125_149kE)/ summary_est * 100)) %>%
  mutate(median150to200k = ((median_income$med_hh_income_150_199kE)/ summary_est * 100)) %>%
  mutate(medianover200k = ((med_hh_income_200kmoreE)/ summary_est * 100))


householdincome <- c(income_grouped_pct$medianunder10k, income_grouped_pct$median10to24k, income_grouped_pct$median25to49k, income_grouped_pct$median50to74k, income_grouped_pct$median75to99k, income_grouped_pct$median100to150k, income_grouped_pct$median150to200k, income_grouped_pct$medianover200k)

incomebrackets <- factor(c("medianunder10k", "median10to24k", "median25to49k", "median50to74k", "median75to99k", "median100to150k", "median150to200k", "medianover200k"), ordered = TRUE, levels = c("medianunder10k", "median10to24k", "median25to49k", "median50to74k", "median75to99k", "median100to150k", "median150to200k", "medianover200k"))

incomedf <- data.frame(incomebrackets, householdincome)

income2019 <- ggplot(incomedf, aes(x = incomebrackets, y = householdincome, fill = incomebrackets)) +
  geom_col(position = "dodge") +
  ggtitle("Median Income of the Population by Income Bracket", subtitle = "2015-2019 5-year American Community Survey") +
  ylab("Percentage of Population") +
  xlab("Income Bracket") +
  coord_flip() +
  plot_theme


############################# 2018


#ACS call for median income wide so that the housing income levels are separate. This is so they can be grouped separately
median_income <- rapp_wide(medianincome_var, median_income_all, 2018)

#Grouping income income levels
income_grouped <- median_income %>% 
  mutate(medianunder10k = median_income$med_hh_income_less10kE) %>%
  mutate(median10to24k = median_income$med_hh_income_10_14kE + median_income$med_hh_income_15_19kE + median_income$med_hh_income_20_24kE) %>%
  mutate(median25to49k = median_income$med_hh_income_25_29kE + median_income$med_hh_income_30_34kE + median_income$med_hh_income_35_39kE + median_income$med_hh_income_40_44kE + median_income$med_hh_income_45_49kE) %>%
  mutate(median50to74k = median_income$med_hh_income_50_59kE + median_income$med_hh_income_60_74kE) %>%
  mutate(median75to99k = median_income$med_hh_income_75_99kE) %>%
  mutate(median100to150k = median_income$med_hh_income_100_124kE + median_income$med_hh_income_125_149kE) %>%
  mutate(median150to200k = median_income$med_hh_income_150_199kE) %>%
  mutate(medianover200k = med_hh_income_200kmoreE)


#grouping income levels but giving them a percentage when I'm done
income_grouped_pct <- median_income %>%
  mutate(medianunder10k = ((median_income$med_hh_income_less10kE)/ summary_est * 100)) %>%
  mutate(median10to24k = ((median_income$med_hh_income_10_14kE + median_income$med_hh_income_15_19kE + median_income$med_hh_income_20_24kE)/summary_est * 100)) %>%
  mutate(median25to49k = ((median_income$med_hh_income_25_29kE + median_income$med_hh_income_30_34kE + median_income$med_hh_income_35_39kE + median_income$med_hh_income_40_44kE + median_income$med_hh_income_45_49kE)/ summary_est * 100)) %>%
  mutate(median50to74k = ((median_income$med_hh_income_50_59kE + median_income$med_hh_income_60_74kE)/ summary_est * 100)) %>%
  mutate(median75to99k = ((median_income$med_hh_income_75_99kE)/ summary_est * 100)) %>%
  mutate(median100to150k = ((median_income$med_hh_income_100_124kE + median_income$med_hh_income_125_149kE)/ summary_est * 100)) %>%
  mutate(median150to200k = ((median_income$med_hh_income_150_199kE)/ summary_est * 100)) %>%
  mutate(medianover200k = ((med_hh_income_200kmoreE)/ summary_est * 100))


householdincome <- c(income_grouped_pct$medianunder10k, income_grouped_pct$median10to24k, income_grouped_pct$median25to49k, income_grouped_pct$median50to74k, income_grouped_pct$median75to99k, income_grouped_pct$median100to150k, income_grouped_pct$median150to200k, income_grouped_pct$medianover200k)

incomebrackets <- factor(c("medianunder10k", "median10to24k", "median25to49k", "median50to74k", "median75to99k", "median100to150k", "median150to200k", "medianover200k"), ordered = TRUE, levels = c("medianunder10k", "median10to24k", "median25to49k", "median50to74k", "median75to99k", "median100to150k", "median150to200k", "medianover200k"))

incomedf <- data.frame(incomebrackets, householdincome)

income2018 <- ggplot(incomedf, aes(x = incomebrackets, y = householdincome, fill = incomebrackets)) +
  geom_col(position = "dodge") +
  ggtitle("Median Income of the Population by Income Bracket", subtitle = "2014-2018 5-year American Community Survey") +
  ylab("Percentage of Population") +
  xlab("Income Bracket") +
  coord_flip() +
  plot_theme


############################# 2017


#ACS call for median income wide so that the housing income levels are separate. This is so they can be grouped separately
median_income <- rapp_wide(medianincome_var, median_income_all, 2017)

#Grouping income income levels
income_grouped <- median_income %>% 
  mutate(medianunder10k = median_income$med_hh_income_less10kE) %>%
  mutate(median10to24k = median_income$med_hh_income_10_14kE + median_income$med_hh_income_15_19kE + median_income$med_hh_income_20_24kE) %>%
  mutate(median25to49k = median_income$med_hh_income_25_29kE + median_income$med_hh_income_30_34kE + median_income$med_hh_income_35_39kE + median_income$med_hh_income_40_44kE + median_income$med_hh_income_45_49kE) %>%
  mutate(median50to74k = median_income$med_hh_income_50_59kE + median_income$med_hh_income_60_74kE) %>%
  mutate(median75to99k = median_income$med_hh_income_75_99kE) %>%
  mutate(median100to150k = median_income$med_hh_income_100_124kE + median_income$med_hh_income_125_149kE) %>%
  mutate(median150to200k = median_income$med_hh_income_150_199kE) %>%
  mutate(medianover200k = med_hh_income_200kmoreE)


#grouping income levels but giving them a percentage when I'm done
income_grouped_pct <- median_income %>%
  mutate(medianunder10k = ((median_income$med_hh_income_less10kE)/ summary_est * 100)) %>%
  mutate(median10to24k = ((median_income$med_hh_income_10_14kE + median_income$med_hh_income_15_19kE + median_income$med_hh_income_20_24kE)/summary_est * 100)) %>%
  mutate(median25to49k = ((median_income$med_hh_income_25_29kE + median_income$med_hh_income_30_34kE + median_income$med_hh_income_35_39kE + median_income$med_hh_income_40_44kE + median_income$med_hh_income_45_49kE)/ summary_est * 100)) %>%
  mutate(median50to74k = ((median_income$med_hh_income_50_59kE + median_income$med_hh_income_60_74kE)/ summary_est * 100)) %>%
  mutate(median75to99k = ((median_income$med_hh_income_75_99kE)/ summary_est * 100)) %>%
  mutate(median100to150k = ((median_income$med_hh_income_100_124kE + median_income$med_hh_income_125_149kE)/ summary_est * 100)) %>%
  mutate(median150to200k = ((median_income$med_hh_income_150_199kE)/ summary_est * 100)) %>%
  mutate(medianover200k = ((med_hh_income_200kmoreE)/ summary_est * 100))


householdincome <- c(income_grouped_pct$medianunder10k, income_grouped_pct$median10to24k, income_grouped_pct$median25to49k, income_grouped_pct$median50to74k, income_grouped_pct$median75to99k, income_grouped_pct$median100to150k, income_grouped_pct$median150to200k, income_grouped_pct$medianover200k)

incomebrackets <- factor(c("medianunder10k", "median10to24k", "median25to49k", "median50to74k", "median75to99k", "median100to150k", "median150to200k", "medianover200k"), ordered = TRUE, levels = c("medianunder10k", "median10to24k", "median25to49k", "median50to74k", "median75to99k", "median100to150k", "median150to200k", "medianover200k"))

incomedf <- data.frame(incomebrackets, householdincome)

income2017 <- ggplot(incomedf, aes(x = incomebrackets, y = householdincome, fill = incomebrackets)) +
  geom_col(position = "dodge") +
  ggtitle("Median Income of the Population by Income Bracket", subtitle = "2013-2017 5-year American Community Survey") +
  ylab("Percentage of Population") +
  xlab("Income Bracket") +
  coord_flip() +
  plot_theme



############################# 2016


#ACS call for median income wide so that the housing income levels are separate. This is so they can be grouped separately
median_income <- rapp_wide(medianincome_var, median_income_all, 2016)

#Grouping income income levels
income_grouped <- median_income %>% 
  mutate(medianunder10k = median_income$med_hh_income_less10kE) %>%
  mutate(median10to24k = median_income$med_hh_income_10_14kE + median_income$med_hh_income_15_19kE + median_income$med_hh_income_20_24kE) %>%
  mutate(median25to49k = median_income$med_hh_income_25_29kE + median_income$med_hh_income_30_34kE + median_income$med_hh_income_35_39kE + median_income$med_hh_income_40_44kE + median_income$med_hh_income_45_49kE) %>%
  mutate(median50to74k = median_income$med_hh_income_50_59kE + median_income$med_hh_income_60_74kE) %>%
  mutate(median75to99k = median_income$med_hh_income_75_99kE) %>%
  mutate(median100to150k = median_income$med_hh_income_100_124kE + median_income$med_hh_income_125_149kE) %>%
  mutate(median150to200k = median_income$med_hh_income_150_199kE) %>%
  mutate(medianover200k = med_hh_income_200kmoreE)


#grouping income levels but giving them a percentage when I'm done
income_grouped_pct <- median_income %>%
  mutate(medianunder10k = ((median_income$med_hh_income_less10kE)/ summary_est * 100)) %>%
  mutate(median10to24k = ((median_income$med_hh_income_10_14kE + median_income$med_hh_income_15_19kE + median_income$med_hh_income_20_24kE)/summary_est * 100)) %>%
  mutate(median25to49k = ((median_income$med_hh_income_25_29kE + median_income$med_hh_income_30_34kE + median_income$med_hh_income_35_39kE + median_income$med_hh_income_40_44kE + median_income$med_hh_income_45_49kE)/ summary_est * 100)) %>%
  mutate(median50to74k = ((median_income$med_hh_income_50_59kE + median_income$med_hh_income_60_74kE)/ summary_est * 100)) %>%
  mutate(median75to99k = ((median_income$med_hh_income_75_99kE)/ summary_est * 100)) %>%
  mutate(median100to150k = ((median_income$med_hh_income_100_124kE + median_income$med_hh_income_125_149kE)/ summary_est * 100)) %>%
  mutate(median150to200k = ((median_income$med_hh_income_150_199kE)/ summary_est * 100)) %>%
  mutate(medianover200k = ((med_hh_income_200kmoreE)/ summary_est * 100))


householdincome <- c(income_grouped_pct$medianunder10k, income_grouped_pct$median10to24k, income_grouped_pct$median25to49k, income_grouped_pct$median50to74k, income_grouped_pct$median75to99k, income_grouped_pct$median100to150k, income_grouped_pct$median150to200k, income_grouped_pct$medianover200k)

incomebrackets <- factor(c("medianunder10k", "median10to24k", "median25to49k", "median50to74k", "median75to99k", "median100to150k", "median150to200k", "medianover200k"), ordered = TRUE, levels = c("medianunder10k", "median10to24k", "median25to49k", "median50to74k", "median75to99k", "median100to150k", "median150to200k", "medianover200k"))

incomedf <- data.frame(incomebrackets, householdincome)

income2016 <- ggplot(incomedf, aes(x = incomebrackets, y = householdincome, fill = incomebrackets)) +
  geom_col(position = "dodge") +
  ggtitle("Median Income of the Population by Income Bracket", subtitle = "2012-2016 5-year American Community Survey") +
  ylab("Percentage of Population") +
  xlab("Income Bracket") +
  coord_flip() +
  plot_theme



############################# 2015

#ACS call for median income wide so that the housing income levels are separate. This is so they can be grouped separately
median_income <- rapp_wide(medianincome_var, median_income_all, 2015)

#Grouping income income levels
income_grouped <- median_income %>% 
  mutate(medianunder10k = median_income$med_hh_income_less10kE) %>%
  mutate(median10to24k = median_income$med_hh_income_10_14kE + median_income$med_hh_income_15_19kE + median_income$med_hh_income_20_24kE) %>%
  mutate(median25to49k = median_income$med_hh_income_25_29kE + median_income$med_hh_income_30_34kE + median_income$med_hh_income_35_39kE + median_income$med_hh_income_40_44kE + median_income$med_hh_income_45_49kE) %>%
  mutate(median50to74k = median_income$med_hh_income_50_59kE + median_income$med_hh_income_60_74kE) %>%
  mutate(median75to99k = median_income$med_hh_income_75_99kE) %>%
  mutate(median100to150k = median_income$med_hh_income_100_124kE + median_income$med_hh_income_125_149kE) %>%
  mutate(median150to200k = median_income$med_hh_income_150_199kE) %>%
  mutate(medianover200k = med_hh_income_200kmoreE)


#grouping income levels but giving them a percentage when I'm done
income_grouped_pct <- median_income %>%
  mutate(medianunder10k = ((median_income$med_hh_income_less10kE)/ summary_est * 100)) %>%
  mutate(median10to24k = ((median_income$med_hh_income_10_14kE + median_income$med_hh_income_15_19kE + median_income$med_hh_income_20_24kE)/summary_est * 100)) %>%
  mutate(median25to49k = ((median_income$med_hh_income_25_29kE + median_income$med_hh_income_30_34kE + median_income$med_hh_income_35_39kE + median_income$med_hh_income_40_44kE + median_income$med_hh_income_45_49kE)/ summary_est * 100)) %>%
  mutate(median50to74k = ((median_income$med_hh_income_50_59kE + median_income$med_hh_income_60_74kE)/ summary_est * 100)) %>%
  mutate(median75to99k = ((median_income$med_hh_income_75_99kE)/ summary_est * 100)) %>%
  mutate(median100to150k = ((median_income$med_hh_income_100_124kE + median_income$med_hh_income_125_149kE)/ summary_est * 100)) %>%
  mutate(median150to200k = ((median_income$med_hh_income_150_199kE)/ summary_est * 100)) %>%
  mutate(medianover200k = ((med_hh_income_200kmoreE)/ summary_est * 100))


householdincome <- c(income_grouped_pct$medianunder10k, income_grouped_pct$median10to24k, income_grouped_pct$median25to49k, income_grouped_pct$median50to74k, income_grouped_pct$median75to99k, income_grouped_pct$median100to150k, income_grouped_pct$median150to200k, income_grouped_pct$medianover200k)

incomebrackets <- factor(c("medianunder10k", "median10to24k", "median25to49k", "median50to74k", "median75to99k", "median100to150k", "median150to200k", "medianover200k"), ordered = TRUE, levels = c("medianunder10k", "median10to24k", "median25to49k", "median50to74k", "median75to99k", "median100to150k", "median150to200k", "medianover200k"))

incomedf <- data.frame(incomebrackets, householdincome)

income2015 <- ggplot(incomedf, aes(x = incomebrackets, y = householdincome, fill = incomebrackets)) +
  geom_col(position = "dodge") +
  ggtitle("Median Income of the Population by Income Bracket", subtitle = "2011-2015 5-year American Community Survey") +
  ylab("Percentage of Population") +
  xlab("Income Bracket") +
  coord_flip() +
  plot_theme



############################# 2014


#ACS call for median income wide so that the housing income levels are separate. This is so they can be grouped separately
median_income <- rapp_wide(medianincome_var, median_income_all, 2014)

#Grouping income income levels
income_grouped <- median_income %>% 
  mutate(medianunder10k = median_income$med_hh_income_less10kE) %>%
  mutate(median10to24k = median_income$med_hh_income_10_14kE + median_income$med_hh_income_15_19kE + median_income$med_hh_income_20_24kE) %>%
  mutate(median25to49k = median_income$med_hh_income_25_29kE + median_income$med_hh_income_30_34kE + median_income$med_hh_income_35_39kE + median_income$med_hh_income_40_44kE + median_income$med_hh_income_45_49kE) %>%
  mutate(median50to74k = median_income$med_hh_income_50_59kE + median_income$med_hh_income_60_74kE) %>%
  mutate(median75to99k = median_income$med_hh_income_75_99kE) %>%
  mutate(median100to150k = median_income$med_hh_income_100_124kE + median_income$med_hh_income_125_149kE) %>%
  mutate(median150to200k = median_income$med_hh_income_150_199kE) %>%
  mutate(medianover200k = med_hh_income_200kmoreE)


#grouping income levels but giving them a percentage when I'm done
income_grouped_pct <- median_income %>%
  mutate(medianunder10k = ((median_income$med_hh_income_less10kE)/ summary_est * 100)) %>%
  mutate(median10to24k = ((median_income$med_hh_income_10_14kE + median_income$med_hh_income_15_19kE + median_income$med_hh_income_20_24kE)/summary_est * 100)) %>%
  mutate(median25to49k = ((median_income$med_hh_income_25_29kE + median_income$med_hh_income_30_34kE + median_income$med_hh_income_35_39kE + median_income$med_hh_income_40_44kE + median_income$med_hh_income_45_49kE)/ summary_est * 100)) %>%
  mutate(median50to74k = ((median_income$med_hh_income_50_59kE + median_income$med_hh_income_60_74kE)/ summary_est * 100)) %>%
  mutate(median75to99k = ((median_income$med_hh_income_75_99kE)/ summary_est * 100)) %>%
  mutate(median100to150k = ((median_income$med_hh_income_100_124kE + median_income$med_hh_income_125_149kE)/ summary_est * 100)) %>%
  mutate(median150to200k = ((median_income$med_hh_income_150_199kE)/ summary_est * 100)) %>%
  mutate(medianover200k = ((med_hh_income_200kmoreE)/ summary_est * 100))


householdincome <- c(income_grouped_pct$medianunder10k, income_grouped_pct$median10to24k, income_grouped_pct$median25to49k, income_grouped_pct$median50to74k, income_grouped_pct$median75to99k, income_grouped_pct$median100to150k, income_grouped_pct$median150to200k, income_grouped_pct$medianover200k)

incomebrackets <- factor(c("medianunder10k", "median10to24k", "median25to49k", "median50to74k", "median75to99k", "median100to150k", "median150to200k", "medianover200k"), ordered = TRUE, levels = c("medianunder10k", "median10to24k", "median25to49k", "median50to74k", "median75to99k", "median100to150k", "median150to200k", "medianover200k"))

incomedf <- data.frame(incomebrackets, householdincome)

income2014 <- ggplot(incomedf, aes(x = incomebrackets, y = householdincome, fill = incomebrackets)) +
  geom_col(position = "dodge") +
  ggtitle("Median Income of the Population by Income Bracket", subtitle = "2010-2014 5-year American Community Survey") +
  ylab("Percentage of Population") +
  xlab("Income Bracket") +
  coord_flip() +
  plot_theme



############################# 2013

#ACS call for median income wide so that the housing income levels are separate. This is so they can be grouped separately
median_income <- rapp_wide(medianincome_var, median_income_all, 2013)

#Grouping income income levels
income_grouped <- median_income %>% 
  mutate(medianunder10k = median_income$med_hh_income_less10kE) %>%
  mutate(median10to24k = median_income$med_hh_income_10_14kE + median_income$med_hh_income_15_19kE + median_income$med_hh_income_20_24kE) %>%
  mutate(median25to49k = median_income$med_hh_income_25_29kE + median_income$med_hh_income_30_34kE + median_income$med_hh_income_35_39kE + median_income$med_hh_income_40_44kE + median_income$med_hh_income_45_49kE) %>%
  mutate(median50to74k = median_income$med_hh_income_50_59kE + median_income$med_hh_income_60_74kE) %>%
  mutate(median75to99k = median_income$med_hh_income_75_99kE) %>%
  mutate(median100to150k = median_income$med_hh_income_100_124kE + median_income$med_hh_income_125_149kE) %>%
  mutate(median150to200k = median_income$med_hh_income_150_199kE) %>%
  mutate(medianover200k = med_hh_income_200kmoreE)


#grouping income levels but giving them a percentage when I'm done
income_grouped_pct <- median_income %>%
  mutate(medianunder10k = ((median_income$med_hh_income_less10kE)/ summary_est * 100)) %>%
  mutate(median10to24k = ((median_income$med_hh_income_10_14kE + median_income$med_hh_income_15_19kE + median_income$med_hh_income_20_24kE)/summary_est * 100)) %>%
  mutate(median25to49k = ((median_income$med_hh_income_25_29kE + median_income$med_hh_income_30_34kE + median_income$med_hh_income_35_39kE + median_income$med_hh_income_40_44kE + median_income$med_hh_income_45_49kE)/ summary_est * 100)) %>%
  mutate(median50to74k = ((median_income$med_hh_income_50_59kE + median_income$med_hh_income_60_74kE)/ summary_est * 100)) %>%
  mutate(median75to99k = ((median_income$med_hh_income_75_99kE)/ summary_est * 100)) %>%
  mutate(median100to150k = ((median_income$med_hh_income_100_124kE + median_income$med_hh_income_125_149kE)/ summary_est * 100)) %>%
  mutate(median150to200k = ((median_income$med_hh_income_150_199kE)/ summary_est * 100)) %>%
  mutate(medianover200k = ((med_hh_income_200kmoreE)/ summary_est * 100))


householdincome <- c(income_grouped_pct$medianunder10k, income_grouped_pct$median10to24k, income_grouped_pct$median25to49k, income_grouped_pct$median50to74k, income_grouped_pct$median75to99k, income_grouped_pct$median100to150k, income_grouped_pct$median150to200k, income_grouped_pct$medianover200k)

incomebrackets <- factor(c("medianunder10k", "median10to24k", "median25to49k", "median50to74k", "median75to99k", "median100to150k", "median150to200k", "medianover200k"), ordered = TRUE, levels = c("medianunder10k", "median10to24k", "median25to49k", "median50to74k", "median75to99k", "median100to150k", "median150to200k", "medianover200k"))

incomedf <- data.frame(incomebrackets, householdincome)

income2013       <- ggplot(incomedf, aes(x = incomebrackets, y = householdincome, fill = incomebrackets)) +
  geom_col(position = "dodge") +
  ggtitle("Median Income of the Population by Income Bracket", subtitle = "2009-2013 5-year American Community Survey") +
  ylab("Percentage of Population") +
  xlab("Income Bracket") +
  coord_flip() +
  plot_theme




############################# 2012

#ACS call for median income wide so that the housing income levels are separate. This is so they can be grouped separately
median_income <- rapp_wide(medianincome_var, median_income_all, 2012)

#Grouping income income levels
income_grouped <- median_income %>% 
  mutate(medianunder10k = median_income$med_hh_income_less10kE) %>%
  mutate(median10to24k = median_income$med_hh_income_10_14kE + median_income$med_hh_income_15_19kE + median_income$med_hh_income_20_24kE) %>%
  mutate(median25to49k = median_income$med_hh_income_25_29kE + median_income$med_hh_income_30_34kE + median_income$med_hh_income_35_39kE + median_income$med_hh_income_40_44kE + median_income$med_hh_income_45_49kE) %>%
  mutate(median50to74k = median_income$med_hh_income_50_59kE + median_income$med_hh_income_60_74kE) %>%
  mutate(median75to99k = median_income$med_hh_income_75_99kE) %>%
  mutate(median100to150k = median_income$med_hh_income_100_124kE + median_income$med_hh_income_125_149kE) %>%
  mutate(median150to200k = median_income$med_hh_income_150_199kE) %>%
  mutate(medianover200k = med_hh_income_200kmoreE)


#grouping income levels but giving them a percentage when I'm done
income_grouped_pct <- median_income %>%
  mutate(medianunder10k = ((median_income$med_hh_income_less10kE)/ summary_est * 100)) %>%
  mutate(median10to24k = ((median_income$med_hh_income_10_14kE + median_income$med_hh_income_15_19kE + median_income$med_hh_income_20_24kE)/summary_est * 100)) %>%
  mutate(median25to49k = ((median_income$med_hh_income_25_29kE + median_income$med_hh_income_30_34kE + median_income$med_hh_income_35_39kE + median_income$med_hh_income_40_44kE + median_income$med_hh_income_45_49kE)/ summary_est * 100)) %>%
  mutate(median50to74k = ((median_income$med_hh_income_50_59kE + median_income$med_hh_income_60_74kE)/ summary_est * 100)) %>%
  mutate(median75to99k = ((median_income$med_hh_income_75_99kE)/ summary_est * 100)) %>%
  mutate(median100to150k = ((median_income$med_hh_income_100_124kE + median_income$med_hh_income_125_149kE)/ summary_est * 100)) %>%
  mutate(median150to200k = ((median_income$med_hh_income_150_199kE)/ summary_est * 100)) %>%
  mutate(medianover200k = ((med_hh_income_200kmoreE)/ summary_est * 100))


householdincome <- c(income_grouped_pct$medianunder10k, income_grouped_pct$median10to24k, income_grouped_pct$median25to49k, income_grouped_pct$median50to74k, income_grouped_pct$median75to99k, income_grouped_pct$median100to150k, income_grouped_pct$median150to200k, income_grouped_pct$medianover200k)

incomebrackets <- factor(c("medianunder10k", "median10to24k", "median25to49k", "median50to74k", "median75to99k", "median100to150k", "median150to200k", "medianover200k"), ordered = TRUE, levels = c("medianunder10k", "median10to24k", "median25to49k", "median50to74k", "median75to99k", "median100to150k", "median150to200k", "medianover200k"))

incomedf <- data.frame(incomebrackets, householdincome)

income2012 <- ggplot(incomedf, aes(x = incomebrackets, y = householdincome, fill = incomebrackets)) +
  geom_col(position = "dodge") +
  ggtitle("Median Income of the Population by Income Bracket", subtitle = "2008-2012 5-year American Community Survey") +
  ylab("Percentage of Population") +
  xlab("Income Bracket") +
  coord_flip() +
  plot_theme




############################# 2011

#ACS call for median income wide so that the housing income levels are separate. This is so they can be grouped separately
median_income <- rapp_wide(medianincome_var, median_income_all, 2011)

#Grouping income income levels
income_grouped <- median_income %>% 
  mutate(medianunder10k = median_income$med_hh_income_less10kE) %>%
  mutate(median10to24k = median_income$med_hh_income_10_14kE + median_income$med_hh_income_15_19kE + median_income$med_hh_income_20_24kE) %>%
  mutate(median25to49k = median_income$med_hh_income_25_29kE + median_income$med_hh_income_30_34kE + median_income$med_hh_income_35_39kE + median_income$med_hh_income_40_44kE + median_income$med_hh_income_45_49kE) %>%
  mutate(median50to74k = median_income$med_hh_income_50_59kE + median_income$med_hh_income_60_74kE) %>%
  mutate(median75to99k = median_income$med_hh_income_75_99kE) %>%
  mutate(median100to150k = median_income$med_hh_income_100_124kE + median_income$med_hh_income_125_149kE) %>%
  mutate(median150to200k = median_income$med_hh_income_150_199kE) %>%
  mutate(medianover200k = med_hh_income_200kmoreE)


#grouping income levels but giving them a percentage when I'm done
income_grouped_pct <- median_income %>%
  mutate(medianunder10k = ((median_income$med_hh_income_less10kE)/ summary_est * 100)) %>%
  mutate(median10to24k = ((median_income$med_hh_income_10_14kE + median_income$med_hh_income_15_19kE + median_income$med_hh_income_20_24kE)/summary_est * 100)) %>%
  mutate(median25to49k = ((median_income$med_hh_income_25_29kE + median_income$med_hh_income_30_34kE + median_income$med_hh_income_35_39kE + median_income$med_hh_income_40_44kE + median_income$med_hh_income_45_49kE)/ summary_est * 100)) %>%
  mutate(median50to74k = ((median_income$med_hh_income_50_59kE + median_income$med_hh_income_60_74kE)/ summary_est * 100)) %>%
  mutate(median75to99k = ((median_income$med_hh_income_75_99kE)/ summary_est * 100)) %>%
  mutate(median100to150k = ((median_income$med_hh_income_100_124kE + median_income$med_hh_income_125_149kE)/ summary_est * 100)) %>%
  mutate(median150to200k = ((median_income$med_hh_income_150_199kE)/ summary_est * 100)) %>%
  mutate(medianover200k = ((med_hh_income_200kmoreE)/ summary_est * 100))


householdincome <- c(income_grouped_pct$medianunder10k, income_grouped_pct$median10to24k, income_grouped_pct$median25to49k, income_grouped_pct$median50to74k, income_grouped_pct$median75to99k, income_grouped_pct$median100to150k, income_grouped_pct$median150to200k, income_grouped_pct$medianover200k)

incomebrackets <- factor(c("medianunder10k", "median10to24k", "median25to49k", "median50to74k", "median75to99k", "median100to150k", "median150to200k", "medianover200k"), ordered = TRUE, levels = c("medianunder10k", "median10to24k", "median25to49k", "median50to74k", "median75to99k", "median100to150k", "median150to200k", "medianover200k"))

incomedf <- data.frame(incomebrackets, householdincome)

income2011 <- ggplot(incomedf, aes(x = incomebrackets, y = householdincome, fill = incomebrackets)) +
  geom_col(position = "dodge") +
  ggtitle("Median Income of the Population by Income Bracket", subtitle = "2007-2011 5-year American Community Survey") +
  ylab("Percentage of Population") +
  xlab("Income Bracket") +
  coord_flip() +
  plot_theme


############################# 2010

#ACS call for median income wide so that the housing income levels are separate. This is so they can be grouped separately
median_income <- rapp_wide(medianincome_var, median_income_all, 2010)

#Grouping income income levels
income_grouped <- median_income %>% 
  mutate(medianunder10k = median_income$med_hh_income_less10kE) %>%
  mutate(median10to24k = median_income$med_hh_income_10_14kE + median_income$med_hh_income_15_19kE + median_income$med_hh_income_20_24kE) %>%
  mutate(median25to49k = median_income$med_hh_income_25_29kE + median_income$med_hh_income_30_34kE + median_income$med_hh_income_35_39kE + median_income$med_hh_income_40_44kE + median_income$med_hh_income_45_49kE) %>%
  mutate(median50to74k = median_income$med_hh_income_50_59kE + median_income$med_hh_income_60_74kE) %>%
  mutate(median75to99k = median_income$med_hh_income_75_99kE) %>%
  mutate(median100to150k = median_income$med_hh_income_100_124kE + median_income$med_hh_income_125_149kE) %>%
  mutate(median150to200k = median_income$med_hh_income_150_199kE) %>%
  mutate(medianover200k = med_hh_income_200kmoreE)


#grouping income levels but giving them a percentage when I'm done
income_grouped_pct <- median_income %>%
  mutate(medianunder10k = ((median_income$med_hh_income_less10kE)/ summary_est * 100)) %>%
  mutate(median10to24k = ((median_income$med_hh_income_10_14kE + median_income$med_hh_income_15_19kE + median_income$med_hh_income_20_24kE)/summary_est * 100)) %>%
  mutate(median25to49k = ((median_income$med_hh_income_25_29kE + median_income$med_hh_income_30_34kE + median_income$med_hh_income_35_39kE + median_income$med_hh_income_40_44kE + median_income$med_hh_income_45_49kE)/ summary_est * 100)) %>%
  mutate(median50to74k = ((median_income$med_hh_income_50_59kE + median_income$med_hh_income_60_74kE)/ summary_est * 100)) %>%
  mutate(median75to99k = ((median_income$med_hh_income_75_99kE)/ summary_est * 100)) %>%
  mutate(median100to150k = ((median_income$med_hh_income_100_124kE + median_income$med_hh_income_125_149kE)/ summary_est * 100)) %>%
  mutate(median150to200k = ((median_income$med_hh_income_150_199kE)/ summary_est * 100)) %>%
  mutate(medianover200k = ((med_hh_income_200kmoreE)/ summary_est * 100))


householdincome <- c(income_grouped_pct$medianunder10k, income_grouped_pct$median10to24k, income_grouped_pct$median25to49k, income_grouped_pct$median50to74k, income_grouped_pct$median75to99k, income_grouped_pct$median100to150k, income_grouped_pct$median150to200k, income_grouped_pct$medianover200k)

incomebrackets <- factor(c("medianunder10k", "median10to24k", "median25to49k", "median50to74k", "median75to99k", "median100to150k", "median150to200k", "medianover200k"), ordered = TRUE, levels = c("medianunder10k", "median10to24k", "median25to49k", "median50to74k", "median75to99k", "median100to150k", "median150to200k", "medianover200k"))

incomedf <- data.frame(incomebrackets, householdincome)

income2010 <- ggplot(incomedf, aes(x = incomebrackets, y = householdincome, fill = incomebrackets)) +
  geom_col(position = "dodge") +
  ggtitle("Median Income of the Population by Income Bracket", subtitle = "2006-2010 5-year American Community Survey") +
  ylab("Percentage of Population") +
  xlab("Income Bracket") +
  coord_flip() +
  plot_theme

################################

income2010_2014 <- ggarrange(income2010, income2011, income2012, income2013, income2014)

income2015_2019 <- ggarrange(income2015, income2016, income2017, income2018, income2019)






