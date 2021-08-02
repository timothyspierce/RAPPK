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
#Is the horizontal line thing! Now my primary issue is what to do about scaling.
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


#Factoring these values for useage!
median_income$variable <- factor(medianincomevector, order = TRUE, levels = c(medianincomevector))

grouped_median_income <- median_income %>% group_by(variable)



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

 ggplot(incomedf, aes(x = incomebrackets, y = householdincome, fill = incomebrackets)) +
  geom_col(position = "dodge") +
  ggtitle("Median Income of the Population by Income Bracket", subtitle = "2015-2019 5-year American Community Survey") +
  ylab("Percentage of Population") +
  xlab("Income Bracket") +
  coord_flip() +
  plot_theme
 
 
 
 
 
 
#################################################################


bar_graph_and_medianline(incomedf)

median_income_dollars = c("Median Household Income" = "B19013_001")
                                               
incometotal <- rapp_var(median_income_dollars, median_income_dollars) %>%
  subset(select = -c(NAME.y)) %>%
  rename(NAME = NAME.x)

bar_graph_and_medianline(incometotal)


################################################################

incomebydistrict <- rapp_var(medianincome_var, median_income_all)

incomebydistrict_grouped <- incomebydistrict %>% data.framgroup_by(variable)

medianincomeva <- get_acs(geography = "state",
                                    state = 51,
                                    variables = median_income_dollars,
                                    geometry = TRUE,
                                    keep_geo_vars = TRUE,
                                    cache = TRUE)


bar_graph <- ggplot(incomebydistrict, aes(x = variable, y = percent, fill = variable)) +
  geom_col(position = "dodge") +
  ggtitle("Median Income of the Population by Income Bracket") +
  ylab("Percentage of Population") +
  xlab("Income Bracket") +
  coord_flip() +
  facet_wrap(~NAME.x)



####################################################33

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


medianincome_grouped <-  medianincome_wide %>%
  mutate(medianunder10k = ((medianincome_wide$med_hh_income_less10kE/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median10to24k = (((medianincome_wide$med_hh_income_10_14kE + medianincome_wide$med_hh_income_15_19kE + medianincome_wide$med_hh_income_20_24kE)/medianincome_wide$summary_est) * 100)) %>%
  mutate(median25to49k = (((medianincome_wide$med_hh_income_25_29kE + medianincome_wide$med_hh_income_30_34kE + medianincome_wide$med_hh_income_35_39kE + medianincome_wide$med_hh_income_40_44kE + medianincome_wide$med_hh_income_45_49kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median50to74k = (((medianincome_wide$med_hh_income_50_59kE + medianincome_wide$med_hh_income_60_74kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median75to99k = (((medianincome_wide$med_hh_income_75_99kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median100to150k = (((medianincome_wide$med_hh_income_100_124kE + medianincome_wide$med_hh_income_125_149kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median150to200k = ((medianincome_wide$med_hh_income_150_199kE/ medianincome_wide$summary_est) * 100)) %>%
  mutate(medianover200k = ((med_hh_income_200kmoreE/ medianincome_wide$summary_est) * 100)) 


incomepercent <- c(medianincome_grouped$medianunder10k, medianincome_grouped$median10to24k, medianincome_grouped$median25to49k, medianincome_grouped$median50to74k, medianincome_grouped$median75to99k, medianincome_grouped$median100to150k, medianincome_grouped$median150to200k, medianincome_grouped$medianover200k)
districts <- c("Wakefield", "Piedmont", "Hampton", "Jackson", "Stonewall-Hawthorne",
               "Wakefield", "Piedmont", "Hampton", "Jackson", "Stonewall-Hawthorne",
               "Wakefield", "Piedmont", "Hampton", "Jackson", "Stonewall-Hawthorne",
               "Wakefield", "Piedmont", "Hampton", "Jackson", "Stonewall-Hawthorne",
               "Wakefield", "Piedmont", "Hampton", "Jackson", "Stonewall-Hawthorne",
               "Wakefield", "Piedmont", "Hampton", "Jackson", "Stonewall-Hawthorne",
               "Wakefield", "Piedmont", "Hampton", "Jackson", "Stonewall-Hawthorne",
               "Wakefield", "Piedmont", "Hampton", "Jackson", "Stonewall-Hawthorne")



incomebrackets <- factor(c("medianunder10k", "medianunder10k", "medianunder10k", "medianunder10k", "medianunder10k", 
                           "median10to24k", "median10to24k", "median10to24k", "median10to24k", "median10to24k",
                            "median25to49k", "median25to49k", "median25to49k", "median25to49k", "median25to49k", 
                           "median50to74k", "median50to74k", "median50to74k", "median50to74k", "median50to74k",
                           "median75to99k", "median75to99k", "median75to99k", "median75to99k", "median75to99k", 
                           "median100to150k", "median100to150k", "median100to150k", "median100to150k", "median100to150k",
                           "median150to200k", "median150to200k", "median150to200k", "median150to200k", "median150to200k",
                           "medianover200k", "medianover200k", "medianover200k", "medianover200k", "medianover200k"
                           ), ordered = TRUE, levels = c("medianunder10k", "median10to24k", "median25to49k", "median50to74k", "median75to99k", "median100to150k", "median150to200k", "medianover200k"))


districtincomedf <- data.frame(districts, incomebrackets, incomepercent)

ggplot(districtincomedf, aes(x = incomebrackets, y = incomepercent, fill = incomebrackets)) + 
  geom_col() +
  ggtitle("Median Income of the Population by Income Bracket", subtitle = "2006-2010 5-year American Community Survey") +
  ylab("Percentage of Population") +
  xlab("Income Bracket") +
  coord_flip() +
  facet_grid(~districts) +
  scale_color_manual(values=c("#56B4E9","#0072B2")) +
  plot_theme
 
################################################# Same as above but better


################
#For every year I summon forth the median income, combine them into succinct brackets, and give them a percentage value


medianincome_wide <- get_district_acs_wide(medianincome_var, median_income_all)

medianincome2019 <-  medianincome_wide %>%
  mutate(medianunder10k = ((medianincome_wide$med_hh_income_less10kE/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median10to24k = (((medianincome_wide$med_hh_income_10_14kE + medianincome_wide$med_hh_income_15_19kE + medianincome_wide$med_hh_income_20_24kE)/medianincome_wide$summary_est) * 100)) %>%
  mutate(median25to49k = (((medianincome_wide$med_hh_income_25_29kE + medianincome_wide$med_hh_income_30_34kE + medianincome_wide$med_hh_income_35_39kE + medianincome_wide$med_hh_income_40_44kE + medianincome_wide$med_hh_income_45_49kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median50to74k = (((medianincome_wide$med_hh_income_50_59kE + medianincome_wide$med_hh_income_60_74kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median75to99k = (((medianincome_wide$med_hh_income_75_99kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median100to150k = (((medianincome_wide$med_hh_income_100_124kE + medianincome_wide$med_hh_income_125_149kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median150to200k = ((medianincome_wide$med_hh_income_150_199kE/ medianincome_wide$summary_est) * 100)) %>%
  mutate(medianover200k = ((med_hh_income_200kmoreE/ medianincome_wide$summary_est) * 100)) %>%
  subset(select = -c(5, 11:42)) %>%
  cbind(year = 2019)



medianincome_wide <- get_district_acs_wide(medianincome_var, median_income_all, 2018)

medianincome2018 <-  medianincome_wide %>%
  mutate(medianunder10k = ((medianincome_wide$med_hh_income_less10kE/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median10to24k = (((medianincome_wide$med_hh_income_10_14kE + medianincome_wide$med_hh_income_15_19kE + medianincome_wide$med_hh_income_20_24kE)/medianincome_wide$summary_est) * 100)) %>%
  mutate(median25to49k = (((medianincome_wide$med_hh_income_25_29kE + medianincome_wide$med_hh_income_30_34kE + medianincome_wide$med_hh_income_35_39kE + medianincome_wide$med_hh_income_40_44kE + medianincome_wide$med_hh_income_45_49kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median50to74k = (((medianincome_wide$med_hh_income_50_59kE + medianincome_wide$med_hh_income_60_74kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median75to99k = (((medianincome_wide$med_hh_income_75_99kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median100to150k = (((medianincome_wide$med_hh_income_100_124kE + medianincome_wide$med_hh_income_125_149kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median150to200k = ((medianincome_wide$med_hh_income_150_199kE/ medianincome_wide$summary_est) * 100)) %>%
  mutate(medianover200k = ((med_hh_income_200kmoreE/ medianincome_wide$summary_est) * 100)) %>%
  subset(select = -c(5, 11:42)) %>%
  cbind(year = 2018)




medianincome_wide <- get_district_acs_wide(medianincome_var, median_income_all, 2017)

medianincome2017 <-  medianincome_wide %>%
  mutate(medianunder10k = ((medianincome_wide$med_hh_income_less10kE/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median10to24k = (((medianincome_wide$med_hh_income_10_14kE + medianincome_wide$med_hh_income_15_19kE + medianincome_wide$med_hh_income_20_24kE)/medianincome_wide$summary_est) * 100)) %>%
  mutate(median25to49k = (((medianincome_wide$med_hh_income_25_29kE + medianincome_wide$med_hh_income_30_34kE + medianincome_wide$med_hh_income_35_39kE + medianincome_wide$med_hh_income_40_44kE + medianincome_wide$med_hh_income_45_49kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median50to74k = (((medianincome_wide$med_hh_income_50_59kE + medianincome_wide$med_hh_income_60_74kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median75to99k = (((medianincome_wide$med_hh_income_75_99kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median100to150k = (((medianincome_wide$med_hh_income_100_124kE + medianincome_wide$med_hh_income_125_149kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median150to200k = ((medianincome_wide$med_hh_income_150_199kE/ medianincome_wide$summary_est) * 100)) %>%
  mutate(medianover200k = ((med_hh_income_200kmoreE/ medianincome_wide$summary_est) * 100)) %>%
  subset(select = -c(5, 11:42)) %>%
  cbind(year = 2017)



medianincome_wide <- get_district_acs_wide(medianincome_var, median_income_all, 2016)

medianincome2016 <-  medianincome_wide %>%
  mutate(medianunder10k = ((medianincome_wide$med_hh_income_less10kE/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median10to24k = (((medianincome_wide$med_hh_income_10_14kE + medianincome_wide$med_hh_income_15_19kE + medianincome_wide$med_hh_income_20_24kE)/medianincome_wide$summary_est) * 100)) %>%
  mutate(median25to49k = (((medianincome_wide$med_hh_income_25_29kE + medianincome_wide$med_hh_income_30_34kE + medianincome_wide$med_hh_income_35_39kE + medianincome_wide$med_hh_income_40_44kE + medianincome_wide$med_hh_income_45_49kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median50to74k = (((medianincome_wide$med_hh_income_50_59kE + medianincome_wide$med_hh_income_60_74kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median75to99k = (((medianincome_wide$med_hh_income_75_99kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median100to150k = (((medianincome_wide$med_hh_income_100_124kE + medianincome_wide$med_hh_income_125_149kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median150to200k = ((medianincome_wide$med_hh_income_150_199kE/ medianincome_wide$summary_est) * 100)) %>%
  mutate(medianover200k = ((med_hh_income_200kmoreE/ medianincome_wide$summary_est) * 100)) %>%
  subset(select = -c(5, 11:42)) %>%
  cbind(year = 2016)



medianincome_wide <- get_district_acs_wide(medianincome_var, median_income_all, 2015)

medianincome2015 <-  medianincome_wide %>%
  mutate(medianunder10k = ((medianincome_wide$med_hh_income_less10kE/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median10to24k = (((medianincome_wide$med_hh_income_10_14kE + medianincome_wide$med_hh_income_15_19kE + medianincome_wide$med_hh_income_20_24kE)/medianincome_wide$summary_est) * 100)) %>%
  mutate(median25to49k = (((medianincome_wide$med_hh_income_25_29kE + medianincome_wide$med_hh_income_30_34kE + medianincome_wide$med_hh_income_35_39kE + medianincome_wide$med_hh_income_40_44kE + medianincome_wide$med_hh_income_45_49kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median50to74k = (((medianincome_wide$med_hh_income_50_59kE + medianincome_wide$med_hh_income_60_74kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median75to99k = (((medianincome_wide$med_hh_income_75_99kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median100to150k = (((medianincome_wide$med_hh_income_100_124kE + medianincome_wide$med_hh_income_125_149kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median150to200k = ((medianincome_wide$med_hh_income_150_199kE/ medianincome_wide$summary_est) * 100)) %>%
  mutate(medianover200k = ((med_hh_income_200kmoreE/ medianincome_wide$summary_est) * 100)) %>%
  subset(select = -c(5, 11:42)) %>%
  cbind(year = 2015)



medianincome_wide <- get_district_acs_wide(medianincome_var, median_income_all, 2014)

medianincome2014 <-  medianincome_wide %>%
  mutate(medianunder10k = ((medianincome_wide$med_hh_income_less10kE/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median10to24k = (((medianincome_wide$med_hh_income_10_14kE + medianincome_wide$med_hh_income_15_19kE + medianincome_wide$med_hh_income_20_24kE)/medianincome_wide$summary_est) * 100)) %>%
  mutate(median25to49k = (((medianincome_wide$med_hh_income_25_29kE + medianincome_wide$med_hh_income_30_34kE + medianincome_wide$med_hh_income_35_39kE + medianincome_wide$med_hh_income_40_44kE + medianincome_wide$med_hh_income_45_49kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median50to74k = (((medianincome_wide$med_hh_income_50_59kE + medianincome_wide$med_hh_income_60_74kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median75to99k = (((medianincome_wide$med_hh_income_75_99kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median100to150k = (((medianincome_wide$med_hh_income_100_124kE + medianincome_wide$med_hh_income_125_149kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median150to200k = ((medianincome_wide$med_hh_income_150_199kE/ medianincome_wide$summary_est) * 100)) %>%
  mutate(medianover200k = ((med_hh_income_200kmoreE/ medianincome_wide$summary_est) * 100)) %>%
  subset(select = -c(5, 11:42)) %>%
  cbind(year = 2014)



medianincome_wide <- get_district_acs_wide(medianincome_var, median_income_all, 2013)

medianincome2013 <-  medianincome_wide %>%
  mutate(medianunder10k = ((medianincome_wide$med_hh_income_less10kE/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median10to24k = (((medianincome_wide$med_hh_income_10_14kE + medianincome_wide$med_hh_income_15_19kE + medianincome_wide$med_hh_income_20_24kE)/medianincome_wide$summary_est) * 100)) %>%
  mutate(median25to49k = (((medianincome_wide$med_hh_income_25_29kE + medianincome_wide$med_hh_income_30_34kE + medianincome_wide$med_hh_income_35_39kE + medianincome_wide$med_hh_income_40_44kE + medianincome_wide$med_hh_income_45_49kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median50to74k = (((medianincome_wide$med_hh_income_50_59kE + medianincome_wide$med_hh_income_60_74kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median75to99k = (((medianincome_wide$med_hh_income_75_99kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median100to150k = (((medianincome_wide$med_hh_income_100_124kE + medianincome_wide$med_hh_income_125_149kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median150to200k = ((medianincome_wide$med_hh_income_150_199kE/ medianincome_wide$summary_est) * 100)) %>%
  mutate(medianover200k = ((med_hh_income_200kmoreE/ medianincome_wide$summary_est) * 100)) %>%
  subset(select = -c(5, 11:42)) %>%
  cbind(year = 2013)




medianincome_wide <- get_district_acs_wide(medianincome_var, median_income_all, 2012)

medianincome2012 <-  medianincome_wide %>%
  mutate(medianunder10k = ((medianincome_wide$med_hh_income_less10kE/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median10to24k = (((medianincome_wide$med_hh_income_10_14kE + medianincome_wide$med_hh_income_15_19kE + medianincome_wide$med_hh_income_20_24kE)/medianincome_wide$summary_est) * 100)) %>%
  mutate(median25to49k = (((medianincome_wide$med_hh_income_25_29kE + medianincome_wide$med_hh_income_30_34kE + medianincome_wide$med_hh_income_35_39kE + medianincome_wide$med_hh_income_40_44kE + medianincome_wide$med_hh_income_45_49kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median50to74k = (((medianincome_wide$med_hh_income_50_59kE + medianincome_wide$med_hh_income_60_74kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median75to99k = (((medianincome_wide$med_hh_income_75_99kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median100to150k = (((medianincome_wide$med_hh_income_100_124kE + medianincome_wide$med_hh_income_125_149kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median150to200k = ((medianincome_wide$med_hh_income_150_199kE/ medianincome_wide$summary_est) * 100)) %>%
  mutate(medianover200k = ((med_hh_income_200kmoreE/ medianincome_wide$summary_est) * 100)) %>%
  subset(select = -c(7, 9:14, 17:50)) %>%
  cbind(year = 2012)




medianincome_wide <- get_district_acs_wide(medianincome_var, median_income_all, 2011)

medianincome2011 <-  medianincome_wide %>%
  mutate(medianunder10k = ((medianincome_wide$med_hh_income_less10kE/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median10to24k = (((medianincome_wide$med_hh_income_10_14kE + medianincome_wide$med_hh_income_15_19kE + medianincome_wide$med_hh_income_20_24kE)/medianincome_wide$summary_est) * 100)) %>%
  mutate(median25to49k = (((medianincome_wide$med_hh_income_25_29kE + medianincome_wide$med_hh_income_30_34kE + medianincome_wide$med_hh_income_35_39kE + medianincome_wide$med_hh_income_40_44kE + medianincome_wide$med_hh_income_45_49kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median50to74k = (((medianincome_wide$med_hh_income_50_59kE + medianincome_wide$med_hh_income_60_74kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median75to99k = (((medianincome_wide$med_hh_income_75_99kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median100to150k = (((medianincome_wide$med_hh_income_100_124kE + medianincome_wide$med_hh_income_125_149kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median150to200k = ((medianincome_wide$med_hh_income_150_199kE/ medianincome_wide$summary_est) * 100)) %>%
  mutate(medianover200k = ((med_hh_income_200kmoreE/ medianincome_wide$summary_est) * 100)) %>%
  subset(select = -c(7, 9:14, 17:50)) %>%
  cbind(year = 2011)




medianincome_wide <- get_district_acs_wide(medianincome_var, median_income_all, 2010)

medianincome2010 <-  medianincome_wide %>%
  mutate(medianunder10k = ((medianincome_wide$med_hh_income_less10kE/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median10to24k = (((medianincome_wide$med_hh_income_10_14kE + medianincome_wide$med_hh_income_15_19kE + medianincome_wide$med_hh_income_20_24kE)/medianincome_wide$summary_est) * 100)) %>%
  mutate(median25to49k = (((medianincome_wide$med_hh_income_25_29kE + medianincome_wide$med_hh_income_30_34kE + medianincome_wide$med_hh_income_35_39kE + medianincome_wide$med_hh_income_40_44kE + medianincome_wide$med_hh_income_45_49kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median50to74k = (((medianincome_wide$med_hh_income_50_59kE + medianincome_wide$med_hh_income_60_74kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median75to99k = (((medianincome_wide$med_hh_income_75_99kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median100to150k = (((medianincome_wide$med_hh_income_100_124kE + medianincome_wide$med_hh_income_125_149kE)/ medianincome_wide$summary_est) * 100)) %>%
  mutate(median150to200k = ((medianincome_wide$med_hh_income_150_199kE/ medianincome_wide$summary_est) * 100)) %>%
  mutate(medianover200k = ((med_hh_income_200kmoreE/ medianincome_wide$summary_est) * 100)) %>% 
  cbind(ALAND = c(108000845, 150321841, 204017992, 83078752, 144459698)) %>% cbind(AWATER = c(454601, 512121, 269998, 393131, 438758)) %>%
  cbind(COUSUBFP = medianincome_wide$COUSUB, COUSUBNS = medianincome_wide$COUSUB) %>%
  subset(select = c(STATEFP, COUNTYFP, COUSUBFP, COUSUBNS, GEOID, NAME.x, LSAD, ALAND, AWATER, summary_est, summary_moe, medianunder10k, median10to24k, median25to49k, median50to74k, median75to99k, median100to150k, median150to200k, medianover200k, geometry)) %>%
  cbind(year = 2010)



#The only justification for these separations are incompatible geography
medianincome2015_2019 <- medianincome2019 %>% 
  rbind(medianincome2018) %>% 
  rbind(medianincome2017) %>% 
  rbind(medianincome2016) %>% 
  rbind(medianincome2015)
  
medianincome2012_2014 <-  medianincome2014 %>% 
  rbind(medianincome2013) %>% 
  rbind(medianincome2012)
  
  
medianincome2010_2011 <- medianincome2011 %>% 
    rbind(medianincome2010)



tm_shape(medianincome2015_2019) +
tm_borders() +
tm_fill("medianunder10k") +
tm_text("NAME.x") +
tm_facets(by = "year")

tm_shape(medianincome2012_2014) +
  tm_borders() +
  tm_fill("medianunder10k") +
  tm_text("NAME.x") +
  tm_facets(by = "year")

tm_shape(medianincome2010_2011) +
  tm_borders() +
  tm_fill("medianunder10k") +
  tm_text("NAME.x") +
  tm_facets(by = "year")


under10k <- ggplot() +


