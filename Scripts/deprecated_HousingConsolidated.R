#####################  #All intended libraries for this plan  #################
#################################################################################
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


#Color-blind friendly  Palette
cbPalette <- c("#E69F00","#56B4E9","#009E73","#0072B2","#D55E00", "#CC79A7")


######################################################################
poptotal <- c("Total Population" = "B02001_001")


#Just gonna slap this at the end of every ggplot for consistency
plot_theme <- theme(plot.title = element_text(hjust = 0.5),
                    axis.text=element_text(size=12),
                    legend.text = element_text(size=12),
                    axis.title.x=element_text(size =13),
                    axis.title.y=element_text(size =13)) +
  scale_fill_manual()


get_va <- function(varcode, summary_var, year = 2019){
  get_acs(geography = "county",
          state = 51,
          #county = 157,
          variables = varcode,
          summary_var = summary_var,
          year = year,
          geometry = TRUE,
          keep_geo_vars = TRUE,
          cache = TRUE) %>%
    mutate(percent = (estimate/summary_est)*100) %>%
    st_transform(crs = "WGS84")} 

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


#Function for getting variables by zip code and makes a pct column
rapp_single_var <- function(varcode, summary_var, year = 2019){
  get_acs(geography = "county",
          state = 51,
          county = 157,
          variables = varcode,
          summary_var = summary_var,
          year = year,
          geometry = TRUE,
          keep_geo_vars = TRUE,
          cache = TRUE) %>%
    mutate(summary_est = sum(summary_est)) %>%
    mutate(percent = (estimate/summary_est)*100) %>%
    st_transform(crs = "WGS84") #Converts the dataframe into a coord system most commonly used
}

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
    st_transform(crs = "WGS84") #Converts the dataframe into a coord system most commonly used
}

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
# 
# bar_graph_and_medianline <- function(dataset){
#   ggplot(dataset, aes(x = NAME, y = estimate, fill = NAME)) +
#     geom_col(position = "dodge") + 
#     ggtitle("Median Household income") +
#     geom_hline(aes(yintercept = median(estimate)), color = "black", size = 1.5, alpha = 0.25) +
#     labs(yintercept = "Median") +
#     facet_wrap(~variable)} 

#########################################################################

####################### For Housing Values ###########################

#Selects and names housing values
homevalue_var = c(home_values_10kless = "B25075_002",
                  home_values_10_14k = "B25075_003",
                  home_values_15_19k = "B25075_004",
                  home_values_20_24k = "B25075_005",
                  home_values_25_29k = "B25075_006",
                  home_values_30_34k = "B25075_007",
                  home_values_35_39k = "B25075_008",
                  home_values_40_49k = "B25075_009",
                  home_values_50_59k = "B25075_010",
                  home_values_60_69k = "B25075_011",
                  home_values_70_79k = "B25075_012",
                  home_values_80_89k = "B25075_013",
                  home_values_90_99k = "B25075_014",
                  home_values_100_124k = "B25075_015",
                  home_values_125_149k = "B25075_016",
                  home_values_150_174k = "B25075_017",
                  home_values_175_199k = "B25075_018",
                  home_values_200_249k = "B25075_019",
                  home_values_250_299k = "B25075_020",
                  home_values_300_399k = "B25075_021",
                  home_values_400_499k = "B25075_022",
                  home_values_500_749k = "B25075_023",
                  home_values_750_999k = "B25075_024",
                  home_values_1_1.4m = "B25075_025",
                  home_values_1.5_1.9m = "B25075_026",
                  home_values_2mmore = "B25075_027")
homes_all = "B25075_001"


homevalue2010_2014_var = c(home_values_10kless = "B25075_002",
                  home_values_10_14k = "B25075_003",
                  home_values_15_19k = "B25075_004",
                  home_values_20_24k = "B25075_005",
                  home_values_25_29k = "B25075_006",
                  home_values_30_34k = "B25075_007",
                  home_values_35_39k = "B25075_008",
                  home_values_40_49k = "B25075_009",
                  home_values_50_59k = "B25075_010",
                  home_values_60_69k = "B25075_011",
                  home_values_70_79k = "B25075_012",
                  home_values_80_89k = "B25075_013",
                  home_values_90_99k = "B25075_014",
                  home_values_100_124k = "B25075_015",
                  home_values_125_149k = "B25075_016",
                  home_values_150_174k = "B25075_017",
                  home_values_175_199k = "B25075_018",
                  home_values_200_249k = "B25075_019",
                  home_values_250_299k = "B25075_020",
                  home_values_300_399k = "B25075_021",
                  home_values_400_499k = "B25075_022",
                  home_values_500_749k = "B25075_023",
                  home_values_750_999k = "B25075_024",
                  home_values_1_1.4m = "B25075_025")

######################################################################

get_rappkdistrict  <- function(varcode, summary, year = 2019){get_acs(geography = "county subdivision",
                                                      state = 51,
                                                      county = 157,
                                                      variables = varcode,
                                                      summary_var = summary,
                                                      year = year,
                                                      output = "wide",
                                                      geometry = TRUE,
                                                      keep_geo_vars = TRUE) %>%
    rename(NAME = NAME.x) %>%
    subset(select = -c(COUSUBFP, COUSUBNS))}

get_rappk <- function(varcode, summary, year = 2019){get_acs(geography = "county",
                                             state = 51,
                                             county = 157,
                                             variables = varcode,
                                             summary_var = summary,
                                             year = year,
                                             output = "wide",
                                             geometry = TRUE,
                                             keep_geo_vars = TRUE) %>%
    rename(NAME = NAME.x) %>%
    subset(select = -c(COUNTYNS))}



######################################################

######################### 2019!
housingwide <- get_rappkdistrict(homevalue_var, homes_all)
rappahannockhousing <- get_rappk(homevalue_var, homes_all)

#Funny thing after doing this
#is finding out that the number of houses that are under 100k are in the double digits
#The low double digits. Like, less than 50.
#Had to consolidate my bottom 3 categories just so this made sense
#
# housinggrouped <- housingwide %>%
#   mutate("Less Than 10k" = housingprices$home_values_10klessE) %>%
#   
#   mutate("10k to 50k" = (housingprices$home_values_10_14kE +
#                            housingprices$home_values_15_19kE +
#                            housingprices$home_values_20_24kE +
#                            housingprices$home_values_25_29kE +
#                            housingprices$home_values_30_34kE +
#                            housingprices$home_values_40_49kE)) %>%
#   
#   mutate("50k to 100k" = (housingprices$home_values_50_59kE +
#                             housingprices$home_values_60_69kE +
#                             housingprices$home_values_70_79kE +
#                             housingprices$home_values_80_89kE +
#                             housingprices$home_values_90_99kE)) %>%
#   
#   mutate(price100kto300k = (housingprices$home_values_100_124kE +
#                              housingprices$home_values_125_149kE +
#                              housingprices$home_values_150_174kE +
#                              housingprices$home_values_175_199kE +
#                              housingprices$home_values_200_249kE +
#                              housingprices$home_values_250_299kE)) %>% 
#   
#   mutate(price300kto500k = (housingprices$home_values_300_399kE +
#                              housingprices$home_values_400_499kE)) %>% 
#   
#   mutate(price500kto1mil = (housingprices$home_values_500_749kE +
#                              housingprices$home_values_750_999kE)) %>% 
#   
#   mutate(price1milorGreater = (housingprices$home_values_1_1.4mE +
#                                 housingprices$home_values_1.5_1.9mE +
#                                 housingprices$home_values_2mmoreE))

housingprices <- rappahannockhousing %>% rbind(housingwide)

housingprices$NAME <- factor(as.factor(housingprices$NAME), levels = c("Piedmont", "Stonewall-Hawthorne", "Jackson", "Hampton", "Wakefield", "Rappahannock"))



housinggrouped <- housingprices %>%
  mutate(LessThan100k = (housingprices$home_values_10klessE + 
                            housingprices$home_values_10_14kE +
                            housingprices$home_values_15_19kE +
                            housingprices$home_values_20_24kE +
                            housingprices$home_values_25_29kE +
                            housingprices$home_values_30_34kE +
                            housingprices$home_values_40_49kE + 
                            housingprices$home_values_50_59kE +
                            housingprices$home_values_60_69kE +
                            housingprices$home_values_70_79kE +
                            housingprices$home_values_80_89kE +
                            housingprices$home_values_90_99kE)) %>%

  mutate(price100kto300k = (housingprices$home_values_100_124kE +
                             housingprices$home_values_125_149kE +
                             housingprices$home_values_150_174kE +
                             housingprices$home_values_175_199kE +
                             housingprices$home_values_200_249kE +
                             housingprices$home_values_250_299kE)) %>%

  mutate(price300kto500k = (housingprices$home_values_300_399kE +
                             housingprices$home_values_400_499kE)) %>%

  mutate(price500kto1mil = (housingprices$home_values_500_749kE +
                             housingprices$home_values_750_999kE)) %>%

  mutate(price1milorGreater = (housingprices$home_values_1_1.4mE +
                                housingprices$home_values_1.5_1.9mE +
                                housingprices$home_values_2mmoreE)) %>%
  
  subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater))

housing2019 <- housinggrouped 
  

plot1 <- ggplot(housinggrouped, aes(x = NAME, y = LessThan100k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot2 <- ggplot(housinggrouped, aes(x = NAME, y = price100kto300k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot3 <- ggplot(housinggrouped, aes(x = NAME, y = price300kto500k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot4 <- ggplot(housinggrouped, aes(x = NAME, y = price500kto1mil, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot5 <- ggplot(housinggrouped, aes(x = NAME, y = price1milorGreater, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme

housinggraphs2019 <- ggarrange(plot1, plot2, plot3, plot4, plot5,
                          ncol = 3, nrow = 2, common.legend = TRUE) %>%
  annotate_figure(top = "Number of houses by district in 2019")



map1 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = LessThan100k)) +
  coord_sf(datum = NA) +
  plot_theme

map2 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price100kto300k)) +
  coord_sf(datum = NA) +
  plot_theme

map3 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price300kto500k)) +
  coord_sf(datum = NA) +
  plot_theme

map4 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price500kto1mil)) +
  coord_sf(datum = NA) +
  plot_theme

map5 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price1milorGreater)) +
  coord_sf(datum = NA) +
  plot_theme

housingmaps2019 <- ggarrange(map1, map2, map3, map4, map5,
                               ncol = 3, nrow = 2) %>%
  annotate_figure(top = "Number of houses At Specific Price Brackets by district in 2019")



######################### 2018 ###############################



housingwide <- get_rappkdistrict(homevalue_var, homes_all, 2018)
rappahannockhousing <- get_rappk(homevalue_var, homes_all, 2018)


housingprices <- rappahannockhousing %>% rbind(housingwide)

housingprices$NAME <- factor(as.factor(housingprices$NAME), levels = c("Piedmont", "Stonewall-Hawthorne", "Jackson", "Hampton", "Wakefield", "Rappahannock"))



housinggrouped <- housingprices %>%
  mutate(LessThan100k = (housingprices$home_values_10klessE + 
                           housingprices$home_values_10_14kE +
                           housingprices$home_values_15_19kE +
                           housingprices$home_values_20_24kE +
                           housingprices$home_values_25_29kE +
                           housingprices$home_values_30_34kE +
                           housingprices$home_values_40_49kE + 
                           housingprices$home_values_50_59kE +
                           housingprices$home_values_60_69kE +
                           housingprices$home_values_70_79kE +
                           housingprices$home_values_80_89kE +
                           housingprices$home_values_90_99kE)) %>%
  
  mutate(price100kto300k = (housingprices$home_values_100_124kE +
                              housingprices$home_values_125_149kE +
                              housingprices$home_values_150_174kE +
                              housingprices$home_values_175_199kE +
                              housingprices$home_values_200_249kE +
                              housingprices$home_values_250_299kE)) %>%
  
  mutate(price300kto500k = (housingprices$home_values_300_399kE +
                              housingprices$home_values_400_499kE)) %>%
  
  mutate(price500kto1mil = (housingprices$home_values_500_749kE +
                              housingprices$home_values_750_999kE)) %>%
  
  mutate(price1milorGreater = (housingprices$home_values_1_1.4mE +
                                 housingprices$home_values_1.5_1.9mE +
                                 housingprices$home_values_2mmoreE)) 

housing2018 <- housinggrouped

plot1 <- ggplot(housinggrouped, aes(x = NAME, y = LessThan100k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot2 <- ggplot(housinggrouped, aes(x = NAME, y = price100kto300k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot3 <- ggplot(housinggrouped, aes(x = NAME, y = price300kto500k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot4 <- ggplot(housinggrouped, aes(x = NAME, y = price500kto1mil, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot5 <- ggplot(housinggrouped, aes(x = NAME, y = price1milorGreater, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme

housinggraphs2018 <- ggarrange(plot1, plot2, plot3, plot4, plot5,
                               ncol = 3, nrow = 2, common.legend = TRUE) %>%
  annotate_figure(top = "Number of houses by district in 2018")



map1 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = LessThan100k)) +
  coord_sf(datum = NA) +
  plot_theme

map2 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price100kto300k)) +
  coord_sf(datum = NA) +
  plot_theme

map3 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price300kto500k)) +
  coord_sf(datum = NA) +
  plot_theme

map4 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price500kto1mil)) +
  coord_sf(datum = NA) +
  plot_theme

map5 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price1milorGreater)) +
  coord_sf(datum = NA) +
  plot_theme

housingmaps2018 <- ggarrange(map1, map2, map3, map4, map5,
                             ncol = 3, nrow = 2) %>%
  annotate_figure(top = "Number of houses At Specific Price Brackets by district in 2018")


######################### 2017 ##########################


housingwide <- get_rappkdistrict(homevalue_var, homes_all, 2017)
rappahannockhousing <- get_rappk(homevalue_var, homes_all, 2017)


housingprices <- rappahannockhousing %>% rbind(housingwide)

housingprices$NAME <- factor(as.factor(housingprices$NAME), levels = c("Piedmont", "Stonewall-Hawthorne", "Jackson", "Hampton", "Wakefield", "Rappahannock"))



housing2017 <- housinggrouped <- housingprices %>%
  mutate(LessThan100k = (housingprices$home_values_10klessE + 
                           housingprices$home_values_10_14kE +
                           housingprices$home_values_15_19kE +
                           housingprices$home_values_20_24kE +
                           housingprices$home_values_25_29kE +
                           housingprices$home_values_30_34kE +
                           housingprices$home_values_40_49kE + 
                           housingprices$home_values_50_59kE +
                           housingprices$home_values_60_69kE +
                           housingprices$home_values_70_79kE +
                           housingprices$home_values_80_89kE +
                           housingprices$home_values_90_99kE)) %>%
  
  mutate(price100kto300k = (housingprices$home_values_100_124kE +
                              housingprices$home_values_125_149kE +
                              housingprices$home_values_150_174kE +
                              housingprices$home_values_175_199kE +
                              housingprices$home_values_200_249kE +
                              housingprices$home_values_250_299kE)) %>%
  
  mutate(price300kto500k = (housingprices$home_values_300_399kE +
                              housingprices$home_values_400_499kE)) %>%
  
  mutate(price500kto1mil = (housingprices$home_values_500_749kE +
                              housingprices$home_values_750_999kE)) %>%
  
  mutate(price1milorGreater = (housingprices$home_values_1_1.4mE +
                                 housingprices$home_values_1.5_1.9mE +
                                 housingprices$home_values_2mmoreE)) %>%
  
  subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater))



plot1 <- ggplot(housinggrouped, aes(x = NAME, y = LessThan100k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot2 <- ggplot(housinggrouped, aes(x = NAME, y = price100kto300k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot3 <- ggplot(housinggrouped, aes(x = NAME, y = price300kto500k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot4 <- ggplot(housinggrouped, aes(x = NAME, y = price500kto1mil, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot5 <- ggplot(housinggrouped, aes(x = NAME, y = price1milorGreater, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme

housinggraphs2017 <- ggarrange(plot1, plot2, plot3, plot4, plot5,
                               ncol = 3, nrow = 2, common.legend = TRUE) %>%
  annotate_figure(top = "Number of houses by district in 2017")



map1 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = LessThan100k)) +
  coord_sf(datum = NA) +
  plot_theme

map2 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price100kto300k)) +
  coord_sf(datum = NA) +
  plot_theme

map3 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price300kto500k)) +
  coord_sf(datum = NA) +
  plot_theme

map4 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price500kto1mil)) +
  coord_sf(datum = NA) +
  plot_theme

map5 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price1milorGreater)) +
  coord_sf(datum = NA) +
  plot_theme

housingmaps2017 <- ggarrange(map1, map2, map3, map4, map5,
                             ncol = 3, nrow = 2) %>%
  annotate_figure(top = "Number of houses At Specific Price Brackets by district in 2017")




######################### 2016 ##########################


housingwide <- get_rappkdistrict(homevalue_var, homes_all, 2016)
rappahannockhousing <- get_rappk(homevalue_var, homes_all, 2016)


housingprices <- rappahannockhousing %>% rbind(housingwide)

housingprices$NAME <- factor(as.factor(housingprices$NAME), levels = c("Piedmont", "Stonewall-Hawthorne", "Jackson", "Hampton", "Wakefield", "Rappahannock"))



housing2016 <- housinggrouped <- housingprices %>%
  mutate(LessThan100k = (housingprices$home_values_10klessE + 
                           housingprices$home_values_10_14kE +
                           housingprices$home_values_15_19kE +
                           housingprices$home_values_20_24kE +
                           housingprices$home_values_25_29kE +
                           housingprices$home_values_30_34kE +
                           housingprices$home_values_40_49kE + 
                           housingprices$home_values_50_59kE +
                           housingprices$home_values_60_69kE +
                           housingprices$home_values_70_79kE +
                           housingprices$home_values_80_89kE +
                           housingprices$home_values_90_99kE)) %>%
  
  mutate(price100kto300k = (housingprices$home_values_100_124kE +
                              housingprices$home_values_125_149kE +
                              housingprices$home_values_150_174kE +
                              housingprices$home_values_175_199kE +
                              housingprices$home_values_200_249kE +
                              housingprices$home_values_250_299kE)) %>%
  
  mutate(price300kto500k = (housingprices$home_values_300_399kE +
                              housingprices$home_values_400_499kE)) %>%
  
  mutate(price500kto1mil = (housingprices$home_values_500_749kE +
                              housingprices$home_values_750_999kE)) %>%
  
  mutate(price1milorGreater = (housingprices$home_values_1_1.4mE +
                                 housingprices$home_values_1.5_1.9mE +
                                 housingprices$home_values_2mmoreE)) %>%
  
  subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater))



plot1 <- ggplot(housinggrouped, aes(x = NAME, y = LessThan100k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot2 <- ggplot(housinggrouped, aes(x = NAME, y = price100kto300k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot3 <- ggplot(housinggrouped, aes(x = NAME, y = price300kto500k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot4 <- ggplot(housinggrouped, aes(x = NAME, y = price500kto1mil, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot5 <- ggplot(housinggrouped, aes(x = NAME, y = price1milorGreater, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme

housinggraphs2016 <- ggarrange(plot1, plot2, plot3, plot4, plot5,
                               ncol = 3, nrow = 2, common.legend = TRUE) %>%
  annotate_figure(top = "Number of houses by district in 2016")



map1 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = LessThan100k)) +
  coord_sf(datum = NA) +
  plot_theme

map2 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price100kto300k)) +
  coord_sf(datum = NA) +
  plot_theme

map3 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price300kto500k)) +
  coord_sf(datum = NA) +
  plot_theme

map4 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price500kto1mil)) +
  coord_sf(datum = NA) +
  plot_theme

map5 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price1milorGreater)) +
  coord_sf(datum = NA) +
  plot_theme

housingmaps2016 <- ggarrange(map1, map2, map3, map4, map5,
                             ncol = 3, nrow = 2) %>%
  annotate_figure(top = "Number of houses At Specific Price Brackets by district in 2016")




######################### 2015 ##########################


housingwide <- get_rappkdistrict(homevalue_var, homes_all, 2015)
rappahannockhousing <- get_rappk(homevalue_var, homes_all, 2015)


housingprices <- rappahannockhousing %>% rbind(housingwide)

housingprices$NAME <- factor(as.factor(housingprices$NAME), levels = c("Piedmont", "Stonewall-Hawthorne", "Jackson", "Hampton", "Wakefield", "Rappahannock"))



housing2015 <- housinggrouped <- housingprices %>%
  mutate(LessThan100k = (housingprices$home_values_10klessE + 
                           housingprices$home_values_10_14kE +
                           housingprices$home_values_15_19kE +
                           housingprices$home_values_20_24kE +
                           housingprices$home_values_25_29kE +
                           housingprices$home_values_30_34kE +
                           housingprices$home_values_40_49kE + 
                           housingprices$home_values_50_59kE +
                           housingprices$home_values_60_69kE +
                           housingprices$home_values_70_79kE +
                           housingprices$home_values_80_89kE +
                           housingprices$home_values_90_99kE)) %>%
  
  mutate(price100kto300k = (housingprices$home_values_100_124kE +
                              housingprices$home_values_125_149kE +
                              housingprices$home_values_150_174kE +
                              housingprices$home_values_175_199kE +
                              housingprices$home_values_200_249kE +
                              housingprices$home_values_250_299kE)) %>%
  
  mutate(price300kto500k = (housingprices$home_values_300_399kE +
                              housingprices$home_values_400_499kE)) %>%
  
  mutate(price500kto1mil = (housingprices$home_values_500_749kE +
                              housingprices$home_values_750_999kE)) %>%
  
  mutate(price1milorGreater = (housingprices$home_values_1_1.4mE +
                                 housingprices$home_values_1.5_1.9mE +
                                 housingprices$home_values_2mmoreE)) %>%
  
  subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater))



plot1 <- ggplot(housinggrouped, aes(x = NAME, y = LessThan100k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot2 <- ggplot(housinggrouped, aes(x = NAME, y = price100kto300k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot3 <- ggplot(housinggrouped, aes(x = NAME, y = price300kto500k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot4 <- ggplot(housinggrouped, aes(x = NAME, y = price500kto1mil, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot5 <- ggplot(housinggrouped, aes(x = NAME, y = price1milorGreater, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme

housinggraphs2015 <- ggarrange(plot1, plot2, plot3, plot4, plot5,
                               ncol = 3, nrow = 2, common.legend = TRUE) %>%
  annotate_figure(top = "Number of houses by district in 2015")



map1 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = LessThan100k)) +
  coord_sf(datum = NA) +
  plot_theme

map2 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price100kto300k)) +
  coord_sf(datum = NA) +
  plot_theme

map3 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price300kto500k)) +
  coord_sf(datum = NA) +
  plot_theme

map4 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price500kto1mil)) +
  coord_sf(datum = NA) +
  plot_theme

map5 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price1milorGreater)) +
  coord_sf(datum = NA) +
  plot_theme

housingmaps2015 <- ggarrange(map1, map2, map3, map4, map5,
                             ncol = 3, nrow = 2) %>%
  annotate_figure(top = "Number of houses At Specific Price Brackets by district in 2015")






######################### 2014 ##########################


housingwide <- get_rappkdistrict(homevalue2010_2014_var, homes_all, 2014)
rappahannockhousing <- get_rappk(homevalue2010_2014_var, homes_all, 2014)


housingprices <- rappahannockhousing %>% rbind(housingwide)

housingprices$NAME <- factor(as.factor(housingprices$NAME), levels = c("Piedmont", "Stonewall-Hawthorne", "Jackson", "Hampton", "Wakefield", "Rappahannock"))



housing2014 <- housinggrouped <- housingprices %>%
  mutate(LessThan100k = (housingprices$home_values_10klessE + 
                           housingprices$home_values_10_14kE +
                           housingprices$home_values_15_19kE +
                           housingprices$home_values_20_24kE +
                           housingprices$home_values_25_29kE +
                           housingprices$home_values_30_34kE +
                           housingprices$home_values_40_49kE + 
                           housingprices$home_values_50_59kE +
                           housingprices$home_values_60_69kE +
                           housingprices$home_values_70_79kE +
                           housingprices$home_values_80_89kE +
                           housingprices$home_values_90_99kE)) %>%
  
  mutate(price100kto300k = (housingprices$home_values_100_124kE +
                              housingprices$home_values_125_149kE +
                              housingprices$home_values_150_174kE +
                              housingprices$home_values_175_199kE +
                              housingprices$home_values_200_249kE +
                              housingprices$home_values_250_299kE)) %>%
  
  mutate(price300kto500k = (housingprices$home_values_300_399kE +
                              housingprices$home_values_400_499kE)) %>%
  
  mutate(price500kto1mil = (housingprices$home_values_500_749kE +
                              housingprices$home_values_750_999kE)) %>%
  
  mutate(price1milorGreater = (housingprices$home_values_1_1.4mE )) %>%
  
  subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater))



plot1 <- ggplot(housinggrouped, aes(x = NAME, y = LessThan100k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot2 <- ggplot(housinggrouped, aes(x = NAME, y = price100kto300k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot3 <- ggplot(housinggrouped, aes(x = NAME, y = price300kto500k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot4 <- ggplot(housinggrouped, aes(x = NAME, y = price500kto1mil, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot5 <- ggplot(housinggrouped, aes(x = NAME, y = price1milorGreater, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme

housinggraphs2014 <- ggarrange(plot1, plot2, plot3, plot4, plot5,
                               ncol = 3, nrow = 2, common.legend = TRUE) %>%
  annotate_figure(top = "Number of houses by district in 2014")



map1 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = LessThan100k)) +
  coord_sf(datum = NA) +
  plot_theme

map2 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price100kto300k)) +
  coord_sf(datum = NA) +
  plot_theme

map3 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price300kto500k)) +
  coord_sf(datum = NA) +
  plot_theme

map4 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price500kto1mil)) +
  coord_sf(datum = NA) +
  plot_theme

map5 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price1milorGreater)) +
  coord_sf(datum = NA) +
  plot_theme

housingmaps2014 <- ggarrange(map1, map2, map3, map4, map5,
                             ncol = 3, nrow = 2) %>%
  annotate_figure(top = "Number of houses At Specific Price Brackets by district in 2014")




######################### 2013 ##########################


housingwide <- get_rappkdistrict(homevalue2010_2014_var, homes_all, 2013)
rappahannockhousing <- get_rappk(homevalue2010_2014_var, homes_all, 2013)


housingprices <- rappahannockhousing %>% rbind(housingwide)

housingprices$NAME <- factor(as.factor(housingprices$NAME), levels = c("Piedmont", "Stonewall-Hawthorne", "Jackson", "Hampton", "Wakefield", "Rappahannock"))



housing2013 <- housinggrouped <- housingprices %>%
  mutate(LessThan100k = (housingprices$home_values_10klessE + 
                           housingprices$home_values_10_14kE +
                           housingprices$home_values_15_19kE +
                           housingprices$home_values_20_24kE +
                           housingprices$home_values_25_29kE +
                           housingprices$home_values_30_34kE +
                           housingprices$home_values_40_49kE + 
                           housingprices$home_values_50_59kE +
                           housingprices$home_values_60_69kE +
                           housingprices$home_values_70_79kE +
                           housingprices$home_values_80_89kE +
                           housingprices$home_values_90_99kE)) %>%
  
  mutate(price100kto300k = (housingprices$home_values_100_124kE +
                              housingprices$home_values_125_149kE +
                              housingprices$home_values_150_174kE +
                              housingprices$home_values_175_199kE +
                              housingprices$home_values_200_249kE +
                              housingprices$home_values_250_299kE)) %>%
  
  mutate(price300kto500k = (housingprices$home_values_300_399kE +
                              housingprices$home_values_400_499kE)) %>%
  
  mutate(price500kto1mil = (housingprices$home_values_500_749kE +
                              housingprices$home_values_750_999kE)) %>%
  
  mutate(price1milorGreater = (housingprices$home_values_1_1.4mE )) %>%
  
  subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater))



plot1 <- ggplot(housinggrouped, aes(x = NAME, y = LessThan100k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot2 <- ggplot(housinggrouped, aes(x = NAME, y = price100kto300k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot3 <- ggplot(housinggrouped, aes(x = NAME, y = price300kto500k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot4 <- ggplot(housinggrouped, aes(x = NAME, y = price500kto1mil, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot5 <- ggplot(housinggrouped, aes(x = NAME, y = price1milorGreater, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme

housinggraphs2013 <- ggarrange(plot1, plot2, plot3, plot4, plot5,
                               ncol = 3, nrow = 2, common.legend = TRUE) %>%
  annotate_figure(top = "Number of houses by district in 2013")



map1 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = LessThan100k)) +
  coord_sf(datum = NA) +
  plot_theme

map2 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price100kto300k)) +
  coord_sf(datum = NA) +
  plot_theme

map3 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price300kto500k)) +
  coord_sf(datum = NA) +
  plot_theme

map4 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price500kto1mil)) +
  coord_sf(datum = NA) +
  plot_theme

map5 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price1milorGreater)) +
  coord_sf(datum = NA) +
  plot_theme

housingmaps2013 <- ggarrange(map1, map2, map3, map4, map5,
                             ncol = 3, nrow = 2) %>%
  annotate_figure(top = "Number of houses At Specific Price Brackets by district in 2013")






######################### 2012 ##########################


housingwide <- get_rappkdistrict(homevalue2010_2014_var, homes_all, 2012)
rappahannockhousing <- get_rappk(homevalue2010_2014_var, homes_all, 2012)


housingprices <- rappahannockhousing %>% rbind(housingwide)

housingprices$NAME <- factor(as.factor(housingprices$NAME), levels = c("Piedmont", "Stonewall-Hawthorne", "Jackson", "Hampton", "Wakefield", "Rappahannock"))



housing2012 <- housinggrouped <- housingprices %>%
  mutate(LessThan100k = (housingprices$home_values_10klessE + 
                           housingprices$home_values_10_14kE +
                           housingprices$home_values_15_19kE +
                           housingprices$home_values_20_24kE +
                           housingprices$home_values_25_29kE +
                           housingprices$home_values_30_34kE +
                           housingprices$home_values_40_49kE + 
                           housingprices$home_values_50_59kE +
                           housingprices$home_values_60_69kE +
                           housingprices$home_values_70_79kE +
                           housingprices$home_values_80_89kE +
                           housingprices$home_values_90_99kE)) %>%
  
  mutate(price100kto300k = (housingprices$home_values_100_124kE +
                              housingprices$home_values_125_149kE +
                              housingprices$home_values_150_174kE +
                              housingprices$home_values_175_199kE +
                              housingprices$home_values_200_249kE +
                              housingprices$home_values_250_299kE)) %>%
  
  mutate(price300kto500k = (housingprices$home_values_300_399kE +
                              housingprices$home_values_400_499kE)) %>%
  
  mutate(price500kto1mil = (housingprices$home_values_500_749kE +
                              housingprices$home_values_750_999kE)) %>%
  
  mutate(price1milorGreater = (housingprices$home_values_1_1.4mE )) %>%
  
  subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater))



plot1 <- ggplot(housinggrouped, aes(x = NAME, y = LessThan100k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot2 <- ggplot(housinggrouped, aes(x = NAME, y = price100kto300k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot3 <- ggplot(housinggrouped, aes(x = NAME, y = price300kto500k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot4 <- ggplot(housinggrouped, aes(x = NAME, y = price500kto1mil, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot5 <- ggplot(housinggrouped, aes(x = NAME, y = price1milorGreater, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme

housinggraphs2012 <- ggarrange(plot1, plot2, plot3, plot4, plot5,
                               ncol = 3, nrow = 2, common.legend = TRUE) %>%
  annotate_figure(top = "Number of houses by district in 2012")



map1 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = LessThan100k)) +
  coord_sf(datum = NA) +
  plot_theme

map2 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price100kto300k)) +
  coord_sf(datum = NA) +
  plot_theme

map3 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price300kto500k)) +
  coord_sf(datum = NA) +
  plot_theme

map4 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price500kto1mil)) +
  coord_sf(datum = NA) +
  plot_theme

map5 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price1milorGreater)) +
  coord_sf(datum = NA) +
  plot_theme

housingmaps2012 <- ggarrange(map1, map2, map3, map4, map5,
                             ncol = 3, nrow = 2) %>%
  annotate_figure(top = "Number of houses At Specific Price Brackets by district in 2012")






######################### 2011 ##########################


housingwide <- get_rappkdistrict(homevalue2010_2014_var, homes_all, 2011)
rappahannockhousing <- get_rappk(homevalue2010_2014_var, homes_all, 2011)


housingprices <- rappahannockhousing %>% rbind(housingwide)

housingprices$NAME <- factor(as.factor(housingprices$NAME), levels = c("Piedmont", "Stonewall-Hawthorne", "Jackson", "Hampton", "Wakefield", "Rappahannock"))



housing2011 <- housinggrouped <- housingprices %>%
  mutate(LessThan100k = (housingprices$home_values_10klessE + 
                           housingprices$home_values_10_14kE +
                           housingprices$home_values_15_19kE +
                           housingprices$home_values_20_24kE +
                           housingprices$home_values_25_29kE +
                           housingprices$home_values_30_34kE +
                           housingprices$home_values_40_49kE + 
                           housingprices$home_values_50_59kE +
                           housingprices$home_values_60_69kE +
                           housingprices$home_values_70_79kE +
                           housingprices$home_values_80_89kE +
                           housingprices$home_values_90_99kE)) %>%
  
  mutate(price100kto300k = (housingprices$home_values_100_124kE +
                              housingprices$home_values_125_149kE +
                              housingprices$home_values_150_174kE +
                              housingprices$home_values_175_199kE +
                              housingprices$home_values_200_249kE +
                              housingprices$home_values_250_299kE)) %>%
  
  mutate(price300kto500k = (housingprices$home_values_300_399kE +
                              housingprices$home_values_400_499kE)) %>%
  
  mutate(price500kto1mil = (housingprices$home_values_500_749kE +
                              housingprices$home_values_750_999kE)) %>%
  
  mutate(price1milorGreater = (housingprices$home_values_1_1.4mE )) %>%
  
  subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater))



plot1 <- ggplot(housinggrouped, aes(x = NAME, y = LessThan100k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot2 <- ggplot(housinggrouped, aes(x = NAME, y = price100kto300k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot3 <- ggplot(housinggrouped, aes(x = NAME, y = price300kto500k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot4 <- ggplot(housinggrouped, aes(x = NAME, y = price500kto1mil, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot5 <- ggplot(housinggrouped, aes(x = NAME, y = price1milorGreater, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme

housinggraphs2011 <- ggarrange(plot1, plot2, plot3, plot4, plot5,
                               ncol = 3, nrow = 2, common.legend = TRUE) %>%
  annotate_figure(top = "Number of houses by district in 2011")



map1 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = LessThan100k)) +
  coord_sf(datum = NA) +
  plot_theme

map2 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price100kto300k)) +
  coord_sf(datum = NA) +
  plot_theme

map3 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price300kto500k)) +
  coord_sf(datum = NA) +
  plot_theme

map4 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price500kto1mil)) +
  coord_sf(datum = NA) +
  plot_theme

map5 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price1milorGreater)) +
  coord_sf(datum = NA) +
  plot_theme

housingmaps2011 <- ggarrange(map1, map2, map3, map4, map5,
                             ncol = 3, nrow = 2) %>%
  annotate_figure(top = "Number of houses At Specific Price Brackets by district in 2011")






######################### 2010 ##########################


housingwide <- get_rappkdistrict(homevalue2010_2014_var, homes_all, 2010)
rappahannockhousing <- get_rappk(homevalue2010_2014_var, homes_all, 2010)


housingprices <- rappahannockhousing %>% rbind(housingwide)

housingprices$NAME <- factor(as.factor(housingprices$NAME), levels = c("Piedmont", "Stonewall-Hawthorne", "Jackson", "Hampton", "Wakefield", "Rappahannock"))



housing2010 <- housinggrouped <- housingprices %>%
  mutate(LessThan100k = (housingprices$home_values_10klessE + 
                           housingprices$home_values_10_14kE +
                           housingprices$home_values_15_19kE +
                           housingprices$home_values_20_24kE +
                           housingprices$home_values_25_29kE +
                           housingprices$home_values_30_34kE +
                           housingprices$home_values_40_49kE + 
                           housingprices$home_values_50_59kE +
                           housingprices$home_values_60_69kE +
                           housingprices$home_values_70_79kE +
                           housingprices$home_values_80_89kE +
                           housingprices$home_values_90_99kE)) %>%
  
  mutate(price100kto300k = (housingprices$home_values_100_124kE +
                              housingprices$home_values_125_149kE +
                              housingprices$home_values_150_174kE +
                              housingprices$home_values_175_199kE +
                              housingprices$home_values_200_249kE +
                              housingprices$home_values_250_299kE)) %>%
  
  mutate(price300kto500k = (housingprices$home_values_300_399kE +
                              housingprices$home_values_400_499kE)) %>%
  
  mutate(price500kto1mil = (housingprices$home_values_500_749kE +
                              housingprices$home_values_750_999kE)) %>%
  
  mutate(price1milorGreater = (housingprices$home_values_1_1.4mE )) %>%
  
  subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater))



plot1 <- ggplot(housinggrouped, aes(x = NAME, y = LessThan100k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot2 <- ggplot(housinggrouped, aes(x = NAME, y = price100kto300k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot3 <- ggplot(housinggrouped, aes(x = NAME, y = price300kto500k, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot4 <- ggplot(housinggrouped, aes(x = NAME, y = price500kto1mil, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme
plot5 <- ggplot(housinggrouped, aes(x = NAME, y = price1milorGreater, fill = NAME)) +
  geom_col() +
  coord_flip() +
  plot_theme

housinggraphs2010 <- ggarrange(plot1, plot2, plot3, plot4, plot5,
                               ncol = 3, nrow = 2, common.legend = TRUE) %>%
  annotate_figure(top = "Number of houses by district in 2010")



map1 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = LessThan100k)) +
  coord_sf(datum = NA) +
  plot_theme

map2 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price100kto300k)) +
  coord_sf(datum = NA) +
  plot_theme

map3 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price300kto500k)) +
  coord_sf(datum = NA) +
  plot_theme

map4 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price500kto1mil)) +
  coord_sf(datum = NA) +
  plot_theme

map5 <- ggplot(housinggrouped %>% filter(NAME != "Rappahannock")) +
  geom_sf(aes(fill = price1milorGreater)) +
  coord_sf(datum = NA) +
  plot_theme

housingmaps2010 <- ggarrange(map1, map2, map3, map4, map5,
                             ncol = 3, nrow = 2) %>%
  annotate_figure(top = "Number of houses At Specific Price Brackets by district in 2010")


