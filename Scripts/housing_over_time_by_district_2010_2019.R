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


######################################################################
poptotal <- c(population = "B02001_001")


#Just gonna slap this at the end of every ggplot for consistency
plot_theme <- theme(plot.title = element_text(hjust = 0.5),
                    axis.text=element_text(size=12),
                    legend.text = element_text(size=12),
                    axis.title.x=element_text(size =13),
                    axis.title.y=element_text(size =13),
                    panel.background = element_blank())

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
                  home_values_2mmore = "B25075_027",
                  poptotal)
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
                           home_values_1_1.4m = "B25075_025",
                           poptotal)

homevalue_vector <- c("LessThan100k", "price100kto300k", "price300kto500k", "price500kto1mil", "price1milorGreater")

get_rappkdistrict  <- function(varcode, summary, year = 2019){get_acs(geography = "county subdivision",
                                                                      state = 51,
                                                                      county = 157,
                                                                      variables = varcode,
                                                                      summary_var = summary,
                                                                      year = year,
                                                                      output = "wide",
                                                                      geometry = TRUE,
                                                                      keep_geo_vars = TRUE) %>%
    rename(NAME = NAME.x)}

get_rappk <- function(varcode, summary, year = 2019){get_acs(geography = "county",
                                                             state = 51,
                                                             county = 157,
                                                             variables = varcode,
                                                             summary_var = summary,
                                                             year = year,
                                                             output = "wide",
                                                             geometry = TRUE,
                                                             keep_geo_vars = TRUE) %>%
    rename(NAME = NAME.x)}

housingwide <- get_rappkdistrict(homevalue_var, homes_all)
rappahannockhousing <- get_rappk(homevalue_var, homes_all)



housingwide2019 <- get_rappkdistrict(homevalue_var, homes_all)
 housingwide2019 <- housingwide2019 %>%  mutate(LessThan100k = (housingwide2019$home_values_10klessE + 
                           housingwide2019$home_values_10_14kE +
                           housingwide2019$home_values_15_19kE +
                           housingwide2019$home_values_20_24kE +
                           housingwide2019$home_values_25_29kE +
                           housingwide2019$home_values_30_34kE +
                           housingwide2019$home_values_40_49kE + 
                           housingwide2019$home_values_50_59kE +
                           housingwide2019$home_values_60_69kE +
                           housingwide2019$home_values_70_79kE +
                           housingwide2019$home_values_80_89kE +
                           housingwide2019$home_values_90_99kE)) %>%
  
  mutate(price100kto300k = (housingwide2019$home_values_100_124kE +
                              housingwide2019$home_values_125_149kE +
                              housingwide2019$home_values_150_174kE +
                              housingwide2019$home_values_175_199kE +
                              housingwide2019$home_values_200_249kE +
                              housingwide2019$home_values_250_299kE)) %>%
  
  mutate(price300kto500k = (housingwide2019$home_values_300_399kE +
                              housingwide2019$home_values_400_499kE)) %>%
  
  mutate(price500kto1mil = (housingwide2019$home_values_500_749kE +
                              housingwide2019$home_values_750_999kE)) %>%
  
  mutate(price1milorGreater = (housingwide2019$home_values_1_1.4mE +
                                 housingwide2019$home_values_1.5_1.9mE +
                                 housingwide2019$home_values_2mmoreE))
  
 housingwide2019 <-  housingwide2019 %>% subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater, summary_est, populationE, geometry))%>% 
   add_column(year = "2019")





housingwide2018 <- get_rappkdistrict(homevalue_var, homes_all, 2018)
housingwide2018 <- housingwide2018 %>%  mutate(LessThan100k = (housingwide2018$home_values_10klessE + 
                                                                 housingwide2018$home_values_10_14kE +
                                                                 housingwide2018$home_values_15_19kE +
                                                                 housingwide2018$home_values_20_24kE +
                                                                 housingwide2018$home_values_25_29kE +
                                                                 housingwide2018$home_values_30_34kE +
                                                                 housingwide2018$home_values_40_49kE + 
                                                                 housingwide2018$home_values_50_59kE +
                                                                 housingwide2018$home_values_60_69kE +
                                                                 housingwide2018$home_values_70_79kE +
                                                                 housingwide2018$home_values_80_89kE +
                                                                 housingwide2018$home_values_90_99kE)) %>%
  
  mutate(price100kto300k = (housingwide2018$home_values_100_124kE +
                              housingwide2018$home_values_125_149kE +
                              housingwide2018$home_values_150_174kE +
                              housingwide2018$home_values_175_199kE +
                              housingwide2018$home_values_200_249kE +
                              housingwide2018$home_values_250_299kE)) %>%
  
  mutate(price300kto500k = (housingwide2018$home_values_300_399kE +
                              housingwide2018$home_values_400_499kE)) %>%
  
  mutate(price500kto1mil = (housingwide2018$home_values_500_749kE +
                              housingwide2018$home_values_750_999kE)) %>%
  
  mutate(price1milorGreater = (housingwide2018$home_values_1_1.4mE +
                                 housingwide2018$home_values_1.5_1.9mE +
                                 housingwide2018$home_values_2mmoreE))

housingwide2018 <-  housingwide2018 %>% subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater, summary_est, populationE, geometry))%>% 
  add_column(year = "2018")






housingwide2017 <- get_rappkdistrict(homevalue_var, homes_all, 2017)
housingwide2017 <- housingwide2017 %>%  mutate(LessThan100k = (housingwide2017$home_values_10klessE + 
                                                                 housingwide2017$home_values_10_14kE +
                                                                 housingwide2017$home_values_15_19kE +
                                                                 housingwide2017$home_values_20_24kE +
                                                                 housingwide2017$home_values_25_29kE +
                                                                 housingwide2017$home_values_30_34kE +
                                                                 housingwide2017$home_values_40_49kE + 
                                                                 housingwide2017$home_values_50_59kE +
                                                                 housingwide2017$home_values_60_69kE +
                                                                 housingwide2017$home_values_70_79kE +
                                                                 housingwide2017$home_values_80_89kE +
                                                                 housingwide2017$home_values_90_99kE)) %>%
  
  mutate(price100kto300k = (housingwide2017$home_values_100_124kE +
                              housingwide2017$home_values_125_149kE +
                              housingwide2017$home_values_150_174kE +
                              housingwide2017$home_values_175_199kE +
                              housingwide2017$home_values_200_249kE +
                              housingwide2017$home_values_250_299kE)) %>%
  
  mutate(price300kto500k = (housingwide2017$home_values_300_399kE +
                              housingwide2017$home_values_400_499kE)) %>%
  
  mutate(price500kto1mil = (housingwide2017$home_values_500_749kE +
                              housingwide2017$home_values_750_999kE)) %>%
  
  mutate(price1milorGreater = (housingwide2017$home_values_1_1.4mE +
                                 housingwide2017$home_values_1.5_1.9mE +
                                 housingwide2017$home_values_2mmoreE))

housingwide2017 <-  housingwide2017 %>% subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater, summary_est, populationE, geometry))%>% 
  add_column(year = "2017")



housingwide2016 <- get_rappkdistrict(homevalue_var, homes_all, 2016)
housingwide2016 <- housingwide2016 %>%  mutate(LessThan100k = (housingwide2016$home_values_10klessE + 
                                                                 housingwide2016$home_values_10_14kE +
                                                                 housingwide2016$home_values_15_19kE +
                                                                 housingwide2016$home_values_20_24kE +
                                                                 housingwide2016$home_values_25_29kE +
                                                                 housingwide2016$home_values_30_34kE +
                                                                 housingwide2016$home_values_40_49kE + 
                                                                 housingwide2016$home_values_50_59kE +
                                                                 housingwide2016$home_values_60_69kE +
                                                                 housingwide2016$home_values_70_79kE +
                                                                 housingwide2016$home_values_80_89kE +
                                                                 housingwide2016$home_values_90_99kE)) %>%
  
  mutate(price100kto300k = (housingwide2016$home_values_100_124kE +
                              housingwide2016$home_values_125_149kE +
                              housingwide2016$home_values_150_174kE +
                              housingwide2016$home_values_175_199kE +
                              housingwide2016$home_values_200_249kE +
                              housingwide2016$home_values_250_299kE)) %>%
  
  mutate(price300kto500k = (housingwide2016$home_values_300_399kE +
                              housingwide2016$home_values_400_499kE)) %>%
  
  mutate(price500kto1mil = (housingwide2016$home_values_500_749kE +
                              housingwide2016$home_values_750_999kE)) %>%
  
  mutate(price1milorGreater = (housingwide2016$home_values_1_1.4mE +
                                 housingwide2016$home_values_1.5_1.9mE +
                                 housingwide2016$home_values_2mmoreE))

housingwide2016 <-  housingwide2016 %>% subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater, summary_est, populationE, geometry)) %>% 
  add_column(year = "2016")


housingwide2015 <- get_rappkdistrict(homevalue_var, homes_all, 2015)
housingwide2015 <- housingwide2015 %>%  mutate(LessThan100k = (housingwide2015$home_values_10klessE + 
                                                                 housingwide2015$home_values_10_14kE +
                                                                 housingwide2015$home_values_15_19kE +
                                                                 housingwide2015$home_values_20_24kE +
                                                                 housingwide2015$home_values_25_29kE +
                                                                 housingwide2015$home_values_30_34kE +
                                                                 housingwide2015$home_values_40_49kE + 
                                                                 housingwide2015$home_values_50_59kE +
                                                                 housingwide2015$home_values_60_69kE +
                                                                 housingwide2015$home_values_70_79kE +
                                                                 housingwide2015$home_values_80_89kE +
                                                                 housingwide2015$home_values_90_99kE)) %>%
  
  mutate(price100kto300k = (housingwide2015$home_values_100_124kE +
                              housingwide2015$home_values_125_149kE +
                              housingwide2015$home_values_150_174kE +
                              housingwide2015$home_values_175_199kE +
                              housingwide2015$home_values_200_249kE +
                              housingwide2015$home_values_250_299kE)) %>%
  
  mutate(price300kto500k = (housingwide2015$home_values_300_399kE +
                              housingwide2015$home_values_400_499kE)) %>%
  
  mutate(price500kto1mil = (housingwide2015$home_values_500_749kE +
                              housingwide2015$home_values_750_999kE)) %>%
  
  mutate(price1milorGreater = (housingwide2015$home_values_1_1.4mE +
                                 housingwide2015$home_values_1.5_1.9mE +
                                 housingwide2015$home_values_2mmoreE))

housingwide2015 <-  housingwide2015 %>% subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater, summary_est, populationE, geometry)) %>% 
  add_column(year = "2015")


housingwide2014 <- get_rappkdistrict(homevalue2010_2014_var, homes_all, 2014)
housingwide2014 <- housingwide2014 %>%
  mutate(LessThan100k = (housingwide2014$home_values_10klessE + 
                           housingwide2014$home_values_10_14kE +
                           housingwide2014$home_values_15_19kE +
                           housingwide2014$home_values_20_24kE +
                           housingwide2014$home_values_25_29kE +
                           housingwide2014$home_values_30_34kE +
                           housingwide2014$home_values_40_49kE + 
                           housingwide2014$home_values_50_59kE +
                           housingwide2014$home_values_60_69kE +
                           housingwide2014$home_values_70_79kE +
                           housingwide2014$home_values_80_89kE +
                           housingwide2014$home_values_90_99kE)) %>%
  
  mutate(price100kto300k = (housingwide2014$home_values_100_124kE +
                              housingwide2014$home_values_125_149kE +
                              housingwide2014$home_values_150_174kE +
                              housingwide2014$home_values_175_199kE +
                              housingwide2014$home_values_200_249kE +
                              housingwide2014$home_values_250_299kE)) %>%
  
  mutate(price300kto500k = (housingwide2014$home_values_300_399kE +
                              housingwide2014$home_values_400_499kE)) %>%
  
  mutate(price500kto1mil = (housingwide2014$home_values_500_749kE +
                              housingwide2014$home_values_750_999kE)) %>%
  
  mutate(price1milorGreater = (housingwide2014$home_values_1_1.4mE ))
  housingwide2014 <-  housingwide2014 %>% subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater, summary_est, populationE, geometry))  %>% 
  add_column(year = "2014") %>% 
  st_zm(drop = TRUE, what = "ZM")



housingwide2013 <- get_rappkdistrict(homevalue2010_2014_var, homes_all, 2013)
housingwide2013 <- housingwide2013 %>%
  mutate(LessThan100k = (housingwide2013$home_values_10klessE + 
                           housingwide2013$home_values_10_14kE +
                           housingwide2013$home_values_15_19kE +
                           housingwide2013$home_values_20_24kE +
                           housingwide2013$home_values_25_29kE +
                           housingwide2013$home_values_30_34kE +
                           housingwide2013$home_values_40_49kE + 
                           housingwide2013$home_values_50_59kE +
                           housingwide2013$home_values_60_69kE +
                           housingwide2013$home_values_70_79kE +
                           housingwide2013$home_values_80_89kE +
                           housingwide2013$home_values_90_99kE)) %>%
  
  mutate(price100kto300k = (housingwide2013$home_values_100_124kE +
                              housingwide2013$home_values_125_149kE +
                              housingwide2013$home_values_150_174kE +
                              housingwide2013$home_values_175_199kE +
                              housingwide2013$home_values_200_249kE +
                              housingwide2013$home_values_250_299kE)) %>%
  
  mutate(price300kto500k = (housingwide2013$home_values_300_399kE +
                              housingwide2013$home_values_400_499kE)) %>%
  
  mutate(price500kto1mil = (housingwide2013$home_values_500_749kE +
                              housingwide2013$home_values_750_999kE)) %>%
  
  mutate(price1milorGreater = (housingwide2013$home_values_1_1.4mE ))
  housingwide2013 <-  housingwide2013 %>% subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater, summary_est, populationE, geometry))  %>% 
  add_column(year = "2013")


housingwide2012 <- get_rappkdistrict(homevalue2010_2014_var, homes_all, 2012)
housingwide2012 <- housingwide2012 %>%
  mutate(LessThan100k = (housingwide2012$home_values_10klessE + 
                           housingwide2012$home_values_10_14kE +
                           housingwide2012$home_values_15_19kE +
                           housingwide2012$home_values_20_24kE +
                           housingwide2012$home_values_25_29kE +
                           housingwide2012$home_values_30_34kE +
                           housingwide2012$home_values_40_49kE + 
                           housingwide2012$home_values_50_59kE +
                           housingwide2012$home_values_60_69kE +
                           housingwide2012$home_values_70_79kE +
                           housingwide2012$home_values_80_89kE +
                           housingwide2012$home_values_90_99kE)) %>%
  
  mutate(price100kto300k = (housingwide2012$home_values_100_124kE +
                              housingwide2012$home_values_125_149kE +
                              housingwide2012$home_values_150_174kE +
                              housingwide2012$home_values_175_199kE +
                              housingwide2012$home_values_200_249kE +
                              housingwide2012$home_values_250_299kE)) %>%
  
  mutate(price300kto500k = (housingwide2012$home_values_300_399kE +
                              housingwide2012$home_values_400_499kE)) %>%
  
  mutate(price500kto1mil = (housingwide2012$home_values_500_749kE +
                              housingwide2012$home_values_750_999kE)) %>%
  
  mutate(price1milorGreater = (housingwide2012$home_values_1_1.4mE ))
  housingwide2012 <-  housingwide2012 %>% subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater, summary_est, populationE, geometry))  %>% 
  add_column(year = "2012")


housingwide2011 <- get_rappkdistrict(homevalue2010_2014_var, homes_all, 2011)
housingwide2011 <- housingwide2011 %>%
  mutate(LessThan100k = (housingwide2011$home_values_10klessE + 
                           housingwide2011$home_values_10_14kE +
                           housingwide2011$home_values_15_19kE +
                           housingwide2011$home_values_20_24kE +
                           housingwide2011$home_values_25_29kE +
                           housingwide2011$home_values_30_34kE +
                           housingwide2011$home_values_40_49kE + 
                           housingwide2011$home_values_50_59kE +
                           housingwide2011$home_values_60_69kE +
                           housingwide2011$home_values_70_79kE +
                           housingwide2011$home_values_80_89kE +
                           housingwide2011$home_values_90_99kE)) %>%
  
  mutate(price100kto300k = (housingwide2011$home_values_100_124kE +
                              housingwide2011$home_values_125_149kE +
                              housingwide2011$home_values_150_174kE +
                              housingwide2011$home_values_175_199kE +
                              housingwide2011$home_values_200_249kE +
                              housingwide2011$home_values_250_299kE)) %>%
  
  mutate(price300kto500k = (housingwide2011$home_values_300_399kE +
                              housingwide2011$home_values_400_499kE)) %>%
  
  mutate(price500kto1mil = (housingwide2011$home_values_500_749kE +
                              housingwide2011$home_values_750_999kE)) %>%
  
  mutate(price1milorGreater = (housingwide2011$home_values_1_1.4mE ))
  housingwide2011 <-  housingwide2011 %>% subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater, summary_est, populationE, geometry))  %>% 
  add_column(year = "2011")


housingwide2010 <- get_rappkdistrict(homevalue2010_2014_var, homes_all, 2010)
housingwide2010 <- housingwide2010 %>%
  mutate(LessThan100k = (housingwide2010$home_values_10klessE + 
                           housingwide2010$home_values_10_14kE +
                           housingwide2010$home_values_15_19kE +
                           housingwide2010$home_values_20_24kE +
                           housingwide2010$home_values_25_29kE +
                           housingwide2010$home_values_30_34kE +
                           housingwide2010$home_values_40_49kE + 
                           housingwide2010$home_values_50_59kE +
                           housingwide2010$home_values_60_69kE +
                           housingwide2010$home_values_70_79kE +
                           housingwide2010$home_values_80_89kE +
                           housingwide2010$home_values_90_99kE)) %>%
  
  mutate(price100kto300k = (housingwide2010$home_values_100_124kE +
                              housingwide2010$home_values_125_149kE +
                              housingwide2010$home_values_150_174kE +
                              housingwide2010$home_values_175_199kE +
                              housingwide2010$home_values_200_249kE +
                              housingwide2010$home_values_250_299kE)) %>%
  
  mutate(price300kto500k = (housingwide2010$home_values_300_399kE +
                              housingwide2010$home_values_400_499kE)) %>%
  
  mutate(price500kto1mil = (housingwide2010$home_values_500_749kE +
                              housingwide2010$home_values_750_999kE)) %>%
  
  mutate(price1milorGreater = (housingwide2010$home_values_1_1.4mE ))
  housingwide2010 <-  housingwide2010 %>% subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater, summary_est, populationE, geometry))  %>% 
  add_column(year = "2010")



housing2010_2019 <-  housingwide2019 %>% 
  rbind(housingwide2018) %>% 
  rbind(housingwide2017) %>% 
  rbind(housingwide2016) %>% 
  rbind(housingwide2015) %>% 
  rbind(housingwide2014) %>% 
  rbind(housingwide2013) %>% 
  rbind(housingwide2012) %>% 
  rbind(housingwide2011) %>% 
  rbind(housingwide2010) %>%
  rename(population = populationE) %>%
  pivot_longer(cols = c(LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater), names_to = "homevalues", values_to = "estimated_total") %>% 
  st_as_sf()

housing2010_2019$homevalues <- factor(housing2010_2019$homevalues, levels = homevalue_vector) 
housing2010_2019 <- housing2010_2019  %>%  mutate(percent_of_houses = (housing2010_2019$estimated_total/housing2010_2019$summary_est * 100))
housing2010_2019 <- housing2010_2019 %>% mutate(pop_per_home = estimated_total/population)

View(housing2010_2019)

saveRDS(housing2010_2019, "shiny_app/data/housing2010_2019_by_district.Rds")

ggplot(housing2010_2019, aes(x = year, y = pop_per_home, group = homevalues, color = homevalues)) +
  geom_line(aes(size = estimated_total)) +
  facet_wrap(~NAME) +
  labs(size = "Number of Homes") +
  ylab("Proportion of People Per Home") +
  ggtitle("Housing Prices (In US Dollars) From 2010 to 2019") +
  scale_color_viridis_d(name = "Home Value Brackets") +
  plot_theme


ggplot(housing2010_2019, aes(x = year, y = estimated_total, group = homevalues, color = homevalues)) +
  geom_line(aes(size = estimated_total)) +
  ylab("Number of Homes") +
  facet_wrap(~NAME) +
  labs(size = "Number of Homes") +
  ggtitle("Housing Prices (In US Dollars) From 2010 to 2019") +
  scale_color_viridis_d(name = "Home Value Brackets") +
  plot_theme


ggplot(housing2010_2019, aes(x = year, y = percent_of_houses, group = homevalues, color = homevalues)) +
  geom_line(aes(size = estimated_total)) +
  ylab("Percentage of Homes") +
  facet_wrap(~NAME) +
  labs(size = "Number of Homes") +
  ggtitle("Housing Prices (In US Dollars) From 2010 to 2019") +
  scale_color_viridis_d(name = "Home Value Brackets") +
  plot_theme



