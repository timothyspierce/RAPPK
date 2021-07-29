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



###########################################################

#All of Rapphannock


rappahannockhousing <- get_rappk(homevalue_var, homes_all)
rappahannockhousing <- rappahannockhousing %>%  mutate(LessThan100k = (rappahannockhousing$home_values_10klessE +
                                                                 rappahannockhousing$home_values_10_14kE +
                                                                 rappahannockhousing$home_values_15_19kE +
                                                                 rappahannockhousing$home_values_20_24kE +
                                                                 rappahannockhousing$home_values_25_29kE +
                                                                 rappahannockhousing$home_values_30_34kE +
                                                                 rappahannockhousing$home_values_40_49kE +
                                                                 rappahannockhousing$home_values_50_59kE +
                                                                 rappahannockhousing$home_values_60_69kE +
                                                                 rappahannockhousing$home_values_70_79kE +
                                                                 rappahannockhousing$home_values_80_89kE +
                                                                 rappahannockhousing$home_values_90_99kE)) %>%

  mutate(price100kto300k = (rappahannockhousing$home_values_100_124kE +
                              rappahannockhousing$home_values_125_149kE +
                              rappahannockhousing$home_values_150_174kE +
                              rappahannockhousing$home_values_175_199kE +
                              rappahannockhousing$home_values_200_249kE +
                              rappahannockhousing$home_values_250_299kE)) %>%

  mutate(price300kto500k = (rappahannockhousing$home_values_300_399kE +
                              rappahannockhousing$home_values_400_499kE)) %>%

  mutate(price500kto1mil = (rappahannockhousing$home_values_500_749kE +
                              rappahannockhousing$home_values_750_999kE)) %>%

  mutate(price1milorGreater = (rappahannockhousing$home_values_1_1.4mE +
                                 rappahannockhousing$home_values_1.5_1.9mE +
                                 rappahannockhousing$home_values_2mmoreE))

rappahannockhousing2019 <-  rappahannockhousing %>% subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater, summary_est, populationE, geometry))%>%
  add_column(year = "2019")


rappahannockhousing <- get_rappk(homevalue_var, homes_all, 2018)
rappahannockhousing <- rappahannockhousing %>%  mutate(LessThan100k = (rappahannockhousing$home_values_10klessE +
                                                                         rappahannockhousing$home_values_10_14kE +
                                                                         rappahannockhousing$home_values_15_19kE +
                                                                         rappahannockhousing$home_values_20_24kE +
                                                                         rappahannockhousing$home_values_25_29kE +
                                                                         rappahannockhousing$home_values_30_34kE +
                                                                         rappahannockhousing$home_values_40_49kE +
                                                                         rappahannockhousing$home_values_50_59kE +
                                                                         rappahannockhousing$home_values_60_69kE +
                                                                         rappahannockhousing$home_values_70_79kE +
                                                                         rappahannockhousing$home_values_80_89kE +
                                                                         rappahannockhousing$home_values_90_99kE)) %>%

  mutate(price100kto300k = (rappahannockhousing$home_values_100_124kE +
                              rappahannockhousing$home_values_125_149kE +
                              rappahannockhousing$home_values_150_174kE +
                              rappahannockhousing$home_values_175_199kE +
                              rappahannockhousing$home_values_200_249kE +
                              rappahannockhousing$home_values_250_299kE)) %>%

  mutate(price300kto500k = (rappahannockhousing$home_values_300_399kE +
                              rappahannockhousing$home_values_400_499kE)) %>%

  mutate(price500kto1mil = (rappahannockhousing$home_values_500_749kE +
                              rappahannockhousing$home_values_750_999kE)) %>%

  mutate(price1milorGreater = (rappahannockhousing$home_values_1_1.4mE +
                                 rappahannockhousing$home_values_1.5_1.9mE +
                                 rappahannockhousing$home_values_2mmoreE))

rappahannockhousing2018 <-  rappahannockhousing %>% subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater, summary_est, populationE, geometry))%>%
  add_column(year = "2018")





rappahannockhousing <- get_rappk(homevalue_var, homes_all, 2017)
rappahannockhousing <- rappahannockhousing %>%  mutate(LessThan100k = (rappahannockhousing$home_values_10klessE +
                                                                         rappahannockhousing$home_values_10_14kE +
                                                                         rappahannockhousing$home_values_15_19kE +
                                                                         rappahannockhousing$home_values_20_24kE +
                                                                         rappahannockhousing$home_values_25_29kE +
                                                                         rappahannockhousing$home_values_30_34kE +
                                                                         rappahannockhousing$home_values_40_49kE +
                                                                         rappahannockhousing$home_values_50_59kE +
                                                                         rappahannockhousing$home_values_60_69kE +
                                                                         rappahannockhousing$home_values_70_79kE +
                                                                         rappahannockhousing$home_values_80_89kE +
                                                                         rappahannockhousing$home_values_90_99kE)) %>%

  mutate(price100kto300k = (rappahannockhousing$home_values_100_124kE +
                              rappahannockhousing$home_values_125_149kE +
                              rappahannockhousing$home_values_150_174kE +
                              rappahannockhousing$home_values_175_199kE +
                              rappahannockhousing$home_values_200_249kE +
                              rappahannockhousing$home_values_250_299kE)) %>%

  mutate(price300kto500k = (rappahannockhousing$home_values_300_399kE +
                              rappahannockhousing$home_values_400_499kE)) %>%

  mutate(price500kto1mil = (rappahannockhousing$home_values_500_749kE +
                              rappahannockhousing$home_values_750_999kE)) %>%

  mutate(price1milorGreater = (rappahannockhousing$home_values_1_1.4mE +
                                 rappahannockhousing$home_values_1.5_1.9mE +
                                 rappahannockhousing$home_values_2mmoreE))

rappahannockhousing2017 <-  rappahannockhousing %>% subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater, summary_est, populationE, geometry))%>%
  add_column(year = "2017")


rappahannockhousing <- get_rappk(homevalue_var, homes_all, 2016)
rappahannockhousing <- rappahannockhousing %>%  mutate(LessThan100k = (rappahannockhousing$home_values_10klessE +
                                                                         rappahannockhousing$home_values_10_14kE +
                                                                         rappahannockhousing$home_values_15_19kE +
                                                                         rappahannockhousing$home_values_20_24kE +
                                                                         rappahannockhousing$home_values_25_29kE +
                                                                         rappahannockhousing$home_values_30_34kE +
                                                                         rappahannockhousing$home_values_40_49kE +
                                                                         rappahannockhousing$home_values_50_59kE +
                                                                         rappahannockhousing$home_values_60_69kE +
                                                                         rappahannockhousing$home_values_70_79kE +
                                                                         rappahannockhousing$home_values_80_89kE +
                                                                         rappahannockhousing$home_values_90_99kE)) %>%

  mutate(price100kto300k = (rappahannockhousing$home_values_100_124kE +
                              rappahannockhousing$home_values_125_149kE +
                              rappahannockhousing$home_values_150_174kE +
                              rappahannockhousing$home_values_175_199kE +
                              rappahannockhousing$home_values_200_249kE +
                              rappahannockhousing$home_values_250_299kE)) %>%

  mutate(price300kto500k = (rappahannockhousing$home_values_300_399kE +
                              rappahannockhousing$home_values_400_499kE)) %>%

  mutate(price500kto1mil = (rappahannockhousing$home_values_500_749kE +
                              rappahannockhousing$home_values_750_999kE)) %>%

  mutate(price1milorGreater = (rappahannockhousing$home_values_1_1.4mE +
                                 rappahannockhousing$home_values_1.5_1.9mE +
                                 rappahannockhousing$home_values_2mmoreE))

rappahannockhousing2016 <-  rappahannockhousing %>% subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater, summary_est, populationE, geometry))%>%
  add_column(year = "2016")


rappahannockhousing <- get_rappk(homevalue_var, homes_all, 2015)
rappahannockhousing <- rappahannockhousing %>%  mutate(LessThan100k = (rappahannockhousing$home_values_10klessE +
                                                                         rappahannockhousing$home_values_10_14kE +
                                                                         rappahannockhousing$home_values_15_19kE +
                                                                         rappahannockhousing$home_values_20_24kE +
                                                                         rappahannockhousing$home_values_25_29kE +
                                                                         rappahannockhousing$home_values_30_34kE +
                                                                         rappahannockhousing$home_values_40_49kE +
                                                                         rappahannockhousing$home_values_50_59kE +
                                                                         rappahannockhousing$home_values_60_69kE +
                                                                         rappahannockhousing$home_values_70_79kE +
                                                                         rappahannockhousing$home_values_80_89kE +
                                                                         rappahannockhousing$home_values_90_99kE)) %>%

  mutate(price100kto300k = (rappahannockhousing$home_values_100_124kE +
                              rappahannockhousing$home_values_125_149kE +
                              rappahannockhousing$home_values_150_174kE +
                              rappahannockhousing$home_values_175_199kE +
                              rappahannockhousing$home_values_200_249kE +
                              rappahannockhousing$home_values_250_299kE)) %>%

  mutate(price300kto500k = (rappahannockhousing$home_values_300_399kE +
                              rappahannockhousing$home_values_400_499kE)) %>%

  mutate(price500kto1mil = (rappahannockhousing$home_values_500_749kE +
                              rappahannockhousing$home_values_750_999kE)) %>%

  mutate(price1milorGreater = (rappahannockhousing$home_values_1_1.4mE +
                                 rappahannockhousing$home_values_1.5_1.9mE +
                                 rappahannockhousing$home_values_2mmoreE))

rappahannockhousing2015 <-  rappahannockhousing %>% subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater, summary_est, populationE, geometry))%>%
  add_column(year = "2015")


rappahannockhousing <- get_rappk(homevalue2010_2014_var, homes_all, 2014)
rappahannockhousing <- rappahannockhousing %>%  mutate(LessThan100k = (rappahannockhousing$home_values_10klessE +
                                                                         rappahannockhousing$home_values_10_14kE +
                                                                         rappahannockhousing$home_values_15_19kE +
                                                                         rappahannockhousing$home_values_20_24kE +
                                                                         rappahannockhousing$home_values_25_29kE +
                                                                         rappahannockhousing$home_values_30_34kE +
                                                                         rappahannockhousing$home_values_40_49kE +
                                                                         rappahannockhousing$home_values_50_59kE +
                                                                         rappahannockhousing$home_values_60_69kE +
                                                                         rappahannockhousing$home_values_70_79kE +
                                                                         rappahannockhousing$home_values_80_89kE +
                                                                         rappahannockhousing$home_values_90_99kE)) %>%

  mutate(price100kto300k = (rappahannockhousing$home_values_100_124kE +
                              rappahannockhousing$home_values_125_149kE +
                              rappahannockhousing$home_values_150_174kE +
                              rappahannockhousing$home_values_175_199kE +
                              rappahannockhousing$home_values_200_249kE +
                              rappahannockhousing$home_values_250_299kE)) %>%

  mutate(price300kto500k = (rappahannockhousing$home_values_300_399kE +
                              rappahannockhousing$home_values_400_499kE)) %>%

  mutate(price500kto1mil = (rappahannockhousing$home_values_500_749kE +
                              rappahannockhousing$home_values_750_999kE)) %>%

  mutate(price1milorGreater = (rappahannockhousing$home_values_1_1.4mE))

rappahannockhousing2014 <-  rappahannockhousing %>% subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater, summary_est, populationE, geometry))%>%
  add_column(year = "2014") %>%
  st_zm(drop = TRUE, what = "ZM")


rappahannockhousing2013 <- get_rappk(homevalue2010_2014_var, homes_all, 2013)
rappahannockhousing <- rappahannockhousing %>%  mutate(LessThan100k = (rappahannockhousing$home_values_10klessE +
                                                                         rappahannockhousing$home_values_10_14kE +
                                                                         rappahannockhousing$home_values_15_19kE +
                                                                         rappahannockhousing$home_values_20_24kE +
                                                                         rappahannockhousing$home_values_25_29kE +
                                                                         rappahannockhousing$home_values_30_34kE +
                                                                         rappahannockhousing$home_values_40_49kE +
                                                                         rappahannockhousing$home_values_50_59kE +
                                                                         rappahannockhousing$home_values_60_69kE +
                                                                         rappahannockhousing$home_values_70_79kE +
                                                                         rappahannockhousing$home_values_80_89kE +
                                                                         rappahannockhousing$home_values_90_99kE)) %>%

  mutate(price100kto300k = (rappahannockhousing$home_values_100_124kE +
                              rappahannockhousing$home_values_125_149kE +
                              rappahannockhousing$home_values_150_174kE +
                              rappahannockhousing$home_values_175_199kE +
                              rappahannockhousing$home_values_200_249kE +
                              rappahannockhousing$home_values_250_299kE)) %>%

  mutate(price300kto500k = (rappahannockhousing$home_values_300_399kE +
                              rappahannockhousing$home_values_400_499kE)) %>%

  mutate(price500kto1mil = (rappahannockhousing$home_values_500_749kE +
                              rappahannockhousing$home_values_750_999kE)) %>%

  mutate(price1milorGreater = (rappahannockhousing$home_values_1_1.4mE))

rappahannockhousing2013 <-  rappahannockhousing %>% subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater, summary_est, populationE, geometry))%>%
  add_column(year = "2013")


rappahannockhousing <- get_rappk(homevalue2010_2014_var, homes_all, 2012)
rappahannockhousing <- rappahannockhousing %>%  mutate(LessThan100k = (rappahannockhousing$home_values_10klessE +
                                                                         rappahannockhousing$home_values_10_14kE +
                                                                         rappahannockhousing$home_values_15_19kE +
                                                                         rappahannockhousing$home_values_20_24kE +
                                                                         rappahannockhousing$home_values_25_29kE +
                                                                         rappahannockhousing$home_values_30_34kE +
                                                                         rappahannockhousing$home_values_40_49kE +
                                                                         rappahannockhousing$home_values_50_59kE +
                                                                         rappahannockhousing$home_values_60_69kE +
                                                                         rappahannockhousing$home_values_70_79kE +
                                                                         rappahannockhousing$home_values_80_89kE +
                                                                         rappahannockhousing$home_values_90_99kE)) %>%

  mutate(price100kto300k = (rappahannockhousing$home_values_100_124kE +
                              rappahannockhousing$home_values_125_149kE +
                              rappahannockhousing$home_values_150_174kE +
                              rappahannockhousing$home_values_175_199kE +
                              rappahannockhousing$home_values_200_249kE +
                              rappahannockhousing$home_values_250_299kE)) %>%

  mutate(price300kto500k = (rappahannockhousing$home_values_300_399kE +
                              rappahannockhousing$home_values_400_499kE)) %>%

  mutate(price500kto1mil = (rappahannockhousing$home_values_500_749kE +
                              rappahannockhousing$home_values_750_999kE)) %>%

  mutate(price1milorGreater = (rappahannockhousing$home_values_1_1.4mE))

rappahannockhousing2012 <-  rappahannockhousing %>% subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater, summary_est, populationE, geometry))%>%
  add_column(year = "2012")


rappahannockhousing2011 <- get_rappk(homevalue2010_2014_var, homes_all, 2011)
rappahannockhousing <- rappahannockhousing %>%  mutate(LessThan100k = (rappahannockhousing$home_values_10klessE +
                                                                         rappahannockhousing$home_values_10_14kE +
                                                                         rappahannockhousing$home_values_15_19kE +
                                                                         rappahannockhousing$home_values_20_24kE +
                                                                         rappahannockhousing$home_values_25_29kE +
                                                                         rappahannockhousing$home_values_30_34kE +
                                                                         rappahannockhousing$home_values_40_49kE +
                                                                         rappahannockhousing$home_values_50_59kE +
                                                                         rappahannockhousing$home_values_60_69kE +
                                                                         rappahannockhousing$home_values_70_79kE +
                                                                         rappahannockhousing$home_values_80_89kE +
                                                                         rappahannockhousing$home_values_90_99kE)) %>%

  mutate(price100kto300k = (rappahannockhousing$home_values_100_124kE +
                              rappahannockhousing$home_values_125_149kE +
                              rappahannockhousing$home_values_150_174kE +
                              rappahannockhousing$home_values_175_199kE +
                              rappahannockhousing$home_values_200_249kE +
                              rappahannockhousing$home_values_250_299kE)) %>%

  mutate(price300kto500k = (rappahannockhousing$home_values_300_399kE +
                              rappahannockhousing$home_values_400_499kE)) %>%

  mutate(price500kto1mil = (rappahannockhousing$home_values_500_749kE +
                              rappahannockhousing$home_values_750_999kE)) %>%

  mutate(price1milorGreater = (rappahannockhousing$home_values_1_1.4mE))

rappahannockhousing2011 <-  rappahannockhousing %>% subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater, summary_est, populationE, geometry))%>%
  add_column(year = "2011")


rappahannockhousing <- get_rappk(homevalue2010_2014_var, homes_all, 2010)
rappahannockhousing <- rappahannockhousing %>%  mutate(LessThan100k = (rappahannockhousing$home_values_10klessE +
                                                                         rappahannockhousing$home_values_10_14kE +
                                                                         rappahannockhousing$home_values_15_19kE +
                                                                         rappahannockhousing$home_values_20_24kE +
                                                                         rappahannockhousing$home_values_25_29kE +
                                                                         rappahannockhousing$home_values_30_34kE +
                                                                         rappahannockhousing$home_values_40_49kE +
                                                                         rappahannockhousing$home_values_50_59kE +
                                                                         rappahannockhousing$home_values_60_69kE +
                                                                         rappahannockhousing$home_values_70_79kE +
                                                                         rappahannockhousing$home_values_80_89kE +
                                                                         rappahannockhousing$home_values_90_99kE)) %>%

  mutate(price100kto300k = (rappahannockhousing$home_values_100_124kE +
                              rappahannockhousing$home_values_125_149kE +
                              rappahannockhousing$home_values_150_174kE +
                              rappahannockhousing$home_values_175_199kE +
                              rappahannockhousing$home_values_200_249kE +
                              rappahannockhousing$home_values_250_299kE)) %>%

  mutate(price300kto500k = (rappahannockhousing$home_values_300_399kE +
                              rappahannockhousing$home_values_400_499kE)) %>%

  mutate(price500kto1mil = (rappahannockhousing$home_values_500_749kE +
                              rappahannockhousing$home_values_750_999kE)) %>%

  mutate(price1milorGreater = (rappahannockhousing$home_values_1_1.4mE))

rappahannockhousing2010 <-  rappahannockhousing %>% subset(select = c(GEOID, NAME, LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater, summary_est, populationE, geometry))%>%
  add_column(year = "2010")

rapp_housing2010_2019 <-  rappahannockhousing2019 %>%
  rbind(rappahannockhousing2018) %>%
  rbind(rappahannockhousing2017) %>%
  rbind(rappahannockhousing2016) %>%
  rbind(rappahannockhousing2015) %>%
  rbind(rappahannockhousing2014) %>%
  rbind(rappahannockhousing2013) %>%
  rbind(rappahannockhousing2012) %>%
  rbind(rappahannockhousing2011) %>%
  rbind(rappahannockhousing2010) %>%
  rename(population = populationE) %>%
  pivot_longer(cols = c(LessThan100k, price100kto300k, price300kto500k, price500kto1mil, price1milorGreater), names_to = "homevalues", values_to = "estimated_total") %>%
  st_as_sf()

rapp_housing2010_2019$homevalues <- factor(rapp_housing2010_2019$homevalues, levels = homevalue_vector)
rapp_housing2010_2019 <- rapp_housing2010_2019  %>%  mutate(percent_of_houses = (rapp_housing2010_2019$estimated_total/rapp_housing2010_2019$summary_est * 100))
rapp_housing2010_2019 <- rapp_housing2010_2019 %>% mutate(pop_per_home = estimated_total/population)



ggplot(rapp_housing2010_2019, aes(x = year, y = pop_per_home, group = homevalues, color = homevalues)) +
  geom_line(aes(size = estimated_total)) +
  ylab("Proportion of People Per Home") +
  labs(size = "Number of Homes") +
  ggtitle("Housing Prices (In US Dollars) From 2010 to 2019") +
  scale_color_viridis_d(name = "Home Value Brackets", labels = c("Under $100,000", "$100,000 to $300,000", "$300,000 to 500,000", "$500,000 to 1 Million", "Over 1 Million")) +
  plot_theme


ggplot(rapp_housing2010_2019, aes(x = year, y = estimated_total, group = homevalues, color = homevalues)) +
  geom_line(aes(size = estimated_total)) +
  ylab("Number of Homes") +
  labs(size = "Number of Homes") +
  ggtitle("Housing Prices (In US Dollars) From 2010 to 2019") +
  scale_color_viridis_d(name = "Home Value Brackets", labels = c("Under $100,000", "$100,000 to $300,000", "$300,000 to 500,000", "$500,000 to 1 Million", "Over 1 Million")) +
  plot_theme


ggplot(rapp_housing2010_2019, aes(x = year, y = percent_of_houses, group = homevalues, color = homevalues)) +
  geom_line(aes(size = estimated_total)) +
  ylab("Percentage of Homes") +
  labs(size = "Number of Homes") +
  ggtitle("Housing Prices (In US Dollars) From 2010 to 2019") +
  scale_color_viridis_d(name = "Home Value Brackets", labels = c("Under $100,000", "$100,000 to $300,000", "$300,000 to 500,000", "$500,000 to 1 Million", "Over 1 Million")) +
  plot_theme


