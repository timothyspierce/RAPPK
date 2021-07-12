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
devtools::install_github("rCarto/osrm")
options(tigris_use_cache = TRUE)
options(tigris_class="sf")

######################################################################
poptotal <- c("Total Population" = "B02001_001")

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

housingwide <- get_acs(geography = "county subdivision",
                       state = 51,
                       county = 157,
                       variables = homevalue_var,
                       summary_var = "B25075_001",
                       year = 2019,
                       output = "wide",
                       geometry = TRUE,
                       keep_geo_vars = TRUE) %>%
  subset(select = -c(NAME.y)) %>%
  rename(NAME = NAME.x)

#Funny thing after doing this
#is finding out that the number of houses that are under 100k are in the double digits
#The low double digits. Like, less than 50.
#Had to consolidate my bottom 3 categories just so this made sense
#
# housinggrouped <- housingwide %>%
#   mutate("Less Than 10k" = housingwide$home_values_10klessE) %>%
#   
#   mutate("10k to 50k" = (housingwide$home_values_10_14kE +
#                            housingwide$home_values_15_19kE +
#                            housingwide$home_values_20_24kE +
#                            housingwide$home_values_25_29kE +
#                            housingwide$home_values_30_34kE +
#                            housingwide$home_values_40_49kE)) %>%
#   
#   mutate("50k to 100k" = (housingwide$home_values_50_59kE +
#                             housingwide$home_values_60_69kE +
#                             housingwide$home_values_70_79kE +
#                             housingwide$home_values_80_89kE +
#                             housingwide$home_values_90_99kE)) %>%
#   
#   mutate("100k to 300k" = (housingwide$home_values_100_124kE +
#                              housingwide$home_values_125_149kE +
#                              housingwide$home_values_150_174kE +
#                              housingwide$home_values_175_199kE +
#                              housingwide$home_values_200_249kE +
#                              housingwide$home_values_250_299kE)) %>% 
#   
#   mutate("300k to 500k" = (housingwide$home_values_300_399kE +
#                              housingwide$home_values_400_499kE)) %>% 
#   
#   mutate("500k to 1mil" = (housingwide$home_values_500_749kE +
#                              housingwide$home_values_750_999kE)) %>% 
#   
#   mutate("1mil or Greater" = (housingwide$home_values_1_1.4mE +
#                                 housingwide$home_values_1.5_1.9mE +
#                                 housingwide$home_values_2mmoreE))


housinggrouped <- housingwide %>%
  mutate("Less Than 100k" = (housingwide$home_values_10klessE + 
                            housingwide$home_values_10_14kE +
                            housingwide$home_values_15_19kE +
                            housingwide$home_values_20_24kE +
                            housingwide$home_values_25_29kE +
                            housingwide$home_values_30_34kE +
                            housingwide$home_values_40_49kE + 
                            housingwide$home_values_50_59kE +
                            housingwide$home_values_60_69kE +
                            housingwide$home_values_70_79kE +
                            housingwide$home_values_80_89kE +
                            housingwide$home_values_90_99kE)) %>%

  mutate("100k to 300k" = (housingwide$home_values_100_124kE +
                             housingwide$home_values_125_149kE +
                             housingwide$home_values_150_174kE +
                             housingwide$home_values_175_199kE +
                             housingwide$home_values_200_249kE +
                             housingwide$home_values_250_299kE)) %>%

  mutate("300k to 500k" = (housingwide$home_values_300_399kE +
                             housingwide$home_values_400_499kE)) %>%

  mutate("500k to 1mil" = (housingwide$home_values_500_749kE +
                             housingwide$home_values_750_999kE)) %>%

  mutate("1mil or Greater" = (housingwide$home_values_1_1.4mE +
                                housingwide$home_values_1.5_1.9mE +
                                housingwide$home_values_2mmoreE))




selectedhousing <- data.frame(NAME = housinggrouped$NAME,
                              'Less Than 100k' = housinggrouped$`Less Than 100k`,
                              `100k to 300k` = housinggrouped$`100k to 300k`,
                              `300k to 500k` = housinggrouped$`300k to 500k`,
                              `500k to 1mil` = housinggrouped$`500k to 1mil`,
                              `1mil or Greater` = housinggrouped$`1mil or Greater`)
selectedhousing %>% 
  add_column(total = c( 7+312+55+85+80, 12+151+102+41+41, 8+91+110+115+44, 10+97+268+168, 11+69+121+104+63)) %>%
  add_row(NAME = "Rappahannock", Less.Than.100k = sum(selectedhousing$Less.Than.100k), X100k.to.300k = sum(selectedhousing$X100k.to.300k), X300k.to.500k = sum(selectedhousing$X300k.to.500k), X500k.to.1mil = sum(selectedhousing$X500k.to.1mil), X1mil.or.Greater = sum(selectedhousing$X1mil.or.Greater), total = (539+347+368+543+368))


# selecthousingflip <- data.frame(t(selectedhousing[-1]))
# colnames(selecthousingflip) <- selectedhousing[,1]


plot1 <- ggplot(selectedhousing, aes(x = NAME, y = Less.Than.100k)) +
  geom_col()
plot2 <- ggplot(selectedhousing, aes(x = NAME, y = X100k.to.300k)) +
  geom_col()
plot3 <- ggplot(selectedhousing, aes(x = NAME, y = X300k.to.500k)) +
  geom_col()
plot4 <- ggplot(selectedhousing, aes(x = NAME, y = X500k.to.1mil)) +
  geom_col()
plot5 <- ggplot(selectedhousing, aes(x = NAME, y = X1mil.or.Greater)) +
  geom_col()

housingplots <- ggarrange(plot1, plot2, plot3, plot4, plot5,
                          ncol = 3, nrow = 2)



########################################################################
