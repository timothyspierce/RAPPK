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

bar_graph_and_medianline <- function(dataset){
  ggplot(dataset, aes(x = NAME, y = estimate, fill = NAME)) +
    geom_col(position = "dodge") + 
    ggtitle("Median Household income") +
    geom_hline(aes(yintercept = median(estimate)), color = "black", size = 1.5, alpha = 0.25) +
    labs(yintercept = "Median") +
    facet_wrap(~variable)} 

#########################################################################

housingwide <- get_acs(geography = "county subdivision",
                       state = 51,
                       county = 157,
                       variables = homevaluesvector,
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




#ACS call for median income
median_income <- get_rapp(medianincome_var, median_income_all)


#Give each zipcode a name
median_income$NAME <- str_replace_all(median_income$NAME, "ZCTA5 23112|ZCTA5 23113|ZCTA5 23114", "Midlothan")
median_income$NAME <- str_replace_all(median_income$NAME, "ZCTA5 23120", "Mosely")
median_income$NAME <- str_replace_all(median_income$NAME, "ZCTA5 23831|ZCTA5 23836", "Chester")
median_income$NAME <- str_replace_all(median_income$NAME, "ZCTA5 23224|ZCTA5 23225|ZCTA5 23234|ZCTA5 23235|ZCTA5 23236|ZCTA5 23237", "N. Chesterfield")
median_income$NAME <- str_replace_all(median_income$NAME, "ZCTA5 23832|ZCTA5 23838", "Chesterfield")
median_income$NAME <- str_replace_all(median_income$NAME, "ZCTA5 23834", "S. Chesterfield")
median_income$NAME <- str_replace_all(median_income$NAME, "ZCTA5 23805|ZCTA5 23804|ZCTA5 23803", "Petersburg")
median_income$NAME <- str_replace_all(median_income$NAME, "ZCTA5 23806", "VSU Campus")

#Factoring these values for useage!
median_income$variable <- factor(medianincomevector, order = TRUE, levels = c(medianincomevector))

grouped_median_income <- median_income %>% group_by(variable)








summary_est

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

bar_graph <- ggplot(incomedf, aes(x = incomebrackets, y = householdincome)) +
    geom_col(position = "dodge") +
  ggtitle("Median Income of the Population by Income Bracket") +
    ylab("Percentage of Population") +
    xlab("Income Bracket")



bar_graph_and_medianline(incomedf)
                
median_income_dollars = c("Median Household Income" = "B19013_001")                                               

incometotal <- rapp_var(median_income_dollars, median_income_dollars) %>%
  subset(select = -c(NAME.y)) %>%
  rename(NAME = NAME.x)

bar_graph_and_medianline(incometotal)