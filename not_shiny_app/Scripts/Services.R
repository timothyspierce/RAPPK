#setwd("C:/Users/Leo Allen/Desktop/Isochrones/isochrones")  #set the working directory

library(RColorBrewer)
library(traveltime)
library(tidyverse)
library(tidycensus)
library(sf)
library(osmdata)
library(leaflet)
library(sp)
library(purrr)
library(mapview)
library(osrm)
library(rmapzen)
library(rgdal)
library(ggplot2)
library(scales)
library(nycflights13)
library(readxl)
#webshot::install_phantomjs()

################################################################################################################################################################
#BELOW WE ARE LOADING THE DATA FROM THE ACS DATABASE

#TRAVELTIME; install this in R console (NOT R STUDIO)!!!
#Install the latest version of this package by entering the following in R:
#install.packages("remotes")
#remotes::install_github("tlorusso/traveltimeR")


#getting population and maps of surrounding of page from acs


#I registered for my personal key from the website
myACSkey <- "2580514e97d888fe585f59b4e328fce92342fe8f"

#show available variables for ACS survey
acs5 <- load_variables(2018, "acs5", cache=T)

# B19013_001 - MedianIncome
# B01003_001 - Total Population

################################################################################################################################################################

#FUNCTIONS:

# 1. "acs_tables" calls "get_acs" (from tidycensus) on a vector of table names. It returns a dataframe of
# all the tables bound together.  The function requires a vector of table names,
# a census API key, and a geographical unit.  The user can add other parameters as well.

acs_tables<-function(tables,key,geography,...){
  acs_data<-NULL
  for(i in 1:length(tables)){
    data<-get_acs(geography = geography,
                  table = tables[i],
                  key = myACSkey,
                  show_call = T,
                  cache_table=T,
                  ...
    )
    acs_data<-rbind(acs_data,data.frame(data))
  }
  return(acs_data)
}


# 2. "acs_wide" cleans the data returned from a census API call.  More specifically,
# it separates the variable column into separate variables, and it separates "NAME" into
# different columns with pre-defined column names (NAME_col_names). The function also
# drops the "margin of error" column.

acs_wide<-function(data,NAME_col_names){
  data%>%
    select (-moe)%>%
    pivot_wider(names_from = variable,values_from=estimate)%>%
    separate(NAME, into=NAME_col_names, sep = ", ")
}


#3. acs_years retrieves individual variables (or a list of variables) across a series of years.
acs_years<-function(years,key,geography,...){
  acs_data<-NULL
  for(i in 1:length(years)){
    acs<-get_acs(geography = geography,
                 #variables = vars,
                 key = key,
                 year=years[i],
                 output = "wide",
                 show_call = T,
                 geometry = F,
                 ...)
    acs_data<-(rbind(acs_data,data.frame(acs)))
  }
  acs_data<-cbind(acs_data,year=rep((years),each=length(unique(acs_data$GEOID))))
  return(acs_data)
}


#4. "acs_years_tables" uses two previously defined functions (acs_tables and acs_wide) to return multiple
# variable tables across multiple years in one single tibble.  A couple of notes: the way that
# get_acs handles variables before 2013 varies, so this function only works for 2013 and after.
# For variable tables before 2013, use acs_tables to pull individual sets of tables.  Also, I have
# not included "geometry" in the function.  If the user includes geometry, he/she may need
# to modify the call to acs_wide.


acs_years_tables<-function(tables,years,key,geography,NAME_col_names,...){
  acs_data<-NULL
  for (j in 1:length(years)){
    acs<-acs_tables(tables=tables,year=years[j],key=key,geography = geography,...)
    year<-rep(years[j],times=length(acs$GEOID))
    acs_years2<-cbind(year,data.frame(acs))
    acs_data<-(rbind(acs_data,acs_years2))
  }
  acs_data<-acs_wide(acs_data,NAME_col_names = NAME_col_names)
  return(acs_data)
}
################################################################################################################################################################

#BELOW WE ARE LOADING THE VARIABLES

#NATIONAL AND Rappahannock DATA
# Variables
# B19013_001 - Median Income
# B01003_001 - Total Population

tables<-c("B01003","B19013")
years<-c(2018)
colnames=c("Census_tract","County","State")

# Pull ACS data for RPK only
acs_RPK<-acs_years_tables(tables=tables,
                          years=years,
                          key=myACSkey,
                          geography="tract",
                          state="VA",
                          county="Rappahannock",
                          NAME_col_names = colnames)


acs_RPK_dist<-acs_years_tables(tables=tables,
                               years=years,
                               key=myACSkey,
                               geography="county subdivision",
                               state="VA",
                               county="Rappahannock",
                               NAME_col_names = colnames)


# Rename Variable Columns
acs_RPK <- acs_RPK%>%
  rename(
    Median_Income = B19013_001,
    Total_Population = B01003_001
  )



acs_RPK_dist <- acs_RPK_dist%>%
  rename(
    Median_Income = B19013_001,
    Total_Population = B01003_001
  )



# Surrounding Counties
acs_RPK_area<-acs_years_tables(tables=tables,
                               years=years,
                               key= myACSkey,
                               geography="tract",
                               state="VA",
                               county=c("Rappahannock county"
                                        #, "Culpeper County", "Madison county","Page county",
                                        #"Warren county", "Clarke county", "Fauquier county",
                                        #"Orange county", "Fredericksburg"
                               ),
                               NAME_col_names = colnames)

# Rename Variable Columns
acs_RPK_area <- acs_RPK_area %>%
  rename(
    Median_Income = B19013_001,
    Total_Population = B01003_001
  )

################################################################################################################################################################

#BELOW WE ARE GETTING THE OUTLINES FOR THE MAP

# Get VA County Outlines
va_sf<-get_acs(geography = "county",
               state="VA",
               county=c("Rappahannock county", "Culpeper County", "Madison county","Page county",
                        "Warren county", "Clarke county", "Fauquier county",
                        "Orange county", "Fredericksburg"),
               variables = "B19058_002",
               survey = "acs5",
               key = myACSkey,
               year=2019,
               output = "wide",
               show_call = T,
               geometry = T,
               keep_geo_vars = T)%>%
  select(COUNTYFP,geometry)

# Get RPK County outline
RPK_outline<-get_acs(geography = "county",
                     state="VA",
                     county=c("Rappahannock county"),
                     variables = "B19058_002",
                     survey = "acs5",
                     key = myACSkey,
                     year=2019,
                     output = "wide",
                     show_call = T,
                     geometry = T,
                     keep_geo_vars = T)%>%
  select(COUNTYFP,geometry)

RPK_area_outline<-get_acs(geography = "county",
                          state="VA",
                          county=c("Culpeper County", "Madison county","Page county",
                                   "Warren county", "Clarke county", "Fauquier county",
                                   "Orange county", "Fredericksburg"),
                          variables = "B19058_002",
                          survey = "acs5",
                          key = myACSkey,
                          year=2019,
                          output = "wide",
                          show_call = T,
                          geometry = T,
                          keep_geo_vars = T)%>%
  select(COUNTYFP,geometry)

################################################################################################################################################################
RPK_dist_outline<-get_acs(geography = "county subdivision",
                          state="VA",
                          county=c("Rappahannock county"),
                          variables = "B19058_002",
                          survey = "acs5",
                          key = myACSkey,
                          year=2019,
                          output = "wide",
                          show_call = T,
                          geometry = T,
                          keep_geo_vars = T)%>%
  select(COUNTYFP,geometry)
################################################################################################################################################################

#BELOW WE ARE SAVING THE DATA ABOVE AS AN RDS FORMAT AND WE CAN READ THEM WITHOUT HAVING TO GO THROUGH THE STEPS ABOVE

#Save
#saveRDS(acs5, file = paste0('acs5','.RDS'))
acs5 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/acs5.RDS")

#saveRDS(acs_RPK, file = paste0('acs_RPK','.RDS'))
acs_RPK <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/acs_RPK.RDS")

#saveRDS(acs_RPK_dist, file = paste0('acs_RPK_dist','.RDS'))
acs_RPK_dist <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/acs_RPK_dist.RDS")

#saveRDS(acs_RPK_area, file = paste0('acs_RPK_area','.RDS'))
acs_RPK_area <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/acs_RPK_area.RDS")

#saveRDS(va_sf, file = paste0('va_sf','.RDS'))
va_sf <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/va_sf.RDS")

#saveRDS(RPK_outline, file = paste0('RPK_outline','.RDS'))
RPK_outline <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/RPK_outline.RDS")

#saveRDS(RPK_area_outline, file = paste0('RPK_area_outline','.RDS'))
RPK_area_outline <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/RPK_area_outline.RDS")

#saveRDS(RPK_dist_outline, file = paste0('RPK_dist_outline','.RDS'))
RPK_dist_outline <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/RPK_dist_outline.RDS")
################################################################################################################################################################

#BELOW WE ARE READING THE POPULATION CENTROID FOR THE COUNTY


#Centroid of the County
CenterPop_county <- read.csv("C:/Users/Leo Allen/Desktop/Isochrones/CenPop2010_Mean_CO51.txt", header =T)

centerPop_RPK <- CenterPop_county %>% filter(COUNAME %in% c("Rappahannock")
) #%>% filter(COUNTYFP == 157)
pop_centroid_RPK_iso_15 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/pop_centroid_RPK_iso_15.RDS")
pop_centroid_RPK_iso_30 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/pop_centroid_RPK_iso_30.RDS")


#BELOW WE ARE READING THE CENTROID FOR THE DISTRICTS

#Centroid of the Districts
#Obtained from:
#https://www.census.gov/geographies/reference-files/time-series/geo/centers-population.html

CenterPop_dist <- read.csv("C:/Users/Leo Allen/Desktop/Isochrones/CenPop2010_Mean_CO51_157.txt", header =T)

centerPop_RPK_dist <- CenterPop_dist %>%  filter(COUNTYFP == 157)
#%>% filter(COUNAME %in% c("Rappahannock")
#) 
################################################################################################################################################################


#BELOW WE HAVE SEPARATED THE FILE ABOVE INTO THE 5 SEPARATE DISTRICTS BECAUSE THE TRAVEL TIME LIMITS OUR REQUEST SO WE RUN IT ONE DISTRICT AT A TIME


#write.csv(centerPop_RPK_dist, file="centerPop_RPK_dist.csv")
CenterPop_dist1 <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/centerPop_RPK_dist.xlsx", sheet=1)
colnames(CenterPop_dist1)[8] <- "LONGITUDE"
colnames(CenterPop_dist1)[7] <- "LATITUDE"

CenterPop_dist2 <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/centerPop_RPK_dist.xlsx", sheet=2)
colnames(CenterPop_dist2)[8] <- "LONGITUDE"
colnames(CenterPop_dist2)[7] <- "LATITUDE"

CenterPop_dist3 <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/centerPop_RPK_dist.xlsx", sheet=3)
colnames(CenterPop_dist3)[8] <- "LONGITUDE"
colnames(CenterPop_dist3)[7] <- "LATITUDE"

CenterPop_dist4 <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/centerPop_RPK_dist.xlsx", sheet=4)
colnames(CenterPop_dist4)[8] <- "LONGITUDE"
colnames(CenterPop_dist4)[7] <- "LATITUDE"

CenterPop_dist5 <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/centerPop_RPK_dist.xlsx", sheet=5)
colnames(CenterPop_dist5)[8] <- "LONGITUDE"
colnames(CenterPop_dist5)[7] <- "LATITUDE"


################################################################################################################################################################

#BELOW IS THE TRAVEL TIME CODES FOR THE 5 DISTRICTS EACH FOR 15 AND 30, we BASICALLY REPLICATED THE CODES FOR EACH OF THE DISTRICTS

#travel time, I did this once to generate center RDS file
#to get the api and the id I visited the website and signed up:
#the trial expires after about 2 weeks.
#https://traveltime.com/docs/api/overview/getting-keys

traveltime_api <- "7bbbad489d637c38b2bea80e1e413721"
traveltime_id <- "c1fae682"

###################################################################################################################################
#INDIVIDUAL DISTRICTS
#1

#For 30 mins
#indices = c(2,4,9)
#pop_centroid_RPK_dist_1_iso_30 <- traveltime_map(appId= traveltime_id,
#                                       apiKey = traveltime_api,
#                                      location=c(CenterPop_dist1$LATITUDE[1],CenterPop_dist1$LONGITUDE[1]),
#                                        traveltime=1800,
#                                        type="driving",
#                                       departure="2020-08-07T08:00:00+01:00")
# we save each object as an RDS file because the api limits the amount of calls you can do in a given period of time and because we don't want to waste our time looping through this whenever we want to work.
#saveRDS(pop_centroid_RPK_dist_1_iso_30, file = paste0('pop_centroid_RPK_dist_1_iso_30','.RDS'))
#this particular isochrone is for a 10 minute travel window

#POP CENTROID
pop_centroid_RPK_dist_1_iso_30 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/pop_centroid_RPK_dist_1_iso_30.RDS")
st_crs(pop_centroid_RPK_dist_1_iso_30) = 4326



#For 15 mins
#indices = c(2,4,9)
#pop_centroid_RPK_dist_1_iso_15 <- traveltime_map(appId= traveltime_id,
#                                       apiKey = traveltime_api,
#                                       location=c(CenterPop_dist1$LATITUDE[1],CenterPop_dist1$LONGITUDE[1]),
#                                        traveltime=900,
#                                       type="driving",
#                                       departure="2020-08-07T08:00:00+01:00")
# we save each object as an RDS file because the api limits the amount of calls you can do in a given period of time and because we don't want to waste our time looping through this whenever we want to work.
#saveRDS(pop_centroid_RPK_dist_1_iso_15, file = paste0('pop_centroid_RPK_dist_1_iso_15','.RDS'))
#this particular isochrone is for a 10 minute travel window

#POP CENTROID
pop_centroid_RPK_dist_1_iso_15 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/pop_centroid_RPK_dist_1_iso_15.RDS")
st_crs(pop_centroid_RPK_dist_1_iso_15) = 4326

#########################################################################################################################
#2

#For 30 mins
#indices = c(2,4,9)
#pop_centroid_RPK_dist_2_iso_30 <- traveltime_map(appId= traveltime_id,
#                                       apiKey = traveltime_api,
#                                      location=c(CenterPop_dist2$LATITUDE[1],CenterPop_dist2$LONGITUDE[1]),
#                                       traveltime=1800,
#                                        type="driving",
#                                       departure="2020-08-07T08:00:00+01:00")
# we save each object as an RDS file because the api limits the amount of calls you can do in a given period of time and because we don't want to waste our time looping through this whenever we want to work.
#saveRDS(pop_centroid_RPK_dist_2_iso_30, file = paste0('pop_centroid_RPK_dist_2_iso_30','.RDS'))
#this particular isochrone is for a 10 minute travel window

#POP CENTROID
pop_centroid_RPK_dist_2_iso_30 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/pop_centroid_RPK_dist_2_iso_30.RDS")
st_crs(pop_centroid_RPK_dist_2_iso_30) = 4326

#For 15 mins
#indices = c(2,4,9)
#pop_centroid_RPK_dist_2_iso_15 <- traveltime_map(appId= traveltime_id,
#                                        apiKey = traveltime_api,
#                                     location=c(CenterPop_dist2$LATITUDE[1],CenterPop_dist2$LONGITUDE[1]),
#                                        traveltime=900,
#                                        type="driving",
#                                        departure="2020-08-07T08:00:00+01:00")
# we save each object as an RDS file because the api limits the amount of calls you can do in a given period of time and because we don't want to waste our time looping through this whenever we want to work.
#saveRDS(pop_centroid_RPK_dist_2_iso_15, file = paste0('pop_centroid_RPK_dist_2_iso_15','.RDS'))
#this particular isochrone is for a 10 minute travel window

#POP CENTROID
pop_centroid_RPK_dist_2_iso_15 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/pop_centroid_RPK_dist_2_iso_15.RDS")
st_crs(pop_centroid_RPK_dist_2_iso_15) = 4326

#########################################################################################################################
#3

#For 30 mins
#indices = c(2,4,9)
#pop_centroid_RPK_dist_3_iso_30 <- traveltime_map(appId= traveltime_id,
#                                        apiKey = traveltime_api,
#                                       location=c(CenterPop_dist3$LATITUDE[1],CenterPop_dist3$LONGITUDE[1]),
#                                        traveltime=1800,
#                                       type="driving",
#                                        departure="2020-08-07T08:00:00+01:00")
# we save each object as an RDS file because the api limits the amount of calls you can do in a given period of time and because we don't want to waste our time looping through this whenever we want to work.
#saveRDS(pop_centroid_RPK_dist_3_iso_30, file = paste0('pop_centroid_RPK_dist_3_iso_30','.RDS'))
#this particular isochrone is for a 10 minute travel window

#POP CENTROID
pop_centroid_RPK_dist_3_iso_30 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/pop_centroid_RPK_dist_3_iso_30.RDS")
st_crs(pop_centroid_RPK_dist_3_iso_30) = 4326



#For 15 mins
#indices = c(2,4,9)
#pop_centroid_RPK_dist_3_iso_15 <- traveltime_map(appId= traveltime_id,
#                                        apiKey = traveltime_api,
#                                       location=c(CenterPop_dist3$LATITUDE[1],CenterPop_dist3$LONGITUDE[1]),
#                                        traveltime=900,
#                                        type="driving",
#                                        departure="2020-08-07T08:00:00+01:00")
# we save each object as an RDS file because the api limits the amount of calls you can do in a given period of time and because we don't want to waste our time looping through this whenever we want to work.
#saveRDS(pop_centroid_RPK_dist_3_iso_15, file = paste0('pop_centroid_RPK_dist_3_iso_15','.RDS'))
#this particular isochrone is for a 10 minute travel window

#POP CENTROID
pop_centroid_RPK_dist_3_iso_15 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/pop_centroid_RPK_dist_3_iso_15.RDS")
st_crs(pop_centroid_RPK_dist_3_iso_15) = 4326

#########################################################################################################################
#4

#For 30 mins
#indices = c(2,4,9)
#pop_centroid_RPK_dist_4_iso_30 <- traveltime_map(appId= traveltime_id,
#                                        apiKey = traveltime_api,
#                                       location=c(CenterPop_dist4$LATITUDE[1],CenterPop_dist4$LONGITUDE[1]),
#                                        traveltime=1800,
#                                       type="driving",
#                                        departure="2021-08-07T08:00:00+01:00")
# we save each object as an RDS file because the api limits the amount of calls you can do in a given period of time and because we don't want to waste our time looping through this whenever we want to work.
#saveRDS(pop_centroid_RPK_dist_4_iso_30, file = paste0('pop_centroid_RPK_dist_4_iso_30','.RDS'))
#this particular isochrone is for a 10 minute travel window

#POP CENTROID
pop_centroid_RPK_dist_4_iso_30 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/pop_centroid_RPK_dist_4_iso_30.RDS")
st_crs(pop_centroid_RPK_dist_4_iso_30) = 4326



#For 15 mins
#indices = c(2,4,9)
#pop_centroid_RPK_dist_4_iso_15 <- traveltime_map(appId= traveltime_id,
#                                        apiKey = traveltime_api,
#                                       location=c(CenterPop_dist4$LATITUDE[1],CenterPop_dist4$LONGITUDE[1]),
#                                        traveltime=900,
#                                       type="driving",
#                                        departure="2021-07-07T08:00:00+01:00")
# we save each object as an RDS file because the api limits the amount of calls you can do in a given period of time and because we don't want to waste our time looping through this whenever we want to work.
#saveRDS(pop_centroid_RPK_dist_4_iso_15, file = paste0('pop_centroid_RPK_dist_4_iso_15','.RDS'))
#this particular isochrone is for a 10 minute travel window

#POP CENTROID
pop_centroid_RPK_dist_4_iso_15 <- readRDS("C:/Users/Leo Allen/Desktop/pop_centroid_RPK_dist_4_iso_15.RDS")
st_crs(pop_centroid_RPK_dist_4_iso_15) = 4326


################################################################################################################################################################
#########################################################################################################################

#NOT NEEDED!!!
#BECAUSE WE SEE THE SMALL ISOCHRONE FOR ONE OF THE DISTRICTS WE HAVE CREATED ADDITIONAL TIMES FOR THAT TO CHECK THE ISSUE
#This section is just to create different times to check for sensitivity since it looked different in the final map
#For 20 mins
#indices = c(2,4,9)
#pop_centroid_RPK_dist_4_iso_20 <- traveltime_map(appId= traveltime_id,
#                                        apiKey = traveltime_api,
#                                       location=c(CenterPop_dist4$LATITUDE[1],CenterPop_dist4$LONGITUDE[1]),
#                                        traveltime=1200,
#                                       type="driving",
#                                        departure="2020-08-07T08:00:00+01:00")
# we save each object as an RDS file because the api limits the amount of calls you can do in a given period of time and because we don't want to waste our time looping through this whenever we want to work.
#saveRDS(pop_centroid_RPK_dist_4_iso_20, file = paste0('pop_centroid_RPK_dist_4_iso_20','.RDS'))
#this particular isochrone is for a 10 minute travel window

#POP CENTROID
pop_centroid_RPK_dist_4_iso_20 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/pop_centroid_RPK_dist_4_iso_20.RDS")
st_crs(pop_centroid_RPK_dist_4_iso_20) = 4326

#For 25 mins
#indices = c(2,4,9)
#pop_centroid_RPK_dist_4_iso_25 <- traveltime_map(appId= traveltime_id,
#                                                 apiKey = traveltime_api,
#                                                 location=c(CenterPop_dist4$LATITUDE[1],CenterPop_dist4$LONGITUDE[1]),
#                                                 traveltime=1500,
#                                                 type="driving",
#                                                 departure="2020-08-07T08:00:00+01:00")
# we save each object as an RDS file because the api limits the amount of calls you can do in a given period of time and because we don't want to waste our time looping through this whenever we want to work.
#saveRDS(pop_centroid_RPK_dist_4_iso_25, file = paste0('pop_centroid_RPK_dist_4_iso_25','.RDS'))
#this particular isochrone is for a 10 minute travel window

#POP CENTROID
pop_centroid_RPK_dist_4_iso_25 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/pop_centroid_RPK_dist_4_iso_25.RDS")
st_crs(pop_centroid_RPK_dist_4_iso_25) = 4326

#For 40 mins
#indices = c(2,4,9)
#pop_centroid_RPK_dist_4_iso_40 <- traveltime_map(appId= traveltime_id,
#                                                 apiKey = traveltime_api,
#                                                 location=c(CenterPop_dist4$LATITUDE[1],CenterPop_dist4$LONGITUDE[1]),
#                                                 traveltime=2400,
#                                                 type="driving",
#                                                 departure="2020-08-07T08:00:00+01:00")
# we save each object as an RDS file because the api limits the amount of calls you can do in a given period of time and because we don't want to waste our time looping through this whenever we want to work.
#saveRDS(pop_centroid_RPK_dist_4_iso_40, file = paste0('pop_centroid_RPK_dist_4_iso_40','.RDS'))
#this particular isochrone is for a 10 minute travel window

#POP CENTROID
pop_centroid_RPK_dist_4_iso_40 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/pop_centroid_RPK_dist_4_iso_40.RDS")
st_crs(pop_centroid_RPK_dist_4_iso_40) = 4326


#For 50 mins
#indices = c(2,4,9)
#pop_centroid_RPK_dist_4_iso_50 <- traveltime_map(appId= traveltime_id,
#                                                 apiKey = traveltime_api,
#                                                 location=c(CenterPop_dist4$LATITUDE[1],CenterPop_dist4$LONGITUDE[1]),
#                                                 traveltime=3000,
#                                                 type="driving",
#                                                 departure="2020-08-07T08:00:00+01:00")
# we save each object as an RDS file because the api limits the amount of calls you can do in a given period of time and because we don't want to waste our time looping through this whenever we want to work.
#saveRDS(pop_centroid_RPK_dist_4_iso_50, file = paste0('pop_centroid_RPK_dist_4_iso_50','.RDS'))
#this particular isochrone is for a 10 minute travel window

#POP CENTROID
pop_centroid_RPK_dist_4_iso_50 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/pop_centroid_RPK_dist_4_iso_50.RDS")
st_crs(pop_centroid_RPK_dist_4_iso_50) = 4326
#########################################################################################################################
################################################################################################################################################################

#BELOW IS THE CONTINUATION OF THE TRAVEL TIME FOR THE LAST DISTRICT 4
#5

#For 30 mins
#indices = c(2,4,9)
#pop_centroid_RPK_dist_5_iso_30 <- traveltime_map(appId= traveltime_id,
#                                        apiKey = traveltime_api,
#                                       location=c(CenterPop_dist5$LATITUDE[1],CenterPop_dist5$LONGITUDE[1]),
#                                       traveltime=1800,
#                                       type="driving",
#                                        departure="2020-08-07T08:00:00+01:00")
# we save each object as an RDS file because the api limits the amount of calls you can do in a given period of time and because we don't want to waste our time looping through this whenever we want to work.
#saveRDS(pop_centroid_RPK_dist_5_iso_30, file = paste0('pop_centroid_RPK_dist_5_iso_30','.RDS'))
#this particular isochrone is for a 10 minute travel window

#POP CENTROID
pop_centroid_RPK_dist_5_iso_30 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/pop_centroid_RPK_dist_5_iso_30.RDS")
st_crs(pop_centroid_RPK_dist_5_iso_30) = 4326



#For 15 mins
#indices = c(2,4,9)
#pop_centroid_RPK_dist_5_iso_15 <- traveltime_map(appId= traveltime_id,
#                                       apiKey = traveltime_api,
#                                      location=c(CenterPop_dist5$LATITUDE[1],CenterPop_dist5$LONGITUDE[1]),
#                                        traveltime=900,
#                                        type="driving",
#                                        departure="2020-08-07T08:00:00+01:00")
# we save each object as an RDS file because the api limits the amount of calls you can do in a given period of time and because we don't want to waste our time looping through this whenever we want to work.
#saveRDS(pop_centroid_RPK_dist_5_iso_15, file = paste0('pop_centroid_RPK_dist_5_iso_15','.RDS'))
#this particular isochrone is for a 10 minute travel window

#POP CENTROID
pop_centroid_RPK_dist_5_iso_15 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/pop_centroid_RPK_dist_5_iso_15.RDS")
st_crs(pop_centroid_RPK_dist_5_iso_15) = 4326


################################################################################################################################################################
#########################################################################################################################

#NOT NEEDED!!!
#THIS IS THE CENTER COORDINATES FROM GOOGLE MAPS; TO CHECK THE ISSUE FOR THE TRAVEL TIMES IF WE CHANGE THE DISTRICT WITH THE ISSUE
#This section is just to create different times to check for sensitivity since it looked different in the final map
#This uses random coordinates from google maps
#PIEDMONT
centerPop_RPK_dist_Pied <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/districts.xlsx", sheet=6)
colnames(centerPop_RPK_dist_Pied)[7] <- "LONGITUDE"
colnames(centerPop_RPK_dist_Pied)[8] <- "LATITUDE"


pop_centroid_RPK_dist_Pied_iso_30 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/pop_centroid_RPK_dist_Pied_iso_30.RDS")
st_crs(pop_centroid_RPK_dist_Pied_iso_30) = 4326

pop_centroid_RPK_dist_Pied_iso_15 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/pop_centroid_RPK_dist_Pied_iso_15.RDS")
st_crs(pop_centroid_RPK_dist_Pied_iso_15) = 4326
#########################################################################################################################
################################################################################################################################################################


#Map and shape files into one file
# Read in all files file
mapVA  <- st_read("C:/Users/Leo Allen/Downloads/tl_2019_51_tract/tl_2019_51_tract.shp",
                  stringsAsFactors = FALSE)

map_and_data <- inner_join(mapVA, acs_RPK_area, by = "GEOID")

################################################################################################################################################################

#BELOW WE ARE READING THE MAP AND THE SERVICES FROM OUR EXCEL
#Services
Adultcare <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/assistedAdultDisabilityElderly.xlsx", sheet=1)
colnames(Adultcare)[7] <- "Longitude"
colnames(Adultcare)[8] <- "Latitude"

Food_Clothing <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/FoodClothing.xlsx")
colnames(Food_Clothing)[7] <- "Longitude"
colnames(Food_Clothing)[8] <- "Latitude"

entertain <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/Entertainment.xlsx")
colnames(entertain)[7] <- "Longitude"
colnames(entertain)[8] <- "Latitude"

Hospital <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/hospitalsHealthDentalFreeClinics.xlsx")
colnames(Hospital)[7] <- "Longitude"
colnames(Hospital)[8] <- "Latitude"

education <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/education.xlsx")
colnames(education)[7] <- "Longitude"
colnames(education)[8] <- "Latitude"

professional <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/professional.xlsx")
colnames(professional)[7] <- "Longitude"
colnames(professional)[8] <- "Latitude"

recreation <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/recreation.xlsx")
colnames(recreation)[7] <- "Longitude"
colnames(recreation)[8] <- "Latitude"

transit <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/transit.xlsx")
colnames(transit)[7] <- "Longitude"
colnames(transit)[8] <- "Latitude"

banks <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/banks.xlsx")
colnames(transit)[7] <- "Longitude"
colnames(transit)[8] <- "Latitude"

################################################################################################################################################################

#REORDER THE DISTRICTS FROM THE INITIAL "acs_RPK_dist" TO MATCH THE MAP
#I am reordering the districts to correct them in the map, without this the map
#does not have the right figures.
acs_RPK_dist <- read.csv("C:/Users/Leo Allen/Desktop/Isochrones/Distrpk.csv", header =T)


################################################################################################################################################################

#BELOW WE PLOT OUR MAP; USING THE DISTRICT CENTROID COORDINATES FROM THE ACS AND SERVICES

#CREATING A PALLETE FROM THE DISTRICT POPULATION
mypalette <- colorNumeric(palette="viridis", centerPop_RPK_dist$POPULATION)

#making the map
map_with_all_point <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("Esri") %>%
  
  addPolygons(data=RPK_dist_outline,color = mypalette(centerPop_RPK_dist$POPULATION),
              smoothFactor = 0.2, fillOpacity=.6, weight = 1,stroke = F, 
              label=paste(" ", acs_RPK_dist$Census_tract,", Population: ",centerPop_RPK_dist$POPULATION), 
  )%>%
  addLegend(pal = mypalette,position = "topleft",values = centerPop_RPK_dist$POPULATION,
            opacity = .6,title= paste("Total Population")) %>%
  
  addPolylines(data = RPK_dist_outline, color = "black", opacity = 2, weight = 2,)       %>%
  addPolylines(data = RPK_area_outline, color = "black", opacity = 1, weight = 1)       %>%
  addPolylines(data = RPK_outline, color = "black", opacity = 2, weight = 2 ) %>%
  
  ###Adding the travel time bounds
  addPolygons(data = pop_centroid_RPK_dist_1_iso_15 , color = "green",
              opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Wakefield Driving")%>%
  #addLegend(colors = "slategray", labels = "60 Minute Drive Boundary") %>%
  addPolygons(data = pop_centroid_RPK_dist_1_iso_30, color = "red",
              opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Wakefield Driving")%>%
  #addLegend(colors = "blue", labels = "30 Minute Drive Boundary") %>%
  
  
  addPolygons(data = pop_centroid_RPK_dist_2_iso_15 , color = "green",
              opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Hampton Driving")%>%
  #addLegend(colors = "blue", labels = "30 Minute Drive Boundary") %>%
  addPolygons(data = pop_centroid_RPK_dist_2_iso_30 , color = "red",
              opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Hampton Driving")%>%
  # addLegend(colors = "green", labels = "15 Minute Drive Boundary") %>%
  
  
  addPolygons(data = pop_centroid_RPK_dist_3_iso_15 , color = "green",
              opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Jackson Driving")%>%
  #addLegend(colors = "green", labels = "15 Minute Drive Boundary") %>%
  addPolygons(data = pop_centroid_RPK_dist_3_iso_30 , color = "red",
              opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Jackson Driving")%>%
  #addLegend(colors = "slategray", labels = "60 Minute Drive Boundary") %>%
  
  
  addPolygons(data = pop_centroid_RPK_dist_4_iso_15 , color = "green",
              opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Piedmont Driving")%>%
  #addLegend(colors = "blue", labels = "30 Minute Drive Boundary") %>%
  addPolygons(data = pop_centroid_RPK_dist_4_iso_30 , color = "red",
              opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Piedmont Driving")%>%
  # addLegend(colors = "green", labels = "15 Minute Drive Boundary") %>%
  
  addPolygons(data = pop_centroid_RPK_dist_5_iso_15 , color = "green",
              opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Stonewall-Hawthorne Driving")%>%
  #addLegend(colors = "blue", labels = "30 Minute Drive Boundary") %>%
  addPolygons(data = pop_centroid_RPK_dist_5_iso_30 , color = "red",
              opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Stonewall-Hawthorne Driving")%>%
  # addLegend(colors = "green", labels = "15 Minute Drive Boundary") %>%
  
  addPolygons(data = pop_centroid_RPK_iso_30 , color = "red",
              opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Rappahannock Driving")%>%
  # addLegend(colors = "blue", labels = "30 Minute Drive Boundary") %>%
  addPolygons(data = pop_centroid_RPK_iso_15 , color = "green",
              opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Rappahannock Driving")%>%
  # addLegend(colors = "green", labels = "15 Minute Drive Boundary") %>%
  
  
  addLayersControl(overlayGroups = c("Rappahannock Driving", "Wakefield Driving", 
                                     "Piedmont Driving", "Hampton Driving", "Jackson Driving", "Stonewall-Hawthorne Driving"), 
                   options = layersControlOptions(collapsed = FALSE), position = "topright") %>%
  hideGroup("Rappahannock Driving")%>% 
  hideGroup("Wakefield Driving")%>% 
  hideGroup("Piedmont Driving")%>%
  hideGroup("Hampton Driving")%>%
  hideGroup("Jackson Driving")%>%
  hideGroup("Stonewall-Hawthorne Driving")%>%
  
  
  addCircleMarkers(centerPop_RPK_dist,lat = centerPop_RPK_dist$LATITUDE, lng= centerPop_RPK_dist$LONGITUDE,
                   radius =  4,
                   color = "red",
                   stroke = TRUE, fillOpacity = 1,
  ) %>%
  addLegend(colors = "red", labels = "District Population Centroid") %>%
  
  #addCircleMarkers(CenterPop_dist4,lat = CenterPop_dist4$LATITUDE, lng= CenterPop_dist4$LONGITUDE,
  #                 radius =  4,
  #                 color = "white",
  #                 stroke = TRUE, fillOpacity = 1,
  #) %>%
  
  
  addCircleMarkers(centerPop_RPK,lat = centerPop_RPK$LATITUDE, lng= centerPop_RPK$LONGITUDE,
                   radius =  4,
                   color = "white",
                   stroke = TRUE, fillOpacity = 1
  ) %>%
  addLegend(colors = "white", labels = " Rappahannock Population Centroid") %>%
  
  #addCircleMarkers(centerPop_RPK_dist_Pied,lat = centerPop_RPK_dist_Pied$LATITUDE, lng= centerPop_RPK_dist_Pied$LONGITUDE,
  #                 radius =  4,
  #                 color = "white",
  #                 stroke = TRUE, fillOpacity = 1
  #) %>%
  #addLegend(colors = "white", labels = " Piedmont Population Centroid") %>%
  #addPolygons(data = pop_centroid_RPK_dist_Pied_iso_30 , color = "red",
  #            opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "PP Driving")%>%
  # addLegend(colors = "blue", labels = "30 Minute Drive Boundary") %>%
  #addPolygons(data = pop_centroid_RPK_dist_Pied_iso_15 , color = "green",
#            opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "PP Driving")%>%
# addLegend(colors = "green", labels = "15 Minute Drive Boundary") %>%


###SERVICES
addCircleMarkers(entertain,lat = entertain$Latitude, lng= entertain$Longitude,
                 radius =  4,
                 color = "#55DDE0",
                 stroke = TRUE, fillOpacity = 1
) %>%
  addLegend(colors = "#55DDE0", labels = "Entertainment Centers") %>%
  
  addCircleMarkers(Adultcare,lat = Adultcare$Latitude ,
                   lng = Adultcare$Longitude,
                   radius =  3,
                   color = "magenta",
                   stroke = TRUE, fillOpacity = 1
  ) %>%
  addLegend(colors = "magenta", labels = "Adultcare/Assisted Living/Disability Center/Elderly") %>%
  
  addCircleMarkers(Food_Clothing,lat = Food_Clothing$Latitude ,
                   lng = Food_Clothing$Longitude,
                   radius =  3,
                   color = "tomato",
                   stroke = TRUE, fillOpacity = 1
  ) %>%
  addLegend(colors = "tomato", labels = "Food & Clothing")%>%
  addCircleMarkers(Hospital,lat = Hospital$Latitude ,
                   lng = Hospital$Longitude,
                   radius =  3,
                   color = "orange",
                   stroke = TRUE, fillOpacity = 1
  ) %>%
  addLegend(colors = "orange", labels = "Hospitals/Healthcare/Dental/FreeClinics")%>%
  
  addCircleMarkers(education,lat = education$Latitude ,
                   lng = education$Longitude,
                   radius =  3,
                   color = "black",
                   stroke = TRUE, fillOpacity = 1
  )%>%
  addLegend(colors = "black", labels = "Education")%>%
  addCircleMarkers(professional,lat = professional$Latitude ,
                   lng = professional$Longitude,
                   radius =  3,
                   color = "brown",
                   stroke = TRUE, fillOpacity = 1
  )%>%
  addLegend(colors = "brown", labels = "Professional")%>%
  addCircleMarkers(recreation,lat = recreation$Latitude ,
                   lng = recreation$Longitude,
                   radius =  3,
                   color = "purple",
                   stroke = TRUE, fillOpacity = 1
  )%>%
  addLegend(colors = "purple", labels = "Recreation")%>%
  
  addCircleMarkers(transit,lat = transit$Latitude ,
                   lng = transit$Longitude,
                   radius =  3,
                   color = "darkgreen",
                   stroke = TRUE, fillOpacity = 1
  )%>%
  addLegend(colors = "darkgreen", labels = "Transit")%>%
  addCircleMarkers(banks,lat = banks$Latitude ,
                   lng = banks$Longitude,
                   radius =  3,
                   color = "deeppink",
                   stroke = TRUE, fillOpacity = 1
  )%>%
  addLegend(colors = "deeppink", labels = "Banks")%>% 
  setView(lat = centerPop_RPK$LATITUDE, lng= centerPop_RPK$LONGITUDE, zoom =10)


#Show the map
map_with_all_point
################################################################################################################################################################