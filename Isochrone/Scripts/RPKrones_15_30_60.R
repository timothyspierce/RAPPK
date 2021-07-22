setwd("C:/Users/Leo Allen/Desktop/Isochrones/isochrones")  #set the working directory
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


#TRAVELTIME; install this in R console (NOT R STUDIO)!!!
#Install the latest version of this package by entering the following in R:
#  install.packages("remotes")
#remotes::install_github("tlorusso/traveltimeR")



#getting population and maps of surrounding of page from acs



#I registered for my personal key from the website - Leo
myACSkey <- "2580514e97d888fe585f59b4e328fce92342fe8f"

#show available variables for ACS survey
acs5 <- load_variables(2018, "acs5", cache=T)

# B19013_001 - MedianIncome
# B01003_001 - Total Population


#FUNCTIONS:

# 1. "acs_tables" calls "get_acs" (from tidycensus) on a vector of table names. It returns a dataframe of
# all the tables bound together.  The function requires a vector of table names,
# a census API key, and a geographical unit.  The user can add other parameters as well.

acs_tables<-function(tables,key,geography,...){
  acs_data<-NULL
  for(i in 1:length(tables)){
    data<-get_acs(geography = geography,
                  table = tables[i],
                  key = key,
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


#NATIONAL AND Page DATA

# Variables
# B19013_001 - Median Income
# B01003_001 - Total Population

tables<-c("B01003","B19013")
years<-c(2018)
colnames=c("Census_tract","County","State")

# Pull ACS data for Page only
acs_RPK<-acs_years_tables(tables=tables,
                          years=years,
                          key=myACSkey,
                          geography="tract",
                          state="VA",
                          county="Rappahannock",
                          NAME_col_names = colnames)
# Rename Variable Columns
acs_RPK <- acs_RPK%>%
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

# Get Page County outline
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

#RPK_dist_outline<-get_acs(geography = "district",
#                          state="VA",
#                          district=c("Jackson district", "Wakefield district",
#                                   "Stonewall-Hawthorne district", "Hampton district", "Piedmont district"),
#                          variables = "B19058_002",
#                          survey = "acs5",
#                          key = myACSkey,
#                          year=2019,
 #                         output = "wide",
#                          show_call = T,
#                          geometry = T,
#                          keep_geo_vars = T)%>%
 # select(COUNTYFP,geometry)

#Center of the County
CenterPop_county <- read.csv("C:/Users/Leo Allen/Desktop/Isochrones/CenPop2010_Mean_CO51.txt", header =T)

centerPop_RPK <- CenterPop_county %>% filter(COUNAME %in% c("Rappahannock")
) #%>% filter(COUNTYFP != 620)



#travel time, I did this once to generate center RDS file
#to get the api and the id I visited the website and signed up:
#https://traveltime.com/docs/api/overview/getting-keys

traveltime_api <- "7bbbad489d637c38b2bea80e1e413721"
traveltime_id <- "c1fae682"

#For 60 mins
#indices = c(2,4,9)
#pop_centroid_RPK_iso_60 <- traveltime_map(appId= traveltime_id,
#                                        apiKey = traveltime_api,
#                                       location=c(centerPop_RPK$LATITUDE[1],centerPop_RPK$LONGITUDE[1]),
#                                        traveltime=3600,
#                                        type="driving",
#                                        departure="2020-08-07T08:00:00+01:00")
# we save each object as an RDS file because the api limits the amount of calls you can do in a given period of time and because we don't want to waste our time looping through this whenever we want to work.
#saveRDS(pop_centroid_RPK_iso_60, file = paste0('pop_centroid_RPK_iso_60','.RDS'))
#this particular isochrone is for a 10 minute travel window

#POP CENTROID
#pop_centroid_RPK_60 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/pop_centroid_RPK_iso_60.RDS")

pop_centroid_RPK_iso_60 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/pop_centroid_RPK_iso_60.RDS")
st_crs(pop_centroid_RPK_iso_60) = 4326


#For 30 mins
#indices = c(2,4,9)
#pop_centroid_RPK_iso_30 <- traveltime_map(appId= traveltime_id,
#                                        apiKey = traveltime_api,
#                                       location=c(centerPop_RPK$LATITUDE[1],centerPop_RPK$LONGITUDE[1]),
#                                        traveltime=1800,
#                                        type="driving",
#                                        departure="2020-08-07T08:00:00+01:00")
# we save each object as an RDS file because the api limits the amount of calls you can do in a given period of time and because we don't want to waste our time looping through this whenever we want to work.
#saveRDS(pop_centroid_RPK_iso_30, file = paste0('pop_centroid_RPK_iso_30','.RDS'))
#this particular isochrone is for a 10 minute travel window

#POP CENTROID
#pop_centroid_RPK_30 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/pop_centroid_RPK_iso_30.RDS")

pop_centroid_RPK_iso_30 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/pop_centroid_RPK_iso_30.RDS")
st_crs(pop_centroid_RPK_iso_30) = 4326



#For 15 mins
indices = c(2,4,9)
#pop_centroid_RPK_iso_15 <- traveltime_map(appId= traveltime_id,
#                                        apiKey = traveltime_api,
#                                       location=c(centerPop_RPK$LATITUDE[1],centerPop_RPK$LONGITUDE[1]),
#                                        traveltime=900,
#                                        type="driving",
#                                        departure="2020-08-07T08:00:00+01:00")
# we save each object as an RDS file because the api limits the amount of calls you can do in a given period of time and because we don't want to waste our time looping through this whenever we want to work.
#saveRDS(pop_centroid_RPK_iso_15, file = paste0('pop_centroid_RPK_iso_15','.RDS'))
#this particular isochrone is for a 10 minute travel window

#POP CENTROID
#pop_centroid_RPK_15 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/pop_centroid_RPK_iso_15.RDS")

pop_centroid_RPK_iso_15 <- readRDS("C:/Users/Leo Allen/Desktop/Isochrones/pop_centroid_RPK_iso_15.RDS")
st_crs(pop_centroid_RPK_iso_15) = 4326






# Read in all files file

mapVA  <- st_read("C:/Users/Leo Allen/Downloads/tl_2019_51_tract/tl_2019_51_tract.shp",
                  stringsAsFactors = FALSE)

map_and_data <- inner_join(mapVA, acs_RPK_area, by = "GEOID")

#Services
Adultcare <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/assistedAdultDisabilityElderly.xlsx", sheet=1)
colnames(Adultcare)[7] <- "Longitude"
colnames(Adultcare)[8] <- "Latitude"


#Assistedlvg <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/assistedliving.xlsx")
#colnames(Assistedlvg)[7] <- "Longitude"
#colnames(Assistedlvg)[8] <- "Latitude"


#Dental <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/dental.xlsx")
#colnames(Dental)[7] <- "Longitude"
#colnames(Dental)[8] <- "Latitude"
#

#disability_cent <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/disabilitycent.xlsx")
#colnames(disability_cent)[7] <- "Longitude"
#colnames(disability_cent)[8] <- "Latitude"


Food_Clothing <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/FoodClothing.xlsx")
colnames(Food_Clothing)[7] <- "Longitude"
colnames(Food_Clothing)[8] <- "Latitude"


entertain <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/Entertainment.xlsx")
colnames(entertain)[7] <- "Longitude"
colnames(entertain)[8] <- "Latitude"


Hospital <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/hospitalsHealthDentalFreeClinics.xlsx")
colnames(Hospital)[7] <- "Longitude"
colnames(Hospital)[8] <- "Latitude"


#independent_living <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/indeplvgfaci.xlsx")
#colnames(independent_living)[7] <- "Longitude"
#colnames(independent_living)[8] <- "Latitude"

education <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/education.xlsx")
colnames(education)[7] <- "Longitude"
colnames(education)[8] <- "Latitude"

professional <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/professional.xlsx")
colnames(professional)[7] <- "Longitude"
colnames(professional)[8] <- "Latitude"

recreation <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/recreation.xlsx")
colnames(recreation)[7] <- "Longitude"
colnames(recreation)[8] <- "Latitude"

#healthcare <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/healthcare.xlsx")
#colnames(healthcare)[7] <- "Longitude"
#colnames(healthcare)[8] <- "Latitude"

#elderly <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/elderly.xlsx")
#colnames(elderly)[7] <- "Longitude"
#colnames(elderly)[8] <- "Latitude"

transit <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/transit.xlsx")
colnames(transit)[7] <- "Longitude"
colnames(transit)[8] <- "Latitude"

banks <- read_excel("C:/Users/Leo Allen/Desktop/Isochrones/isochrones/Services/banks.xlsx")
colnames(transit)[7] <- "Longitude"
colnames(transit)[8] <- "Latitude"

#making the map

mypalette <- colorNumeric(palette="viridis", map_and_data$Total_Population)

map_with_all_point <- leaflet() %>%
  addTiles() %>%
  addPolygons(data=map_and_data,color = mypalette(map_and_data$Total_Population),
              smoothFactor = 0.2, fillOpacity=.6, weight = 1,stroke = F, 
              label=paste(" county name ", map_and_data$NAME ,", Value: ",map_and_data$Total_Population))%>%
  addLegend(pal = mypalette,position = "topleft",values = map_and_data$Total_Population,
            
            opacity = .6,title= paste("Total Population")) %>%
  
  addPolylines(data = RPK_area_outline, color = "black", opacity = 1, weight = 1)       %>%
  addPolylines(data = RPK_outline, color = "red", opacity = 2, weight = 2 ) %>%
  addPolygons(data = pop_centroid_RPK_iso_60 , color = "slategray",
              opacity = 1, weight = 1, fillColor = "white",fillOpacity = .1)%>%
  addLegend(colors = "slategray", labels = "60 Minute Drive Boundary") %>%
  addPolygons(data = pop_centroid_RPK_iso_30 , color = "blue",
              opacity = 1, weight = 1, fillColor = "white",fillOpacity = .1)%>%
  addLegend(colors = "blue", labels = "30 Minute Drive Boundary") %>%
  addPolygons(data = pop_centroid_RPK_iso_15 , color = "green",
              opacity = 1, weight = 1, fillColor = "white",fillOpacity = .1)%>%
  addLegend(colors = "green", labels = "15 Minute Drive Boundary") %>%
  
  
  addCircleMarkers(centerPop_RPK,lat = centerPop_RPK$LATITUDE, lng= centerPop_RPK$LONGITUDE,
                   radius =  4,
                   color = "red",
                   stroke = TRUE, fillOpacity = 1
  ) %>%
  addLegend(colors = "red", labels = "Rappahannock Population Centroid") %>%
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
  #addCircleMarkers(disability_cent,lat = disability_cent$Latitude ,
  #                 lng = disability_cent$Longitude,
  #                 radius =  3,
  #                 color = "pink",
  #                 stroke = FALSE, fillOpacity = 1
  #) %>%
  #addLegend(colors = "pink", labels = "Disability centers")%>%
  #addCircleMarkers(Dental,lat = Dental$Latitude ,
  #                 lng = Dental$Longitude,
   #                radius =  3,
  #                 color = "yellow",
  #                 stroke = FALSE, fillOpacity = 1
  #) %>%
  #addLegend(colors = "yellow", labels = "Dental")%>%
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
  #addCircleMarkers(Free_clinic,lat = Free_clinic$Latitude ,
  #                 lng = Free_clinic$Longitude,
  #                 radius =  3,
  #                 color = "black",
  #                 stroke = FALSE, fillOpacity = 1
  #)%>%
  #addLegend(colors = "black", labels = "Free Clinic")%>%
  #addCircleMarkers(independent_living,lat = independent_living$Latitude ,
  #                 lng = independent_living$Longitude,
  #                 radius =  3,
  #                 color = "purple",
  #                 stroke = FALSE, fillOpacity = 1
  #)%>%
  #addLegend(colors = "purple", labels = "Independent Living Facilities")%>%
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
  #addCircleMarkers(healthcare,lat = healthcare$Latitude ,
  #                 lng = healthcare$Longitude,
  #                 radius =  3,
  #                 color = "cyan",
  #                 stroke = FALSE, fillOpacity = 1
  #)%>%
  #addLegend(colors = "cyan", labels = "Healthcare")%>%
  #addCircleMarkers(elderly,lat = elderly$Latitude ,
  #                 lng = elderly$Longitude,
  #                 radius =  3,
  #                 color = "deeppink",
  #                 stroke = FALSE, fillOpacity = 1
  #)%>%
  #addLegend(colors = "deeppink", labels = "Elderly")%>%
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
  addLegend(colors = "deeppink", labels = "Banks")


#Show the map
map_with_all_point





#Write up
#To understand a general suite of services available in Rappahannock, we used 
#publicly available demographic and infrastructure data to provide an overview 
#of the services and amenities in Rappahannock.


#In many respects, Rappahannock County is uniquely endowed with built amenities 
#attractive to businesses (William and Lamb, 2010). It is situated at the 
#intersection of two major interstates, and it is within a six-to-eight-hour 
#drive of most of the population in the United States. As the map shows, it also 
#has easy access to rail and other supporting infrastructure (e.g., powerplants) 
#for commerce and manufacturing. From an "access to major markets" perspective, 
#Wythe is an attractive location for both light and heavy industry.
#https://www.rappahannock.com/business-directory