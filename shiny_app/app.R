# Load Packages ---------------------------------------------------------------
library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(sjmisc)
library(shinythemes)
library(DT)
library(data.table)
library(rsconnect)
library(shinycssloaders)
library(readxl)
library(readr)
library(stringr)
library(shinyjs)
library(mapdata)
library(htmlwidgets)
library(leafpop)
library(lattice)
library(ggplot2)
library(htmltools)
library(tigris)
library(leaflegend)
library(dplyr)
library(ggplotify)
library(grid)
library(gridExtra)

prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
options(spinner.color = prettyblue, spinner.color.background = '#ffffff', spinner.size = 3, spinner.type = 7)

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

# data -----------------------------------------------------------
# Read in Demographic Data ----------------------------------------------------------
rappk_ageGroups <- read.csv("data/TableB01001FiveYearEstimates/rappkAgeGroups.csv")
va_ageGroups <- read.csv("data/TableB01001FiveYearEstimates/vaAgeGroups.csv")
counties_median_age<- read.csv("data/TableB01001FiveYearEstimates/countyMedianAge.csv")
sub_median_age<- read.csv("data/TableB01001FiveYearEstimates/subdivisionMedianAge.csv")
county_dep<- read.csv("data/TableB01001FiveYearEstimates/countyAgeDependency.csv")
sub_dep<- read.csv("data/TableB01001FiveYearEstimates/subdivisionAgeDependency.csv")
va_rappk_dep<- read.csv("data/TableB01001FiveYearEstimates/rappkAgeDependency.csv")
rappage_timeseries <- readRDS("data/rapp_age_time_series.Rda")
agetimeseries <- readRDS("data/district_age_time_series.Rds")
intByIncome <- read.csv("data/TableS2801FiveYearEstimates/internetIncome.csv")
income2010_2019 <- readRDS("data/income2010_2019.Rda")
householdSize <- read.csv("data/TableS2501FiveYearEstimates/householdSize.csv")
own <- read.csv("data/TableS2501FiveYearEstimates/ownerOccupied.csv")
rent <- read.csv("data/TableS2501FiveYearEstimates/renterOccupied.csv")
rappk_veh <- read.csv("data/TableS2501FiveYearEstimates/vehiclesHousehold.csv")
county_veh <- read.csv("data/TableS2501FiveYearEstimates/vehiclesHouseholdCounty.csv")

compDist <- read.csv("data/TableS2801FiveYearEstimates/districtComputers.csv")
intDist <- read.csv("data/TableS2801FiveYearEstimates/districtInternet.csv")
# Read in Housing Data ----------------------------------------------------------
          
# Read in Traffic Data ----------------------------------------------------------
traffic_2010 <- read_excel("data/traffic/traffic_data_2010.xls")
traffic_2011 <- read_excel("data/traffic/traffic_data_2011.xls")
traffic_2012 <- read_excel("data/traffic/traffic_data_2012.xls")
traffic_2013 <- read_excel("data/traffic/traffic_data_2013.xls")
traffic_2014 <- read_excel("data/traffic/traffic_data_2014.xls")
traffic_2015 <- read_excel("data/traffic/traffic_data_2015.xls")
traffic_2016 <- read_excel("data/traffic/traffic_data_2016.xls")
traffic_2017 <- read_excel("data/traffic/traffic_data_2017.xls")
traffic_2018 <- read_excel("data/traffic/traffic_data_2018.xls")
traffic_2019 <- read_excel("data/traffic/traffic_data_2019.xls")
traffic_2020 <- read_excel("data/traffic/traffic_data_2020.xlsx") 
coordinates <- read_excel("data/traffic/traffic_coordinates.xlsx")

# Read in Service Data ----------------------------------------------------------
#Save
#saveRDS(acs5, file = paste0('acs5','.RDS'))
acs5 <- readRDS("data/Isochrone/acs5.RDS")

#saveRDS(acs_RPK, file = paste0('acs_RPK','.RDS'))
acs_RPK <- readRDS("data/Isochrone/acs_RPK.RDS")

#saveRDS(acs_RPK_dist, file = paste0('acs_RPK_dist','.RDS'))
acs_RPK_dist <- readRDS("data/Isochrone/acs_RPK_dist.RDS")

#saveRDS(acs_RPK_area, file = paste0('acs_RPK_area','.RDS'))
acs_RPK_area <- readRDS("data/Isochrone/acs_RPK_area.RDS")

#saveRDS(va_sf, file = paste0('va_sf','.RDS'))
va_sf <- readRDS("data/Isochrone/va_sf.RDS")

#saveRDS(RPK_outline, file = paste0('RPK_outline','.RDS'))
RPK_outline <- readRDS("data/Isochrone/RPK_outline.RDS")

#saveRDS(RPK_area_outline, file = paste0('RPK_area_outline','.RDS'))
RPK_area_outline <- readRDS("data/Isochrone/RPK_area_outline.RDS")

#saveRDS(RPK_dist_outline, file = paste0('RPK_dist_outline','.RDS'))
RPK_dist_outline <- readRDS("data/Isochrone/RPK_dist_outline.RDS")
################################################################################################################################################################

#Centroid of the County
#CenterPop_county <- read.csv("data/Isochrone/CenPop2010_Mean_CO51.txt", header =T)

#centerPop_RPK <- CenterPop_county %>% filter(COUNAME %in% c("Rappahannock")
#) #%>% filter(COUNTYFP == 157)
#saveRDS(centerPop_RPK, file = paste0('centerPop_RPK','.RDS'))
centerPop_RPK <- readRDS("data/Isochrone/centerPop_RPK.RDS")

#saveRDS(centerPop_RPK_dist, file = paste0('centerPop_RPK_dist','.RDS'))
centerPop_RPK_dist <- readRDS("data/Isochrone/centerPop_RPK_dist.RDS")



pop_centroid_RPK_iso_15 <- readRDS("data/Isochrone/pop_centroid_RPK_iso_15.RDS")
pop_centroid_RPK_iso_30 <- readRDS("data/Isochrone/pop_centroid_RPK_iso_30.RDS")


#Centroid of the Districts
#Obtained from:
#https://www.census.gov/geographies/reference-files/time-series/geo/centers-population.html

#CenterPop_dist <- read.csv("data/Isochrone/CenPop2010_Mean_CO51_157.txt", header =T)

#centerPop_RPK_dist <- CenterPop_dist %>%  filter(COUNTYFP == 157)
#%>% filter(COUNAME %in% c("Rappahannock")
#) 






#write.csv(centerPop_RPK_dist, file="centerPop_RPK_dist.csv")
CenterPop_dist1 <- read_excel("data/Isochrone/centerPop_RPK_dist.xlsx", sheet=1)
colnames(CenterPop_dist1)[8] <- "LONGITUDE"
colnames(CenterPop_dist1)[7] <- "LATITUDE"

CenterPop_dist2 <- read_excel("data/Isochrone/centerPop_RPK_dist.xlsx", sheet=2)
colnames(CenterPop_dist2)[8] <- "LONGITUDE"
colnames(CenterPop_dist2)[7] <- "LATITUDE"

CenterPop_dist3 <- read_excel("data/Isochrone/centerPop_RPK_dist.xlsx", sheet=3)
colnames(CenterPop_dist3)[8] <- "LONGITUDE"
colnames(CenterPop_dist3)[7] <- "LATITUDE"

CenterPop_dist4 <- read_excel("data/Isochrone/centerPop_RPK_dist.xlsx", sheet=4)
colnames(CenterPop_dist4)[8] <- "LONGITUDE"
colnames(CenterPop_dist4)[7] <- "LATITUDE"

CenterPop_dist5 <- read_excel("data/Isochrone/centerPop_RPK_dist.xlsx", sheet=5)
colnames(CenterPop_dist5)[8] <- "LONGITUDE"
colnames(CenterPop_dist5)[7] <- "LATITUDE"


################################################################################################################################################################
#travel time, I did this once to generate center RDS file
#to get the api and the id I visited the website and signed up:
#https://traveltime.com/docs/api/overview/getting-keys

traveltime_api <- "7bbbad489d637c38b2bea80e1e413721"
traveltime_id <- "c1fae682"

###################################################################################################################################
#INDIVIDUAL DISTRICTS
#1

#For 30 mins
#POP CENTROID
pop_centroid_RPK_dist_1_iso_30 <- readRDS("data/Isochrone/pop_centroid_RPK_dist_1_iso_30.RDS")
st_crs(pop_centroid_RPK_dist_1_iso_30) = 4326

#For 15 mins
#POP CENTROID
pop_centroid_RPK_dist_1_iso_15 <- readRDS("data/Isochrone/pop_centroid_RPK_dist_1_iso_15.RDS")
st_crs(pop_centroid_RPK_dist_1_iso_15) = 4326

#########################################################################################################################
#2

#For 30 mins
#POP CENTROID
pop_centroid_RPK_dist_2_iso_30 <- readRDS("data/Isochrone/pop_centroid_RPK_dist_2_iso_30.RDS")
st_crs(pop_centroid_RPK_dist_2_iso_30) = 4326

#For 15 mins
#POP CENTROID
pop_centroid_RPK_dist_2_iso_15 <- readRDS("data/Isochrone/pop_centroid_RPK_dist_2_iso_15.RDS")
st_crs(pop_centroid_RPK_dist_2_iso_15) = 4326

#########################################################################################################################
#3

#For 30 mins
#POP CENTROID
pop_centroid_RPK_dist_3_iso_30 <- readRDS("data/Isochrone/pop_centroid_RPK_dist_3_iso_30.RDS")
st_crs(pop_centroid_RPK_dist_3_iso_30) = 4326

#For 15 mins
#POP CENTROID
pop_centroid_RPK_dist_3_iso_15 <- readRDS("data/Isochrone/pop_centroid_RPK_dist_3_iso_15.RDS")
st_crs(pop_centroid_RPK_dist_3_iso_15) = 4326

#########################################################################################################################
#4

#For 30 mins
#POP CENTROID
pop_centroid_RPK_dist_4_iso_30 <- readRDS("data/Isochrone/pop_centroid_RPK_dist_4_iso_30.RDS")
st_crs(pop_centroid_RPK_dist_4_iso_30) = 4326

#For 15 mins
#POP CENTROID
pop_centroid_RPK_dist_4_iso_15 <- readRDS("data/Isochrone/pop_centroid_RPK_dist_4_iso_15.RDS")
st_crs(pop_centroid_RPK_dist_4_iso_15) = 4326
#########################################################################################################################
#########################################################################################################################

#5

#For 30 mins
#POP CENTROID
pop_centroid_RPK_dist_5_iso_30 <- readRDS("data/Isochrone/pop_centroid_RPK_dist_5_iso_30.RDS")
st_crs(pop_centroid_RPK_dist_5_iso_30) = 4326



#For 15 mins
#POP CENTROID
pop_centroid_RPK_dist_5_iso_15 <- readRDS("data/Isochrone/pop_centroid_RPK_dist_5_iso_15.RDS")
st_crs(pop_centroid_RPK_dist_5_iso_15) = 4326
################################################################################################################################################################


################################################################################################################################################################

# Read in all files file
mapVA  <- st_read("data/Isochrone/tract/tl_2019_51_tract.shp",
                  stringsAsFactors = FALSE)

map_and_data <- inner_join(mapVA, acs_RPK_area, by = "GEOID")

################################################################################################################################################################

#Services
Adultcare <- read_excel("data/Isochrone/assistedAdultDisabilityElderly.xlsx", sheet=1)
colnames(Adultcare)[7] <- "Longitude"
colnames(Adultcare)[8] <- "Latitude"

Food_Clothing <- read_excel("data/Isochrone/FoodClothing.xlsx")
colnames(Food_Clothing)[7] <- "Longitude"
colnames(Food_Clothing)[8] <- "Latitude"

entertain <- read_excel("data/Isochrone/Entertainment.xlsx")
colnames(entertain)[7] <- "Longitude"
colnames(entertain)[8] <- "Latitude"

Hospital <- read_excel("data/Isochrone/hospitalsHealthDentalFreeClinics.xlsx")
colnames(Hospital)[7] <- "Longitude"
colnames(Hospital)[8] <- "Latitude"

education <- read_excel("data/Isochrone/education.xlsx")
colnames(education)[7] <- "Longitude"
colnames(education)[8] <- "Latitude"

professional <- read_excel("data/Isochrone/professional.xlsx")
colnames(professional)[7] <- "Longitude"
colnames(professional)[8] <- "Latitude"

recreation <- read_excel("data/Isochrone/recreation.xlsx")
colnames(recreation)[7] <- "Longitude"
colnames(recreation)[8] <- "Latitude"

transit <- read_excel("data/Isochrone/transit.xlsx")
colnames(transit)[7] <- "Longitude"
colnames(transit)[8] <- "Latitude"

banks <- read_excel("data/Isochrone/banks.xlsx")
colnames(transit)[7] <- "Longitude"
colnames(transit)[8] <- "Latitude"

################################################################################################################################################################


#I am reordering the districts to correct them in the map, without this the map
#does not have the right figures.
acs_RPK_dist <- read.csv("data/Isochrone/Distrpk.csv", header =T)
################################################################################################################################################################



# Examples of previous data being read in  ----------------------------------------------------------

# socdem_block <- readRDS("data/socdem_block.Rds")
# socdem_block <- st_transform(socdem_block, '+proj=longlat +datum=WGS84')
# 
# socdem_tract <- readRDS("data/socdem_tract.Rds")
# socdem_tract <- st_transform(socdem_tract, '+proj=longlat +datum=WGS84')
# 
# connectivity <- readRDS("data/connectivity.Rds")
# connectivity <- st_transform(connectivity, '+proj=longlat +datum=WGS84')
# 
# ems <- readRDS("data/ems.Rds")
# ems <- st_transform(ems, '+proj=longlat +datum=WGS84')
# 
# groceries <- readRDS("data/groceries.Rds")
# groceries <- st_as_sf(groceries, coords = c("longitude", "latitude"))
# st_crs(groceries) <- "+proj=longlat +datum=WGS84"
# groceries <- st_transform(groceries, '+proj=longlat +datum=WGS84')
# groceries <- subset(groceries, type == "farmers market" | type == "supermarket")
# groceries_latlong <- readRDS("data/groceries.Rds")
# groceries_latlong <- subset(groceries_latlong, type == "farmers market" | type == "supermarket")

# CODE TO DETECT ORIGIN OF LINK AND CHANGE LOGO ACCORDINGLY
jscode <- "function getUrlVars() {
                var vars = {};
                var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m,key,value) {
                    vars[key] = value;
                });
                return vars;
            }

           function getUrlParam(parameter, defaultvalue){
                var urlparameter = defaultvalue;
                if(window.location.href.indexOf(parameter) > -1){
                    urlparameter = getUrlVars()[parameter];
                    }
                return urlparameter;
            }

            var mytype = getUrlParam('type','Empty');

            function changeLinks(parameter) {
                links = document.getElementsByTagName(\"a\");

                for(var i = 0; i < links.length; i++) {
                   var link = links[i];
                   var newurl = link.href + '?type=' + parameter;
                   link.setAttribute('href', newurl);
                 }
            }

           var x = document.getElementsByClassName('navbar-brand');

           if (mytype != 'economic') {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/events/symposium2020/poster-sessions\">' +
                              '<img src=\"DSPG_black-01.png\", alt=\"DSPG 2020 Symposium Proceedings\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('dspg');
           } else {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights/case-studies\">' +
                              '<img src=\"AEMLogoGatesColorsBlack-11.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('economic');
           }
           "

# user -------------------------------------------------------------
ui <- navbarPage(title = "I'm a title!",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                 useShinyjs(),

                 # main tab -----------------------------------------------------------
                 tabPanel("Project Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   # br("", style = "padding-top:2px;"),
                                   # img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                   br(""),
                                   h1(strong("Availability of Services"),
                                      h2(strong("Evolving Demographics, Housing, and Traffic in Rappahannock")),
                                      br(""),
                                      h4("Data Science for the Public Good Program"),
                                      h4("Virginia Polytechnic Institute and State University"),
                                      h4("[updat this]"),
                                      br()
                                   )
                          ),
                          fluidRow(style = "margin: 6px;",
                                   column(4,
                                          h2(strong("Project Introduction")),
                                          p(strong("Intro Word."), "Introduction to the project"),
                                          p(),
                                          p(strong("Another Word."), "lots more description of project"),
                                          p(),
                                          p(strong("final word."), "final project descriptions")
                                   ),
                                   column(4,
                                          h2(strong("Our Work")),
                                          p("Our research team worked closely with Patrick County Extension Office, Virginia Department of Health, and Healthy Patrick County coalition stakeholders
                                            to identify the county’s priority challenges in the area of health. The research team reviewed a prior", a(href = "https://www.vdh.virginia.gov/west-piedmont/2020/05/27/patrick-county-health-needs-improvement-plan-completed/",
                                            "community health assessment,", target = "blank"), a(href = "https://www.pubs.ext.vt.edu/VCE/VCE-596/VCE-596-75/VCE-1002-75.html", "situation analysis", target = "_blank"),
                                            "relevant funding applications, and held a listening meeting with stakeholders to identify these challenges. Lack of
                                            data on health care access, food access as related to diabetes and heart disease prevalence, older adult health, and digital connectivity that would facilitate
                                            access to telemedicine emerged as key problems where providing actionable insights could address barriers to Patrick County residents’ health."),
                                          p(),
                                          p("We implemented the", a(href = "https://doi.org/10.1162/99608f92.2d83f7f5", "data science framework", target = "_blank"), "and identified, acquired, profiled, and used
                                            publicly available data to provide Patrick County with data-driven resources in each of the four priority areas. We:"),
                                          tags$li("Provided census tract- and census block group-level maps of Patrick County residents'", strong("sociodemographic and socioeconomic characteristics,"), " highlighting underprivileged areas."),
                                          tags$li("Created census tract-level maps on", strong("older adult health"), "to show the geographic distribution of older adults in the county by gender and
                                                  type of disability, identifying areas where providing telehealth or travelling preventive care services may be particularly important."),
                                          tags$li("Mapped residents'", strong("computing device and internet access"), "at census block group level, and constructed 10- and 15-minute isochrones (areas of equal travel time) from households to free
                                                  wifi hotspots to highlight internet gaps that could suggest where new wi-fi hotspots could be optimally placed to provide internet access to more residents."),
                                          tags$li("Calculated and mapped", strong("emergency medical service (EMS) station coverage"), "of households within 8-, 10-, and 12-minute travel times, identifying areas difficult to reach within
                                                   standard EMS travel thresholds."),
                                          tags$li("Constructed", strong("food access"), "maps by census tract, 10- and 15-minute isochrones from households to grocery stores and farmers markets, and maps of food security resources in the county,
                                                highlighting food deserts and areas that could benefit from programs facilitating access to fresh produce."),
                                          p(),
                                          p("This dashboard compiles our findings and allows extension professionals, stakeholders, and other users to explore the information interactively.")
                                   ),
                                   column(4,
                                          h2(strong("Project Outcomes")),
                                          p("Our dashboard is aimed at:"),
                                          p(strong("Patrick County extension professionals and the communities they serve."), "Information available through the interface helps extension
                                            agents identify areas where residents may not have access to internet, or areas with a high smartphone ownership share, suggesting what channels agents may
                                            want to use to disseminate health-related information most effectively. Information on older adult populations and grocery store access can help extension agents
                                            better understand where underserved populations live and how to advocate on their behalf."),
                                          p(strong("Local health-related agencies and departments seeking data insights to inform their decision-making."), "For local stakeholders, identifying broadband
                                            access gaps that limit access to telemedicine, grocery store access gaps, and areas with high proportions of older adults with independent living difficulty can suggest
                                            optimal locations for placing free wifi hotspots, providing grocery delivery services, devising mobile health unit routes, or can inform other solutions that would benefit
                                            a broad population base."),
                                          p(strong("State government representatives in the Virginia Department of Health and the State Office of Rural Health."), "These and similar stakeholders may
                                            need small or rural area-specific insights that Centers for Disease Control and other county-level datasets cannot provide.")
                                   )
                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: August 2020'))))
                 ),

                 # county profile tab -----------------------------------------------------------
                 tabPanel("County Profile", value = "socio",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Rappahannock County Sociodemographic Profile"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(4,
                                          h4(strong("Who lives in Rappahannock County?")),
                                          p("Some words"),
                                          p("More words"),
                                          p("final words"))
                          ),
                          tabsetPanel(
                            tabPanel("Age Demographic",
                                     
                                     column(9, 
                                              selectInput("agedrop", "Select Variable:", width = "100%", choices = c(
                                              "Age Composition" = "ageGroups",
                                              "Age Composition Over Time" = "ageTime",
                                              "Age Composition Over Time by District" ="ageTime2",
                                              "Median Age" = "medAge",
                                              "Age Dependency" = "ageDep")),
                                            withSpinner(plotOutput("ageplot", height = "800px")),
                                            p(tags$small("Data Source: ACS Five Year Estimate Table B01001"))
                                            
                                      ),
                                     column(3,
                                            h4("Age Description.....")
                                            )
                                   

                          ),
  
                          tabPanel("Income",
                                   column(9,
                                          withSpinner(plotOutput("incomePlot", height = "1000px")),
                                          p(tags$small("Data Source: ACS Five Year Estimate Table ???"))
                                          ),
                                   column(3,
                                          h4("Income Description....")
                                     
                                   )
                                   
                                   
                                   ),
                          
                          tabPanel("Household Characteristics",
                                   column(8,
                                          selectInput("hcdrop", "Select Variable:", width = "100%", choices = c(
                                            "Household Size" = "houseSize",
                                            "Households Occupied by Renters and Owners" = "rentOwn",
                                            "Vehicles per Household" = "vehicles")
                                          ),
                                          withSpinner(plotOutput("hcplot", height ="800px")),
                                          p(tags$small("Data Source: ACS Five Year Estimate Tables S2504 and S2501"))
                                          
                                   ),
                                   column(4,
                                          h4("Houshold Characteristics Description......")
                                          
                                   )  
                            
                          ),
                          tabPanel("Broadband",
                                   
                                   column(8,
                                          selectInput("bbdrop", "Select Variable:", width = "100%", choices = c(
                                            "Internet Subscriptions by Income in Rappahannock" = "intIncome",
                                            "Internet Subscription and Computer Ownership by District" = "compDist")
                                          ),
                                          withSpinner(plotOutput("bbplot", height ="800px")),
                                          p(tags$small("Data Source: ACS Five Year Estimate Table S2801"))
                                          
                                   ),
                                   column(4,
                                          h4(" Broadband Description......")
                                          
                                   )
                                   
                          )
                
                ),
        
                
                ),

                 # traffic data tab-----------------------------------------------------------
                 tabPanel("Traffic", value = "connectivity",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Traffic in Rappahannock County"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(6,
                                          h4("Annual Average Daily Traffic in Rappahannock County"),
                                          p("leflet map description...")
                                          ),
                                   column(12,
                                          withSpinner(leafletOutput("traffic_markers_map", height ="700px")),
                                          p(tags$small("Data Source: Virginia Department of Transportation"))
                                   ),
                                   column(10,
                                          h4("Route Segments"),
                                          withSpinner(plotOutput("traffic_data_table"))
                                   )
                                
                               
                          )
                 ),
                 
                 # Services data tab-----------------------------------------------------------
                 tabPanel("Services", value = "",
                          fluidRow(style = "margin: 6px;",
                                   h2(strong("Services Available Rappahannock County"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(6,
                                          h4("Services in Rappahannock County"),
                                          p("Rappahannock Map with services and Driving boundaries.")
                                   ),
                                   column(12,h2(strong("About the Map")),
                                          p(" Write up ->To understand a general suite of services available in Rappahannock, we sourced publicly available demographic and infrastructure data to provide an overview of the services and amenities in Rappahannock. The data were primarily obtained from the 
 American Community Survey. The list of services was compiled from the Rappahannock news guide and resource guide."),
                                   column(12,
                                          withSpinner(leafletOutput("map_with_all_point", height ="700px")),
                                          p(tags$small("Data Source: American Community Survey"))
                                   ),
                                   column(12,h2(strong("Services")),
                                          p(" In many respects, Rappahannock County is uniquely endowed with built amenities and services. However, most of them are clustered or available outside the county. The map highlights the key resources that are available and accessed in the county.
 As the map shows, it most of the services are situated in and around Washington district where  inhabitants have easy access to  and other supporting infrastructure. "),
                                          
                                  column(12,h2(strong("Distance")),
                                          p("From the map we can see two boundaries. The inner driving boundary the map surrounding District centroid represents all parts of the region that are within a 15-minute drive from the center of the county (the green cloud). The outer driving boundary on the map surrounding District centroid represents all parts of the region 
 that are within a 30-minute drive from the center of the county (the red cloud). Additionally, the Rappahanock centroud has a driving map included. Talk about the Piedmont issue"),

column(12,h2(strong("Sources")),
 p("Services Check these and get a standardized reference
 https://www.rappahannock.com/business-directory
 resource_guide pdf
 rappk_news_guide pdf"),
p(tags$small("Data Source: American Community Survey"))
                                   ),
                                   
                                   
                          )
                 ),),),),                  
                 # data tab -----------------------------------------------------------
                 tabPanel("Data", value = "data",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Data Sources"), align = "center"),
                                   br()
                          ),
                          tabsetPanel(
                            tabPanel("Data Sources",
                                     h3("", align = "center"),
                                     br(""),
                                     column(4,
                                            
                                            img(src = "data-gmaps.png", style = "display: inline; float: left;", width = "130px"),
                                            p(strong("Google Maps."), "Google Maps is a comprehensive web mapping service created by Google. Its goal is to provide an interactive map
                                              of all the geographical contents of the world. This resource has a variety of uses, ranging from examining all service locations within
                                              a city to finding the quickest route between locations. It provides data at latitude and longitude level. We used Google Maps to locate
                                              all supermarkets, convenience stores, and farmers’ markets in Patrick County, and subsequently employed the information in calculating
                                              grocery access and coverage isochrones."),
                                            br(""),
                                     ),
                                     column(4,
                                            img(src = "data-acs.png", style = "display: inline; float: left;", width = "200px"),
                                            p(strong("American Community Survey."), "The American Community Survey (ACS) is an ongoing yearly survey conducted by the U.S Census
                                            Bureau. ACS samples households to compile 1-year and 5-year datasets providing information on population sociodemographic and
                                            socioeconomic characteristics including employment, disability, and health insurance coverage. We used AC 2014/18 5-year
                                            estimates to obtain census tract and census block group-level to explore Patrick County resident characteristics."),
                                            br(""),
                                            
                                            br(""),
                                         
                                     ),
                                     column(4,
                                            img(src = "data-traveltime.png", style = "display: inline; float: left;", width = "140px"),
                                            p(strong("TravelTime."), "TravelTime Application Programming Interface (API) aggregates data from OpenStreetMap, transport timetables and
                                           speed profiles to generate isochrones. An isochrone is a shape covering all locations that can be reached within the same timeframe
                                           given a start location, departure time, and a mode of transportation. We used the TravelTime API to produce isochrones of 10- and
                                           15-minute drive time interval from supermarkets, farmers' markets, and free wi-fi hotspots, and of 8-, 10-, and 12-minute drive
                                           time intervals from all emergency medical service stations in Patrick County."),
                                            br(""),
                                          
                                     )
                            )

                            )
                          ) ,

                 # team tab -----------------------------------------------------------
                 tabPanel("Meet the Team", value = "contact",
                          fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                            h1(strong("Contact"), align = "center"),
                            br(),
                            h4(strong("UVA Data Science for the Public Good")),
                            p("The", a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics/dspg-program', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"),
                              "is a summer immersive program held at the", a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics', 'University of Virginia Biocomplexity Institute’s Social and Decision Analytics division (SDAD).'),
                              "In its seventh year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around
                              critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences
                              to determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program
                              highlights, how to apply, and our annual symposium, please visit", a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics/dspg-program', 'the official Biocomplexity DSPG website.', target = "_blank")),
                            p("", style = "padding-top:10px;")
                            ),
                          fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                            column(6, align = "center",
                            h4(strong("DSPG Team Members")),
                            img(src = "team-tim.jpeg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                            img(src = "team-mousa.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                            img(src = "team-christina.jpeg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                            p(a(href = 'www.linkedin.com/in/timothyspierce', 'Timothy Pierce', target = '_blank'), "(Virginia Tech, Agricultural and Applied Economics);",
                              a(href = 'https://www.linkedin.com/in/reginald-mousa-toure-32b550106/', 'Mousa Toure', target = '_blank'), "(Virginia State University, Computer Science);",
                              a(href = 'https://www.linkedin.com/in/igomez-3099/', 'Christina Prisbe', target = '_blank'), "(Virginia Tech, Computational Modeling and Data Analytics)."),
                            p("", style = "padding-top:10px;")
                            ),
                            column(6, align = "center",
                            h4(strong("UVA SDAD Team Members")),
                            img(src = "faculty-gupta.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                            img(src = "faculty-mulu.jpeg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                            img(src = "team-leo.jpeg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                            p(a(href = "https://aaec.vt.edu/people/faculty/gupta-anubhab.html", 'Dr. Anubhab Gupta', target = '_blank'), "(Faculty Lead);",
                              a(href = "https://www.vsu.edu/cet/departments/technology/faculty-staff/kahsai-mulugeta.php", 'Dr. Mulugeta Kahsai', target = '_blank'), "(Faculty Affiliate);",
                              a(href = 'https://aaec.vt.edu/people/graduatestudents/index/quaye-leonard-allen.html', 'Leonard-Allen Quaye', target = '_blank'), "(Research Associate)."),
                            p("", style = "padding-top:10px;")
                            )
                            ),
                          fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                            h4(strong("Project Stakeholders")),
                            p(a(href = 'https://www.linkedin.com/in/nancy-bell-aa293810/', 'Nancy Bell', target = '_blank'), "(Virginia Department of Health);",
                              a(href = 'https://www.linkedin.com/in/terri-alt-3138b4101/', 'Terri Alt', target = '_blank'), "(Virginia Cooperative Extension, Patrick County at Virginia Tech)."),
                            p("", style = "padding-top:10px;"),
                            h4(strong("Acknowledgments")),
                            p("We would like to thank Healthy Patrick County, an association of concerned Patrick County residents, and Brandon Kramer for their input to this project.")
                          )
                 ),
                 inverse = T)



# server -----------------------------------------------------------
server <- function(input, output, session) {
  # Run JavaScript Code
  runjs(jscode)

  
  
           #age tabset -----------------------------------------------------
  ageVar <- reactive({
    input$agedrop
  })
  
  output$ageplot <- renderPlot({
    
    if (ageVar() == "ageGroups") {
      
      cbPalette <- c("#E69F00","#56B4E9","#009E73","#0072B2","#D55E00", "#CC79A7")
      
      age_group_rappk_pie_plot  <- ggplot(rappk_ageGroups, aes(x="", y=`Percent.of.Population`, fill=Key)) +
        geom_bar(stat="identity", width=1, color =1) +
        coord_polar("y", start=0) +
        geom_text(aes(label = paste0(Rounded, "%")), position = position_stack(vjust=0.5), size =9) +
        labs(x = NULL, y = NULL) +
        theme_classic() +
        theme(axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              legend.title = element_blank(),
              legend.text = element_text(size =15)) +
        scale_fill_manual(values=cbPalette) +
        ggtitle("Rappahannock") +
        theme(plot.title = element_text(hjust = 0.5, size = 25))
      
       age_group_va_pie_plot <- ggplot(va_ageGroups, aes(x="", y=`Percent.of.Population`, fill=Key)) +
         geom_bar(stat="identity", width=1, color=1) +
         coord_polar("y", start=0) +
        geom_text(aes(label = paste0(Rounded, "%")), position = position_stack(vjust=0.5), size=9) +
         labs(x = NULL, y = NULL) +
         theme_classic() +
         theme(axis.line = element_blank(),
               axis.text = element_blank(),
               axis.ticks = element_blank()) +
         scale_fill_manual(values=cbPalette) + 
         ggtitle("Virginia") +
         theme(plot.title = element_text(hjust = 0.5, size =25), legend.title = element_blank(),
               legend.text = element_text(size =15))
      
       ageplot <- grid.arrange(age_group_rappk_pie_plot,age_group_va_pie_plot, ncol =1)
       ageplot
      
    }
    else if (ageVar() == "medAge") {
      counties_median_age_plot <- ggplot(counties_median_age, aes(x=County, y=`Median.Age`, fill=Key)) + 
        geom_bar(stat="identity") +
        geom_text(aes(label=paste0(round(`Median.Age`))), vjust=1.5, colour="white", size=10) +
        theme(axis.text = element_text(size = 15)) +
        geom_hline(aes(yintercept= 38.2, linetype = "   Virginia Median Age: 38"), 
                   color= "black", size = 1.5, alpha = 0.25) +
        ggtitle("Virginia Counties") +
        ylab("Median Age") +
        theme(plot.title = element_text(hjust = 0.5, size =20), legend.title = element_blank(), axis.title.x=element_blank(),
              legend.text = element_text(size =15),
              axis.title.y = element_text(size=15)) +
        scale_fill_viridis_d()
      
      district_median_age <- ggplot(data = sub_median_age, aes(x = District, y = `Median.Age`, fill = Key)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=paste0(round(`Median.Age`))), vjust=1.5, colour="white", size=10) +
        geom_hline(aes(yintercept= 50.1, linetype = "Rappahannock Median Age: 50"), color= "black", size = 1.5, alpha = 0.25) +
        ggtitle("Rappahannock Districts") +
        ylab("Median Age")+
        theme(plot.title = element_text(hjust = 0.5, size =20),legend.title = element_blank(),
              axis.title.x=element_blank(),
              axis.text = element_text(size=15),
              legend.text = element_text(size =15),
              axis.title.y =element_text(size=15)) +
        scale_fill_viridis_d()
      
      ageplot <- grid.arrange(counties_median_age_plot, district_median_age)
      ageplot
    }
    else if (ageVar() == "ageDep") {
      counties_dep_plot <- ggplot(county_dep, aes(x=County, y=`Dependency.Ratio`, fill=Key)) +
        geom_bar(stat='identity', position='dodge') +
        ggtitle("Age Dependecy Ratios in Virginia Counties") +
        theme(plot.title = element_text(hjust = 0.5, size=20)) +
        theme(axis.text = element_text(size = 15),legend.title = element_blank(), axis.title.x = element_blank(),
              legend.text = element_text(size=15),axis.title.y = element_text(size=15) ) +
        scale_fill_viridis_d()
      
      district_dep_plot <- ggplot(sub_dep, aes(x=District, y=`Dependency.Ratio`, fill=Key)) +
        geom_bar(stat='identity', position='dodge')  +
        ggtitle("Age Dependecy Ratios in Rappahannock Districts") +
        theme(plot.title = element_text(hjust = 0.5, size=20)) +
        theme(axis.text = element_text(size = 15), legend.title = element_blank(),
              axis.title.x = element_blank(), legend.text = element_text(size=15), axis.title.y = element_text(size=15)) +
        scale_fill_viridis_d()
      
      va_dep_plot <- ggplot(va_rappk_dep, aes(x=`Location`, y=`Dependency.Ratio`, fill=Key)) +
        geom_bar(stat='identity', position='dodge')  +
        ggtitle("Age Dependecy Ratios") +
        theme(plot.title = element_text(hjust = 0.5, size =20)) +
        theme(axis.text = element_text(size = 15), legend.title = element_blank(),  axis.title.x=element_blank(),
              legend.text = element_text(size =15),axis.title.y = element_text(size=15)) +
        scale_fill_viridis_d()
      
      ageplot <- grid.arrange(va_dep_plot, counties_dep_plot,district_dep_plot)
      ageplot
    }
    else if (ageVar() == "ageTime") {
      ageplot <- ggplot(rappage_timeseries, aes(x = year, y = percent, group = ages, color = ages)) +
        geom_line(aes(size = estimate)) +
        labs(title = "Age of Population from 2010 to 2019", color = "Age Categories") +
        ylab("Percent of the population") +
        scale_color_viridis_d(
          labels = c("under18" = "Under 18", 
                     "age18_29" = "18 to 29", 
                     "age30_64" = "30 to 64", 
                     "age65_older" = "65 and Older")) +
        theme(plot.title = element_text(hjust=0.5, size =20),
              legend.text = element_text(size=15),
              legend.title =element_text(size=15),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size=15),
              axis.text = element_text(size=15))
      ageplot
      
    }
    else if (ageVar() == "ageTime2") {
      ageplot <- ggplot(agetimeseries, aes(x = year, y = percent, color = ages, group = ages)) +
        geom_line(aes(size = estimate)) +
        labs(title = "Age of Population from 2010 to 2019", color = "Age Categories") +
        xlab("Years") +
        ylab("Percent of the population") +
        scale_color_viridis_d(
          labels = c("under18" = "Under 18", 
                     "age18_29" = "18 to 29", 
                     "age30_64" = "30 to 64", 
                     "age65_older" = "65 and Older")) +
        facet_wrap(~NAME) +
        theme(plot.title = element_text(hjust=0.5, size=20),
              axis.text = element_text(size=15),
              legend.title = element_text(size=15),
              legend.text = element_text(size=15),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size=15)
         )
       ageplot
    }
    
  })
  
  
           #income plot ----------------------------------------------------------
  output$incomePlot <- renderPlot({
   incomePlot <- ggplot(income2010_2019, aes(x = incomebracket, y = percent, fill = NAME.x, group = NAME.x)) +
      geom_col(position = "dodge") +
      facet_wrap(~year) +
      coord_flip() +
      scale_fill_viridis_d(name="District") +
      ylab("Median Income")+
      ggtitle("Median Income from 2010 to 2019") +
      theme(plot.title = element_text(hjust=0.5, size=20),
            legend.text = element_text(size=15),
            axis.text = element_text(size=15),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=15),
            legend.title=element_text(size=15))
   incomePlot
  })
  
           #housheold characteristics -----------------------------------------------
  hcVar <- reactive({
    input$hcdrop
  })
  
  output$hcplot <- renderPlot({
    
    if(hcVar() == "houseSize") {
       hcplot <- ggplot(householdSize, aes(x = "", y = estimate, fill = fct_inorder(People))) +
         geom_col(width = 1, color = 1) +
       geom_text(aes(label = paste0(estimate, "%")),
                   position = position_stack(vjust = 0.5), colour="white", size =8) +
         coord_polar(theta = "y") +
        guides(fill = guide_legend(title = "Number of People")) +
         theme(plot.title = element_text(hjust = 0.5, size =20),
               axis.ticks = element_blank(),
               axis.title = element_blank(),
               axis.text = element_blank(), 
               legend.text = element_text(size=15),
               legend.title = element_text(size=15),
               panel.background = element_rect(fill = "white")) +
         ggtitle("Rappahannock Household Size") +
         scale_fill_viridis_d()
       hcplot <- grid.arrange(hcplot)
    }
    
    
    else if(hcVar() == "rentOwn") {
      own_graph <- own %>%
        ggplot(aes(x = year, y = `Household.Units`)) + 
        geom_line(position = "identity", show.legend = TRUE, size=1.5) +
        ggtitle("Owner-Occupied") + theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size=20),
              axis.title.y = element_text("Housing Units", size=15),
              axis.title.x = element_blank(),
              axis.text = element_text(size=15)) +
        scale_x_continuous(breaks=seq(2010,2019,by=1))
      #Graph (rent)
      rent_graph <- rent %>%
        ggplot(aes(x = year, y = `Household.Units`)) + 
        geom_line(position = "identity", show.legend = TRUE, size=1.5) +
        theme_minimal() +
        ggtitle("Renter-Occupied") +
        theme(plot.title = element_text(hjust = 0.5, size=20),
              axis.title.y = element_text("Housing Units", size =15),
              axis.title.x = element_blank(),
              axis.text = element_text(size=15)) +
        scale_x_continuous(breaks=seq(2010,2019,by=1))
      #putting the graphs together
      hcplot <- grid.arrange(own_graph, rent_graph, ncol=1)
    }
    if(hcVar() == "vehicles"){
      rappk_veh_plot <- ggplot(rappk_veh, aes(x = "", y = estimate, fill = fct_inorder(type))) +
        geom_col(width = 1, color = 1) +
        geom_text(aes(label = paste0(estimate, "%")),
                  position = position_stack(vjust = 0.5),colour="white", size =8) +
        coord_polar(theta = "y") +
        guides(fill = guide_legend(title = "Number of Vehicles")) +
        theme(plot.title = element_text(hjust = 0.5, size =20),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              axis.text = element_blank(), 
              legend.text = element_text(size=15),
              legend.title = element_text(size=15),
              panel.background = element_rect(fill = "white")) +
        ggtitle("Vehicles Available per Household") +
        scale_fill_viridis_d()
      
      county_veh3 <- county_veh %>%
        group_by(county) %>%
        arrange(county, desc(num)) %>%
        mutate(lab_ypos = cumsum(estimate) - 0.20 * estimate) 
      #Graph
      county_veh3$num <- factor(county_veh3$num, levels = c("None", "One", "Two", "Three or more"))
      vehicle_graph <- ggplot(data = county_veh3, aes(x = county, y = estimate)) +
        geom_col(aes(fill = num), width = 0.7)+
        geom_text(aes(y = lab_ypos, label = paste0(estimate, "%"), group =county), color = "white",size=6, hjust =1.1)+
        coord_flip() +
        ylab("County")+
        ggtitle("Number of Vehicles per Household in Surrounding Counties")+
        theme(plot.title = element_text(hjust = 0.5, size=20),
              legend.title = element_blank(),
              axis.text=element_text(size=12),
              legend.text = element_text(size=15),
              axis.title.x=element_blank(),
              axis.title.y=element_blank()) +
        scale_fill_viridis_d()
      
      hcplot <- grid.arrange(rappk_veh_plot, vehicle_graph, ncol=1)
      hcplot
      
    }
 
  })
  
  
  
           #broadband tab -------------------------------------------------------------
  bbVar <- reactive({
    input$bbdrop
  })
  
  output$bbplot <- renderPlot({
    
    if (bbVar() == "intIncome") {
      rappk_data4 <- intByIncome%>%
        group_by(`Income.Range`) %>%
        arrange(`Income.Range`, desc(Int)) %>%
        mutate(lab_ypos = cumsum(Percentage) - 0.25 * Percentage) 
      
     bbplot <- ggplot(data = rappk_data4, aes(x = `Income.Range`, y = Percentage)) +
        geom_col(aes(fill = Int), width = 0.7)+
        geom_text(aes(y = lab_ypos, label = paste0(round(Percentage),"%"), group =Int), color = "white", size =8)+
        coord_flip() +ggtitle("Internet Subscription based on Income in Rappahannock") +
        theme(plot.title = element_text(hjust = 0.5, size=20),
              legend.title = element_blank(),
              axis.text=element_text(size=15),
              legend.text = element_text(size=15),
              axis.title.x=element_blank(),
              axis.title.y=element_text(size =15)) +
        xlab("Income Range") +
        scale_fill_manual(values=c("#009E73","#D55E00"))
      #plot
      bbplot
    }
    else if (bbVar() == "compDist") {
      sub_comp3 <- compDist %>%
        group_by(District) %>%
        arrange(District, desc(key)) %>%
        mutate(lab_ypos = cumsum(Percentage) - 0.25 * Percentage) 
      
      comp_plot <- ggplot(data = sub_comp3, aes(x = District, y = Percentage)) +
        geom_col(aes(fill = key), width = 0.7)+
        geom_text(aes(y = lab_ypos, label = paste0(round(Percentage),"%"), group =key), color = "white", size=8)+
        coord_flip() +
        ggtitle("Computer Ownership") +
        theme(plot.title = element_text(hjust=0.5, size=20),
              legend.title = element_blank(),
              axis.text=element_text(size=15),
              legend.text = element_text(size=15),
              axis.title.x=element_blank(),
              axis.title.y=element_text(size =15)) +
        scale_fill_manual(values=c("#009E73","#D55E00"))
      
      sub_int3 <- intDist %>%
        group_by(District) %>%
        arrange(District, desc(key)) %>%
        mutate(lab_ypos = cumsum(Percentage) - 0.25 * Percentage) 
      
      sub_plot <- ggplot(data = sub_int3, aes(x = District, y = Percentage)) +
        geom_col(aes(fill = key), width = 0.7)+
        geom_text(aes(y = lab_ypos, label = paste0(round(Percentage), "%"), group =key), color = "white", size=8)+
        coord_flip() +
        ggtitle("Internet Subscription") +
        theme(plot.title = element_text(hjust=0.5, size=20),
              legend.title = element_blank(),
              axis.text=element_text(size=15),
              legend.text = element_text(size=15),
              axis.title.x=element_blank(),
              axis.title.y=element_text(size =15)) +
        scale_fill_manual(values=c("#009E73","#D55E00"))
      
      bbplot <- grid.arrange(sub_plot, comp_plot, ncol=1)
      bbplot
    }
    
  })
  
  
  
  
  
  
  
  
  
  
  #traffic  leaflet -------------------------------------------------------------
  output$traffic_markers_map <- renderLeaflet({
    #changing 2010-2015 data to have only the same roads that 2016-2020 has (getting rid of one route from 2014,2015)
    traff_2010 <- filter(traffic_2010, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    traff_2011 <- filter(traffic_2011, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    traff_2012 <- filter(traffic_2012, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    traff_2013 <- filter(traffic_2013, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    traff_2014 <- filter(traffic_2014, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    traff_2015 <- filter(traffic_2015, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    #combining them based on id (id name for id column in Virginia Department pf Transportation)
    traffic1 <- cbind(traff_2010[6:267,c(1,14)],traff_2011[6:267,c(1,14)],traff_2012[6:267,c(1,14)],traff_2013[6:267,c(1,14)],traff_2014[6:267,c(1,14)],traff_2015[6:267,c(1,14)])
    #traffic 2016-2019 has an extra header row than 2014-2015, so we are going to merge 2016-2019 first and then get rid of it(to merge w/ 2014-2015)
    traffic2 <- cbind(traffic_2016[7:268,c(1,14)], traffic_2017[7:268,c(1,14)], traffic_2018[7:268,c(1,14)], traffic_2019[7:268,c(1,14)], traffic_2020[7:268,c(1,14)])
    #combining all the years estimates
    traffic_year <- cbind(traffic1[,c(2,4,6,8,10,12)], traffic2[,c(2,4,6,8,10)])
    colnames(traffic_year) <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
    #assigning the routes IDs
    traffic_year <- mutate(traffic_year, ID =1:n())
    #setting ID as the far left column
    traffic_year <- traffic_year[,c(12,1:11)]
    #getting route info (using 2020 to get the info but all the years should have the same info)
    traffic_info <- traffic_2020[7:268,c(1:3,4:13)]
    traffic_year2 <- cbind(traffic_info, traffic_year)
    #2010-2020
    traffic_data <- as.data.frame(sapply(traffic_year, as.numeric))
    traffic_data <- mutate(traffic_data, pctchange = (traffic_data[,12]-traffic_data[,2])/traffic_data[,2] *100)
    traffic_data2 <- mutate(traffic_data, abs=abs(traffic_data[,13]))
    #setting ID to character for graphing
    traffic_data2$ID<- as.character(traffic_data2$ID)
    #put in descending order then select top
    traffic_data3 <- traffic_data2 %>%
      arrange(desc(abs))
    #get's rid of the lnf(the ones that start with 0 for the initial year)
    traffic_data4 <- traffic_data3[7:262,] %>%
      slice(1:20)
    #traffic_data4 from above gives us the 20 roads with the highest pct change 
    traffic_data5 <- mutate(traffic_data4, count = `2020`-`2010`)
    traffic_data5 <- mutate(traffic_data5, count2 = abs(count))
    #We are getting rid of the roads that have changed less than 30 counts
    traffic_data6 <- traffic_data5 %>% filter(count2 >30)
    #merging the data with the coordinates(by ID)
    rappk_data <- merge(coordinates, traffic_data6, by="ID")
    #rearranging rappk_map data to be able to graph line plots for each marker
    rappk_data2 <- reshape(rappk_data, direction = "long",
                           varying = list(names(rappk_data)[7:17]),
                           v.names ="estimate",
                           idvar="ID",
                           timevar= "Year",
                           times = 2010:2020)
    #makes year a chracters to fix future graph formatting issues
    rappk_data2$Year <- as.character(rappk_data2$Year)
        #gets polygon data for the 5 subdivision in rappahannock county
    subdivisions <- county_subdivisions(state= "VA", county =157, cb = TRUE)
    #adds a column labeleing whether that road has a negative or posive change(used for layering)
    rappk_data2 <- mutate(rappk_data2, sign = if_else(sign(count)>0, "Positive", "Negative"))
    #making the size of the markers based on the percent change (absolute value) and the color based on count(absolute value)
    numPal <- colorNumeric(palette = "viridis", domain = rappk_data2$count, reverse = TRUE)
    pos_data <- rappk_data2 %>% filter(sign == "Positive")
    neg_data <- rappk_data2 %>% filter(sign== "Negative")
    #positive data charts
    pick_p <- function(id){
      dataFiltered <- filter(pos_data, ID== id)
      
      p <- ggplot(dataFiltered, aes(x=Year, y=estimate)) + 
        geom_line(position = "identity", aes(group=1)) +
        theme_minimal()+
        ggtitle("AADT from 2010 to 2020") +
        theme(plot.title = element_text(hjust=0.5),
              axis.title.x = element_blank()) +
        labs(y ="Annual Average Daily Traffic") 
      p
      return(p)
    }
    q <- lapply(1:length(unique(pos_data$ID)), function(i) {
      pick_p(pos_data$ID[i])
    })
    #negative data charts
    pick_n <- function(id){
      dataFiltered <- filter(neg_data, ID== id)
      
      p <- ggplot(dataFiltered, aes(x=Year, y=estimate)) + 
        geom_line(position = "identity", aes(group=1)) +
        theme_minimal()+
        ggtitle("AADT from 2010 to 2020") +
        theme(plot.title = element_text(hjust=0.5),
              axis.title.x = element_blank()) +
        labs(y ="Annual Average Daily Traffic") 
      p
      return(p)
    }
    r <- lapply(1:length(unique(neg_data$ID)), function(i) {
      pick_n(neg_data$ID[i])
    })
    #marker pop up labels
    label_p <- lapply(paste("ID: ", pos_data$ID, "<br />", "Count Change: ", pos_data$count,
                            "<br />", "Percent Change: ", round(pos_data$pctchange, digits =2),"%"), HTML)
    label_n <- lapply(paste("ID: ", neg_data$ID, "<br />", "Count Change: ", neg_data$count,
                            "<br />", "Percent Change: ", round(neg_data$pctchange, digits =2),"%"), HTML)
    map <- leaflet() %>% 
      addProviderTiles("Esri") %>%
      fitBounds(lng1=-78.345941, lat1=38.875265,
                lng2=-77.934027, lat2=38.518670) %>%
      addPolygons(data=subdivisions, popup = subdivisions$NAME, color = "black",
                  fillColor = "#ffffff00")
    #map
    traffic_markers_map <- map %>%
      addCircleMarkers(data= pos_data,
                       lng=~lng,
                       lat=~lat,
                       color = numPal(pos_data$count),
                       opacity = 0.3,
                       radius = sqrt(pos_data$abs),
                       label =label_p,
                       clusterId = pos_data$ID,
                       group= "Positive",
                       popup = popupGraph(q)) %>%
      addCircleMarkers(data= neg_data,
                       lng=~lng,
                       lat=~lat,
                       color=numPal(neg_data$count),
                       opacity = 0.1,
                       label =label_n,
                       radius = sqrt(neg_data$abs),
                       clusterId = neg_data$ID,
                       group = "Negative",
                       popup = popupGraph(r)) %>%
      addLayersControl(overlayGroups = c("Positive", "Negative"), options = layersControlOptions(collapsed = FALSE)) %>%
      addLegendSize(values =rappk_data2$abs, pal= numPal, opacity = 0.3, shape = "circle", title = "Percent Change",
                    position = "bottomright", strokeWidth = 10, breaks = 5) %>%
      addLegendNumeric(pal = numPal,bins=12, title = "Count change", values = rappk_data2$count, position= "bottomright",
                       height = 200)
    #plot
    traffic_markers_map

  })
  
      #trafficd route table ------------------------------------------------------------
  output$traffic_data_table <- renderPlot({
    #changing 2010-2015 data to have only the same roads that 2016-2020 has (getting rid of one route from 2014,2015)
    traff_2010 <- filter(traffic_2010, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    traff_2011 <- filter(traffic_2011, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    traff_2012 <- filter(traffic_2012, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    traff_2013 <- filter(traffic_2013, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    traff_2014 <- filter(traffic_2014, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    traff_2015 <- filter(traffic_2015, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    #combining them based on id (id name for id column in Virginia Department pf Transportation)
    traffic1 <- cbind(traff_2010[6:267,c(1,14)],traff_2011[6:267,c(1,14)],traff_2012[6:267,c(1,14)],traff_2013[6:267,c(1,14)],traff_2014[6:267,c(1,14)],traff_2015[6:267,c(1,14)])
    #traffic 2016-2019 has an extra header row than 2014-2015, so we are going to merge 2016-2019 first and then get rid of it(to merge w/ 2014-2015)
    traffic2 <- cbind(traffic_2016[7:268,c(1,14)], traffic_2017[7:268,c(1,14)], traffic_2018[7:268,c(1,14)], traffic_2019[7:268,c(1,14)], traffic_2020[7:268,c(1,14)])
    #combining all the years estimates
    traffic_year <- cbind(traffic1[,c(2,4,6,8,10,12)], traffic2[,c(2,4,6,8,10)])
    colnames(traffic_year) <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
    #assigning the routes IDs
    traffic_year <- mutate(traffic_year, ID =1:n())
    #setting ID as the far left column
    traffic_year <- traffic_year[,c(12,1:11)]
    #getting route info (using 2020 to get the info but all the years should have the same info)
    traffic_info <- traffic_2020[7:268,c(1:3,4:13)]
    traffic_year2 <- cbind(traffic_info, traffic_year)
    traffic_data <- as.data.frame(sapply(traffic_year, as.numeric))
    traffic_data <- mutate(traffic_data, pctchange = (traffic_data[,12]-traffic_data[,2])/traffic_data[,2] *100)
    traffic_data2 <- mutate(traffic_data, abs=abs(traffic_data[,13]))
    #setting ID to character for graphing
    traffic_data2$ID<- as.character(traffic_data2$ID)
    #put in descending order then select top
    traffic_data3 <- traffic_data2 %>%
      arrange(desc(abs))
    #get's rid of the lnf(the ones that start with 0 for the initial year)
    traffic_data4 <- traffic_data3[7:262,] %>%
      slice(1:20)
    traffic_data5 <- mutate(traffic_data4, count = `2020`-`2010`)
    traffic_data5 <- mutate(traffic_data5, count2 = abs(count))
    #We are getting rid of the roads that have changed less than 30 counts
    traffic_data6 <- traffic_data5 %>% filter(count2 >30)
    traffic_year3 <-traffic_year2[,c(14,6,12,13)]
    leaflet_tbl <- filter(traffic_year3, ID %in% traffic_data6$ID) 
    leaflet_tbl<- merge(leaflet_tbl, traffic_data6[,c(1,13,15)], by ="ID")
    colnames(leaflet_tbl) <- c("ID", "Road Alias", "Start", "End", "pct", "Count Change")
    leaflet_tbl <- mutate(leaflet_tbl, `Percent Change`= paste0(round(pct, digits = 2)))
    leaflet_tbl <- leaflet_tbl[,c(1:4,7,6)]
    leaflet_tbl_final <- tableGrob(leaflet_tbl, rows = NULL)
    traffic_data_table<-as.ggplot(leaflet_tbl_final)
    #plot
    traffic_data_table
    
  })
  
  
  
  #Service  leaflet -------------------------------------------------------------
  output$map_with_all_point <- renderLeaflet({
    
    #making the map
    mypalette <- colorNumeric(palette="viridis", centerPop_RPK_dist$POPULATION)
    
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
      
      addCircleMarkers(centerPop_RPK,lat = centerPop_RPK$LATITUDE, lng= centerPop_RPK$LONGITUDE,
                       radius =  4,
                       color = "white",
                       stroke = TRUE, fillOpacity = 1
      ) %>%
      addLegend(colors = "white", labels = " Rappahannock Population Centroid") %>%
      
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
      setView(lat = centerPop_RPK$LATITUDE, lng= centerPop_RPK$LONGITUDE, zoom =11)
    
    
    #Show the map
    map_with_all_point
  
  }) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)

