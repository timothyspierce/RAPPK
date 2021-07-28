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
library(forcats)

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
                                     
                                     column(12, 
                                              selectInput("agedrop", "Select Variable:", width = "100%", choices = c(
                                              "Age Composition" = "ageGroups",
                                              "Age Composition Over Time" = "ageTime",
                                              "Age Composition Over Time by District" ="ageTime2",
                                              "Median Age" = "medAge",
                                              "Age Dependency" = "ageDep")),
                                            withSpinner(plotOutput("ageplot", height = "800px")),
                                            p(tags$small("Data Source: ACS Five Year Estimate Table B01001"))
                                            
                                      ),
                                     column(12,
                                            h4("description.....")
                                            )
                                   

                          ),
  
                          tabPanel("Income",
                                   column(12,
                                          withSpinner(plotOutput("incomePlot", height = "1000px")),
                                          p(tags$small("Data Source: ACS Five Year Estimate Table ???"))
                                          )
                                   
                                   
                                   ),
                          
                          tabPanel("Household Characteristics",
                                   column(12,
                                          selectInput("hcdrop", "Select Variable:", width = "100%", choices = c(
                                            "Household Size" = "houseSize",
                                            "Households Occupied by Renters and Owners" = "rentOwn",
                                            "Vehicles per Household" = "vehicles")
                                          ),
                                          withSpinner(plotOutput("hcplot", height ="800px")),
                                          p(tags$small("Data Source: ACS Five Year Estimate Tables S2504 and S2501"))
                                          
                                   ),
                                   column(12,
                                          h4("Description......")
                                          
                                   )  
                            
                          ),
                          tabPanel("Broadband",
                                   
                                   column(12,
                                          selectInput("bbdrop", "Select Variable:", width = "100%", choices = c(
                                            "Internet Subscriptions by Income in Rappahannock" = "intIncome",
                                            "Internet Subscription and Computer Ownership by District" = "compDist")
                                          ),
                                          withSpinner(plotOutput("bbplot", height ="800px")),
                                          p(tags$small("Data Source: ACS Five Year Estimate Table S2801"))
                                          
                                   ),
                                   column(12,
                                          h4("Description......")
                                          
                                   )
                                   
                          )
                
                ),
        
                
                ),

                 # housing market tab -----------------------------------------------------------
                 tabPanel("Housing Market", value = "older",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Housing Data Rappahannock County"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(4,
                                          h4(strong("some descriptor")),
                                          p("The US population is aging, and in Patrick County, over 30% of residents are older adults aged 65 years and over. This represents more than 5,000
                                           individuals with varying health conditions that may benefit from locally accessible health care and social services resources. However, access to
                                           health care resources is limited in rural areas, particularly for older adults in need of assistance with activities of daily life."),
                                          p("To help Patrick County better understand their older adult population, we used American Community Survey (ACS) data and obtained census tract
                                           level information for the age group. ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households to compile
                                           1-year and 5-year estimates of population sociodemographic and socioeconomic characteristics. We used the most recently available 5-year data
                                           from 2014/18 to calculate the percentage of the Patrick County older adults with different types of disability, as well as provided information
                                           on their living arrangements and socioeconomic status. We provided all information at census tract level and by gender."),
                                          p("These insights on the health and socioeconomic status of older adults in Patrick County can assist the county in identifying areas of high need
                                          for health care resources.")
                                   ),
                                   column(8,
                                          h4(strong("Map of Older Adult Characteristics by Census Tract")),
                                          tabsetPanel(
                                            tabPanel("Older Adult Characteristics",
                                                     p(""),
                                                     column(6,
                                                            selectInput("olddrop", "1. Select Variable:", width = "100%", choices = c(
                                                              "Percent with Vision Difficulty" = "visdiff",
                                                              "Percent with Ambulatory Difficulty" = "ambdiff",
                                                              "Percent with Self-Care Difficulty" = "carediff",
                                                              "Percent with Cognitive Difficulty" = "cogdiff",
                                                              "Percent with Independent Living Difficulty" = "ildiff",
                                                              "Percent with Any Disability" = "disab",
                                                              "Percent in Poverty" = "inpov",
                                                              "Percent in Labor Force" = "labfor")
                                                            )),
                                                     column(6,
                                                            selectInput("oldspecdrop", "2. Select Group:", width = "100%", choices = c(
                                                              "Total",
                                                              "Female" = "_f",
                                                              "Male" = "_m")
                                                            )),
                                                     withSpinner(leafletOutput("oldplot")),
                                                     p(tags$small("Data Source: American Community Survey 2014/18 5-Year Estimates."))
                                            ),
                                            tabPanel("Older Adult Household Characteristics",
                                                     p(""),
                                                     selectInput("hhdrop", "Select Variable:", width = "100%", choices = c(
                                                       "Percent Married Couple Households with one or more 60+ Member" = "hhsixty_married",
                                                       "Percent Households with one or more 60+ Members" = "hhsixty_total",
                                                       "Percent Single (no partner present) Households with one or more 60+ Member" = "hhsixty_nonfam",
                                                       "Percent Households with one or more Male 60+ Members" = "hhsixty_mhh",
                                                       "Households with one or more Female 60+ Members" = "hhsixty_fhh")),
                                                     withSpinner(leafletOutput("householdplot")),
                                                     p(tags$small("Data Source: American Community Survey 2014/18 5-Year Estimates."))
                                            )
                                          )
                                   )
                          )
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
                 
                 # traffic data tab-----------------------------------------------------------
                 tabPanel("Services", value = "connectivity",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Services Available Rappahannock County"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(6,
                                          h4(strong("Computing Device Ownership and Internet Access Type")),
                                          p("Internet connection and computing devices are key for access to health information and participation in online health-related services like
                                             telemedicine. Rural areas frequently lack broadband access, experience low internet speeds, and have fewer internet providers available
                                             than urban areas. It is crucial to consider digital connectivity in improving health care access. We examined digital connectivity in Patrick County in two ways to
                                             provide the county with insights on where increasing connectivity would facilitate communicating health information and improve online health service access."),
                                          p("We first examined access to computing devices and internet connection types in Patrick County. We used American Community Survey (ACS) data to
                                            obtain this information at census block group level. ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households
                                            to compile 1-year and 5-year estimates of population sociodemographic and socioeconomic characteristics. We used the most
                                            recently available 5-year data from 2014/18 to calculate the percentage of the Patrick County residents with access to devices
                                            and internet by census block group."),
                                          br(),
                                          selectInput("devicedrop", "Select Variable:", width = "100%", choices = c(
                                            "Percent Households with No Computer" = "nocomputer",
                                            "Percent Households with Laptop" = "laptop",
                                            "Percent Households with Smartphone" = "smartphone",
                                            "Percent Households with Tablet" = "tablet",
                                            "Percent Households without Internet" = "nointernet",
                                            "Percent Households with Satellite Internet" = "satellite",
                                            "Percent Households with Cellular Internet" = "cellular",
                                            "Percent Households with Broadband Internet" = "broadband")
                                          ),
                                          p(strong("Map of Access by Census Block Group")),
                                          withSpinner(leafletOutput("deviceplot")),
                                          p(tags$small("Data Source: American Community Survey 2014/18 5-Year Estimates."))),
                                   column(6,
                                          h4(strong("Free WiFi Hotspot Access")),
                                          p("To understand internet access at a more granular level, we examined access to free wi-fi hotspots in the county."),
                                          p("We obtained wifi hotspot locations using the Virginia Tech and CommonwealthConnect hotspot map. CommonwealthConnect identifies where people can connect to
                                            the internet for free, decreasing constraints placed on families that do not have internet access at home. We retrieved free internet locations in Patrick
                                            County from the data. We extracted locations of Patrick County residential properties from 2019 CoreLogic, a proprietary dataset for US real estate that
                                            includes information on building characteristics. Finally, we used the TravelTime Application Programming Interface (API) to calculate 10- and 15-minute
                                            car travel time isochrones—areas of equal travel time given a departure time and mode of transportation—from wifi hotspots. TravelTime API aggregates data
                                            from Open Street Maps, transport timetables and speed profiles to generate isochrones. Isochrones allowed us to identify wifi gaps, or clusters of
                                            residential properties that cannot reach a free internet location within a selected travel time range."),
                                          p("This information equips extension agents with knowledge on how best to reach their constituents, as well as identifies internet gaps that suggest where
                                            new wi-fi hotspots could be optimally placed to provide internet access to more residents."),
                                          br(),
                                          tabsetPanel(
                                            tabPanel("Explore Hotspot Coverage",
                                                     p(""),
                                                     selectInput("wifidrop", "Select Free Wifi Location:", width = "100%", choices = c(
                                                       "Meadows of Dan Elementary School",
                                                       "Woolwine Elementary School",
                                                       "Patrick Springs Primary School",
                                                       "Blue Ridge Elementary School",
                                                       "Patrick County High School",
                                                       "Stuart Elementary School",
                                                       "Patrick County Branch Library",
                                                       "Hardin Reynolds Memorial School",
                                                       "Stuart Baptist Church",
                                                       "Patrick Henry Community College Stuart Campus")),
                                                     p(strong("Percent Residential Properties Covered")),
                                                     withSpinner(tableOutput("wifitable")),
                                                     p(strong("Map of Coverage")),
                                                     withSpinner(leafletOutput("wifiplot")),
                                                     p(tags$small("Data Sources: CommonwealthConnect, 2020; CoreLogic, 2019; TravelTime API."))
                                            ),
                                            tabPanel("Explore 'Deserts'",
                                                     p(""),
                                                     p(strong("Percent Residential Properties Covered")),
                                                     withSpinner(tableOutput("allwifitable")),
                                                     p(strong("Map of Free Wi-Fi Deserts")),
                                                     withSpinner(leafletOutput("allwifi")),
                                                     p(tags$small("Data Sources: CommonwealthConnect, 2020; CoreLogic, 2019; TravelTime API."))
                                            )
                                          )
                                   )
                          )
                 ),
                 
                 # data tab -----------------------------------------------------------
                 tabPanel("Data and Measures", value = "data",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Data and Measures"), align = "center"),
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
                            ),
                            tabPanel("Measures",
                                     h3(strong(""), align = "center"),
                                     selectInput("topic", "Select Topic:", width = "100%", choices = c(
                                       "All Measures",
                                       "Sociodemographic Measures",
                                       "Older Adult Population Measures",
                                       "Connectivity Measures",
                                       "Food Access Measures",
                                       "Health Care Access Measures")
                                     ),
                                     withSpinner(DTOutput("datatable"))
                            )
                          )
                 ),

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
       hcplot1 <- ggplot(householdSize, aes(x = "", y = estimate, fill = fct_inorder(People))) +
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
       
       hcplot <- grid.arrange(hcplot1)
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
        geom_text(aes(y = lab_ypos, label = paste0(estimate, "%"), group =county), color = "white",size=8, hjust =1.1)+
        coord_flip() +
        ylab("County")+
        ggtitle("Number of Vehicles per Household")+
        theme(plot.title = element_text(hjust = 0.5, size=20),
              legend.title = element_blank(),
              axis.text=element_text(size=12),
              legend.text = element_text(size=15),
              axis.title.x=element_blank(),
              axis.title.y=element_text(size =15)) +
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # socio plots: done -----------------------------------------------------

  var <- reactive({
    input$sociodrop
  })
  #age65
  output$socioplot <- renderLeaflet({
    if(var() == "age65") {

      pal <- colorQuantile("Blues", domain = socdem_block$age65, probs = seq(0, 1, length = 6), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population age 65 or over:</strong>",
              round(socdem_block$age65, 2)),
        htmltools::HTML
      )

      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$age65),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$age65),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
      #under18
    }else if(var() == "under18"){
      pal <- colorQuantile("Blues", domain = socdem_block$under18, probs = seq(0, 1, length = 6), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population age 18 or under: </strong>",
              round(socdem_block$under18, 2)),
        htmltools::HTML
      )

      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$under18),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$under18),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
      #population-tract
    }else if(var() == "totalpop_trct"){
      pal <- colorQuantile("Blues", domain = socdem_tract$totalpop_trct, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>Total population: </strong>",
              formatC(socdem_tract$totalpop_trct, format = "f", big.mark =",", digits = 0)),
        htmltools::HTML
      )

      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$totalpop_trct),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$totalpop_trct),
                  title = "Total Population<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 0), " &ndash; ", round(cuts[-1], 0), ")")
                  })
      #population-block group
    }else if(var() == "totalpop_bgrp"){
      pal <- colorQuantile("Blues", domain = socdem_block$totalpop_bgrp, probs = seq(0, 1, length = 6), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>Total population: </strong>",
              formatC(socdem_block$totalpop_bgrp, format = "f", big.mark =",", digits = 0)),
        htmltools::HTML
      )

      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$totalpop_bgrp),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$totalpop_bgrp),
                  title = "Total Population<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 0), " &ndash; ", round(cuts[-1], 0), ")")
                  })
    }else if(var() == "black"){
      pal <- colorQuantile("Blues", domain = socdem_block$black, probs = seq(0, 1, length = 6), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population Black: </strong>",
              round(socdem_block$black, 2)),
        htmltools::HTML
      )

      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$black),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$black),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "noba"){
      pal <- colorQuantile("Blues", domain = socdem_block$noba, probs = seq(0, 1, length = 6), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population without BA degree: </strong>",
              round(socdem_block$noba, 2)),
        htmltools::HTML
      )

      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$noba),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$noba),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "unempl"){
      pal <- colorQuantile("Blues", domain = socdem_block$unempl, probs = seq(0, 1, length = 6), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population in labor force unemployed: </strong>",
              round(socdem_block$unempl, 2)),
        htmltools::HTML
      )

      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$unempl),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$unempl),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "nohealthins2"){
      pal <- colorQuantile("Blues", domain = socdem_block$nohealthins2, probs = seq(0, 1, length = 6), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population without health insurance: </strong>",
              round(socdem_block$nohealthins2, 2)),
        htmltools::HTML
      )

      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$nohealthins2),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$nohealthins2),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "snap"){
      pal <- colorQuantile("Blues", domain = socdem_block$snap, probs = seq(0, 1, length = 6), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population receiving public assistance or SNAP benefits: </strong>",
              round(socdem_block$snap, 2)),
        htmltools::HTML
      )

      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$snap),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$snap),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "inpov"){
      pal <- colorQuantile("Blues", domain = socdem_tract$inpov, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% Population in poverty: </strong>",
              round(socdem_tract$inpov, 2)),
        htmltools::HTML
      )

      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$inpov),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$inpov),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "hispanic"){
      pal <- colorQuantile("Blues", domain = socdem_tract$hispanic, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% Population Hispanic or Latino: </strong>",
              round(socdem_tract$hispanic, 2)),
        htmltools::HTML
      )

      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$hispanic),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$hispanic),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "privateins"){
      pal <- colorQuantile("Blues", domain = socdem_tract$privateins, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% Population with private health insurance: </strong>",
              round(socdem_tract$privateins, 2)),
        htmltools::HTML
      )

      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$privateins),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$privateins),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else{
      pal <- colorQuantile("Blues", domain = socdem_tract$publicins, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% Population with public health insurance: </strong>",
              round(socdem_tract$publicins, 2)),
        htmltools::HTML
      )

      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$publicins),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$publicins),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })


  # old plots - snap -----------------------------------------------
  var_old <- reactive({
    input$olddrop
  })
  var_hh <- reactive({
    input$hhdrop
  })
  output$oldplot <- renderLeaflet({
    # healthins wasn't coded properly so it's just all zeroes
    if(var_old() == "visdiff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$visdiff,
                     "_f" = olderadults$visdiff_f,
                     "_m" = olderadults$visdiff_m)

      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with vision difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "ambdiff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$ambdiff,
                     "_f" = olderadults$ambdiff_f,
                     "_m" = olderadults$ambdiff_m)

      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with ambulatory difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "cogdiff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$cogdiff,
                     "_f" = olderadults$cogdiff_f,
                     "_m" = olderadults$cogdiff_m)

      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with cognitive difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "carediff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$carediff,
                     "_f" = olderadults$carediff_f,
                     "_m" = olderadults$carediff_m)

      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with self-care difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "ildiff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$ildiff,
                     "_f" = olderadults$ildiff_f,
                     "_m" = olderadults$ildiff_m)

      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with independent living difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "disab") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$disab,
                     "_f" = olderadults$disab_f,
                     "_m" = olderadults$disab_m)

      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with any disability: </strong>",
              round(data, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "inpov") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$inpov,
                     "_f" = olderadults$inpov_f,
                     "_m" = olderadults$inpov_m)

      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults in poverty: </strong>",
              round(data, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else
      # if(var_old() == "labfor")
    {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$labfor,
                     "_f" = olderadults$labfor_f,
                     "_m" = olderadults$labfor_m)

      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults in the labor force: </strong>",
              round(data, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  output$householdplot <- renderLeaflet({
    if(var_hh() == "hhsixty_total") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_total,
                     "_f" = olderadults$hhsixty_total,
                     "_m" = olderadults$hhsixty_total)

      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_total, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Housholds with a 60+ member: </strong>",
              round(olderadults$hhsixty_total, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_total),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_total),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_hh() == "hhsixty_fhh") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_fhh,
                     "_f" = olderadults$hhsixty_fhh,
                     "_m" = olderadults$hhsixty_fhh)

      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_fhh, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Housholds with a female 60+ member:</strong>",
              round(olderadults$hhsixty_fhh, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_fhh),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_fhh),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_hh() == "hhsixty_mhh") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_mhh,
                     "_f" = olderadults$hhsixty_mhh,
                     "_m" = olderadults$hhsixty_mhh)

      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_mhh, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Housholds with a male 60+ member: </strong>",
              round(olderadults$hhsixty_mhh, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_mhh),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_mhh),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_hh() == "hhsixty_nonfam") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_nonfam,
                     "_f" = olderadults$hhsixty_nonfam,
                     "_m" = olderadults$hhsixty_nonfam)

      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_nonfam, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Single housholds with a 60+ member: </strong>",
              round(olderadults$hhsixty_nonfam, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_nonfam),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_nonfam),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else{
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_marr,
                     "_f" = olderadults$hhsixty_marr,
                     "_m" = olderadults$hhsixty_marr)

      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_marr, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Married households with a 60+ member: </strong>",
              round(olderadults$hhsixty_marr, 2)),
        htmltools::HTML
      )

      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_marr),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_marr),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })


  # data and measures table: done ----------------------------------------
  var_topic <- reactive({
    input$topic
  })
  output$datatable <- renderDataTable({
    if(var_topic() == "All Measures"){
      table <- as.data.frame(measures_table)
     datatable(table, rownames = FALSE, options = list(pageLength = 15)) %>% formatStyle(0, target = 'row', lineHeight = '80%')
    }
    else{
      data <- switch(input$topic,
                     "Connectivity Measures" = "connectivity",
                     "Sociodemographic Measures" = "demographics",
                     "Food Access Measures" = "food access",
                     "Health Care Access Measures" = "health",
                     "Older Adult Population Measures" = "older adults")
      table <- subset(measures_table, Topic == data)
      table <- as.data.frame(table)
      datatable(table, rownames = FALSE, options = list(pageLength = 15)) %>% formatStyle(0, target = 'row', lineHeight = '80%')
    }
  })

  # device: done ---------------------------------------------------------

  output$deviceplot <- renderLeaflet({
      data <- switch(input$devicedrop,
                     "nocomputer" = connectivity$nocomputer,
                     "laptop" = connectivity$laptop,
                     "smartphone" = connectivity$smartphone,
                     "tablet" = connectivity$tablet,
                     "nointernet" = connectivity$nointernet,
                     "satellite" = connectivity$satellite,
                     "cellular" = connectivity$cellular,
                     "broadband" = connectivity$broadband)

      device_spec <- switch(input$devicedrop,
                            "nocomputer" = "no computer",
                            "laptop" = "laptop",
                            "smartphone" = "smartphone",
                            "tablet" = "tablet",
                            "nointernet" = "no internet access",
                            "satellite" = "satellite internet",
                            "cellular" = "cellular internet",
                            "broadband" = "broadband internet")

      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 6), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              connectivity$NAME.y,
              "<br />",
              "<strong>% Households with",
              device_spec,
              "access: </strong>",
              round(data, 2)),
        htmltools::HTML
      )

      leaflet(data = connectivity, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
  })


  # wifi: done -----------------------------------------------------------

  # Iso selector
  output$wifiplot <- renderLeaflet({
      colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

      wifi_iso10 <- switch(input$wifidrop,
                     "Meadows of Dan Elementary School" = wifi_iso_10_1,
                     "Woolwine Elementary School" = wifi_iso_10_2,
                     "Patrick Springs Primary School" = wifi_iso_10_3,
                     "Blue Ridge Elementary School" = wifi_iso_10_4,
                     "Patrick County High School" = wifi_iso_10_5,
                     "Stuart Elementary School" = wifi_iso_10_6,
                     "Patrick County Branch Library" = wifi_iso_10_7,
                     "Hardin Reynolds Memorial School" = wifi_iso_10_8,
                     "Stuart Baptist Church" = wifi_iso_10_9,
                     "Patrick Henry Community College Stuart Campus" = wifi_iso_10_10)

      wifi_iso15 <- switch(input$wifidrop,
                           "Meadows of Dan Elementary School" = wifi_iso_15_1,
                           "Woolwine Elementary School" = wifi_iso_15_2,
                           "Patrick Springs Primary School" = wifi_iso_15_3,
                           "Blue Ridge Elementary School" = wifi_iso_15_4,
                           "Patrick County High School" = wifi_iso_15_5,
                           "Stuart Elementary School" = wifi_iso_15_6,
                           "Patrick County Branch Library" = wifi_iso_15_7,
                           "Hardin Reynolds Memorial School" = wifi_iso_15_8,
                           "Stuart Baptist Church" = wifi_iso_15_9,
                           "Patrick Henry Community College Stuart Campus" = wifi_iso_15_10)

       data <- switch(input$wifidrop,
                           "Meadows of Dan Elementary School" = 1,
                           "Woolwine Elementary School" = 2,
                           "Patrick Springs Primary School" = 3,
                           "Blue Ridge Elementary School" = 4,
                           "Patrick County High School" = 5,
                           "Stuart Elementary School" = 6,
                           "Patrick County Branch Library" = 7,
                           "Hardin Reynolds Memorial School" = 8,
                           "Stuart Baptist Church" = 9,
                           "Patrick Henry Community College Stuart Campus" = 10)

       labels <- lapply(
         paste("<strong>Name: </strong>",
               wifi_latlong[data, ]$name,
               "<br />",
               "<strong>Address:</strong>",
               wifi_latlong[data, ]$fulladdress,
               "<br />",
               "<strong>Notes:</strong>",
               wifi_latlong[data, ]$notes),
         htmltools::HTML
       )

      m1 <- leaflet(options = leafletOptions(minZoom = 10)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircles(data = residential,
                   fillColor = colors[5],
                   fillOpacity = .8,
                   stroke = FALSE,
                   group = "Residential Properties") %>%
        addPolygons(data = wifi_iso10,
                    fillColor = colors[1],
                    fillOpacity = .8,
                    stroke = FALSE,
                    group = "10 Minute Isochrone") %>%
        addPolygons(data = wifi_iso15,
                    fillColor = colors[2],
                    fillOpacity = .8,
                    stroke = FALSE,
                    group = "15 Minute Isochrone") %>%
        addMarkers(data = wifi_latlong, ~longitude[data], ~latitude[data],
                   label = labels,
                   labelOptions = labelOptions(direction = "bottom",
                                               style = list(
                                                 "font-size" = "12px",
                                                 "border-color" = "rgba(0,0,0,0.5)",
                                                 direction = "auto")))  %>%
        addLayersControl(
          position = "topright",
          overlayGroups = c("10 Minute Isochrone",
                            "15 Minute Isochrone",
                            "Residential Properties"),
          options = layersControlOptions(collapsed = FALSE))
      m1
  })

  # Coverage table
  output$wifitable <- renderTable({
    data <- switch(input$wifidrop,
                         "Meadows of Dan Elementary School" = 1,
                         "Woolwine Elementary School" = 2,
                         "Patrick Springs Primary School" = 3,
                         "Blue Ridge Elementary School" = 4,
                         "Patrick County High School" = 5,
                         "Stuart Elementary School" = 6,
                         "Patrick County Branch Library" = 7,
                         "Hardin Reynolds Memorial School" = 8,
                         "Stuart Baptist Church" = 9,
                         "Patrick Henry Community College Stuart Campus" = 10)

    table <- read.csv(paste0("data/isochrones/tables/wifi_iso_table_",data,".csv"))
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)

  # Wifi deserts
  output$allwifi <- renderLeaflet({

    labels <- lapply(
      paste("<strong>Name: </strong>",
            wifi_latlong$name,
            "<br />",
            "<strong>Address:</strong>",
            wifi_latlong$fulladdress,
            "<br />",
            "<strong>Notes:</strong>",
            wifi_latlong$notes),
      htmltools::HTML
    )

    leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential,
                 fillColor = colors[5],
                 fillOpacity = .5,
                 stroke = FALSE,
                 group = "Residential Properties") %>%
      addPolygons(data = wifi_iso_10_1,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_2,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_3,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_4,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_5,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_6,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_7,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_8,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_9,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_1,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_2,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_3,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_4,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_5,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_6,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_7,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_8,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_9,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_10,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addMarkers(data = wifi_latlong,
                 group = "Free Wi-Fi Locations",
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto")))  %>%
      addLayersControl(
        position = "topright",
        overlayGroups = c("Free Wi-Fi Locations",
                          "Residential Properties"),
        baseGroups = c("10 Minute Isochrones",
                       "15 Minute Isochrones"),
        options = layersControlOptions(collapsed = FALSE))
  })

  output$allwifitable <- renderTable({
    table <- read.csv("data/isochrones/tables/wifi_iso_table.csv")
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)

  # ems: done ------------------------------------------------------------

  output$emsplot <- renderLeaflet({
      colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

      ems_iso8 <- switch(input$emsdrop,
                     "STUART VOLUNTEER FIRE DEPARTMENT" = ems_iso_8_1,
                     "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = ems_iso_8_2,
                     "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = ems_iso_8_3,
                     "VESTA RESCUE SQUAD" = ems_iso_8_4,
                     "ARARAT RESCUE SQUAD" = ems_iso_8_5,
                     "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = ems_iso_8_6,
                     "JEB STUART RESCUE SQUAD" = ems_iso_8_7,
                     "SMITH RIVER RESCUE SQUAD" = ems_iso_8_8,
                     "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = ems_iso_8_9)

      ems_iso10 <- switch(input$emsdrop,
                         "STUART VOLUNTEER FIRE DEPARTMENT" = ems_iso_10_1,
                         "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = ems_iso_10_2,
                         "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = ems_iso_10_3,
                         "VESTA RESCUE SQUAD" = ems_iso_10_4,
                         "ARARAT RESCUE SQUAD" = ems_iso_10_5,
                         "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = ems_iso_10_6,
                         "JEB STUART RESCUE SQUAD" = ems_iso_10_7,
                         "SMITH RIVER RESCUE SQUAD" = ems_iso_10_8,
                         "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = ems_iso_10_9)

      ems_iso12 <- switch(input$emsdrop,
                          "STUART VOLUNTEER FIRE DEPARTMENT" = ems_iso_12_1,
                          "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = ems_iso_12_2,
                          "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = ems_iso_12_3,
                          "VESTA RESCUE SQUAD" = ems_iso_12_4,
                          "ARARAT RESCUE SQUAD" = ems_iso_12_5,
                          "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = ems_iso_12_6,
                          "JEB STUART RESCUE SQUAD" = ems_iso_12_7,
                          "SMITH RIVER RESCUE SQUAD" = ems_iso_12_8,
                          "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = ems_iso_12_9)

      data <- switch(input$emsdrop,
                         "STUART VOLUNTEER FIRE DEPARTMENT" = 1,
                         "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = 2,
                         "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = 3,
                         "VESTA RESCUE SQUAD" = 4,
                         "ARARAT RESCUE SQUAD" = 5,
                         "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = 6,
                         "JEB STUART RESCUE SQUAD" = 7,
                         "SMITH RIVER RESCUE SQUAD" = 8,
                         "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = 9)

      labels <- lapply(
        paste("<strong>Name: </strong>",
              str_to_title(ems[data, ]$NAME),
              "<br />",
              "<strong>Address:</strong>",
              str_to_title(ems[data, ]$ADDRESS), ",", str_to_title(ems[data, ]$CITY), ", VA", ems[data, ]$ZIP,
              "<br />",
              "<strong>Type:</strong>",
              str_to_title(ems[data, ]$NAICSDESCR)),
        htmltools::HTML
      )

      m1 <- leaflet(options = leafletOptions(minZoom = 10)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircles(data = residential,
                   fillColor = colors[5],
                   fillOpacity = .8,
                   stroke = FALSE,
                   group = "Residential Properties") %>%
        addPolygons(data = ems_iso8,
                    fillColor = colors[1],
                    fillOpacity = .8,
                    stroke = FALSE,
                    group = "8 Minute Isochrone") %>%
        addPolygons(data = ems_iso10,
                    fillColor = colors[2],
                    fillOpacity = .8,
                    stroke = FALSE,
                    group = "10 Minute Isochrone") %>%
        addPolygons(data = ems_iso12,
                    fillColor = colors[2],
                    fillOpacity = .8,
                    stroke = FALSE,
                    group = "12 Minute Isochrone") %>%
        addMarkers(data = ems, ~LONGITUDE[data], ~LATITUDE[data],
                   group = "EMS Locations",
                   label = labels,
                   labelOptions = labelOptions(direction = "bottom",
                                               style = list(
                                                 "font-size" = "12px",
                                                 "border-color" = "rgba(0,0,0,0.5)",
                                                 direction = "auto"))) %>%
        addLayersControl(
          position = "topright",
          overlayGroups = c("8 Minute Isochrone",
                         "10 Minute Isochrone",
                         "12 Minute Isochrone",
                         "Residential Properties"),
          options = layersControlOptions(collapsed = FALSE))
      m1
  })

  output$emstable <- renderTable({
    data <- switch(input$emsdrop,
                       "STUART VOLUNTEER FIRE DEPARTMENT" = 1,
                       "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = 2,
                       "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = 3,
                       "VESTA RESCUE SQUAD" = 4,
                       "ARARAT RESCUE SQUAD" = 5,
                       "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = 6,
                       "JEB STUART RESCUE SQUAD" = 7,
                       "SMITH RIVER RESCUE SQUAD" = 8,
                       "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = 9)


    table <- read.csv(paste0("data/isochrones/tables/ems_iso_table_",data,".csv"))
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)

  # EMS deserts
  output$allems <- renderLeaflet({

    labels <- lapply(
      paste("<strong>Name: </strong>",
            str_to_title(ems$NAME),
            "<br />",
            "<strong>Address:</strong>",
            paste0(str_to_title(ems$ADDRESS), ", ", str_to_title(ems$CITY), ", VA ", ems$ZIP),
            "<br />",
            "<strong>Type:</strong>",
            str_to_title(ems$NAICSDESCR)),
      htmltools::HTML
    )

    leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential,
                 fillColor = colors[5],
                 fillOpacity = .5,
                 stroke = FALSE,
                 group = "Residential Properties") %>%
      addPolygons(data = ems_iso_8_1,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_2,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_3,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_4,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_5,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_6,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_7,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_8,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_9,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_1,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_2,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_3,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_4,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_5,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_6,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_7,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_8,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_9,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_1,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_2,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_3,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_4,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_5,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_6,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_7,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_8,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_9,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "12 Minute Isochrones") %>%
      addMarkers(data = ems,
                 group = "EMS Locations",
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto"))) %>%
      addLayersControl(
        position = "topright",
        baseGroups = c("8 Minute Isochrones",
                       "10 Minute Isochrones",
                       "12 Minute Isochrones"),
        overlayGroups = c("EMS Locations",
                       "Residential Properties"),
        options = layersControlOptions(collapsed = FALSE))
  })

  output$allemstable <- renderTable({
    table <- read.csv("data/isochrones/tables/ems_iso_table.csv")
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)


  # usda - lahunv10share  -----------------------------------------------------------
  var_usda <- reactive({
    input$usdadrop
  })
  output$usdaplot <- renderLeaflet({
      data <- switch(input$usdadrop,
                     "lakids1share" = usda$lakids1share,
                     "lakids10share" = usda$lakids10share,
                     "lalowi1share" = usda$lalowi1share,
                     "lalowi10share" = usda$lalowi10share,
                     "lapop1share" = usda$lapop1share,
                     "lapop10share" = usda$lapop10share,
                     "laseniors1share" = usda$laseniors1share,
                     "laseniors10share" = usda$laseniors10share)

      usda_spec <- switch(input$usdadrop,
                          "lakids1share" = "low food access for children at 1 mile",
                          "lakids10share" = "low food access for children at 10 miles",
                          "lalowi1share" = "low food access for low income population at 1 mile",
                          "lalowi10share" = "low food access for low income population at 10 miles",
                          "lapop1share" = "low food access at 1 mile",
                          "lapop10share" = "low food access at 10 miles",
                          "laseniors1share" = "low food access for seniors at 1 mile",
                          "laseniors10share" = "low food access for seniors at 10 miles")

      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)

      labels <- lapply(
        paste("<strong>Area: </strong>",
              usda$NAME.y,
              "<br />",
              "<strong>% Population with",
              usda_spec,
              round(data, 2)),
        htmltools::HTML
      )

      leaflet(data = usda, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data),
                    fillOpacity = 0.7,
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
  })

  # grocery --------------------------------------------------------

  # Iso selector
  output$grocplot <- renderLeaflet({
      colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

      groc_iso10 <- switch(input$grocdrop,
                     "Mountain Meadow Farm and Craft Market" = grc_iso_10_1,
                     "Lowes Foods of Stuart" = grc_iso_10_2,
                     "Patrick County Local Farmers Market" = grc_iso_10_3,
                     "Stuart Farmers Market" = grc_iso_10_4,
                     "W & W Produce" = grc_iso_10_5,
                     "Walmart Supercenter" = grc_iso_10_6,
                     "Poor Farmers Farm" = grc_iso_10_7)

      groc_iso15 <- switch(input$grocdrop,
                           "Mountain Meadow Farm and Craft Market" = grc_iso_15_1,
                           "Lowes Foods of Stuart" = grc_iso_15_2,
                           "Patrick County Local Farmers Market" = grc_iso_15_3,
                           "Stuart Farmers Market" = grc_iso_15_4,
                           "W & W Produce" = grc_iso_15_5,
                           "Walmart Supercenter" = grc_iso_15_6,
                           "Poor Farmers Farm" = grc_iso_15_7)

      data <- switch(input$grocdrop,
                           "Mountain Meadow Farm and Craft Market" = 1,
                           "Lowes Foods of Stuart" = 2,
                           "Patrick County Local Farmers Market" = 3,
                           "Stuart Farmers Market" = 4,
                           "W & W Produce" = 5,
                           "Walmart Supercenter" = 6,
                           "Poor Farmers Farm" = 7)

      labels <- lapply(
        paste("<strong>Name: </strong>",
              groceries_latlong[data, ]$name,
              "<br />",
              "<strong>Address:</strong>",
              groceries_latlong[data, ]$fulladdress,
              "<br />",
              "<strong>Type:</strong>",
              groceries_latlong[data, ]$type),
        htmltools::HTML
      )

      m1 <- leaflet(options = leafletOptions(minZoom = 10)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircles(data = residential,
                   fillColor = colors[5],
                   fillOpacity = .8,
                   stroke = FALSE,
                   group = "Residential Properties") %>%
        addPolygons(data = groc_iso10,
                    fillColor = colors[1],
                    fillOpacity = .8,
                    stroke = FALSE,
                    group = "10 Minute Isochrone") %>%
        addPolygons(data = groc_iso15,
                    fillColor = colors[2],
                    fillOpacity = .8,
                    stroke = FALSE,
                    group = "15 Minute Isochrone") %>%
        addMarkers(data = groceries_latlong, ~longitude[data], ~latitude[data],
                   group = "Fresh Food Location",
                   label = labels,
                   labelOptions = labelOptions(direction = "bottom",
                                               style = list(
                                                 "font-size" = "12px",
                                                 "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"))) %>%
        addLayersControl(
          position = "topright",
          overlayGroups = c("15 Minute Isochrone",
                            "10 Minute Isochrone",
                            "Residential Properties",
                            "Fresh Food Location"),
          options = layersControlOptions(collapsed = FALSE))
      m1
  })

  # Grocery table
  output$groctable <- renderTable({
    data <- switch(input$grocdrop,
                         "Mountain Meadow Farm and Craft Market" = 1,
                         "Lowes Foods of Stuart" = 2,
                         "Patrick County Local Farmers Market" = 3,
                         "Stuart Farmers Market" = 4,
                         "W & W Produce" = 5,
                         "Walmart Supercenter" = 6,
                         "Poor Farmers Farm" = 7)

    table <- read.csv(paste0("data/isochrones/tables/grc_iso_table_",data,".csv"))
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)

  # Food deserts
  output$allgroc <- renderLeaflet({

    labels <- lapply(
      paste("<strong>Name: </strong>",
            groceries_latlong$name,
            "<br />",
            "<strong>Address:</strong>",
            groceries_latlong$fulladdress,
            "<br />",
            "<strong>Type:</strong>",
            groceries_latlong$type),
      htmltools::HTML
    )

    leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential,
                 fillColor = colors[5],
                 fillOpacity = .5,
                 stroke = FALSE,
                 group = "Residential Properties") %>%
      addPolygons(data = grc_iso_10_1,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_2,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_3,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_4,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_5,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_6,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_7,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_1,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_2,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_3,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_4,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_5,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_6,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_7,
                  fillColor = colors[1],
                  fillOpacity = .5,
                  stroke = FALSE,
                  group = "15 Minute Isochrones") %>%
      addMarkers(data = groceries_latlong,
                 group = "Fresh Food Locations",
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto")))  %>%
      addLayersControl(
        position = "topright",
        baseGroups = c("10 Minute Isochrones",
                       "15 Minute Isochrones"),
        overlayGroups = c("Residential Properties",
                          "Fresh Food Locations"),
        options = layersControlOptions(collapsed = FALSE))
  })

   # Other food resources
  output$othermap <- renderLeaflet({

    pal <- colorFactor(c("#0E879C", "#D9E12B", "#E6A01D"), domain = otherfood$type)

    labels <- lapply(
      paste("<strong>Name: </strong>",
            otherfood$name,
            "<br />",
            "<strong>Address:</strong>",
            otherfood$fulladdress,
            "<br />",
            "<strong>Type:</strong>",
            otherfood$type,
            "<br />",
            "<strong>Open to:</strong>",
            otherfood$audience,
            "<br />",
            "<strong>Notes:</strong>",
            otherfood$notes),
      htmltools::HTML
    )

    leaflet(data = otherfood,
            options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = patrickborder, stroke = T, weight = 2, color = "grey", fillOpacity = 0) %>%
      addCircleMarkers(data = otherfood,
                       stroke = FALSE,
                       fillOpacity = 1,
                       color = ~pal(type),
                       radius = 7,
                       opacity = 1,
                       label = labels,
                       labelOptions = labelOptions(direction = "bottom",
                                                   style = list(
                                                     "font-size" = "12px",
                                                     "border-color" = "rgba(0,0,0,0.5)",
                                                     direction = "auto"))) %>%
      addLegend("bottomleft",
                pal = pal,
                values =  ~type,
                title = "Type",
                opacity = 0.9)
  })

  output$allgrctable <- renderTable({
    table <- read.csv("data/isochrones/tables/grc_iso_table.csv")
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)

}

shinyApp(ui = ui, server = server)

