library(dplyr)
library(tidyverse)
library(readxl)
library(readr)
#organzing data
traffic_2010 <- read_excel("Data/traffic/traffic_data_2010.xls")
traffic_2011 <- read_excel("Data/traffic/traffic_data_2011.xls")
traffic_2012 <- read_excel("Data/traffic/traffic_data_2012.xls")
traffic_2013 <- read_excel("Data/traffic/traffic_data_2013.xls")
traffic_2014 <- read_excel("Data/traffic/traffic_data_2014.xls")
traffic_2015 <- read_excel("Data/traffic/traffic_data_2015.xls")
traffic_2016 <- read_excel("Data/traffic/traffic_data_2016.xls")
traffic_2017 <- read_excel("Data/traffic/traffic_data_2017.xls")
traffic_2018 <- read_excel("Data/traffic/traffic_data_2018.xls")
traffic_2019 <- read_excel("Data/traffic/traffic_data_2019.xls")
traffic_2020 <- read_excel("Data/traffic/traffic_data_2020.xlsx")
#raw counts 2010-2020
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
#table
traffic_year3 <-traffic_year2[,c(14,6,12,13)]
leaflet_tbl <- filter(traffic_year3, ID %in% rappk_data$ID) 
#adding count anf pctchnage to the table
leaflet_tbl<- merge(leaflet_tbl, rappk_data[,c(1,18,20)], by ="ID")
colnames(leaflet_tbl) <- c("ID", "Road Alias", "Start", "End", "pct", "Count Change")
leaflet_tbl <- mutate(leaflet_tbl, `Percent Change`= paste0(round(pct, digits = 2)))
leaflet_tbl <- leaflet_tbl[,c(1:4,7,6)]
leaflet_tbl <- data.table::as.data.table(leaflet_tbl)
leaflet_tbl_final <- grid.table(leaflet_tbl, rows = NULL)

