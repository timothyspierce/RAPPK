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

#2010-2020
traffic_data <- as.data.frame(sapply(traffic_year, as.numeric))
traffic_data <- mutate(traffic_data, pctchange = (traffic_data[,12]-traffic_data[,2])/traffic_data[,2] *100)
#getting the routes with the greatest pct change
#add a column with the absolute value of pct change column
traffic_data2 <- mutate(traffic_data, abs=abs(traffic_data[,13]))
#setting ID to character for graphing
traffic_data2$ID<- as.character(traffic_data2$ID)
#put in descending order then select top
traffic_data3 <- traffic_data2 %>%
  arrange(desc(abs))
#get's rid of the lnf(the ones that start with 0 for the initial year)
traffic_data4 <- traffic_data3[7:262,] %>%
  slice(1:20)
#graph
graph1 <- ggplot(traffic_data4, aes(x = reorder(ID,abs), y = pctchange)) +
  geom_bar(
    stat = "identity", position = position_stack(),
    color = "white") +
  ggtitle("2010 to 2020") +
  theme(plot.title = element_text(hjust=0.5))+
  geom_text(aes(label=paste0(round(pctchange), "%: ", abs(`2010`-`2020`))), size =4, position = position_dodge(width = 0.9))+
  coord_flip()

##########################################################mapping 2010-2020 data
library(mapdata)
library(leaflet)
library(htmlwidgets)
library(leafpop)
library(lattice)
library(leafpop)
library(ggplot2)
library(htmltools)
library(tigris)
library(tidycensus)
library(tidyverse)
library(leaflegend)

#traffic_data4 from above gives us the 20 roads with the highest pct change 
traffic_data5 <- mutate(traffic_data4, count = `2020`-`2010`)
traffic_data5 <- mutate(traffic_data5, count2 = abs(count))
#We are getting rid of the roads that have changed less than 30 counts
traffic_data6 <- traffic_data5 %>% filter(count2 >30)
#importing coordinate for the 18 roads in excel
coordinates <- read_excel("C:/Users/Christina Prisbe/Documents/R/Rappahannock/RAPPK/Data/traffic/traffic_coordinates.xlsx")
#merging the data with the coordinates(by ID)
rappk_data <- merge(coordinates, traffic_data6, by="ID")

#leaflet
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
#sizes <- sizeNumeric(rappk_data2$abs, baseSize = 20)

#positive data will be circles
pos_data <- rappk_data2 %>% filter(sign == "Positive")

#negative data will be triangles
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

#loops through all the data (18 routes)
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

#loops through all the data (18 routes)
r <- lapply(1:length(unique(neg_data$ID)), function(i) {
  pick_n(neg_data$ID[i])
})

#marker pop up labels
label_p <- lapply(paste("ID: ", pos_data$ID, "<br />", "Count Change: ", pos_data$count,
                        "<br />", "Percent Change: ", round(pos_data$pctchange, digits =2),"%"), HTML)

label_n <- lapply(paste("ID: ", neg_data$ID, "<br />", "Count Change: ", neg_data$count,
                        "<br />", "Percent Change: ", round(neg_data$pctchange, digits =2),"%"), HTML)

#addProviderTiles("CartoDB")
#addProviderTiles("Esri")
#map
map <- leaflet() %>% 
  addProviderTiles("Esri") %>%
  fitBounds(lng1=-78.345941, lat1=38.875265,
            lng2=-77.934027, lat2=38.518670) %>%
  addPolygons(data=subdivisions, popup = subdivisions$NAME, color = "black",
              fillColor = "#ffffff00")

#adding markers
markers_map <- map %>%
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
  addLegendSize(values =rappk_data2$abs, pal= numPal, opacity = 0.3, shape = "circle", title = "Percent Change from 2010 to 2020",
                position = "bottomright", strokeWidth = 10, breaks = 5) %>%
  addLegendNumeric(pal = numPal,bins=12, title = "Count change from 2010 to 2020", values = rappk_data2$count, position= "bottomright",
                   height = 200)

#Create a table with ID and road information (use the ID from the leaflet map to get more info)
#getting just the ID, start, end, and road alias
traffic_year3 <-traffic_year2[,c(14,6,12,13)]
leaflet_tbl <- filter(traffic_year3, ID %in% rappk_data$ID) 
#adding count anf pctchnage to the table
leaflet_tbl<- merge(leaflet_tbl, rappk_data[,c(1,18,20)], by ="ID")

