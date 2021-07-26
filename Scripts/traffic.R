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
#setting ID as the farleft column
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

#2010 to 2015 
traff_data<- as.data.frame(sapply(traffic_year[,1:7], as.numeric))
#getting pct change
traff_data <- mutate(traff_data, pctchange = (traff_data[,7]-traffic_data[,2])/traff_data[,2] *100)
#add a column with the absolute value of pct change column
traff_data <- mutate(traff_data, abs=abs(traff_data[,8]))
#setting ID to character for graphing
traff_data$ID <- as.character(traff_data$ID)
#put in descending order then select top
traff_data2 <- traff_data %>%
  arrange(desc(abs))
#get's rid of the lnf(the ones that start with 0 for the initial year)
traff_data2 <- traff_data2[5:260,] %>%
  slice(1:20)
#Graph
graph2 <- ggplot(traff_data2 , aes(x = reorder(ID,abs), y = pctchange)) +
  geom_bar(
    stat = "identity", position = position_stack(),
    color = "white") +
  ggtitle("2010 to 2015")+
  theme(plot.title = element_text(hjust=0.5))+
  geom_text(aes(label=paste0(round(pctchange), "%: ", abs(`2010`-`2015`))), size =4, position = position_dodge(width = 0.9))+
  coord_flip()

#2015-2020
t_data <- as.data.frame(sapply(traffic_year[,c(1,7:12)], as.numeric))
#getting pct change
t_data <- mutate(t_data, pctchange = (t_data[,7]-t_data[,2])/t_data[,2] *100)
#add a column with the absolute value of pct change column    
t_data <- mutate(t_data, abs=abs(t_data[,8]))
#setting ID to character for graphing
t_data$ID <- as.character(t_data$ID)
#put in descending order then select top
t_data2 <- t_data %>%
  arrange(desc(abs))
#get's rid of the lnf(the ones that start with 0 for the initial year)
t_data2 <- t_data2[3:262,] %>%
  slice(1:20)
#Graph
graph3 <- ggplot(t_data2 , aes(x = reorder(ID, abs), y = pctchange)) +
  geom_bar(
    stat = "identity", position = position_stack(),
    color = "white") +
  ggtitle("2015 to 2020") + 
  theme(plot.title = element_text(hjust=0.5)) +
geom_text(aes(label=paste0(round(pctchange), "%: ", abs(`2015`-`2020`))), size =4, position = position_dodge(width = 0.9)) +
  coord_flip()

#graphs
graph1
graph2
graph3

##########################################################mapping 2010-2020 data
library(mapdata)
library(leaflet)
library(htmlwidgets)
library(leafpop)
library(lattice)
library(leafpop)
#traffic_data4 from above gives us the 20 roads with the highest pct change 
traffic_data5 <- mutate(traffic_data4, count = abs(`2010`-`2020`))
#We are getting rid of the roads that have changed less than 30 counts
traffic_data6 <- traffic_data5 %>% filter(count >30)
#importing coordinate for the 18 roads in excel
coordinates <- read_excel("Data/traffic//traffic_coordinates.xlsx")
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

#using both rappk_data(for labels ,size etc) and rappk_data2 for the line plots
map <- leaflet(data =rappk_data2) %>% 
  addTiles() %>%
  fitBounds(lng1=-78.345941, lat1=38.875265,
            lng2=-77.934027, lat2=38.518670) %>%
  addCircleMarkers(lng=~lng,
                   lat=~lat,
                   radius = ~sqrt(abs(pctchange)),
                   color=ifelse(rappk_data2$pctchange <0, "red", "green"),
                   label =paste(rappk_data2$ID, "Counts Changed= ", rappk_data2$count))



map
