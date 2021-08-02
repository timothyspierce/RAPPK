library(ggplot2)
library(tidycensus)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(grid)

###########################################################people in a household
rapp_table <- function(varcode, year){
  get_acs(geography = "county",
          state = 51,
          county = 157,
          table = varcode,
          year = year)
}
#person per household in occupied housing units
one <- c(rapp_table("S2501_C02", 2019)[2,4])
two <-c(rapp_table("S2501_C02", 2019)[3,4])
three <- c(rapp_table("S2501_C02", 2019)[4,4])
four_plus <- c(rapp_table("S2501_C02", 2019)[5,4])
size <- data.frame(rbind(one, two, three, four_plus))
size <- mutate(size, People = c("One", "Two", "Three", "Four or more"))
 
write.csv(size, file = "shiny_app/data/TableS2501FiveYearEstimates/householdSize.csv")
householdSize <- read.csv("shiny_app/data/TableS2501FiveYearEstimates/householdSize.csv")

#graph
household_size_graph <- ggplot(householdSize, aes(x = "", y = estimate, fill = fct_inorder(People))) +
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

#################################################################owner vs renter
#over time with two data points (owners vs renters)
rapp_var <- function(varcode, year){
  get_acs(geography = "county",
          state = 51,
          county = 157,
          variables = varcode,
          year = year,
          survey = "acs5",
          output = "wide")}
#pulling that data for 2010-2019
own_rent1 <- rbind(
  rapp_var(c("S2504_C03_001","S2504_C05_001"), 2019),
  rapp_var(c("S2504_C03_001","S2504_C05_001"), 2018),
  rapp_var(c("S2504_C03_001","S2504_C05_001"), 2017))
#changed to set up of the table after 2016
own_rent2 <- rbind(
  rapp_var(c("S2504_C02_001","S2504_C03_001"), 2016),
  rapp_var(c("S2504_C02_001","S2504_C03_001"), 2015),
  rapp_var(c("S2504_C02_001","S2504_C03_001"), 2014),
  rapp_var(c("S2504_C02_001","S2504_C03_001"), 2013),
  rapp_var(c("S2504_C02_001","S2504_C03_001"), 2012),
  rapp_var(c("S2504_C02_001","S2504_C03_001"), 2011),
  rapp_var(c("S2504_C02_001","S2504_C03_001"), 2010))
#putting in same format in order to rbind
own_rent1 <- own_rent1[,c(3,5)]
colnames(own_rent1) <- c("own", "rent")
own_rent2 <- own_rent2[,c(3,5)]
colnames(own_rent2) <- c("own", "rent")
own_rent3 <- rbind(own_rent1, own_rent2)
#graphing rent and own separately
own <- own_rent3[,1]
rent <- own_rent3[,2]
#adding a year column
years <- c(2019:2010)
own <- mutate(own, year = years)
rent <- mutate(rent, year = years)
colnames(own) <- c("Household Units", "year")
colnames(rent) <- c("Household Units", "year")

write.csv(own, file = "shiny_app/data/TableS2501FiveYearEstimates/ownerOccupied.csv")
write.csv(rent, file = "shiny_app/data/TableS2501FiveYearEstimates/renterOccupied.csv")

own <- read.csv("shiny_app/data/TableS2501FiveYearEstimates/ownerOccupied.csv")
rent <- read.csv("shiny_app/data/TableS2501FiveYearEstimates/renterOccupied.csv")


#Graph(own)
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
house_occ <- grid.arrange(own_graph, rent_graph, ncol=1)


###########################################################Vehicle per household
#rappk data alone
no_veh <- c(rapp_table("S2504_C02", 2019)[27,4])
one_veh <- c(rapp_table("S2504_C02", 2019)[28,4])
two_veh <- c(rapp_table("S2504_C02", 2019)[29,4])
three_veh <- c(rapp_table("S2504_C02", 2019)[30,4])
rappk_veh <- data.frame(rbind(no_veh,one_veh,two_veh,three_veh))
rappk_veh <- mutate(rappk_veh, type = c("None", "One", "Two", "Three or more"))


write.csv(rappk_veh, file ="shiny_app/data/TableS2501FiveYearEstimates/vehiclesHousehold.csv")
rappk_veh <- read.csv("shiny_app/data/TableS2501FiveYearEstimates/vehiclesHousehold.csv")
rappk_veh <- mutate(rappk_veh, type = c("None", "One", "Two", "Three or more"))

#graph
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



#surrounding counties data (including rappk)
county_tbl <- function(varcode, year){
  data.frame(get_acs(geography = "county", state = 51,
                     county =  c(157, 047, 113, 139, 187, 061),
                     table = varcode,
                     year = year))}

county_data <- county_tbl("S2504_C02",2019)
county_veh <- county_data %>% group_by(NAME) %>% slice(27:30) %>% ungroup()
county_veh2 <- county_veh[,4]
county_veh2 <- mutate(county_veh2, county= c(replicate(4, "Culpeper"), replicate(4,"Fauquier"),
                                             replicate(4,"Madison"), replicate(4,"Page"),
                                             replicate(4,"Rappahannock"), replicate(4,"Warren")))
county_veh2 <- mutate(county_veh2, num = rep( c("None", "One", "Two", "Three or more"),6))
#ordering the number of cars column
county_veh2$county <- factor(county_veh2$county, levels = c("Warren", "Madison","Page", "Fauquier","Culpeper","Rappahannock"))
county_veh2$num <- factor(county_veh2$num, levels = c("None", "One", "Two", "Three or more"))


write.csv(county_veh2, file ="shiny_app/data/TableS2501FiveYearEstimates/vehiclesHouseholdCounty.csv")

county_veh2 <- read.csv("shiny_app/data/TableS2501FiveYearEstimates/vehiclesHouseholdCounty.csv")

#Graph position

county_veh2$num <- factor(county_veh2$num, levels = c("None", "One", "Two", "Three or more"))

county_veh3 <- county_veh2 %>%
  group_by(county) %>%
  arrange(county, desc(num)) %>%
  mutate(lab_ypos = cumsum(estimate) - 0.20 * estimate) 


#Graph
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


grid.arrange(rappk_veh_plot, vehicle_graph, ncol=1)


########################################################combining all the graphs
row <- grid.arrange(household_size_graph, rappk_veh_plot, ncol=2)
grid.arrange(house_occ, row, vehicle_graph, ncol =1)
