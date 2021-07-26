
library(gridExtra)
library(sf)
library(dplyr)
library(ggplot2)
library(tidycensus)
library(fpp2)
#functions
rapp_table <- function(varcode, year){
  get_acs(geography = "county subdivision",
          state = 51,
          county = 157,
          table = varcode,
          year = year,
          cache = TRUE,
          geometry = TRUE, output = "wide")
       
  }

rapp_var <- function(varcode, year){
  get_acs(geography = "county",
          state = 51,
          county = 157,
          variables = varcode,
          year = year,
          survey = "acs5",
          cache = TRUE,
          output = "wide")}

#####################################################################Percentages
#variable of the years used
years <- c(2019:2010)

#total, 18 to 24, 15 to 44, 60+, pulled from the years with the updated table (estimates given in number of people)
age_groups <- rbind(
  rapp_var(c("S0101_C01_001","S0101_C01_023","S0101_C01_024","S0101_C01_028"), 2019),
  rapp_var(c("S0101_C01_001","S0101_C01_023","S0101_C01_024","S0101_C01_028"), 2018),
  rapp_var(c("S0101_C01_001","S0101_C01_023","S0101_C01_024","S0101_C01_028"), 2017))
  
#total, 18 to 24, 15 to 44, 60+, pulled from the years with the old table (estimates given in percents)
 age_groups2 <- rbind(
                rapp_var(c("S0101_C01_001", "S0101_C01_022", "S0101_C01_023","S0101_C01_026"), 2016),
                rapp_var(c("S0101_C01_001", "S0101_C01_022", "S0101_C01_023","S0101_C01_026"), 2015),
                rapp_var(c("S0101_C01_001", "S0101_C01_022", "S0101_C01_023","S0101_C01_026"), 2014),
                rapp_var(c("S0101_C01_001", "S0101_C01_022", "S0101_C01_023","S0101_C01_026"), 2013),
                rapp_var(c("S0101_C01_001", "S0101_C01_022", "S0101_C01_023","S0101_C01_026"), 2012),
                rapp_var(c("S0101_C01_001", "S0101_C01_022", "S0101_C01_023","S0101_C01_026"), 2011),
                rapp_var(c("S0101_C01_001", "S0101_C01_022", "S0101_C01_023","S0101_C01_026"), 2010))

#pulling the wanted columns (total, 18 to 24, 15 to 44, 60+)
age_groups <- age_groups[, c(3,5,7,9)]
colnames(age_groups) <- c("Total", "18 to 24", "15 to 44", "60 and older")
#turning the number estimates into percentages to match the age_groups2
age_groups <- mutate(age_groups, first_pct =`18 to 24`/Total*100)
age_groups <- mutate(age_groups, sec_pct =`15 to 44`/Total*100)
age_groups <- mutate(age_groups, third_pct =`60 and older`/Total*100)
age_groups <- age_groups<- age_groups[,c(5:7)]
colnames(age_groups) <- c("18 to 24", "15 to 44", "60 and older")

#Getting rid of extra columns and labeling
age_groups2 <- age_groups2[,c(5,7,9)]
colnames(age_groups2) <- c("18 to 24", "15 to 44", "60 and older")
#combines the two data frames
age_groups3 <-bind_rows(age_groups, age_groups2)
#adds the a year column
age_groups3 <- cbind(years, age_groups3)

#creates a data frame just with the estimate columns
age_groups4 <- age_groups3[,c(2:4)]
#putting estimates in the same column
age_groups5 <- data.frame(estimate = unlist(age_groups4, use.names = FALSE))
#labeling
age_groups5 <- mutate(age_groups5, Year =c("2019", "2018", "2017", "2016", "2015","2014",
                      "2013", "2012", "2011", "2010","2019", "2018", "2017", "2016", "2015","2014",
                      "2013", "2012", "2011", "2010","2019", "2018", "2017", "2016", "2015","2014",
                      "2013", "2012", "2011", "2010"))
age_groups5 <- mutate(age_groups5, Ages = c("18 to 24", "18 to 24","18 to 24","18 to 24","18 to 24",
                                            "18 to 24","18 to 24","18 to 24","18 to 24","18 to 24",
                                            "15 to 44","15 to 44","15 to 44","15 to 44","15 to 44",
                                            "15 to 44","15 to 44","15 to 44","15 to 44","15 to 44",
                                            "60 and older", "60 and older", "60 and older", "60 and older", 
                                            "60 and older", "60 and older", "60 and older", "60 and older",
                                            "60 and older", "60 and older"))
colnames(age_groups5) <- c("Percent of Population", "Year", "Age Groups")

#Basic Graph for 15 to 44, , 18 to 24, 60+ (not used)
age_groups5 %>%
ggplot(aes(x = Year, y = `Percent of Population`, group = `Age Groups`, color = `Age Groups`)) + 
  geom_line(position = "identity", show.legend = TRUE) +
  ggtitle("Age Demographic") +
  theme(plot.title = element_text(hjust = 0.5))


##Creating a data frame for just 15 to 44 and 60+
age_groups6 <- age_groups5[c(11:30),]

#Graph for percent population
#geom_line adjusted the size and legend of the lines
#theme centers the title, adjusts the axis and legend text and titles
#the two colors selected were a part of cbPalette <- c("#E69F00","#56B4E9","#009E73","#0072B2","#D55E00", "#CC79A7")
perc_plot <- age_groups6 %>%
  ggplot(aes(x = Year, y = `Percent of Population`, group = `Age Groups`, color = `Age Groups`)) + 
  geom_line(position = "identity", show.legend = TRUE, size=2) +
  ggtitle("Age Population Percentages") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        axis.title.x=element_text(size =13),
        axis.title.y=element_text(size =13)) +
  scale_color_manual(values=c("#56B4E9","#0072B2"))


######################################################################estimates
#total, 18 to 24, 15 to 44, 60+, pulled from the years with the updated table (estimates given in number of people)
rapp_age_groups <- rbind(
  rapp_var(c("S0101_C01_001","S0101_C01_023","S0101_C01_024","S0101_C01_028"), 2019),
  rapp_var(c("S0101_C01_001","S0101_C01_023","S0101_C01_024","S0101_C01_028"), 2018),
  rapp_var(c("S0101_C01_001","S0101_C01_023","S0101_C01_024","S0101_C01_028"), 2017))

#total, 18 to 24, 15 to 44, 60+, pulled from the years with the old table (estimates given in percents)
rapp_age_groups2 <- rbind(
  rapp_var(c("S0101_C01_001", "S0101_C01_022", "S0101_C01_023","S0101_C01_026"), 2016),
  rapp_var(c("S0101_C01_001", "S0101_C01_022", "S0101_C01_023","S0101_C01_026"), 2015),
  rapp_var(c("S0101_C01_001", "S0101_C01_022", "S0101_C01_023","S0101_C01_026"), 2014),
  rapp_var(c("S0101_C01_001", "S0101_C01_022", "S0101_C01_023","S0101_C01_026"), 2013),
  rapp_var(c("S0101_C01_001", "S0101_C01_022", "S0101_C01_023","S0101_C01_026"), 2012),
  rapp_var(c("S0101_C01_001", "S0101_C01_022", "S0101_C01_023","S0101_C01_026"), 2011),
  rapp_var(c("S0101_C01_001", "S0101_C01_022", "S0101_C01_023","S0101_C01_026"), 2010))

#getting rid of extra columns and labeling
rapp_age_groups <- rapp_age_groups[, c(5,7,9)]
colnames(rapp_age_groups) <- c("18 to 24", "15 to 44", "60 and older")

#getting rid of extra columns and labeling
rapp_age_groups2 <- rapp_age_groups2[, c(3,5,7,9)]
colnames(rapp_age_groups2) <- c("Total", "18 to 24", "15 to 44", "60 and older")
# changing the percentages into estimates to match rapp_age_groups
rapp_age_groups2 <- mutate(rapp_age_groups2, est1 =`18 to 24`*Total/100)
rapp_age_groups2 <- mutate(rapp_age_groups2, est2 =`15 to 44`*Total/100)
rapp_age_groups2 <- mutate(rapp_age_groups2, est3 =`60 and older`*Total/100)

#selecting just the estimates
rapp_age_groups3 <- rapp_age_groups2[, c(5:7)]
colnames(rapp_age_groups3) <- c("18 to 24", "15 to 44", "60 and older")
#combines the two data frames
rapp_age_groups4 <-bind_rows(rapp_age_groups, rapp_age_groups3)
#adds a year column
rapp_age_groups4 <- cbind(years,rapp_age_groups4)

#Creates a datframe with just the estimate
rapp_age_groups5 <- rapp_age_groups4[,c(2:4)]
#puts the estimates in the same column
rapp_age_groups6 <- data.frame(estimate = unlist(rapp_age_groups5, use.names = FALSE))
#labeling
rapp_age_groups6 <- mutate(rapp_age_groups6, Year =c("2019", "2018", "2017", "2016", "2015","2014",
                                           "2013", "2012", "2011", "2010","2019", "2018", "2017", "2016", "2015","2014",
                                           "2013", "2012", "2011", "2010","2019", "2018", "2017", "2016", "2015","2014",
                                           "2013", "2012", "2011", "2010"))
rapp_age_groups6 <- mutate(rapp_age_groups6, Ages = c("18 to 24", "18 to 24","18 to 24","18 to 24","18 to 24",
                                            "18 to 24","18 to 24","18 to 24","18 to 24","18 to 24",
                                            "15 to 44","15 to 44","15 to 44","15 to 44","15 to 44",
                                            "15 to 44","15 to 44","15 to 44","15 to 44","15 to 44",
                                            "60 and older", "60 and older", "60 and older", "60 and older", 
                                            "60 and older", "60 and older", "60 and older", "60 and older",
                                            "60 and older", "60 and older"))
colnames(rapp_age_groups6) <- c("Population", "Year", "Age Groups")

##Creating a data frame for just 15 to 44 and 60+
rapp_age_groups7 <- rapp_age_groups6[c(11:30),]

#Graph for age overtime (estimates)
#geom_line adjusted the size and legend of the lines
#theme centers the title, adjusts the axis and legend text and titles
#the two colors selected were a part of cbPalette <- c("#E69F00","#56B4E9","#009E73","#0072B2","#D55E00", "#CC79A7")
est_plot <- rapp_age_groups7 %>%
  ggplot(aes(x = Year, y = `Population`, group = `Age Groups`, color = `Age Groups`)) + 
  geom_line(position = "identity", show.legend = TRUE, size =2) +
  ggtitle("Age Populations") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        axis.title.x=element_text(size =13),
        axis.title.y=element_text(size =13)) +
  scale_color_manual(values=c("#56B4E9","#0072B2"))

########################################################arranging the two graphs
#arranging the percentage graph and estimate graph side by side
grid.arrange(est_plot, perc_plot, ncol=2,
             top = textGrob("Age Demographic in Rappahannock from 2010 to 2019",
                            gp=gpar(fontsize=20,font=3)))
