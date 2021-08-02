## Rappahannock County Age
###Age categories
library(tidycensus)
library(tidyverse)
library(dplyr)
library(ggplot2)

rapp_table <- function(varcode, year){
  data.frame(get_acs(geography = "county", state = 51,
                     county = 157,
                     table = varcode,
                     year = year))}

##################################################################Age categories
#Rappk County
age1 <- rapp_table("B01001", 2019)

#total population
total_pop<- age1[1,4]

#adding the male and female estimates to get the total
male_age <- age1[3:25,]
female_age <- age1[27:49,]
male_age <- tibble::rowid_to_column(male_age, "ID")
female_age <- tibble::rowid_to_column(female_age, "ID")
ages <- merge(female_age, male_age, by = "ID")
ages <- mutate(ages, total = estimate.x + estimate.y)

#adding columns to make the four groups (under18, young adult, middle age, elder)
rappk_ages1<-data.frame(t(ages[,12]))
rappk_ages1 <- mutate(rappk_ages1, Under18 = X1 + X2 + X3 + X4)
rappk_ages1 <- mutate(rappk_ages1, YoungAdult = X5 + X6 + X7 + X8 + X9)
rappk_ages1 <- mutate(rappk_ages1, MiddleAge = X10 + X11 + X12 + X13 + X14 +X15 + X16 +X17)
rappk_ages1 <- mutate(rappk_ages1, Elder = X18 + X19 + X20 + X21 + X22 +X23)

#selecting the four columns we want (four age groups)
rappk_ages2 <- rappk_ages1[,24:27]

#turning the rows back into columns then making percentages
row.names(rappk_ages2) <- c("Estimate")
rappk_ages2 <- data.frame(t(rappk_ages2))
rappk_ages2 <- mutate(rappk_ages2, TotalPopulation = total_pop)
rappk_ages2 <- mutate(rappk_ages2, PctPop = Estimate/TotalPopulation*100)

#labeling
rappk_ages2 <- mutate(rappk_ages2, labels = c("Under 18", "18 to 30", "30 to 65", "65 and above"))
rappk_ages2 <- mutate(rappk_ages2, Key = c("Adolescent", "Young Adult", "Middle-Aged", "Senior"))
rappk_ages2 <- mutate(rappk_ages2, roundPct = round(PctPop))
colnames(rappk_ages2) <- c("Estimate", "Total Population", "Percent of Population", "Ages", "Key", "Rounded")
#Put the groups in the order I want
rappk_ages3 <- rappk_ages2
rappk_ages3$Key <- factor(rappk_ages3$Key, levels = c("Adolescent", "Young Adult", "Middle-Aged", "Senior"))

#Color-blind friendly  Palette
cbPalette <- c("#E69F00","#56B4E9","#009E73","#0072B2","#D55E00", "#CC79A7")

###Graph(bar plot of ages in rappk)
age_group_rappk_bar_plot <- ggplot(data = rappk_ages3, aes(x = Ages, y = `Percent of Population`, fill=Key)) +
  geom_bar(stat="identity") +
  scale_fill_manual("Key", values=cbPalette) +
  scale_x_discrete(limits=c("Under 18", "18 to 30", "30 to 65", "65 and above")) +
  geom_text(aes(label=paste0(Rounded,"%")), vjust=1.5, colour="white", size=3.5) + 
  theme(legend.title = element_blank())
  
#getting rid of extra columns and labeling for graph
rappk_ages4 <- rappk_ages3[,c(3,6)]
rappk_ages4 <- mutate(rappk_ages4, Key = c("Adolescent: Under 18", "Young Adult: 18 to 30", "Middle-Aged: 30 to 65", "Senior: 65 and Over"))
colnames(rappk_ages4) <- c("Percent of Population","Rounded", "Key")
rappk_ages4$Key <- factor(rappk_ages4$Key, levels = c("Adolescent: Under 18", "Young Adult: 18 to 30", "Middle-Aged: 30 to 65", "Senior: 65 and Over"))


####Graph(Age Groups in Rappahannock County Pie chart)
age_group_rappk_pie_plot <- ggplot(rappk_ages4, aes(x="", y=`Percent of Population`, fill=Key)) +
  geom_bar(stat="identity", width=1, color =1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(Rounded, "%")), position = position_stack(vjust=0.5), size =3.5) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_manual(values=cbPalette) +
  ggtitle("Rappahannock Age Demographic") +
  theme(plot.title = element_text(hjust = 0.5))


##General Virginia Age breakdown
va_table <- function(varcode, year){
  data.frame(get_acs(geography = "state", state = 51,
                     table = varcode,
                     year = year))}

va_ages <- va_table("B01001", 2019)

#total population
va_total_pop<- va_ages[1,4]

#adding the male and female estimates to get the total
va_male_age <- va_ages[3:25,]
va_female_age <- va_ages[27:49,]
va_male_age <- tibble::rowid_to_column(va_male_age, "ID")
va_female_age <- tibble::rowid_to_column(va_female_age, "ID")
va_ages2 <- merge(va_female_age, va_male_age, by = "ID")
va_ages2 <- mutate(va_ages2, total = estimate.x + estimate.y)

#adding columns to make the four groups (under18, young adult, middle age, elder)
va_ages3<-data.frame(t(va_ages2[,12]))
va_ages3 <- mutate(va_ages3, Under18 = X1 + X2 + X3 + X4)
va_ages3 <- mutate(va_ages3, YoungAdult = X5 + X6 + X7 + X8 + X9)
va_ages3 <- mutate(va_ages3, MiddleAge = X10 + X11 + X12 + X13 + X14 +X15 + X16 +X17)
va_ages3 <- mutate(va_ages3, Elder = X18 + X19 + X20 + X21 + X22 +X23)

#selecting the four columns we want (four age groups)
va_ages4 <- va_ages3[,24:27]

#turning the rows back into columns then making percentages
row.names(va_ages4) <- c("Estimate")
va_ages4 <- data.frame(t(va_ages4))
va_ages4 <- mutate(va_ages4, TotalPopulation = va_total_pop)
va_ages4 <- mutate(va_ages4, PctPop = Estimate/TotalPopulation*100)

#labeling
va_ages4 <- mutate(va_ages4, labels = c("Under 18", "18 to 30", "30 to 65", "65 and above"))
va_ages4 <- mutate(va_ages4, Key = c("Adolescent", "Young Adult", "Middle-Aged", "Senior"))
va_ages4 <- mutate(va_ages4, roundPct = round(PctPop))
colnames(va_ages4) <- c("Estimate", "Total Population", "Percent of Population", "Ages", "Key", "Rounded")

#Put the groups in the order I want
va_ages5 <- va_ages4
va_ages5$Key <- factor(va_ages5$Key, levels = c("Adolescent", "Young Adult", "Middle-Aged", "Senior"))

#getting rid of extra columns and labeling
va_ages6 <- va_ages5[,c(3,6)]
va_ages6 <- mutate(va_ages6, Key = c("Adolescent: Under 18", "Young Adult: 18 to 30", "Middle-Aged: 30 to 65", "Senior: 65 and Over"))
colnames(va_ages6) <- c("Percent of Population","Rounded", "Key")
va_ages6$Key <- factor(va_ages6$Key, levels = c("Adolescent: Under 18", "Young Adult: 18 to 30", "Middle-Aged: 30 to 65", "Senior: 65 and Over"))

#graph for VA ages
age_group_va_pie_plot <- ggplot(va_ages6, aes(x="", y=`Percent of Population`, fill=Key)) +
  geom_bar(stat="identity", width=1, color=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(Rounded, "%")), position = position_stack(vjust=0.5), size=3.5) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_manual(values=cbPalette) + ggtitle("Virginia Age Demographic") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())


################################################################Age Dependencies
##Surrounding Counties including Rappk
county_tbl <- function(varcode, year){
  data.frame(get_acs(geography = "county", state = 51,
                     county =  c(157, 047, 113, 139, 187, 061),
                     table = varcode,
                     year = year))}
county_data <- county_tbl("S0101", 2019)

#Getting the three dependencies (age, old-age, child)
county_dep <- county_data %>%
  group_by(NAME) %>%
  slice(34:36) 

#getting rid of extra columns and labeling
county_dep <- county_dep[,c(2,4,5)]
county_dep <- mutate(county_dep, Stat = c("Age Dependecy", "Old-age Dependecy", "Child Dependecy"))
county_dep <- county_dep %>% ungroup() 
county_dep <- mutate(county_dep, county = c("Culpeper", "Culpeper","Culpeper", "Fauquier", "Fauquier", "Fauquier",
                                            "Madison", "Madison", "Madison", "Page", "Page", "Page", "Rappahannock",
                                            "Rappahannock", "Rappahannock","Warren", "Warren", "Warren"))
colnames(county_dep) <- c("Name", "Dependency Ratio", "MOE", "Key", "County")

#Graph(Age Dependency Ratios in Virginia Counties)
counties_dep_plot <- ggplot(county_dep, aes(x=County, y=`Dependency Ratio`, fill=Key)) +
  geom_bar(stat='identity', position='dodge') +
  ggtitle("Age Dependecy Ratios in Virginia Counties") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 8.5),legend.title = element_blank()) +
  scale_fill_manual(values=cbPalette)

##Subdivision(districts)
sub_tbl <- function(varcode, year){
  data.frame(get_acs(geography = "county subdivision", state = 51,
                     county =  157,
                     table = varcode,
                     year = year))}

sub_data1 <- sub_tbl("S0101", 2019)


#Getting the three depednecies (age, old-age, child)
sub_dep <- sub_data1 %>%
  group_by(NAME) %>%
  slice(34:36) 

#getting rid of extra columns and labeling
sub_dep <- sub_dep[,c(2,4,5)]
sub_dep <- mutate(sub_dep, Stat = c("Age Dependecy", "Old-age Dependecy", "Child Dependecy"))
sub_dep <- sub_dep %>% ungroup()
sub_dep <- mutate(sub_dep, district = c("Hampton", "Hampton", "Hampton", "Jackson", "Jackson", "Jackson",
                                        "Piedmont", "Piedmont", "Piedmont", "Stonewall-Hawthorne", "Stonewall-Hawthorne",
                                        "Stonewall-Hawthorne", "Wakefield", "Wakefield", "Wakefield" ))
colnames(sub_dep) <- c("Name", "Dependency Ratio", "MOE", "Key", "District")

#Graph(Age Dependency Ratios in Rappahannock Districts)
district_dep_plot <- ggplot(sub_dep, aes(x=District, y=`Dependency Ratio`, fill=Key)) +
  geom_bar(stat='identity', position='dodge')  +
  ggtitle("Age Dependecy Ratios in Rappahannock Districts") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 8.5), legend.title = element_blank()) +
  scale_fill_manual(values=cbPalette)


##Getting Virginia  age dependencies
va_dep_data <- va_table("S0101", 2019)

#getting the three estimates(age, old-age, child)
va_dep <- va_dep_data %>%
  group_by(NAME) %>%
  slice(34:36) 

#Getting rid of extra columns
va_dep <- va_dep[,c(2,4,5)]
va_dep <- mutate(va_dep, Stat = c("Age Dependecy", "Old-age Dependecy", "Child Dependecy"))
va_dep <- va_dep %>% ungroup()

#selecting the estimates column
va_dep2 <- va_dep[,2]
colnames(va_dep2) <- "ratio"
county_dep2 <- county_dep[13:15,2]
colnames(county_dep2) <- "ratio"

#labeling
va_rappk_dep <- rbind(va_dep2, county_dep2)
va_rappk_dep <- mutate(va_rappk_dep, Location = c("Virginia", "Virginia", "Virginia", "Rappahannock",
                                             "Rappahannock", "Rappahannock"))
va_rappk_dep <- mutate(va_rappk_dep, stat = c("Age Dependency", "Old-Age Dependency", "Child Dependency", 
                                              "Age Dependency", "Old-Age Dependency", "Child Dependency"))
colnames(va_rappk_dep) <- c("Dependency Ratio", "Location", "Key")

#graph rappk dependencies
va_dep_plot <- ggplot(va_rappk_dep, aes(x=`Location`, y=`Dependency Ratio`, fill=Key)) +
  geom_bar(stat='identity', position='dodge')  +
  ggtitle("Age Dependecy Ratios") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 8.5), legend.title = element_blank(),  axis.title.x=element_blank()) +
  scale_fill_manual(values=cbPalette)

######################################################################Median Age
##VA Median Age 
va_table <- function(varcode, year){
  data.frame(get_acs(geography = "state", state = 51,
                     table = varcode,
                     year = year))}

va_data <-va_table("S0101", 2019)

#Gets the median age (38.2)
va_median_age <- va_data[32,4]


##Surrounding counties(including Rappk)
#use county_tbl from above(age depency section)
counties_data <- county_tbl("S0101", 2019)

#getting only the median age data
counties_median_age <- counties_data %>%
  group_by(NAME) %>%
  slice(32) %>%
  ungroup()

#labeling
counties_median_age <- mutate(counties_median_age, County = c("Culpeper", "Faquier", "Madison", "Page", "Rappahannock", "Warren"))
counties_median_age <- mutate(counties_median_age, Key = c("Culpeper County", "Faquier County", "Madison County", "Page County", "Rappahannock County", "Warren County"))                              
colnames(counties_median_age) <- c("GEOID", "Name", "Variable", "Median Age", "MOE", "County", "Key")


#Graph(Median Ages in Virginia Counties)
counties_median_age_plot <- ggplot(counties_median_age, aes(x=County, y=`Median Age`, fill=Key)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(round(`Median Age`))), vjust=1.5, colour="white", size=3.5) +
  theme(axis.text.x = element_text(size = 8.5)) +
  geom_hline(aes(yintercept= 38.2, linetype = "   Virginia Median Age: 38"), 
             color= "black", size = 1.5, alpha = 0.25) +
  ggtitle("Median Age in Virginia Counties") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(values=cbPalette)

##Subdivision (districts)
#use sub_tbl function from above
sub_data2 <- sub_tbl("S0101", 2019)

#getting only the median age data
sub_median_age <- sub_data2 %>%
  group_by(NAME) %>%
  slice(32) %>%
  ungroup()

#labeling
sub_median_age <- mutate(sub_median_age, Districts = c("Hampton", "Jackson", "Piedmont", "Stonewall-Hawthorne", "Wakefield"))
sub_median_age <- mutate(sub_median_age, Key = c("Hampton District", "Jackson District", "Piedmont District", "Stonewall-Hawthorne District", "Wakefield District"))
colnames(sub_median_age) <- c("GEOID", "Name", "Variable", "Median Age", "MOE", "District", "Key")

#putting the bars in alphabetical order
sub_median_age$District <- factor(sub_median_age$District, levels = c("Hampton", "Jackson", "Piedmont", "Stonewall-Hawthorne", "Wakefield"))

#Graph(Median Age in Rappahannock Districts)
district_median_age <- ggplot(data = sub_median_age, aes(x = District, y = `Median Age`, fill = Key)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(round(`Median Age`))), vjust=1.5, colour="white", size=3.5) +
  theme(axis.text.x = element_text(size = 8.5)) +
  geom_hline(aes(yintercept= 50.1, linetype = "Rappahannock Median Age: 50"), color= "black", size = 1.5, alpha = 0.25) +
  ggtitle("Median Age in Rappahannock Districts") +
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),
        axis.title.x=element_blank()) +
  scale_fill_manual(values=cbPalette)

#################################################################Plot all Graphs
library(ggplot2)
library(gridExtra)

row1 <- grid.arrange(age_group_va_pie_plot, age_group_rappk_pie_plot, ncol =2)
row2 <- grid.arrange(age_group_rappk_bar_plot, va_dep_plot, ncol=2)
row3 <- grid.arrange(counties_dep_plot, district_dep_plot, ncol =2)
row4 <- grid.arrange(counties_median_age_plot, district_median_age, ncol =2)
final <- grid.arrange(row1, row2, row3, row4, ncol =1)





