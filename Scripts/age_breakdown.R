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

total_pop<- age1[1,4]

male_age <- age1[3:25,]
female_age <- age1[27:49,]
male_age <- tibble::rowid_to_column(male_age, "ID")
female_age <- tibble::rowid_to_column(female_age, "ID")

ages <- merge(female_age, male_age, by = "ID")
ages <- mutate(ages, total = estimate.x + estimate.y)

rappk_ages1<-data.frame(t(ages[,12]))
rappk_ages1 <- mutate(rappk_ages1, Under18 = X1 + X2 + X3 + X4)
rappk_ages1 <- mutate(rappk_ages1, YoungAdult = X5 + X6 + X7 + X8 + X9)
rappk_ages1 <- mutate(rappk_ages1, MiddleAge = X10 + X11 + X12 + X13 + X14 +X15 + X16 +X17)
rappk_ages1 <- mutate(rappk_ages1, Elder = X18 + X19 + X20 + X21 + X22 +X23)

rappk_ages2 <- rappk_ages1[,24:27]

row.names(rappk_ages2) <- c("Estimate")
rappk_ages2 <- data.frame(t(rappk_ages2))
rappk_ages2 <- mutate(rappk_ages2, TotalPopulation = total_pop)
#Make Percentage
rappk_ages2 <- mutate(rappk_ages2, PctPop = Estimate/TotalPopulation*100)


##Graph(Age Groups in Rappahannock County)
library(forcats)

rappk_ages2 <- mutate(rappk_ages2, labels = c("Under 18", "18 to 30", "30 to 65", "65 and above"))
rappk_ages2 <- mutate(rappk_ages2, Key = c("Adolescent", "Young Adult", "Middle-Aged", "Senior"))
rappk_ages2 <- mutate(rappk_ages2, roundPct = round(PctPop))
colnames(rappk_ages2) <- c("Estimate", "Total Population", "Percent of Population", "Ages", "Key", "Rounded")

rappk_ages3 <- rappk_ages2
rappk_ages3$Key <- factor(rappk_ages3$Key, levels = c("Adolescent", "Young Adult", "Middle-Aged", "Senior"))



age_group_rappk_plot <- ggplot(data = rappk_ages3, aes(x = Ages, y = `Percent of Population`, fill=Key)) +
  geom_bar(stat="identity") +
  scale_fill_manual("Key", values = c("Adolescent" = "slategray2", "Young Adult" = "slategray3",
                                      "Middle-Aged" = "slategray4",  "Senior" = "slategray")) +
  scale_x_discrete(limits=c("Under 18", "18 to 30", "30 to 65", "65 and above")) +
  ggtitle("Age Groups in Rappahannock County") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=Rounded), vjust=1.5, colour="white", size=3.5)

################################################################Age Dependencies
##Surrounding Counties including Rappk
county_tbl <- function(varcode, year){
  data.frame(get_acs(geography = "county", state = 51,
                     county =  c(157, 047, 113, 139, 187, 061),
                     table = varcode,
                     year = year))}
county_data <- county_tbl("S0101", 2019)

county_dep <- county_data %>%
  group_by(NAME) %>%
  slice(34:36) 

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
  theme(axis.text.x = element_text(size = 7.7))

##Subdivision(districts)
sub_tbl <- function(varcode, year){
  data.frame(get_acs(geography = "county subdivision", state = 51,
                     county =  157,
                     table = varcode,
                     year = year))}

sub_data1 <- sub_tbl("S0101", 2019)

sub_dep <- sub_data1 %>%
  group_by(NAME) %>%
  slice(34:36) 

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
  theme(axis.text.x = element_text(size = 7.7))

######################################################################Median Age
##VA Median Age 
va_table <- function(varcode, year){
  data.frame(get_acs(geography = "state", state = 51,
                     table = varcode,
                     year = year))}

va_data <-va_table("S0101", 2019)
va_median_age <- va_data[32,4]
#median age is 38.2

##Surrounding counties(including Rappk)
#use county_tbl from above(age depency section)
counties_data <- county_tbl("S0101", 2019)

counties_median_age <- counties_data %>%
  group_by(NAME) %>%
  slice(32) %>%
  ungroup()

counties_median_age <- mutate(counties_median_age, County = c("Culpeper", "Faquier", "Madison", "Page", "Rappahannock", "Warren"))
counties_median_age <- mutate(counties_median_age, Key = c("Culpeper COunty", "Faquier County", "Madison County", "Page County", "Rappahannock County", "Warren County"))                              
colnames(counties_median_age) <- c("GEOID", "Name", "Variable", "Median Age", "MOE", "County", "Key")


#Graph(Median Ages in Virginia Counties)
counties_median_age_plot <- ggplot(counties_median_age, aes(x=County, y=`Median Age`, fill=Key)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=`Median Age`), vjust=1.5, colour="white", size=3.5) +
  #theme(axis.text.x = element_text(size = 7.7)) +
  geom_hline(aes(yintercept= 38.2, linetype = "Virginia Median Age"), 
             color= "black", size = 1.5, alpha = 0.25) +
  ggtitle("Median Age in Virginia Counties") +
  theme(plot.title = element_text(hjust = 0.5))

##Subdivision (districts)
#use sub_tbl function from above
sub_data2 <- sub_tbl("S0101", 2019)

sub_median_age <- sub_data2 %>%
  group_by(NAME) %>%
  slice(32) %>%
  ungroup()

sub_median_age <- mutate(sub_median_age, Districts = c("Hampton", "Jackson", "Piedmont", "Stonewall-Hawthorne", "Wakefield"))
sub_median_age <- mutate(sub_median_age, Key = c("Hampton District", "Jackson District", "Piedmont District", "Stonewall-Hawthorne District", "Wakefield District"))
colnames(sub_median_age) <- c("GEOID", "Name", "Variable", "Median Age", "MOE", "District", "Key")

#Graph(Median Age in Rappahannock Districts)
district_median_age <- ggplot(data = sub_median_age, aes(x = District, y = `Median Age`, fill = Key)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=`Median Age`), vjust=1.5, colour="white", size=3.5) +
  #theme(axis.text.x = element_text(size = 7.7)) +
  geom_hline(aes(yintercept= 50.1, linetype = "Rappahannock Median Age"), color= "black", size = 1.5, alpha = 0.25) +
  ggtitle("Median Age in Rappahannock Districts") +
  theme(plot.title = element_text(hjust = 0.5))

########################################################################Plot all Graphs
library(ggplot2)
library(gridExtra)


row1 <- age_group_rappk_plot
row2 <- grid.arrange(counties_dep_plot, district_dep_plot, ncol =2)
row3 <- grid.arrange(counties_median_age_plot, district_median_age, ncol =2)
final <- grid.arrange(row1, row2, row3, ncol =1)

final




