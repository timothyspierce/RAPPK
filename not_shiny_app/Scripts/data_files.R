library(tidycensus)
library(tidyverse)
library(dplyr)

#age group files-------------------------------------------------------------
rapp_table <- function(varcode, year){
  data.frame(get_acs(geography = "county", state = 51,
                     county = 157,
                     table = varcode,
                     year = year))}
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

#Put the groups in order
rappk_ages3 <- rappk_ages2
rappk_ages3$Key <- factor(rappk_ages3$Key, levels = c("Adolescent", "Young Adult", "Middle-Aged", "Senior"))

#getting rid of extra columns and labeling for graph
rappk_ages4 <- rappk_ages3[,c(3,6)]
rappk_ages4 <- mutate(rappk_ages4, Key = c("Adolescent: Under 18", "Young Adult: 18 to 30", "Middle-Aged: 30 to 65", "Senior: 65 and Over"))
colnames(rappk_ages4) <- c("Percent of Population","Rounded", "Key")
rappk_ages4$Key <- factor(rappk_ages4$Key, levels = c("Adolescent: Under 18", "Young Adult: 18 to 30", "Middle-Aged: 30 to 65", "Senior: 65 and Over"))

write.csv(rappk_ages4, file = "data/TableB01001FiveYearEstimates/rappkAgeGroups.csv")

#----------------------------
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

write.csv(va_ages6, file = "data/TableB01001FiveYearEstimates/vaAgeGroups.csv")

#age dependecy files ----------------------------------------------------------
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

write.csv(county_dep, file = "data/TableB01001FiveYearEstimates/countyAgeDependency.csv")

#--------------------------------
sub_tbl <- function(varcode, year){
  data.frame(get_acs(geography = "county subdivision", state = 51,
                     county =  157,
                     table = varcode,
                     year = year))}

sub_data1 <- sub_tbl("S0101", 2019)


#Getting the three dependencies (age, old-age, child)
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

write.csv(sub_dep, file = "data/TableB01001FiveYearEstimates/subdivisionAgeDependency.csv")

#--------------------------------------
va_table <- function(varcode, year){
  data.frame(get_acs(geography = "state", state = 51,
                     table = varcode,
                     year = year))}


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
county_dep<- read.csv("data/TableB01001FiveYearEstimates/countyAgeDependency.csv")
county_dep2 <- data.frame(county_dep[13:15,3])
colnames(county_dep2) <- "ratio"

#labeling
va_rappk_dep <- rbind(va_dep2, county_dep2)
va_rappk_dep <- mutate(va_rappk_dep, Location = c("Virginia", "Virginia", "Virginia", "Rappahannock",
                                                  "Rappahannock", "Rappahannock"))
va_rappk_dep <- mutate(va_rappk_dep, stat = c("Age Dependency", "Old-Age Dependency", "Child Dependency", 
                                              "Age Dependency", "Old-Age Dependency", "Child Dependency"))
colnames(va_rappk_dep) <- c("Dependency Ratio", "Location", "Key")

write.csv(va_rappk_dep, file = "data/TableB01001FiveYearEstimates/rappkAgeDependency.csv")

#median age files --------------------------------------------------------------
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

write.csv(counties_median_age, file = "data/TableB01001FiveYearEstimates/countyMedianAge.csv")

#-----------------------------------
sub_tbl <- function(varcode, year){
  data.frame(get_acs(geography = "county subdivision", state = 51,
                     county =  157,
                     table = varcode,
                     year = year))}

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

write.csv(sub_median_age, file = "data/TableB01001FiveYearEstimates/subdivisionMedianAge.csv")

#Household size-------------------------------------------------------------
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

write.csv(size, file = "data/TableS2501FiveYearEstimates/householdSize.csv")

#owner and renter------------------------------------------------------------
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

write.csv(own, file = "data/TableS2501FiveYearEstimates/ownerOccupied.csv")
write.csv(rent, file = "data/TableS2501FiveYearEstimates/renterOccupied.csv")

#Vehicle per Household ---------------------------------------------------------
rapp_table <- function(varcode, year){
  get_acs(geography = "county",
          state = 51,
          county = 157,
          table = varcode,
          year = year)
}
no_veh <- c(rapp_table("S2504_C02", 2019)[27,4])
one_veh <- c(rapp_table("S2504_C02", 2019)[28,4])
two_veh <- c(rapp_table("S2504_C02", 2019)[29,4])
three_veh <- c(rapp_table("S2504_C02", 2019)[30,4])
rappk_veh <- data.frame(rbind(no_veh,one_veh,two_veh,three_veh))
rappk_veh <- mutate(rappk_veh, type = c("None", "One", "Two", "Three or more"))

write.csv(rappk_veh, file ="data/TableS2501FiveYearEstimates/vehiclesHousehold.csv")

#-------------------------------------
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

write.csv(county_veh2, file ="data/TableS2501FiveYearEstimates/vehiclesHouseholdCounty.csv")

#internet X income -------------------------------------------------------------
count_tbl <- function(varcode, year){
  data.frame(get_acs(geography = "county", state = 51,
                     county =  157,
                     table = varcode,
                     year = year))}

#county w/o internet subscription (3 income ranges)
rappk_data1 <- c(count_tbl("S2801_C02", 2019)[23,4])
rappk_data2 <- c(count_tbl("S2801_C02", 2019)[27,4])
rappk_data3 <- c(count_tbl("S2801_C02", 2019)[31,4])
#putting them in he same data frame
rappk_data <- data.frame(cbind(rappk_data1, rappk_data2, rappk_data3))
# putting estimate in the same column
rappk_data2 <- data.frame(pct = c(rappk_data[,1], rappk_data[,2], rappk_data[,3]))
#getting the percent that have internet
rappk_data2 <- mutate(rappk_data2, pct2 = 100-pct)
#Putting it estimates in same column
rappk_data3 <- data.frame(pct = c(rappk_data2[,1], rappk_data2[,2]))
rappk_data3 <- mutate(rappk_data3, grp = c("< $20,000", "$20,000-$74,999", "> $75,000",
                                           "< $20,000", "$20,000-$74,999", "> $75,000"))
rappk_data3 <- mutate(rappk_data3, int = c("No Internet", "No Internet", "No Internet",
                                           "Internet", "Internet", "Internet"))
colnames(rappk_data3) <- c("Percentage", "Income Range", "Int")
#putting income range in order
rappk_data3[,2] <- factor(rappk_data3[,2], levels = c("< $20,000", "$20,000-$74,999", "> $75,000"))

write.csv(rappk_data3, file = "data/TableS2801FiveYearEstimates/internetIncome.csv")

#computer access----------------------------------------------------------------
sub_tbl <- function(varcode, year){
  data.frame(get_acs(geography = "county subdivision", state = 51,
                     county =  157,
                     table = varcode,
                     year = year))}

sub_info <- sub_tbl("S2801", 2019)
#using only the computer and no computer estimates from all subdivisions
sub_comp<- sub_info %>%
  group_by(NAME) %>%
  slice(c(33,42)) 

#labeling
sub_comp <- mutate(sub_comp, key = c("Computer", "No Computer"))
sub_comp2 <- sub_comp %>% ungroup()
sub_comp2 <- mutate(sub_comp2, District = c("Hampton", "Hampton", "Jackson", "Jackson",
                                            "Piedmont", "Piedmont", "Stonewall-Hawthorne",
                                            "Stonewall-Hawthorne", "Wakefield", "Wakefield"))
colnames(sub_comp2) <- c("id", "NAME", "variable", "Percentage", "moe", "key", "District")
#putting the districts in the order I want to present (should appear alphabetical on graph)
sub_comp2$District <- factor( sub_comp2$District, levels = c("Wakefield", "Stonewall-Hawthorne","Piedmont", "Jackson", "Hampton"))

write.csv(sub_comp2, file = "data/TableS2801FiveYearEstimates/districtComputers.csv")

#internet subscription----------------------------------------------------------
sub_int<- sub_info %>%
  group_by(NAME) %>%
  slice(c(43,50)) 

#labeling
sub_int <- mutate(sub_int, key = c("Internet", "No Internet"))
sub_int2 <- sub_int %>% ungroup()
sub_int2 <- mutate(sub_int2, District = c("Hampton", "Hampton", "Jackson", "Jackson",
                                          "Piedmont", "Piedmont", "Stonewall-Hawthorne",
                                          "Stonewall-Hawthorne", "Wakefield", "Wakefield"))
colnames(sub_int2) <- c("id", "NAME", "variable", "Percentage", "moe", "key", "District")

#putting the districts in the order I want to present (should appear alphabetical on graph)
sub_int2$District <- factor(sub_int2$District, levels = c("Wakefield", "Stonewall-Hawthorne","Piedmont", "Jackson", "Hampton"))

write.csv(sub_int2, file = "data/TableS2801FiveYearEstimates/districtInternet.csv")