library(gridExtra)
library(sf)
library(dplyr)
library(ggplot2)
library(tidycensus)
library(fpp2)


#Introduce 5Function
rapp_map <- function(varcode){
  get_acs(geography = "county subdivision",
          state = 51,
          county = c(157, 163, 063 , 121),
          variables = varcode,
          year = 2019,
          geometry = TRUE) %>% 
    ggplot() + geom_sf(aes(fill = estimate))}

rapp_table <- function(varcode, year){
                     get_acs(geography = "county subdivision",
                     state = 51,
                     county = 157,
                     table = varcode,
                     output = "wide",
                     year = year,
                     geometry = TRUE)}

rapp_var <- function(varcode, year){
  get_acs(geography = "county subdivision",
          state = 51,
          county = 157,
          variables = varcode,
          year = year,
          survey = "acs5",
          output = "wide")}

ability_table <- rapp_table("B18101", 2019)
ability_table <- mutate(ability_table, under18 = (B18101_004E + B18101_007E + B18101_023E + B18101_026E)/(B18101_003E + B18101_006E + B18101_022E + B18101_025E))
ability_table <- mutate(ability_table, adultunder35 = (B18101_010E + B18101_029E)/(B18101_028E + B18101_009E))
ability_table <- mutate(ability_table, adultover35 = (B18101_013E + B18101_032E)/(B18101_031E + B18101_012E))
ability_table <- mutate(ability_table, retirees = (B18101_016E + B18101_019E + B18101_035E + B18101_038E)/(B18101_015E + B18101_018E + B18101_034E + B18101_037E))
View(ability_table)

U18 <- ability_table %>% ggplot() + geom_sf(aes(fill = under18)) + labs(title = "Under 18")
U35 <- ability_table %>% ggplot() + geom_sf(aes(fill = adultunder35)) + labs(title = "Young Adult")
U65 <- ability_table %>% ggplot() + geom_sf(aes(fill = adultover35)) + labs(title = "Older Adult")
O65 <- ability_table %>% ggplot() + geom_sf(aes(fill = retirees)) + labs(title = "Retirees")
disability_map <- grid.arrange(U18, U35, U65, O65, ncol =2, top = "Disability by Age Group and County Subdivision")
disability_map

disability_values <- c(ability_table[,81], ability_table[,82], ability_table[,83], ability_table[,84])
disability_names <- c("Children", "Adults Under 35", "Adults Over 35", "Retirees")
disability <- rbind(disability_names, disability_values)
barplot(disability_values ~ disability_names, data = disability, main = "Disabled Population", ylab = "Percent of Age Group", xlab = "Age Group")

#disability %>% t() %>%  data.frame()  %>% ggplot() + geom_bar(aes(y=disability_names))
#disability %>% t() %>% data.frame()  %>% ggplot(aes(y=disability_values)) + geom_bar()
#View(disability)

# #ability_geo_table <- data.frame(get_acs(geography = "county subdivision",
#                                         state = 51,
#                                         county = 157,
#                                         table = "B18101",
#                                         output = "wide",
#                                         year = 2019,
#                                         geometry = TRUE))

#Education by Magnitude 
phd <- rapp_map("B15003_025") + labs(title = "PhD")
masters <- rapp_map("B15003_023") + labs(title = "Masters")
bachelors <- rapp_map("B15003_022") + labs(title = "Bachelors")
associates <- rapp_map("B15003_021") + labs(title = "Associates")
somecollege <-rapp_map("B15003_019") + labs(title = "Some College")
diploma <- rapp_map("B15003_017") + labs(title = "HS Diploma")


highEd <- grid.arrange(phd, masters, bachelors, ncol = 3)
lowEd <- grid.arrange(diploma, somecollege, associates, ncol =3)
allEd <- grid.arrange(highEd, lowEd, nrow = 2, top = "Education")


# Importing and mutating table to include percentages of education
rapp_table("B15003", 2019) -> edu_table
edu_table <- mutate(edu_table, phd_pct = B15003_025E/B15003_001E)
edu_table <- mutate(edu_table, ms_pct = B15003_023E/B15003_001E)
edu_table <- mutate(edu_table, bs_pct = B15003_022E/B15003_001E)
edu_table <- mutate(edu_table, as_pct = B15003_021E/B15003_001E)
edu_table <- mutate(edu_table, sc_pct = B15003_019E/B15003_001E)
edu_table <- mutate(edu_table, hs_pct = B15003_017E/B15003_001E)
edu_table <- edu_table %>% data.frame()

#Add labels to the counties 
area_names <- c("Wakefield", "Piedmont", "Hampton", "Jackson", "Stonewall-Hawthorne")
edu_table <- cbind(area_names, edu_table)

#Barplots of Edu percentages by subdivision
barplot(edu_table$phd_pct~ edu_table$area_names, main = "Percent PhD", xlab = "County", ylab = "Percent of Population")
barplot(edu_table$ms_pct ~ edu_table$area_names, main = "Percent Masters", xlab = "County", ylab = "Percent of Population")
barplot(edu_table$bs_pct ~ edu_table$area_names, main = "Percent Bachelors", xlab = "County", ylab = "Percent of Population") 
barplot(edu_table$as_pct ~ edu_table$area_names, main = "Percent Associates", xlab = "County", ylab = "Percent of Population") 
barplot(edu_table$sc_pct ~ edu_table$area_names, main = "Percent Some College", xlab = "County", ylab = "Percent of Population") 
barplot(edu_table$hs_pct ~ edu_table$area_names, main = "Percent High School", xlab = "County", ylab = "Percent of Population")
    

#Make some maps
hspct <- edu_table %>% ggplot() + geom_sf(aes(fill = hs_pct)) + labs(title = "High School") + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank()) 
scpct <- edu_table %>% ggplot() + geom_sf(aes(fill = sc_pct)) + labs(title = "Some College") + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank())
aspct <- edu_table %>% ggplot() + geom_sf(aes(fill = as_pct)) + labs(title = "Associates") + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank())
bspct <- edu_table %>% ggplot() + geom_sf(aes(fill = bs_pct)) + labs(title = "Bachelors") + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank())
mspct <- edu_table %>% ggplot() + geom_sf(aes(fill = ms_pct)) + labs(title = "Masters") + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank())
phdpct <-edu_table %>% ggplot() + geom_sf(aes(fill = phd_pct)) + labs(title = "PhD") + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank())
allEdpct <- grid.arrange(phdpct, mspct, bspct, aspct, scpct, hspct, ncol = 3, nrow =2, top = "Education Percentages by County Subdivision")


#Create vector for years
year <- c(2019, 2019, 2019, 2019, 2019,
         2018, 2018, 2018, 2018, 2018,
         2017, 2017, 2017, 2017, 2017,
         2016, 2016, 2016, 2016, 2016,
         2015, 2015, 2015, 2015, 2015,
         2014, 2014, 2014, 2014, 2014,
         2013, 2013, 2013, 2013, 2013,
         2012, 2012, 2012, 2012, 2012)

#Line graph for PhD percentages
phds <- rbind(
rapp_var(c("B15003_001", "B15003_025"), 2019),
rapp_var(c("B15003_001", "B15003_025"), 2018),
rapp_var(c("B15003_001", "B15003_025"), 2017),
rapp_var(c("B15003_001", "B15003_025"), 2016),
rapp_var(c("B15003_001", "B15003_025"), 2015),
rapp_var(c("B15003_001", "B15003_025"), 2014),
rapp_var(c("B15003_001", "B15003_025"), 2013),
rapp_var(c("B15003_001", "B15003_025"), 2012))
#Add year, percent, and edu classification
phds <- cbind(year, phds)
phds <- mutate(phds, pct = B15003_025E/B15003_001E)
phds <- mutate(phds, edu =rep(c("phd"),40))

#Line graph for MS/MA percentages

masters <- rbind(
  rapp_var(c("B15003_001", "B15003_023"), 2019),
  rapp_var(c("B15003_001", "B15003_023"), 2018),
  rapp_var(c("B15003_001", "B15003_023"), 2017),
  rapp_var(c("B15003_001", "B15003_023"), 2016),
  rapp_var(c("B15003_001", "B15003_023"), 2015),
  rapp_var(c("B15003_001", "B15003_023"), 2014),
  rapp_var(c("B15003_001", "B15003_023"), 2013),
  rapp_var(c("B15003_001", "B15003_023"), 2012))
#Add year, percent, and edu classification
masters <- cbind(year, masters)
masters <- mutate(masters, pct = B15003_023E/B15003_001E)
masters <- mutate(masters, edu =rep(c("masters"),40))


#Line graph for BS/MA
bachelors <- rbind(
  rapp_var(c("B15003_001", "B15003_022"), 2019),
  rapp_var(c("B15003_001", "B15003_022"), 2018),
  rapp_var(c("B15003_001", "B15003_022"), 2017),
  rapp_var(c("B15003_001", "B15003_022"), 2016),
  rapp_var(c("B15003_001", "B15003_022"), 2015),
  rapp_var(c("B15003_001", "B15003_022"), 2014),
  rapp_var(c("B15003_001", "B15003_022"), 2013),
  rapp_var(c("B15003_001", "B15003_022"), 2012))
#Add year, percent, and edu classification
bachelors <- cbind(year, bachelors)
bachelors <- mutate(bachelors, pct = B15003_022E/B15003_001E)
bachelors <- mutate(bachelors, edu =rep(c("bachelors"),40))


#Line Graph for AA/AS
associates <- rbind(
  rapp_var(c("B15003_001", "B15003_021"), 2019),
  rapp_var(c("B15003_001", "B15003_021"), 2018),
  rapp_var(c("B15003_001", "B15003_021"), 2017),
  rapp_var(c("B15003_001", "B15003_021"), 2016),
  rapp_var(c("B15003_001", "B15003_021"), 2015),
  rapp_var(c("B15003_001", "B15003_021"), 2014),
  rapp_var(c("B15003_001", "B15003_021"), 2013),
  rapp_var(c("B15003_001", "B15003_021"), 2012))
#Add year, percent, and edu classification
associates <- cbind(year, associates)
associates <- mutate(associates, pct = B15003_021E/B15003_001E)
associates <- mutate(associates, edu =rep(c("associates"),40))


#Some college but less than one year table 
somecollege <- rbind(
  rapp_var(c("B15003_001", "B15003_019"), 2019),
  rapp_var(c("B15003_001", "B15003_019"), 2018),
  rapp_var(c("B15003_001", "B15003_019"), 2017),
  rapp_var(c("B15003_001", "B15003_019"), 2016),
  rapp_var(c("B15003_001", "B15003_019"), 2015),
  rapp_var(c("B15003_001", "B15003_019"), 2014),
  rapp_var(c("B15003_001", "B15003_019"), 2013),
  rapp_var(c("B15003_001", "B15003_019"), 2012))
#Add year, percent, and edu classification
somecollege <- cbind(year, somecollege)
somecollege <- mutate(somecollege, pct = B15003_019E/B15003_001E) 
somecollege <- mutate(somecollege, edu =rep(c("somecollege"),40))

#high school diploma table
highschool <- rbind(
  rapp_var(c("B15003_001", "B15003_017"), 2019),
  rapp_var(c("B15003_001", "B15003_017"), 2018),
  rapp_var(c("B15003_001", "B15003_017"), 2017),
  rapp_var(c("B15003_001", "B15003_017"), 2016),
  rapp_var(c("B15003_001", "B15003_017"), 2015),
  rapp_var(c("B15003_001", "B15003_017"), 2014),
  rapp_var(c("B15003_001", "B15003_017"), 2013),
  rapp_var(c("B15003_001", "B15003_017"), 2012))
#Add year, percent, and edu classification
highschool <- cbind(year, highschool)
highschool <- mutate(highschool, pct = B15003_017E/B15003_001E)
highschool <- mutate(highschool, edu =rep(c("highschool"),40))

#Make a table that puts all percentages in one column and classifies them by edu type
#This lets us use ggplot(aes(group = xxx)) to make stacked visuals 
edutable <- rbind(phds[,c(1:3,8:9)], 
                  masters[,c(1:3,8:9)], 
                  bachelors[,c(1:3,8:9)], 
                  associates[,c(1:3,8:9)], 
                  somecollege[,c(1:3,8:9)], 
                  highschool[,c(1:3,8:9)])

#Line Graphs by Education Type 
edutable %>% 
  filter(edu == "highschool") %>% 
  ggplot(aes(x = year, y = pct, group = NAME, color = NAME)) + 
  geom_line(position = "identity", show.legend = TRUE) +
  labs(title = "High School Education")

edutable %>% 
 filter(edu == "somecollege") %>% 
  ggplot(aes(x = year, y = pct, group = NAME, color = NAME)) + 
  geom_line(position = "identity", show.legend = TRUE) +
  labs(title= "Some College")

edutable %>% 
  filter(edu == "associates") %>% 
  ggplot(aes(x = year, y = pct, group = NAME, color = NAME)) + 
  geom_line(position = "identity", show.legend = TRUE) +
  labs(title = "Associates Degree")

edutable %>% 
  filter(edu == "bachelors") %>% 
  ggplot(aes(x = year, y = pct, group = NAME, color = NAME)) + 
  geom_line(position = "identity", show.legend = TRUE) +
  labs(title = "Bachelors Degree")

edutable %>% 
  filter(edu == "masters") %>% 
  ggplot(aes(x = year, y = pct, group = NAME, color = NAME)) + 
  geom_line(position = "identity", show.legend = TRUE) +
  labs(title = "Masters Degree")

edutable %>% 
  filter(edu == "phd") %>% 
  ggplot(aes(x = year, y = pct, group = NAME, color = NAME)) + 
  geom_line(position = "identity", show.legend = TRUE) +
  labs(title = "Doctoral Degree")

#Line Graphs for Education in Piedmont 
  edutable %>% 
  filter(GEOID == 5115795063) %>% 
  ggplot() +
  geom_line(aes(x = year, y = pct, group = edu, color = edu))+
  labs(title = "Education Attainment in Piedmont District",
       subtitle = "ACS5 Survey Data: 2012-2019",
       y = "Percent of Population",
       x = "Year")

#Wakefield 
  edutable %>% 
  filter(GEOID == 5115796199) %>% 
  ggplot() +
  geom_line(aes(x = year, y = pct, group = edu, color = edu))+
  labs(title = "Education Attainment in Wakefield District",
       subtitle = "ACS5 Survey Data: 2012-2019",
       y = "Percent of Population",
       x = "Year")

#try this in Jackson
edutable %>% 
  filter(GEOID == 5115794007) %>% 
  ggplot() +
  geom_line(aes(x = year, y = pct, group = edu, color = edu))+
  labs(title = "Education Attainment in Jackson District",
       subtitle = "ACS5 Survey Data: 2012-2019",
       y = "Percent of Population",
       x = "Year")

#Stonewall-Hawthorne
edutable %>% 
  filter(GEOID == 5115795967) %>% 
  ggplot() +
  geom_line(aes(x = year, y = pct, group = edu, color = edu))+
  labs(title = "Education Attainment in Stonewall-Hawthorne District",
       subtitle = "ACS5 Survey Data: 2012-2019",
       y = "Percent of Population",
       x = "Year")

#Hampton
edutable %>% 
  filter(GEOID == 5115793823) %>% 
  ggplot() +
  geom_line(aes(x = year, y = pct, group = edu, color = edu))+
  labs(title = "Education Attainment in Hampton District",
       subtitle = "ACS5 Survey Data: 2012-2019",
       y = "Percent of Population",
       x = "Year")


            