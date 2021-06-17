library(gridExtra)
library(sf)
library(dplyr)
library(ggplot2)


#Introduce Mapping Function
rapp_map <- function(varcode){
  get_acs(geography = "county subdivision",
          state = 51,
          county = c(157, 163, 063 , 121),
          variables = varcode,
          year = 2019,
          geometry = TRUE) %>% 
    ggplot() + geom_sf(aes(fill = estimate))}

rapp_table <- function(varcode, year){
  data.frame(get_acs(geography = "county",
                     state = 51,
                     county = 157,
                     table = varcode,
                     output = "wide",
                     year = year))}

ability_table <- rapp_table("B18101", 2019)
ability_table <- mutate(ability_table, under18 = (B18101_004E + B18101_007E + B18101_023E + B18101_026E)/(B18101_003E + B18101_006E + B18101_022E + B18101_025E))
ability_table <- mutate(ability_table, adultunder35 = (B18101_010E + B18101_029E)/(B18101_028E + B18101_009E))
ability_table <- mutate(ability_table, adultover35 = (B18101_013E + B18101_032E)/(B18101_031E + B18101_012E))
ability_table <- mutate(ability_table, retirees = (B18101_016E + B18101_019E + B18101_035E + B18101_038E)/(B18101_015E + B18101_018E + B18101_034E + B18101_037E))


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


rapp_table <- function(varcode, year){
  get_acs(geography = "county subdivision",
          state = 51,
          county = 157,
          table = varcode,
          year = year,
          output = "wide",
          geometry = TRUE)}

rapp_table("B15003", 2019) -> edu_table

edu_table <- mutate(edu_table, phd_pct = B15003_025E/B15003_001E)
edu_table <- mutate(edu_table, ms_pct = B15003_023E/B15003_001E)
edu_table <- mutate(edu_table, bs_pct = B15003_022E/B15003_001E)
edu_table <- mutate(edu_table, as_pct = B15003_021E/B15003_001E)
edu_table <- mutate(edu_table, sc_pct = B15003_019E/B15003_001E)
edu_table <- mutate(edu_table, hs_pct = B15003_017E/B15003_001E)
edu_table$phd_pct %>% ggplot() + geom_sf()
View(edu_table)
edu_table <- edu_table %>% data.frame()

chart1 <- barplot(edu_table$phd_pct~ edu_table$area_names, main = "Percent PhD", xlab = "County", ylab = "Percent of Population")
barplot(edu_table$ms_pct ~ edu_table$area_names, main = "Percent Masters", xlab = "County", ylab = "Percent of Population")
barplot(edu_table$bs_pct ~ edu_table$area_names, main = "Percent Bachelors", xlab = "County", ylab = "Percent of Population") 
barplot(edu_table$as_pct ~ edu_table$area_names, main = "Percent Associates", xlab = "County", ylab = "Percent of Population") 
barplot(edu_table$sc_pct ~ edu_table$area_names, main = "Percent Some College", xlab = "County", ylab = "Percent of Population") 
barplot(edu_table$hs_pct ~ edu_table$area_names, main = "Percent High School", xlab = "County", ylab = "Percent of Population")
    
View(edu_table)
area_names <- c("Wakefield", "Piedmont", "Hampton", "Jackson", "Stonewall-Hawthorne")
edu_table <- cbind(area_names, edu_table)
View(edu_table)

hspct <- edu_table %>% ggplot() + geom_sf(aes(fill = hs_pct)) + labs(title = "High School")
scpct <- edu_table %>% ggplot() + geom_sf(aes(fill = sc_pct)) + labs(title = "Some College")
aspct <- edu_table %>% ggplot() + geom_sf(aes(fill = as_pct)) + labs(title = "Associates")
bspct <- bspct <- edu_table %>% ggplot() + geom_sf(aes(fill = bs_pct)) + labs(title = "Bachelors")
mspct <- edu_table %>% ggplot() + geom_sf(aes(fill = ms_pct)) + labs(title = "Masters")
phdpct <-edu_table %>% ggplot() + geom_sf(aes(fill = phd_pct)) + labs(title = "PhD")
allEdpct <- grid.arrange(phdpct, mspct, bspct, aspct, scpct, hspct, ncol = 3, nrow =2, top = "Education Percentages by County Subdivision")
