library(gridExtra)
library(sf)
library(dplyr)
library(ggplot2)
library(tidycensus)
library(fpp2)
library(stringr)


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
                     year = year)}

rapp_var <- function(varcode, year){
  get_acs(geography = "county subdivision",
          state = 51,
          county = 157,
          variables = varcode,
          year = year,
          survey = "acs5",
          output = "wide")}

rapp_var_county <- function(varcode, year){
  get_acs(geography = "county",
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
rapp_table <- function(varcode, year){
  get_acs(geography = "county subdivision",
          state = 51,
          county = 157,
          table = varcode,
          output = "wide",
          year = year)}

get_edu <- function(year){
rapp_table("B15003", 2019) -> edu_table
edu_table <- mutate(edu_table, 'Above Bachelors' = (B15003_025E + B15003_024E + B15003_023E)/B15003_001E)
edu_table <- mutate(edu_table, 'Bachelors Degree' = B15003_022E/B15003_001E)
edu_table <- mutate(edu_table, 'Some College' = (B15003_021E + B15003_020E + B15003_019E)/B15003_001E)
edu_table <- mutate(edu_table, 'HS Diploma or GED' = (B15003_017E + B15003_018E)/B15003_001E)
edu_table <- mutate(edu_table, 'Less Than High School' = (B15003_016E+B15003_015E+B15003_014E+B15003_013E+B15003_012E + B15003_011E+ B15003_010E+B15003_009E+B15003_008E+B15003_007E+B15003_006E+B15003_005E+B15003_004E+B15003_003E+B15003_002E)/B15003_001E)
}

get_edu(2019)[c(2, 53:57)] %>% 
  pivot_longer(cols = c("Above Bachelors", "Bachelors Degree", "Some College", "HS Diploma or GED", "Less Than High School"),
               names_to = "EduLevel",
               values_to = "Percent") -> edu2019
edu2019 <- edu2019 %>% mutate(NAME = str_sub(NAME, end = -41 ))
edu2019$EduLevel <- factor(edu2019$EduLevel, c("Above Bachelors", "Bachelors Degree", "Some College", "HS Diploma or GED", "Less Than High School"))
edu2019 <- edu2019 %>% mutate(Percent = Percent*100)
saveRDS(edu2019, "shiny_app/data/edu2019.Rds")
readRDS("shiny_app/data/edu2019.Rds")



ggplot(edu2019, aes(x = NAME, y = Percent, group = EduLevel, fill = EduLevel)) + 
  geom_col() + scale_fill_viridis_d() +
  ggtitle("Education Levels by District")  + xlab("District")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        axis.title.x=element_text(size =13),
        axis.title.y=element_text(size =13),
        panel.background = element_blank()) 




edu2019 <- edu2019 %>% mutate(NAME = str_sub(NAME, end = -32 ))
edu2019$EduLevel <- factor(edu2019$EduLevel, c("Above Bachelors", "Bachelors Degree", "Some College", "HS Diploma or GED", "Less Than High School"))
edu2019 <- edu2019 %>% mutate(Percent = Percent*100)
saveRDS(edu2019, "shiny_app/data/edu2019.Rds")
readRDS("shiny_app/data/edu2019.Rds")
ggplot(edu2019, aes(x = NAME, y = Percent, group = EduLevel, fill = EduLevel)) + 
  geom_col() + scale_fill_viridis_d() +
  ggtitle("Education Levels by District")  + xlab("District")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        axis.title.x=element_text(size =13),
        axis.title.y=element_text(size =13),
        panel.background = element_blank()) 


  
  
