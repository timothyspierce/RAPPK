##### Age Time Series data and Charts #####
rappage_timeseries <- readRDS("shiny_app/data/rapp_age_time_series.Rda")


ggplot(rappage_timeseries, aes(x = year, y = percent, group = ages, color = ages)) +
  geom_line(aes(size = estimate)) +
  labs(title = "Age of Population from 2010 to 2019", color = "Age Categories") +
  xlab("Years") +
  ylab("Percent of the population") +
  scale_color_discrete(
    labels = c("under18" = "Under 18", 
               "age18_29" = "18 to 29", 
               "age30_64" = "30 to 64", 
               "age65_older" = "65 and Older")) 

agetimeseries <- readRDS("shiny_app/data/district_age_time_series.Rda")

ggplot(agetimeseries, aes(x = year, y = percent, color = ages, group = ages)) +
  geom_line(aes(size = estimate)) +
  labs(title = "Age of Population from 2010 to 2019", color = "Age Categories") +
  xlab("Years") +
  ylab("Percent of the population") +
  scale_color_discrete(
    labels = c("under18" = "Under 18", 
               "age18_29" = "18 to 29", 
               "age30_64" = "30 to 64", 
               "age65_older" = "65 and Older")) +
  facet_wrap(~NAME)



#### Housing Data ####

housing2010_2019 <- readRDS("shiny_app/data/housing_over_time.Rda")
ggplot(housing2010_2019, aes(x = year, y = pop_per_home, group = homevalues, color = homevalues)) +
  geom_line(aes(size = estimated_total))

#heres another housing map
  housing2010_2019_by_district <- readRDS("shiny_app/data/housing2010_2019_by_district.Rds")
  
  ggplot(housing2010_2019_by_district, aes(x = year, y = percent_of_houses, group = homevalues, color = homevalues)) +
  geom_line(aes(size = estimated_total)) +
  ylab("Percentage of Homes") +
  facet_wrap(~NAME) +
  labs(size = "Number of Homes") +
  ggtitle("Housing Prices (In US Dollars) From 2010 to 2019") +
  scale_color_viridis_d(name = "Home Value Brackets") +
  plot_theme

#### Income Over Time

income2010_2019 <- readRDS("shiny_app/data/income2010_2019.Rda")

ggplot(income2010_2019, aes(x = incomebracket, y = percent, fill = NAME.x, group = NAME.x)) +
  geom_col(position = "dodge") +
  facet_wrap(~year) +
  coord_flip()

#### Population Over Time #### 
population2010_2019 <- readRDS("shiny_app/data/population2010_2019.Rds")


ggplot(population2010_2019 %>% filter(NAME != "Rappahannock"), aes(x = year, y = estimate, group = NAME, color = NAME)) +
  geom_line(aes(size = "Percent of Population" <- percent)) +
  ggtitle(label = "Estimated Total Population 2010-2019")

ggplot(population2010_2019 %>% filter(NAME != "Rappahannock"), aes(x = year, y = percent, group = NAME, color = NAME)) +
  geom_line(aes(size = "Percent of Population" <- estimate)) +
  ggtitle(label = "Estimated Total Population 2010-2019")


#Race Map
race_district <- readRDS("shiny_app/data/race_district.Rds")
View(race_district)
race_district %>% filter(Race != "White") %>% ggplot(aes(x = Year, y = Percent, fill = NAME, group = NAME)) + geom_col() + theme(plot.title = element_text(hjust = 0.5),
                                                                                                                                 axis.text=element_text(size=12),
                                                                                                                                 legend.text = element_text(size=12),
                                                                                                                                 axis.title.x=element_text(size =13),
                                                                                                                                 axis.title.y=element_text(size =13),
                                                                                                                                 panel.background = element_blank())  + scale_fill_viridis_d()


#Education Levels by District

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
