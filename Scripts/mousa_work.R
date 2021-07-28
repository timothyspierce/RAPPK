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

#### Income Over Time

income2010_2019 <- readRDS("shiny_app/data/income2010_2019.Rda")

ggplot(income2010_2019, aes(x = incomebracket, y = percent, fill = NAME.x, group = NAME.x)) +
  geom_col(position = "dodge") +
  facet_wrap(~year) +
  coord_flip()
