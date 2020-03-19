library(tidyverse)
library(readr)

cutoff <- Sys.Date()

d <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") 

read_corona <- function(file, variable){
  f <- file
  d <- read_csv(f) %>%
     pivot_longer(cols = matches("1|2|3|4|5|6|7|8|9"), names_to = "date", values_to = variable) %>%
     rename(country_region=`Country/Region`, lat=Lat, lon=Long, province_state=`Province/State`) %>%
     mutate(date=as.Date(gsub("X", "", date), "%m/%d/%y"), 
            province_state=ifelse(province_state=="", "NA", province_state))
  return(d)
}

confirmed <- read_corona("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv", "confirmed_cases")
deaths <- read_corona("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv", "deaths")
recovered <- read_corona("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv", "recovered")

corona <- 
  left_join(confirmed, deaths, by=c("country_region", "province_state", "date", "lat", "lon")) %>%
  left_join(recovered, by=c("country_region", "province_state", "date", "lat", "lon")) %>%
  filter(date<=cutoff) %>%
  replace(is.na(.), 0) %>%
  arrange(country_region, province_state) %>%
  group_by(country_region, province_state) %>%
  mutate(new_confirmed_cases=confirmed_cases-lag(confirmed_cases), 
         new_deaths=deaths-lag(deaths),
         new_recoveries=recovered-lag(recovered)) %>%
  ungroup()

ts <- 
  group_by(corona,  country_region, date) %>% 
  summarise_at(vars(new_confirmed_cases, new_deaths, new_recoveries), funs(sum)) %>% 
  mutate(new_case_growth_rate=new_confirmed_cases/lag(new_confirmed_cases)-1) %>%
  filter(country_region=="US" & date >= as.Date("2020-03-01"))

ggplot(ts, aes(x=date, y=new_confirmed_cases)) + geom_line() + scale_x_date(date_breaks = "1 day") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = scales::comma) +
  xlab("Date") + ylab("New Confirmed Cases")

ggplot(ts, aes(x=date, y=new_case_growth_rate)) + geom_line() + scale_x_date(date_breaks = "1 day") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Date") + ylab("New Confirmed Case Growth Rate")

