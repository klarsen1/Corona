library(tidyverse)
library(readr)

setwd("/Users/kim.larsen/Documents/Code/Corona")

read_corona <- function(file, variable){
  f <- file
  d <- read_csv(f) %>%
    pivot_longer(cols = matches("1|2|3|4|5|6|7|8|9"), names_to = "date", values_to = variable) %>%
    rename(country_region=`Country/Region`, lat=Lat, lon=Long, province_state=`Province/State`) %>%
    mutate(date=as.Date(date, "%m/%d/%y"), 
           province_state=ifelse(province_state=="", "NA", province_state))
  return(d)
}

confirmed <- read_corona("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv", "confirmed_cases")
deaths <- read_corona("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv", "deaths")
recovered <- read_corona("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv", "recovered")
populations <- read_csv("populations.csv") %>%
  mutate(d=max((country_region=="US")*population), 
         population_scalar=d/population)

corona <- 
  left_join(confirmed, deaths, by=c("country_region", "province_state", "date", "lat", "lon")) %>%
  left_join(recovered, by=c("country_region", "province_state", "date", "lat", "lon")) %>%
  replace(is.na(.), 0) %>%
  left_join(populations, by="country_region") %>%
  arrange(country_region, province_state, date) %>%
  group_by(country_region, province_state) %>%
  mutate(row=row_number(), 
         new_confirmed_cases=ifelse(row>1, confirmed_cases-lag(confirmed_cases), confirmed_cases),  
         new_deaths=ifelse(row>1, deaths-lag(deaths), deaths),
         new_recoveries=ifelse(row>1, recovered-lag(recovered), recovered), 
         cumulative_cases=cumsum(new_confirmed_cases), 
         cumulative_deaths=cumsum(new_deaths), 
         new_confirmed_cases_scaled=new_confirmed_cases*population_scalar,
         new_deaths_scaled=new_deaths*population_scalar,
         cumulative_cases_scaled=cumulative_cases*population_scalar, 
         cumulative_deaths_scaled=cumulative_deaths*population_scalar) %>%
  ungroup()

saveRDS(corona,"corona.rda")

corona_country_cohorts <- 
  filter(corona, is.na(population_scalar)==FALSE) %>%
  group_by(country_region, date) %>%
  summarise_at(
    vars(new_confirmed_cases, new_deaths, new_recoveries, confirmed_cases, cumulative_cases, cumulative_deaths, new_confirmed_cases_scaled, new_deaths_scaled, cumulative_cases_scaled, cumulative_deaths_scaled), 
    funs(sum)) %>%
  group_by(country_region) %>%
  mutate(cohort_index_first_death=dense_rank(cumulative_deaths)-1,
         cohort_index_first_case=dense_rank(cumulative_cases)-1)
  
saveRDS(corona_country_cohorts,"corona_country_cohorts.rda")  


  

