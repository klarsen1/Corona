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
         population_scalar=d/population) %>% 
  select(-d)

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
         cumulative_deaths=cumsum(new_deaths)) %>%
  ungroup()

saveRDS(corona,"corona.rda")

corona_country_cohorts <- 
  group_by(country_region, date) %>%
  summarise_at(
    vars(new_confirmed_cases, new_deaths, new_recoveries, confirmed_cases, cumulative_cases, cumulative_deaths), 
    funs(sum)) %>%
  group_by(country_region) %>%
  mutate(cohort_index_first_death=dense_rank(cumulative_deaths)-1,
         cohort_index_first_case=dense_rank(cumulative_cases)-1)
  
saveRDS(corona_country_cohorts,"corona_country_cohorts.rda")  

check <- function(){
  
   max_date <- max(corona$date)
   cat("Max Date = ", as.character(max_date))
   cat("\n")
   cat("Last date used to check data = ", as.character(max_date-1))
   cat("\n")
   
   c <- filter(corona, date<=max_date-1)
  
   total <- sum(filter(c, date==max_date-1)$confirmed_cases)
   ex_china <- sum(filter(c, date==max_date-1 & country_region!="China")$confirmed_cases)
   us <- sum(filter(c, date==max_date-1 & country_region=="US")$confirmed_cases)
   cat("Total Worldwide Cases as of", as.character(max_date-1), "= ", format(total, big.mark = ",")) 
   cat("\n")
   cat("Total Worldwide Cases (excl China) as of", as.character(max_date-1), "= ", format(ex_china, big.mark = ",")) 
   cat("\n")
   cat("Total US Cases as of", as.character(max_date-1), "= ", format(us, big.mark = ",")) 
   cat("\n")
   cat("\n")
   
   new <- sum(filter(c, date==max_date-1)$new_confirmed_cases) 
   new_ex_china <- sum(filter(c, date==max_date-1 & country_region!="China")$new_confirmed_cases)
   new_us <- sum(filter(c, date==max_date-1 & country_region=="US")$new_confirmed_cases)
   cat("New Worldwide Cases on", as.character(max_date-1), "= ", format(new, big.mark = ",")) 
   cat("\n")
   cat("New Worldwide Cases (excl China) on", as.character(max_date-1), "= ", format(new_ex_china, big.mark = ",")) 
   cat("\n")
   cat("New US Cases on", as.character(max_date-1), "= ", format(new_us, big.mark = ",")) 
   cat("\n")
   cat("Check new case calculations: ", sum(c$new_confirmed_cases)==total)
   cat("\n")
   cat("\n")
   
   total <- sum(filter(c, date==max_date-1)$deaths)
   ex_china <- sum(filter(c, date==max_date-1 & country_region!="China")$deaths)
   us <- sum(filter(c, date==max_date-1 & country_region=="US")$deaths)
   cat("Total Worldwide Deaths as of", as.character(max_date-1), "= ", format(total, big.mark = ",")) 
   cat("\n")
   cat("Total Worldwide Deaths (excl China) as of", as.character(max_date-1), "= ", format(ex_china, big.mark = ",")) 
   cat("\n")
   cat("Total US Deaths as of", as.character(max_date-1), "= ", format(us, big.mark = ",")) 
   cat("\n")
   cat("\n")
   
   new <- sum(filter(c, date==max_date-1)$new_deaths) 
   new_ex_china <- sum(filter(c, date==max_date-1 & country_region!="China")$new_deaths)
   new_us <- sum(filter(c, date==max_date-1 & country_region=="US")$new_deaths)
   cat("New Worldwide Deaths on", as.character(max_date-1), "= ", format(new, big.mark = ",")) 
   cat("\n")
   cat("New Worldwide Cases (excl China) on", as.character(max_date-1), "= ", format(new_ex_china, big.mark = ",")) 
   cat("\n")
   cat("New US Deaths on", as.character(max_date-1), "= ", format(new_us, big.mark = ",")) 
   cat("\n")
   cat("Check new death calculations: ", sum(c$new_deaths)==total)
}

check()

