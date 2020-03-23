library(tidyverse)
library(readr)

setwd("/Users/kim.larsen/Documents/Code/Corona/")

source("functions.R")


d <- readRDS("corona_country_cohorts.rda") %>%
  filter(!(country_region %in% c("China", "Singapore", "Iran", "United Kingdom", "Japan"))) %>%
  filter(cohort_index_first_death>0)

m1 <- ceiling(max(d$cumulative_deaths_scaled)/1000)*1000
b1 <- seq(from=0, to=m1, by=1000)
m2 <- ceiling(max(d$cumulative_cases_scaled)/10000)*10000
b2 <- seq(from=0, to=m2, by=5000)

gd <- ggplot(data=d, aes(x=cohort_index_first_death,  y=cumulative_deaths, colour=country_region)) +
  geom_point(aes(shape=country_region), size=2) + geom_line(size=1) + 
  scale_y_continuous(labels = scales::comma, breaks=b1) +
  ylab("Total Deaths") + 
  xlab("Days Since First Death") + 
  theme(legend.title = element_blank())

ggsave(file="/Users/kim.larsen/Documents/Code/Corona/charts/death_cohorts.png", gd, device = "png", dpi=72, width=9, height=6)


c <- readRDS("corona_country_cohorts.rda") %>%
  filter(!(country_region %in% c("China", "Singapore", "Iran", "United Kingdom", "Japan"))) %>%
  filter(cohort_index_first_case>0)

gc <- ggplot(data=c, aes(x=cohort_index_first_case,  y=cumulative_cases, colour=country_region)) +
  geom_point(aes(shape=country_region), size=2) + geom_line(size=1) +
  scale_y_continuous(labels = scales::comma,  breaks=b2) +
  ylab("Total Cases") + 
  xlab("Days Since First Case") + 
  theme(legend.title = element_blank())

ggsave(file="/Users/kim.larsen/Documents/Code/Corona/charts/cases_cohorts.png", gc, device = "png", dpi=72, width=9, height=6)
