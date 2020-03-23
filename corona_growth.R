library(tidyverse)
library(readr)

setwd("/Users/kim.larsen/Documents/Code/Corona/")

source("functions.R")

us <- charts(country_region_filter = "US", min_date = "2020-03-01")
us[[1]]
us[[2]]
us[[3]]
us[[4]]
us[[5]]
us[[6]]


ca <- charts(province_state_filter = "California", min_date = "2020-03-01")
us[[1]]
us[[2]]
us[[3]]
us[[4]]
us[[5]]
us[[6]]
