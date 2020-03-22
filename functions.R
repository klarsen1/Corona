

charts <- function(country_region_filter, min_date){
  
     df <- readRDS("corona.rda")
     
     if (is.null(country_region_filter)==FALSE){
       dd <- group_by(df, country_region, date) %>% 
             summarise_at(vars(new_confirmed_cases, new_deaths, new_recoveries, confirmed_cases, cumulative_cases), funs(sum)) %>% 
             mutate(new_case_growth_rate=new_confirmed_cases/lag(new_confirmed_cases)-1, 
                   new_deaths_growth_rate=new_deaths/lag(new_deaths)-1) %>%
             filter(date >= as.Date(min_date) & country_region==country_region_filter)
     } else{
       dd <- group_by(df, date) %>% 
             summarise_at(vars(new_confirmed_cases, new_deaths, new_recoveries, confirmed_cases, cumulative_cases), funs(sum)) %>% 
             mutate(new_case_growth_rate=new_confirmed_cases/lag(new_confirmed_cases)-1, 
                    new_deaths_growth_rate=new_deaths/lag(new_deaths)-1) %>%
            filter(date >= as.Date(min_date))
     }

     g1<- ggplot(dd, aes(x=date, y=new_confirmed_cases)) + 
      geom_bar(stat="identity") + 
      scale_x_date(date_breaks = "1 day") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_y_continuous(labels = scales::comma) +
      xlab("Date") + ylab("New Confirmed Cases")

    g2 <- ggplot(dd, aes(x=date, y=new_case_growth_rate)) + 
      geom_bar(stat="identity") + 
      scale_x_date(date_breaks = "1 day") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_y_continuous(labels = scales::percent) +
      xlab("Date") + ylab("New Confirmed Case Growth Rate")

    g3 <- ggplot(dd, aes(x=date, y=new_deaths_growth_rate)) + 
      geom_bar(stat="identity") + 
      scale_x_date(date_breaks = "1 day") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_y_continuous(labels = scales::percent) +
      xlab("Date") + ylab("New Confirmed Deaths Growth Rate")

   g4 <- ggplot(dd, aes(x=date, y=new_deaths)) + 
     geom_bar(stat="identity") + 
      scale_x_date(date_breaks = "1 day") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_y_continuous(labels = scales::comma) +
      xlab("Date") + ylab("New Confirmed Deaths")

  return(list(g1, g2, g3, g4, dd))

}

