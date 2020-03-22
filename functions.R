

charts <- function(country_region_filter=NULL, province_state_filter=NULL, min_date){
  
     df <- readRDS("corona.rda")
     
     if (is.null(province_state_filter)==FALSE){
       df <- filter(df, province_state==province_state_filter)
     } else{
       df$province_state <- "ALL"
     }
     
     if (is.null(country_region_filter)==FALSE){
       df <- filter(df, country_region==country_region_filter)
     } else{
       df$country_region <- "ALL"
     }
     
      dd <- group_by(df, country_region, province_state, date) %>% 
             summarise_at(vars(new_confirmed_cases, new_deaths, new_recoveries, confirmed_cases, cumulative_cases, cumulative_deaths), funs(sum)) %>% 
             group_by(country_region, province_state) %>%
             mutate(new_case_growth_rate=new_confirmed_cases/lag(new_confirmed_cases)-1, 
                    new_deaths_growth_rate=new_deaths/lag(new_deaths)-1) %>%
        filter(date >= as.Date(min_date))

     g1<- ggplot(dd, aes(x=date, y=new_confirmed_cases)) + 
      geom_bar(stat="identity") + 
      scale_x_date(date_breaks = "1 day") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_y_continuous(labels = scales::comma) +
      xlab("Date") + ylab("New Confirmed Cases") + 
      geom_text(aes(label=prettyNum(new_confirmed_cases, big.mark=",")), position=position_dodge(width=0.9), vjust=-0.25, size=3) 

    g2 <- ggplot(dd, aes(x=date, y=new_case_growth_rate)) + 
      geom_bar(stat="identity") + 
      scale_x_date(date_breaks = "1 day") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_y_continuous(labels = scales::percent) +
      xlab("Date") + ylab("New Confirmed Case Growth Rate") + 
      geom_text(aes(label=paste0(round(new_case_growth_rate*100,1), "%")), position=position_dodge(width=0.9), vjust=-0.25, size=3)

    g3 <- ggplot(dd, aes(x=date, y=new_deaths_growth_rate)) + 
      geom_bar(stat="identity") + 
      scale_x_date(date_breaks = "1 day") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_y_continuous(labels = scales::percent) +
      xlab("Date") + ylab("New Confirmed Deaths Growth Rate") +
      geom_text(aes(label=paste0(round(new_deaths_growth_rate*100,1), "%")), position=position_dodge(width=0.9), vjust=-0.25, size=3)
    
   g4 <- ggplot(dd, aes(x=date, y=new_deaths)) + 
      geom_bar(stat="identity") + 
      scale_x_date(date_breaks = "1 day") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_y_continuous(labels = scales::comma) +
      xlab("Date") + ylab("New Confirmed Deaths") + 
      geom_text(aes(label=prettyNum(new_deaths, big.mark=",")), position=position_dodge(width=0.9), vjust=-0.25, size=3) 

   g5<- ggplot(dd, aes(x=date, y=cumulative_cases)) + 
     geom_bar(stat="identity") + 
     geom_smooth() +
     scale_x_date(date_breaks = "1 day") + 
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     scale_y_continuous(labels = scales::comma) +
     xlab("Date") + ylab("Cumulative Confirmed Cases") + 
     geom_text(aes(label=prettyNum(cumulative_cases, big.mark=",")), position=position_dodge(width=0.9), vjust=-0.25, size=3) 

   g6<- ggplot(dd, aes(x=date, y=cumulative_deaths)) + 
     geom_bar(stat="identity") + 
     geom_smooth() +
     scale_x_date(date_breaks = "1 day") + 
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     scale_y_continuous(labels = scales::comma) +
     xlab("Date") + ylab("Cumulative Deaths") + 
     geom_text(aes(label=prettyNum(cumulative_deaths, big.mark=",")), position=position_dodge(width=0.9), vjust=-0.25, size=3) 
   
  return(list(g1, g2, g3, g4, g5, g6))

}

