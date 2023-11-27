### Confluence Crew Averages Comparison ###


#Loading packages
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)
library(plotly)
library(ggplot2)
library(here)
library(janitor)
library(purrr)



#Reading in data (CCCD is "Confluence Crew Clean Data")
CCCD <- read.csv("R:\\_04_Project_Data\\R\\Confluence_Crew\\data\\Confluence_Crew_Clean_Data.csv")



#Saving data to the outputs folder
write.csv(CCCD,here("R:\\_04_Project_Data\\R\\Confluence_Crew\\outputs","CCCD_TEST.csv"))



#Relevant Abbreviations:
## CCCD = Confluence Crew Clean Data, 
## CCCDF = Confluence Crew Clean Data Filtered, 
## avg = CCCDF with a column representing the average monthly values across all sites for each wq metric
### All graphs are titled with abbreviations of the parameter they represent. Names of parameters are found in the "Parameter" column of CCCDF or avg. 
### The letter "g" on the end of an object stands for "graph"



#Filtering null values (CCCDF is "Confluence Crew Clean Data Filtered")
CCCDF <- filter(CCCD, !is.na(Value))



#Cleaning up objects
rm(CCCD)


#Indicating Datetime column contains dates
CCCDF$Datetime <- mdy_hms(CCCDF$Datetime) 



#Adding column "month" to CCCDF
CCCDF$month <- month(ymd_hms(CCCDF$Datetime))



#Creating "mean_value" column for all wq metrics in a new table
avg <- CCCDF %>% 
  group_by(Parameter,month) %>% 
  mutate(mean_value = mean(Value)) %>% 
  ungroup()
    

  
#Plotting NFVg all sites (Nitrate Final Value graph)  
NFVg <- avg %>%
  filter(Parameter == "Nitrate Final Value (ppm or mg/L)") %>%
  ggplot()+
  geom_line(aes(x = Datetime,y = Value, color = 'Individual Site'))+
  geom_point(aes(x = Datetime,y = Value, color = 'Individual Site'))+
  geom_line(aes(x = Datetime, y = mean_value, color = 'Average All Sites'))+
  geom_point(aes(x = Datetime, y = mean_value, color = 'Average All Sites'))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 40))+
  labs(y= "Nitrate Final Value in ppm or mg/L", x = "Month")+
  ggtitle("Monthly Nitrate Final Values at Individual Confluence Crew 
       Sites Compared with Monthly Averages Across All Sites")+
  labs(color = "Legend")+
  facet_wrap(~Station, nrow = 2, ncol = 5)
  


#Adjusting plot title positioning
  ggplotly(NFVg) %>% 
    layout(title = list(x = .45, y = .95, xref = "plot"),
           margin = list(l = 50, t = 100))
  

    
#Experimenting with functions
  
  

###### Attempt 3 FIX AVERAGE ALL SITES LINE TO BE BLACK 
plot <- function(parameter,title){
  a <- avg %>% 
    filter(Parameter == parameter) %>%
    ggplot() +
    geom_line(aes(Datetime, Value, color = Station))+
    geom_point(aes(Datetime, Value, color = Station))+
    geom_line(aes(Datetime, mean_value, color = "Average All Sites"))+
    geom_point(aes(Datetime, mean_value, color = "Average All Sites"))+
    scale_color_manual(values = c('#333333', '#999966', '#66CC00', '#66FF00', '#33FF99', '#33FFFF', '#00CCCC', '#9966FF', '#FF99FF', '#FF0066', '#FF9933'))+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 40))+
    ggtitle(title)+
    labs(color = "Legend")
 
  ggplotly(a)
}

 plot("Air Temperature Average (C)", "Monthly Air Temperature Average Values Found at Individual Confluence Crew 
       Sites Compared with Monthly Averages Across All Sites")

d <- plot("Dissolved Oxygen (ppm or mg/L)", "Monthly Dissolved Oxygen Average Values Found at Individual Confluence
          Crew Sites Compared with Monthly Averages Across All Sites" )

