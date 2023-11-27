# Ryan's confluence Crew data analysis

library(readr)
library(dplyr)
library(tidyr)
library(plyr)
library(plotly)
library(ggplot2)
library(lubridate)
library(here)
library(janitor)

data <- read_csv(here("data","Confluence_Crew_Clean_Data.csv"))
data <- clean_names(data)
data$datetime <- mdy_hms(data$datetime)
data$date <- date(data$datetime)
data$month <- month(data$datetime)
data$year <- year(data$datetime)
data$monyr <- myd(paste(data$month,data$year,"1"))


sum <- data %>% 
  drop_na() %>% 
  group_by(station, parameter) %>% 
  summarise(Min = min(value),
            Mean = mean(value),
            Standard_Dev = sd(value),
            Max = max(value),
            Samples = n())
write_csv(sum,here("outputs","23_data_summary_Site.csv"))

sumpar <- data %>% 
  drop_na() %>% 
  group_by(parameter) %>% 
  summarise(Min = min(value),
            Mean = mean(value),
            Medin = median(value),
            Standard_Dev = sd(value),
            Max = max(value),
            Samples = n())
sum(sumpar$Samples)
write_csv(sumpar,here("outputs","23_data_summary_parameter.csv"))


avg <- data %>% 
  drop_na() %>% 
  group_by(monyr,parameter) %>% 
  mutate(mean_value = mean(value)) %>% 
  ungroup() %>% 
  clean_names()

mon <- avg %>% 
  group_by(monyr,parameter) %>% 
  summarise(mean = mean(value))

n <- mon %>% 
  filter(parameter=="Nitrate Final Value (ppm or mg/L)")

CCplot <- avg %>% 
  filter(parameter == "Nitrate Final Value (ppm or mg/L)") %>% 
  ggplot(aes(x = date, y = value))+
    geom_line(aes(color = station))+
    geom_point(aes(color = station))+
    geom_line(aes(x = monyr, y = mean_value),color = "black",show.legend = TRUE)+
    theme(axis.text.x = element_text(angle = 40))+
    theme_minimal()+
  labs(x = "Date", y = "Nitrate Concentration (mg/L)")+
  ggtitle("Monthly Nitrate Concentrations for all Confluence Crew Sites")


ggplotly(CCplot)
  
plots <- function(para,ylab,title){
  ccplot <- avg %>% 
    filter(parameter == para) %>% 
    ggplot(aes(x = date, y = value))+
    geom_line(aes(color = station))+
    geom_point(aes(color = station))+
    geom_line(aes(x = monyr, y = mean_value),color = "black",show.legend = TRUE)+
    theme(axis.text.x = element_text(angle = 40))+
    theme_minimal()+
    labs(x = "Date", y = ylab)+
    ggtitle(title)
  ggplotly(ccplot)
  
}

plots("Water Temperature Average (C)",
      "Water Temperature (C)", 
      "Water Temperature for all Confluence Crew Sites")
plots("Nitrate Final Value (ppm or mg/L)",
      "Nitrate Concentration (mg/L)", 
      "Nitrate Concentrations for all Confluence Crew Sites")
plots("Dissolved Oxygen (ppm or mg/L)",
      "Dissolved Oxygen mg/L",
      "Dissolved Oxygen Levels for all Confluence Crew Sites")
plots("pH",
      "pH", 
      "pH levels for all Confluence Crew Sites")
plots("Transparency (NTUs)", 
      "Transparency (NTU)", 
      "Transparency Measurments for All Confluence Crew Sites" )
