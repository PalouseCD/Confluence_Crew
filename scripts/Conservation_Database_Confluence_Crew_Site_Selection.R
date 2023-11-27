### Sorting through and exporting RCPP data from the PCD Data Conservation 
### Datebase

# Created April 2023 by RDB

library(dplyr)
library(plotly)
library(ggplot2)
library(lubridate)
library(here)
library(tidyr)
library(readr)
library(janitor)
library(curl)

source("R:/_04_Project_Data/R/PCD_Functions/Smartsheet to Csv.R")


data<- get_smartsheet(4701405874284420,"GxrvfnkIi07vvAAk775hUigIcFeQC7dxUzyj7")
  
data <- data %>% 
  clean_names() 

data <- data %>%
  select(cooperator_name,
         funding_source,
         project_category,
         email,
         phone,
         address,
         latitude_1,
         longitude_1,
         latitude_2,
         longitude_2,
         latitude_3,
         longitude_3,
         start_date,
         completion_date,
         project_year,
         huc_12_name,
         practice_type_1_nrcs_code_and_description)

names(data)
unique(data$project_category)

d<- data[,c(1:5)]
head(data$cooperator_name)

rip<- data %>% 
  filter(project_category == "Riparian Restoration") %>% 
  drop_na(latitude_1)

write_csv(rip,here("data","potential confluence crew sites.csv"))
