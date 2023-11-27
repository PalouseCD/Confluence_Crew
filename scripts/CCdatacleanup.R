###Data cleanup for Confluence Crew data
# Tal Atkins & Ryan Boylan updated 7/27/2023

#  To run this code you have to either 
# 1) be logged into the remote server and ArcPro should be connected to your (or Ryan's) ArcGIS online account or 
# 2) running ArcPro and R through your personal computer and connected to an ArcOnline account that has access to the CC data  

library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(here)
library(arcgisbinding) # if this isn't currently loaded as a package on R your can use the code below to install it. 
                      #install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")

#checking the projects working directory with the here package
here() 

# Checking ArcPro is loaded on the machine.  If it is not this code won't work
arc.check_product()

# Checking that you are logged into an Arc Online account.  If you are not and don't have access the rest of the code won't work
arc.check_portal()

#reading in the data from Arc Online into a arc.feature
d <-  arc.open("https://services2.arcgis.com/foElntyAQLIyOURr/arcgis/rest/services/service_40ea60c1ac0e47bd9c2608d34c3283f4/FeatureServer/0")

#creating a data frame from the data assigned to d with the arc.selection function
CCrawdata <- arc.select(d)

#selecting the columns that will be used for plotting later on
CCdata <- CCrawdata[,c(4, 9, 27, 30, 36, 38:40, 54:56, 81, 133:134)]

# Changes the column names with rename, gets ride of a station name error with mutate/recode, and converts the datat from wide to long

clean <- CCdata %>% rename(
                "Station" = "sitename",
                "Datetime" = "datetime_sampled",
                "Air Temperature Average (C)" = "air_temperature_average_c",
                "Water Temperature Average (C)" = "water_temperature_average_c",
                "Transparency (cm)" = "transparency_cm",
                "Dissolved Oxygen (mg/L)" = "dissolved_oxygen_do_ppm_or_mgl",
                "Phosphate (mg/L)" = "phosphate_ppm_or_mgl",
                "pH" = "ph_standard_unit",
                "Total Organisms Found" = "total_group_scores",
                "Total Taxa (Species) Found"  = "total_taxa",
                "Macro Water Quality Score"  = "final_score",
                "Streamflow (ft^3/sec)" = "stream_flow_ft3sec",
                "Nitrate Scale Value" = "nitrate",
                "Nitrate Final Value (mg/L)" = "nitrate_final_value" ) %>%
  mutate(Station = recode(Station, 
                              " UFC40.72" = "UFC40.72")) %>%
  pivot_longer(cols = !Station & !Datetime,
               names_to = "Parameter",
               values_to = "Value" )

here()
#Writes
write_csv(clean, (here("data", "Confluence_Crew_Clean_Data.csv")))

#------------------------------------------------------------------------------#

###below is for quick plotting of the data if you interested

#loading in the data
look <- read_csv(here("data", "Confluence_Crew_Clean_Data.csv"))

# Box plots of all parameters by time period
for (var in unique(look$Parameter)) { 
  print( ggplot(look[look$Parameter==var,], 
                aes(Datetime, Value,color=Station)) + 
           geom_boxplot() + 
           facet_grid(~Parameter) )
}  

# points a lines of all parameters
for (var in unique(look$Parameter)) { 
  print( ggplot(look[look$Parameter==var,], 
                aes(Datetime, Value,color=Station)) + 
           geom_line() + 
           geom_point()+
           facet_grid(~Parameter) )
}
