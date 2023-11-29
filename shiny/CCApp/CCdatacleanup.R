###Data cleanup for Confluence Crew data
# Tal Atkins

library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(tools)
library(readr)
library(anytime)
library(here)
library(data.table)

install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")

here()

CCrawdata <- read.csv(here("data", "PCD_Confluence_Crew_Data_Reporting_Form_0.csv"))
CCcleandata <- CCrawdata[,c(4, 9, 27, 30, 36, 38:40, 54:56, 81, 133:134)]
CCcleandata$"Site.Name" <- recode_factor(CCcleandata$"Site.Name", " UFC40.72" = "UFC40.72")

colnames(CCcleandata)[colnames(CCcleandata) == "Site.Name"] <- "Station"
colnames(CCcleandata)[colnames(CCcleandata) == "Date.Time.Sampled"] <- "Datetime"
colnames(CCcleandata)[colnames(CCcleandata) == "Air.Temperature.Average...C."] <- "Air Temperature Average (C)"
colnames(CCcleandata)[colnames(CCcleandata) == "Water.Temperature.Average...C."] <- "Water Temperature Average (C)"
colnames(CCcleandata)[colnames(CCcleandata) == "Transparency..NTUs."] <- "Transparency (NTUs)"
colnames(CCcleandata)[colnames(CCcleandata) == "Dissolved.Oxygen..DO...ppm.or.mg.L."] <- "Dissolved Oxygen (ppm or mg/L)"
colnames(CCcleandata)[colnames(CCcleandata) == "Phosphate..ppm.or.mg.L."] <- "Phosphate (ppm or mg/L)"
colnames(CCcleandata)[colnames(CCcleandata) == "pH..Standard.Unit."] <- "pH"
colnames(CCcleandata)[colnames(CCcleandata) == "Total.Organisms.Found"] <- "Total Organisms Found"
colnames(CCcleandata)[colnames(CCcleandata) == "Total.Taxa..species..Found"] <- "Total Taxa (Species) Found"
colnames(CCcleandata)[colnames(CCcleandata) == "Water.Quality"] <- "Water Quality"
colnames(CCcleandata)[colnames(CCcleandata) == "Stream.Flow..ft.3.sec."] <- "Streamflow (ft^3/sec)"
colnames(CCcleandata)[colnames(CCcleandata) == "Nitrate.Scale.Value"] <- "Nitrate Scale Value"
colnames(CCcleandata)[colnames(CCcleandata) == "Nitrate.Final.Value..ppm.or.mg.L."] <- "Nitrate Final Value (ppm or mg/L)"

CCcleandata <- melt(setDT(CCcleandata), id.vars = c("Station", "Datetime"), variable.names = "Parameter")
colnames(CCcleandata)[colnames(CCcleandata) == "variable"] <- "Parameter"
colnames(CCcleandata)[colnames(CCcleandata) == "value"] <- "Value"

write.csv(CCcleandata, (here("data", "Confluence_Crew_Clean_Data.csv")), row.names = FALSE)
write.csv(CCcleandata, (here("shiny", "CCApp", "data", "Confluence_Crew_Clean_Data.csv")), row.names = FALSE)
