###R Shiny App for Confluence Crew Data
# Tal Atkins


library(tidyr)
library(dplyr)
library(shiny)
library(rsconnect)
library(plotly)
library(shinyWidgets)
library(shinythemes)
library(lubridate)
library(anytime)
library(gt)
library(kableExtra)
library(here)
library(data.table)

here()

########

### CREATING DATAFRAME ###
#when running in RStudio use:
ccdata <- read.csv(here("data", "Confluence_Crew_Clean_Data.csv"))


#alphabetically organized sites
ccdata <- ccdata[order(ccdata$Parameter),]

#reorganizing dates 
ccdata$Datetime <- format(as.POSIXct(ccdata$Datetime,
                           format = '%m/%d/%Y %H:%M:%S'),
                format = '%m/%d/%Y')
ccdata <- ccdata[order(as.Date(ccdata$Datetime, format="%m/%d/%Y")),]
ccdata$Datetime <- mdy(ccdata$Datetime)

#alphabetically organizing station
ccdata <- ccdata[order(ccdata$Station),]

#renaming stations to make them more descriptive
ccdata <- mutate(ccdata, Station=recode(Station,
                                      "PC04.38" = "Paradise Creek (PC04.38)",
                                       "ROSC00.80" = "Rose Creek (ROSC00.80)",
                                       "SFPR12.50" ="South Fork Palouse River (SFPR12.50)",
                                       "SFPR22.37" = "South Fork Palouse River (SFPR22.37)",
                                       "SFPR23.38" = "South Fork Palouse River (SFPR23.38)",
                                       "SFPR23.89" = "South Fork Palouse River (SFPR23.89)",
                                       "SFPR24.27" = "South Fork Palouse River (SFPR24.27)",
                                       "UFC40.72" = "Union Flat Creek (UFC40.72)",
                                       "UFC42.03" = "Union Flat Creek (UFC42.03)",
                                       "UNTRIB-FMC03.33" = "Unnamed Tributary Four Mile Creek (UNTRIB-FMC03.33)",
                                       "UNTRIB-UFC0.12" = "Unnamed Tributary Union Flat Creek (UNTRIB-UFC0.12)"))

#ADDING TMDLS TO DATAFRAME
#TMDLs according to ECY
#DO_Min = 8.5
#pH_Min = 6.5
#pH_Max = 8.5
#Temp_Max = 17.5
#FC_Max = 100

#adding WA State water quality max and min values for DO, temp, and pH
ccdata$Criteriamax<- ifelse(ccdata$Parameter == "Dissolved Oxygen (ppm or mg/L)", 8.5,
                            ifelse(ccdata$Parameter == "Water Temperature Average (C)", 17.5,
                                   ifelse(ccdata$Parameter == "pH", 8.5, NA)))
ccdata$Criteriamin<- ifelse(ccdata$Parameter == "pH", 6.5, NA)


### RSCONNECT TO SERVER ###

rsconnect::setAccountInfo(name='pcdrm',
                          token= '22F8B9DB932C3F6582F2B3E5DDDE6A14',
                          secret= 'T0EH/V2HUtckmKizHDYSeLK4Q3pCxOyYZ0bY+Qzj')

## Define UI

#theme can be changed to make it more visually appealing
ui <- fluidPage(theme = shinytheme("cosmo"),
                
                #the following code chunk controls the appearance and functionality of the sidebar
                sidebarLayout(position = "left",
                              sidebarPanel(id = "sidebar", width = 2, height = "500px",
                                           h3(strong("Graphing Options"), align = 'center'),
                                           
                                           #objects are ordered sequentially from top down
                                           #example: put the line of code for bringing in site images here
                                           #if you want the image to be at the top of the sidebar
                                           
                                           #dropdown selection for monitoring sites
                                           selectInput("sta", 
                                                       label = "Select Desired Water Quality Monitoring Site:",
                                                       choices = c("Paradise Creek (PC04.38)",
                                                                   "Rose Creek (ROSC00.80)",
                                                                   "South Fork Palouse River (SFPR12.50)",
                                                                   "South Fork Palouse River (SFPR22.37)",
                                                                   "South Fork Palouse River (SFPR23.38)",
                                                                   "South Fork Palouse River (SFPR23.89)",
                                                                   "South Fork Palouse River (SFPR24.27)",
                                                                   "Union Flat Creek (UFC40.72)",
                                                                   "Union Flat Creek (UFC42.03)",
                                                                   "Unnamed Tributary Four Mile Creek (UNTRIB-FMC03.33)",
                                                                   "Unnamed Tributary Union Flat Creek (UNTRIB-UFC0.12)")),
                                           
                                           #dropdown selection for water quality parameters
                                           selectInput("para", 
                                                       label = "Select Desired Parameter:",
                                                       choices = c( "Air Temperature Average (C)",
                                                                    "Dissolved Oxygen (ppm or mg/L)",
                                                                    "Nitrate Final Value (ppm or mg/L)",
                                                                    "Nitrate Scale Value",
                                                                    "pH",
                                                                    "Streamflow (ft^3/sec)",
                                                                    "Total Organisms Found",
                                                                    "Total Taxa (Species) Found",
                                                                    "Transparency (NTUs)",
                                                                    "Water Quality",
                                                                    "Water Temperature Average (C)")),
                                           
                                           #Site image
                                           imageOutput("site_image"), 
                                           
                                           #button for markdown
                                           downloadButton("report", "Generate Report")
                                           
                              ),
                              
                              #chunk for controlling center panel (graph, table, param descriptions)
                              mainPanel(width =10,
                                        fluidRow(
                                          column(12,
                                                 align = "left",
                                                 h2("Water Quality Data for Confluence Crew Sites"))
                                        ),
                                        
                                        fluidRow(
                                          column(10,
                                                 align = "center",
                                                 plotlyOutput("line", width = "100%", height = "100%"
                                                 )),
                                          column(10,
                                                 align = "right",
                                                 textOutput("warnings1"),
                                                 tags$head(tags$style("#warnings1{color: red;
                                                                      font-size: 17px;
                                                                      font-style: bold;
                                                                      }")))
                                        ),
                                        
                                        fluidRow(
                                          column(3,
                                                 align = "right",
                                                 tableOutput("table"))),
                                        
                                        #inserts parameter descriptions  
                                        textOutput("par_desc")
                              
                              
                              )))



server = function(input, output){
  
  #This section handles the downloading of r markdowns for individual sites. 
  #This is currently not functioning correctly. 
  #Code was taken from here: https://resources.symbolix.com.au/2020/10/28/downloadable-reports-shiny/
  #it seems as if it doesn't understand where the markdown code is, so it just generates blank html reports
  
  output$report <- downloadHandler(
    filename = "Confluence Crew Report.docx ",
    content = function(file){
      tempReport <- file.path(tempdir(), here("RMD", "CCmarkdown.Rmd"))
      file.copy(here("RMD", "CCmarkdown.Rmd"), tempReport, overwrite = T)
      
      params <- list(n = input$sta)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
                        )
      
    }
  )
  
  
  #creating descriptions for each parameter
  DOdescription <- "This graph displays dissolved oxygen (DO), in milligrams per liter (mg/L) and in percentages. The red dotted line represents the lower threshold for DO (9.5 mg/L) set by the State of Washington (WAC 173-201A-200). DO is the amount of gaseous oxygen dissolved in the water, which benefits aquatic life."
  NitrateFinaldescription <- "This graph displays final nitrate measurements, measured in milligrams per liter (mg/L) or parts per million (ppm). Nitrate is a common form of nitrogen found in streams. Nitrogen is found naturally in the environment and is fairly abundant. However, excess nitrogen from sewage systems and fertilizer applications can overstimulate the growth of aquatic plants and algae, throwing aquatic ecosystems out of equilibrium."
  NitrateScaledescription <- "This graph displays nitrate scale values, measured in milligrams per liter (mg/L) or parts per million (ppm). Nitrate is a common form of nitrogen found in streams. Nitrogen is found naturally in the environment and is fairly abundant. However, excess nitrogen from sewage systems and fertilizer applications can overstimulate the growth of aquatic plants and algae, throwing aquatic ecosystems out of equilibrium."
  pHdescription <- "This graph displays pH levels. pH measurements indicate levels of acidity or alkalinity. The pH scale ranges from 0 to 14 and is an important indicator of health in aquatic ecosystems. Variations in pH levels allow for pollutants such as heavy metals and nutrients to be more easily dissolved in water. The red dotted lines on the graph indicate the minimum and maximum thresholds, 6.5 and 8.5 pH units respectively, for aquatic pH according to the Water Quality Standards for Surface Water of the State of Washington (WAC 173-201A-200)."
  Transparencydescription <- "This graph displays a measure of water clarity, measured in nephelometric turbidity units (NTU). Turbidity is measured by determining how much light can pass through the water. Water clarity directly affects ecological productivity and habitat quality in aquatic environments. Excessive turbidity can be caused by physical disruption of the stream channel or through sediment inputs as a result of runoff and upland erosion."
  
  
  ###PARAMETER DESCRIPTION
  #assigning descriptions to certain parameters
  output$par_desc <- renderText({
    if(input$para=="Dissolved Oxygen (ppm or mg/L)"){
       paste(DOdescription)
    } else {
      if (input$para=="Nitrate Final Value (ppm or mg/L)"){
        paste(NitrateFinaldescription)
      } else {
        if(input$para=="Nitrate Scale Value"){
          paste(NitrateScaledescription)
        } else {
          if(input$para=="pH"){
            paste(pHdescription)
          } else {
            if (input$para=="Transparency (NTUs)")
              paste(Transparencydescription)
          }
            }   
              }
                }
    }) 

  #This chunk controls bringing in site images to the app
  #If you want to add or change images to the app, 
  #add them to the www folder in R:\_04_Project_Data\R\Confluence_Crew\shiny\CCApp,
  #then name the image the same as the site name (see below)
 output$site_image<- renderImage({
    if(input$sta=="Paradise Creek (PC04.38)"){
      list(src="WWW/PC04.38.jpg", height="300px", width="200px", deleteFile=FALSE)
    } else {
      if (input$sta=="Rose Creek (ROSC00.80)"){
        list(src="WWW/ROSC00.80.JPG", height="300px", width="200px", deleteFile=FALSE)
      } else {
        if (input$sta=="South Fork Palouse River (SFPR12.50)"){
          list(src="WWW/SFPR12.50.jpg", height="300px", width="200px", deleteFile=FALSE)
        } else {
          if (input$sta=="South Fork Palouse River (SFPR22.37)"){
            list(src="WWW/SFPR22.37.JPG", height="300px", width="200px", deleteFile=FALSE)
          } else {
            if (input$sta=="South Fork Palouse River (SFPR23.38)"){
              list(src="WWW/SFPR23.38.JPG", height="300px", width="200px", deleteFile=FALSE)
            } else {
              if (input$sta=="South Fork Palouse River (SFPR23.89)"){
                list(src="WWW/SFPR23.89.jpg", height="300px", width="200px", deleteFile=FALSE)
              } else {
              if (input$sta=="South Fork Palouse River (SFPR24.27)"){
                list(src="WWW/SFPR24.27.jpg", height="300px", width="200px", deleteFile=FALSE)
              } else {
                if (input$sta=="Union Flat Creek (UFC40.72)"){
                  list(src="WWW/UFC40.72.jpg", height="300px", width="200px", deleteFile=FALSE)
                } else {
                  if (input$sta=="Union Flat Creek (UFC42.03)"){
                    list(src="WWW/UFC42.03.jpg", height="300px", width="200px", deleteFile=FALSE)
                  } else {
                    if (input$sta=="Unnamed Tributary Four Mile Creek (UNTRIB-FMC03.33)"){
                      list(src="WWW/UNTRIB-FMC03.33.JPG", height="300px", width="200px", deleteFile=FALSE)
                    } else {
                      if (input$sta=="Unnamed Tributary Union Flat Creek (UNTRIB-UFC0.12)"){
                        list(src="WWW/UNTRIB-UFC0.12.jpg", height="300px", width="200px", deleteFile=FALSE)
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
        }
    })
  
  ### SETTING UP DATAFRAMES FOR GRAPH AND SUMMARY TABLE ###
  
 #creates reactive dataframes based on what you input for site and parameter
  data <-reactive({
    new_data <-filter(ccdata,Station == input$sta & Parameter == input$para)
    return(new_data)
  }) 
  
  #For each parameter, min mean and max values are generated
  tdata<-reactive({
    ntdata<- data() %>%
      select('Value') %>%
      summarise(Min = min(Value, na.rm = TRUE), Mean=mean(Value, na.rm = TRUE),Max = max(Value, na.rm = TRUE))
    return(ntdata)
  })
  
  
  
  ### PLOTTING GRAPH ###
  # This displays the trace of parameter data and TMDLs if there is one for that parameter
  output$line <- renderPlotly({
    plot_ly(data(), x = ~Datetime)%>%
      add_trace(y = ~Value,
                type = 'scatter',
                mode = 'lines+markers',
                connectgaps = TRUE,
                name = input$sta)%>%
      add_lines(y = ~Criteriamax,
                line = list(color = 'red'),
                connectgaps = TRUE,
                name = "Water Quality Criteria Max")%>%
      add_lines(y = ~Criteriamin,
                line = list(color = 'red'),
                connectgaps = TRUE,
                name = "Water Quality Criteria Min")%>%
      layout(
        plot_bgcolor='#e5ecf6',
        xaxis = list(title ='Date'),
        yaxis = list(title = input$para),
        legend = list(orientation = 'h'))
    ##(x = 0.05, y = 0.95) 
  })
  
  ### OUTPUTTING TABLE ###
  # This outputs a table with min, max and mean
  output$table <- renderTable({
    tdata()},
    striped = TRUE, bordered = TRUE, width = '300', align = 'c')
  
  
}



shinyApp(ui = ui, server = server)



