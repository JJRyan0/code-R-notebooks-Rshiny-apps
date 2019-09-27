# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(leaflet)
library(dplyr)


#Global variable - filter to subset data
exposure_subset <- dplyr:: filter(fm_plt_summary %>%
                                    select(event_id, mean, exposure_value, occ_year, Latitude, Longitude ))
# Define UI for application that draws a map
shinyApp(
  ui <- fluidPage(
    sliderInput(inputId = 'Occurance',
                label = "Ocurrance Year",
                min = -50, max = 10000, value = 0, step = 500),
    tags$div(title = "sampled loss",
             selectInput(inputId = "exposure_value",
                         label = "Exposure Value",
                         choices = sort(unique(exposure_subset$exposure_value)))),
    leafletOutput("MapPlot1")
  ),
  
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    
    #create leaflet map with markers
    output$MapPlot1 <- renderLeaflet({
      leaflet() %>%
        addMarkers(lng = exposure_subset$Longitude, lat = exposure_subset$Latitude, popup="exposure")
      addProviderTiles('Stamen.watertone') %>%
        setView(lng = exposure_subset$Longitude, lat = exposure_subset$Latitude, zoom=5) 
    })
    
    #storing the click
    observe({
      Occurance <- input$Ocurrance
      exposure_value <- input$exposure_value
      
      places <- exposure_subset %>%
        filter(findInterval(exposure_subset$occ_year, c(Occurance - 200, Occurance + 200)) == 1 & exposure_subset$exposure_value)
      leafletProxy("MapPlot1") %>% clearMarkers() %>%
        addCircleMarkers(lng = places$Longitude, lat = places$Latitude,
                         opacity = places$mean)
    })
  },
  options = list(height = 600)
)
