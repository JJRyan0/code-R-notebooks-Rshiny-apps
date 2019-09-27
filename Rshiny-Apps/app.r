
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
#

library(shiny)

# Define UI for application that draws a scatterplot
ui <- fluidPage(
   
   # Application title
   titlePanel("Scatterplot of Employee Job Satifaction Vs. Job Involvement"),
      mainPanel(
        plotOutput("scatterPlot")
      )
   )

# Define server logic required to draw a scatterplot of Employee Job Satisfaction Vs. Job Involvement
server <- function(input, output)
  output$scatterPlot <- renderPlot({
    
    library(ggplot2)
    df <- (Attr_test)
    #Scatterplot plot
    qplot(JobInvolvement, JobSatisfaction, data=df, shape= Department, color= Department, facets= Gender ~ JobRole ,size=I(2), main= "Scatterplot of Employee Job Satifaction Vs. Job Involvement", 
          xlab = "Job Involvement", ylab = "Job Satisfaction") + theme(axis.title = element_text(face="bold.italic", size= "12", color="black"),
                                                                       axis.text.x = element_text(colour = "grey20", size= "8", angle=45, hjust=0, vjust=1, face = "plain"),
                                                                       axis.text.y = element_text(colour = "grey20", size="8", angle=0, hjust = 0, vjust = 1, face = "plain"))
    

  })

# Run the application 
shinyApp(ui = ui, server = server)