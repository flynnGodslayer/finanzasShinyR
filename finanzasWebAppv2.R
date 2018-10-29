library(shiny)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
        dashboardHeader(),
        dashboardSidebar(),
        dashboardBody(
        fileInput("file","Adjunta Archivo",width = "400px"),
        mainPanel(
          tabPanel("Tabla de datos", DTOutput("file")))
        )
)

server <- function(input, output) { 
    #output$csv <- renderTable({
     # csv <- input$file
    #  if(is.null(csv)){return()} 
    #  read.csv(csv$datapath,header = TRUE,skip = 1,colClasses = c(NA,NA,NA,NA,'NULL','NULL'))
    #})
    #archivo <- read.csv("TDCR.csv",header = TRUE,skip = 1,colClasses = c(NA,NA,NA,NA,'NULL','NULL'))
    output$file <- renderDT({
      req(input$file)
      read.csv(input$file$datapath,header = TRUE,skip = 1,colClasses = c(NA,NA,NA,NA,'NULL','NULL'))
      #archivo,  options = list(lengthChange = FALSE)
    })
  }

shinyApp(ui, server)
