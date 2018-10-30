library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(dplyr)

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

    output$file <- renderDT({
      req(input$file)
      archivo <-read.csv(input$file$datapath,header = TRUE,skip = 1,
                                    colClasses = c(NA,NA,NA,-NA,'NULL','NULL'))
      transform(archivo,Crédito,Crédito = Crédito * -1) 
      archivo <- unite(archivo,Débito,c(Débito,Crédito),sep = "",remove = TRUE)
      datatable(archivo,selection = "none",editable = TRUE)
      
    })
  }

shinyApp(ui, server)
