library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(dplyr)
#setwd("/home/curso/finanzasShinyR")

ui <- dashboardPage(
        dashboardHeader(),
        dashboardSidebar(),
        dashboardBody(
          fileInput("file", "Adjunta Archivo", width = "400px"),
          mainPanel(
            tabPanel("Tabla de datos", DTOutput("file")),
            conditionalPanel(
              condition = "output.file",
              actionButton("escribir", "Guardar CSV")
            )
          )
        )
)

server <- function(input, output) { 

    output$file <- renderDT({
      req(input$file)
      
      archivo <-read.csv(input$file$datapath, header = TRUE, skip = 1,
                                    colClasses = c("character", "character", "character", "character",
                                                   "character", "character"))
      colnames(archivo) <- c("fecha", "descripcion", "debito", "credito", "saldo", "moneda")
      archivo$debito <- as.numeric(gsub(",", "", archivo$debito ))
      archivo$credito <- as.numeric(gsub(",", "", archivo$credito))
      archivo$debito[!is.na(archivo$credito)] <- -archivo$credito[!is.na(archivo$credito)]
      archivo <- select(archivo, -credito, -saldo, -moneda)
      datatable(archivo, selection = "none", editable = TRUE, options = list(scrollY = '300px', scrollX = TRUE, paging = FALSE, searching = FALSE))
    })
    
    observeEvent(
      input$escribir, {
        write.csv2(archivo, file = "memoria.csv")
      }
    ) 
  }

shinyApp(ui, server)
