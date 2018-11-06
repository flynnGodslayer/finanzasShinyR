library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(dplyr)
library(DBI)
library(RSQLite)


ui <- dashboardPage(
        dashboardHeader(),
        dashboardSidebar(),
        dashboardBody(
          fileInput("file", "Adjunta Archivo", width = "300px"),
          mainPanel(
            tabPanel("Tabla de datos", DTOutput("file")),
            conditionalPanel(
              condition = "output.file",
              actionButton("save", "Guardar CSV"),
              selectInput("categoria", label = "Categoria:"  ,choice = c("Comida", "transporte"))
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
      categoria <- vector(mode = 'character', length = 41)
      categoria <- input$categoria
      archivo <- data.frame(archivo, categoria)
      ar <- select(archivo, -credito, -saldo, -moneda)
      datatable(ar, selection ="none", editable = TRUE,
                options = list(scrollY = '400px', scrollX = TRUE, paging = FALSE, 
                               searching = TRUE))
     
    })
     
  }

shinyApp(ui, server)
