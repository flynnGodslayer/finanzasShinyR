library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(dplyr)

ui <- dashboardPage(
        dashboardHeader(),
        dashboardSidebar(),
        dashboardBody(
        fileInput("file", "Adjunta Archivo", width = "400px"),
        mainPanel(
          tabPanel("Tabla de datos", DTOutput("file")))
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
      datatable(archivo, selection = "none", editable = TRUE)
    })
  }

shinyApp(ui, server)
