library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(dplyr)
library(DBI)
library(RSQLite)
library(rhandsontable)


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    fileInput("ar", "Adjunta Archivo", width = "300px"),
    mainPanel(
      rHandsontableOutput("file")
    )
  )
)


server <- function(input, output) { 
  
  output$file <- renderRHandsontable({
    req(input$ar)
    archivo <-read.csv(input$ar$datapath, header = TRUE, skip = 1,
                       colClasses = c("character", "character", "character", "character",
                                      "character", "character"))
    colnames(archivo) <- c("fecha", "descripcion", "debito", "credito", "saldo", "moneda")
    archivo$debito <- as.numeric(gsub(",", "", archivo$debito ))
    archivo$credito <- as.numeric(gsub(",", "", archivo$credito))
    archivo$debito[!is.na(archivo$credito)] <- -archivo$credito[!is.na(archivo$credito)]
    categoria <- vector(mode = 'character', length = 41)
    archivo <- data.frame(archivo, categoria, {fecha2 = archivo$fecha})
    ar <- select(archivo, -credito, -saldo, -moneda)
    rhandsontable(ar, width = 600, height = 300, selectCallback = TRUE)%>%
      hot_col(col = "categoria", type = 'dropdown', source = c("Comida", "Entretenimiento", 
                                                               "Oficina", "Infraestructura", 
                                                               "Transporte", "Compras", "Ropa", 
                                                               "Teléfono", "Salud", "Deporte", 
                                                               "Electrónica", "Cuidado personal",
                                                               "Mascota", "Viajes", "Educación", 
                                                               "Impuestos", "Automóvil", 
                                                               "Estados de cuenta" ))
    #datatable(ar, selection =list(target = "cell"), editable = TRUE,
    #options = list(scrollY = '400px', scrollX = TRUE, paging = FALSE, 
    #searching = TRUE))
  })
  # %>%
  #hot_col(col = "fecha 2", type = 'date', format = 'MM/DD/YYYY', source = ar$fecha)
}

shinyApp(ui, server)