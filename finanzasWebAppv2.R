list.of.packages <- c("shiny", "shinydashboard","shinydashboardPlus","DT","tidyverse","dplyr","DBI","RSQLite","rhandsontable","sweetalertR","formattable")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages) != 0){
  install.packages(new.packages)
} else if (!is.logical(length(new.packages) != 0 & (new.packages %in% "sweetalertR"))){
  devtools::install_github("timelyportfolio/sweetalertR") 
}

#lectura de multiples paqueterias
lapply(list.of.packages, require, character.only = TRUE)

ui <- dashboardPage(
  dashboardHeaderPlus(
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "gears"
  ),
  dashboardSidebar(
    sidebarMenu(
      sidebarMenu(
        menuItem("Automatico", tabName = "dashboard", icon = icon("dashboard"))
      ),
      sidebarMenu(
        menuItem("Manual", icon = icon("th"), tabName = "manual"),
        menuItem("Agregar Datos", icon = icon("th"), tabName = "datos",
                 textInput("fecha", label = "Fecha", value = "dd/mm/aa", width = 150),
                 textInput("des", label = "Descripción", value = "Descripción", width = 150),
                 textInput("monto", label = "Monto", value = "Monto", width = 150),
                 textInput("cat", label = "Categoria", value = "Categoria", width = 150),
                 actionButton("Agregar", label = "Agregar")
        )
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fileInput("ar", "Adjunta Archivo", width = "300px"),
              mainPanel(
                rHandsontableOutput("file"),
                actionButton("Guardar", label = "Guardar", width = "100")
              )
      ),
      
      tabItem(tabName = "manual",
              rHandsontableOutput("tabla")
              
      )
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
    fecha <- seq(from = Sys.Date(), by = "days", length.out = 10)
    #categoria <- factor(LETTERS[41:1],levels =LETTERS[41:1])
    archivo <- data.frame(archivo, categoria)
    ar <- select(archivo, -credito, -saldo, -moneda)
    rhandsontable(ar, width = 700, stretchH = "all",height = 300, selectCallback = TRUE)%>%
      hot_col(col = "categoria", type = 'dropdown', source = c("Comida", "Entretenimiento", 
                                                               "Oficina", "Infraestructura", 
                                                               "Transporte", "Compras", "Ropa", 
                                                               "Teléfono", "Salud", "Deporte", 
                                                               "Electrónica", "Cuidado personal",
                                                               "Mascota", "Viajes", "Educación", 
                                                               "Impuestos", "Automóvil", 
                                                               "Estados de cuenta" ))%>%
      hot_col(col = "fecha", type = 'date', source = Sys.Date())%>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)%>%
      hot_cols(colWidths = 160) %>%
      hot_cols(fixedColumnsLeft = 1)
  })
  
  
  observeEvent(input$Agregar,{
    newrow <- data.frame(
      Fecha = input$fecha,
      Descripcion = input$des,
      Debito = input$monto,
      Categoria = input$cat
    )
    write.csv2(newrow, file = "NTDCR")
    ntab <- read.csv2("NTDCR",skip = 1)
  })
  
  output$tabla <- renderRHandsontable({
    ntab <- read.csv2("NTDCR",skip = 1)
    rhandsontable(ntab, width = 700, stretchH = "all",height = 300, selectCallback = TRUE)
      
  })
}

shinyApp(ui, server)
