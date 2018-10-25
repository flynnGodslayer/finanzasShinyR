library(shinydashboard)
library(shiny)
library(ggplot2)
library(dplyr)


ui <- dashboardPage( skin = 'red',
  dashboardHeader(title = "Manejo de Finanzas",
                  dropdownMenu(type = "notifications",
                               notificationItem(
                                 text = "Límite de 'Diversión' alcanzado",
                                 icon = icon("exclamation-triangle"),
                                 status = "warning"
                               )           
                    ),
                  tags$li(class = "dropdown",
                          tags$li(class = "dropdown", textOutput("logged_user"), 
                                  style = "padding-top: 15px; padding-bottom: 15px; color: #fff;"),
                          tags$li(class = "dropdown", actionLink("login", textOutput("logintext")))
                  ),
                  tags$li(actionLink("openModal", label = "", icon = icon("info")),
                          class = "dropdown")
                  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Datos del Usuario", icon = icon("dashboard"),
                  menuSubItem("Ingreso Automático",tabName = "dashboard",
                              icon = icon("dashboard")),
                  menuSubItem("Ingreso Manual",tabName = "Inputsidebars",
                              icon = icon("cog", lib = "glyphicon"))
               ),
      menuItem("Visualización", tabName = "graph", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              h1("Clasificación de los Ingresos"),
              fluidRow(
                tabBox(
                  # The id lets us use input$tabset1 on the server to find the current tab
                  width = "auto",
                  id = "tabset1", height = "auto",
                  tabPanel( "Ingresos automáticos"),
                  sidebarPanel(
                  fileInput("file","Subir Archivo"),
                  checkboxGroupInput("cot","Categorias",
                                     choices = c("Comida","Entretenimiento","Oficina","Infraestructura",
                                                 "Transporte","Telefono","Salud","Electronica",
                                                 "Cuidado Personal","Mascotas","Viajes","Estados de Cuenta"))
                  ),
                  mainPanel(h3("Tabla de Ingresos"),tableOutput("input_file"))
                )
              ) 
      ),
      tabItem(tabName = "Inputsidebars",
              h2("Ingresos Manuales"),
              fluidRow(
                sidebarPanel(
                  selectInput(inputId = "name",#Funcion selectInput(Es una funcion reactive); Filtro 
                              label = "Categorias",
                              choices = c("Comida","Entretenimiento","Oficina","Infraestructura",
                                          "Transporte","Telefono","Salud","Electronica",
                                          "Cuidado Personal","Mascotas","Viajes","Estados de Cuenta")),
                  textInput("text",h4("Gasto"),value = "Enter text..."),
                  numericInput("moneda",h4("Importe"),value = NULL),
                  actionButton("click","Adjuntar")
                ),
                mainPanel(h3("Tabla de Ingresos"),tableOutput("input_file"))
            )
      ),
      tabItem(tabName = "graph",
              h2("Análisis de los Ingresos"),
        fluidRow(
          box(plotOutput("plot1", height = 250)),
      
          box(
            title = "Controls",
            sliderInput("slider", "Number of observations:", 1, 100, 50)
          ),
          box(plotOutput("plot2", height = 250))
        )
      )
    )
  )
)
server <- function(input, output) {
  
  set.seed(153)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  output$plot2 <- renderPlot({
    der <- read.csv('TDCR.csv',skip = 1)
    ggplot(data = der, aes(x = Fecha, y = Débito)) + geom_point(stat = 'identity')
  })
  observeEvent(input$openModal, {
    showModal(
      modalDialog(title = "Instituto de Fisica",
                  p("Esta aplicación fue realizada para el manejo de finanzas personales"))
    )
  })
  
  
  logged_in <- reactiveVal(FALSE)
  
  # switch value of logged_in variable to TRUE after login succeeded
  observeEvent(input$login, {
    logged_in(ifelse(logged_in(), FALSE, TRUE))
  })
  
  # show "Login" or "Logout" depending on whether logged out or in
  output$logintext <- renderText({
    if(logged_in()) return("Logout here.")
    return("Login here")
  })
  
  # show text of logged in user
  output$logged_user <- renderText({
    if(logged_in()) return("User 1 is logged in.")
    return("")
  })
  #upload files of our device
  output$input_file <- renderTable({
     file_to_read = input$file
     if(is.null(file_to_read)){
       return()
     }
     read.csv(file_to_read$datapath, header = TRUE, skip = 1, colClasses = c(NA, NA, NA, NA, "NULL", NA))
   })
   #output$cat <- 
}

shinyApp(ui, server)