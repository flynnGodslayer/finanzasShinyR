library(shinydashboard)
library(shiny)
library(ggplot2)
library(dplyr)
library(formattable)

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
                  fileInput("file","Subir Archivo",width = "1000px"),
                  checkboxGroupInput("cot","Categorias",
                                     choices = c(Comida = 'Rest',Entretenimiento = 'MINISO',"Oficina","Infraestructura",
                                                 Transporte = 'Gasol',"Telefono","Salud","Electronica",
                                                 "Cuidado Personal","Mascotas","Viajes","Estados de Cuenta"))
                  ),
                  mainPanel(h3("Tabla de Ingresos"),formattableOutput("input_file"))
                )
              ) 
      ),
      tabItem(tabName = "Inputsidebars",
              h2("Ingresos Manuales"),
              fluidRow(
                sidebarPanel(
                  fila <- 1, #inicializa variable para darle un id a cada ingreso del usuario
                  selectInput(inputId = "name",#Funcion selectInput(Es una funcion reactive); Filtro 
                              label = "Categorias",
                              choices = c("Comida","Entretenimiento","Oficina","Infraestructura",
                                          "Transporte","Telefono","Salud","Electronica",
                                          "Cuidado Personal","Mascotas","Viajes","Estados de Cuenta")),
                  textInput("ingresoFila",label = "Descripción", placeholder = "Describe tu gasto"), 
                  numericInput(inputId = fila + 1, label = "moneda", value = "NULL", min = .01),
                  actionButton("click","Adjuntar")
                ),
                mainPanel(h3("Tabla de Ingresos"))#poner el table output de acuerdo a una nueva variable para hacer el display 
            )
      ),
      tabItem(tabName = "graph",
              h2("Análisis de los Ingresos"),
        fluidRow(
          box(plotOutput("plot1", height = 250,width = 250)),
          box(plotOutput("plot2", height = 250, width = 1000))
        )
      )
      
    )
  )
)
server <- function(input, output) {
  
  set.seed(153)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    der <- read.csv('TDCR.csv',skip = 1)
   der <- ggplot(data =der) + geom_bar(mapping = aes(x =Fecha , fill =Descripción),
      show.legend = FALSE,width = 1) + theme(aspect.ratio = 1) + labs(x = NULL, y = NULL)
   der + coord_flip()  
    
  })

  output$plot2 <- renderPlot({
    der <- read.csv('TDCR.csv',skip = 1)
    ggplot(data = der) + geom_bar(mapping = aes(x = Débito, fill = Descripción),
        position = "dodge")
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
  #Lee el csv y lo mete en la variable input_file
  output$input_file <- renderFormattable({
     file_to_read = input$file
     #file_to_read$Crédito[!is.na(file_to_read$Débito)] <- -file_to_read$Débito[!is.na(file_to_read$Crédito)]
     #file_to_read$Crédito <- sprintf('-%', file_to_read$Crédito)
     if(is.null(file_to_read)){
       return()
     }
<<<<<<< HEAD
     x <- read.csv(file_to_read$datapath, header = TRUE, skip = 1,
              colClasses = c(NA, NA, NA, NA, "NULL", "NULL"))
     formattable(x)
   })
=======
     file_to_read <- read.csv(file_to_read$datapath, header = TRUE, skip = 1, colClasses = c(NA, NA, NA, NA, "NULL", "NULL"))
     
     file_to_read$Crédito[!is.na(file_to_read$Débito)] <- -file_to_read$Crédito[!is.na(file_to_read$Crédito)] 
     #file_to_read$Saldo <- paste(file_to_read$Débito, file_to_read$Crédito)
     #file_to_read$col4 <- "prueba"
     #file_to_read$Crédito <- sprintf('-%i', file_to_read$Crédito)
     #file_to_read$Débito[is.na(file_to_read$Débito)] <- sub("^", "-", !is.na(file_to_read$Crédito) )
     #file_to_read$Crédito[!is.na(file_to_read$Débito)] <- -file_to_read$Débito[!is.na(file_to_read$Crédito)]
   })
  
  #file_to_read$Saldo <- paste(file_to_read$Débito, file_to_read$Credito)
   #output$cat <- 
>>>>>>> refs/remotes/origin/master
}

shinyApp(ui, server)