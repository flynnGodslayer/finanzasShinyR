list.of.packages <- c("shiny", "shinydashboard","DT","tidyverse","dplyr","DBI","RSQLite","rhandsontable","sweetalertR","formattable")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages) != 0){
  install.packages(new.packages)
} else if (!is.logical(length(new.packages) != 0 & (new.packages %in% "sweetalertR"))){
  devtools::install_github("timelyportfolio/sweetalertR") 
}

#lectura de multiples paqueterias
lapply(list.of.packages, require, character.only = TRUE)


################################################################################################
#El diseno del ShinyDashbord, se compone de 3 elementos;                                   
#
# header <- dashboardHeader()
# sidebar <- dashboardSidebar()
# body <- dashboardBody()
#
#Al terminar de definirlos se utilizan como parametros de la funcion dashboardPage()          
################################################################################################

######################
#Definicion de header#
######################

#Recordatorios en la parte superior derecha, se liga en la parte del server "output$task_menu"
header<- dashboardHeader()

########################
#Definicion de Siderbar#
########################
sidebar <- dashboardSidebar(
  sidebarMenu( #Funcion siderbarMenu; permite contener a las funciones menuItem, menuSubItem
    menuItem("Carga de Datos", #Funcion menuItem; Genera los iconos en la barra lateral izquierda.
             tabName = "Datos"),
    menuItem("Graficas",
             tabName = "dashboard"))
)


####################
#Definicion de Body#
####################
body <- dashboardBody(
  #TabItems, nos permite especificar los objetos que se muestran en cada MenuItem
  tabItems(
    tabItem(tabName = "Datos",
            #fluidrow, Permite almacenar varios objetos y especificar su tamaño y formato.        
            fluidRow(
              #Box1
              box(
                width = 3, title = "Formato de Carga Arvhivo",
                fileInput(inputId = "Archivo", "Carga de Archivo",
                          multiple = TRUE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                
                # Linea horizontal
                tags$hr(),
                
                # Input: Checkbox para encabezado
                checkboxInput(inputId = "header", "Header", TRUE),
                
                # Input: Metodo de Separacion de los Datos
                radioButtons(inputId = "sep", "Separacion",
                             choices = c(Comma = ",",
                                         Semicolon = ";",
                                         Tab = "\t"),
                             selected = ","),
                
                # Input: Select quotes ----
                radioButtons(inputId = "quote", "Quote",
                             choices = c(None = "",
                                         "Double Quote" = '"',
                                         "Single Quote" = "'"),
                             selected = '"'),
                
                # Horizontal line ----
                tags$hr(),
                
                # Input: Select number of rows to display ----
                radioButtons(inputId = "disp", "Display",
                             choices = c(Head = "head",
                                         All = "all"),
                             selected = "head")
              ),
              #Box2
              box(width = 8,title = "Tabla Editable",
                  rHandsontableOutput(outputId = "Tabla"), #Muestra la Tabla
                  br(),
                  #Boton para Guardar los Resultados
                  actionButton(inputId = "Btn_Guardar","Guardar Cambios")
              )
            )
    ),
    #MenuItem, dashboard
    tabItem(tabName = "dashboard")
  )
)

# Funcion dashboadPage; Crea una pagina de panel de control para usar una aplicacion Shiny
ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body)

########
#Server#
########

server <- function(input, output) {
  #Funcion render conectada con rHandsontableOutput
  output$Tabla <- renderRHandsontable({
    
    req(input$Archivo)
    
    df <- read.csv(input$Archivo$datapath,
                   skip = 1,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote,
                   stringsAsFactors = FALSE)
    colnames(df) <- c("fecha", "descripcion", "debito", "credito", "saldo", "moneda")
    df$debito <- as.numeric(gsub(",", "", df$debito ))
    df$credito <- as.numeric(gsub(",", "", df$credito))
    df$debito[!is.na(df$credito)] <- -df$credito[!is.na(df$credito)]
    df <- select(df, -credito, -saldo, -moneda)
    df$categoria <- as.factor("-")
    
    if(input$disp == "head"){
      return(head(rhandsontable(df, width = 800,height = 300) %>% #Funcion rhandsontable, permite mostrar los datos con formato de Excell
                    hot_col(col = "categoria", type = "dropdown",allowInvalid = TRUE, #hot_col, da el formato a las columnas de nuestra tabla
                            source = c("Comida", "Entretenimiento","Oficina", "Infraestructura","Transporte", "Compras", "Ropa", 
                                       "Teléfono", "Salud", "Deporte","Electrónica","Cuidado personal","Mascota","Viajes","Educación", 
                                       "Impuestos", "Automóvil","Estados de cuenta")     
                    )%>%
                    hot_col(col = "fecha", type = 'date', source = Sys.Date())%>%
                    hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)%>%
                    hot_cols(colWidths = 160) %>%
                    hot_cols(fixedColumnsLeft = 1) 
      )
      )
    } else{
      rhandsontable(df, width = 800,height = 1000) %>%
        hot_col(col = "categoria", type = "dropdown",allowInvalid = TRUE,
                source = c("Comida", "Entretenimiento","Oficina", "Infraestructura","Transporte", "Compras", "Ropa", 
                           "Teléfono", "Salud", "Deporte","Electrónica","Cuidado personal","Mascota","Viajes","Educación", 
                           "Impuestos", "Automóvil","Estados de cuenta")
        )
    }
  })
  #Guarda los cambios en la Tabla, de la ruta que tomamos el arhivo.
  observeEvent(input$Btn_Guardar,
               write.csv(hot_to_r(df),file = "TDC.csv", row.names = FALSE))
}

shinyApp(ui, server)