library(shinydashboard)
library(shiny)
library(ggplot2)


ui <- dashboardPage( skin = 'red',
  dashboardHeader(title = "Manejo de Finanzas",
                  dropdownMenu(type = "notifications",
                               notificationItem(
                                 text = "Límite de 'Diversión' alcanzado",
                                 icon = icon("exclamation-triangle"),
                                 status = "warning"
                               )           
                    )
                  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Datos del Usuario", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Visualización", tabName = "graph", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              h1("Clasificación de los Ingresos"),
              fluidRow(
                tabBox(
                  title = "Tabla de Ingresos",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = "500px",
                  tabPanel( "Ingresos automáticos"),
                  tabPanel( "Ingresos manuales")
                )
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
    der <- read.csv('baseshiny.csv')
    ggplot(data = der, aes(x = der$Date, y = der$Money)) + geom_area(stat = 'identity')
  })
}

shinyApp(ui, server)