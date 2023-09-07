library(shiny)
library(ggplot2)
library(deSolve)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Simulation en fonction des valeurs de delta A et delta V"),
  sidebarLayout(
    sidebarPanel(
      numericInput("delta_V", label = "If delta V is", min = 0, max = 100, value = 2.7, step = 0.001),
      numericInput("delta_A", label = "If delta A is", min = 0, max = 100, value = 0.21, step = 0.001)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  parms <- reactive({
    c(delta_V = input$delta_V, delta_A = input$delta_A)
  })
  
  derivs <- function(t, y, parms) {
    with(as.list(c(y, parms)), {
      V =  exp(-(delta_V * t))
      dA = V - delta_A * A
      return(list(c(dA)))
    })
  }
  
  y0 <- c( A = 0)
  
  times <- seq(from = 0, to = 365, by = 0.01)
  
  out <- reactive({
    ode(y = y0, times = times, func = derivs, parms = parms(), method = "impAdams", atol = 1e-10 )
  })
  
  data <- reactive({
    as.data.frame(out())
  })
  
  output$plot <- renderPlot({
    ggplot(data = data(), aes(x = time)) +
      geom_line(aes(y = A, color = "Antigen"), linewidth = 1.2) +
      geom_line(aes(y = exp(-(input$delta_V * time)), color = "Vaccin")) +
      scale_x_continuous(expand=c(0,0), breaks=c(0,7,14,28),
                         label=c("0","7","14", "28"),
                         limits = c(0,30)) +
      scale_y_continuous(expand=c(0,0), breaks=c(0,0.1,0.2),
                         label=c("0","0.1","0.2"),
                         limits = c(0,0.35)) +
      scale_color_manual(name = "Variables", values = c( "Antigen"="blue","Vaccin" = "red")) +
      theme_minimal()
  }, res = 96)
  
}

shinyApp(ui, server)
