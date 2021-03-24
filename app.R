

# Load R packages
library(shiny)
library(shinythemes)


# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "My first app",
                  tabPanel("Name",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("txt1", "First Name:", ""),
                             textInput("txt2", "Middle Name:", ""),
                             textInput("txt3", "Surname:", ""),
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Output"),
                             h4("Full Name"),
                             verbatimTextOutput("txtout1"),
                             
                           ) # mainPanel
                           
                  ), # Navbar 2, tabPanel
                  tabPanel("Address",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("txt4", "Building:", ""),
                             textInput("txt5", "Society:", ""),
                             textInput("txt6", "Landmark:", ""),
                             textInput("txt7", "Sector:", ""),
                             textInput("txt8", "Station:", ""),
                             textInput("txt9", "City:", ""),
                             textInput("txt10", "State:", ""),
                             textInput("txt11", "Country:", ""),
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Output"),
                             
                             h4("Full Address"),
                             verbatimTextOutput("txtout2"),
                             
                           ) # mainPanel
                           
                  ), # Navbar 2, tabPanel
                  tabPanel("Solar Layer",
                           sidebarLayout(
                             
                             # Sidebar panel for inputs ----
                             sidebarPanel(
                               
                               # Input: Slider for the number of bins ----
                               sliderInput(inputId = "bins",
                                           label = "Number of bins:",
                                           min = 0,
                                           max = 40,
                                           value = 20,
                                           step = 2)
                               
                             ),
                             
                             # Main panel for displaying outputs ----
                             mainPanel(
                               
                               # Output: Histogram ----
                               plotOutput(outputId = "distPlot")
                               
                             )
                           )
                           
                  )
                  
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  
  output$txtout1 <- renderText({
    paste( input$txt1, input$txt2, input$txt3, sep = " " )
  })
  
  output$txtout2 <- renderText({
    paste( input$txt4, input$txt5, input$txt6, input$txt7, input$txt8, input$txt9, input$txt10, input$txt11, sep = "\n" )
  })
  
  output$distPlot <- renderPlot({
    x    <- airquality$Solar.R
    x    <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "red", border = "black",
         xlab = "Solar level",
         main = "Histogram of Solar level")
    
  })
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
