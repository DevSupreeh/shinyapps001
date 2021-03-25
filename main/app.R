

# Load R packages
library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(randomForest)

# Read data
weather <- read.csv("https://raw.githubusercontent.com/dataprofessor/data/master/weather-weka.csv", stringsAsFactors = T )

# Build model
model <- randomForest(play ~ ., data = weather, ntree = 500, mtry = 4, importance = TRUE)

# Read in the RF model
model2 <- readRDS("model.rds")

# Training set
TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]


# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage(
                  "My first app",
                  tabPanel("Name",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("txt1", "First Name:", ""),
                             textInput("txt2", "Middle Name:", ""),
                             textInput("txt3", "Surname:", ""),
                             actionButton("submitbutton1", "Submit", class = "btn btn-primary")
                           ), # sidebarPanel
                           mainPanel(
                             h1("Output"),
                             h4("Full Name"),
                             verbatimTextOutput("txtout1")
                             
                           ) # mainPanel
                           
                  ), # Navbar , tabPanel
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
                             actionButton("submitbutton2", "Submit", class = "btn btn-primary")
                           ), # sidebarPanel
                           mainPanel(
                             h1("Output"),
                             
                             h4("Full Address"),
                             verbatimTextOutput("txtout2")
                             
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
                                           step = 2),
                               
                               
                               
                             ),
                             
                             # Main panel for displaying outputs ----
                             mainPanel(
                               
                               # Output: Histogram ----
                               plotOutput(outputId = "distPlot")
                               
                             )
                           )),
                  tabPanel("Play Golf?",
                           sidebarPanel(
                             tags$label(h3('Play Golf?')),
                             HTML("<h3>Input parameters</h3>"),
                             
                             selectInput("outlook", label = "Outlook:", 
                                         choices = list("Sunny" = "sunny", "Overcast" = "overcast", "Rainy" = "rainy"), 
                                         selected = "Sunny"),
                             sliderInput("temperature", "Temperature:",
                                         min = 64, max = 86,
                                         value = 70),
                             sliderInput("humidity", "Humidity:",
                                         min = 65, max = 96,
                                         value = 90),
                             selectInput("windy", label = "Windy:", 
                                         choices = list("Yes" = "TRUE", "No" = "FALSE"), 
                                         selected = "TRUE"),
                             
                             actionButton("submitbutton3", "Submit", class = "btn btn-primary")
                           ),
                           
                           mainPanel(
                             tags$label(h3('Status/Output')), # Status/Output Text Box
                             verbatimTextOutput('contents'),
                             tableOutput('tabledata') # Prediction results table
                             
                           )
                           ),
                  tabPanel("Iris Predictor",
                           sidebarPanel(
                             tags$label(h3('Iris Predictor')),
                             #HTML("<h3>Input parameters</h3>"),
                             tags$label(h3('Input parameters')),
                             sliderInput("Sepal.Length", label = "Sepal Length", value = 5.0,
                                         min = min(TrainSet$Sepal.Length),
                                         max = max(TrainSet$Sepal.Length)
                             ),
                             sliderInput("Sepal.Width", label = "Sepal Width", value = 3.6,
                                         min = min(TrainSet$Sepal.Width),
                                         max = max(TrainSet$Sepal.Width)),
                             sliderInput("Petal.Length", label = "Petal Length", value = 1.4,
                                         min = min(TrainSet$Petal.Length),
                                         max = max(TrainSet$Petal.Length)),
                             sliderInput("Petal.Width", label = "Petal Width", value = 0.2,
                                         min = min(TrainSet$Petal.Width),
                                         max = max(TrainSet$Petal.Width)),
                             
                             actionButton("submitbutton4", "Submit", 
                                          class = "btn btn-primary")
                           ),
                           
                           mainPanel(
                             tags$label(h3('Status/Output')), # Status/Output Text Box
                             verbatimTextOutput('contents2'),
                             tableOutput('tabledata2') # Prediction results table
                             
                           )
                           )         
                           
                           
                
                  
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output, session) {
  
  output$txtout1 <- renderText({
    if (input$submitbutton1>0){
      isolate(paste( input$txt1, input$txt2, input$txt3, sep = " " ))
    }
  })
  
  output$txtout2 <- renderText({
    if (input$submitbutton2>0){isolate(
      paste( input$txt4, input$txt5, input$txt6, input$txt7, input$txt8, input$txt9, input$txt10, input$txt11, sep = "\n" )
    )}
  })
  
  output$distPlot <- renderPlot({
    x    <- airquality$Solar.R
    x    <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "red", border = "black",
         xlab = "Solar level",
         main = "Histogram of Solar level")
  })

  
  datasetInput <- reactive({  
    
    # outlook,temperature,humidity,windy,play
    df <- data.frame(
      Name = c("outlook",
               "temperature",
               "humidity",
               "windy"),
      Value = as.character(c(input$outlook,
                             input$temperature,
                             input$humidity,
                             input$windy)),
      stringsAsFactors =  FALSE)
    
    play <- "play"
    df <- rbind(df, play)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    test$outlook <- factor(test$outlook, levels = c("overcast", "rainy", "sunny"))
    
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  datasetInput2 <- reactive({  
    
    df <- data.frame(
      Name = c("Sepal Length",
               "Sepal Width",
               "Petal Length",
               "Petal Width"),
      Value = as.character(c(input$Sepal.Length,
                             input$Sepal.Width,
                             input$Petal.Length,
                             input$Petal.Width)),
      stringsAsFactors = FALSE)
    
    Species <- 0
    df <- rbind(df, Species)
    input <- transpose(df)
    write.table(input,"input2.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input2", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Prediction=predict(model2,test), round(predict(model2,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton3>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton3>0) { 
      isolate(datasetInput()) 
    } 
  })  

  # Status/Output Text Box
  output$contents2 <- renderPrint({
    if (input$submitbutton4>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata2 <- renderTable({
    if (input$submitbutton4>0) { 
      isolate(datasetInput2()) 
    } 
  })
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
