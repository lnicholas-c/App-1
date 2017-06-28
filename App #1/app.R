library(shiny)
library(datasets)

ui <- fluidPage(
  
  titlePanel("Motor Trend Car Road Tests Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput( "xVariable", 
                   "Choose the variable to be displayed on the X axis:",
                   choices = c( "Miles Per Gallon", "Horse Power", "Weight", 
                                "Quarter Mile Time", "Number of Cylinders", 
                                "Number of Carburetors")),
      
      selectInput( "yVariable", 
                   "Choose the variable to be displayed on the Y axis:", 
                   choices = c("Miles Per Gallon", "Horse Power", "Weight", 
                               "Quarter Mile Time", "Number of Cylinders", 
                               "Number of Carburetors"))
    ),
    
    mainPanel(
      plotOutput("mtcarsPlot")
    )
  )
)

server <- function( input, output ) {
  
  datasetInputX <- reactive({
    switch(input$xVariable,
           "Miles Per Gallon" = mtcars$mpg,
           "Horse Power" = mtcars$hp, 
           "Weight" = mtcars$wt, 
           "Quarter Mile Time" = mtcars$qsec, 
           "Number of Cylinders" = mtcars$cyl,
           "Number of Carburetors" = mtcars$carb)
  })  
    
  datasetInputY <- reactive({
    switch(input$yVariable,
           "Miles Per Gallon" = mtcars$mpg,
           "Horse Power" = mtcars$hp, 
           "Weight" = mtcars$wt, 
           "Quarter Mile Time" = mtcars$qsec, 
           "Number of Cylinders" = mtcars$cyl,
           "Number of Carburetors" = mtcars$carb)  
  })
  
  output$mtcarsPlot <- renderPlot({
    plot( datasetInputX(), datasetInputY(),
          xlab = input$xVariable, ylab = input$yVariable)
    abline(lm(datasetInputY()~datasetInputX()), col = "red")
    
  })
  
}

shinyApp( ui = ui, server = server )
