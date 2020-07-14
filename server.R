library('npROCRegression')
library('Comp2ROC')
library('pROC')
library('readxl')
library("tools")

shinyServer(function(input, output, session) {
  
  loadedData <- reactiveVal()
  
  listentoDataInputs <-
    reactive({
      list(input$main, input$sampledata)
    })
  
  textobj <- reactiveVal('')
  output$reporttext <- renderText(textobj())
  
  # - Import Data Module  
  source('M1.R',local = T)
  
  # - Classic ROC Module  
  source('M2.R',local = T)
  
  # - AROC Module  
  source('M3.R',local = T)  
  
  # - Comparison Module  
  source('M4.R',local = T)  
  
  # - Avanced Module  
  source('Advanced.R',local = T)  
  
  observeEvent(listentoDataInputs(), {
    callModule(roccondi, "counter1", loadedData())
    callModule(roccondi, "counter2", loadedData())
    callModule(roccondi, "counter3", loadedData())
    
  })
    
})


