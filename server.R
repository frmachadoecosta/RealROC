shinyServer(function(input, output, session) {
  
  loadedData <- reactive({
    
    read.csv(input$main$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)
  })

  output$filetable<- DT::renderDataTable({
    
    req(input$main)
    
    DT::datatable(loadedData())
  })
  
}

) asadasda