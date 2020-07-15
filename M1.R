library('shiny')

observeEvent(input$sampledata, {
  data('endosim')
  loadedData(endosim)
  
  textobj(paste(textobj(),'Using the Sample Data provided by RealROC.',' ', sep='<br/>'))
  
})

observe({
  req(input$main$datapath) # make sure variable isn't empty
  try({
    if (file_ext(input$main$datapath) == 'csv') {
      loadedData(
        read.csv(
          # initialize reactiveVal
          input$main$datapath,
          header = input$header,
          sep = input$sep,
          quote = input$quote
        )
      )
    } else {
      loadedData(read_excel(input$main$datapath,
                            sheet = input$sheetId))
    }
    
  })
})

observeEvent(listentoDataInputs(), {
  output$filetable <- renderDataTable({
    DT::datatable(loadedData())
  })
})

