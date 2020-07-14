library('shiny')

observeEvent(listentoDataInputs(),{
  output$advancedsignalchange <- renderUI({
    tagList(
      selectInput(
        'signalchangecolumn',
        'Signal Change Column',
        choices = names(loadedData())
      ),
      actionButton('signalchange', 'Submit signal change')
    )
    
  })
})

observeEvent(input$signalchange, {
  try({
    aa <- as.array(input$signalchangecolumn)
    tmp <- loadedData()
    tmp[aa] <- tmp[aa] * -1
    loadedData(tmp) # update reactiveVal
    
    
    changetext <- isolate(paste0(
      'Signal changed to variable ',
      input$signalchangecolumn,
      ' sucessfully'
    ))
    
    output$signalchangeoutput <- renderText(changetext)
    
  })
})
