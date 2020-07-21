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

observeEvent(listentoDataInputs(),{
  output$advancedfactor <- renderUI({
    tagList(
      selectInput(
        'factorcolumn',
        'Force Factor Column',
        choices = names(loadedData())
      ),
      actionButton('factorchange', 'Change column to factor')
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

observeEvent(input$factorchange,{
  try({
    aa <- as.array(input$factorcolumn)
    tmp <- loadedData()
    
    #print(head(tmp[[aa]]))
    #print(typeof(tmp[[aa]]))
    
    tmp[[aa]] <- as.factor(tmp[[aa]])
    
    #print(head(tmp[aa]))
    loadedData(tmp) # update reactiveVal
    
    
    changetext <- isolate(paste0(
      input$factorcolumn, ' changed to factor variable sucessfully'
    ))
    
    output$signalchangeoutput <- renderText(changetext)
    
  })
})



