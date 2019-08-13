shinyServer(function(input, output, session) {
  loadedData <- reactive({
    read.csv(
      input$main$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote
    )
  })
  
  
  #Classic ROC UI generation
  observeEvent(input$main,{
    output$ROCcondicionals <- renderUI({
      tagList(
        selectInput('marker','Select Marker or Test ',
                    multiple=FALSE, choices = names(loadedData())),
        selectInput('resultcol','Select Result Column',
                    multiple=FALSE, choices = names(loadedData())),
        textInput('healthy_pop','Select Healthy Value'),
        textInput('disease_pop','Select Disease Value')
      )
    })
  })
  
  #---------
  observeEvent(input$gene_classic,{
    #updateTabsetPanel(session, 'ROCplot', selected = 'ROCplot')
    roccurve <- autopooled(loadedData(), input$marker, input$resultcol, as.integer(input$healthy_pop), as.integer(input$disease_pop))
    output$roccurve <- renderPlot({plot(roccurve)
    })
  })
  
  observeEvent(input$gene_classic, {
    #dense_plot <- density_builder(loadedData(), input$marker, input$resultcol)
    output$classic_density <- renderPlot({
      density_builder(loadedData(), input$marker, input$resultcol)})
  })
  
  output$filetable <- DT::renderDataTable({
    req(input$main)
    
    DT::datatable(loadedData())
  })
  
})
