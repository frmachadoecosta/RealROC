library('npROCRegression')
shinyServer(function(input, output, session) {
  
  loadedData <- reactiveVal()
  
  observeEvent(input$sampledata,{
    data('endosim')
    loadedData(endosim)
    })
  
  observe({
    req(input$main$datapath) # make sure variable isn't empty
    loadedData(read.csv( # initialize reactiveVal
      input$main$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote)) 
  })

  #Classic ROC UI generation
  #observeEvent(input$main,{
  #  output$ROCcondicionals <- renderUI({
  #    tagList(
  #      selectInput('marker','Select Marker or Test ',
  #                  multiple=FALSE, choices = names(loadedData())),
  #      selectInput('resultcol','Select Result Column',
  #                  multiple=FALSE, choices = names(loadedData())),
  #      textInput('healthy_pop','Select Healthy Value'),
  #      textInput('disease_pop','Select Disease Value')
  #    )
  #  })
  #})
  
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
  
  #AROC Module
  observeEvent({input$main|input$sampledata},{
    output$AROCcovariate <- renderUI({
        selectInput('cov','Select Select Covariate ',
                    multiple=TRUE, choices = names(loadedData()))
      })
    })
  
  observeEvent(input$gene_aroc, {
    aroc_curve <- gene_aroc_analysis(loadedData(), input$marker, input$resultcol, input$cov, as.integer(input$healthy_pop))
    
    output$aroc <- renderPlot({plot(aroc_curve)})
  })
  
  
  observeEvent(input$gene_aroc, {
  output$aroc_density <- renderPlot({
    aroc_density_builder(loadedData(), input$marker, input$resultcol, input$cov)
    })
  })
  
  
  
  observeEvent({input$main|input$sampledata}, {
    callModule(roccondi, "counter1",loadedData())
    callModule(roccondi, "counter2",loadedData())
    
    #----Advanced
    output$advancedsignalchange <- renderUI({
      tagList(
        selectInput('signalchangecolumn', 'Signal Change Column', choices = names(loadedData())),
        actionButton('signalchange','Submit signal change')
      )
      
    })
    })
  
  observeEvent(input$signalchange, {
    aa <- as.array(input$signalchangecolumn)
    tmp <- loadedData()
    tmp[aa] <- tmp[aa] * -1
    loadedData(tmp) # update reactiveVal
  })
  
  observeEvent(input$comptype,{
    if (input$comptype == 'AROC'){
      tagList(
        callModule(roccondi,'counter3',loadedData()),
        output$AROCcovariatecomp <- 
          renderUI(selectInput('cov','Select Select Covariate ',
                    multiple=TRUE, choices = names(loadedData())))
      )
              
    }
    else print('ola')
  })
  
  observeEvent(input$compOnAROC,{
    AROCobj <- gene_aroc_analysis(loadedData(), 
                                  input$marker, 
                                  input$resultcol, 
                                  input$cov, 
                                  as.integer(input$healthy_pop))
    
    polROCobj <- autopooled(loadedData(), 
                            input$marker, 
                            input$resultcol, 
                            as.integer(input$healthy_pop), 
                            as.integer(input$disease_pop))
 
    output$AROCcompplot <-renderPlot(compAROC_ggplot(AROCobj,polROCobj,'Title','AROC','ROC'))
    
  })
  
  
  observeEvent({input$main|input$sampledata},{
    output$filetable <- DT::renderDataTable({
      
      DT::datatable(loadedData())
    })
  })
  
  
})
