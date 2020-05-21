library('npROCRegression')
shinyServer(function(input, output, session) {
  loadedData <- reactiveVal()
  listentoDataInputs <- reactive({list(input$main, input$sampledata)})
  
  observeEvent(input$sampledata, {
    data('endosim')
    loadedData(endosim)
  })
  
  observe({
    req(input$main$datapath) # make sure variable isn't empty
    loadedData(
      read.csv(
        # initialize reactiveVal
        input$main$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote
      )
    )
  })

  #---------
  observeEvent(input$gene_classic, {
    
    if (input$classicurve_type == 'Empirical' ||
        input$classicurve_type == 'Empirical Smooth') {
      try(roccurve <- empiricalcurve(
        loadedData(),
        input$marker,
        input$resultcol,
        as.integer(input$healthy_pop),
        as.integer(input$disease_pop)
      ))
    }
    if (input$classicurve_type == 'Pooled Empirical' ||
        input$classicurve_type == 'Pooled Bayesian') {
      try(roccurve <- autopooled(
        loadedData(),
        input$marker,
        input$resultcol,
        as.integer(input$healthy_pop),
        as.integer(input$disease_pop),
        input$classicurve_type
      ))
    }
    
    if (input$classicurve_type == 'Empirical Smooth') {
      output$roccurve <- renderPlot({
        plot(pROC::smooth(roccurve))
        
        
      })
    } else {
      output$roccurve <- renderPlot({
        plot(roccurve)
      })
    }
    
  })
  
  observeEvent(input$gene_classic, {
    #dense_plot <- density_builder(loadedData(), input$marker, input$resultcol)
    output$classic_density <- renderPlot({
      density_builder(loadedData(), input$marker, input$resultcol)
    })
  })
  
  #AROC Module
  observeEvent(
    listentoDataInputs()
  , {
    output$AROCcovariate <- renderUI({
      selectInput(
        'cov',
        'Select Select Covariate ',
        multiple = TRUE,
        choices = names(loadedData())
      )
    })
  })
  
  observeEvent(input$gene_aroc, {
    aroc_curve <-
      gene_aroc_analysis(
        loadedData(),
        input$marker,
        input$resultcol,
        input$cov,
        as.integer(input$healthy_pop)
      )
    
    output$aroc <- renderPlot({
      plot(aroc_curve)
    })
  })
  
  
  observeEvent(input$gene_aroc, {
    output$aroc_density <- renderPlot({
      aroc_density_builder(loadedData(), input$marker, input$resultcol, input$cov)
    })
  })
  
  
  
  observeEvent(
    listentoDataInputs(), {
    callModule(roccondi, "counter1", loadedData())
    callModule(roccondi, "counter2", loadedData())
    
    #----Advanced
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
    aa <- as.array(input$signalchangecolumn)
    tmp <- loadedData()
    tmp[aa] <- tmp[aa] * -1
    loadedData(tmp) # update reactiveVal
  })
  
  observeEvent(input$comptype, {
    if (input$comptype == 'AROC') {
      tagList(
        callModule(roccondi, 'counter3', loadedData()),
        output$AROCcovariatecomp <-
          renderUI(
            selectInput(
              'cov',
              'Select Select Covariate ',
              multiple = TRUE,
              choices = names(loadedData())
            )
          )
      )
      
    }
    else
      print('ola')
  })
  
  observeEvent(input$compOnAROC, {
    AROCobj <- gene_aroc_analysis(
      loadedData(),
      input$marker,
      input$resultcol,
      input$cov,
      as.integer(input$healthy_pop)
    )
    
    polROCobj <- autopooled(
      loadedData(),
      input$marker,
      input$resultcol,
      as.integer(input$healthy_pop),
      as.integer(input$disease_pop)
    )
    
    output$AROCcompplot <-
      renderPlot(compAROC_ggplot(AROCobj, polROCobj, 'Title', 'AROC', 'ROC'))
    
  })
  
  
  observeEvent(
    listentoDataInputs(), {
    output$filetable <-renderDataTable({
      DT::datatable(loadedData())
    })
  })
  
  
  
})
