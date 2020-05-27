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
    try(
    aroc_curve <-
      gene_aroc_analysis(
        loadedData(),
        input$marker,
        input$resultcol,
        input$cov,
        as.integer(input$healthy_pop),
        input$aroc_type
      )
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
  
  #------- Comp Module
  
  observeEvent(input$comptype, {
    if (input$comptype == 'AROC') {
      tagList(
        callModule(roccondi, 'counter3', loadedData()),
        output$AROCcovariatecomp <-
          renderUI(
            selectInput(
              'cov',
              'Select Covariate ',
              multiple = TRUE,
              choices = names(loadedData())
            )
          )
      )
      
    }
    else

      print('bla')
  })
  
  observeEvent(input$compOnAROC, {
    if (input$comptype == 'AROC'){
    try(
    AROCobj <- gene_aroc_analysis(
      loadedData(),
      input$marker,
      input$resultcol,
      input$cov,
      as.integer(input$healthy_pop), 
      aroc_type = 'Semiparametric'
    ))
    
    try(
    polROCobj <- autopooled(
      loadedData(),
      input$marker,
      input$resultcol,
      as.integer(input$healthy_pop),
      as.integer(input$disease_pop),
      type = 'Pooled Empirical'
    ))
      output$AROCcompplot <-
        renderPlot(compAROC_ggplot(AROCobj, polROCobj, 'Title', 'AROC', 'ROC'))
      
    } else {
        
      tempvar <- loadedData()
      templist <- split(tempvar, tempvar[input$cov])
      print(str(templist))
      
      binary1 <- templist[[1]]
      binary1 <- binary1[,c(input$marker,input$resultcol)]
      
      binary2 <- templist[[2]]
      binary2 <- binary2[,c(input$marker,input$resultcol)]
      
      
   
      #print(head(binary1))
      #print(head(binary2))

      datafromfunc <- comp_converter(binary1,binary2,input$resultcol,input$resultcol,FALSE)

      sim1.ind = unlist(datafromfunc[1])
      sim2.ind = unlist(datafromfunc[2])
      sim1.sta = unlist(datafromfunc[3])
      sim2.sta = unlist(datafromfunc[4])
      
      sim1.pred = prediction(sim1.ind, sim1.sta)
      sim2.pred = prediction(sim2.ind, sim2.sta)
      sim1.curve = performance(sim1.pred, "tpr", "fpr")
      sim2.curve = performance(sim2.pred, "tpr", "fpr")
      
      roc.curves.plot(sim1.curve, sim2.curve, mod1=moda1, mod2=moda2)
      res <- roc.curves.boot(datafromfunc,100, 0.05,name='CRIB_sex_ind',"CRIBM","CRIBF",FALSE)
      
      
      
      
        
      output$AROCcompplot <-
        renderPlot(roc.curves.plot(sim1.curve, sim2.curve, mod1=moda1, mod2=moda2))
      
      }
    

    
  })
  
  
  observeEvent(
    listentoDataInputs(), {
    output$filetable <-renderDataTable({
      DT::datatable(loadedData())
    })
  })
  
  
  
})
