library('shiny')

#AROC Module
observeEvent(listentoDataInputs()
             , {
               output$AROCcovariate <- renderUI({
                 selectInput(
                   'cov',
                   'Select Select Covariate ',
                   multiple = F,
                   choices = names(loadedData())
                 )
               })
             })

observeEvent(input$gene_aroc, {
  try({
    aroc_curve <-
      gene_aroc_analysis(
        loadedData(),
        input$marker,
        input$resultcol,
        input$cov,
        as.integer(input$healthy_pop),
        input$aroc_type
      )
    
    toreport <-
      propersummary(aroc_curve,
                    input$marker,
                    input$resultcol,
                    input$healthy_pop,
                    input$cov)
    for (line in toreport) {
      textobj(newreport(textobj, tags$div(line)))
    }
    
    
  })
  
  
  
  output$aroc <- renderPlot({
    loadfunc()
    plot(aroc_curve)
  })
  
  
})


observeEvent(input$gene_aroc, {
  
  isolate({ #makes only responde to Action Button
    tempmarker <- input$marker
    tempresult <- input$resultcol
    tempcov <- input$cov
  })
  
  
  output$aroc_density <- renderPlot({
    aroc_density_builder(loadedData(), tempmarker, tempresult, tempcov)
  })
})

