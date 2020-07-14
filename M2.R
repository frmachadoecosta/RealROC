library('shiny')

observeEvent(input$gene_classic, {
  try({
    roccurve <- do_classicroc(
      loadedData(),
      input$marker,
      input$resultcol,
      as.integer(input$healthy_pop),
      as.integer(input$disease_pop),
      input$classicurve_type
    )
    
    classicrep <-
      classicsummary(roccurve,
                     input$classicurve_type,
                     input$marker,
                     input$resultcol)
    for (line in classicrep) {
      textobj(newreport(textobj, tags$div(line)))
    }
    
  })
  
  
  output$roccurve <- renderPlot({
    loadfunc()
    plot(roccurve)
  })
  
  
})

observeEvent(input$gene_classic, {
  isolate({ #makes only responde to Action Button
    tempmarker <- input$marker
    tempresult <- input$resultcol
  })
  
  output$classic_density <- renderPlot({
    density_builder(loadedData(), tempmarker, tempresult)
  })
})