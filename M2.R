library('shiny')



observeEvent(input$gene_classic, {
  try({
    roccurve <- do_classicroc(
      loadedData(),
      input$marker2,
      input$resultcol2,
      as.integer(input$healthy_pop2),
      as.integer(input$disease_pop2),
      input$classicurve_type
    )
    
    classicrep <-
      classicsummary(roccurve,
                     input$classicurve_type2,
                     input$marker2,
                     input$resultcol2)
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
    tempmarker <- input$marker2
    tempresult <- input$resultcol2
  })
  
  output$classic_density <- renderPlot({
    density_builder(loadedData(), tempmarker, tempresult)
  })
})