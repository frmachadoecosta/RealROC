library('shiny')

#AROC Module


observeEvent(input$gene_aroc, {
  try({
    aroc_curve <-
      gene_aroc_analysis(
        loadedData(),
        input$marker3,
        input$resultcol3,
        input$cov3,
        as.integer(input$healthy_pop3),
        input$aroc_type
      )
    
    toreport <-
      propersummary(aroc_curve,
                    input$marker3,
                    input$resultcol3,
                    input$healthy_pop3,
                    input$cov3)
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
    tempmarker <- input$marker3
    tempresult <- input$resultcol3
    tempcov <- input$cov3
  })
  
  
  output$aroc_density <- renderPlot({
    aroc_density_builder(loadedData(), tempmarker, tempresult, tempcov)
  })
})

