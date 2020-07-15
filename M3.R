library('shiny')

#AROC Module

aroc_curve <- eventReactive(input$gene_aroc, {
  try(
  gene_aroc_analysis(
    loadedData(),
    input$marker3,
    input$resultcol3,
    input$cov3,
    as.integer(input$healthy_pop3),
    input$aroc_type
  )
  )
})

toreport <- eventReactive(aroc_curve(),{
  try(
  propersummary(aroc_curve(),
                input$marker3,
                input$resultcol3,
                input$healthy_pop3,
                input$cov3))
})

observeEvent(toreport(),{
  if (class(toreport())!="try-error"){
    
  textobj(paste0(textobj(),toreport()))
    }
})

output$aroc <- renderPlot({plot(aroc_curve())})


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

