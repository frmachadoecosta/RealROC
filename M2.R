library('shiny')


roccurve <- eventReactive(input$gene_classic, {
  isolate(
  try({
    do_classicroc(
    loadedData(),
    input$marker2,
    input$resultcol2,
    as.integer(input$healthy_pop2),
    as.integer(input$disease_pop2),
    input$classicurve_type
  )}))
})



classicrep <- eventReactive(roccurve(), {
  isolate(
    try({
      classicsummary(
      roccurve(),
      input$classicurve_type,
      input$marker2,
      input$resultcol2)
      }
    )
  )
})


observeEvent(classicrep(), {
  if (class(classicrep())!="try-error"){
    textobj(paste0(textobj(), classicrep()))
  }

})


output$roccurve <- renderPlot({
  plot(roccurve())
})




observeEvent(input$gene_classic, {
  isolate({
    #makes only respond to Action Button
    tempmarker <- input$marker2
    tempresult <- input$resultcol2
  })
  
  output$classic_density <- renderPlot({
    density_builder(loadedData(), tempmarker, tempresult)
  })
})