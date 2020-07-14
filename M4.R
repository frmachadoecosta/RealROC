library('shiny')

#Comp Module


observeEvent(input$compOnAROC, {
  if (input$comptype == 'AROC') {
    try({
      AROCobj <- gene_aroc_analysis(
        loadedData(),
        input$marker4,
        input$resultcol4,
        input$cov4,
        as.integer(input$healthy_pop4),
        aroc_type = 'Semiparametric'
      )
      
      
      polROCobj <- autopooled(
        loadedData(),
        input$marker4,
        input$resultcol4,
        as.integer(input$healthy_pop4),
        as.integer(input$disease_pop4),
        type = 'Pooled Empirical'
      )
      
      #report
      toreportcomp <-  summaryCompAroc(AROCobj, polROCobj, input$cov4)
      for (line in toreportcomp) {
        textobj(newreport(textobj, tags$div(line)))
      }
      
    })
    comptitle <-
      paste0('ROC adjustment Comparison for ', input$cov4)
    output$AROCcompplot <-
      renderPlot({
        loadfunc()
        compAROC_ggplot(AROCobj, polROCobj, comptitle, 'AROC', 'ROC')
      })
    
  } else {
    try({
      tempvar <- loadedData()
      templist <- split(tempvar, tempvar[input$cov4])
      print(str(templist))
      
      binary1 <- templist[[1]]
      binary1 <- binary1[, c(input$marker4, input$resultcol4)]
      
      binary2 <- templist[[2]]
      binary2 <- binary2[, c(input$marker4, input$resultcol4)]
      
      
      
      #print(head(binary1))
      #print(head(binary2))
      
      datafromfunc <-
        comp_converter(binary1,
                       binary2,
                       input$resultcol4,
                       input$resultcol4,
                       FALSE)
      
      name1 <- names(templist)[1]
      name2 <- names(templist)[2]
      
      moda1 = paste0(input$cov4, '-', name1)
      moda2 = paste0(input$cov4, '-', name2)
      
      sim1.ind = unlist(datafromfunc[1])
      sim2.ind = unlist(datafromfunc[2])
      sim1.sta = unlist(datafromfunc[3])
      sim2.sta = unlist(datafromfunc[4])
      
      sim1.pred = prediction(sim1.ind, sim1.sta)
      sim2.pred = prediction(sim2.ind, sim2.sta)
      sim1.curve = performance(sim1.pred, "tpr", "fpr")
      sim2.curve = performance(sim2.pred, "tpr", "fpr")
      
      roc.curves.plot(sim1.curve,
                      sim2.curve,
                      mod1 = moda1,
                      mod2 = moda2)
      res <-
        roc.curves.boot(datafromfunc, 100, 0.05, name = 'CRIB_sex_ind', "CRIBM", "CRIBF", FALSE)
      
      toreportcomp <-  summaryCOmp2ROC(res, name1, name2)
      for (line in toreportcomp) {
        textobj(newreport(textobj, tags$div(line)))
      }
      
    })
    
    
    output$AROCcompplot <-
      
      renderPlot({
        loadfunc()
        roc.curves.plot(sim1.curve,
                        sim2.curve,
                        mod1 = moda1,
                        mod2 = moda2)
      })
    
  }
  
  
  
})

