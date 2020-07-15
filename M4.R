library('shiny')

#Comp Module
curveboot <- reactiveVal(0)

AROCobj <- eventReactive(input$compOnAROC, {
  validate(need(input$comptype == 'AROC',''))
  isolate(
    try(
      if (input$comptype == 'AROC') {
      gene_aroc_analysis(
        loadedData(),
        input$marker4,
        input$resultcol4,
        input$cov4,
        as.integer(input$healthy_pop4),
        aroc_type = 'Semiparametric'
      )
    }))
})

polROCobj <- eventReactive(input$compOnAROC,{
  validate(need(input$comptype == 'AROC',''))
  isolate(
    try(
    if (input$comptype == 'AROC') {
      autopooled(
        loadedData(),
        input$marker4,
        input$resultcol4,
        as.integer(input$healthy_pop4),
        as.integer(input$disease_pop4),
        type = 'Pooled Empirical'
      )
    }))
})


comp2rocobj <- eventReactive(input$compOnAROC,{
  validate(need(input$comptype == 'Comp2ROC',''))
  isolate(
    try({
    if (input$comptype == 'Comp2ROC'){
      tempvar <- loadedData()
      templist <- split(tempvar, tempvar[input$cov4])
      #print(str(templist))
      
      binary1 <- templist[[1]]
      binary1 <- binary1[, c(input$marker4, input$resultcol4)]
      
      binary2 <- templist[[2]]
      binary2 <- binary2[, c(input$marker4, input$resultcol4)]
      
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
      
      
      curveboot(roc.curves.boot(datafromfunc, 100, 0.05, name = 'CRIB_sex_ind', "CRIBM", "CRIBF", FALSE))
      
      c(sim1.curve,
        sim2.curve,
        mod1 = moda1,
        mod2 = moda2)
    
    }}))
})


AROCplot <- eventReactive(input$compOnAROC,{
  validate(need(input$compOnAROC, ''))

  comptitle <-isolate(
    paste0('ROC adjustment Comparison for ', input$cov4))
  
  compAROC_ggplot(AROCobj(), polROCobj(), comptitle, 'AROC', 'ROC')
  })


Comp2Plot <- eventReactive(input$compOnAROC,{
  validate(need(input$compOnAROC, ''))
  tempcom <- isolate(comp2rocobj())
  roc.curves.plot2(tempcom[[1]],tempcom[[2]],tempcom[[3]],tempcom[[4]])
})


output$AROCcompplot <- renderPlot({
  validate(need(input$compOnAROC,''))
  isolate({
    if (input$comptype == 'AROC'){
      AROCplot()
      
      
    } else{
      Comp2Plot()
    }
  })
  
})


#reports

toreportcomp <- eventReactive(AROCobj(),{
  if (input$comptype == 'AROC'){
    try(summaryCompAroc(AROCobj(), polROCobj(), input$cov4))
  }
})

toreportcomp2roc <-eventReactive(curveboot(), {
  if (input$comptype == 'Comp2ROC') {
    
    tempvar <- loadedData()
    templist <- split(tempvar, tempvar[input$cov4])
    name1 <- names(templist)[1]
    name2 <- names(templist)[2]
    
    
    summaryCOmp2ROC(curveboot(), name1, name2)
  }
})


observeEvent(toreportcomp2roc(),{
  if (class(toreportcomp2roc())!="try-error"){
  textobj(paste0(textobj(), toreportcomp2roc()))
    }
  
})


observeEvent(toreportcomp(),{
  if (class(toreportcomp())!="try-error"){
      textobj(paste0(textobj(), toreportcomp()))
    }
  
})



