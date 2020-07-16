library('npROCRegression')
library('Comp2ROC')
library('pROC')
library('readxl')
library("tools")

shinyServer(function(input, output, session) {
  loadedData <- reactiveVal()
  
  listentoDataInputs <-
    reactive({
      list(input$main, input$sampledata, loadedData())
    })
  
  textobj <- reactiveVal('')
  output$reporttext <- renderText(textobj())
  
  # - Import Data Module
  source('M1.R', local = T)
  
  # - Classic ROC Module
  source('M2.R', local = T)
  
  # - AROC Module
  source('M3.R', local = T)
  
  # - Comparison Module
  source('M4.R', local = T)
  
  # - Avanced Module
  source('Advanced.R', local = T)
  
  observeEvent(listentoDataInputs(), {
    callModule(roccondi, "counter1", loadedData(), n = 2)
    callModule(roccondi, "counter2", loadedData(), n = 3)
    callModule(roccondi, "counter3", loadedData(), n = 4)
    callModule(covselect, "c33", loadedData(), n = 3)
    callModule(covselect, "c44", loadedData(), n = 4)
  })
  
  
  
  listentoMarker <-
    reactive({
      list(input$marker2, input$marker3, input$marker4)
    })
  listentoResultcol <-
    reactive({
      list(input$resultcol2, input$resultcol3, input$resultcol4)
    })
  listentoHealthypop <-
    reactive({
      list(input$healthy_pop2,
           input$healthy_pop3,
           input$healthy_pop4)
    })
  listentoDiseasepop <-
    reactive({
      list(input$disease_pop2,
           input$disease_pop3,
           input$disease_pop4)
    })
  
  listentoUpdateinputs <- reactive({
    list(
      listentoMarker(),
      listentoResultcol(),
      listentoHealthypop(),
      listentoDiseasepop()
    )
  })
  
  
  
  
  
  observeEvent(listentoUpdateinputs(), {
    if (is.not.unique(listentoMarker())) {
      myneeds <- seedifferent(
        session,
        c('input$marker2', 'input$marker3', 'input$marker4'),
        c(input$marker2, input$marker3, input$marker4)
      )
      try(updateMyReactive(session, myneeds[[1]], myneeds[[2]]))
      
    }
    
    else if (is.not.unique(listentoResultcol())) {
      myneeds <- seedifferent(
        session,
        c(
          'input$resultcol2',
          'input$resultcol3',
          'input$resultcol4'
        ),
        c(input$resultcol2, input$resultcol3, input$resultcol4)
      )
      try(updateMyReactive(session, myneeds[[1]], myneeds[[2]]))
      
    }
    else if (is.not.unique(listentoHealthypop())) {
      myneeds <- seedifferent(
        session,
        c(
          'input$healthy_pop2',
          'input$healthy_pop3',
          'input$healthy_pop4'
        ),
        c(
          input$healthy_pop2,
          input$healthy_pop3,
          input$healthy_pop4
        )
      )
      try(updateMyReactive(session, myneeds[[1]], myneeds[[2]]))
      
    }
    else if (is.not.unique(listentoDiseasepop())) {
      myneeds <- seedifferent(
        session,
        c(
          'input$disease_pop2',
          'input$disease_pop3',
          'input$disease_pop4'
        ),
        c(
          input$disease_pop2,
          input$disease_pop3,
          input$disease_pop4
        )
      )
      try(updateMyReactive(session, myneeds[[1]], myneeds[[2]]))
      
    }
    
    
  })
  
  observeEvent(input$cov3, {
    updateNumericInput(session, "cov4", value = input$cov3)
  })
  
  observeEvent(input$cov4, {
    updateNumericInput(session, "cov3", value = input$cov4)
    
  })
  
  
  
  
})


