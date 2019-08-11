shinyUI(fluidPage(theme = shinytheme("flatly"),
                  
    # Application title
    titlePanel("RealROC"),
    
    # Sidebar  
    sidebarLayout(
      sidebarPanel(
        
        fileInput('main', 'Input file', multiple = FALSE,
                  accept =".csv"),
        
        checkboxInput("header", "Header", TRUE),
        
        
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),
        
        
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"')
      ),
  
      
      # Main Panel
      mainPanel(
        tabsetPanel(
          tabPanel('Datatable', dataTableOutput('filetable'))
        )
      )
    )
))
