shinyUI(navbarPage('RealROC', theme = shinytheme("flatly"),
                  

    tabPanel(title='1. Import Data',
             
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
            ))
          ),
    tabPanel(title='2. Classic ROC'
             )
))
