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
              tabPanel('Table', dataTableOutput('filetable'))
              )
            ))
          ),
    
    tabPanel(title='2. Classic ROC',
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput('classicurve_type','Select Curve Type', 
                             choices=c('Empirical', 'Pooled Empirical', 'Pooled Bayesian')
                             ),
                 
                 checkboxInput('smoother', 'Smooth Curve'),
                 
                 uiOutput("ROCcondicionals"),
                 
                 actionButton('gene_classic',"Plot Distributions")
                 
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel('Curve Plot', plotOutput('roccurve')),
                   tabPanel('Population Distribution')
                 )
               )
             )
             ),
    
    tabPanel(title='3. AROC'
             ),
    
    tabPanel(title='4. Comp2ROC'
             ),
    
    tabPanel(title='5. Report'
             ),
    
    tabPanel(title='Advance'
             ),
    
    tabPanel(title='Help'
             )
))
