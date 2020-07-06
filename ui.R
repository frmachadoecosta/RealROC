library('shinythemes')
library('dplyr')

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
                         selected = '"'),
          
            actionButton('sampledata', 'Use sample data')
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
                             choices=c('Empirical','Empirical Smooth', 'Pooled Empirical', 'Pooled Bayesian')
                             ),
                 
                 #checkboxInput('smoother', 'Smooth Curve'),
                 
                 roccondiButtons("counter1", "ROCParam Classic"),
                 
                 actionButton('gene_classic',"Plot ROC")
                 
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel('Curve Plot',plotOutput('roccurve')),
                   tabPanel('Population Distribution',plotOutput('classic_density'))
                 )
               )
             )
             ),
    
    tabPanel(title='3. AROC',
             sidebarLayout(
               sidebarPanel(
                 selectInput('aroc_type','Curve Type', 
                             choices=c('Semiparametric', 
                                       'Nonparametric Bayesian', 
                                       'Semiparametric Bayesian')),
                 
                 roccondiButtons("counter2", "ROCParam Cov"),
                 
                 uiOutput('AROCcovariate'),
                 
                 actionButton('gene_aroc', 'Plot AROC')
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel('AROC Curve', plotOutput('aroc')),
                 tabPanel('Population Distribution',plotOutput('aroc_density'))
               )
              )
             )
             ),
    
    tabPanel(title='4. Comparison',
             sidebarLayout(
               sidebarPanel(
               radioButtons('comptype','Select Comparison', choices = c('AROC','Comp2ROC')),
               tagList(
               roccondiButtons("counter3", "ROCParam Classic"),
               uiOutput('AROCcovariatecomp'),
               actionButton('compOnAROC','See Comparison'))
               ),
             mainPanel(
               plotOutput('AROCcompplot')
             )
             )
             
             ),
    
    tabPanel(title='5. Report',
             fillPage(
               h4('This section is reserved to log application usage and function reports'),
               h6('All relevant statistics can be found here'),
               htmlOutput('reporttext')
             )),
    
    tabPanel(title='Advanced',
             sidebarLayout(
               sidebarPanel(
                 textInput('seed', 'Input Seed'),
                 uiOutput('advancedsignalchange')
               ),
               mainPanel()
             )
             ),
    
    tabPanel(title='Help',
             includeMarkdown("README.md"), 
             style = 'overflow-y:scroll; max-height: 90vh'
             )
))
