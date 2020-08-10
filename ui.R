library('shinythemes')
library('dplyr')
library(shinycssloaders)

options(shiny.sanitize.errors = TRUE)

shinyUI(
  tagList(
    tags$head(tags$style(HTML("
                           .navbar-nav {
                           float: none !important;
                           }
                           .navbar-nav > li:nth-child(7) { 
                           float: right;
                           right: 0px;

                           }
                           .navbar-nav > li:nth-child(6) {
                           float: right;
                           right: 150px;
                           }
                           "))),
  navbarPage(
    'RealROC',
    theme = shinytheme("flatly"),

    tabPanel(title = '1. Import Data',
             # Sidebar

             style = 'overflow:auto;
             max-height: 90vh;
             overflow-x:hidden;
             ',
             
             sidebarLayout(
               sidebarPanel(
                 fileInput('main', '', multiple = FALSE, buttonLabel = "Choose File",
                           accept = c(".csv", ".xls", ".xlsx")),
                 
                 h4('.csv options'),
                 checkboxInput("header", "Header", TRUE),
                 
                 
                 radioButtons(
                   "sep",
                   "Separator",
                   choices = c(
                     Comma = ",",
                     Semicolon = ";",
                     Tab = "\t"
                   ),
                   selected = ","
                 ),
                 
                 
                 radioButtons(
                   "quote",
                   "Quote",
                   choices = c(
                     None = "",
                     "Double Quote" = '"',
                     "Single Quote" = "'"
                   ),
                   selected = '"'
                 ),
                 #HTML('<br>'),
                 #h4('.xls/.xlsx options'),
                 
                 #numericInput(
                #   'sheetId',
                #   'Sheet',
                #   value = 1,
                #   min = 1,
                #   width = '50%'
                # ),
                 
                 HTML('<br>'),
                 h4('Try app with no data import'),
                 actionButton('sampledata', 'Use sample data'),
               ),
               
               
               # Main Panel
               mainPanel(
                 #p(
                #   HTML(
                #     "<br><A HREF=\"javascript:history.go(0)\">Reset all inputs</A>"
                #   ),
                #  style = "position:absolute;top: 0px;right:1em"
                # ),
                 tabsetPanel(
                   tabPanel('Table', dataTableOutput('filetable'))
                   )
                 )
             )),
    
    tabPanel(title = '2. Classic ROC',
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   'classicurve_type',
                   'Select Curve Type',
                   choices = c(
                     'Empirical',
                     'Empirical Smooth',
                     'Pooled Empirical',
                     'Pooled Bayesian'
                   )
                 ),
                 
                 #checkboxInput('smoother', 'Smooth Curve'),
                 
                 roccondiButtons("counter1", "ROCParam Classic"),
                 
                 actionButton('gene_classic', "Plot ROC")
                 
               ),
               mainPanel(
                 style ="width: 70vh;",
                 
                 tabsetPanel(
                   tabPanel('Curve Plot', plotOutput('roccurve')%>% 
                              withSpinner(color = 'lightseagreen',
                                          type = getOption("spinner.type", default = 5))),
                   
                   tabPanel('Population Distribution', plotOutput('classic_density'))
               ))
             )),
    
    tabPanel(title = '3. AROC',
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   'aroc_type',
                   'Curve Type',
                   choices = c(
                     'Semiparametric',
                     'Nonparametric Bayesian',
                     'Semiparametric Bayesian'
                   )
                 ),
                 
                 roccondiButtons("counter2", "ROCParam Cov"),
                 
                 covselButtons("c33", "AROC cov"),
                 
                 
                 actionButton('gene_aroc', 'Plot AROC')
               ),
               mainPanel(
                 style ="width: 70vh;",
                 tabsetPanel(
                 tabPanel('AROC Curve', plotOutput('aroc')%>% 
                            withSpinner(color = 'lightseagreen',
                                        type = getOption("spinner.type", default = 5))),
                 tabPanel('Population Distribution', plotOutput('aroc_density'))
               ))
             )),
    
    tabPanel(title = '4. Comparison',
             sidebarLayout(
               sidebarPanel(
                 radioButtons('comptype', 'Select Comparison', choices = c('AROC', 'Comp2ROC')),
                 tagList(
                   roccondiButtons("counter3", "ROCParam Classic"),
                   covselButtons("c44", "Comp cov"),
                   actionButton('compOnAROC', 'See Comparison')
                 )
               ),
               mainPanel(
                 style ="width: 70vh;",
                 plotOutput('AROCcompplot') %>% withSpinner(color = 'lightseagreen',
                                                               type = getOption("spinner.type", default = 5)))
             )),
    
    tabPanel(
      title = '5. Report',
      fillPage(
        h4(
          'This section is reserved to log application usage and function reports'
        ),
        h6('All relevant statistics can be found here'),
        div('==================================='),
        htmlOutput('reporttext')
      ),
      style = 'overflow-y:scroll; max-height: 90vh'
    ),
    
    tabPanel(title = 'Advanced',
             sidebarLayout(sidebarPanel(
               uiOutput('advancedsignalchange'),
               HTML('<br>'),
               uiOutput('advancedfactor'),
               HTML('<br>'),
               uiOutput('advancedlog'),
               #HTML('<br>'),
               #actionButton('advancedNA', label = 'Remove NA values')
             ),
             mainPanel(textOutput('signalchangeoutput')))),
    
    tabPanel(
      title = 'Help',
      includeMarkdown("README.md"),
      style = 'overflow-y:scroll; max-height: 90vh'
    )
  )
)


)