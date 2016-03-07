shinyUI(pageWithSidebar(
  headerPanel('K-Means Clustering'),
  sidebarPanel(
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 ','),
    
    tags$hr(),
    uiOutput("choose_columns"),
    numericInput('clusters_num', 'Number of Clusters', 3,
                 min = 2, max = NA),
    h6('(Must be numeric variables; at Least 3 for visualization; records conatining missing values of selected variables will be deleted in the final data set)'),
    actionButton("doCluster", "Run Clusters! :D"),
    
    tags$hr(),
    uiOutput("choose_clusters"),
    tags$hr(),
   
    uiOutput("choose_profVar")
  ),
  mainPanel(
    plotOutput(outputId = "radar", height = "600px"),
    tags$hr(),
    tableOutput("clutser_result"), 
    tags$hr(),
    tableOutput("profile_result"), 
    h6("Source code can be found at Github: https://github.com/xxiang13"),
    h6("If you have questions, please contact xxiang13@gmail.com")
  )
))