########################################################
# author: Xiang Li (xxiang13@gmail.com)                #
# March 7 2016                                         #
# Code Reference:                                      #
# http://shiny.rstudio.com/gallery/kmeans-example.html #
# http://shiny.rstudio.com/gallery/file-upload.html    #
########################################################

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
shiny.maxRequestSize=100*1024^2
shinyServer(function(input, output) {
  
  #### input data ####  
  inputFile <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    # If missing input, return to avoid error later in function
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data = read.csv(inFile$datapath, header=TRUE, sep=input$sep, 
                    quote=input$quote)
    #na.omit(data)
  })
  
  #### Check boxes #### 
  # to select kmean variables
  output$choose_columns <- renderUI({
    data = inputFile()
    
    if (is.null(data))
      return(NULL)
    
    colnames <- names(data)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("kmeans_var", label = "Choose Variables to Cluster", 
                       choices  = colnames,
                       selected = colnames)

  })
  
  #### delete records containing missing value of selected kmeans variables ####
  FinalData <- reactive({
    # Take a dependency on input$goButton
    if (input$doCluster == 0) return(NULL)
    input$doCluster
    
    data=inputFile()
    if (is.null(data)) return(NULL)
    
    # Make sure columns are correct for data set (when data set changes, the
    # columns will initially be for the previous data set)
    if (is.null(input$kmeans_var) || !(input$kmeans_var %in% names(data)))
      return()
    
    # Use isolate() to avoid dependency on input$obs
    isolate({
      data=inputFile()
      kmeans_var=input$kmeans_var
      # delete records having missing value of the selected columns
      data <- data[which(complete.cases(data[,kmeans_var])),]
    })
  })
  
  
  #### select kmeans variables ####
  clustData <- reactive({
    # Take a dependency on input$goButton
    if (input$doCluster == 0) return(NULL)
    input$doCluster
    
    data=FinalData()
    if (is.null(data)) return(NULL)
    
    # Make sure columns are correct for data set (when data set changes, the
    # columns will initially be for the previous data set)
    if (is.null(input$kmeans_var) || !(input$kmeans_var %in% names(data)))
      return()
    
    # Use isolate() to avoid dependency on input$obs
    isolate({
      data= FinalData()
      kmeans_var=input$kmeans_var
      # Keep the selected columns
      data <- data[, kmeans_var, drop = FALSE]
    })
  })
  
  #### Standardize data ####
  ZData <- reactive({
    # Take a dependency on input$goButton
    if (input$doCluster == 0) return(NULL)
    input$doCluster
    
    if (is.null(clustData())) return(NULL)
    
        # Use isolate() to avoid dependency on input$obs
    isolate({
      # Standardize data to Z-scores
      data.frame(lapply(clustData(), scale, scale=T))
    })
  })
  
  
  #### Kmean Clustering ####
  clusters <- reactive({
    if (input$doCluster == 0) return(NULL)
    input$doCluster
    
    if (is.null(ZData())) return(NULL)
    
    isolate({
    set.seed(123)
    kmeans(ZData(), input$clusters_num,nstart=100)
    })
  })
  
  #### Check Box ####
  # to choose clusters to view
  output$choose_clusters <- renderUI({

    if (is.null(clusters()))
      return(NULL)
    
    checkboxGroupInput("cluster_num", "Choose Clusters to Visualize", 
                       choices  = seq(1,nrow(clusters()$centers),step = 1),
                       selected = seq(1,nrow(clusters()$centers),step = 1))

  })
  
  #### radar chart ####
  output$radar <- renderPlot({
    
    if (is.null(clusters()))
      return(NULL)
    
    if (is.null(input$cluster_num))
      return(NULL)
    
    source('radar_chart.R')
    radart.chart(clusters(),input$cluster_num)
  })
  
  #### summarize clusters on kmeans vars ####
  output$clutser_result <- renderTable({
    if (is.null(clusters()))
      return(NULL)
    
    if (is.null(clustData()))
      return(NULL)
    
    source('summary_clusters_v2.R')
    summary.clusters(clustData(),clusters(),input$kmeans_var)},
    caption=paste("Clusters Summary"),
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL)
    )
  
  #### Dropdown List #### 
  # to select profile variables
  output$choose_profVar <- renderUI({
    data = FinalData()
    
    if (is.null(data))
      return(NULL)
    
    colnames <- names(data)
    
    # Create the checkboxes and select NULL by default
    selectInput("var_profile", label = "Variable to Profile", 
                choices  = colnames,
                selected = tail(colnames,n = 1))
    
  })
  
  #### profile clusters on kmeans vars ####
  output$profile_result <- renderTable({
    if (is.null(clusters()))
      return(NULL)
    
    if (is.null(FinalData()))
      return(NULL)
    
    source('profile_clusters_v2.R')
    profile.clusters.v2(clusters(),input$var_profile,FinalData())},
    caption=paste("Profiling Result"),
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL)
)
  
  
})