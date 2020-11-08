OkTargetS <- OkWeightsS <- OkfileS <- costMatS <- costMatProxyS <- TarLevelsS <- T
OkTargetT <- OkWeightsT <- OkfileT <- costMatT <- costMatProxyT <- TarLevelsT <- T
OkTargetI <- OkWeightsI <- OkfileI <- costMatI <- costMatProxyI <- ambMatI <- ambMatProxyI <- TarLevelsI <- T

server <- function( input, output, session ) {
  
  #### Input files ####
  input_fileS <- reactive({
    
    req( input$fileS )
    
    OkfileS <<- T
    
    if( length( grep("arff",input$fileS$name) ) ){
      
      dfS <- read.arff( input$fileS$datapath )
      
    }else{
      
      dfS <- read.table( input$fileS$datapath, header = input$headerS, sep = input$sepS, quote = input$quoteS, check.names = F )
      
    }
    
    if( any( sapply( dfS, anyNA ) ) ){
      
      OkfileS <<- F
      dfS <- data.frame( " " = "*** Missing values are not allowed ***", check.names = F )

    }
    
    return( dfS )
    
  })
  
  input_fileT <- reactive({
    
    req( input$fileT )
    
    if( length( grep("arff",input$fileT$name) ) ){
      
      dfT <- read.arff( input$fileT$datapath )
      
    }else{
      
      dfT <- read.table( input$fileT$datapath, header = input$headerT, sep = input$sepT, quote = input$quoteT, check.names = F )
      
    }
    
    if( any( sapply( dfT, anyNA ) ) ){
      
      dfT <- data.frame( " " = "*** Missing values are not allowed ***", check.names = F )
      
    }
    
    return( dfT )
    
  })
  
  input_fileI <- reactive({
    
    req( input$fileI )
    
    if( length( grep("arff",input$fileI$name) ) ){
      
      dfI <- read.arff( input$fileI$datapath )
      
    }else{
      
      dfI <- read.table( input$fileI$datapath, header = input$headerI, sep = input$sepI, quote = input$quoteI, check.names = F )
      
    }
    if( any( sapply( dfI, anyNA ) ) ){
      
      dfI <- data.frame( " " = "*** Missing values are not allowed ***", check.names = F )
      
    }
    
    return( dfI )
    
  })
  
  input_fileP <- reactive({
    
    req( input$fileP )
    
    if( length( grep("arff",input$fileP$name) ) ){
      
      dfP <- read.arff( input$fileP$datapath )
      
    }else{
      
      dfP <- read.table( input$fileP$datapath, header = input$headerP, sep = input$sepP, quote = input$quoteP, check.names = F )
      
    }
    if( any( sapply( dfP, anyNA ) ) ){
      
      dfP <- data.frame( " " = "*** Missing values are not allowed ***", check.names = F )
      
    }
    
    return( dfP )
    
  })
  
  #### Input tree Predict ####
  input_treeP <- reactive({
    
    req( input$treeP )
    
    treeP <- readRDS( input$treeP$datapath )
    
    return( treeP )
    
  })
  
  #### Dynamic parameters Static ####
  output$paramsS <- renderUI({
    
    if( input$typeS %in% c("Renyi", "Tsallis") ){
      
      numericInput("qS", "Q", value = 1, -10, 10, 0.01)
      
    }else if( input$typeS %in% c("Sharma-Mittal", "Sharma-Taneja", "Kapur") ){
      
      tabPanel("AlphaBetaS", 
               
               numericInput("aS", "Alpha", value = 1, -10, 10, 0.01),
               numericInput("bS", "Beta", value = 1, -10, 10, 0.01)
               
      )
      
    }
    
  })
  
  observe({

    if( input$weightscostS %in% c("weights") ){

      shinyjs::show("AreaWeightsS")

    }else{

      shinyjs::hide("AreaWeightsS")

    }

  })
  
  output$overfittingS <- renderUI({
    
    if( input$overfitS!= "prune" ){
      
      numericInput("cpS", "Cp", value = 0, 0, 1, 0.01)
      
    }else{
      
      numericInput("cfS", "Confidence intervals", value = 0.25, 0.01, 0.5, 0.01)
      
    }
    
  })
  #### Dynamic parameters Tune ####
  observe({
    
    if( input$weightscostT %in% c("weights") ){
      
      shinyjs::show("AreaWeightsT")
      
    }else{
      
      shinyjs::hide("AreaWeightsT")
      
    }
    
  })
  
  #### Dynamic parameters Interactive ####
  output$paramsI <- renderUI({
    
    if( input$typeI %in% c("Renyi", "Tsallis") ){
      
      numericInput("qI", "Q", value = 1, -10, 10, 0.01)
      
    }else if( input$typeI %in% c("Sharma-Mittal", "Sharma-Taneja", "Kapur") ){
      
      tabPanel("AlphaBetaI", 
               
               numericInput("aI", "Alpha", value = 1, -10, 10, 0.01),
               numericInput("bI", "Beta", value = 1, -10, 10, 0.01)
               
      )
      
    }
    
  })
  
  output$amb_typeI <- renderUI({
    
    if( input$amb_decI == "Probability" ){
      
      numericInput("amb_probI", "Ambiguous probability", value = 1, 0, 1, 0.01)
      
    }
    
  })
  
  observe({
    
    if( input$weightscostI %in% c("weights") ){
      
      shinyjs::show("AreaWeightsI")
      
    }else{
      
      shinyjs::hide("AreaWeightsI")
      
    }
    
  })
  
  output$overfittingI <- renderUI({
    
    if( input$overfitI != "prune" ){
      
      numericInput("cpI", "Cp", value = 0, 0, 1, 0.01)
      
    }else{
      
      numericInput("cfI", "Confidence intervals", value = 0.25, 0.01, 0.5, 0.01)
      
    }
    
  })
  
  #### Dynamic names ####
  observeEvent( input_fileS(), ignoreInit = T, { 
    
    input_namesS <- colnames( input_fileS() )

    updateSelectInput( session, "Y_nameS", choices = input_namesS, selected = "" )
    updateSelectInput( session, "X_namesS", choices = input_namesS )
    updateSelectInput( session, "WeightsS", choices = input_namesS, selected = "" )
    
  })
  
  observe({
    
    input_namesT <- colnames( input_fileT() )
    
    n_row <- nrow(input_fileT())
    
    updateSelectInput( session, "Y_nameT", choices = input_namesT, selected = "" )
    updateSelectInput( session, "X_namesT", choices = input_namesT )
    updateSelectInput( session, "WeightsT", choices = input_namesT, selected = "" )
    
    updateSliderInput( session, "min_obsT", min = 1, max = n_row, value = c(floor(n_row*0.05),floor(n_row*0.05)), step = 1)
    updateSliderInput( session, "qT", step = input$stepT)
    updateSliderInput( session, "aT", step = input$stepT)
    updateSliderInput( session, "bT", step = input$stepT)
    updateSliderInput( session, "cpT", step = input$stepcpT)
    updateSliderInput( session, "cfT", step = input$stepcfT)
    
  })
  
  observe({
    
    input_namesI <- colnames( input_fileI() )
    
    updateSelectInput( session, "Y_nameI", choices = input_namesI, selected = "" )
    updateSelectInput( session, "X_namesI", choices = input_namesI )
    updateSelectInput( session, "WeightsI", choices = input_namesI, selected = "" )
    
  })
  
  #### Files render ####
  output$InputFileS <- renderTable( input_fileS() )
  output$InputFileT <- renderTable( input_fileT() )
  output$InputFileI <- renderTable( input_fileI() )
  output$InputFileP <- renderTable( input_fileP() )

  output$plotP <- renderPrint( PrintTree( input_treeP() ) )
  
  entropy_parS <- reactive( if( input$typeS == "Shannon" ){ 1 }else if( input$typeS %in% c("Renyi","Tsallis") ){ input$qS }else{ c( input$aS, input$bS ) } )
  entropy_parI <- reactive( if( input$typeI == "Shannon" ){ 1 }else if( input$typeI %in% c("Renyi","Tsallis") ){ input$qI }else{ c( input$aI, input$bI ) } )
  
  #### Static Model ####
  observeEvent( input$WeightsS, ignoreInit = T, {
   
    if( OkfileS == T & input$WeightsS != "" ){
      
      if( !is.numeric( input_fileS()[,input$WeightsS] ) ){
        
        OkWeightsS <<- F
        output$wrongWeightsS <- renderText("Weights should not be a factor")
        
      }else if( any( input_fileS()[,input$WeightsS] < 1 ) ){
        
        OkWeightsS <<- F
        output$wrongWeightsS <- renderText("Weights should not be less than 1")
        
      }else{
        
        OkWeightsS <<- T
        output$wrongWeightsS <- renderText("")
        
      }
      
    } 

  })

  observeEvent( input$Y_nameS, ignoreInit = T, {
    
    req( OkfileS == T & input$Y_nameS != "" )

    if( is.numeric( input_fileS()[,input$Y_nameS] ) ){
      
      OkTargetS <<- F
      output$wrongTargetS <- renderText( "Target should be a factor" )
      
    }else{
      
      OkTargetS <<- T
      output$wrongTargetS <- renderText("")
      
    }
    
  })

  observeEvent( input$Y_nameS, ignoreInit = T, {  

    req( input$Y_nameS )
    
    TarLevelsS <<- levels( input_fileS()[,input$Y_nameS] )
    costMatS <<- 1 - diag( length( TarLevelsS ) )
    dimnames(costMatS) <<- list( TarLevelsS, TarLevelsS )

    output$CostMatS <- renderDT(costMatS, selection = 'none', server = F, editable = "cell",
                                options = list(dom = 't', pageLength = 500) )

    costMatProxyS <<- dataTableProxy("costMatS")

  })
  
  observeEvent( input$CostMatS_cell_edit,{
    
    info <- input$CostMatS_cell_edit 

    if( info$row != info$col & if( is.na(as.numeric(info$value)) ){ F }else{ as.numeric(info$value) > 0} ){

      costMatS <<- editData( costMatS, info )
      replaceData( costMatProxyS, costMatS, resetPaging = FALSE )
      dimnames( costMatS ) <<- list( TarLevelsS, TarLevelsS )
      
    }else{
      
      output$CostMatS <- renderDT( costMatS, selection = "none", server = F, editable = "cell",
                                   options = list(dom = "t", pageLength = 500) )
      
    }

  })
  
  observeEvent( input$weightscostS, {
    
    if( input$weightscostS == "cost matrix") {
      
      showTab( inputId = "tabsestS", target = "Cost matrix" )
      
    }else{
      
      hideTab( inputId = "tabsestS", target = "Cost matrix" )
      
    }
    
  })
  
  observeEvent( input$startS, {

    shinyjs::hide("AreaSaveS")
    shinyjs::hide("AreaRulesS")

    req( input$Y_nameS, input$X_namesS, OkTargetS == T,
         (OkWeightsS == T & input$weightscostS == "weights" & input$class_thS == "equal") |
         (input$weightscostS == "none" & input$class_thS == "equal") | 
             (input$weightscostS == "cost matrix") 
       )

    TreeS <- ImbTreeEntropy( Y_name = input$Y_nameS, X_names = input$X_namesS, data = input_fileS(), 
                             type = input$typeS, depth = input$depthS, min_obs = input$min_obsS, 
                             entropy_par = entropy_parS(), cp = input$cpS, n_cores = 1, 
                             weights = if( input$WeightsS == "" | !input$weightscostS == "weights" ){ NULL }else{ input_fileS()[,input$WeightsS] }, 
                             cost = if( input$weightscostS == "cost matrix" ){ costMatS }else{ NULL },
                             class_th = input$class_thS, overfit = input$overfitS, 
                             cf = ifelse( is.null(input$cfS), 0.25, input$cfS ) )

    shinyjs::show("AreaSaveS")
    shinyjs::show("AreaRulesS")
    
    output$plotS <- renderPrint({
        
        PrintTreeInter(TreeS)

    })
    
    output$AccuracyS <- renderPrint({
      
      req( OkfileS == T & input$Y_nameS != "" )
      
      stop_pred <- ImbTreeEntropy:::StopPredict( TreeS, input_fileS() )
      if( stop_pred[1,] != "OK" ){
        
        stop_pred

      }else{
        
        predS <- PredictTree( TreeS, input_fileS() )$Class
        tarS <- input_fileS()[,input$Y_nameS]
        cat( paste0( capture.output( confusionMatrix( tarS, predS ) ), collapse = "\n") )
        
      }
      
    })
    
    rulesS <- ExtractRules( TreeS )
    output$RulesS <- renderPrint({
      
      req( OkfileS == T )
      
      cat( paste0( capture.output( rulesS ), collapse = "\n") )
      
    })
    
    output$saveS <- downloadHandler(
      
      filename = function(){
        paste0("Tree-", Sys.time(), ".rds")
      },
      
      content = function( file ){
        saveRDS( TreeS, file )
      }
      
    )
    
    output$saveRulesS <- downloadHandler(
      
      filename = function(){
        paste0("Rules-", Sys.time(), ".txt")
      },
      
      content = function( file ){
        write.table( rulesS, file, sep = ";", row.names = F )
      }
      
    )
    
  })
  
  #### Tune Model ####
  observeEvent( input$WeightsT, ignoreInit = T, {
    
    if( OkfileT == T & input$WeightsT != "" ){
      
      if( !is.numeric( input_fileT()[,input$WeightsT] ) ){
        
        OkWeightsT <<- F
        output$wrongWeightsT <- renderText("Weights should not be a factor")
        
      }else if( any( input_fileT()[,input$WeightsT] < 1 ) ){
        
        OkWeightsT <<- F
        output$wrongWeightsT <- renderText("Weights should not be less than 1")
        
      }else{
        
        OkWeightsT <<- T
        output$wrongWeightsT <- renderText("")
        
      }
      
    } 
    
  })
  
  observeEvent( input$Y_nameT, ignoreInit = T, {
    
    req( OkfileT == T & input$Y_nameT != "" )
    
    if( is.numeric( input_fileT()[,input$Y_nameT] ) ){
      
      OkTargetT <<- F
      output$wrongTargetT <- renderText( "Target should be a factor" )
      
    }else{
      
      OkTargetT <<- T
      output$wrongTargetT <- renderText("")
      
    }
    
  })
  
  observeEvent( input$Y_nameT, ignoreInit = T, {  
    
    req( input$Y_nameT )
    
    TarLevelsT <<- levels( input_fileT()[,input$Y_nameT] )
    costMatT <<- 1 - diag( length( TarLevelsT ) )
    dimnames( costMatT ) <<- list( TarLevelsT, TarLevelsT )
    
    output$CostMatT <- renderDT( costMatT, selection = 'none', server = F, editable = "cell",
                                 options = list(dom = 't', pageLength = 500) )
    
    costMatProxyT <<- dataTableProxy("costMatT")
    
  })
  
  observeEvent( input$CostMatT_cell_edit,{
    
    info <- input$CostMatT_cell_edit 
    
    if( info$row != info$col & if( is.na(as.numeric(info$value)) ){ F }else{ as.numeric(info$value) > 0} ){
      
      costMatT <<- editData( costMatT, info )
      replaceData( costMatProxyT, costMatT, resetPaging = FALSE )
      dimnames( costMatT ) <<- list( TarLevelsT, TarLevelsT )
      
    }else{
      
      output$CostMatT <- renderDT( costMatT, selection = "none", server = F, editable = "cell",
                                   options = list(dom = "t", pageLength = 500) )
      
    }
    
  })
  
  observeEvent( input$weightscostT, {
    
    if( input$weightscostT == "cost matrix") {
      
      showTab( inputId = "tabsestT", target = "Cost matrix" )
      
    }else{
      
      hideTab( inputId = "tabsestT", target = "Cost matrix" )
      
    }
    
  })
  
  observeEvent( input$startT, {
    
    shinyjs::hide("AreaTrainAggT")
    shinyjs::hide("AreaValidAggT")
    shinyjs::hide("AreaTrainT")
    shinyjs::hide("AreaValidT")
    
    req( input$Y_nameT, input$X_namesT, input$typeT, OkTargetT == T,
         (OkWeightsT == T & input$weightscostT == "weights" & input$class_thT == "equal") |
           (input$weightscostT == "none" & input$class_thT == "equal") | 
           (input$weightscostT == "cost matrix") 
         )
    
    Tune <- ImbTreeEntropy:::CrossValid( Y_name = input$Y_nameT, X_names = input$X_namesT, dat = input_fileT(), 
                                         type = input$typeT, 
                                         depth = seq(input$depthT[1], input$depthT[2]), 
                                         min_obs = seq(input$min_obsT[1],input$min_obsT[2]), 
                                         Qval = seq(input$qT[1],input$qT[2],input$stepT), 
                                         Alpha = seq(input$aT[1],input$aT[2],input$stepT), 
                                         Beta = seq(input$bT[1],input$bT[2],input$stepT), 
                                         weights = if( input$WeightsT == "" | !input$weightscostT == "weights" ){ NULL }else{ input_fileT()[,input$WeightsT] }, 
                                         cost = if( input$weightscostT == "cost matrix" ){ costMatT }else{ NULL },
                                         overfit = input$overfitT, 
                                         cp = seq(input$cpT[1],input$cpT[2],input$stepcpT), 
                                         cf = seq(input$cfT[1],input$cfT[2],input$stepcfT),
                                         k_fold = input$kfoldT, seed = input$seedT )
    
    shinyjs::show("AreaTrainAggT")
    shinyjs::show("AreaValidAggT")
    shinyjs::show("AreaTrainT")
    shinyjs::show("AreaValidT")
    
    output$TrainT <- renderDT({
      
      Tune$Train
      
    })
    output$TrainAggT <- renderDT({
      
      Tune$TrainAgg
      
    })
    
    output$ValidT <- renderDT({
      
      Tune$Valid
      
    })
    
    output$ValidAggT <- renderDT({
      
      Tune$ValidAgg
      
    })
    
    output$saveTune <- downloadHandler(
      
      filename = function(){
        paste0("Tune-", Sys.time(), ".txt")
      },
      
      content = function( file ){
        write.table( rulesS, file, sep = ";", row.names = F )
      }
      
    )
    
    output$saveTrainAggT <- downloadHandler(
      
      filename = function(){
        paste0("TrainAgg-", Sys.time(), ".txt")
      },
      
      content = function( file ){
        write.table( Tune$TrainAgg, file, sep = ";", row.names = F )
      }
      
    )
    
    output$saveValidAggT <- downloadHandler(
      
      filename = function(){
        paste0("ValidAgg-", Sys.time(), ".txt")
      },
      
      content = function( file ){
        write.table( Tune$ValidAgg, file, sep = ";", row.names = F )
      }
      
    )
    
    output$saveTrainT <- downloadHandler(
      
      filename = function(){
        paste0("TrainDetail-", Sys.time(), ".txt")
      },
      
      content = function( file ){
        write.table( Tune$Train, file, sep = ";", row.names = F )
      }
      
    )
    
    output$saveValidT <- downloadHandler(
      
      filename = function(){
        paste0("ValidDetail-", Sys.time(), ".txt")
      },
      
      content = function( file ){
        write.table( Tune$Valid, file, sep = ";", row.names = F )
      }
      
    )
    
  })
  
  #### Interactive Model ####
  observeEvent( input$WeightsI, ignoreInit = T, {
    
    if( OkfileI == T & input$WeightsI != "" ){
      
      if( !is.numeric( input_fileI()[,input$WeightsI] ) ){
        
        OkWeightsI <<- F
        output$wrongWeightsI <- renderText("Weights should not be a factor")
        
      }else if( any( input_fileI()[,input$WeightsI] < 1 ) ){
        
        OkWeightsI <<- F
        output$wrongWeightsI <- renderText("Weights should not be less than 1")
        
      }else{
        
        OkWeightsI <<- T
        output$wrongWeightsI <- renderText("")
        
      }
      
    } 
    
  })
  
  observeEvent( input$Y_nameI, ignoreInit = T, {
    
    req( OkfileI == T & input$Y_nameI != "" )
    
    if( is.numeric( input_fileI()[,input$Y_nameI] ) ){
      
      OkTargetI <<- F
      output$wrongTargetI <- renderText( "Target should be a factor" )
      
    }else{
      
      OkTargetI <<- T
      output$wrongTargetI <- renderText("")
      
    }
    
  })
  
  observeEvent( input$Y_nameI, ignoreInit = T, {  
    
    req( input$Y_nameI )
    
    TarLevelsI <<- levels( input_fileI()[,input$Y_nameI] )
    costMatI <<- 1 - diag( length( TarLevelsI ) )
    dimnames( costMatI ) <<- list( TarLevelsI, TarLevelsI )
    
    output$CostMatI <- renderDT( costMatI, selection = 'none', server = F, editable = "cell",
                                 options = list(dom = 't', pageLength = 500) )
    
    costMatProxyI <<- dataTableProxy("costMatI")
    
    ambMatI <<- data.frame( Class = TarLevelsI, Frequencies = 1 )
    
    output$AmbMatI <- renderDT( ambMatI, selection = 'none', server = F, editable = "cell",
                                 options = list(dom = 't', pageLength = 500) )
    
    ambMatProxyI <<- dataTableProxy("ambMatI")
    
  })
  
  observeEvent( input$CostMatI_cell_edit,{
    
    info <- input$CostMatI_cell_edit 
    
    if( info$row != info$col & if( is.na(as.numeric(info$value)) ){ F }else{ as.numeric(info$value) > 0} ){
      
      costMatI <<- editData( costMatI, info )
      replaceData( costMatProxyI, costMatI, resetPaging = FALSE )
      dimnames( costMatI ) <<- list( TarLevelsI, TarLevelsI )
      
    }else{
      
      output$CostMatI <- renderDT( costMatI, selection = "none", server = F, editable = "cell",
                                   options = list(dom = "t", pageLength = 500) )
      
    }
    
  })
  
  observeEvent( input$AmbMatI_cell_edit,{
    
    info <- input$AmbMatI_cell_edit 
    
    if( (info$value %in% TarLevelsI) | if( is.na(as.numeric(info$value)) ){ F }else{ (as.numeric(info$value) <= 1 & as.numeric(info$value) >= 0) }){
      
      ambMatI <<- editData( ambMatI, info )
      replaceData( ambMatProxyI, ambMatI, resetPaging = FALSE )
      
    }else{
      
      output$AmbMatI <- renderDT( ambMatI, selection = "none", server = F, editable = "cell",
                                   options = list(dom = "t", pageLength = 500) )
      
    }
    
  })
  
  observeEvent( input$weightscostI, {
    
    if( input$weightscostI == "cost matrix") {
      
      showTab( inputId = "tabsestI", target = "Cost matrix" )
      
    }else{
      
      hideTab( inputId = "tabsestI", target = "Cost matrix" )
      
    }
    
  })
  
  observeEvent( input$amb_decI, {
    
    if( input$amb_decI == "Class" ) {
      
      showTab( inputId = "tabsestI", target = "Ambiguity matrix" )
      
    }else{
      
      hideTab( inputId = "tabsestI", target = "Ambiguity matrix" )
      
    }
    
  })
  
  observeEvent( input$startI, {
    
    shinyjs::hide("AreaSaveI")
    shinyjs::hide("AreaRulesI")
    
    req( input$Y_nameI, input$X_namesI, OkTargetI == T,
         (OkWeightsI == T & input$weightscostI == "weights" & input$class_thI == "equal") |
           (input$weightscostI == "none" & input$class_thI == "equal") | 
           (input$weightscostI == "cost matrix") 
         )
    
    ImbTreeEntropy:::ImbTreeEntropyInterShiny1( Y_name = input$Y_nameI, X_names = input$X_namesI, data = input_fileI(), 
                                                type = input$typeI, depth = input$depthI, min_obs = input$min_obsI, 
                                                entropy_par = entropy_parI(), cp = input$cpI, n_cores = 1,
                                                weights = if( input$WeightsI == "" | !input$weightscostI == "weights" ){ NULL }else{ input_fileI()[,input$WeightsI] }, 
                                                cost = if( input$weightscostI == "cost matrix" ){ costMatI }else{ NULL },
                                                class_th = input$class_thI, overfit = input$overfitI, 
                                                cf = ifelse( is.null(input$cfI), 0.25, input$cfI ), 
                                                amb_prob = if( input$amb_decI == "Probability" ){ input$amb_probI }else{ NULL }, 
                                                var_lev = input$attr_levI, top_split = input$top_splitI, 
                                                amb_class = if( input$amb_decI == "Class" ){ ambMatI$Class }else{ NULL }, 
                                                amb_class_freq = if( input$amb_decI == "Class" ){ ambMatI$Frequencies }else{ NULL }, 
                                                shiny = list( input = input, output = output ) )
    
    shinyjs::show("decisionAreaTree")
    
    if( all( imb$PROCESS$finish == T ) ){
      
      TreeI <- ImbTreeEntropy:::ImbTreeEntropyInterShiny2()
      
      shinyjs::hide("decisionAreaTree")
      shinyjs::show("AreaSaveI")
      shinyjs::show("AreaRulesI")
      
      output$plotI <- renderPrint({
        
        PrintTreeInter(TreeI)
        
      })
      
      output$AccuracyI <- renderPrint({
        
        req( OkfileI == T & input$Y_nameI != "" )
        
        stop_pred <- ImbTreeEntropy:::StopPredict( TreeI, input_fileI() )
        if( stop_pred[1,] != "OK" ){
          
          stop_pred
          
        }else{
          
          predI <- PredictTree( TreeI, input_fileI() )$Class
          tarI <- input_fileI()[,input$Y_nameI]
          cat( paste0( capture.output( confusionMatrix( tarI, predI ) ), collapse = "\n") )
          
        }
        
      })
      
      rulesI <- ExtractRules( TreeI )
      output$RulesI <- renderPrint({
        
        req( OkfileI == T )
        
        cat( paste0( capture.output( rulesI ), collapse = "\n") )
        
      })
      
      output$saveI <- downloadHandler(
        
        filename = function(){
          paste0("Tree-", Sys.time(), ".rds")
        },
        
        content = function( file ){
          saveRDS( TreeI, file )
        }
        
      )
      
      output$saveRulesI <- downloadHandler(
        
        filename = function(){
          paste0("Rules-", Sys.time(), ".txt")
        },
        
        content = function( file ){
          write.table( rulesI, file, sep = ";", row.names = F )
        }
        
      )
      
    }
    
  })
  
  observeEvent( input$decisionI, {
    
    breakREPEAT <<- 0
    repeat{
      
      rollout <<- 0
      ImbTreeEntropy:::BuildTreeInterShiny( get("n0", envir = imb)$Tree, get("n0", envir = imb)$Y_name,
                                            get("n0", envir = imb)$X_names, get("n0", envir = imb)$data,
                                            get("n0", envir = imb)$depth, get("n0", envir = imb)$min_obs,
                                            get("n0", envir = imb)$type, get("n0", envir = imb)$entropy_par,
                                            get("n0", envir = imb)$cp, get("n0", envir = imb)$n_cores,
                                            get("n0", envir = imb)$weights, get("n0", envir = imb)$cost,
                                            get("n0", envir = imb)$class_th, get("n0", envir = imb)$overfit,
                                            get("n0", envir = imb)$cf, get("n0", envir = imb)$amb_prob,
                                            get("n0", envir = imb)$top_split, get("n0", envir = imb)$var_lev,
                                            get("n0", envir = imb)$amb_class, get("n0", envir = imb)$amb_class_freq,
                                            1, shiny = list( input = input, output = output) )
      
      if( all( imb$PROCESS$finish == T ) | (breakREPEAT == 2 ) ) break 
      
    }
    
    if( all( imb$PROCESS$finish == T ) ){
      
      TreeI <- ImbTreeEntropy:::ImbTreeEntropyInterShiny2()
      
      shinyjs::hide("decisionAreaTree")
      shinyjs::show("AreaSaveI")
      shinyjs::show("AreaRulesI")
      
      output$plotI <- renderPrint({
        
        PrintTreeInter(TreeI)
        
      })
      
      output$AccuracyI <- renderPrint({
        
        req( OkfileI == T & input$Y_nameI != "" )
        
        stop_pred <- ImbTreeEntropy:::StopPredict( TreeI, input_fileI() )
        if( stop_pred[1,] != "OK" ){
          
          stop_pred
          
        }else{
          
          predI <- PredictTree( TreeI, input_fileI() )$Class
          tarI <- input_fileI()[,input$Y_nameI]
          cat( paste0( capture.output( confusionMatrix( tarI, predI ) ), collapse = "\n") )
          
        }
        
      })
      
      rulesI <- ExtractRules( TreeI )
      output$RulesI <- renderPrint({
        
        req( OkfileI == T )
        
        cat( paste0( capture.output( rulesI ), collapse = "\n") )
        
      })
      
      output$saveI <- downloadHandler(
        
        filename = function(){
          paste0("Tree-", Sys.time(), ".rds")
        },
        
        content = function( file ){
          saveRDS( TreeI, file )
        }
        
      )
      
      output$saveRulesI <- downloadHandler(
        
        filename = function(){
          paste0("Rules-", Sys.time(), ".txt")
        },
  
        content = function( file ){
          write.table( rulesI, file, sep = ";", row.names = F )
        }
        
      )
      
    }
    
  })
  
  observeEvent( input$startP, {
    
    shinyjs::hide("AreaSaveP")
    
    req( input_treeP(), input$treeP )
    
    stop_pred <- ImbTreeEntropy:::StopPredict( input_treeP(), input_fileP() )
    if( stop_pred[1,] != "OK" ){
      
      predP <- stop_pred
      
    }else{
      
      predP <- PredictTree( input_treeP(), input_fileP() )
      
      
    }
    output$Pred <- renderTable({
      
      predP
      
    })
    
    shinyjs::show("AreaSaveP")
    
    output$saveP <- downloadHandler(
      
      filename = function(){
        paste0("Predict-", Sys.time(), ".txt")
      },
      
      content = function( file ){
        write.table( predP, file, sep = ";", row.names = F )
      }
      
    )
    
  })
  
  
}
