ImbTreeEntropyInterShiny1 <- function( Y_name, X_names, data, depth = 5, min_obs = 5, type = "Shannon", entropy_par = 1, cp = 0, n_cores = 1,
                                      weights = NULL, cost = NULL, class_th = "equal", overfit = "leafcut", cf = 0.25, amb_prob =  1, top_split = 2,
                                      var_lev = T, amb_class = NULL, amb_class_freq = NULL, shiny = list() ){

  # Check if all parameters are correctly specified, if no terminate the program
  Stop <- StopIfNot( Y_name, X_names, data, depth, min_obs, type, entropy_par, cp, n_cores, weights, cost, 
                     class_th, overfit, cf, amb_prob, top_split, var_lev, amb_class, amb_class_freq )
  if( !Stop ){
    
    return( invisible() )
    
  }
  
  # Assign Shiny global variable
  assign( "answer", 0, envir = .GlobalEnv )
  assign( "rollout", 0, envir = .GlobalEnv )
  assign( "breakREPEAT", 0, envir = .GlobalEnv )
  assign( "imb", new.env(), envir = .GlobalEnv )
  assign( "PROCESS",  data.frame( node = "n1", parent = "", sibling = "", left = "n2", right = "n3", 
                                  finish = F, wait = T, stringsAsFactors = F ), envir = imb )

  # Create probability matrix, initially all probabilities are equal
  Y_statistics <- CalcProb( data, Y_name, weights, cost )
  
  # Determine class labels
  Y_levels <- levels( data[,Y_name] )
  
  # Assign global probability matrix and auc
  AssignProbMatrix( data, Y_name, Y_statistics, Y_levels )
  
  # Create the root of the Tree
  Tree <- Node$new("Root")
  
  # Assign various initial measures
  AssignInitMeasures( Tree, data, Y_statistics, entropy_par, type, weights, cost, class_th )
  
  # If needed start cluster for parallel processing
  if( n_cores > 1 ){
    
    assign( "Global_Cluster", makeCluster( n_cores ), envir = .GlobalEnv )
    
  }
  
  local_env <- new.env()
  copyEnv( environment(), local_env, names = ls(environment(), all.names = T ) )
  assign( "n0", local_env, envir = imb )
  
  # Call of the main Building function
  BuildTreeInterShiny( Tree, Y_name, X_names, data, depth, min_obs, type, entropy_par, cp, n_cores, weights, cost, 
                       class_th, overfit, cf, amb_prob, top_split, var_lev, amb_class, amb_class_freq, 1, shiny )
  
}

ImbTreeEntropyInterShiny2 <- function(){
  
  # Prune tree if needed
  if( imb$n0$overfit == "leafcut" ){
    
    PruneTree( imb$n0$Tree ) 
    
  }else if( imb$n0$overfit == "prune" ){
    
    PessimisticErrorPruning( imb$n0$Tree, imb$n0$cf )
    
  }
  
  # Create various info for later use
  AddAttr( imb$n0$Tree, imb$n0$data, imb$n0$Y_levels, imb$n0$Y_statistics, imb$n0$min_obs, imb$n0$entropy_par, 
           imb$n0$type, imb$n0$cp, imb$n0$weights, imb$n0$cost, imb$n0$class_th, NULL, imb$n0$overfit, 
           imb$n0$cf, imb$n0$amb_prob, imb$n0$top_split, imb$n0$var_lev, imb$n0$amb_class, imb$n0$amb_class_freq )
  
  # If needed stop cluster for parallel processing
  if( imb$n0$n_cores > 1 ){
    
    stopCluster( Global_Cluster )
    
  }
  
  Tree <- imb$n0$Tree
  
  # Remove no more required objects
  rm(list = c("Probability_matrix", "Decision", "answer", "rollout", "breakREPEAT", "imb"), envir = .GlobalEnv)
  
  # Return Final Tree
  return( Tree )
  
}

BuildTreeInterShiny <- function( node, Y_name, X_names, data, depth, min_obs, type, entropy_par, cp, n_cores, weights, cost, 
                                 class_th, overfit, cf, amb_prob, top_split, var_lev, amb_class, amb_class_freq, number, shiny ){

  if( (rollout == 1) ){ return( invisible() ) }
  
  env_name <- paste0("n", number)
  lname <- paste0( "n", number + number )
  rname <- paste0( "n", number + number + 1 )
  
  NewContener( env_name, lname, rname )
    
  process <- imb$PROCESS[ env_name == imb$PROCESS$node, , drop = F ]
  
  if( process$wait == T ){
    
    # Number of observations in the node
    node$Count <- nrow(data)
    
    # Probability of the node
    probability <- CalcProb( data, Y_name, weights, cost )
    
    # Assign probability to the node
    node$Probability <- probability
    
    # Assign class to the node
    node$Class <- ChooseClass( probability, class_th, cost )
    
    ### Filtr
    # Calculate various statistics of all possible best local splits, choose the best one
    split_rule <- BestSplitGlobalInter( data[, c(X_names, Y_name)], Y_name, node$measure, min_obs, type, entropy_par, cp, 
                                        node$indexes, n_cores, weights, cost, overfit )
    
    # Check if the improvement is greater than the threshold
    no_improvement <- IfImprovementInter( node, split_rule, type, cp )
    split_rule <- split_rule[!no_improvement,]
    
    # Start Interactive Learning procedure if required
    if( nrow(split_rule) !=0 ){
      
      class_prob_learn <- ClassProbLearn( amb_class, amb_class_freq, node$Count, probability, node$root$Count, node$root$Probability )
      
      split_rule <- InteractiveLearningShiny( split_rule, node, Y_name, X_names, data, depth, min_obs, type, entropy_par, cp, n_cores, weights, 
                                              cost, class_th, overfit, cf, amb_prob, top_split, var_lev, class_prob_learn, number, shiny )

      if( (attr( split_rule, "Decision" ) == -666) ) return( invisible() )
      
      imb$PROCESS[ env_name == imb$PROCESS$node, "wait" ] <<- F

      # If decision was made, update the decision number
      if( attr( split_rule, "Decision" ) ){
        
        Decision <<- Decision + 1
        
      }else{
        
        Decision <- ""
        
      }
      
    }
    
    # If any is TRUE then node is considered as a leaf
    if( all(probability %in% c(0,1)) | {depth <= 0} | {nrow(split_rule) == 0} | {node$Count / 2 <= min_obs} ){
      
      # Assign leaf flag
      node$Leaf <- "*"
      
      # Update Global Probability matrix
      Probability_matrix[node$indexes, -ncol(Probability_matrix)] <<- matrix(probability, nrow = length(node$indexes), ncol = ncol(Probability_matrix)-1, byrow = T )
      
      imb$PROCESS[ env_name == imb$PROCESS$node, "wait" ] <<- F
      imb$PROCESS[ env_name == imb$PROCESS$node, "finish" ] <<- T

      SiblingFinished( process )
      
      return( node )
      
    }
    
  }

  if( (answer == 0 & rollout != 0 ) | !(env_name %in% imb$PROCESS$node & rollout == 0 ) | !env_name %in% ls(envir = imb)){
    
    # Prepare node name and indexes of observations for each child, name is different for numerical or nominal attribute
    node_name <- PrepareNames( split_rule, data )
    split_indexes <- node_name[[ 1 ]]
    l_name <- node_name[[ 2 ]]
    r_name <- node_name[[ 3 ]]
    
    # Split data for each child
    child_frame <- split(data, split_indexes)
    
    # Create left child
    childl <- CreateLeafInter( node, split_rule, l_name, split_indexes, "value_left", "l_class_error", 2 * number, Decision )
    # Create right child
    childr <- CreateLeafInter( node, split_rule, r_name, !split_indexes, "value_right", "r_class_error", 2 * number + 1, Decision )
    
    local_env <- new.env()
    copyEnv( environment(), local_env, names = ls(environment(), all.names = T ) )
    assign( env_name, local_env, envir = imb )
    
  }
  
  if( Skip( lname ) ){

    # Recursive call of the building function (BuildTreeInter) for the left child
    BuildTreeInterShiny( get(env_name, envir = imb)$childl, get(env_name, envir = imb)$Y_name, 
                         get(env_name, envir = imb)$X_names, get(env_name, envir = imb)$child_frame[[2]], 
                         get(env_name, envir = imb)$depth - 1, get(env_name, envir = imb)$min_obs,
                         get(env_name, envir = imb)$type, get(env_name, envir = imb)$entropy_par, 
                         get(env_name, envir = imb)$cp, get(env_name, envir = imb)$n_cores, 
                         get(env_name, envir = imb)$weights, get(env_name, envir = imb)$cost, 
                         get(env_name, envir = imb)$class_th, get(env_name, envir = imb)$overfit, 
                         get(env_name, envir = imb)$cf, get(env_name, envir = imb)$amb_prob, 
                         get(env_name, envir = imb)$top_split, get(env_name, envir = imb)$var_lev, 
                         get(env_name, envir = imb)$amb_class, get(env_name, envir = imb)$amb_class_freq, 
                         2 * get(env_name, envir = imb)$number, shiny )
    
  }

  if( Skip( rname ) ){

    # Recursive call of the building function (BuildTreeInter) for the right child
    BuildTreeInterShiny( get(env_name, envir = imb)$childr, get(env_name, envir = imb)$Y_name, 
                         get(env_name, envir = imb)$X_names, get(env_name, envir = imb)$child_frame[[1]], 
                         get(env_name, envir = imb)$depth - 1, get(env_name, envir = imb)$min_obs, 
                         get(env_name, envir = imb)$type, get(env_name, envir = imb)$entropy_par, 
                         get(env_name, envir = imb)$cp, get(env_name, envir = imb)$n_cores, 
                         get(env_name, envir = imb)$weights, get(env_name, envir = imb)$cost, 
                         get(env_name, envir = imb)$class_th, get(env_name, envir = imb)$overfit, 
                         get(env_name, envir = imb)$cf, get(env_name, envir = imb)$amb_prob, 
                         get(env_name, envir = imb)$top_split, get(env_name, envir = imb)$var_lev, 
                         get(env_name, envir = imb)$amb_class, get(env_name, envir = imb)$amb_class_freq, 
                         2 * get(env_name, envir = imb)$number + 1, shiny )
    
  }
  
  ChildrenFinished( env_name )
  
}

InteractiveLearningShiny <- function( split_rule, tree, Y_name, X_names, data, depth, min_obs, type, entropy_par, cp, n_cores, weights, cost, 
                                      class_th, overfit, cf, amb_prob, top_split, var_lev, amb_class, number, shiny ){

  if( answer == 0 ){
    
    # Check if interactive learning should be performed
    stop_cond <- StopCond( split_rule, amb_prob, amb_class )
    
    # Possible splits are above the threshold, no interaction required
    if( stop_cond == F ){
      
      # Take the best split
      split_rule <- split_rule[ order(-split_rule[,"value"]), ][ 1, , drop = F ]
      
      # Remove additional numbers from the row names
      rownames(split_rule) <- gsub( ".[[:digit:]]{1,5}", "", rownames(split_rule) )
      attr( split_rule, "Decision" ) <- F
      
      return( split_rule )
      
    }
    
    # Sort in descending order splits according to the value
    split_rule_inter <- split_rule[order(-split_rule[,"value"]),]
    
    if( var_lev ){
      
      # Take best split of each variable
      split_rule_inter <- do.call( "rbind", lapply( split( split_rule_inter, gsub( ".[[:digit:]]{1,5}", "", rownames(split_rule_inter) ) ), 
                                                    function(x){ x[ 1, , drop = F ] } ) )
      
      # Sort in descending order splits according to the value
      split_rule_inter <- split_rule_inter[order(-split_rule_inter[,"value"]),]
      
      # Take top splits
      split_rule_inter <- split_rule_inter[ 1:min( nrow(split_rule_inter), top_split ),]
      
    }else{
      
      # Take top splits
      split_rule_inter <- split_rule_inter[ 1:min( nrow(split_rule_inter), top_split ),]
      
    }
    
    prune_vec <- c()
    for( i in 1:nrow(split_rule_inter) ){
      
      node_name <- PrepareNames( split_rule_inter[i,], data )
      split_indexes <- node_name[[ 1 ]]
      l_name <- node_name[[ 2 ]]
      r_name <- node_name[[ 3 ]]
      
      # Deep clone of the current tree structure
      Tree <- Clone( tree$root )
      node <- Clone( tree )
      
      # Derive path to this node on the initial tree structure 
      leaf <- LeafPath( tree )
      
      # Split data for each child
      child_frame <- split(data, split_indexes)
      
      # Create left child
      child <- CreateLeafInter( node, split_rule_inter[i, , drop = F], l_name, split_indexes, "value_left", "l_class_error",  2 * number, "*NOW*" )
      
      # Recursive call of the building function (BuildTree) for the left child
      BuildTree( child, Y_name, X_names, child_frame[[2]], depth - 1, min_obs, type, entropy_par, cp, n_cores, weights, cost, class_th, overfit, 2 * number )
      
      # Create right child
      child <- CreateLeafInter( node, split_rule_inter[i, , drop = F], r_name, !split_indexes, "value_right", "r_class_error", 2 * number + 1, "*NOW*" )
      
      # Recursive call of the building function (BuildTree) for the right child
      BuildTree( child, Y_name, X_names, child_frame[[1]], depth - 1, min_obs, type, entropy_par, cp, n_cores, weights, cost, class_th, overfit, 2 * number + 1 )
      
      # Prune tree if needed
      if( overfit == "leafcut" ){
        
        prune <- PruneTreeInter( node ) 
        
      }else if( overfit == "prune" ){
        
        prune <- PessimisticErrorPruningInter( node, cf )
        
      }else{
        
        prune <- "YES"
        
      }
      
      prune_vec <- c( prune_vec, prune )
      
      if( !prune == "NO" ){
        
        # Insert into the original/currnet Tree structure the created subbranches. For display purpose only
        eval( parse( text = leaf ) )$AddChildNode( node$children[[1]] )
        eval( parse( text = leaf ) )$AddChildNode( node$children[[2]] )
        
      }
      
      # Create output file with the possible tree
      assign( sprintf( "Tree%s", i ), Tree )

    }
    
    shiny$output$plotI <- renderPrint({
      
      cat( sprintf( "Current Tree. Accuracy: %2.2f%%.", round( ErrorInteractive( tree$root ) * 100, 2 ) ), "\n" )
      PrintTreeInter( tree$root )
      cat( "\n\n" )
      
      for( i in 1:nrow(split_rule_inter) ){
        
        cat( sprintf( "Tree%s. Accuracy: %2.2f%%.", i, round( ErrorInteractive( get( sprintf( "Tree%s", i ) ) ) * 100, 2 ) ), "\n" )
        PrintTreeInter( get( sprintf( "Tree%s", i ) ) )
        cat( "\n\n" )
        
      }
      
    })
    
    answer <<- 1
    rollout <<- 1
    
    shiny$output$Ntree <- renderUI({
      
      selectInput( "Ntree", "Provide Tree number", choices = 1:nrow(split_rule_inter) )
      
    })
    
    assign( "split_rule_inter", split_rule_inter, envir = imb)
    assign( "i", i, envir = imb)
    assign( "prune_vec", prune_vec, envir = imb)
    
    # Prepare dummy table, there is no sense to perform any split
    split_rule <- matrix( 0, 0, 12, dimnames = list( NULL, colnames(split_rule) ) )
    attr( split_rule, "Decision" ) <- -666
    
    if( all( prune_vec == "NO") ){

      breakREPEAT <<- 1

    }else{
      
      breakREPEAT <<- breakREPEAT + 1
      
    }

    return( split_rule )
    
  }
  
  if( nrow(imb$split_rule_inter) == 1 | all( imb$prune_vec == "NO") ){
    
    ANSWER <- 1
    
  }else{
    
    ANSWER <- as.numeric( shiny$input$Ntree )
    
  }

  
  answer <<- 0
  rollout <<- 1
  
  if( imb$prune_vec[ANSWER] == "NO" ){
    
    # Prepare dummy table, there is no sense to perform any split
    split_rule <- matrix( 0, 0, 12, dimnames = list( NULL, colnames(split_rule) ) )
    attr( split_rule, "Decision" ) <- F
    breakREPEAT <<- 1
    
    return( split_rule )
    
  }else{
    
    # Take the chosen one
    split_rule <- imb$split_rule_inter[ANSWER,]
    
    # Remove additional numbers from the row names
    rownames(split_rule) <- gsub( ".[[:digit:]]{1,5}", "", rownames(split_rule) )
    
    if( imb$i == 1 ){
      
      attr( split_rule, "Decision" ) <- F
      
    }else{
      
      attr( split_rule, "Decision" ) <- T
      
    }
    
    breakREPEAT <<- breakREPEAT + 1
    
    return( split_rule )
    
  }
  
}

copyEnv <- function( from, to, names = ls( from, all.names = T ) ){
  
  mapply( assign, names, mget(names, from), list(to), SIMPLIFY = F, USE.NAMES = F )
  
  return( invisible( NULL ) )
  
}

Skip <- function( name ){
  
  skip <- if( nrow( imb$PROCESS[ imb$PROCESS$node == name, , drop = F] ) == 0 ){
    
    T
    
  }else{
    
    imb$PROCESS[imb$PROCESS$node == name, "finish" , drop = F] == F
    
  }
  
  return( skip )
  
}

ChildrenFinished <- function( env_name ){
  
  children <- imb$PROCESS[ imb$PROCESS$parent == env_name, "finish", drop = F ]
  
  if( nrow(children) == 2 ){
    
    if( all( children == T ) ){
      
      imb$PROCESS[ imb$PROCESS$node == env_name, "finish" ] <<- T
      
    }
    
  }
  
}

SiblingFinished <- function( process ){
  
  sibling <- imb$PROCESS[ imb$PROCESS$node == process$sibling, "finish", drop = F ]
  
  if( nrow( sibling ) ){
    
    if( sibling == T ){
      
      imb$PROCESS[ imb$PROCESS$node == process$parent, "finish" ] <<- T
      
    }
    
  }
  
}

NewContener <- function( env_name, lname, rname ){
  
  if( env_name != "n1" ){
    
    if( !(env_name %in% imb$PROCESS$node) ){
      
      indx_left <- env_name == imb$PROCESS$left
      indx_right <- env_name == imb$PROCESS$right
      sib <- ifelse( max( indx_left ) == 1, "right", "left" )
      
      parent <- imb$PROCESS[ indx_left | indx_right, , drop = F ]
      process <- data.frame( node = env_name, parent = parent$node, sibling = parent[,sib], 
                             left = lname, right = rname, finish = F, wait = T, stringsAsFactors = F )
      
      imb$PROCESS <<- rbind( imb$PROCESS, process )
      
    }
    
  }
  
}

CrossValid <- function( Y_name, X_names, dat, type, depth, min_obs, Qval, Alpha, Beta, weights, cost, overfit, cp, cf, k_fold, seed ){
  
  Shannon <- RenyiTsallis <- MittalTanejaKapur <- NULL
  
  if( "Shannon" %in% type ){
    
    if( c("prune") %in% overfit ){
      
      Shannon1 <- expand.grid( Type = "Shannon", Q = 1, Alpha = 1, Beta = 1, Depth = depth, Min_obs = min_obs, 
                               Overfit = overfit[which("prune"!=overfit)], Cp = cp, Cf = 0.25, stringsAsFactors = F )
      Shannon2 <- expand.grid( Type = "Shannon", Q = 1, Alpha = 1, Beta = 1, Depth = depth, Min_obs = min_obs, 
                               Overfit = "prune", Cp = 0, Cf = cf, stringsAsFactors = F )
      Shannon <- rbind( Shannon1, Shannon2 )
      
    }else{
      
      Shannon <- expand.grid( Type = "Shannon", Q = 1, Alpha = 1, Beta = 1, Depth = depth, Min_obs = min_obs, 
                              Overfit = overfit, Cp = cp, Cf = 0.25, stringsAsFactors = F )
      
    }
    
  }
  if( any(c("Renyi","Tsallis") %in% type) ){
    
    if( c("prune") %in% overfit ){
      
      RenyiTsallis1 <- expand.grid( Type = type[type%in%c("Renyi","Tsallis")], Q = Qval, Alpha = 1, Beta = 1, 
                                    Depth = depth, Min_obs = min_obs, Overfit = overfit[which("prune"!=overfit)], 
                                    Cp = cp, Cf = 0.25, stringsAsFactors = F )
      RenyiTsallis2 <- expand.grid( Type = type[type%in%c("Renyi","Tsallis")], Q = Qval, Alpha = 1, Beta = 1, 
                                    Depth = depth, Min_obs = min_obs, Overfit = "prune", Cp = 0, Cf = cf, 
                                    stringsAsFactors = F )
      RenyiTsallis <- rbind( RenyiTsallis1, RenyiTsallis2 )
      
    }else{
      
      RenyiTsallis <- expand.grid( Type = type[type%in%c("Renyi","Tsallis")], Q = Qval, Alpha = 1, Beta = 1,
                                   Depth = depth, Min_obs = min_obs, Overfit = overfit, Cp = cp, Cf = 0.25, stringsAsFactors = F )
      
    }
    
  }
  if( any(c("Sharma-Mittal", "Sharma-Taneja", "Kapur") %in% type) ){
    
    if( c("prune") %in% overfit ){
      
      MittalTanejaKapur1 <- expand.grid( Type = type[type%in%c("Sharma-Mittal","Sharma-Taneja","Kapur")],
                                         Q = 1, Alpha = Alpha, Beta = Beta, Depth = depth, Min_obs = min_obs, 
                                         Overfit = overfit[which("prune"!=overfit)], Cp = cp, Cf = 0.25, stringsAsFactors = F )
      MittalTanejaKapur2 <- expand.grid( Type = type[type%in%c("Sharma-Mittal","Sharma-Taneja","Kapur")], 
                                         Q = 1, Alpha = Alpha, Beta = Beta, Depth = depth, Min_obs = min_obs, 
                                         Overfit = "prune", Cp = 0, Cf = cf, stringsAsFactors = F )
      MittalTanejaKapur <- rbind( MittalTanejaKapur1, MittalTanejaKapur2 )
      
    }else{
      
      MittalTanejaKapur <- expand.grid( Type = type[type%in%c("Sharma-Mittal","Sharma-Taneja","Kapur")], 
                                        Q = 1, Alpha = Alpha, Beta = Beta, Depth = depth, Min_obs = min_obs, 
                                        Overfit = overfit, Cp = cp, Cf = 0.25, stringsAsFactors = F )
      
    }
    
  }
  exclude <- (MittalTanejaKapur$Type == "Kapur" & (MittalTanejaKapur$Alpha == 1 | MittalTanejaKapur$Alpha <= 0 & 
                                                     MittalTanejaKapur$Alpha <= 0 | MittalTanejaKapur$Alpha + MittalTanejaKapur$Beta - 1 <= 0)) | 
    (MittalTanejaKapur$Type == "Sharma-Taneja" & MittalTanejaKapur$Alpha == MittalTanejaKapur$Beta)
  MittalTanejaKapur <- MittalTanejaKapur[!exclude,]
  
  Grid <- rbind( Shannon, RenyiTsallis, MittalTanejaKapur )
  
  set.seed( seed )
  
  cross_validation <- createFolds( dat[,Y_name], k_fold, T )
  cross_validation <- lapply( cross_validation, function( valid, tar ){ 
    temp <- rep(1, length(tar)); temp[valid] <- 2; temp 
  }, tar = dat[,Y_name])
  
  train <- data.frame( matrix( 0, length(cross_validation) * nrow(Grid), ncol(Grid) + 5 ) )
  colnames(train) <- c( "Cross", colnames(Grid), "Accuracy", "Kappa", "Nleaves", "Nclass" )
  valid <- train
  
  # To speed up computations, it does not require revalidation of the App
  isolate(
    
    withProgress(message = "Tuning process", detail = sprintf("1 out of %s", nrow(train)), value = 0, {
      
      k <- 1
      for( i in 1:length(cross_validation) ){
        
        for( j in 1:nrow(Grid) ){
          
          Tree <- ImbTreeEntropy( Y_name = Y_name, X_names = X_names, data = dat[ cross_validation[[i]] == 1, ], type = Grid[j,"Type"], 
                                  depth = Grid[j,"Depth"], min_obs = Grid[j,"Min_obs"], cp = Grid[j,"Cp"], cf = Grid[j,"Cf"], 
                                  overfit = Grid[j,"Overfit"], weights = weights, cost = cost, class_th = "equal",
                                  entropy_par = if(Grid[j,"Type"] %in% c("Sharma-Mittal", "Sharma-Taneja", "Kapur")){c(Grid[j,"Alpha"], Grid[j,"Beta"])}else{Grid[j,"Q"]} )
          
          pred_train <- PredictTree( Tree, dat[ cross_validation[[i]] == 1, ] )
          pred_valid <- PredictTree( Tree, dat[ cross_validation[[i]] == 2, ] )
          
          conf_train <- confusionMatrix( pred_train$Class, dat[ cross_validation[[i]] == 1, Y_name ] )$overall[1:2]
          conf_valid <- confusionMatrix( pred_valid$Class, dat[ cross_validation[[i]] == 2, Y_name ] )$overall[1:2]
          
          train[k, "Cross"] <- i
          valid[k, "Cross"] <- i
          
          train[k, !colnames(train) %in% c("Cross","Accuracy","Kappa") ] <- Grid[j,]
          valid[k, !colnames(valid) %in% c("Cross","Accuracy","Kappa")] <- Grid[j,]
          
          train[k, c("Accuracy", "Kappa")] <- conf_train[c("Accuracy", "Kappa")]
          valid[k, c("Accuracy", "Kappa")] <- conf_valid[c("Accuracy", "Kappa")]
          
          valid[k, "Nleaves"] <- train[k, "Nleaves"] <- length(Tree$Get("pathString", filterFun = isLeaf))
          valid[k, "Nclass"] <- train[k, "Nclass"] <- length(unique(Tree$Get("Class", filterFun = isLeaf)))
          
          k <- k + 1
          
          incProgress( 1/nrow(train), detail = sprintf("%s out of %s", k, nrow(train)))
          
        }
        
      }
      
    })
    
  )
  
  train[, "Nleaves"] <- as.numeric(train[, "Nleaves"])
  valid[, "Nleaves"] <- as.numeric(valid[, "Nleaves"])
  
  train_agg <- aggregate( train[,c("Accuracy","Kappa","Nleaves","Nclass")], as.list(train[,2:10]), mean )
  valid_agg <- aggregate( valid[,c("Accuracy","Kappa","Nleaves","Nclass")], as.list(valid[,2:10]), mean )
  
  return( list(Train = train, TrainAgg = train_agg, Valid = valid, ValidAgg = valid_agg) )
  
}

StopPredict <- function( tree, data ){
  
  required_features <- names( attr(tree, "Required_features") )
  
  if ( !all( required_features %in% colnames(data) ) ) {
    
    col <- !required_features %in% colnames(data)
    col <- required_features[col]
    out <- data.frame( " " = sprintf("*** The following features are required: %s. ***", paste0(col,collapse = ", ")), 
                       check.names = F )
    
    return( out )
    
  }
  
  required_features <- attr(tree, "Required_features")
  required_features <- required_features[ !unlist( lapply( required_features, is.null ) ) ]
  
  col <- unlist( sapply( names(required_features), function(i, lev, dat){ all( levels( dat[,i] ) %in% lev[[i]] ) },
                         lev = required_features, dat = data, simplify = F) )
  if ( !all( col ) ) {
    
    col <- names( required_features )[!col]
    out <- data.frame( " " = sprintf("*** The following features have to many levels: %s. ***", paste0(col,collapse = ", ")), 
                       check.names = F )
    return( out )
    
  }
  
  out <- data.frame( " " = "OK", check.names = F )
  
  return( out )
  
}

ErrorInteractive <- function( tree ){
  
  prob <- do.call( "cbind", tree$Get("Probability", filterFun = isLeaf, simplify = F) )
  class <- tree$Get("Class", filterFun = isLeaf)
  count <- tree$Get("Count", filterFun = isLeaf)
  
  probs <- prob[ match( do.call(paste, list( class, names(class) ) ), 
                        outer(rownames(prob), colnames(prob), FUN = paste) ) ]
  
  sum1 <- sum( probs * count, na.rm = T )
  sum2 <- ( tree$root$Count - sum( count, na.rm = T ) ) * max( tree$root$Probability )
  
  return( ( sum1 + sum2 ) / tree$root$Count )
  
}