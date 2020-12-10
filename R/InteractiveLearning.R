InteractiveLearning <- function( split_rule, tree, Y_name, X_names, data, depth, min_obs, type, entropy_par, cp, n_cores, weights, cost, 
                                 class_th, overfit, cf, amb_prob, top_split, var_lev, amb_class, number, tree_path ){

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

  cat("\n\n********** Start Interactive Learning **********\n\n")
  
  # Create output file with the initial tree
  writeLines( capture.output( PrintTreeInter( tree$root ) ), sprintf( "%s/tree%s.txt", tree_path, 0 ) )

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
    child <- CreateLeafInter( node, split_rule_inter[i, , drop = F], l_name, split_indexes, "value_left", "l_class_error", 2 * number, "*NOW*" )

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
    writeLines( capture.output( PrintTreeInter( Tree ) ), sprintf( "%s/tree%s.txt", tree_path, i ) )

  }
  
  # Choose the desired tree
  repeat{
    
    # If there is only on split end
    ANSWER <- 1
    if( nrow(split_rule_inter) == 1 | all( prune_vec == "NO") ) break
    
    # Choose the desired split
    ANSWER <- as.integer( readline( sprintf( "Please choose the tree number from 1 to %s: ", nrow(split_rule_inter) ) ) )
    if( ANSWER %in% 1:nrow(split_rule_inter) ) break
    
  }
  
  if( prune_vec[ANSWER] == "NO" ){
    
    # Prepare dummy table, there is no sense to perform any split
    split_rule <- matrix( 0, 0, 12, dimnames = list( NULL, colnames(split_rule) ) )
    attr( split_rule, "Decision" ) <- F
    
    cat("******* Subtree collapsed: no new split ********")
    
    return( split_rule )
    
  }else{
    
    # Take the chosen one
    split_rule <- split_rule_inter[ANSWER,]
    
    # Remove additional numbers from the row names
    rownames(split_rule) <- gsub( ".[[:digit:]]{1,5}", "", rownames(split_rule) )
    
    if( i == 1 ){
      
      attr( split_rule, "Decision" ) <- F
      cat("*********** There was only one split ***********")
      
    }else{
      
      attr( split_rule, "Decision" ) <- T
      
    }
    
    return( split_rule )
    
  }
  
}

StopCond <- function( split_rule, amb_prob, amb_class ){
  
  if( amb_class == 3 ){
    # & or | to decide later
    cond <- split_rule[,"l_prob_peak"] > amb_prob & split_rule[,"r_prob_peak"] > amb_prob
    cond <- !any( cond )
    
  }else if( amb_class == 2 ){
    
    cond <- F
    
  }else if( amb_class == 1 ){
    
    cond <- T
    
  }
  
  return( cond )
  
}

LeafPath <- function( tree ){
  
  temp <- strsplit( tree$Get("pathString"), "/" )[[1]]
  if( length(temp) == 1 ){
    
    leaf <- "Tree"
    
  }else{
    
    leaf <- paste( "Tree", paste0( paste0( "'", temp[-1] ), "'", collapse = "$" ), sep = "$" )
    
  }
  
  return( leaf )
  
}