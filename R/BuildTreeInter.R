BuildTreeInter <- function( node, Y_name, X_names, data, depth, min_obs, type, entropy_par, cp, n_cores, weights, cost, 
                            class_th, overfit, cf, amb_prob, top_split, var_lev, amb_class, amb_class_freq, number, tree_path ){

  # Number of observations in the node
  node$Count <- nrow(data)

  # Probability of the node
  probability <- CalcProb( data, Y_name, weights, cost )

  # Assign probability to the node
  node$Probability <- probability
  
  # Assign class to the node
  node$Class <- ChooseClass( probability, class_th, cost )
  
  # Calculate various statistics of all possible best local splits, choose the best one
  split_rule <- BestSplitGlobalInter( data[, c(X_names, Y_name)], Y_name, node$measure, min_obs, type, entropy_par, cp, 
                                      node$indexes, n_cores, weights, cost, overfit )

  # Check if the improvement is greater than the threshold
  no_improvement <- IfImprovementInter( node, split_rule, type, cp )
  split_rule <- split_rule[!no_improvement,]

  # Start Interactive Learning procedure if required
  if( nrow(split_rule) !=0 ){

    class_prob_learn <- ClassProbLearn( amb_class, amb_class_freq, node$Count, probability, node$root$Count, node$root$Probability )
      
    split_rule <- InteractiveLearning( split_rule, node, Y_name, X_names, data, depth, min_obs, type, entropy_par, cp, n_cores, 
                                       weights, cost, class_th, overfit, cf, amb_prob, top_split, var_lev, class_prob_learn, number, tree_path )

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
    
    return( node )

  }else{

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

    # Recursive call of the building function (BuildTreeInter) for the left child
    BuildTreeInter( childl, Y_name, X_names, child_frame[[2]], depth - 1, min_obs, type, entropy_par, cp, n_cores, 
                    weights, cost, class_th, overfit, cf, amb_prob, top_split, var_lev, amb_class, amb_class_freq, 2 * number, tree_path )
    
    # Recursive call of the building function (BuildTreeInter) for the right child
    BuildTreeInter( childr, Y_name, X_names, child_frame[[1]], depth - 1, min_obs, type, entropy_par, cp, n_cores, 
                    weights, cost, class_th, overfit, cf, amb_prob, top_split, var_lev, amb_class, amb_class_freq, 2 * number + 1, tree_path )
    
  }

}

ClassProbLearn <- function( amb_class, amb_class_freq, node_count, node_prob, tree_count, tree_prob ){
  
  # If amb_class is not null check if classes of interest are above the thresholds
  if( !is.null(amb_class) ){
    
    prob_glob <- ( node_prob * node_count ) / ( tree_prob * tree_count )
    # prob_glob or node_prob
    if( any( prob_glob[amb_class] > amb_class_freq ) ){

      class_prob_learn <- 1
      
    }else{
      
      class_prob_learn <- 2
      
    }
    
  }else{
    
    class_prob_learn <- 3
    
  }

  return( class_prob_learn )
  
}

CreateLeafInter <- function( node, split_rule, name, split_indexes, value, class_error, number, decision ){
  
  # Create child
  child <- node$AddChild( name )
  child$feature <- rownames(split_rule)
  child$value <- split_rule[,"split"]
  child$measure <- split_rule[,value]
  child$indexes <- node$indexes[split_indexes]
  child$depth <- node$depth + 1
  child$localerror <- split_rule[,class_error]
  child$Number <- number
  child$Decision <- decision
  
  return( child )
  
}

IfImprovementInter <- function( node, split_rule, type, cp ){

  if( cp > 0 ){

    global_error_improvement <- apply( split_rule[, c("l_class_error", "r_class_error")], 1,
                                       function(x, n, cp){ ClassErrorGlobal( n, x, cp ) }, n = node, cp = cp )
    no_improvement <- global_error_improvement <= cp | apply( split_rule, 1, function(x){ all(x == 0) })

  }else{

    no_improvement <- apply( split_rule, 1, function(x){ all(x == 0) | x[1] == 0 })

  }

  return( no_improvement )

}
