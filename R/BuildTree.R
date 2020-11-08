BuildTree <- function( node, Y_name, X_names, data, depth, min_obs, type, entropy_par, cp, n_cores, weights, cost, class_th, overfit, number ){

  # Number of observations in the node
  node$Count <- nrow(data)

  # Probability of the node
  probability <- CalcProb( data, Y_name, weights, cost )

  # Assign probability to the node
  node$Probability <- probability
  
  # Assign class to the node
  node$Class <- ChooseClass( probability, class_th, cost )

  # Calculate various statistics of all possible best local splits, choose the best one
  split_rule <- BestSplitGlobal( data[, c(X_names, Y_name)], Y_name, node$measure, min_obs, type, entropy_par, cp, node$indexes, n_cores,
                                 weights, cost, overfit )

  # Check if the improvement is greater than the threshold
  no_improvement <- IfImprovement( node, split_rule, type, cp )

  # If any is TRUE then node is considered as a leaf
  if( all(probability %in% c(0,1)) | {depth <= 0} | {no_improvement} | {node$Count / 2 <= min_obs} ){

    # Assign leaf flag
    node$Leaf <- "*"
    
    # Update Global Probability matrix
    Probability_matrix[node$indexes, -ncol(Probability_matrix)] <<- matrix(probability, nrow = length(node$indexes), ncol = ncol(Probability_matrix)-1, byrow = T )
    
    return(node)

  }else{

    # Prepare node name and indexes of observations for each child, name is different for numerical or nominal attribute
    node_name <- PrepareNames( split_rule, data )
    split_indexes <- node_name[[ 1 ]]
    l_name <- node_name[[ 2 ]]
    r_name <- node_name[[ 3 ]]

    # Split data for each child
    child_frame <- split(data, split_indexes)

    # Create left child
    childl <- CreateLeaf( node, split_rule, l_name, split_indexes, "value_left", "l_class_error", 2 * number )
    
    # Recursive call of the building function (BuildTree) for the left child
    BuildTree( childl, Y_name, X_names, child_frame[[2]], depth - 1, min_obs, type, entropy_par, cp, n_cores, weights, cost, class_th, overfit, 2 * number )
    
    # Create right child
    childr <- CreateLeaf( node, split_rule, r_name, !split_indexes, "value_right", "r_class_error", 2 * number + 1 )
    
    # Recursive call of the building function (BuildTree) for the right child
    BuildTree( childr, Y_name, X_names, child_frame[[1]], depth - 1, min_obs, type, entropy_par, cp, n_cores, weights, cost, class_th, overfit, 2 * number + 1 )
    
  }

}

PrepareNames <- function( split_rule, data ){

  # Remove additional numbers from the row names
  rownames(split_rule) <- gsub( "\\.[[:digit:]]{1,5}", "", rownames(split_rule) )

  if_fac <- length(grep("\\(", split_rule[,"split"])) > 0
  if( if_fac ){
    
    fac_rul <- split_rule[,"split"]
    fac_rul <- eval( parse( text = paste0( "c(",paste0( paste0( "'", strsplit(substr( fac_rul, 2, nchar(fac_rul)-1 ), "," )[[1]] ), "'", collapse = "," ), ")") ) )
    split_indexes <- data[,rownames(split_rule)] %in% fac_rul
    l_name <- sprintf("%s = %s",rownames(split_rule), split_rule[,"split"])
    r_name <- sprintf("%s = %s",rownames(split_rule), split_rule[,"split_rest"])
    
  }else{
    
    split_indexes <- data[,rownames(split_rule)] <= ( eval(parse(text = split_rule[,"split"])) )
    l_name <- sprintf("%s <= %s",rownames(split_rule), split_rule[,"split"])
    r_name <- sprintf("%s >  %s",rownames(split_rule), split_rule[,"split"])
    
  }
  
  return( list( split_indexes = split_indexes, l_name = l_name, r_name = r_name ) )
  
}

CreateLeaf <- function( node, split_rule, name, split_indexes, value, class_error, number ){
  
  # Create child
  child <- node$AddChild( name )
  child$feature <- rownames(split_rule)
  child$value <- split_rule[,"split"]
  child$measure <- split_rule[,value]
  child$indexes <- node$indexes[split_indexes]
  child$depth <- node$depth + 1
  child$localerror <- split_rule[,class_error]
  child$Number <- number
  
  return( child )
  
}

IfImprovement <- function( node, split_rule, type, cp ){

  if( cp > 0 ){

    global_error_improvement <- ClassErrorGlobal( node, split_rule[, c("l_class_error", "r_class_error")], cp )
    no_improvement <- global_error_improvement <= cp | all(split_rule == 0)

  }else{

    no_improvement <- if( all(split_rule == 0) ){ TRUE }else{ FALSE }

  }

  return( no_improvement )

}
