PruneTree <- function( tree ){
  
  # Check if there is only a ROOT
  if( length( tree$Get("pathString", filterFun = isLeaf) ) == 1 ) return( NULL )
  
  # Create empty list of the visited leaves
  If_all <- c()
  
  repeat{
    
    # Prepare path to each leaf
    Leaf_path <- tree$Get("pathString", filterFun = isLeaf)
    
    # Check if there are no other leaves to collapse
    if( all( Leaf_path %in% If_all ) | Leaf_path[1] == "Root" ) break
    
    # Split path to node parts
    temp <- strsplit( Leaf_path[ !Leaf_path %in% If_all ][1], "/" )[[1]]

    # Take leaf
    leaf <- eval( parse( text = paste( "tree", paste0( paste0( "'", temp[-1] ), "'", collapse = "$" ), sep = "$" ) ) )
    
    # Take class of the leaf and its sibling
    leaf_class <- leaf$Class
    sibling_class <- Get( leaf$siblings, "Class" )
    
    # Check if sibling of the leaf is also a leaf
    leaf_isLeaf <- leaf$isLeaf
    sibling_isLeaf <- Get( leaf$siblings, "isLeaf" )
    
    # Take name of the leaf and its sibling
    leaf_name <- leaf$name
    sibling_name <- Get( leaf$siblings, "name" )
    
    # Check if collapsing is needed
    if( leaf_class == sibling_class & leaf_isLeaf == sibling_isLeaf ){
      
      # Take parent node
      leaf_parent <- leaf$parent
      
      # Remove children
      leaf_parent$RemoveChild( sibling_name )
      leaf_parent$RemoveChild( leaf_name )

      # Assign leaf flag
      leaf_parent$Leaf <- "*"

    }else{
      
      # Update list of the visited leaves
      If_all <- c( If_all, leaf$pathString )
      
    }
    
  }

}

AvoidSameClass <- function( probs_l, probs_r, cost ){
  
  # Calculate number of classes
  k <- length(probs_l)
  if( is.null(cost) ){
    
    # Set up equal thresholds
    threshold <- rep( 1/k, k )
    
  }else{
    
    # For multiclass case there are k thresholds
    threshold <- 1 / rowSums( cost )
    threshold <- threshold / sum(threshold)
    
  }
  
  # Calculate how much each threshold is exceeded by the probability
  probs_l <- probs_l / threshold
  probs_l[ is.nan(probs_l) ] <- Inf
  
  # Take probability of the class which exceeds its threshold most
  new_class_l <- which.max( probs_l )
  
  # Calculate how much each threshold is exceeded by the probability
  probs_r <- probs_r / threshold
  probs_r[ is.nan(probs_r) ] <- Inf
  
  # Take probability of the class which exceeds its threshold most
  new_class_r <- which.max( probs_r )
  
  # If split is unacceptable then TRUE
  result <- unname( ifelse( new_class_l == new_class_r, 1, 0 ) )
  
  return( result )
  
}

ErrorPrune <- function( err, N, z ){

  p <- err / N
  error <- ( p + (z^2)/(2*N) + z * sqrt( p/N - (p^2)/N + (z^2)/(4*N^2) ) ) / ( 1 + z^2/N )
  
  return( error )
  
}

PessimisticErrorPruning <- function( tree, cf ){

  errcf <- qnorm( 1 - cf )
  
  # Check if there is only a ROOT
  if( length( tree$Get("pathString", filterFun = isLeaf) ) == 1 ) return( NULL )
  
  # Create empty list of the visited leaves
  If_all <- c()
  
  repeat{

    # Prepare path to each leaf
    Leaf_path <- tree$Get("pathString", filterFun = isLeaf)

    # Check if there are no other leaves to collapse
    if( all( Leaf_path %in% If_all ) | Leaf_path[1] == "Root" ) break
    
    # Split path to node parts
    temp <- strsplit( Leaf_path[ !Leaf_path %in% If_all ][1], "/" )[[1]]
    
    # Take leaf
    leaf <- eval( parse( text = paste( "tree", paste0( paste0( "'", temp[-1] ), "'", collapse = "$" ), sep = "$" ) ) )
    
    # Take parent node
    parent <- leaf$parent
    
    # Take class of the leaf and its sibling
    leaf_class <- leaf$Class
    sibling_class <- Get( leaf$siblings, "Class" )
    parent_class <- parent$Class
    
    # Take probability of the leaf and its sibling
    leaf_prob <- leaf$Probability
    sibling_prob <- Get( leaf$siblings, "Probability" )[,1]
    parent_prob <- parent$Probability
    
    # Take counts of the leaf and its sibling
    leaf_count <- leaf$Count
    sibling_count <- Get( leaf$siblings, "Count" )
    parent_count <- parent$Count
    
    # Check if sibling of the leaf is also a leaf
    leaf_isLeaf <- leaf$isLeaf
    sibling_isLeaf <- Get( leaf$siblings, "isLeaf" )
    
    # Take name of the leaf and its sibling
    leaf_name <- leaf$name
    sibling_name <- Get( leaf$siblings, "name" )
    
    err_leaf <- ErrorPrune( (1 - leaf_prob[leaf_class]) * leaf_count, leaf_count, errcf )
    err_sibling <- ErrorPrune( (1 - sibling_prob[sibling_class]) * sibling_count, sibling_count, errcf )
    err_parent <- ErrorPrune( (1 - parent_prob[parent_class]) * parent_count, parent_count, errcf )
    
    if_prune <- err_parent <= (err_leaf * leaf_count / parent_count + err_sibling * sibling_count / parent_count)

    # Check if collapsing is needed
    if( if_prune & leaf_isLeaf == sibling_isLeaf ){
      
      # Remove children
      parent$RemoveChild( sibling_name )
      parent$RemoveChild( leaf_name )
      
      # Assign leaf flag
      parent$Leaf <- "*"
      
    }else{
      
      # Update list of the visited leaves
      If_all <- c( If_all, leaf$pathString )
      
    }
    
  }
  
}