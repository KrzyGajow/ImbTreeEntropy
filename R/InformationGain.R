InformationGain <- function( variable, target, entropy_parent, entropy_par = 1, cp = 0, type = "Shannon", weights = NULL, cost = NULL, overfit = "prune" ){

  # Create temporary table, if weights and cost are null then then computations are based on counts not sums
  dat <- data.frame( variable, target, weights = ifelse( is.null(weights), 1, weights ) )

  if( !is.null(cost) ){

    # Prepare sums of each row
    costCases <- rowSums( cost )

    # Assign a particular cost to the corresponding class
    for( i in names(costCases) ){

      dat[ dat[, "target"] == i, "weights"] <- costCases[i]

    }

  }

  # Calculate counts/sums of each node (only right or left nodes) and class
  temp <- tapply(dat[, "weights"], list( dat[, "variable"], dat[, "target"] ), sum )
  temp <- ifelse( is.na(temp), 0, temp )

  # Calculate proportions required for the weighted entropy
  split_prop <- rowSums(temp)
  
  # Probability distribution of each leaf
  probability_l <- temp["TRUE",] / sum( temp["TRUE",] )
  probability_r <- temp["FALSE",] / sum( temp["FALSE",] )
  
  # Calculate entropy of each node
  entropy <-  apply( temp, 1, function(x, entropy_par, type){ Entropy(x, entropy_par, type) }, entropy_par = entropy_par, type = type )

  # Entropy of each node
  left <- unname( entropy["TRUE"] )
  right <- unname( entropy["FALSE"] )
  
  # Calculate weighted entropy
  entropy_after <- sum(split_prop / sum(split_prop) * entropy)

  information_gain <- entropy_parent - entropy_after
  
  # Calculate gain ratio taking into account number of possible splits
  intrinsic_information <- -sum( split_prop / sum(split_prop) * log2( split_prop / sum(split_prop) ) )
  gain_ratio <- information_gain / intrinsic_information
  
  # Check out if both leaves chooses the same class
  if( overfit == "avoid" ){
    
    same_class_split <- AvoidSameClass( probability_l, probability_r, cost )
    
  }else{
    
    same_class_split <- 0
    
  }
  
  # Calculate number of incorrectly classified observation of each child
  if( cp > 0 ){
    
    class_error <- ClassErrorLocal( temp )
    
  }else{
    
    class_error <- c( 0, 0 )
    
  }

  return( c(gain = information_gain, left_value = left, right_value = right, l_class_error = class_error[2], r_class_error = class_error[1], same_class = same_class_split ) )

}

Entropy <- function( target, entropy_par = 1, type = "Shannon" ){

  if( type == "Shannon" | (entropy_par[1] == 1 & !type %in% c( "Sharma-Mittal", "Sharma-Taneja", "Kapur" ) ) ){

    # Calculate Shannon entropy
    res <- target / sum(target) * log2(target / sum(target))

    # If there is NA or NaN replace with 0
    res[target == 0] <- 0
    res <- -sum(res)

  }else if( type == "Renyi" & entropy_par[1] != 1 ){

    # Calculate Renyi entropy
    res <- ( 1 / ( 1-entropy_par ) ) * log2( sum( ( target/sum(target) ) ^ entropy_par) )

  }else if( type == "Tsallis" & entropy_par[1] != 1 ){

    # Calculate Tsallis entropy
    res <- ( 1 / ( entropy_par-1 ) ) * (1 - sum( ( target/sum(target) ) ^ entropy_par ) )

  }else if( type == "Sharma-Mittal" ){

    rval <- ifelse( length(entropy_par) == 1, 1, entropy_par[2] )
    qval <- entropy_par[1]

    # Calculate Sharma-Mittal entropy
    res <- ( 1 / (1-rval) ) * ( sum( ( target/sum(target) ) ^ qval ) ^ ( ( 1 - rval ) / ( 1 - qval ) ) )

  }else if( type == "Sharma-Taneja" ){
    
    beta <- ifelse( length(entropy_par) == 1, 1, entropy_par[2] )
    alpha <- entropy_par[1]
    
    # Calculate Sharma-Taneja entropy: alpha != beta
    res <- ( 2 ^ ( 1 - alpha ) - 2 ^ ( 1 - beta ) ) ^ (-1) * ( sum( ( target/sum(target) ) ^ alpha ) - sum( ( target/sum(target) ) ^ beta ) )
    res <- ifelse( alpha == beta, Inf, res)

  }else if( type == "Kapur" ){
    
    beta <- ifelse( length(entropy_par) == 1, 1, entropy_par[2] )
    alpha <- entropy_par[1]
    
    # Calculate Kapur entropy: alpha != 1, alpha and beta > 0, alpha + beta - 1 > 0
    res <- ( 1 / ( 1 - alpha ) ) * log( sum( ( target/sum(target) ) ^ ( alpha + beta - 1 ) ) / sum( ( target/sum(target) ) ^ beta ) )
    
  }

  return( res )

}

ClassErrorGlobal <- function( tree, child, cp ){
  
  # Calculate number of incorrectly classified observation of the entire tree (all leaves)
  leaves <- tree$root$Get( "localerror", filterFun = isLeaf )
  n_leaves <- length(leaves)
  leaves_error <- sum( leaves )
  leaves_error <- ifelse( is.na(leaves_error), 0, leaves_error)
  
  # Take number of incorrectly classified observation of the current leaf
  curr_leaf <- tree$localerror
  
  # Take number of observations (denominator)
  nobs <- tree$root$Count

  # Calculate misclassification rate of the entire tree
  curr_leaf_error <- leaves_error / nobs 

  # Calculate misclassification rate of the tree after adding new split
  new_error <- ( (leaves_error - curr_leaf) + sum(child) ) / nobs

  # Calculate improvement
  # improvement <- curr_leaf_error - new_error 
  cp_curr <- curr_leaf_error + 0 * n_leaves# * (tree$root$localerror / nobs) 
  cp_new <- new_error + 0 * ( n_leaves + 1 )# * (tree$root$localerror / nobs) 
  cp_root <- (tree$root$localerror / nobs)#  + cp * ( 1 ) * (tree$root$localerror / nobs) 
  improvement <- (cp_curr - cp_new) / cp_root

  return( improvement )
  
}

ClassErrorLocal <- function( dat ){
  
  # Calculate number of incorrectly classified observation of each child
  error <- unname( apply( dat, 1, function( x ){ sum(x) - x[ which.max(x) ] } ) )

  return( error )
  
}