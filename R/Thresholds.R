SetThresholds <- function( pred, threshold ){

  # Take levels of the target
  levs <- levels( pred[, "target"] )

  # Adjustment for the binary case
  if( length(levs) == 2 & length(threshold) == 1 ){

    threshold <- c(threshold, 1 - threshold)
    names(threshold) <- levs

  }

  # Take only the probability of each class
  probs <- pred[, 1:(ncol(pred)-2)]

  # Calculate how much each threshold is exceeded by the probability
  probs <- sweep( as.matrix(probs), MARGIN = 2, FUN = "/", threshold )
  probs[is.nan(probs)] <- Inf

  # Take probability of the class which exceeds its threshold most (it might happen that all probabilities are greater than their thresholds)
  new_class <- getMaxIndexOfRows( probs, ties.method = "first" )

  class(new_class) <- "factor"
  attr(new_class, "levels") <- levs
  pred[, "pred"] <- new_class
  
  return(pred)

}

TuneThresholds <- function( pred, cost, nsub = 50, control = list() ) {

  # Take levels of the target
  levs <- levels( pred[, "target"] )

  k <- length(levs)

  # Fitness function that should be minimised taking into account cost matrix
  fitfun <- function(x){

    # Set classes for new thresholds
    temp <- SetThresholds(pred, x)

    # Take classification cost from cost matrix
    res <- cost[ match( do.call(paste, temp[, c("target","pred")]), outer(rownames(cost), colnames(cost), FUN = paste) ) ]
    sum( res ) / nrow(temp)

  }

  if( k > 2 ) {

    # Each threshold at the beginning has the same value
    start <- rep(1 / k, k)

    # Parameters of the Simulated Annealing
    ctrl <- list(smooth = FALSE, simple.function = TRUE, max.call = 3000L, temperature = 250, visiting.param = 2.5, acceptance.param = -15)

    # Simulated Annealing algorithm
    res <- GenSA(par = start, fn = fitfun, lower = rep(0, k), upper = rep(1, k), control = ctrl)

    thresholds <- res$par / sum(res$par)
    names(thresholds) <- levs

  }else{

    # Naive multi-start version of optimize for global optimization
    res <- optimizeSubInts(f = fitfun, lower = 0, upper = 1, maximum = F, nsub = nsub)
    thresholds <- res[[1]]

  }
  
  return( c(thresholds = thresholds) )

}

AssignClass <- function( class_th, cost ){

  # Check number of classes
  k <- length( levels(Probability_matrix[, "target"]) )

  if( class_th != "tuned" ){
    
    if( class_th == "equal" ){
      
      thresholds <- if( k == 2 ){ 0.5 }else{ rep( 1/k, k ) }

    }else{
      
      if( k == 2 ){
        
        # For binary case there is only one treshold
        thresholds <- cost[2,1] /( cost[2,1] + cost[1,2] )

      }else{

        # For multiclass case there are k thresholds
        thresholds <- 1 / rowSums( cost )
        thresholds <- thresholds / sum(thresholds)

      }
      
    }

    return( c( thresholds = thresholds ) )

  }
  
  # Create dummy column with the predicted class which will be shortly updated
  Probability_matrix[, "pred"] <<- Probability_matrix[, "target"]

  if( class_th == "equal" ){

    thresholds <- if( k == 2 ){ 0.5 }else{ rep( 1/k, k ) }

    # Determine class based on the equal thresholds
    Probability_matrix <<- SetThresholds( Probability_matrix, thresholds )

  }else if( class_th == "theoretical" ){

    if( k == 2 ){

      # For binary case there is only one treshold
      thresholds <- cost[2,1] /( cost[2,1] + cost[1,2] )

    }else{

      # For multiclass case there are k thresholds
      thresholds <- 1 / rowSums( cost )
      thresholds <- thresholds / sum(thresholds)

    }

    # Determine class based on the theoretical thresholds
    Probability_matrix <<- SetThresholds( Probability_matrix, thresholds )

  }else if( class_th == "tuned" ){

    # Adjustment for empty cost matrix
    if( is.null(cost) ){
      
      class_names <- colnames( Probability_matrix )[ 1:( c(ncol(Probability_matrix)) -2 ) ]
      cost <- matrix( 1, length(class_names), length(class_names), dimnames = list( class_names, class_names ))
      diag(cost) <- 0
      
    }
    
    # Tune thresholds using either Naive multi-start version of optimize for global optimization (binary) or Simulated Annealing algorithm (multiclass)
    thresholds <- TuneThresholds( Probability_matrix, cost )

    # Determine class based on the tuned thresholds
    Probability_matrix <<- SetThresholds( Probability_matrix, thresholds )

  }
  
  return( c(thresholds = thresholds) )

}

UpdateTree <- function( tree ){

  # Prepare path to each leaf
  Leaf_path <- tree$Get("pathString", filterFun = isLeaf)

  # Prepare table with probabilities and classes
  tab <- unique( Probability_matrix[, !colnames( Probability_matrix ) == "target" ] )
  
  if( length(Leaf_path) == 1 ){
    
    # Match probability from leaf and probability matrix
    indx <- sapply( 1:nrow(tab), function( i, dat, vec ){ all( dat[i,] == vec ) }, dat = tab[, -ncol(tab) ], vec = tree$Probability )
    
    # Assign final class to a leaf
    tree$Class <- as.character( tab[indx, "pred"] )
    
  }else{
    
    for( i in 1:length(Leaf_path) ){
      
      # Split path to node parts
      temp <- strsplit( Leaf_path[i], "/" )[[1]]
      
      # Take leaf
      leaf <- eval( parse( text = paste( "tree", paste0( paste0( "'", temp[-1] ), "'", collapse = "$" ), sep = "$" ) ) )
      
      # Match probability from leaf and probability matrix
      indx <- sapply( 1:nrow(tab), function( i, dat, vec ){ all( dat[i,] == vec ) }, dat = tab[, -ncol(tab) ], vec = leaf$Probability )
      
      # Assign final class to a leaf
      leaf$Class <- as.character( tab[indx, "pred"] )
      
    }
    
  }
  
}
