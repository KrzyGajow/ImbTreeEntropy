BestSplitLocal <- function( variable, target, measure_parent, min_obs, type, entropy_par, cp, indexes, n_cores, weights, cost, overfit ){

  # If nominal attribute prepare it in a right manner
  if( is.factor(variable) ){

    # Prepare cross table for all values of the attribute and classes of the target variable
    levels_ord <- as.data.frame.array( prop.table( table(variable, target) ) )

    # If nominal attribute is not ordered then sort categories / values
    if( !is.ordered(variable) ){

      if( length( levels(target) ) == 2 ){

        # According to the increasing ratio of the target variable (simple heuristic rule optimal for Gini and Entropy)
        levels_ord <- levels_ord[ order(levels_ord[,2]) ,]

      }else{

        # According to the score from the Principal Component Analysis
        levels_ordPCA <- PCAorder( target, variable )
        levels_ord <- levels_ord[ levels_ordPCA ,]

      }

    }

    # Remove attribute levels which has no observation in a particular node
    levels_indx <- apply( levels_ord, 1, function(x){ !all( x==0 ) })
    levels_ord <- levels_ord[levels_indx,]

    # Define possible split points
    k_unique <- rownames(levels_ord)
    fac_rest <- k_unique
    k_unique <- head(k_unique, -1)

    # Calculate various statistics of all possible nominal split points
    results <- SplitLocalFac( variable, target, measure_parent, indexes, k_unique, min_obs, type, entropy_par, cp, n_cores, weights, cost, overfit )

    # Adjustment for nominal attribute, define values which fall into the right child
    fac_rest <- sapply( 2:length(fac_rest), function(i, x){ x[i:length(x)] }, x = fac_rest )

    results <- sapply( 1:length(fac_rest), function(i, vec, dat){ dat[i, "split_rest"] <- paste0("(",paste0(vec[[i]], collapse = ","),")"); dat[i,] },
                      vec = fac_rest, dat = results, simplify = F)

    # Transform results from list into table
    results <- do.call("rbind", results)

  }else{

    # Sort in ascending order all possible numerical split points
    k_unique <- sort( unique(variable) )

    # Remove last value because otherwise right child would have no observations, variable <= split point, variable > split point = empty set
    k_unique <- head(k_unique, -1)

    # Calculate various statistics of all possible numerical split points
    results <- SplitLocalNum( variable, target, measure_parent, indexes, k_unique, min_obs, type, entropy_par, cp, n_cores, weights, cost, overfit )

  }

  # Check which splits are possible in terms of control parameters
  if( overfit == "avoid" ){
    
    cond <- results$n_left >= min_obs & results$n_right >= min_obs & results$same_class == 0
    
  }else{
    
    cond <- results$n_left >= min_obs & results$n_right >= min_obs
    
  }
  results_n_pct <- results[ cond, -11, drop = F ]

  # If no possible split prepare dummy table
  if( nrow(results_n_pct) == 0 ){

    results_n_pct[1,] <- 0

  }

  # Choose the best local split
  index_best <- which( max( results_n_pct[, "value"] ) == results_n_pct[, "value"] )

  # If there are more than one best split, choose the one producing the best balanced split
  index_best <- index_best[which.min( results_n_pct[index_best, "balance"] )]
  best_result <- results_n_pct[index_best, , drop = F]

  return( best_result )

}

BestSplitGlobal <- function( data, Y_name, measure_parent, min_obs, type, entropy_par, cp, indexes, n_cores, weights, cost, overfit ) {

  # Choose all possible attributes
  X_variables <- !colnames(data) %in% Y_name

  # Calculate various statistics of all possible best local attributes and split points
  best_splits_local <- sapply( colnames(data)[X_variables], function(x){

      BestSplitLocal( data[,x], data[,Y_name], measure_parent, min_obs, type, entropy_par, cp, indexes, n_cores, weights, cost, overfit )

    }, simplify = F )

  # Transform results from list into table
  best_splits_local <- do.call("rbind", best_splits_local)

  # Check which attributes are possible
  which_possible <- apply( best_splits_local, 1, function(x){ !all(x == 0) } )
  best_splits_local <- best_splits_local[which_possible, , drop = F]

  # If no possible split prepare dummy table
  if( nrow(best_splits_local) == 0 ){

    best_result <- matrix(0, 1, 10)
    colnames(best_result) <- c( "value", "value_left", "value_right", "split", "split_rest", "n_left", "n_right", "balance", "l_class_error", "r_class_error")
    rownames(best_result) <- c("none")

  }else{

    # Choose the best global split
    index_best <- which( max(best_splits_local[, "value"]) == best_splits_local[, "value"] )

    # If there are more than one best split, choose the one producing the best balanced split
    index_best <- index_best[ which.min( best_splits_local[index_best, "balance"] ) ]
    best_result <- best_splits_local[index_best, , drop = F]

  }
  
  return( best_result )

}
