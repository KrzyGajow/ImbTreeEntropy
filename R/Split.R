# Function for numerical attributes
SplitLocalNum <- function( variable, target, measure_parent, indexes, k_unique, min_obs, type, entropy_par, cp, n_cores, weights, cost, overfit ){

  # Number of observation in parent node
  n_obs <- length(variable)

  # Sequential processing
  if( n_cores == 1 ){

    # Prepare table with results
    results <- data.frame( matrix(0, length(k_unique), 13) )
    colnames(results) <- c( "value", "value_left", "value_right", "split", "split_rest", "n_left", "n_right", 
                            "balance", "l_class_error", "r_class_error", "same_class", "l_prob_peak", "r_prob_peak" )

    # Loop for each possible split in particular node, it might have less values than in all dataset
    for( i in seq_along(k_unique) ){

      # Split variable into two parts, i.e. less or equal to the split point and greater then the split point
      variable_temp <- variable <= k_unique[i]
      
      # Calculate number of observations in each child
      n_obs_child <- sum( variable_temp )
      n_obs_child <- c( n_obs_child, n_obs - n_obs_child)

      # If there is less observation than a threshold in a given child there is no point to perform calculation
      if( any( n_obs_child < min_obs ) ){
        
        # Update result matrix for each split point
        results[i, ] <- 0
        
      }else{
        
        # Choose learning type
        temp <- InformationGainCpp( variable_temp, n_obs, target, length(levels(target)), measure_parent, entropy_par, cp, type, weights, cost, overfit )
        
        # Update result matrix for each split point
        results[i, "value"] <- temp["gain"]
        results[i, "value_left"] <- temp["left_value"]
        results[i, "value_right"] <- temp["right_value"]
        results[i, "split"] <- k_unique[i]
        results[i, "n_left"] <- n_obs_child[1]
        results[i, "n_right"] <- n_obs - results[i, "n_left"]
        results[i, "balance"] <- abs( 0.5 - results[i, "n_left"] / n_obs )
        results[i, "l_class_error"] <- temp["l_class_error"]
        results[i, "r_class_error"] <- temp["r_class_error"]
        results[i, "same_class"] <- temp["same_class"]
        results[i, "l_prob_peak"] <- temp["l_prob_peak"]
        results[i, "r_prob_peak"] <- temp["r_prob_peak"]
        
      }

    }

  }else{# Parallel processing

    # Define variables that should be exported into each cluster
    clusterExport(Global_Cluster, c("InformationGain", "Entropy", "CalcProb", "weights", "cost", "cp", "variable", 
                                    "target", "measure_parent", "indexes", "entropy_par", "type", "k_unique", "n_obs",
                                     "ClassErrorLocal", "AvoidSameClass", "overfit", "InformationGainCpp"), 
                  envir = environment())

    # Loop for each possible split in particular node, it might have less values than in all dataset
    results <- parSapply(cl = Global_Cluster, seq_along(k_unique), simplify = F, function(i){

      # Prepare table with results
      out <- data.frame( matrix(0, 1, 13) )
      colnames(out) <- c( "value", "value_left", "value_right", "split", "split_rest", "n_left", "n_right", 
                          "balance", "l_class_error", "r_class_error", "same_class", "l_prob_peak", "r_prob_peak" )

      # Split variable into two parts, i.e. less or equal to the split point and greater then the split point
      variable_temp <- variable <= k_unique[i]
      
      # Calculate number of observations in each child
      n_obs_child <- sum( variable_temp )
      n_obs_child <- c( n_obs_child, n_obs - n_obs_child)
      
      # If there is less observation than a threshold in a given child there is no point to perform calculation
      if( any( n_obs_child < min_obs ) ){
        
        # Update result matrix for each split point
        out[, ] <- 0
        
      }else{
        
        # Choose learning type
        temp <- InformationGainCpp( variable_temp, n_obs, target, length(levels(target)), measure_parent, entropy_par, cp, type, weights, cost, overfit )
        
        # Update result matrix for each split point
        out[, "value"] <- temp["gain"]
        out[, "value_left"] <- temp["left_value"]
        out[, "value_right"] <- temp["right_value"]
        out[, "split"] <- k_unique[i]
        out[, "n_left"] <- n_obs_child[1]
        out[, "n_right"] <- n_obs - out[,"n_left"]
        out[, "balance"] <- abs( 0.5 - out[,"n_left"] / n_obs )
        out[, "l_class_error"] <- temp["l_class_error"]
        out[, "r_class_error"] <- temp["r_class_error"]
        out[, "same_class"] <- temp["same_class"]
        out[, "l_prob_peak"] <- temp["l_prob_peak"]
        out[, "r_prob_peak"] <- temp["r_prob_peak"]
        
      }

      return( out )

    })

    # Transform results from listo into table
    results <- do.call("rbind", results)

    # Adjustment when there is no observation in a particular child node
    if(is.null(results)){

      # Create dummy / empty table with results
      results <- data.frame( matrix(0, 0, 13) )
      colnames(results) <- c( "value", "value_left", "value_right", "split", "split_rest", "n_left", "n_right", 
                              "balance", "l_class_error", "r_class_error", "same_class", "l_prob_peak", "r_prob_peak" )

    }

  }

  return( results )

}

# Function for nominal attributes
SplitLocalFac <- function( variable, target, measure_parent, indexes, k_unique, min_obs, type, entropy_par, cp, n_cores, weights, cost, overfit ){

  # Number of observation in parent node
  n_obs <- length(variable)

  # Sequential processing
  if( n_cores == 1 ){

    # Prepare table with results
    results <- data.frame( matrix(0, length(k_unique), 13) )
    colnames(results) <- c( "value", "value_left", "value_right", "split", "split_rest", "n_left", "n_right", 
                            "balance", "l_class_error", "r_class_error", "same_class", "l_prob_peak", "r_prob_peak" )

    # Loop for each possible split in particular node, it might have less values than in all dataset
    for( i in seq_along(k_unique) ){

      # Split variable into two parts, i.e. less or equal to the split point and greater then the split point
      variable_temp <- variable %in% k_unique[1:i]
      
      # Calculate number of observations in each child
      n_obs_child <- sum( variable_temp )
      n_obs_child <- c( n_obs_child, n_obs - n_obs_child)
      
      # If there is less observation than a threshold in a given child there is no point to perform calculation
      if( any( n_obs_child < min_obs ) ){
        
        # Update result matrix for each split point
        results[i, ] <- 0
        
      }else{
        
        # Choose learning type
        temp <- InformationGainCpp( variable_temp, n_obs, target, length(levels(target)), measure_parent, entropy_par, cp, type, weights, cost, overfit )
        
        # Update result matrix for each split point
        results[i, "value"] <- temp["gain"]
        results[i, "value_left"] <- temp["left_value"]
        results[i, "value_right"] <- temp["right_value"]
        results[i, "split"] <- paste0( "(", paste0( k_unique[1:i], collapse = "," ) ,")" )
        results[i, "n_left"] <- n_obs_child[1]
        results[i, "n_right"] <- n_obs - results[i, "n_left"]
        results[i, "balance"] <- abs( 0.5 - results[i, "n_left"] / n_obs )
        results[i, "l_class_error"] <- temp["l_class_error"]
        results[i, "r_class_error"] <- temp["r_class_error"]
        results[i, "same_class"] <- temp["same_class"]
        results[i, "l_prob_peak"] <- temp["l_prob_peak"]
        results[i, "r_prob_peak"] <- temp["r_prob_peak"]
        
      }
      
    }
    
  }else{# Parallel processing

    # Define variables that should be exported into each cluster
    clusterExport(Global_Cluster, c("InformationGain", "Entropy", "CalcProb", "weights", "cost", "cp", "variable", 
                                    "target", "measure_parent", "indexes", "entropy_par", "type", "k_unique", "n_obs",
                                    "ClassErrorLocal", "AvoidSameClass", "overfit", "InformationGainCpp"), 
                  envir = environment())

    # Loop for each possible split in particular node, it might have less values than in all dataset
    results <- parSapply(cl = Global_Cluster, seq_along(k_unique), simplify = F, function(i){

      # Prepare table with results
      out <- data.frame( matrix(0, 1, 13) )
      colnames(out) <- c( "value", "value_left", "value_right", "split", "split_rest", "n_left", "n_right", 
                          "balance", "l_class_error", "r_class_error", "same_class", "l_prob_peak", "r_prob_peak" )

      # Split variable into two parts, i.e. less or equal to the split point and greater then the split point
      variable_temp <- variable %in% k_unique[1:i]
      
      # Calculate number of observations in each child
      n_obs_child <- sum( variable_temp )
      n_obs_child <- c( n_obs_child, n_obs - n_obs_child)
      
      # If there is less observation than a threshold in a given child there is no point to perform calculation
      if( any( n_obs_child < min_obs ) ){
        
        # Update result matrix for each split point
        out[, ] <- 0
        
      }else{
        
        # Choose learning type
        temp <- InformationGainCpp( variable_temp, n_obs, target, length(levels(target)), measure_parent, entropy_par, cp, type, weights, cost, overfit )
        
        # Update result matrix for each split point
        out[, "value"] <- temp["gain"]
        out[, "value_left"] <- temp["left_value"]
        out[, "value_right"] <- temp["right_value"]
        out[, "split"] <- paste0( "(", paste0( k_unique[1:i], collapse = "," ) ,")" )
        out[, "n_left"] <- n_obs_child[1]
        out[, "n_right"] <- n_obs - out[,"n_left"]
        out[, "balance"] <- abs( 0.5 - out[,"n_left"] / n_obs )
        out[, "l_class_error"] <- temp["l_class_error"]
        out[, "r_class_error"] <- temp["r_class_error"]
        out[, "same_class"] <- temp["same_class"]
        out[, "l_prob_peak"] <- temp["l_prob_peak"]
        out[, "r_prob_peak"] <- temp["r_prob_peak"]
        
      }
      
      return( out )

    })

    # Transform results from listo into table
    results <- do.call("rbind", results)

    # Adjustment when there is no observation in a particular child node
    if( is.null(results) ){

      # Create dummy / empty table with results
      results <- data.frame( matrix(0, 0, 13) )
      colnames(results) <- c( "value", "value_left", "value_right", "split", "split_rest", "n_left", "n_right", 
                              "balance", "l_class_error", "r_class_error", "same_class", "l_prob_peak", "r_prob_peak" )

    }
    
  }

  return( results )

}

PCAorder <- function( target, variable ){

  # If less than 2 levels there is no sense to do it
  if( nlevels(droplevels(variable) ) < 2 ) {

    return( as.character( levels(droplevels(variable) ) ) )

  }

  ## Create contingency table of the nominal outcome with the nominal covariate
  N <- table( droplevels(variable), droplevels(target) ) #na cost dorobić!!!!!!!

  ## PCA of weighted covariance matrix of class probabilites
  # Class probability matrix
  P <- N / rowSums(N)

  # Weighted covariance matrix
  S <- cov.wt( P, wt = rowSums(N) )$cov

  # First principal component
  pc1 <- prcomp( S, rank. = 1 )$rotation

  # Score
  score <- P %*% pc1

  ## Return ordered factor levels
  return( as.character( levels( droplevels(variable) )[ order(score) ] ) )

}
