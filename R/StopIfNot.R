StopIfNot <- function( Y_name, X_names, data, depth, min_obs, type, entropy_par, cp, n_cores, weights, cost, 
                       class_th, overfit, cf, amb_prob, top_split, var_lev, amb_class, amb_class_freq ){

  if( !type %in% c( "Shannon", "Renyi", "Tsallis", "Sharma-Mittal", "Sharma-Taneja", "Kapur" ) ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Type should be in one of the: %s.", paste0( c( "Shannon", "Renyi", "Tsallis", "Sharma-Mittal", "Sharma-Taneja", "Kapur" ) , collapse = ", ")) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }
  
  if( type %in% c( "Sharma-Mittal", "Sharma-Taneja", "Kapur" ) & length(entropy_par) != 2 ){
    
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( "Entropy_par should have two values" )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )
    
  }
  
  if( type %in% c( "Sharma-Taneja" ) & entropy_par[1] == entropy_par[2] ){
    
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( "For Sharma-Taneja entropy alpha != beta" )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )
    
  }
  
  if( type %in% c( "Kapur" ) & ( entropy_par[1] == 1 | any(entropy_par <=0) | (entropy_par[1] + entropy_par[2] - 1 <= 0) ) ){
    
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( "For Kapur entropy alpha != 1, alpha and beta > 0, alpha + beta - 1 > 0" )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )
    
  }
  
  if( !Y_name %in% colnames(data) ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Traget variable %s does not exist in the table", Y_name ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }

  if( depth < 1 ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Parameter depth should be equal or greater than 1" ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }

  if( min_obs < 1 ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Parameter min_obs should be equal or greater than 1" ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }

  if( cp < 0 ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Parameter cp should not be negative" ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }

  if( !is.factor( data[, Y_name] ) ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Traget variable %s should be a factor.", Y_name ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }

  if( !class_th %in% c("equal", "theoretical","tuned")  ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Class_th should be in one of the: equal, theoretical, tuned" ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }

  if( !is.null(weights) & !is.null(cost) ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Choose only one of the: weights, cost." ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }

  if( is.null(cost) & !class_th %in% c("equal", "tuned") ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Set up both parameters: class_th in (equal, tuned), cost." ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }
  
  if( !overfit %in% c("none", "leafcut", "avoid", "prune") ){
    
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Overfit should be in one of the: none, leafcut, prune, avoid" ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )
    
  }
  
  if( length(amb_class) != length(amb_class_freq) ){
    
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Amb_class and amb_class_freq should have the same length" ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )
    
  }
  
  if( top_split <= 1 ){
    
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Top_split should be greater than 1" ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )
    
  }
  
  if( !(cf > 0 & cf < 1) ){
    
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Cf should be in (0,1)" ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )
    
  }
  
  if( !var_lev %in% c(T,F) ){
    
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Var_lev should be in TRUE or FALSE" ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )
    
  }
  
  if( !is.null(cost) ){

    if( is.null( rownames(cost) ) | is.null( colnames(cost) ) ){

      cat("\n\n********** PROGRAM TERMINATED **********\n\n")
      cat( sprintf( "Rows and columns names of the cost matrix should not be NULL" ) )
      cat("\n\n********** PROGRAM TERMINATED **********\n\n")
      return( F )

    }

    if( !all( rownames(cost) %in% levels(data[,Y_name]) ) & !all( rownames(cost) %in% colnames(cost) ) ){

      cat("\n\n********** PROGRAM TERMINATED **********\n\n")
      cat( sprintf( "Rows and columns names of the cost matrix should be in %s.", paste0( levels(data[,Y_name]), collapse = ", " ) ) )
      cat("\n\n********** PROGRAM TERMINATED **********\n\n")
      return( F )

    }

    if( any( cost < 0 ) ){

      cat("\n\n********** PROGRAM TERMINATED **********\n\n")
      cat( "Costs should not be less than 0" )
      cat("\n\n********** PROGRAM TERMINATED **********\n\n")
      return( F )

    }

    if( any( diag(cost) != 0) ){

      cat("\n\n********** PROGRAM TERMINATED **********\n\n")
      cat( sprintf( "Diagonal elements of the cost matrix should be 0" ) )
      cat("\n\n********** PROGRAM TERMINATED **********\n\n")
      return( F )

    }

  }

  if( !is.null(weights) ){

    if( any( weights < 1 ) ){

      cat("\n\n********** PROGRAM TERMINATED **********\n\n")
      cat( sprintf( "Weights should not be less than 1" ) )
      cat("\n\n********** PROGRAM TERMINATED **********\n\n")
      return( F )

    }

  }

  isNA <- sapply( data, anyNA )
  if( any( isNA ) ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "The following attributes have missing values: %s.", paste0( names(isNA)[isNA], collapse = ", ") ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }

  return( T )

}
