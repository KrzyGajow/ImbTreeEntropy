PredictObservation <- function( tree, observation ){

  # If leaf is reasched return its probability
  if( tree$isLeaf ){

    return( data.frame( t(tree$Probability), tree$Class, stringsAsFactors = F ) )

  }

  # Adjustment for nominal attributes
  if( length(grep("\\(", tree$children[[1]]$value)) > 0 ){
    
    fac_rul <- tree$children[[1]]$value
    fac_rul <- eval( parse( text = paste0( "c(",paste0( paste0( "'", strsplit(substr( fac_rul, 2, nchar(fac_rul)-1 ), "," )[[1]] ), "'", collapse = "," ), ")") ) )
    child <- tree$children[[ ifelse( observation[, tree$children[[1]]$feature] %in% fac_rul, 1, 2) ]]

  }else{

    child <- tree$children[[ ifelse( observation[, tree$children[[1]]$feature] > ( eval( parse(text = tree$children[[1]]$value)) ), 2, 1) ]]

  }

  # Recursive call of the function to traverse the tree
  return( PredictObservation( child, observation ) )

}

#' Predictions from an ImbTreeEntropy Object
#'
#' @param tree Fitted model object. This is assumed to be the result of some function that produces 
#' an object with the same named components as that returned by the ImbTreeEntropy or ImbTreeEntropyInter functions.
#' @param data Data frame of new examples to be predicted.
#' 
#' @return
#' @export PredictTree
#'
#' @examples
#'
#' \dontrun{
#' library("ImbTreeEntropy")
#' data(iris)
#' Tree <- ImbTreeEntropy(Y_name = "Species", 
#'                        X_names = colnames(iris)[-ncol(iris)], 
#'                        data = iris) 
#' Tree <- PredictTree(Tree, iris)
#' }
PredictTree <- function( tree, data ){

  # Check if all required attributes exist in dataset
  required_features <- names( attr(tree, "Required_features") )
  
  if ( is.null( required_features ) ) {
    
    # Take target info
    Y_statistics <- attr(tree, "Y_statistics")
    Y_levels <- attr(tree, "Y_levels")
    
    # Create table with final predictions
    results <- data.frame( matrix( Y_statistics, nrow(data), length( Y_levels ), byrow = T ), Class = Y_levels[ which.max(Y_statistics) ] )
    colnames(results) <- c( Y_levels, "Class" )
    results[, "Class"] <- factor( results[, "Class"], levels = Y_levels )
    
    return( results )
    
  }
  
  if ( !all( required_features %in% colnames(data) ) ) {

    col <- !required_features %in% colnames(data)
    col <- required_features[col]
    stop( sprintf("The following features are required: %s.", paste0(col,collapse = ", ")) )

  }

  # Check if all required levels of a particular nominal attribute exist in dataset
  required_features <- attr(tree, "Required_features")
  required_features <- required_features[ !unlist( lapply( required_features, is.null ) ) ]

  col <- unlist( sapply( names(required_features), function(i, lev, dat){ all( levels( dat[,i] ) %in% lev[[i]] ) },
                       lev = required_features, dat = data, simplify = F) )
  if ( !all( col ) ) {

    col <- names( required_features )[!col]
    stop( sprintf("The following features have to many levels: %s.", paste0(col,collapse = ", ")) )

  }

  # Create table with final predictions
  n_observations <- nrow(data)
  results <- as.data.frame( matrix( 0, n_observations, length( attr(tree, "Y_levels") ) + 1 ) )
  colnames(results) <- c( attr(tree, "Y_levels"), "Class" )

  # Main loop predicting each observation
  for( i in 1:n_observations ){

    results[i, ] <- PredictObservation( tree, data[i, , drop = F] )

  }

  results[, "Class"] <- factor( results[, "Class"], levels = attr(tree, "Y_levels") )

  return( results )

}
