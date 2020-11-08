AssignInitMeasures <- function( tree, data, Y_statistics, entropy_par, type, weights, cost, class_th ){
  
  # Assign initial inpiurity measure
  tree$measure <- Entropy( Y_statistics, entropy_par, type )
  
  # Observation indexes
  tree$indexes <- 1:nrow(data)
  
  # Depth of the Tree
  tree$depth <- 0
  
  # Node number
  tree$Number <- 1
  
  # Decision number for interactive learning
  tree$Decision <- ""
  
  # Number of observations
  tree$Count <- nrow(data)
  
  # Probabilities
  tree$Probability <- Y_statistics

  # Assign class to the ROOT
  tree$Class <- ChooseClass( Y_statistics, class_th, cost )
  
  # Calculate number of incorrectly classified observations
  tree$localerror <- tree$Count - ( Y_statistics[ tree$Class ] * tree$Count )
  
  # Create decision variable Global environment
  assign( "Decision", 0, envir = .GlobalEnv )
  
}

AssignProbMatrix <- function( data, Y_name, Y_statistics, Y_levels ){
  
  # Duplicate probability vector nrow times
  Probability_matrix <- matrix( Y_statistics, nrow = 1 )
  Probability_matrix <- data.frame( Probability_matrix, target = data[,Y_name], row.names = NULL )

  # Assigne new names
  colnames(Probability_matrix) <- c(Y_levels, "target")
  
  # Create attribute with number of classes
  attr( Probability_matrix, "k" ) <- length(Y_levels)
  
  # Create probability matrix in Global environment
  assign( "Probability_matrix", Probability_matrix, envir = .GlobalEnv )
  
  # Remove local probability matrix
  rm(Probability_matrix)
  
}