AddAttr <- function( tree, data, Y_levels, Y_statistics, min_obs, entropy_par, type, cp, weights, cost, class_th, 
                     thresholds, overfitt, cf, amb_prob, top_split, var_lev, amb_class, amb_class_freq ) {

  # Create list of required attributes and their levels (only for factors)
  req_feat <- unique( tree$Get("feature") )[-1]
  required_features <- vector( "list", length(req_feat) )
  names( required_features ) <- req_feat
  req_feat_lev <- lapply( data, levels )
  required_features <- sapply( req_feat, function(i, dat, lev){ dat[[i]] <- lev[[i]] }, dat = required_features, lev = req_feat_lev, simplify = F )

  attr(tree, "Required_features") <- required_features

  # Levels of the target variable
  attr(tree, "Y_levels") <- Y_levels
  
  # Initial class probabilities
  attr(tree, "Y_statistics") <- c( Y_statistics )
  
  # Which learning algorithm and measure was used: Shannon, Renyi, Tsallis, Sharma-Mittal, Sharma-Taneja, Kapur
  attr(tree, "Learning_type") <- type

  # Q value for Renyi or Tsallis entropies, Q and R for Sharma-Mittal, Alpha and Beta for Sharma-Taneja, Kapur
  attr(tree, "Entropy_par") <- entropy_par

  # Complexity parameter i.e. how much a particular measure of the parent should be decreased to perform a split
  attr(tree, "Cp") <- cp

  # Thresholds for class determining
  attr(tree, "Thresholds") <- thresholds

  # Type of thresholds for class determining
  attr(tree, "Thresholds_type") <- class_th

  # Minimal number (%) of observation in each leaf 
  attr(tree, "Min_obs") <- min_obs
  
  # Weights of each observation
  attr(tree, "Weights_observation") <- weights
  
  # Cost classification matrix
  attr(tree, "Cost_matrix") <- cost
  
  # How to handle overfitting
  attr(tree, "Overfitting") <- overfitt
  
  # Confidence intervals
  attr(tree, "Cf") <- cf
  
  # Ambiguity threshold for the difference between the top two highest class probabilities (only for iteractive learning)
  attr(tree, "Amb_prob") <- amb_prob
  
  # Number of best final trees to be presented. (only for iteractive learning)
  attr(tree, "Top_split") <- top_split
  
  # Decision is make on attribute or split point level (only for iteractive learning)
  attr(tree, "Var_lev") <- var_lev
  
  # Labels of class for which the expert will make a decision during the learning (only for iteractive learning)
  attr(tree, "Amb_class") <- amb_class
  
  # Classes frequencies per node above which the expert will make a decision (only for iteractive learning)
  attr(tree, "Amb_class_freq") <- amb_class_freq

}
