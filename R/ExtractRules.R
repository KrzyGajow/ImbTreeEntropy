#' Extract decision rules from an ImbTreeEntropy Object
#'
#' @param tree Fitted model object. This is assumed to be the result of some function that produces an object 
#' with the same named components as that returned by the ImbTreeEntropy or ImbTreeEntropyInter functions.
#' 
#' @return
#' @export ExtractRules
#'
#' @examples
#'
#' \dontrun{
#' library("ImbTreeEntropy")
#' data(iris)
#' Tree <- ImbTreeEntropy(Y_name = "Species", 
#'                        X_names = colnames(iris)[-ncol(iris)], 
#'                        data = iris) 
#' Tree <- ExtractRules(Tree)
#' }
ExtractRules <- function( tree ){
  
  # Take the definition of the rule and transform to the appropriate format (A)
  rules_def <- unname( gsub( pattern = "Root/", replacement = "", tree$Get("pathString", filterFun = isLeaf) ) ) 
  rules_def <- gsub( pattern = "/", replacement = " & ", rules_def )
  
  # Take class of each rule (B)
  rules_class <- unname( tree$Get("Class", filterFun = isLeaf) )
  
  # Combine rule and class (A -> B)
  rules <- paste( rules_def, rules_class, sep = " -> " )
  
  # Take count of each rule 
  rules_count <- unname( tree$Get("Count", filterFun = isLeaf) )
  
  # Take length of each rule 
  rules_length <- tree$Get("depth", filterFun = isLeaf)
  
  # Take probability of each rule 
  rules_prob <- unname( t( tree$Get("Probability", filterFun = isLeaf) ) )
  colnames(rules_prob) <- attr(tree, "Y_levels")
  
  # Take count of each rule 
  class_prob <- attr(tree,"Y_statistics")
  names(class_prob) <- attr(tree,"Y_levels")

  # Take number of observations
  tree_count <- tree$Count 
  
  # Calculate support
  rules_class_prob <- sapply( 1:length(rules_class), function( i, cl, pr ){ pr[ i, colnames(rules_prob) == cl[i] ] }, cl = rules_class, pr = rules_prob )
  AandB <- rules_count * rules_class_prob
  support <- AandB / tree_count
  
  support_class <- sapply( 1:length(rules_class), function( i, cl, pr ){ pr[ names(pr) == cl[i] ] }, cl = rules_class, pr = class_prob )

  # Calculate confidence
  confidence <- AandB / rules_count

  # Calculate lift
  lift <- confidence / support_class
  
  # Calculate conviction
  conviction <- ( 1 - support_class ) / ( 1 - confidence )
  
  # Calculate added value
  addedvalue <- confidence - support_class
    
  # Calculate cosine coefficient
  cosine <- support / sqrt( (rules_count / tree_count) * support_class) 
    
  # Calculate jaccard coefficient
  jaccard <- (support) / ( (rules_count / tree_count) + support_class - support )
  
  # Calculate Laplace accuracy
  laplace <- ( AandB + 1) / ( rules_count + length(class_prob) )
  
  # Calculate Leverage, Piatetsky-Shapiro Measure (PS)
  leverage  <- support - (rules_count / tree_count) * support_class
  
  # Prepare table with results
  results <- data.frame( Rule = rules, Length = rules_length, Support = support, Confidence = confidence, Lift = lift, Conviction = conviction, 
                         AddedValue = addedvalue, Cosine = cosine, Jaccard = jaccard, Laplace = laplace, Leverage = leverage, row.names = NULL )

  return( results )
  
}
