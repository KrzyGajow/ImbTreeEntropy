#' Print an ImbTreeEntropy Object
#'
#' @param tree Fitted model object. This is assumed to be the result of some function that produces an object 
#' with the same named components as that returned by the ImbTreeEntropy function.
#' 
#' @return
#' @export PrintTree
#'
#' @examples
#' library("ImbTreeEntropy")
#' data(iris)
#' Tree <- ImbTreeEntropy(Y_name = "Species", 
#'                        X_names = colnames(iris)[-ncol(iris)], 
#'                        data = iris) 
#' Tree <- PrintTree(Tree)
PrintTree <- function( tree ){
  
  # Set displaying format for probability vector
  SetFormat(tree, "Probability", formatFun = function(x){ paste0("(",paste0(format(round(x,2), nsmall = 2),collapse = ", "),")") })
  
  # Print final tree
  print( tree, "Number", "Count", "Class", "Probability", "Leaf", limit = 1000 )
  
}

#' Print an ImbTreeEntropyInter Object
#'
#' @param tree Fitted model object. This is assumed to be the result of some function that produces an object 
#' with the same named components as that returned by the ImbTreeEntropyInter function.
#' 
#' @return
#' @export PrintTreeInter
#'
#' @examples
#'
#' \dontrun{
#' library("ImbTreeEntropy")
#' data(iris)
#' Tree <- ImbTreeEntropyInter(Y_name = "Species", 
#'                             X_names = colnames(iris)[-ncol(iris)], 
#'                             data = iris) 
#' Tree <- PrintTreeInter(Tree)
#' }
PrintTreeInter <- function( tree ){
  
  # Set displaying format for probability vector
  SetFormat(tree, "Probability", formatFun = function(x){ paste0("(",paste0(format(round(x,2), nsmall = 2),collapse = ", "),")") })
  
  # Print final tree
  print( tree, "Decision", "Number", "Count", "Class", "Probability", "Leaf", limit = 1000 )
  
}