
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ImbTreeEntropy

<!-- badges: start -->

<!-- badges: end -->

Software to build Decision Trees for imbalanced data.

## Installation

You can install the released version of ImbTreeEntropy from
[GitHub](https://github.com/) with:

``` r
library(devtools)
install_github("KrzyGajow/ImbTreeEntropy")
```

## Example

This is a basic example which shows you how to solve a common problem:


```r
library("ImbTreeEntropy")
library("caret")

data(iris)

# Original dataset, multiclass classification, only numeric attributes
iris

# Predicting 1 as 0 will be penalized 5 times
class_cost_bin <- matrix( c(0,5,1,0), 2, 2, dimnames = list( 0:1, 0:1 ) )

# Predicting Setosa is very easy, Versicolor will have cost 5 for Virginica, Virginica will have cost 10 for Versicolor
class_cost_mult <- matrix( c(0,1,1,1,0,10,1,5,0), 3, 3, dimnames = list( levels(iris$Species), levels(iris$Species) ) )

# Assigning higher weights to those observation which are hard to correctly predict
obs_weights <- c( rep(1, 50), c( rep(1, 20), 5, rep(1, 6), 5, rep(1, 22) ), c( rep(1, 19), 10, rep(1, 9), 10, rep(1, 3), 10, 10, rep(1 ,15) ) )

# Dataset for binary classification, only numeric attributes
iris_2 <- iris
iris_2$Species <- factor( rep(0:1, each = 75) )

# Dataset for binary classification, with one unordered factor attribute
iris_3 <- iris_2
iris_3$Petal.Length <- factor( iris_3$Petal.Length )

# Dataset for binary classification, with one ordered factor attribute
iris_4 <- iris_2
iris_4$Petal.Length <- factor( iris_4$Petal.Length, ordered = T )


# Simulation 1: Default settings
Tree1 <- ImbTreeEntropy(Y_name = "Species", X_names = colnames(iris)[-ncol(iris)], data = iris, depth = 5, min_obs = 5, 
                        type = "Shannon", entropy_par = 1, cp = 0, n_cores = 1, weights = NULL, cost = NULL, 
                        class_th = "equal", overfit = "leafcut", cf = 0.25)

PrintTree(Tree1)
Tree1_pred <- PredictTree(Tree1, iris)
confusionMatrix( Tree1_pred$Class, iris$Species )

# Simulation 2: Original dataset, adding cost matrix
Tree2 <- ImbTreeEntropy(Y_name = "Species", X_names = colnames(iris)[-ncol(iris)], data = iris, depth = 5, min_obs = 5, 
                        type = "Shannon", entropy_par = 1, cp = 0, n_cores = 1, weights = NULL, cost = class_cost_mult, 
                        class_th = "equal", overfit = "leafcut", cf = 0.25)

PrintTree(Tree2)
Tree2_pred <- PredictTree(Tree2, iris)
confusionMatrix( Tree2_pred$Class, iris$Species )

# Simulation 3: Original dataset, adding observation weights
Tree3 <- ImbTreeEntropy(Y_name = "Species", X_names = colnames(iris)[-ncol(iris)], data = iris, depth = 5, min_obs = 5, 
                        type = "Shannon", entropy_par = 1, cp = 0, n_cores = 1, weights = obs_weights, cost = NULL, 
                        class_th = "equal", overfit = "leafcut", cf = 0.25)

PrintTree(Tree3)
Tree3_pred <- PredictTree(Tree3, iris)
confusionMatrix( Tree3_pred$Class, iris$Species )

# Simulation 4: Original dataset, tunned thresholds
Tree4 <- ImbTreeEntropy(Y_name = "Species", X_names = colnames(iris)[-ncol(iris)], data = iris, depth = 5, min_obs = 5, 
                        type = "Shannon", entropy_par = 1, cp = 0, n_cores = 1, weights = NULL, cost = NULL, 
                        class_th = "tuned", overfit = "none", cf = 0.25)

PrintTree(Tree4)
Tree4_pred <- PredictTree(Tree4, iris)
confusionMatrix( Tree4_pred$Class, iris$Species )

# Simulation 5: Original dataset, Renyi entropy with q = 2
Tree5 <- ImbTreeEntropy(Y_name = "Species", X_names = colnames(iris)[-ncol(iris)], data = iris, depth = 5, min_obs = 5, 
                        type = "Renyi", entropy_par = 5, cp = 0, n_cores = 1, weights = NULL, cost = NULL, 
                        class_th = "equal", overfit = "leafcut", cf = 0.25)

PrintTree(Tree5) # compared to Tree1 one more observation is classified correctly
Tree5_pred <- PredictTree(Tree5, iris)
confusionMatrix( Tree5_pred$Class, iris$Species )

# Simulation 6: Original dataset, two parameter Sharma-Mittal entropy with q = 2, r = 1.5
Tree6 <- ImbTreeEntropy(Y_name = "Species", X_names = colnames(iris)[-ncol(iris)], data = iris, depth = 5, min_obs = 5, 
                        type = "Sharma-Mittal", entropy_par = c(3, 3), cp = 0, n_cores = 1, weights = NULL, 
                        cost = NULL, class_th = "equal", overfit = "none", cf = 0.25)

PrintTree(Tree6)
Tree6_pred <- PredictTree(Tree6, iris)
confusionMatrix( Tree6_pred$Class, iris$Species )

# Simulation 7: Original dataset, pre-pruning based on the cp parameter
Tree7 <- ImbTreeEntropy(Y_name = "Species", X_names = colnames(iris)[-ncol(iris)], data = iris, depth = 5, min_obs = 5, 
                        type = "Shannon", entropy_par = 1, cp = 0.45, n_cores = 1, weights = NULL, 
                        cost = NULL, class_th = "equal", overfit = "none", cf = 0.25)

PrintTree(Tree7)
Tree7_pred <- PredictTree(Tree7, iris)
confusionMatrix( Tree7_pred$Class, iris$Species )

# Simulation 8: Original dataset, post-pruning based on the cf parameter
Tree8 <- ImbTreeEntropy(Y_name = "Species", X_names = colnames(iris)[-ncol(iris)], data = iris, depth = 5, min_obs = 5, 
                        type = "Shannon", entropy_par = 1, cp = 0, n_cores = 1, weights = NULL, 
                        cost = NULL, class_th = "equal", overfit = "prune", cf = 0.25)

PrintTree(Tree8)
Tree8_pred <- PredictTree(Tree8, iris)
confusionMatrix( Tree8_pred$Class, iris$Species )

# Simulation 9: Original dataset, Shannon entropy weighted by cost matrix, tuned thresholds
Tree9 <- ImbTreeEntropy(Y_name = "Species", X_names = colnames(iris)[-ncol(iris)], data = iris, depth = 5, min_obs = 5, 
                        type = "Shannon", entropy_par = 1, cp = 0, n_cores = 1, weights = NULL, 
                        cost = class_cost_mult, class_th = "tuned", overfit = "leafcut", cf = 0.25)

PrintTree(Tree9)
Tree9_pred <- PredictTree(Tree9, iris)
confusionMatrix( Tree9_pred$Class, iris$Species )

# Simulation 10: Binary classification, Tsallis, only numeric attributes, parallel processing
Tree10 <- ImbTreeEntropy(Y_name = "Species", X_names = colnames(iris_2)[-ncol(iris_2)], data = iris_2, depth = 5, 
                         min_obs = 5, type = "Tsallis", entropy_par = 0.5, cp = 0, n_cores = 10, weights = NULL, 
                         cost = NULL, class_th = "equal", overfit = "leafcut", cf = 0.25) 

PrintTree(Tree10)
Tree10_pred <- PredictTree(Tree10, iris_2)
confusionMatrix( Tree10_pred$Class, iris_2$Species, positive = "1" )

# Simulation 11: Binary classification, Tsallis, with one unordered factor attribute, theoretical thresholds, adding cost matrix, parallel processing
Tree11 <- ImbTreeEntropy(Y_name = "Species", X_names = colnames(iris_3)[-ncol(iris_3)], data = iris_3, depth = 5, 
                         min_obs = 5, type = "Tsallis", entropy_par = 0.5, cp = 0, n_cores = 10, weights = NULL, 
                         cost = class_cost_bin, class_th = "theoretical", overfit = "leafcut", cf = 0.25) 

PrintTree(Tree11)
Tree11_pred <- PredictTree(Tree11, iris_3)
confusionMatrix( Tree11_pred$Class, iris_3$Species, positive = "1" )

# Simulation 12: Interactive learning, original dataset, based on the probability peaks, 1 means that the whole tree is built based on the expert decision,
# top 4 splits on attribute level, default prunning based on the leaf cut

# Choosing sequence: 4, 3, 2, 1, 1
Tree12 <- ImbTreeEntropyInter(Y_name = "Species", X_names = colnames(iris)[-ncol(iris)], data = iris, depth = 5, 
                              min_obs = 5, type = "Shannon", entropy_par = 1, cp = 0, n_cores = 1, weights = NULL, 
                              cost = NULL, class_th = "equal", overfit = "leafcut", cf = 0.25, 
                              amb_prob = 1, top_split = 4, var_lev = T, amb_class = NULL, amb_class_freq = NULL ) 

PrintTreeInter(Tree12)
Tree12_pred <- PredictTree(Tree12, iris)
confusionMatrix( Tree12_pred$Class, iris$Species )

# Simulation 13: Interactive learning, original dataset, based on the probability peaks, 
# top 4 splits on the attribute level, prunning based on the cp

# Choosing sequence: 4, 3, 3, 2
Tree13 <- ImbTreeEntropyInter(Y_name = "Species", X_names = colnames(iris)[-ncol(iris)], data = iris, depth = 5, 
                              min_obs = 5, type = "Shannon", entropy_par = 1, cp = 0.1, n_cores = 1, weights = NULL, 
                              cost = NULL, class_th = "equal", overfit = "leafcut", cf = 0.25, 
                              amb_prob = 1, top_split = 4, var_lev = T, amb_class = NULL, amb_class_freq = NULL ) 

PrintTreeInter(Tree13)
Tree13_pred <- PredictTree(Tree13, iris)
confusionMatrix( Tree13_pred$Class, iris$Species )

# Simulation 14: Interactive learning, original dataset, based on the class frequencies per node, 0 means that the whole tree is built based on the expert decision
# top 4 splits on the for each split of the attribute, default prunning based on the leaf cut, 
# desired classes (versicolor, virginica) with the frequencies (0.5,0.1)

# Choosing sequence: 3, 2, 4
Tree14 <- ImbTreeEntropyInter(Y_name = "Species", X_names = colnames(iris)[-ncol(iris)], data = iris, depth = 5, 
                              min_obs = 5, type = "Shannon", entropy_par = 1, cp = 0, n_cores = 1, weights = NULL, 
                              cost = NULL, class_th = "equal", overfit = "leafcut", cf = 0.25, amb_prob = 1, top_split = 4, 
                              var_lev = F, amb_class = c("versicolor", "virginica"), amb_class_freq = c(0.5,0.1) ) 

PrintTreeInter(Tree14)
Tree14_pred <- PredictTree(Tree14, iris)
confusionMatrix( Tree14_pred$Class, iris$Species )

# Simulation 15: Extracting rules
ExtractRules(Tree1)
```