#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix tapplyCpp( NumericVector &tar, NumericVector &var, NumericVector &cost, int nvar, int ntar, int n ){
  
  NumericMatrix res( nvar, ntar );
  
  for( int i = 0; i < nvar; i++ ){
    
    for( int j = 0; j < ntar; j++ ){
      
      int r = 0;
      for( int k = 0; k < n; k++ ){

        if( tar[k] == (j + 1) && var[k] == i ){

          r = r + 1;

        }

      }

      res( i, j ) = r * cost[j] ;

    }
    
  }
  
  return( res );
  
}

// [[Rcpp::export]]
NumericVector EntropyCpp( NumericVector target, NumericVector entropy_par, String type ){

  NumericVector res( target.size() ) ;
  
  if( type == "Shannon" || entropy_par[0] == 1 ){
    
    // Calculate Shannon entropy (adjustment for 2 base)
    res = target / sum(target) * ( log(target / sum(target) ) / log(2) );
    
    // If there is NA or NaN replace with 0
    res[target == 0] = 0;
    res = -sum(res);
    
  }else if( type == "Renyi" && entropy_par[0] != 1 ){
    
    double qval = entropy_par[0];
    
    // Calculate Renyi entropy
    res = ( 1 / ( 1-qval ) ) * ( log( sum( pow( target/sum(target), qval ) ) ) / log(2) );
    
  }else if( type == "Tsallis" && entropy_par[0] != 1 ){
    
    double qval = entropy_par[0];
    
    // Calculate Tsallis entropy
    res = ( 1 / ( qval-1 ) ) * (1 - sum( pow( target/sum(target), qval ) ) );
    
  }else if( type == "Sharma-Mittal" ){
    
    double qval = entropy_par[0];
    double rval = entropy_par[1];
    
    // Calculate Sharma-Mittal entropy
    res  = ( 1 / (1-rval) ) * pow( sum( pow( target/sum(target), qval ) ), ( ( 1 - rval ) / ( 1 - qval ) ) );
    
  }else if( type == "Sharma-Taneja" ){
    
    double alpha = entropy_par[0];
    double beta = entropy_par[1];
    
    // Calculate Sharma-Taneja entropy: alpha != beta
    res = pow( pow( 2,  1 - alpha ) - pow( 2, 1 - beta ), -1 ) * ( sum( pow( target/sum(target), alpha ) ) - sum( pow( target/sum(target), beta ) ) );
    // res = ifelse( alpha == beta, R_PosInf, res);
    
  }else if( type == "Kapur" ){
    
    double alpha = entropy_par[0];
    double beta = entropy_par[1];
    
    // Calculate Kapur entropy: alpha != 1, alpha and beta > 0, alpha + beta - 1 > 0
    res = ( 1 / ( 1 - alpha ) ) * log( sum( pow( target/sum(target), alpha + beta - 1 ) ) / sum( pow( target/sum(target), beta ) ) );
    
  }
  
  return( res );
  
}

// [[Rcpp::export]]
NumericVector ClassErrorLocalCpp( NumericMatrix dat ){
  
  NumericVector error( 2 ) ;
  
  // Calculate number of incorrectly classified observation of each child
  for( int i = 0; i < 2; i++ ){
    
    error[i] = sum( dat( i, _ ) ) - dat( i, which_max( dat( i, _ ) ) );
    
  }
    
  return( error );
    
}

// [[Rcpp::export]]
int AvoidSameClassCpp( NumericVector probs_l, NumericVector probs_r, RObject cost, int k ){

  NumericVector threshold( k );
  if( cost.isNULL() ){

    double k_ = k; 
    
    // Set up equal thresholds
    for( int i = 0; i < k; i++ ){

      threshold[ i ] = 1 / k_;

    }

  }else{

    NumericVector cost_ = as<NumericVector>( cost );
    
    // For multiclass case there are k thresholds
    threshold = 1 / cost_;
    threshold = threshold / sum(threshold);

  }

  // Calculate how much each threshold is exceeded by the probability
  probs_l = probs_l / ( threshold + 0.0000000001 );

  // Take probability of the class which exceeds its threshold most
  double new_class_l = which_max( probs_l );

  // Calculate how much each threshold is exceeded by the probability
  probs_r = probs_r / ( threshold + 0.0000000001 );

  // Take probability of the class which exceeds its threshold most
  double new_class_r = which_max( probs_r );

  // If split is unacceptable then TRUE
  int res = 0;
  if( new_class_l == new_class_r ){

    res = 1;

  }

  return( res );

}

// [[Rcpp::export]]
double ProbPeak( NumericVector prob, int ntar ){
  
  NumericVector probsost = prob.sort();
  
  return( probsost[ntar-1] - probsost[ntar-2] );
  
}
    
// [[Rcpp::export]]
NumericVector InformationGainCpp( NumericVector variable, int n, NumericVector target, int ntar, 
                                  double entropy_parent, NumericVector entropy_par, double cp, String type, 
                                  RObject weights, RObject cost, String overfit ){
  
  NumericVector res(8);
  res.names() = CharacterVector( {"gain","left_value","right_value","l_class_error","r_class_error", 
            "same_class", "l_prob_peak", "r_prob_peak"} );
  int nvar = 2; 

  NumericMatrix temp( nvar, ntar );
  
  if( !cost.isNULL() ){

    NumericMatrix cost_ = as<NumericMatrix>( cost );
    CharacterVector costnames = colnames(cost);

    // Prepare sums of each row
    int nr = cost_.nrow();

    NumericVector costCases( nr );
    for( int i = 0; i < nr; i++ ){

      costCases[ i ] = sum( cost_(i,_) );

    }
    
    // Calculate counts/sums of each node (only right or left nodes) and class
    temp = tapplyCpp( target, variable, costCases, nvar, ntar, n );
    
  }else{

    NumericVector costCases( ntar );
    for( int i = 0; i < ntar; i++ ){
      
      costCases[i] = 1;
      
    }
    
    // Calculate counts/sums of each node (only right or left nodes) and class
    temp = tapplyCpp( target, variable, costCases, nvar, ntar, n );
      
  }
  
  // Calculate proportions required for the weighted entropy
  NumericVector split_prop( nvar );
  for( int i = 0; i < nvar; i++ ){
    
    split_prop[ i ] = sum( temp(i,_) );
    
  }
    
  // Probability distribution of each leaf
  NumericVector probability_l = temp(1,_) / sum( temp(1,_) );
  NumericVector probability_r = temp(0,_) / sum( temp(0,_) );

  // Calculate entropy of each node
  NumericVector entropy( nvar );
  for( int i = 0; i < nvar; i++ ){
    
    entropy[ i ] = as<double>( EntropyCpp( temp(i,_), entropy_par, type ) );
    
  }
  
  // Entropy of each node
  double left = entropy[1];
  double right = entropy[0];
    
  // Calculate weighted entropy
  double entropy_after = sum(split_prop / sum(split_prop) * entropy);
      
  double information_gain = entropy_parent - entropy_after;
      
  // Calculate gain ratio taking into account number of possible splits
  double intrinsic_information = -sum( split_prop / sum(split_prop) * log( split_prop / sum(split_prop) ) );
  double gain_ratio = information_gain / intrinsic_information;
  
  // Check out if both leaves chooses the same class
  int same_class_split = 0;
  if( overfit == "avoid" ){

    same_class_split = AvoidSameClassCpp( probability_l, probability_r, cost, ntar );

  }
  
  // Calculate number of incorrectly classified observation of each child
  NumericVector class_error( 2 );
  if( cp > 0 ){

    class_error = ClassErrorLocalCpp( temp );

  }

  double _1 = ProbPeak( probability_l, ntar );
  double _2 = ProbPeak( probability_r, ntar );
  
  res[0] = gain_ratio;
  res[1] = left;
  res[2] = right;
  res[3] = class_error[1];
  res[4] = class_error[0];
  res[5] = same_class_split;
  res[6] = _1;
  res[7] = _2;
  
  return( res );
  
}
