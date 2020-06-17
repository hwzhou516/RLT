//  **********************************
//  Reinforcement Learning Trees (RLT)
//  Graph Classification
//  **********************************

// my header file
# include "../RLT.h"
# include "../Trees//Trees.h"
# include "../Utility/Utility.h"
# include "../GraphClaForest.h"

using namespace Rcpp;
using namespace arma;

void Graph_Cla_Split(Multi_Split_Class& TempSplit,
                    const vec& x, // x and Y are same length as obs_id 
                    const uvec& Y,
                    double penalty,
                    int split_gen,
                    int split_rule,
                    int nsplit,
                    size_t nmin,
                    double alpha)
{
  size_t N = x.n_elem;
  
  double temp_score;
  
  uvec indices = span(0, N);
  indices = indices(sort_index(x)); // this is the sorted obs_id
  
  // check identical 
  if ( x(indices(0)) == x(indices(N-1)) ) return;
  
  // best split, check all splitting point 
  
  for(int k=0; k < N-1; k++){

    // get the cut-off point based on the variance
    temp_score = graph_cla_score_gini(indices, Y, k);

    if (temp_score > TempSplit.score){
       TempSplit.value = (x(indices(k)) + x(indices(k+1)))/2;
       TempSplit.score = temp_score;
     }
  }
  return;
}


double graph_cla_score_gini(uvec& indices,
                            const uvec& Y,
                            size_t k)
{
  DEBUG_Rcout <<" --- Supervised with Gini score --- "<< std::endl;
  
  double score = 0;
  int N = indices.n_elem;
    
  double leftmean = arma::mean(Y( indices.subvec(0, k) ));
  double rightmean = arma::mean(Y( indices.subvec(k+1, N) ));  
  
  double gini = (k+1)*leftmean*(1-leftmean) + (N-k-1)*rightmean*(1-rightmean);
  
  return 1-gini; // larger the better 
}

double cla_unsuper_score_var(uvec& indices,
                             const vec& x,
                             size_t temp_ind)
{
  DEBUG_Rcout << " --- UnSupervised with Variance score --- "<< std::endl;
  
  double score = 0;
  
  size_t N = indices.size();
  
  return 0;
}

