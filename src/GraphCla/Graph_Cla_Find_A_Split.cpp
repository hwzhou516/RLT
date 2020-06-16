//  **********************************
//  Reinforcement Learning Trees (RLT)
//  Graph Classification
//  **********************************

// my header file
# include "../RLT.h"
# include "../Trees//Trees.h"
# include "../Utility/Utility.h"
# include "../regForest.h"

using namespace Rcpp;
using namespace arma;

void Graph_Find_A_Split(Multi_Split_Class& OneSplit,
                          const RLT_CLA_DATA& CLA_DATA,
                          const PARAM_GLOBAL& Param,
                          const PARAM_RLT& RLTParam,
                          uvec& obs_id,
                          uvec& var_id)
{
  size_t mtry = Param.mtry;
  size_t nmin = Param.nmin;
  double alpha = Param.alpha;
  int nsplit = Param.nsplit;
  int split_gen = Param.split_gen;
  int split_rule = Param.split_rule;
  
  size_t N = obs_id.n_elem;
  size_t P = var_id.n_elem;

  mtry = ( (mtry <= P) ? mtry:P ); // take minimum
  
  uvec var_try = arma::randperm(P, mtry);
  
  DEBUG_Rcout << " --- Reg_Find_A_Split with mtry = " << mtry << std::endl;
  
  // SVD Decomposition
  arma::mat A = CLA_DATA.X.cols(var_id(var_try));
  arma::mat U; arma::mat V; arma::vec s;
  svd(U,s,V,A);
  
  // Tempmat contains the first k principle component
  // TempLoading contains the corresponding vector
  size_t k = 2;
  arma::mat Tempmat(N,k);
  Tempmat.cols(span(0,k)) = U.cols(span(0,k));
  arma::field<arma::vec> TempLoading(k);
  for(size_t i =0; i<k; i++){
    TempLoading(i) = V.col(i)/s(i);
  }
  
  // select the best variable
  for(size_t j = 0; j < 1; j++)
  {
    Multi_Split_Class TempSplit( TempLoading(j) );
    TempSplit.value = 0;
    TempSplit.score = -1;
      
    Cla_Multi_Split(TempSplit, 
                        obs_id, 
                        Tempmat(j), 
                        CLA_DATA.Y, 
                        0.0, // penalty
                        split_gen, 
                        split_rule, 
                        nsplit, 
                        nmin, 
                        alpha);
    
    if (TempSplit.score > OneSplit.score)
    {
      OneSplit.Loading = TempSplit.Loading;
      OneSplit.value = TempSplit.value;
      OneSplit.score = TempSplit.score;
    }
  }
}