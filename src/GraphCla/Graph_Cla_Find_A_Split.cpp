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

void Graph_Find_A_Split(Multi_Split_Class& OneSplit,
                          const RLT_CLA_DATA& CLA_DATA,
                          const PARAM_GLOBAL& Param,
                          const PARAM_RLT& RLTParam,
                          uvec& obs_id,
                          uvec& var_id,
                          vec& Splitid)
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
  
  DEBUG_Rcout << " --- Reg_Find_A_Split with mtry = " << mtry << std::endl;
  // SVD Decomposition
  int method = 1;
  arma::mat A;
  
  if (method == 1) 
  {// submatrix col same as rows
     A = CLA_DATA.X(obs_id, obs_id);
     OneSplit.SplitVar = obs_id;
  }
  
  if (method == 2) // use all cols 
  {
    if (mtry == P)
    {
      A = CLA_DATA.X(obs_id, var_id);
      OneSplit.SplitVar = var_id;
      
    }else{
        uvec var_try = arma::randperm(P, mtry);
        A = CLA_DATA.X(obs_id, var_id(var_try));  
        OneSplit.SplitVar = var_id(var_try);
      
    }
  }
  
  if (method == 3) // laplacian
  {
    // redefine A = laplacian
    arma::mat A = CLA_DATA.X(obs_id, var_id);
    //A = diagmat(A.each_row( [ ](vec& a){ sum(a); } )) - A;
    DEBUG_Rcout << " laplacian not done yet " << std::endl;
  }
  
  // Centering
  // SVD Decomposition
  arma::mat U; arma::mat V; arma::vec s;
  svd(U,s,V,A);
  // Tempmat contains the first k principle component
  // TempLoading contains the corresponding vector
  
  size_t k = 2;
  
  arma::uvec y = CLA_DATA.Y(obs_id);

  // select the best variable
  for(size_t j = 0; j < k; j++)
  {
    arma::vec TempLoad = V.unsafe_col(j)/s(j);
    arma::uvec TempSplitVar;
    Multi_Split_Class TempSplit(TempLoad, TempSplitVar);
    TempSplit.value = 0;
    TempSplit.score = -1;
     
    Graph_Cla_Split(TempSplit, 
                        U.unsafe_col(j), 
                        y, 
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
      Splitid = U.unsafe_col(j);
    }
     
  }


}