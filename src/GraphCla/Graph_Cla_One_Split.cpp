//  **********************************
//  Reinforcement Learning Trees (RLT)
//  Regression
//  **********************************

// my header file
# include "../RLT.h"
# include "../Trees//Trees.h"
# include "../Utility/Utility.h"
# include "../regForest.h"

using namespace Rcpp;
using namespace arma;

void Cla_Multi_Split(Cla_Split_Class& TempSplit,
                        uvec& obs_id,
                        const vec& x,
                        const vec& Y,
                        const vec& obs_weight,
                        double penalty,
                        int split_gen,
                        int split_rule,
                        int nsplit,
                        size_t nmin,
                        double alpha,
                        bool useobsweight)
{
  size_t N = obs_id.n_elem;

  arma::vec temp_cut_arma;
  double temp_cut;
  size_t temp_ind;
  double temp_score;
  
  uvec indices = obs_id(sort_index(x(obs_id))); // this is the sorted obs_id  
  
  // check identical 
  if ( x(indices(0)) == x(indices(N-1)) ) return;  
  
  for(int k=0; k < N; k++){
    temp_ind = indices(k);
    if(supervise == 1){
      // get the cut-off point based on the variance
        tempt_score = cla_super_score_gini(indices, Y, temp_ind);
     }else{
        tempt_score = cla_unsuper_score_var(indices, x, temp_ind);
     }
  
    if (temp_score > TempSplit.score){
       TempSplit.value = temp_cut;
       TempSplit.score = temp_score;
     }
  }
  return;
}


double cla_super_score_gini(uvec& indices,
                            const vec& Y,
                            size_t temp_ind)
{
  DEBUG_Rcout <<" --- Supervised with Gini score --- "<< std::endl;
  
  double score = 0;

  size_t N = indices.size();
  
  size_t LeftCount = 0;
  size_t RightCouunt = 0;
  
  for (size_t i = 0; i <= temp_ind; i++){
    if(Y(indices(i)) == 1){
      LeftCount += 1
    }
  }
  double p_left = LeftCount;
  for (size_t i = 0; i > temp_ind; i++){
    if(Y(indices(i)) == 1){
      RightCount += 1
    }
  }
  double p_right = RightCount;
  
  double Gini_left = (Y(indices(temp_ind)) - Y(indices(0)))/(Y(indices(N-1)) - Y(indices(0))) * (2 * p_left * (1 - p_left));
  double Gini_right = (Y(indices(N-1)) - Y(indices(temp_ind)))/(Y(indices(N-1)) - Y(indices(0))) * (2 * p_right * (1 - p_right));
  
  return Gini_left + Gini_right;
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

