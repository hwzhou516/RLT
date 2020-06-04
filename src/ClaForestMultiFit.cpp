//  **********************************
//  Reinforcement Learning Trees (RLT)
//  Classification
//  **********************************

// my header file
# include "RLT.h"
# include "Utility/Utility.h"
# include "claForest.h"

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export()]]
List ClaForestMultiFit(arma::mat& X,
          					   arma::uvec& Y,
          					   arma::uvec& Ncat,
          					   List& param,
          					   List& RLTparam,
          					   arma::vec& obsweight,
          					   arma::vec& varweight,
          					   int usecores,
          					   int verbose,
          					   arma::umat& ObsTrack)
{
  DEBUG_Rcout << "/// THIS IS A DEBUG MODE OF Classification Multi ///" << std::endl;

  Graph_Forest_Class GRAPH_FOREST()
    
  Graph_Forest_Build((const RLT_REG_DATA&) REG_DATA,
                       REG_FOREST,
                       (const PARAM_GLOBAL&) Param,
                       (const PARAM_RLT&) Param_RLT,
                       obs_id,
                       var_id,
                       ObsTrack,
                       Prediction,
                       OOBPrediction,
                       VarImp,
                       seed,
                       usecores,
                       verbose);
  
  
  return 0;
}