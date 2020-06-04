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

void Cla_Multi_Split_A_Node(size_t Node,
                          Reg_Uni_Tree_Class& OneTree,
                          const RLT_REG_DATA& REG_DATA,
                          const PARAM_GLOBAL& Param,
                          const PARAM_RLT& Param_RLT,
                          uvec& obs_id,
                          uvec& var_id)
{
  Cla_Multi_Terminate_Node();
  
  Cla_Multi_Find_A_Split;
  
  Cla_Multi_Split_A_Node();
  Cla_Multi_Split_A_Node();
}

// terminate and record a node

void Cla_Multi_Terminate_Node(size_t Node, 
                            Reg_Uni_Tree_Class& OneTree,
                            uvec& obs_id,                            
                            const vec& Y,
                            const vec& obs_weight,                            
                            const PARAM_GLOBAL& Param,
                            bool useobsweight)
{
  
 

}
