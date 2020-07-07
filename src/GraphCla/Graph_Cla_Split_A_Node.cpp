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

void Graph_Cla_Split_A_Node(size_t Node,
                            Cla_Multi_Tree_Class& OneTree,
                            const RLT_CLA_DATA& CLA_DATA,
                            const PARAM_GLOBAL& Param,
                            const PARAM_RLT& Param_RLT,
                            uvec& obs_id,
                            uvec& var_id)
{
  size_t N = obs_id.n_elem;
  size_t P = Param.P;
  size_t nmin = Param.nmin;
  bool useobsweight = Param.useobsweight;
  
  if (N < 2*nmin){
    TERMINATENODE:
    DEBUG_Rcout << "  -- Terminate node --" << Node << std::endl;
    Graph_Cla_Terminate_Node(Node, OneTree, obs_id, CLA_DATA.Y, CLA_DATA.obsweight, Param, useobsweight); 
  }else{
    DEBUG_Rcout << "  -- Do split --" << std::endl;
    
    arma::vec Loading = zeros<vec>(N); 
    Multi_Split_Class OneSplit(Loading);
    
    Graph_Find_A_Split(OneSplit, CLA_DATA, Param, Param_RLT, obs_id, var_id);
    
    //DEBUG_Rcout << "-- Found split on variable --" << OneSplit.Loading << " cut " << OneSplit.value << "Var" << obs_id << std::endl;
    
    // store proportion
    OneTree.NodeAve(Node) = arma::mean(CLA_DATA.Y(obs_id));
    
    // if did not find a good split, terminate
    if (OneSplit.score <= 0)
      goto TERMINATENODE;
    
    // construct indices for left and right nodes
    //DEBUG_Rcout << "  -- splitting value is --" << OneSplit.value << std::endl;
    
    uvec left_id(N);
    
    split_id_multi(CLA_DATA.X, OneSplit.Loading, OneSplit.value, left_id, obs_id);  // get the left and right id
    //DEBUG_Rcout << "-- select cont variable --" << OneSplit.var << " split at " << OneSplit.value << std::endl;
    
    // if this happens something about the splitting rule is wrong
    if (left_id.n_elem == N or obs_id.n_elem == N)
      goto TERMINATENODE;
    
    // check if the current tree is long enough to store two more nodes
    // if not, extend the current tree
    if ( OneTree.NodeType( OneTree.NodeType.size() - 2) > 0 )
    {
      DEBUG_Rcout << "  ------------- extend tree length: this shouldn't happen ----------- " << std::endl;
      
      // extend tree structure
      OneTree.extend();
    }
    
    // find the locations of next left and right nodes     
    OneTree.NodeType(Node) = 2; // 0: unused, 1: reserved; 2: internal node; 3: terminal node	
    size_t NextLeft = Node;
    size_t NextRight = Node;
    
    // SplitVar
    uvec LeftVar(N, fill::zeros);
    uvec RightVar(N, fill::zeros);
    LeftVar(left_id) = ones<uvec>(left_id.n_elem);
    RightVar(obs_id) = ones<uvec>(obs_id.n_elem);
    
    // Find next node
    OneTree.find_next_nodes(NextLeft, NextRight); //*
    
    DEBUG_Rcout << "  -- Next Left at --" << NextLeft << std::endl;
    DEBUG_Rcout << "  -- Next Right at --" << NextRight << std::endl;
    
    // record tree 
    OneTree.SplitLoading(Node) = OneSplit.Loading;
    OneTree.SplitValue(Node) = OneSplit.value;
    OneTree.LeftNode(Node) = NextLeft;
    OneTree.RightNode(Node) = NextRight;  
    OneTree.SplitVar(NextLeft) = LeftVar;
    OneTree.SplitVar(NextRight) = RightVar;
    
    OneTree.NodeSize(Node) = left_id.n_elem + obs_id.n_elem;
    
    // split the left and right nodes 
    Graph_Cla_Split_A_Node(NextLeft,
                           OneTree,
                           CLA_DATA,
                           Param,
                           Param_RLT,
                           left_id,
                           left_id);
    
    Graph_Cla_Split_A_Node(NextRight,                          
                           OneTree,
                           CLA_DATA,
                           Param,
                           Param_RLT, 
                           obs_id, 
                           obs_id);
  }
}

// terminate and record a node

void Graph_Cla_Terminate_Node(size_t Node, 
                              Cla_Multi_Tree_Class& OneTree,
                              uvec& obs_id,                            
                              const uvec& Y,
                              const vec& obs_weight,                            
                              const PARAM_GLOBAL& Param,
                              bool useobsweight)
{
  OneTree.NodeType(Node) = 3; // 0: unused, 1: reserved; 2: internal node; 3: terminal node
  OneTree.NodeSize(Node) = obs_id.n_elem;
  
  // DEBUG_Rcout << "terminate Major" << std::endl;
  OneTree.NodeAve(Node) = arma::mean(Y(obs_id));
  
  return;                                      
}
