
;
;                         ******************************************************************************

;
;                         *

;
;                         *

;
;                         *  This file describes the learning operators of miles called by xmiles

;
;                         *  when a specific button is pressed

;
;                          *

;
;                          *

;
;                          ******************************************************************************


 

   
;
;                            ******************************************************************************

;
;                            *

;
;                            *        groups(<list of groups>).

;
;                            *

;
;                             ******************************************************************************



  (= 
    (groups  
      (preprocess g1 g2 gencon lgg refinement evaluation truncation)) True) 



;
;                              ******************************************************************************

;
;                              *

;
;                               *        groupdef(<groupname>,<buttonname-list>).

;
;                               *

;
;                               ******************************************************************************



  (= 
    (groupdef  preprocess 
      ('argument types' 'clause heads' 'flatten kb' 'flatten rules' 'unflatten kb')) True)
  (= 
    (groupdef  g1 
      ('g1 op' 'apply g1' identify absorb 'inv derivate' 'most spec v' saturate 'elem saturate')) True)
  (= 
    (groupdef  g2 
      ('intraconstruct 1' 'intraconstruct 2' 'g2 op' 'apply g2')) True)
  (= 
    (groupdef  gencon 
      ('learn constrained' 'learn foil' 'learn rul')) True)
  (= 
    (groupdef  lgg 
      ('gen msg' rlgg lgg 'headed lgg' 'nr lgg' gti)) True)
  (= 
    (groupdef  refinement 
      ('unify vars' 'instantiate vars' 'add body lit' 'new predicate')) True)
  (= 
    (groupdef  evaluation 
      ('correct check' 'complete check' 'evaluate examples' fp 'covered examples')) True)
  (= 
    (groupdef  truncation 
      (reduce unconnected redundant 'flat redundant' unconnecting 'negation based' 'flat negation based' facts)) True)
         



;
;                                     ******************************************************************************

;
;                                     *

;
;                                      *         operatordef(<buttonname>,<operatorname>,<in-out-pattern>,

;
;                                      *	               <in-type-checks>,<out-display-functions>,

;
;                                      *	               <refresh-list>,).

;
;                                      *

;
;                                      ******************************************************************************



  (= 
    (operatordef  'argument types' argument_types () () () 
      (rules)) True)
  (= 
    (operatordef  'clause heads' clause_heads () () () 
      (rules)) True)
  (= 
    (operatordef  'flatten kb' flatten_kb () () () 
      (rules)) True)
  (= 
    (operatordef  'flatten rules' flatten_rules () () () 
      (rules)) True)
  (= 
    (operatordef  'unflatten kb' unflatten_kb () () () 
      (rules)) True)

  (= 
    (operatordef  'g1 op' g1_op 
      (xmarg1 xmarg2 xmout1) 
      (isRule isRule) 
      (resultAddRule) ()) True)
  (= 
    (operatordef  'apply g1' apply_g1 
      (xmarg1 xmout1) 
      (isRule) 
      (resultAddRuleList) ()) True)
  (= 
    (operatordef  identify identify 
      (xmarg1 xmarg2 xmout1) 
      (isExOrRule true) 
      (resultAddRule) ()) True)
  (= 
    (operatordef  absorb absorb 
      (xmarg1 xmarg2 xmout1) 
      (isExOrRule isExOrRule) 
      (resultAddRule) ()) True)

  (= 
    (operatordef  'inv derivate' inv_derivate 
      (xmarg1 xmout1) 
      (isExOrRule) 
      (resultAddRule) ()) True)
  (= 
    (operatordef  'most spec v' most_spec_v 
      (xmarg1 xmarg2 xmout1) 
      (isExOrRule isExOrRule) 
      (resultAddRule) ()) True)
  (= 
    (operatordef  saturate saturate 
      (xmarg1 xmout1 xmoptdepth) 
      (isExOrRule isDepth) 
      (resultAddRule) ()) True)
  (= 
    (operatordef  'elem saturate' elem_saturate 
      (xmarg1 xmarg2 xmout1) 
      (isExOrRule true) 
      (resultAddRule) ()) True)

  (= 
    (operatordef  'intraconstruct 1' intra_construct1 
      (xmarg1 xmarg2 xmout1 xmout2 xmout3) 
      (isRule isRule) 
      (resultAddRule resultAddRule resultAddRule) ()) True)
  (= 
    (operatordef  'intraconstruct 2' intra_construct2 
      (xmarg1 xmarg2 xmout1 xmout2 xmout3) 
      (isRule isRule) 
      (resultAddRule resultAddRule resultAddRule) ()) True)
  (= 
    (operatordef  'g2 op' g2_op 
      (xmarg1 xmarg2 xmout1 xmout2 xmout3) 
      (isRule isRule) 
      (resultAddRule resultAddRule resultAddRule) ()) True)
  (= 
    (operatordef  'apply g2' apply_g2 
      (xmarg1 xmarg2 xmout1 xmout2 xmout3) 
      (isRule isRule) 
      (resultAddRule resultAddRule resultAddRule) ()) True)

  (= 
    (operatordef  'learn constrained' learn_constrained () () () 
      (rules)) True)
  (= 
    (operatordef  'learn foil' learn_foil () () () 
      (rules)) True)
  (= 
    (operatordef  'learn rul' learn_rul () () () 
      (rules)) True)


  (= 
    (operatordef  'gen msg' gen_msg 
      (xmarg1 xmarg2 xmout1) 
      (isRule isRule) 
      (resultAddRule) ()) True)
  (= 
    (operatordef  rlgg rlgg 
      (xmarg1 xmarg2 xmout1) 
      (isRule isRule) 
      (resultAddRule) ()) True)
  (= 
    (operatordef  lgg lgg 
      (xmarg1 xmarg2 xmout1) 
      (isRule isRule) 
      (resultAddRule) ()) True)
  (= 
    (operatordef  'headed lgg' headed_lgg 
      (xmarg1 xmarg2 xmout1) 
      (isRule true) 
      (resultAddRule) ()) True)
  (= 
    (operatordef  'nr lgg' nr_lgg 
      (xmarg1 xmarg2 xmout1) 
      (isRule isRule) 
      (resultAddRule) ()) True)
  (= 
    (operatordef  gti gti 
      (xmarg1 xmarg2 xmout1) 
      (isRule isRule) 
      (resultAddRule) ()) True)

  (= 
    (operatordef  'unify vars' refinement_unify_variables 
      (xmarg1 xmout1) 
      (isRule) 
      (resultAddSpec) ()) True)
  (= 
    (operatordef  'instantiate vars' refinement_instantiate_variables 
      (xmarg1 xmout1) 
      (isRule) 
      (resultAddSpec) ()) True)
  (= 
    (operatordef  'add body lit' refinement_add_body_literal 
      (xmarg1 xmout1) 
      (isRule) 
      (resultAddSpec) ()) True)
  (= 
    (operatordef  'new predicate' specialize_with_newpred 
      (xmarg1 xmout1) 
      (isRule) 
      (resultAddNewpreds) ()) True)

  (= 
    (operatordef  'correct check' correct_chk () () () ()) True)
  (= 
    (operatordef  'complete check' complete_chk () () () ()) True)
  (= 
    (operatordef  'evaluate examples' eval_examples () () () ()) True)
  (= 
    (operatordef  fp fp 
      (xmout1) () 
      (resultSelectRules) ()) True)
  (= 
    (operatordef  'covered examples' all_covered_examples 
      (xmout1) () 
      (resultSelectExamples) ()) True)

  (= 
    (operatordef  reduce reduce_complete 
      (xmarg1) 
      (isRule) () 
      (rules)) True)
  (= 
    (operatordef  unconnected truncate_unconnected 
      (xmarg1) 
      (isRule) () 
      (rules)) True)
  (= 
    (operatordef  redundant truncate_r 
      (xmarg1) 
      (isRule) () 
      (rules)) True)
  (= 
    (operatordef  'flat redundant' truncate_flat_r 
      (xmarg1) 
      (isRule) () 
      (rules)) True)
  (= 
    (operatordef  unconnecting truncate_unconnecting 
      (xmarg1) 
      (isRule) () 
      (rules)) True)
  (= 
    (operatordef  'negation based' truncate_neg_based 
      (xmarg1) 
      (isRule) () 
      (rules)) True)
  (= 
    (operatordef  'flat negation based' truncate_flat_neg_based 
      (xmarg1) 
      (isRule) () 
      (rules)) True)
  (= 
    (operatordef  facts truncate_facts 
      (xmarg1) 
      (isRule) () 
      (rules)) True)


