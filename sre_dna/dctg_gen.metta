;                                           (convert-to-metta-file  dctg_gen $_448246 sre_dna/dctg_gen.pl sre_dna/dctg_gen.metta)
;
; DCTG based tree generation for GP
;
; Brian Ross
;
; January, 1999

;
; generate_tree(TopGoal, TreeType, MaxDepth, UserArgs, Tree, Expr):
;
; TopGoal - name of top of tree to generate
;
; TreeType - either grow or full
;
; MaxDepth - maximum depth of tree
;
; UserArgs - list of user-specified args to use in top of rule
;
; Tree - Resulting tree, in DCTG node structure
;
; Expr - DCTG expression list equivalent of Tree
;
; 
;
; Generates a tree of Type and max Depth, with UserArgs used in head of rule.
;
; With the node structure of Tree, it should be possible to access all
;
; semantic rules. Any embedded MeTTa in DCTG definition of rules is executed
;
; as tree is generated; this permits Expr to be constructed. This means
;
; that user must ensure embedded MeTTa goals will execute correctly.
;
; The node structure inserted into call has it's ID removed, to permit random
;
; selection. Any user arg structures in the tree structure are not used
;
; afterwards. 'verification' must be called to account for them.


  (= (generate-tree $TopGoal $TreeType $MaxDepth $UserArgs $Tree $Expr)  
    ( (with-self  
        (fast *) 
        (dctg-rule-info $TopGoal $_ $RuleHead $_ $_)) 
      (=.. $RuleHead 
        (Cons  $Name $Args)) 
      (append $_ 
        (:: 
          (node $X $Y $_) $Expr $_) $Args) 
      (= $Tree 
        (node $X $Y $_)) 
      (append $UserArgs 
        (:: $Tree $Expr Nil) $Args2) 
      (=.. $RuleHead2 
        (Cons  $Name $Args2)) 
      (set-det) 
      (once (gen-tree $RuleHead2 $TreeType $MaxDepth))))
; ; enforce empty diff list
; ; new

;
; gen_tree(RuleHead, TreeType, Depth):
;
; RuleHead - Head of rule at root of tree
;
; TreeType - full or grow
;
; Depth - current depth to make tree


  (= (gen-tree $RuleHead $TreeType $Depth)  
    ( (is $Depth2 
        (- $Depth 1)) 
      (select-random-rule $TreeType $Depth $RuleHead) 
      (==  
        (= $RuleHead $Body) 
        (get-atoms  &self)) 
      (process-goals $Body $TreeType $Depth2)))
; ; select_random_rule(TreeType, Depth2, RuleHead),

;
; process_goals(Goals, TreeType, Depth):
;
; Goals - goals to generate subtrees for
;
; TreeType - grow or full
;
; Depth - current depth to make tree


  (= (process-goals (, $A $B) $TreeType $Depth)  
    (set-det)
    (process-goals $A $TreeType $Depth)
    (process-goals $B $TreeType $Depth))
	;
; !.
  (= (process-goals $A $TreeType $Depth)  
    (det-if-then-else 
      (is-a-rule-call $A) 
      (gen-tree $A $TreeType $Depth) 
      (call $A)))

;
; select_random_rule(TreeType, MaxDepth, RuleHead):
;
; TreeType - grow or full
;
; MaxDepth - maximum depth of resulting tree
;
; RuleHead - Head of selected rule
;
; 
;
; Randomly select a rule for given RuleHead structure, based on type and
;
; max depth. The RuleHead args are unified with head of rule to use.
;
; On backtracking, new selections are tried. Random selection done by
;
; generating a shuffled list of rules to try, and each is tried in succession.
;
; Possible for this to fail, depending on MaxDepth value.


  (= (select-random-rule $TreeType $MaxDepth $RuleHead)  
    ( (=.. $RuleHead 
        (Cons  $RuleName $_)) 
      (shuffle-rule-list $RuleName $TreeType $RuleList) 
      (member $ID $RuleList) 
      (with-self  
        (fast *) 
        (dctg-rule-info $_ $ID $RuleHead $MinDepth $_)) 
      (=< $MinDepth $MaxDepth) 
      (set-det)))
; ; may backtrack to here
; ; otherwise exceeds Depth; forces backtracking ;
; new: June 11/99

;
; shuffle_rule_list(RuleName, Type, RuleList):
;
; RuleName - name of rule to make list for
;
; Type - grow or full
;
; RuleList - shuffled list of rule ID's
;
; 
;
; Returns a shuffled list of rule ID's for given rule.
;
; Grow tree has terminal and nonterminal rules shuffled together.
;
; Full tree has nonterminals shuffled first, followed by terminals.
;
; Idea is that rules will be tried one after another from this list.


  (= (shuffle-rule-list $RuleName grow $RuleList)  
    ( (with-self  
        (fast *) 
        (dctg-id-table $RuleName $IDList $_ $_)) 
      (random-permutation $IDList $RuleList) 
      (set-det)))
  (= (shuffle-rule-list $RuleName full $RuleList)  
    ( (with-self  
        (fast *) 
        (dctg-id-table $RuleName $_ $TermList $NontermList)) 
      (random-permutation $TermList $T1) 
      (random-permutation $NontermList $T2) 
      (append $T2 $T1 $RuleList) 
      (set-det)))
; ; nonterms have precedence


