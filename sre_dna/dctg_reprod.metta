;                                           (convert-to-metta-file  dctg_reprod $_322592 sre_dna/dctg_reprod.pl sre_dna/dctg_reprod.metta)

;
; DCTG based GP reproduction operators: crossover & mutation
;
; Brian Ross
;
; January 25, 1999

;
; crossover(Parent1, Parent2, Child1, Child2):
;
; Parent1, Parent2 - parent trees to reproduce
;
; Child1, Child2 - resulting children
;
; Performs grammar tree expression crossover on two parents.
;
; If internal crossover probability set, then nodes of the specified type
;
; are selected; else all nodes initially counted.
;
; The rules for crossover are:
;
; - only nodes of same rule name from each parent are crossed
;
; - crossover is attempted a max N number of times until successful
;
; (user-specified parameter)
;
; - an attempt fails if the offspring exceed max depth parameter
;
; - if no internal/leaf counting, then counts on all node names done.
;
; - if internal/leaf counting to be done (case 1), then it is done only
;
; for one parent. (If it fails, then 2nd parent tried; if that fails,
;
; then all nodes counted from first parent). Other parent just uses
;
; terminal name count (increases odds that a crossover will be
;
; possible).


  (= (crossover $P1 $P2 $C1 $C2)  
    (prob-internal-crossover-P $PI)
    (not (== $P1 no))
    (det-if-then-else 
      (maybe $PI) 
      (= $Type internal) 
      (= $Type leaf))
    (reprod-P $Tries)
    (or 
      (, 
        (once (count-nodes $P1 $Type $N1)) 
        (= 
          (, $Parent1 $Parent2) 
          (, $P1 $P2))) 
      (, 
        (once (count-nodes $P2 $Type $N1)) 
        (= 
          (, $Parent1 $Parent2) 
          (, $P2 $P1))))
    (do-crossover $Tries $Parent1 $N1 $Parent2 $C1 $C2)
    (set-det))
; ; case 2 
  (= (crossover $P1 $P2 $C1 $C2)  
    (reprod-P $Tries)
    (once (count-nodes $P1 all $N1))
    (do-crossover $Tries $P1 $N1 $P2 $C1 $C2)
    (set-det))
; ; case 1 


  (= (do-crossover 0 $_ $_ $_ $_ $_)  
    ( (set-det) fail))
  (= (do-crossover $_ $Parent1 $N1 $Parent2 $Child1 $Child2)  
    (my-random $N1 $K1)
    (select-subtree $Parent1 $K1 $_ $Child1 $Subtree1 $Subtree2 $NodeName)
    (count-nodes $Parent2 $NodeName $N2)
    (my-random $N2 $K2)
    (select-subtree $Parent2 $K2 $_ $Child2 $Subtree2 $Subtree1 $NodeName)
    (tree-verification $Child1)
    (tree-verification $Child2)
    (set-det))
; ;writel(['A:rand pick ', K1, ' from ', N1, '.', nl]),
; ;writel(['B:rand pick ', K2, ' from ', N2, ' ', NodeName, ' nodes.', nl]),
  (= (do-crossover $Tries $Parent1 $N1 $Parent2 $Child1 $Child2)  
    (is $Tries2 
      (- $Tries 1))
    (do-crossover $Tries2 $Parent1 $N1 $Parent2 $Child1 $Child2)
    (set-det))
; ;writel(['Try ', Tries2, nl]),

;
; check that a new Tree doesn't fail due to
;
; failed embedded code in DCTG rules.


  (= (tree-verification $Child)  
    (det-if-then-else 
      (reprod-verif-P yes) 
      (, 
        (user-args-P $Args) 
        (verification $Child $Args $_)) True)
    (set-det))
; ;writel(['tree_verif: testing child:',nl]),
; ;prettyprint(Child),
; ;writel(['tree_verif: verification succeeded.',nl])

;
; count_nodes(Tree, NodeName, NumNodes):
;
; Tree - DCTG expression structure
;
; NodeName - name of node to count (otherwise: all nodes = 'all';
;
; all internal = 'internal'; all leaf = 'leaf')
;
; NumNodes - number of nodes in Tree
;
; Scans Tree and counts number of nodes.


  (= (count-nodes (node $_ $Children $_) all $NumNodes)  
    (set-det)
    (count-children-nodes $Children all $NumNodes2)
    (is $NumNodes 
      (+ $NumNodes2 1)))
  (= (count-nodes (node $_ $Children $ID) $Type $NumNodes)  
    ( (== $Type internal) 
      (with-self  
        (fast *) 
        (dctg-rule-info $_ $ID $_ $_ nonterminal)) 
      (set-det) 
      (count-children-nodes $Children $Type $NumNodes2) 
      (is $NumNodes 
        (+ $NumNodes2 1))))
  (= (count-nodes (node $_ $Children $ID) $Type $NumNodes)  
    ( (== $Type leaf) 
      (with-self  
        (fast *) 
        (dctg-rule-info $_ $ID $_ $_ terminal)) 
      (set-det) 
      (count-children-nodes $Children $Type $NumNodes2) 
      (is $NumNodes 
        (+ $NumNodes2 1))))
  (= (count-nodes (node $_ $Children $ID) $NodeName $NumNodes)  
    ( (with-self  
        (fast *) 
        (dctg-rule-info $NodeName $ID $_ $_ $_)) 
      (set-det) 
      (count-children-nodes $Children $NodeName $NumNodes2) 
      (is $NumNodes 
        (+ $NumNodes2 1))))
  (= (count-nodes (node $_ $Children $_) $NodeName $NumNodes)  
    (set-det)
    (count-children-nodes $Children $NodeName $NumNodes))
  (= (count-nodes  $_ $_ 0)  True)


  (= (count-children-nodes  () $_ 0)  True)
  (= (count-children-nodes (Cons  $Node $Rest) $NodeName $NumNodes)  
    (count-nodes $Node $NodeName $NumNodes2)
    (count-children-nodes $Rest $NodeName $NumNodes3)
    (is $NumNodes 
      (+ $NumNodes2 $NumNodes3))
    (set-det))

;
; select_subtree(Parent, K, K2, NewParent, SubTree, Hole, NodeName):
;
; Parent - parent tree structure
;
; K - Kth node to select in Parent; must be < number nodes in Parent.
;
; K2 - final K during structure traversal
;
; NewParent - Parent structure with variable Hole in place of removed
;
; subtree Subtree
;
; Subtree - subtree to swap
;
; Hole - location of hole in ParentWithHole (variable)
;
; NodeName - node name of Subtree to select from; if variable, then
;
; select from all nodes
;
; Selects a Kth node in tree for crossover of type NodeName (or all, if
;
; NodeName not set). Sets up the new tree with Hole placeholder for selected
;
; subtree. Hole may be already unified with other parent's subtree.
;
; Cases:
;
; 1. Count = 0, var name --> use that node
;
; 2. Count = 0, name matches given --> use that node
;
; 3. Count > 0, var name or name match -> count and continue
;
; 4. name doesn't match given --> skip and continue
;
; 5. else stop at given count (we've exhausted tree, and we're at
;
; non-node component)


  (= (select-subtree (node $_ $Kids $ID) 1 0 $NewParent (node $NodeName $Kids $ID) $NewParent $NodeName)  
    ( (or 
        (var $NodeName) 
        (with-self  
          (fast *) 
          (dctg-rule-info $NodeName $ID $_ $_ $_))) 
      (set-det) 
      (with-self  
        (fast *) 
        (dctg-rule-info $NodeName $ID $_ $_ $_))))
; ; cases 1, 2
  (= (select-subtree (node $Name $Kids $ID) $K $K2 (node $Name $Kids2 $ID) $Subtree $Hole $NodeName)  
    (or 
      (var $NodeName) 
      (with-self  
        (fast *) 
        (dctg-rule-info $NodeName $ID $_ $_ $_)))
    (set-det)
    (is $K3 
      (- $K 1))
    (select-subtree-children $Kids $K3 $K2 $Kids2 $Subtree $Hole $NodeName))
; ; case 3
  (= (select-subtree (node $Name $Kids $ID) $K $K2 (node $Name $Kids2 $ID) $Subtree $Hole $NodeName)  
    (set-det)
    (select-subtree-children $Kids $K $K2 $Kids2 $Subtree $Hole $NodeName))
; ; case 4
  (= (select-subtree  $Node $K $K $Node $_ $_ $_)  True) ;
; case 5

;
; select_subtree_children applies select_subtree to list of nodes.


  (= (select-subtree-children Nil $K $K Nil $_ $_ $_)  
    (set-det))
  (= (select-subtree-children (Cons  $Node $T) $K $K2 (Cons  $Node2 $T2) $Subtree $Hole $Name)  
    (select-subtree $Node $K $K3 $Node2 $Subtree $Hole $Name)
    (det-if-then-else 
      (== $K3 0) 
      (, 
        (= $T $T2) 
        (= $K3 $K2)) 
      (select-subtree-children $T $K3 $K2 $T2 $Subtree $Hole $Name)))


  (= debug-crossover  
    (dctg-root-P $Root)
    (writel (:: 'Generate tree 1...' nl))
    (generate-tree $Root full 6 $_ $P1 $_)
    (writel (:: 'Generate tree 2...' nl))
    (generate-tree $Root full 6 $_ $P2 $_)
    (writel (:: Parent1... nl))
    (prettyprint $P1)
    (writel (:: Parent2... nl))
    (prettyprint $P2)
    (writel (:: 'Do the crossover...' nl))
    (crossover $P1 $P2 $C1 $C2)
    (writel (:: Child1... nl))
    (prettyprint $C1)
    (writel (:: Child2... nl))
    (prettyprint $C2))


  (= debug-crossover2  
    (generate-tree sentence grow 10 $_ $P1 $_)
    (generate-tree sentence grow 10 $_ $P2 $_)
    (crossover $P1 $P2 $C1 $C2)
    (writel (:: Parent1... nl))
    (prettyprint $P1)
    (writel (:: Parent2... nl))
    (prettyprint $P2)
    (writel (:: Child1... nl))
    (prettyprint $C1)
    (writel (:: Child2... nl))
    (prettyprint $C2))

;
; ---------------------------

;
; mutation(Parent, Child):
;
; Parent - tree to mutate
;
; Child - mutated result
;
; Performs mutation on a tree. A subtree is randomly selected. Then a
;
; new subtree of the same type as selected one is generated using grow
;
; generation, and it replaces the selected subtree. If the resulting tree
;
; is too deep, then it is repeated a maximum number of user-specified times.
;
; If the user is using terminal mutation probability (Case 1) then all nodes
;
; of that type (if it succeeds statisticall) are counted. If none exist, then
;
; all nodes counted (case 2).


  (= (mutation $Parent $Child)  
    (reprod-P $Tries)
    (do-mutation $Tries $Parent $Child)
    (set-det))


  (= (do-mutation 0 $_ $_)  
    ( (set-det) fail))
  (= (do-mutation $_ $Parent $Child)  
    (prob-terminal-mutation-P $PT)
    (not (== $PT no))
    (det-if-then-else 
      (maybe $PT) 
      (= $Type leaf) 
      (= $Type internal))
    (count-nodes $Parent $Type $N)
    (max-depth-P $_ $MaxDepth)
    (my-random $N $K)
    (select-subtree $Parent $K $_ $Child $_ $NewTree $NodeName)
    (is $NewDepth 
      (- $MaxDepth 2))
    (generate-tree $NodeName grow $NewDepth $_ $NewTree $_)
    (tree-verification $Child)
    (set-det))
; ; case 1
; ;writel(['rand pick ', K, ' from ', N, '.', nl]),
; ; a subtree with a node type has depth > 1
  (= (do-mutation $_ $Parent $Child)  
    (max-depth-P $_ $MaxDepth)
    (count-nodes $Parent all $N)
    (my-random $N $K)
    (select-subtree $Parent $K $_ $Child $_ $NewTree $NodeName)
    (is $NewDepth 
      (- $MaxDepth 2))
    (generate-tree $NodeName grow $NewDepth $_ $NewTree $_)
    (tree-verification $Child)
    (set-det))
; ; case 2
; ;writel(['rand pick ', K, ' from ', N, '.', nl]),
; ; a subtree with a node type has depth > 1
  (= (do-mutation $Tries $Parent $Child)  
    (is $Tries2 
      (- $Tries 1))
    (do-mutation $Tries2 $Parent $Child)
    (set-det))
; ;writel(['Try countdown... ', Tries2, nl]),


  (= debug-mutation  
    (dctg-root-P $Root)
    (generate-tree $Root full 6 $_ $Parent $_)
    (mutation $Parent $Child)
    (writel (:: Parent... nl))
    (prettyprint $Parent)
    (writel (:: Child... nl))
    (prettyprint $Child))

;
; ---------------------------

;
; verification(Tree, UserArgs, Expr):
;
; Tree - DCTG tree to verify
;
; UserArgs - Argument list to pass to DCTG rules
;
; Expr - list expression for Tree
;
; The DCTG tree is verified by interpreting the MeTTa DCTG rules
;
; in concert with the Tree structure. The purpose of this is to
;
; execute any embedded MeTTa in the rules, which are
;
; not retained in the tree data structure itself. User args as set by user_args
;
; parameter are also used (those embedded in MeTTa structure are irrelevant).
;
; This routine may cause a tree to fail, in that embedded MeTTa goals or
;
; user args fail.

;
; verification embeds user args into initial call of tree.


  (= (verification (node $Name $Kids $ID) $UserArgs $Expr)  
    ( (with-self  
        (fast *) 
        (dctg-rule-info $_ $ID $Call $_ $_)) 
      (=.. $Call 
        (Cons  $Name $Args)) 
      (append $_ 
        (:: 
          (node $X $Y $Z) $Expr $_) $Args) 
      (append $UserArgs 
        (:: 
          (node $X $Y $Z) $Expr Nil) $Args2) 
      (=.. $RuleHead2 
        (Cons  $Name $Args2)) 
      (set-det) 
      (verify-tree $RuleHead2 
        (node $Name $Kids $ID))))
; ; embed user args, empty diff list


  (= (verify-tree $Call (node $_ $Kids $ID))  
    ( (==  
        (= $Call $Body) 
        (get-atoms  &self)) 
      (same-id $Call $ID) 
      (set-det) 
      (verify-kids $Body $Kids $_)))
; ;writel(['verify_tree: Call=', Call, 'node = ', N, ID, nl]),
; ;writel(['verify_tree: Body= ', Body, 'Kids=', Kids,nl]),
  (= (verify-tree $_ $_)  
    ( (set-det) fail))
; ;writel(['verify_tree: failed', nl]),
	

  (= (verify-kids (, $A $B) $Kids $Kids3)  
    (set-det)
    (verify-kids $A $Kids $Kids2)
    (verify-kids $B $Kids2 $Kids3))
  (= (verify-kids $A (Cons  (node $_ $Kids $ID) $Rest) $Rest)  
    (is-a-rule-call $A)
    (set-det)
    (verify-tree $A 
      (node $_ $Kids $ID)))
; ;writel(['v_k 2: Call=', A, 'Node name = ', N, ID, nl]),
  (= (verify-kids (c $A $X $B) (Cons  (:: $H) $T) $T)  
    (set-det)
    (= $X $H)
    (c $A $X $B))
; ; single constant
; ; X == H,
; ;writel(['v_k 3: Call=', c(A,X,B), 'List=', [[H]|T], nl]),
  (= (verify-kids (c $A $X $B) (Cons  (Cons  $H $T2) $T) (Cons  $T2 $T))  
    (set-det)
    (= $X $H)
    (c $A $X $B))
; ; multiple constants
; ; X == H,
; ;writel(['v_k 4: Call=', c(A,X,B), 'List=', [[H|T2]|T], nl]),
  (= (verify-kids $A $Kids $Kids)  
    (set-det)
    (call $A))
; ;writel(['v_k 5: Call=', A, 'Kids=', Kids, nl]),

;
; Warning: user cannot use node/3 structure in their user arg fields!


  (= (same-id $Call $ID)  
    (=.. $Call 
      (Cons  $_ $Args))
    (member 
      (node $_ $_ $ID2) $Args)
    (set-det)
    (== $ID $ID2))
; ; append(_, [node(_, _, ID)|_], Args),


