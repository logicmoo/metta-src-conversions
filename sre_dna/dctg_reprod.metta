
;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (crossover $P1 $P2 $C1 $C2) 
    ( (prob-internal-crossover-P $PI) 
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
      (set-det)))
; 
 
  (= 
    (crossover $P1 $P2 $C1 $C2) 
    ( (reprod-P $Tries) 
      (once (count-nodes $P1 all $N1)) 
      (do-crossover $Tries $P1 $N1 $P2 $C1 $C2) 
      (set-det)))
; 
 


  (= 
    (do-crossover 0 $_ $_ $_ $_ $_) 
    ( (set-det) (fail)))
; 

  (= 
    (do-crossover $_ $Parent1 $N1 $Parent2 $Child1 $Child2) 
    ( (my-random $N1 $K1) 
      (select-subtree $Parent1 $K1 $_ $Child1 $Subtree1 $Subtree2 $NodeName) 
      (count-nodes $Parent2 $NodeName $N2) 
      (my-random $N2 $K2) 
      (select-subtree $Parent2 $K2 $_ $Child2 $Subtree2 $Subtree1 $NodeName) 
      (tree-verification $Child1) 
      (tree-verification $Child2) 
      (set-det)))
; 

  (= 
    (do-crossover $Tries $Parent1 $N1 $Parent2 $Child1 $Child2) 
    ( (is $Tries2 
        (- $Tries 1)) 
      (do-crossover $Tries2 $Parent1 $N1 $Parent2 $Child1 $Child2) 
      (set-det)))
; 


;
; 

;
; 



  (= 
    (tree-verification $Child) 
    ( (det-if-then-else 
        (reprod-verif-P yes) 
        (, 
          (user-args-P $Args) 
          (verification $Child $Args $_)) True) (set-det)))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (count-nodes 
      (node $_ $Children $_) all $NumNodes) 
    ( (set-det) 
      (count-children-nodes $Children all $NumNodes2) 
      (is $NumNodes 
        (+ $NumNodes2 1))))
; 

  (= 
    (count-nodes 
      (node $_ $Children $ID) $Type $NumNodes) 
    ( (== $Type internal) 
      (with_self  
        (fast *) 
        (dctg-rule-info $_ $ID $_ $_ nonterminal)) 
      (set-det) 
      (count-children-nodes $Children $Type $NumNodes2) 
      (is $NumNodes 
        (+ $NumNodes2 1))))
; 

  (= 
    (count-nodes 
      (node $_ $Children $ID) $Type $NumNodes) 
    ( (== $Type leaf) 
      (with_self  
        (fast *) 
        (dctg-rule-info $_ $ID $_ $_ terminal)) 
      (set-det) 
      (count-children-nodes $Children $Type $NumNodes2) 
      (is $NumNodes 
        (+ $NumNodes2 1))))
; 

  (= 
    (count-nodes 
      (node $_ $Children $ID) $NodeName $NumNodes) 
    ( (with_self  
        (fast *) 
        (dctg-rule-info $NodeName $ID $_ $_ $_)) 
      (set-det) 
      (count-children-nodes $Children $NodeName $NumNodes2) 
      (is $NumNodes 
        (+ $NumNodes2 1))))
; 

  (= 
    (count-nodes 
      (node $_ $Children $_) $NodeName $NumNodes) 
    ( (set-det) (count-children-nodes $Children $NodeName $NumNodes)))
; 

  (= 
    (count_nodes  $_ $_ 0) True)
; 



  (= 
    (count_children_nodes  () $_ 0) True)
; 

  (= 
    (count-children-nodes 
      (Cons  $Node $Rest) $NodeName $NumNodes) 
    ( (count-nodes $Node $NodeName $NumNodes2) 
      (count-children-nodes $Rest $NodeName $NumNodes3) 
      (is $NumNodes 
        (+ $NumNodes2 $NumNodes3)) 
      (set-det)))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (select-subtree 
      (node $_ $Kids $ID) 1 0 $NewParent 
      (node $NodeName $Kids $ID) $NewParent $NodeName) 
    ( (or 
        (var $NodeName) 
        (with_self  
          (fast *) 
          (dctg-rule-info $NodeName $ID $_ $_ $_))) 
      (set-det) 
      (with_self  
        (fast *) 
        (dctg-rule-info $NodeName $ID $_ $_ $_))))
; 

  (= 
    (select-subtree 
      (node $Name $Kids $ID) $K $K2 
      (node $Name $Kids2 $ID) $Subtree $Hole $NodeName) 
    ( (or 
        (var $NodeName) 
        (with_self  
          (fast *) 
          (dctg-rule-info $NodeName $ID $_ $_ $_))) 
      (set-det) 
      (is $K3 
        (- $K 1)) 
      (select-subtree-children $Kids $K3 $K2 $Kids2 $Subtree $Hole $NodeName)))
; 

  (= 
    (select-subtree 
      (node $Name $Kids $ID) $K $K2 
      (node $Name $Kids2 $ID) $Subtree $Hole $NodeName) 
    ( (set-det) (select-subtree-children $Kids $K $K2 $Kids2 $Subtree $Hole $NodeName)))
; 

  (= 
    (select_subtree  $Node $K $K $Node $_ $_ $_) True)
; 
 ;
; 


;
; 



  (= 
    (select-subtree-children Nil $K $K Nil $_ $_ $_) 
    (set-det))
; 

  (= 
    (select-subtree-children 
      (Cons  $Node $T) $K $K2 
      (Cons  $Node2 $T2) $Subtree $Hole $Name) 
    ( (select-subtree $Node $K $K3 $Node2 $Subtree $Hole $Name) (det-if-then-else (== $K3 0) (, (= $T $T2) (= $K3 $K2)) (select-subtree-children $T $K3 $K2 $T2 $Subtree $Hole $Name))))
; 



  (= 
    (debug-crossover) 
    ( (dctg-root-P $Root) 
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
      (prettyprint $C2)))
; 



  (= 
    (debug-crossover2) 
    ( (generate-tree sentence grow 10 $_ $P1 $_) 
      (generate-tree sentence grow 10 $_ $P2 $_) 
      (crossover $P1 $P2 $C1 $C2) 
      (writel (:: Parent1... nl)) 
      (prettyprint $P1) 
      (writel (:: Parent2... nl)) 
      (prettyprint $P2) 
      (writel (:: Child1... nl)) 
      (prettyprint $C1) 
      (writel (:: Child2... nl)) 
      (prettyprint $C2)))
; 


;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (mutation $Parent $Child) 
    ( (reprod-P $Tries) 
      (do-mutation $Tries $Parent $Child) 
      (set-det)))
; 



  (= 
    (do-mutation 0 $_ $_) 
    ( (set-det) (fail)))
; 

  (= 
    (do-mutation $_ $Parent $Child) 
    ( (prob-terminal-mutation-P $PT) 
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
      (set-det)))
; 

  (= 
    (do-mutation $_ $Parent $Child) 
    ( (max-depth-P $_ $MaxDepth) 
      (count-nodes $Parent all $N) 
      (my-random $N $K) 
      (select-subtree $Parent $K $_ $Child $_ $NewTree $NodeName) 
      (is $NewDepth 
        (- $MaxDepth 2)) 
      (generate-tree $NodeName grow $NewDepth $_ $NewTree $_) 
      (tree-verification $Child) 
      (set-det)))
; 

  (= 
    (do-mutation $Tries $Parent $Child) 
    ( (is $Tries2 
        (- $Tries 1)) 
      (do-mutation $Tries2 $Parent $Child) 
      (set-det)))
; 



  (= 
    (debug-mutation) 
    ( (dctg-root-P $Root) 
      (generate-tree $Root full 6 $_ $Parent $_) 
      (mutation $Parent $Child) 
      (writel (:: Parent... nl)) 
      (prettyprint $Parent) 
      (writel (:: Child... nl)) 
      (prettyprint $Child)))
; 


;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 


;
; 



  (= 
    (verification 
      (node $Name $Kids $ID) $UserArgs $Expr) 
    ( (with_self  
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
; 



  (= 
    (verify-tree $Call 
      (node $_ $Kids $ID)) 
    ( (get-symbols  &self 
        (= $Call $Body)) 
      (same-id $Call $ID) 
      (set-det) 
      (verify-kids $Body $Kids $_)))
; 

  (= 
    (verify-tree $_ $_) 
    ( (set-det) (fail)))
; 

	

  (= 
    (verify-kids 
      (, $A $B) $Kids $Kids3) 
    ( (set-det) 
      (verify-kids $A $Kids $Kids2) 
      (verify-kids $B $Kids2 $Kids3)))
; 

  (= 
    (verify-kids $A 
      (Cons  
        (node $_ $Kids $ID) $Rest) $Rest) 
    ( (is-a-rule-call $A) 
      (set-det) 
      (verify-tree $A 
        (node $_ $Kids $ID))))
; 

  (= 
    (verify-kids 
      (c $A $X $B) 
      (Cons  
        (:: $H) $T) $T) 
    ( (set-det) 
      (= $X $H) 
      (c $A $X $B)))
; 

  (= 
    (verify-kids 
      (c $A $X $B) 
      (Cons  
        (Cons  $H $T2) $T) 
      (Cons  $T2 $T)) 
    ( (set-det) 
      (= $X $H) 
      (c $A $X $B)))
; 

  (= 
    (verify-kids $A $Kids $Kids) 
    ( (set-det) (call $A)))
; 


;
; 



  (= 
    (same-id $Call $ID) 
    ( (=.. $Call 
        (Cons  $_ $Args)) 
      (member 
        (node $_ $_ $ID2) $Args) 
      (set-det) 
      (== $ID $ID2)))
; 



