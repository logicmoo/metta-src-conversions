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
    (generate-tree $TopGoal $TreeType $MaxDepth $UserArgs $Tree $Expr) 
    ( (with_self  
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
    (gen-tree $RuleHead $TreeType $Depth) 
    ( (is $Depth2 
        (- $Depth 1)) 
      (select-random-rule $TreeType $Depth $RuleHead) 
      (get-symbols  &self 
        (= $RuleHead $Body)) 
      (process-goals $Body $TreeType $Depth2)))
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
    (process-goals 
      (, $A $B) $TreeType $Depth) 
    ( (set-det) 
      (process-goals $A $TreeType $Depth) 
      (process-goals $B $TreeType $Depth)))
; 

	;
; 

  (= 
    (process-goals $A $TreeType $Depth) 
    (det-if-then-else 
      (is-a-rule-call $A) 
      (gen-tree $A $TreeType $Depth) 
      (call $A)))
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
    (select-random-rule $TreeType $MaxDepth $RuleHead) 
    ( (=.. $RuleHead 
        (Cons  $RuleName $_)) 
      (shuffle-rule-list $RuleName $TreeType $RuleList) 
      (member $ID $RuleList) 
      (with_self  
        (fast *) 
        (dctg-rule-info $_ $ID $RuleHead $MinDepth $_)) 
      (=< $MinDepth $MaxDepth) 
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



  (= 
    (shuffle-rule-list $RuleName grow $RuleList) 
    ( (with_self  
        (fast *) 
        (dctg-id-table $RuleName $IDList $_ $_)) 
      (random-permutation $IDList $RuleList) 
      (set-det)))
; 

  (= 
    (shuffle-rule-list $RuleName full $RuleList) 
    ( (with_self  
        (fast *) 
        (dctg-id-table $RuleName $_ $TermList $NontermList)) 
      (random-permutation $TermList $T1) 
      (random-permutation $NontermList $T2) 
      (append $T2 $T1 $RuleList) 
      (set-det)))
; 



