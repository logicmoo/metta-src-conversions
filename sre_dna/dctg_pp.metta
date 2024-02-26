
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
    (make-grammar-table) 
    ( (cleanup-grammar-data) 
      (make-rule-id-list) 
      (generate-rule-data) 
      (enhance-rule-id-list) 
      (set-det)))
; 



  (= 
    (cleanup-grammar-data) 
    ( (remove-all-symbols  &self 
        (dctg_rule_info  $_ $_ $_ $_)) 
      (remove-all-symbols  &self 
        (dctg_id_table  $_ $_ $_ $_)) 
      (set-det)))
; 


;
; 

;
; 

;
; 



  (= 
    (get-rule-name $Call2) 
    ( (get-symbols  &self 
        (= 
          (semantic_rule  $ID $_ $Call $_) $_)) 
      (=.. $Call 
        (Cons  $Name $Args)) 
      (clone-list $Args $T) 
      (append $T 
        (:: 
          (node $_ $_ $ID) $_ $_) $AllArgs) 
      (=.. $Call2 
        (Cons  $Name $AllArgs))))
; 


;
; 

;
; 



  (= 
    (clone-list Nil Nil) 
    (set-det))
; 

  (= 
    (clone-list 
      (Cons  $_ $T) 
      (Cons  $_ $T2)) 
    ( (clone-list $T $T2) (set-det)))
; 


;
; 

;
; 

;
; 



  (= 
    (generate-rule-data) 
    ( (findall $Call 
        (get-rule-name $Call) $Calls) 
      (rem-dups $Calls $Calls2) 
      (grammar-depth-top-loop $Calls2 Nil Nil $Calls3) 
      (grammar-type-top-loop $Calls2 Nil Nil $Terminal) 
      (set-rule-data $Calls3 $Terminal) 
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



  (= 
    (grammar-depth-top-loop Nil $Known $_ $Known) 
    (set-det))
; 

  (= 
    (grammar-depth-top-loop $Calls $Known $MinCalls $Known3) 
    ( (process-rules $Calls $Known $MinCalls Nil $Known2 $Unknown) 
      (find-rule-mins $Known2 $MinCalls $MinCalls2) 
      (det-if-then-else 
        (, 
          (length $Calls $L) 
          (length $Unknown $L)) 
        (, 
          (write 'Problem - ') 
          (write $L) 
          (write ' rules cannot terminate:') 
          (nl) 
          (writelist $Unknown) 
          (nl) 
          (write 'these terminated - ') 
          (nl) 
          (writelist $Known2) 
          (nl) 
          (write 'These are mincalls - ') 
          (nl) 
          (writelist $MinCalls2) 
          (nl) 
          (fail)) 
        (grammar-depth-top-loop $Unknown $Known2 $MinCalls2 $Known3)) 
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



  (= 
    (process-rules Nil $Known $_ $Unknown $Known $Unknown) 
    (set-det))
; 

  (= 
    (process-rules 
      (Cons  $Call $Rest) $Known $MinCalls $Unknown $Known2 $Unknown2) 
    ( (copy-term $Call $Call2) 
      (get-symbols  &self 
        (= $Call2 $Body)) 
      (find-min-depth-body $Body $MinCalls 0 $BodyDepth) 
      (set-det) 
      (is $MinD 
        (+ $BodyDepth 1)) 
      (process-rules $Rest 
        (Cons  
          (, $Call $MinD) $Known) $MinCalls $Unknown $Known2 $Unknown2)))
; 

  (= 
    (process-rules 
      (Cons  $Call $Rest) $Known $MinCalls $Unknown $Known2 $Unknown2) 
    ( (set-det) (process-rules $Rest $Known $MinCalls (Cons  $Call $Unknown) $Known2 $Unknown2)))
; 


;
; 

;
; 

;
; 



  (= 
    (find-min-depth-body 
      (, $Goal $Rest) $MinCalls $MinDSoFar $MinD) 
    ( (is-a-rule-call $Goal) 
      (set-det) 
      (find-min-depth $Goal $MinCalls $Val) 
      (is $MinDSoFar2 
        (max $Val $MinDSoFar)) 
      (find-min-depth-body $Rest $MinCalls $MinDSoFar2 $MinD)))
; 

  (= 
    (find-min-depth-body 
      (, $_ $Rest) $MinCalls $MinDSoFar $MinD) 
    ( (set-det) (find-min-depth-body $Rest $MinCalls $MinDSoFar $MinD)))
; 

  (= 
    (find-min-depth-body $Goal $MinCalls $MinDSoFar $MinD) 
    ( (is-a-rule-call $Goal) 
      (set-det) 
      (find-min-depth $Goal $MinCalls $Val) 
      (is $MinD 
        (max $Val $MinDSoFar))))
; 

  (= 
    (find-min-depth-body $_ $_ $MinD $MinD) 
    (set-det))
; 


;
; 

;
; 



  (= 
    (find-min-depth $Goal 
      (Cons  
        (, $G $M) $_) $M) 
    ( (=.. $Goal 
        (Cons  $G $_)) (set-det)))
; 

  (= 
    (find-min-depth $Goal 
      (Cons  $_ $R) $M) 
    ( (find-min-depth $Goal $R $M) (set-det)))
; 


;
; 



  (= 
    (is-a-rule-call $Goal) 
    ( (=.. $Goal 
        (Cons  $Name $_)) 
      (dctg-id-table $Name $_ $_ $_) 
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



  (= 
    (find-rule-mins Nil $MinCalls $MinCalls) 
    (set-det))
; 

  (= 
    (find-rule-mins 
      (Cons  
        (, $Call $Depth) $Rest) $MinCalls $MinCalls2) 
    ( (=.. $Call 
        (Cons  $CallName $_)) 
      (not (member (, $CallName $_) $MinCalls)) 
      (set-det) 
      (find-rule-mins $Rest 
        (Cons  
          (, $CallName $Depth) $MinCalls) $MinCalls2)))
; 

  (= 
    (find-rule-mins 
      (Cons  $_ $Rest) $MinCalls $MinCalls2) 
    (find-rule-mins $Rest $MinCalls $MinCalls2))
; 


;
; 



  (= 
    (abstract-member $GoalName 
      (Cons  
        (, $First $_) $_)) 
    (=.. $First 
      (Cons  $GoalName $_)))
; 
	
  (= 
    (abstract-member $GoalName 
      (Cons  $_ $Rest)) 
    (abstract-member $GoalName $Rest))
; 
	

;
; 

;
; 



  (= 
    (find_minimum_depth  $_ () $D $D) True)
; 

  (= 
    (find-minimum-depth $CallName 
      (Cons  
        (, $Call $D) $Rest) $MinSoFar $MinDepth) 
    ( (=.. $Call 
        (Cons  $CallName $_)) 
      (is $NewMin 
        (min $D $MinSoFar)) 
      (find-minimum-depth $CallName $Rest $NewMin $MinDepth) 
      (set-det)))
; 

  (= 
    (find-minimum-depth $CallName 
      (Cons  $_ $Rest) $MinSoFar $MinDepth) 
    ( (find-minimum-depth $CallName $Rest $MinSoFar $MinDepth) (set-det)))
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
    (grammar-type-top-loop $Calls $Terms $Nonterms $Terms2) 
    ( (grammar-type-loop $Calls Nil $Terms $Nonterms $Unknown $Terms3 $Nonterms3) 
      (det-if-then-else 
        (, 
          (length $Calls $A) 
          (length $Unknown $A)) 
        (= $Terms3 $Terms2) 
        (grammar-type-top-loop $Unknown $Terms3 $Nonterms3 $Terms2)) 
      (set-det)))
; 



  (= 
    (grammar-type-loop Nil $Unknown $Term $Nonterm $Unknown $Term $Nonterm) 
    (set-det))
; 

  (= 
    (grammar-type-loop 
      (Cons  $Call $Rest) $Unknown $Term $Nonterm $Unknown2 $Term2 $Nonterm2) 
    ( (user-override $Call $Term $Nonterm $Term3 $Nonterm3) (grammar-type-loop $Rest $Unknown $Term3 $Nonterm3 $Unknown2 $Term2 $Nonterm2)))
; 

  (= 
    (grammar-type-loop 
      (Cons  $Call $Rest) $Unknown $Term $Nonterm $Unknown2 $Term2 $Nonterm2) 
    ( (copy-term $Call $Call2) 
      (get-symbols  &self 
        (= $Call2 $Body)) 
      (goal-type $Call $Body $Rest $Unknown $Term $Nonterm $Unknown3 $Term3 $Nonterm3) 
      (grammar-type-loop $Rest $Unknown3 $Term3 $Nonterm3 $Unknown2 $Term2 $Nonterm2)))
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
    (user-override $Call $Term $Nonterm 
      (Cons  $Call $Term) $Nonterm) 
    ( (=.. $Call 
        (Cons  $Name $_)) 
      (dctg-override-P $OverTerm $_) 
      (member $Name $OverTerm) 
      (set-det)))
; 

  (= 
    (user-override $Call $Term $Nonterm $Term 
      (Cons  $Call $Nonterm)) 
    ( (=.. $Call 
        (Cons  $Name $_)) 
      (dctg-override-P $_ $OverNonterm) 
      (member $Name $OverNonterm) 
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



  (= 
    (goal-type $Call $Goals $_ $U $T $NT $U $T 
      (Cons  $Call $NT)) 
    ( (det-if-then-else 
        (= $Goals 
          (, $A $_)) True 
        (= $Goals $A)) 
      (or 
        (abstract-member2 $A $NT) 
        (same-goal $Call $A)) 
      (set-det)))
; 

  (= 
    (goal-type $Call $Goals $Rest $U $T $NT 
      (Cons  $Call $U) $T $NT) 
    ( (det-if-then-else 
        (= $Goals 
          (, $A $_)) True 
        (= $Goals $A)) 
      (or 
        (abstract-member2 $A $U) 
        (abstract-member2 $A $Rest)) 
      (set-det)))
; 

  (= 
    (goal-type $Call 
      (, $_ $B) $Rest $U $T $NT $U2 $T2 $NT2) 
    ( (set-det) (goal-type $Call $B $Rest $U $T $NT $U2 $T2 $NT2)))
; 

  (= 
    (goal_type  $Call $_ $_ $U $T $NT $U 
      (Cons  $Call $T) $NT) True)
; 
 ;
; 



;
; 



  (= 
    (abstract-member2 $Goal 
      (Cons  $First $_)) 
    (same-goal $Goal $First))
; 

  (= 
    (abstract-member2 $Goal 
      (Cons  $_ $Rest)) 
    (abstract-member2 $Goal $Rest))
; 
	


  (= 
    (same-goal $A $B) 
    ( (=.. $A 
        (Cons  $N $_)) 
      (=.. $B 
        (Cons  $N $_)) 
      (set-det)))
; 


;
; 



  (= 
    (set-rule-data Nil $_) 
    (set-det))
; 

  (= 
    (set-rule-data 
      (Cons  
        (, $Rule $Depth) $Rest) $Terminal) 
    ( (=.. $Rule 
        (Cons  $Name $Args)) 
      (append $_ 
        (:: 
          (node $_ $_ $ID) $_ $_) $Args) 
      (det-if-then-else 
        (member $Rule $Terminal) 
        (= $Type terminal) 
        (= $Type nonterminal)) 
      (add-symbol  &self 
        (dctg_rule_info  $Name $ID $Rule $Depth $Type)) 
      (set-rule-data $Rest $Terminal) 
      (set-det)))
; 


;
; 

;
; 

;
; 



  (= 
    (make-rule-id-list) 
    ( (findall 
        (, $Name $IDs) 
        (make-rule-id-list2 $Name $IDs) $RuleIDs) 
      (make-id-entries $RuleIDs) 
      (set-det)))
; 



  (= 
    (make-rule-id-list2 $Name $RuleIDs2) 
    ( (bagof $ID 
        (get-rule-stuff $Name $ID) $RuleIDs) (rem-dups $RuleIDs $RuleIDs2)))
; 



  (= 
    (get-rule-stuff $Name $ID) 
    ( (get-symbols  &self 
        (= 
          (semantic_rule  $ID $_ $Call $_) $_)) (=.. $Call (Cons  $Name $_))))
; 



  (= 
    (make-id-entries Nil) 
    (set-det))
; 

  (= 
    (make-id-entries (Cons  (, $Name $IDs) $Rest)) 
    ( (add-symbol  &self 
        (dctg_id_table  $Name $IDs $_ $_)) 
      (make-id-entries $Rest) 
      (set-det)))
; 


;
; 

;
; 



  (= 
    (enhance-rule-id-list) 
    ( (remove-symbol  &self 
        (dctg_id_table  $Name $IDs $_ $_)) 
      (identify-type $IDs $Terms $Nonterms) 
      (add-symbol  &self 
        (dctg_id_table  $Name $IDs $Terms $Nonterms)) 
      (fail)))
; 

  (= enhance_rule_id_list True)
; 



  (= 
    (identify_type  () () ()) True)
; 

  (= 
    (identify-type 
      (Cons  $ID $Rest) 
      (Cons  $ID $Terms) $Nonterms) 
    ( (dctg-rule-info $_ $ID $_ $_ terminal) 
      (set-det) 
      (identify-type $Rest $Terms $Nonterms)))
; 

  (= 
    (identify-type 
      (Cons  $ID $Rest) $Terms 
      (Cons  $ID $Nonterms)) 
    (identify-type $Rest $Terms $Nonterms))
; 



