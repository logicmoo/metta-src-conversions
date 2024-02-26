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



  !(my-ensure-loaded (library tracing))
; 


  !(dynamic (/ stativity 2))
; 




  (= 
    (stat $A 0) 
    ( (var $A) 
      (write "~n Warning: Error in Stat routine, Variable!") 
      (nl) 
      (set-det)))
; 


  (= 
    (stat 
      (, $A $B) $S) 
    ( (set-det) 
      (stat $A $StatA) 
      (stat $B $StatB) 
      (max $StatA $StatB $S)))
; 


  (= 
    (stat 
      (or $A $B) $S) 
    ( (set-det) 
      (stat $A $StatA) 
      (stat $B $StatB) 
      (max $StatA $StatB $S)))
; 


  (= 
    (stat 
      (if $Cond $Then $Else) $S) 
    ( (set-det) 
      (stat $Cond $CondS) 
      (stat $Then $ThenS) 
      (stat $Else $ElseS) 
      (max 
        (:: $CondS $ThenS $ElseS) $S)))
; 


  (= 
    (stat 
      (det-if-then-else $Cond $Then $Else) $S) 
    ( (set-det) 
      (stat $Cond $CondS) 
      (stat $Then $ThenS) 
      (stat $Else $ElseS) 
      (max 
        (:: $CondS $ThenS $ElseS) $S)))
; 


  (= 
    (stat 
      (det-if-then $Cond $Then) $S) 
    ( (set-det) 
      (stat $Cond $CondS) 
      (stat $Then $ThenS) 
      (max 
        (:: $CondS $ThenS) $S)))
; 


  (= 
    (stat 
      (call $Call) $S) 
    ( (set-det) (stat $Call $S)))
; 


  (= 
    (stat 
      (not $Call) $S) 
    ( (set-det) (stat $Call $S)))
; 


;
; 

;
; 

  (= 
    (stat 
      (setof $X $Test $Xs) $S) 
    ( (set-det) 
      (stat $Test $TestS) 
      (min 
        (:: $TestS 1) $S)))
; 


  (= 
    (stat 
      (bagof $X $Test $Xs) $S) 
    ( (set-det) 
      (stat $Test $TestS) 
      (min 
        (:: $TestS 1) $S)))
; 


  (= 
    (stat 
      (^ $X $Test) $Is) 
    ( (set-det) (stat $Test $Is)))
; 


  (= 
    (stat $GIn 1) 
    ( (test-goal $GIn) (set-det)))
; 


  (= 
    (stat $GIn 2) 
    ( (change-goal $GIn) (set-det)))
; 


  (= 
    (stat $GIn $S) 
    ( (stativity $GIn $S) (set-det)))
; 


  (= 
    (stat  $_ 0) True)
; 

	


  (= 
    (change_goal  
      (add  $Pred)) True)
; 

  (= 
    (change_goal  
      (del  $Pred)) True)
; 



  (= 
    (test_goal  
      (true  $Pred)) True)
; 


	                 
;
; 

;
; 



  (= 
    (stativity-analysis) 
    ( (initialize-stats 0) (interpret-stativity)))
; 



;
; 



  !(dynamic (/ changed 0))
; 




  (= 
    (initialize-stats $InitVal) 
    ( (reset-stat) 
      (theory-clause $GIn $Body) 
      (variablized-goal $GIn $VGIn) 
      (det-if-then-else 
        (stativity $VGIn $Stat) 
        (det-if-then-else 
          (= $Stat $InitVal) True 
          (format "Error: Some stat entry not reset properly~n" Nil)) 
        (add-symbol  &self 
          (stativity  $VGIn $InitVal))) 
      (fail)))
; 

  (= 
    (initialize_stats  $_) True)
; 
 


  (= 
    (interpret-stativity) 
    ( (tracing-format 
        (stat 1) "Starting an iteration of stativity interpretation~n" Nil) 
      (do-analysis) 
      (loop-if-changed)))
; 



  (= 
    (loop-if-changed) 
    ( (remove-symbol  &self changed) 
      (set-det) 
      (interpret-stativity)))
; 

  (= 
    (loop-if-changed) 
    (tracing-format 
      (stat 1) "Reached fixed point, interpretation has ended.~n" Nil))
; 

	


  (= 
    (do-analysis) 
    ( (clear-changed) (or (, (analysis-item $Item) (analyze-item $Item) (fail)) True)))
; 



  (= 
    (analysis-item (= $Goal $Body)) 
    (theory-clause $Goal $Body))
; 

	

  (= 
    (analyze-item (= $Goal $Body)) 
    ( (stat $Body $Stat) (update-assumption $Goal $Stat)))
; 


;
; 


  (= 
    (update-assumption $P $Stat) 
    ( (stativity $P $StatOld) 
      (=< $Stat $StatOld) 
      (set-det)))
; 

  (= 
    (update-assumption $P $Stat) 
    ( (variablized-goal $P $VP) 
      (remove-symbol  &self 
        (stativity  $VP $StatOld)) 
      (add-symbol  &self 
        (stativity  $VP $Stat)) 
      (tracing-format 
        (stat 2) "Procedure <~p> changed stativity from <~p> to <~p>~n" 
        (:: $P $StatOld $Stat)) 
      (note-changed)))
; 





  (= 
    (note-changed) 
    ( (changed) (set-det)))
; 

  (= 
    (note-changed) 
    ( (tracing-format 
        (stat 2) "Initial stativity changed this iteration~n" Nil) (add-symbol  &self changed)))
; 
	


  (= 
    (clear-changed) 
    (remove-all-symbols  &self changed))
; 


  (= 
    (reset-stat) 
    (remove-all-symbols  &self 
      (stativity  $_ $_)))
; 




  (= 
    (variablized-goal $GoalIn $GoalOut) 
    ( (functor $GoalIn $F $A) (functor $GoalOut $F $A)))
; 


;
; 

;
; 



  (= 
    (set-stat-verbosity $Level $Status) 
    (set-tracing 
      (stat $Level) $Status))
; 


  (= 
    (silent-stat) 
    ( (set-stat-verbosity 1 off) (set-stat-verbosity 2 off)))
; 
 


  !(set-stat-verbosity 1 on)
; 


