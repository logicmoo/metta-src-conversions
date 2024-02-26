;                                           (convert_to_metta_file  stat $_381116 metagame/state/stat.pl metagame/state/stat.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; stat.pl
;
; 
;
; Main procedures:
;
; 
;
; STAT(GIn,State_Req)
;
; tells us whether the Goal GIn requires 0, 1, or 2 extra args
;
; for explicit passing of state variables.
;
; 0: if G is independent of state.
;
; 1: if G has a subgoal which might test state, but no subgoal changes state.
;
; 2: if G has a subgoal which might change state.
;
; 
;
; STATIVITY_ANALYSIS
;
; Performs an abstract interpretation over the loaded domain theory.
;
; This proceeds as follows:
;
; 
;
; Assume all predicates have stativity 0 (ie we know nothing about their
;
; stativity).
;
; At each iteration I+1, update our assumptions on stativity for each predicate based
;
; on the current assumptions:
;
; 1. Compute stativity based on current assumptions.
;
; 2. If new stativity for this pred is different, save it, and note
;
; something changed.
;
; [When we update an assumption, we can immediately change its entry in
;
; the table to use on subsequence steps within the same iteration.]
;
; 
;
; If nothing changed, then we've reached a fixedpoint, so our
;
; assumptions are now correct (at least we'll gain no more knowledge by
;
; repeating this analysis).
;
; Otherwise, do another loop.
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  !(my-ensure-loaded (library tracing))

  !(dynamic (/ stativity 2))



  (= (stat $A 0)  
    (var $A)
    (write "~n Warning: Error in Stat routine, Variable!")
    (nl)
    (set-det))

  (= (stat (, $A $B) $S)  
    (set-det)
    (stat $A $StatA)
    (stat $B $StatB)
    (max $StatA $StatB $S))

  (= (stat (or $A $B) $S)  
    (set-det)
    (stat $A $StatA)
    (stat $B $StatB)
    (max $StatA $StatB $S))

  (= (stat (if $Cond $Then $Else) $S)  
    (set-det)
    (stat $Cond $CondS)
    (stat $Then $ThenS)
    (stat $Else $ElseS)
    (max 
      (:: $CondS $ThenS $ElseS) $S))

  (= (stat (det-if-then-else $Cond $Then $Else) $S)  
    (set-det)
    (stat $Cond $CondS)
    (stat $Then $ThenS)
    (stat $Else $ElseS)
    (max 
      (:: $CondS $ThenS $ElseS) $S))

  (= (stat (det-if-then $Cond $Then) $S)  
    (set-det)
    (stat $Cond $CondS)
    (stat $Then $ThenS)
    (max 
      (:: $CondS $ThenS) $S))

  (= (stat (call $Call) $S)  
    (set-det)
    (stat $Call $S))

  (= (stat (not $Call) $S)  
    (set-det)
    (stat $Call $S))

;
; Setof and bagof are logical, so the may depend on
;
; state (possibly) but not change it.
  (= (stat (setof $X $Test $Xs) $S)  
    (set-det)
    (stat $Test $TestS)
    (min 
      (:: $TestS 1) $S))

  (= (stat (bagof $X $Test $Xs) $S)  
    (set-det)
    (stat $Test $TestS)
    (min 
      (:: $TestS 1) $S))

  (= (stat (^ $X $Test) $Is)  
    (set-det)
    (stat $Test $Is))

  (= (stat $GIn 1)  
    (test-goal $GIn)
    (set-det))

  (= (stat $GIn 2)  
    (change-goal $GIn)
    (set-det))

  (= (stat $GIn $S)  
    (stativity $GIn $S)
    (set-det))

  (= (stat  $_ 0)  True)
	


  (= (change_goal  (add  $Pred))  True)
  (= (change_goal  (del  $Pred))  True)


  (= (test_goal  (true  $Pred))  True)

	                 
;
; ============================================================
;
; Stativity Analysis


  (= (stativity-analysis)  
    (initialize-stats 0)
    (interpret-stativity))


;
; Changed will fail unless something really changed.


  !(dynamic (/ changed 0))



  (= (initialize-stats $InitVal)  
    (reset-stat)
    (theory-clause $GIn $Body)
    (variablized-goal $GIn $VGIn)
    (det-if-then-else 
      (stativity $VGIn $Stat) 
      (det-if-then-else 
        (= $Stat $InitVal) True 
        (format "Error: Some stat entry not reset properly~n" Nil)) 
      (add-is-symbol  &self 
        (stativity  $VGIn $InitVal)))
    (fail))
  (= (initialize_stats  $_)  True) 


  (= (interpret-stativity)  
    (tracing-format 
      (stat 1) "Starting an iteration of stativity interpretation~n" Nil)
    (do-analysis)
    (loop-if-changed))


  (= (loop-if-changed)  
    ( (remove-is-symbol  &self changed) 
      (set-det) 
      (interpret-stativity)))
  (= (loop-if-changed)  
    (tracing-format 
      (stat 1) "Reached fixed point, interpretation has ended.~n" Nil))
	


  (= (do-analysis)  
    (clear-changed)
    (or 
      (, 
        (analysis-item $Item) 
        (analyze-item $Item) 
        (fail)) True))


  (= (analysis-item (= $Goal  $Body))  
    (theory-clause $Goal $Body))
	

  (= (analyze-item (= $Goal  $Body))  
    (stat $Body $Stat)
    (update-assumption $Goal $Stat))

;
; Only update assumptions to values higher in the lattice.

  (= (update-assumption $P $Stat)  
    (stativity $P $StatOld)
    (=< $Stat $StatOld)
    (set-det))
  (= (update-assumption $P $Stat)  
    ( (variablized-goal $P $VP) 
      (remove-is-symbol  &self 
        (stativity  $VP $StatOld)) 
      (add-is-symbol  &self 
        (stativity  $VP $Stat)) 
      (tracing-format 
        (stat 2) "Procedure <~p> changed stativity from <~p> to <~p>~n" 
        (:: $P $StatOld $Stat)) 
      (note-changed)))




  (= (note-changed)  
    (changed)
    (set-det))
  (= (note-changed)  
    ( (tracing-format 
        (stat 2) "Initial stativity changed this iteration~n" Nil) (add-is-symbol  &self changed)))	


  (= (clear-changed)  
    (remove-all-atoms  &self changed))

  (= (reset-stat)  
    (remove-all-atoms  &self 
      (stativity  $_ $_)))



  (= (variablized-goal $GoalIn $GoalOut)  
    (functor $GoalIn $F $A)
    (functor $GoalOut $F $A))

;
; ================================================================================
;
; tracing execution


  (= (set-stat-verbosity $Level $Status)  
    (set-tracing 
      (stat $Level) $Status))

  (= (silent-stat)  
    (set-stat-verbosity 1 off)
    (set-stat-verbosity 2 off)) 


  !(set-stat-verbosity 1 on)

