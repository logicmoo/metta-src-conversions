;                                           (convert-to-metta-file  thread $_189464 metagame/state/thread.pl metagame/state/thread.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ================================================================================
;
; THREAD(GoalsIn,SIn,SOut,ThreadedGoals)
;
; ================================================================================


  (= (thread (det-if-then-else $Cond $Then $Else) $SIn $SOut $Answer)  
    (set-det)
    (thread $Cond $SIn $S1 $CondOut)
    (thread $Then $S1 $S2 $ThenOut)
    (thread $Else $SIn $S3 $ElseOut)
    (= $Answer 
      (det-if-then-else $CondOut 
        (, $ThenOut 
          (= $SOut $S2)) 
        (, $ElseOut 
          (= $SOut $S3)))))

  (= (thread (det-if-then $Cond $Then) $SIn $SOut $Answer)  
    (set-det)
    (thread $Cond $SIn $S1 $CondOut)
    (thread $Then $S1 $S2 $ThenOut)
    (= $Answer 
      (det-if-then $CondOut 
        (, $ThenOut 
          (= $SOut $S2)))))

  (= (thread (if $Cond $Then $Else) $SIn $SOut $Answer)  
    (set-det)
    (thread $Cond $SIn $S1 $CondOut)
    (thread $Then $S1 $S2 $ThenOut)
    (thread $Else $SIn $S3 $ElseOut)
    (= $Answer 
      (if $CondOut 
        (, $ThenOut 
          (= $SOut $S2)) 
        (, $ElseOut 
          (= $SOut $S3)))))

  (= (thread (, $A $B) $SIn $SOut $Answer)  
    (set-det)
    (thread $A $SIn $S1 $AOut)
    (thread $B $S1 $SOut $BOut)
    (= $Answer 
      (, $AOut $BOut)))

  (= (thread (or $A $B) $SIn $SOut $Answer)  
    (set-det)
    (thread $A $SIn $S1 $AOut)
    (thread $B $SIn $S2 $BOut)
    (= $Answer 
      (or 
        (, $AOut 
          (= $SOut $S1)) 
        (, $BOut 
          (= $SOut $S2)))))

;
; If call is variable, won't thread right if requires state.
  (= (thread (call $Call) $SIn $SOut $Answer)  
    (set-det)
    (det-if-then-else 
      (var $Call) 
      (, 
        (= $Answer 
          (call $Call)) 
        (= $SIn $SOut)) 
      (, 
        (thread $Call $SIn $SOut $CallOut) 
        (= $Answer 
          (call $CallOut)))))

  (= (thread (not $Call) $SIn $SOut $Answer)  
    (set-det)
    (thread $Call $SIn $S1 $CallOut)
    (= $Answer 
      (, 
        (not $CallOut) 
        (= $SIn $SOut))))

  (= (thread (setof $X $Test $Xs) $SIn $SIn $Answer)  
    (set-det)
    (thread $Test $SIn $S1 $TestOut)
    (= $Answer 
      (setof $X 
        (^ $S1 $TestOut) $Xs)))

  (= (thread (bagof $X $Test $Xs) $SIn $SIn $Answer)  
    (set-det)
    (thread $Test $SIn $S1 $TestOut)
    (= $Answer 
      (bagof $X 
        (^ $S1 $TestOut) $Xs)))

  (= (thread (^ $X $Test) $SIn $SOut $Answer)  
    (set-det)
    (thread $Test $SIn $SOut $TestOut)
    (= $Answer 
      (^ $X $TestOut)))


  (= (thread !$B $SIn $SOut $Answer)  
    (set-det)
    (thread $B $SIn $SOut $BOut)
    (= $Answer 
      !$BOut))

  (= (thread (= $H  $B) $SIn $SOut $Answer)  
    (set-det)
    (thread $H $SIn $SOut $HOut)
    (thread $B $SIn $SOut $BOut)
    (= $Answer 
      (= $HOut  $BOut)))
	
  (= (thread (true $GIn) $SIn $SOut $Answer)  
    (set-det)
    (= $SIn $SOut)
    (= $Answer 
      (true-in $GIn $SIn)))

  (= (thread (add $GIn) $SIn $SOut $Answer)  
    (set-det)
    (= $Answer 
      (add-in $GIn $SIn $SOut)))

  (= (thread (del $GIn) $SIn $SOut $Answer)  
    (set-det)
    (= $Answer 
      (del-in $GIn $SIn $SOut)))

  (= (thread $GIn $SIn $SOut $GOut)  
    (add-state $GIn $SIn $SOut $GOut))


