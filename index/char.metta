
  (= 
    (char $InICs $PosIn $NegIn $OutICs $PosOut $NegOut) 
    ( (char1 $InICs $PosIn $NegIn Nil $TmpICs $PosOut $NegOut) (cleanup $TmpICs Nil $OutICs)))
; 



  (= 
    (char1  () $P $N $Out $Out $P $N) True)
; 

  (= 
    (char1 
      (Cons  $IC $ICs) $P0 $N0 $Acc $Out $P $N) 
    ( (write-debug (:: 'evaluating ' $IC)) 
      (evaluate $P0 $N0 $IC $Tuples $P1 $N1 $Answer) 
      (det-if-then-else 
        (= $Answer refine) 
        (, 
          (refinements $IC $Tuples $Spec) 
          (append $ICs $Spec $NewICs) 
          (= $NewAcc $Acc)) 
        (det-if-then-else 
          (= $Answer keep) 
          (, 
            (= $NewICs $ICs) 
            (= $NewAcc 
              (Cons  $IC $Acc))) 
          (det-if-then-else 
            (= $Answer 
              (ignore $E)) 
            (, 
              (= $NewICs $ICs) 
              (= $NewAcc $Acc)) 
            (det-if-then 
              (= $Answer 
                (keep $E)) 
              (, 
                (= $NewICs $ICs) 
                (insert-ic $Acc $IC $E $NewAcc)))))) 
      (write-debug (:: '	result: ' $Answer)) 
      (set-det) 
      (char1 $NewICs $P1 $N1 $NewAcc $Out $P $N)))
; 



  (= 
    (evaluate $P0 $N0 $IC $Tuples $P $N $Answer) 
    (det-if-then-else 
      (evaluate1 $P0 $IC $Answer) 
      (, 
        (= $P $P0) 
        (= $N $N0) 
        (= $Tuples Nil)) 
      (det-if-then otherwise 
        (, 
          (contr $P0 $N0 $IC $T $A) 
          (det-if-then-else 
            (= $A yes) 
            (, 
              (= $Answer refine) 
              (= $P $P0) 
              (= $N $N0) 
              (= $Tuples $T)) 
            (det-if-then-else 
              (= $A no) 
              (, 
                (= $Answer keep) 
                (= $P $P0) 
                (= $N $N0) 
                (= $Tuples $T)) 
              (det-if-then 
                (= $A possibly) 
                (, 
                  (queries $P0 $N0 $T $P1 $N1) 
                  (evaluate $P1 $N1 $IC $Tuples $P $N $Answer)))))))))
; 



  (= 
    (contr $P $N $IC $Tuples $Answer) 
    ( (horn $IC $HornIC) (incons $HornIC $P $N $Tuples $Answer)))
; 



  (= 
    (queries  $P $N () $P $N) True)
; 

  (= 
    (queries $P $N 
      (Cons  $T $Ts) $P1 $N1) 
    ( (query $P $N $T $P2 $N2) (queries $P2 $N2 $Ts $P1 $N1)))
; 


;                                (error
;                                  (syntax_error  operator_expected)
;                                  (file  index/char.pl 41 4 1180))

;                                 (error
;                                   (syntax_error  operator_expected)
;                                   (file  index/char.pl 43 4 1267))


  (= 
    (query $P $N 
      (- $T) $P 
      (Cons  $T $N)) 
    ( (switched-on cwa) 
      (write (- $T)) 
      (nl)))
; 

  (= 
    (query  $P $N 
      (+  $T) $P $N) True)
; 



  (= 
    (cleanup 
      (Cons  $X $In) $Acc $Out) 
    ( (member $IC $In) 
      (subsumed $X $IC) 
      (set-det) 
      (cleanup $In $Acc $Out)))
; 

  (= 
    (cleanup 
      (Cons  $X $In) $Acc $Out) 
    ( (member $IC $Acc) 
      (subsumed $X $IC) 
      (set-det) 
      (cleanup $In $Acc $Out)))
; 

  (= 
    (cleanup 
      (Cons  $X $In) $Acc $Out) 
    (cleanup $In 
      (Cons  $X $Acc) $Out))
; 

  (= 
    (cleanup  () $Out $Out) True)
; 



  (= 
    (incons 
      (= $Head $Body) $P $N $Tuples $Answer) 
    ( (satisfied $Body $P $N $TuplesB) 
      (falsified $Head $P $N $TuplesH) 
      (set-det) 
      (= $Answer yes) 
      (append $TuplesB $TuplesH $Tuples)))
; 

  (= 
    (incons 
      (= $Head $Body) $P $N $Tuples $Answer) 
    ( (satisfied $Body $P $N $TuplesB) 
      (unsatisfied $Head $P $N $TuplesH) 
      (set-det) 
      (= $Answer possibly) 
      (append $TuplesB $TuplesH $Tuples)))
; 

  (= 
    (incons 
      (= $Head $Body) $P $N Nil $Answer) 
    (= $Answer no))
; 



  (= 
    (satisfied 
      (, $A $B) $P $N $Tuples) 
    ( (satisfied $A $P $N $TuplesA) 
      (satisfied $B $P $N $TuplesB) 
      (append $TuplesA $TuplesB $Tuples)))
; 

  (= 
    (satisfied 
      (or $A $B) $P $N $Tuples) 
    (or 
      (satisfied $A $P $N $Tuples) 
      (satisfied $B $P $N $Tuples)))
; 

  (= 
    (satisfied $A $P $N 
      (:: (+ $A))) 
    (member $A $P))
; 

  (= 
    (satisfied  
      (= $A $A) $P $N ()) True)
; 
	;
; 

  (= 
    (satisfied $A $P $N Nil) 
    ( (proc $Rel $Proc) (exec-proc $A $Proc)))
; 



  (= 
    (exec-proc $Goal 
      (, $P1 $P2)) 
    (or 
      (exec-proc $Goal $P1) 
      (exec-proc $Goal $P2)))
; 

  (= 
    (exec-proc $Goal 
      (= $Goal $Body)) 
    (call $Body))
; 



  (= 
    (falsified 
      (, $A $B) $P $N $Tuples) 
    (or 
      (, 
        (set-det) 
        (falsified $A $P $N $Tuples)) 
      (falsified $B $P $N $Tuples)))
; 

  (= 
    (falsified $A $P $N 
      (:: (- $A))) 
    (member $A $N))
; 

  (= 
    (falsified 
      (= $A $B) $P $N Nil) 
    (\= $A $B))
; 



  (= 
    (unsatisfied 
      (, $A $B) $P $N $Tuples) 
    (or 
      (, 
        (set-det) 
        (unsatisfied $A $P $N $Tuples)) 
      (unsatisfied $B $P $N $Tuples)))
; 

  (= 
    (unsatisfied $A $P $N 
      (:: (- $A))) 
    ( (not (satisfied $A $P $N $_)) (not (falsified $A $P $N $_))))
; 


