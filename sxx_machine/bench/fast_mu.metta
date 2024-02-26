;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (top) 
    (theorem (:: m u i i u)))
; 


;
; 

;
; 


  (= 
    (theorem $G) 
    ( (length $G $GL1) 
      (is $GL 
        (- $GL1 1)) 
      (derive 
        (:: m i) $G 1 $GL $Derivation 0)))
; 

	;
; 


;
; 

;
; 


  (= 
    (derive $S $G $SL $GL $D $B) 
    (derive2 $S $G $SL $GL 1 $D $B))
; 

  (= 
    (derive $S $G $SL $GL $D $B) 
    ( (is $B1 
        (+ $B 1)) (derive $S $G $SL $GL $D $B1)))
; 


;
; 

;
; 


  (= 
    (derive2  $S $S $SL $SL $_ () $_) True)
; 

  (= 
    (derive2 $S $G $SL $GL $Pin 
      (Cons  
        (rule $N $I) $D) $R) 
    ( (lower-bound $SL $GL $B) 
      (>= $R $B) 
      (is $R1 
        (- $R 1)) 
      (rule $S $I $SL $IL $Pin $Pout $N) 
      (derive2 $I $G $IL $GL $Pout $D $R1)))
; 



  (= 
    (rule 
      (Cons  m $T1) 
      (Cons  m $T2) $L1 $L2 $Pin $Pout $N) 
    (rule $T1 $T2 $L1 $L2 $Pin $Pout 1 i $N $X $X))
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
    (rule 
      (:: i) 
      (:: i u) $L1 $L2 $Pin $Pout $Pos $_ 1 $_ $_) 
    ( (>= $Pos $Pin) 
      (is $Pout 
        (- $Pos 2)) 
      (is $L2 
        (+ $L1 1))))
; 

  (= 
    (rule Nil $L $L1 $L2 $_ 1 $_ $_ 2 $L Nil) 
    (is $L2 
      (+ $L1 $L1)))
; 

  (= 
    (rule 
      (Cons  i 
        (Cons  i 
          (Cons  i $T))) 
      (Cons  u $T) $L1 $L2 $Pin $Pout $Pos $_ 3 $_ $_) 
    ( (>= $Pos $Pin) 
      (is $Pout 
        (- $Pos 1)) 
      (is $L2 
        (- $L1 2))))
; 

  (= 
    (rule 
      (Cons  u 
        (Cons  u $T)) $T $L1 $L2 $Pin $Pout $Pos i 4 $_ $_) 
    ( (>= $Pos $Pin) 
      (is $Pout 
        (- $Pos 2)) 
      (is $L2 
        (- $L1 2))))
; 

  (= 
    (rule 
      (Cons  $H $T1) 
      (Cons  $H $T2) $L1 $L2 $Pin $Pout $Pos $_ $N $L 
      (Cons  $H $X)) 
    ( (is $Pos1 
        (+ $Pos 1)) (rule $T1 $T2 $L1 $L2 $Pin $Pout $Pos1 $H $N $L $X)))
; 


;
; 

;
; 

;
; 

;
; 

;
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
    (lower-bound $N $M 1) 
    (< $N $M))
; 

  (= 
    (lower_bound  $N $N 2) True)
; 

  (= 
    (lower-bound $N $M $B) 
    ( (> $N $M) 
      (is $Diff 
        (- $N $M)) 
      (is $P 
        (/\ $Diff 1)) 
      (det-if-then-else 
        (=:= $P 0) 
        (is $B 
          (>> $Diff 1)) 
        (is $B 
          (+ 
            (>> 
              (+ $Diff 1) 1) 1)))))
; 


;
; 


