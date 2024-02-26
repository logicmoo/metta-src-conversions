;
; 

;
; 

;
; 

;
; 

;
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
    (?-  
      (dynamic  
        (/  been_here 0))) True)
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
    (memberd $A $L) 
    (once (member $A $L)))
; 


;
; 

;
; 



  (= 
    (my-random $R $N) 
    ( (random-between 1 $R $N) (set-det)))
; 


;
; 

;
; 



  (= 
    (probability2 $P $M) 
    ( (random $X) 
      (is $Y 
        (* $X $M)) 
      (=< $Y $P) 
      (set-det)))
; 



  (= 
    (size_of  () 0) True)
; 

  (= 
    (size-of 
      (Cons  $_ $R) $K) 
    ( (size-of $R $L) 
      (is $K 
        (+ $L 1)) 
      (set-det)))
; 


;
; 



  (= 
    (writel Nil) 
    ( (set-det) (ttyflush)))
; 

  (= 
    (writel (Cons  $A $R)) 
    ( (var $A) 
      (set-det) 
      (write $A) 
      (write ' ') 
      (writel $R)))
; 

  (= 
    (writel (Cons  nl $R)) 
    ( (set-det) 
      (nl) 
      (writel $R)))
; 

  (= 
    (writel (Cons  $A $R)) 
    ( (set-det) 
      (write $A) 
      (write ' ') 
      (writel $R)))
; 

  (= 
    (writel $A) 
    ( (write $A) 
      (ttyflush) 
      (set-det)))
; 



  (= 
    (writel2 Nil) 
    ( (set-det) (ttyflush)))
; 

  (= 
    (writel2 (Cons  $A $R)) 
    ( (var $A) 
      (set-det) 
      (write $A) 
      (writel2 $R)))
; 

  (= 
    (writel2 (Cons  nl $R)) 
    ( (set-det) 
      (nl) 
      (writel2 $R)))
; 

  (= 
    (writel2 (Cons  $A $R)) 
    ( (set-det) 
      (write $A) 
      (writel2 $R)))
; 

  (= 
    (writel2 $A) 
    ( (write $A) 
      (ttyflush) 
      (set-det)))
; 



  (= 
    (copy-struct $S $T) 
    ( (add-symbol  &self 
        (temp  $S)) 
      (remove-symbol  &self 
        (temp  $T)) 
      (set-det)))
; 



  (= 
    (count  $_ () 0) True)
; 

  (= 
    (count $X 
      (Cons  $X $Y) $C) 
    ( (set-det) 
      (count $X $Y $C2) 
      (is $C 
        (+ $C2 1))))
; 

  (= 
    (count $X 
      (Cons  $_ $Y) $C) 
    (count $X $Y $C))
; 



  (= 
    (round $X $Y) 
    ( (is $Y 
        (integer (+ $X 0.5))) (set-det)))
; 



  (= 
    (set-random-number-gen) 
    ( (been-here) (set-det)))
; 

  (= 
    (set-random-number-gen) 
    ( (add-symbol  &self been_here) 
      (set-det) 
      (seed-P $X $Y) 
      (set-seed $X $Y)))
; 



  (= 
    (set-seed default $_) 
    (set-det))
; 

  (= 
    (set-seed random $_) 
    ( (datime (datime $Year $Month $Day $Hour $Min $Sec)) 
      (is $N 
        (+ 
          (* 
            (+ 
              (* 
                (+ 
                  (* 
                    (+ 
                      (* 
                        (+ 
                          (* 
                            (- $Year 1970) 12) $Month) 30) $Day) 24) $Hour) 60) $Min) 60) $Sec)) 
      (is $R1 
        (+ 
          (mod $N 30270) 1)) 
      (is $N2 
        (abs (>> $N 2))) 
      (is $R2 
        (+ 
          (mod $N2 30308) 1)) 
      (is $N3 
        (abs (>> $N 4))) 
      (is $R3 
        (+ 
          (mod $N3 30324) 1)) 
      (setrand (rand $R1 $R2 $R3)) 
      (remove-symbol  &self 
        (seed_P  $_ $_)) 
      (add-symbol  &self 
        (seed_P  random 
          (,  $R1 
            (,  $R2 $R3)))) 
      (set-det)))
; 

  (= 
    (set-seed manual 
      (, $X $Y $Z)) 
    (setrand (rand $X $Y $Z)))
; 

	


  (= 
    (debug-echo $L) 
    ( (debug-set-P yes) 
      (set-det) 
      (writel $L)))
; 

  (= 
    (debug_echo  $_) True)
; 



  (= 
    (rem_dups  () ()) True)
; 

  (= 
    (rem-dups 
      (Cons  $A $R) $R2) 
    ( (member $A $R) 
      (set-det) 
      (rem-dups $R $R2)))
; 

  (= 
    (rem-dups 
      (Cons  $A $R) 
      (Cons  $A $R2)) 
    ( (rem-dups $R $R2) (set-det)))
; 



  (= 
    (average $M $Avg) 
    ( (sum-list $M $Sum) 
      (size-of $M $N) 
      (is $Avg 
        (/ $Sum $N)) 
      (set-det)))
; 


;
; 



  (= 
    (extend-list $A $_ $K $A) 
    ( (length $A $K2) 
      (>= $K2 $K) 
      (set-det)))
; 

  (= 
    (extend-list $A $B $K $A2) 
    ( (append $A $B $A3) 
      (extend-list $A3 $B $K $A2) 
      (set-det)))
; 



  (= 
    (num-list 0 Nil) 
    (set-det))
; 

  (= 
    (num-list $N 
      (Cons  $N $R)) 
    ( (is $M 
        (- $N 1)) (num-list $M $R)))
; 



  (= 
    (remove  $_ () ()) True)
; 

  (= 
    (remove  $A 
      (Cons  $A $B) $B) True)
; 

  (= 
    (remove $A 
      (Cons  $X $B) 
      (Cons  $X $C)) 
    (remove $A $B $C))
; 



  (= 
    (remove_all  $_ () ()) True)
; 

  (= 
    (remove-all $A 
      (Cons  $A $B) $C) 
    ( (set-det) (remove-all $A $B $C)))
; 

  (= 
    (remove-all $A 
      (Cons  $X $B) 
      (Cons  $X $C)) 
    (remove-all $A $B $C))
; 




  (= 
    (intersect  () $_ ()) True)
; 

  (= 
    (intersect 
      (Cons  $X $Y) $R 
      (Cons  $X $Z)) 
    ( (member $X $R) 
      (set-det) 
      (intersect $Y $R $Z)))
; 

  (= 
    (intersect 
      (Cons  $_ $Y) $R $Z) 
    (intersect $Y $R $Z))
; 



  (= 
    (set-diff Nil $T $T) 
    (set-det))
; 

  (= 
    (set-diff $T Nil $T) 
    (set-det))
; 

  (= 
    (set-diff 
      (Cons  $A $B) $T $Diff) 
    ( (member $A $T) 
      (set-det) 
      (remove-all $A $T $T2) 
      (set-diff $B $T2 $Diff)))
; 

  (= 
    (set-diff 
      (Cons  $A $B) $T 
      (Cons  $A $R)) 
    (set-diff $B $T $R))
; 



  (= 
    (remove-list Nil $B $B) 
    (set-det))
; 

  (= 
    (remove-list 
      (Cons  $A $B) $C $D) 
    ( (remove $A $C $E) 
      (set-det) 
      (remove-list $B $E $D)))
; 



  (= 
    (writelist Nil) 
    ( (nl) (set-det)))
; 

  (= 
    (writelist (Cons  $A $R)) 
    ( (write $A) 
      (nl) 
      (writelist $R) 
      (set-det)))
; 



  (= 
    (maybe) 
    ( (maybe 0.5) (set-det)))
; 

	
  (= 
    (maybe $X) 
    ( (random $Y) 
      (< $Y $X) 
      (set-det)))
; 



  (= 
    (random-permutation $L $Perm) 
    ( (length $L $Len) 
      (random-permutation2 $L $Len $Perm) 
      (set-det)))
; 



  (= 
    (random_permutation2  () $_ ()) True)
; 

  (= 
    (random-permutation2 $L $Len 
      (Cons  $X $Perm)) 
    ( (random 0 $Len $R) 
      (remove-nth $R $L $X $L2) 
      (is $Len2 
        (- $Len 1)) 
      (random-permutation2 $L2 $Len2 $Perm) 
      (set-det)))
; 



  (= 
    (remove-nth 0 
      (Cons  $X $Y) $X $Y) 
    (set-det))
; 

  (= 
    (remove-nth $N 
      (Cons  $X $Y) $Z 
      (Cons  $X $W)) 
    ( (is $N2 
        (- $N 1)) 
      (remove-nth $N2 $Y $Z $W) 
      (set-det)))
; 



  (= 
    (select-rand $L $R) 
    ( (length $L $Len) 
      (> $Len 0) 
      (random 0 $Len $Rand) 
      (remove-nth $Rand $L $R $_) 
      (set-det)))
; 



;
; 

;
; 



  (= 
    (first-K $M $N $_ Nil) 
    ( (>= $M $N) (set-det)))
; 

  (= 
    (first-K $M $N 
      (Cons  $A $R) 
      (Cons  $A $S)) 
    ( (is $M2 
        (+ $M 1)) 
      (first-K $M2 $N $R $S) 
      (set-det)))
; 



