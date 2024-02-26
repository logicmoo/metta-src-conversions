
;
;                              Nov/99

;
;                               DNA data processing

;
;                              	1. remove duplicates

;
;                              	2. break up upper-case constants to lists of lower-case constants



  (= 
    (dna-proc $List $List3) 
    ( (remove-dups $List $List2) 
      (conv-to-lc-atoms $List2 $List3) 
      (make-new-alpha $List3)))

;
;                                dna_proc2 done by unaligned processing. Pad examples with random

;
;                                alphabet. Don't filter alphabet either, like in dna_proc.



  (= 
    (dna-proc2 $List $List4) 
    ( (remove-dups $List $List2) 
      (conv-to-lc-atoms $List2 $List3) 
      (unaligned-P $_ $StrSize) 
      (= $List3 
        (Cons  $S $_)) 
      (length $S $L) 
      (is $PadSize 
        (integer (/ (- $StrSize $L) 2))) 
      (pad-randomly $List3 $PadSize $List4)))	


  (= 
    (remove-dups Nil Nil) 
    (set-det))
  (= 
    (remove-dups 
      (Cons  $A $R) $S) 
    ( (member $A $R) 
      (set-det) 
      (remove-dups $R $S)))
  (= 
    (remove-dups 
      (Cons  $A $R) 
      (Cons  $A $S)) 
    (remove-dups $R $S))


  (= 
    (conv-to-lc-atoms Nil Nil) 
    (set-det))
  (= 
    (conv-to-lc-atoms 
      (Cons  $A $R) 
      (Cons  $B $S)) 
    ( (name $A $L) 
      (conv-to-lc-atoms2 $L $B) 
      (conv-to-lc-atoms $R $S)))


  (= 
    (conv-to-lc-atoms2 Nil Nil) 
    (set-det))
  (= 
    (conv-to-lc-atoms2 
      (Cons  $A $R) 
      (Cons  $B $S)) 
    ( (is $A2 
        (+ $A 32)) 
      (name $B 
        (:: $A2)) 
      (conv-to-lc-atoms2 $R $S)))
;                                    ; upper-case ascii to lower ascii




  (= 
    (sum-lengths Nil 0) 
    (set-det))
  (= 
    (sum-lengths 
      (Cons  $A $R) $S) 
    ( (sum-lengths $R $T) 
      (length $A $L) 
      (is $S 
        (+ $T $L))))

;
;                                      makes N random sequences of size =< A.



  (= 
    (make-random-strings $N $_ $_ Nil) 
    ( (=< $N 0) (set-det)))
  (= 
    (make-random-strings $N $L $Plist 
      (Cons  $S $R)) 
    ( (is $M 
        (- $N 1)) 
      (make-random-strings $M $L $Plist $R) 
      (repeat) 
      (make-randstring $L $S) 
      (not (member $S $R)) 
      (not (member $S $Plist)) 
      (set-det)))


  (= 
    (make-randstring $N Nil) 
    ( (or 
        (=< $N 0) maybe) (set-det)))
  (= 
    (make-randstring $N 
      (Cons  $A $R)) 
    ( (alphabet-P $_ $L) 
      (select-rand $L $A) 
      (is $M 
        (- $N 1)) 
      (make-randstring $M $R)))



  (= 
    (make-new-alpha $List) 
    ( (append-all $List Nil $All) 
      (remove-dups $All $All2) 
      (length $All2 $L) 
      (remove-atom  &self 
        (alphabet_P  $_ $_)) 
      (add-atom  &self 
        (alphabet_P  $L $All2)) 
      (set-det)))


  (= 
    (append-all Nil $A $A) 
    (set-det))
  (= 
    (append-all 
      (Cons  $A $R) $B $C) 
    ( (append $A $B $D) 
      (append-all $R $D $C) 
      (set-det)))


  (= 
    (pad-randomly Nil $_ Nil) 
    (set-det))
  (= 
    (pad-randomly 
      (Cons  $S $R) $Size 
      (Cons  $S2 $R2)) 
    ( (make-randstring3 $Size $Left) 
      (make-randstring3 $Size $Right) 
      (append $Left $S $T) 
      (append $T $Right $S2) 
      (set-det) 
      (pad-randomly $R $Size $R2)))


  (= 
    (make-randstring3 $N Nil) 
    ( (=< $N 0) (set-det)))
  (= 
    (make-randstring3 $N 
      (Cons  $A $R)) 
    ( (alphabet-P $_ $L) 
      (select-rand $L $A) 
      (is $M 
        (- $N 1)) 
      (make-randstring3 $M $R)))


