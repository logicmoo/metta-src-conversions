;                                           (convert_to_metta_file  eval $_459408 index/eval.pl index/eval.metta)

  (= (evaluate1 $P $IC $Answer)  
    (switched-on eval)
    (dep $IC $Type $Rel $From $To)
    (filter $P $Rel $P1 $Rest)
    (splitsort $P1 $IC $SplitP $NumbersC)
    (calc-conf $NumbersC $Conf)
    (det-if-then-else 
      (compare (conf $Conf)) 
      (, 
        (divides $SplitP $IC $NewP $NumbersA) 
        (calc-acc $NumbersA $Acc) 
        (det-if-then-else 
          (compare (acc $Acc)) 
          (= $Answer 
            (keep (acc $Acc))) 
          (det-if-then 
            (compare (split $Acc)) 
            (= $Answer 
              (keep (split $Acc)))))) 
      (det-if-then otherwise 
        (= $Answer 
          (ignore (conf $Conf))))))
; ; write_debug(['splitsort...']),
; ; write_debug(['divides...']),


  (= (calc-conf (, $NDivs $NTuples) $Confirmation)  
    (is $Confirmation 
      (/ $NTuples $NDivs)))


  (= (calc-acc $Numbers $Accuracy)  
    (calc-acc1 $Numbers 0 $NTuples 0 $NLarge 0 $MaxNP)
    (is $Accuracy 
      (- 1 
        (/ 
          (* 
            (- $MaxNP 1) 
            (- $NTuples $NLarge)) $NTuples))))


  (= (calc_acc1  () $NT $NT $NL $NL $MaxNP $MaxNP)  True)
  (= (calc-acc1 (Cons  $D $Ds) $NT0 $NT $NL0 $NL $MaxNP0 $MaxNP)  
    (eval2 $D 0 $Sum 0 $Largest 0 $NParts)
    (is $NT1 
      (+ $NT0 $Sum))
    (is $NL1 
      (+ $NL0 $Largest))
    (det-if-then-else 
      (> $NParts $MaxNP0) 
      (= $MaxNP1 $NParts) 
      (det-if-then otherwise 
        (= $MaxNP1 $MaxNP0)))
    (calc-acc1 $Ds $NT1 $NT $NL1 $NL $MaxNP1 $MaxNP))


  (= (eval2  () $S $S $L $L $M $M)  True)
  (= (eval2 (Cons  $N $Ns) $S0 $S $L0 $L $M0 $M)  
    (is $S1 
      (+ $S0 $N))
    (det-if-then-else 
      (> $N $L0) 
      (= $L1 $N) 
      (det-if-then otherwise 
        (= $L1 $L0)))
    (is $M1 
      (+ $M0 1))
    (eval2 $Ns $S1 $S $L1 $L $M1 $M))


  (= (insert_ic  $L $IC $E (Cons  $IC $L))  True)

