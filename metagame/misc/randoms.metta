;
; 

;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  !(my-use-module (library random))
; 



  !(dynamic (/ old-seed 1))
; 



  (= 
    (seed $S) 
    (getrand $S))
; 




;
; 

;
; 

;
; 


  (= 
    (reset-random $S) 
    (setrand $S))
; 


;
; 

;
; 

  (= 
    (reset-random) 
    (random $_))
; 



;
; 

;
; 

;
; 



  (= 
    (record-seed) 
    ( (seed $S) 
      (remove-all-symbols  &self 
        (old_seed  $_)) 
      (add-symbol  &self 
        (old_seed  $S))))
; 



;
; 

;
; 


  (= 
    (recover-random) 
    ( (old-seed $S) (reset-random $S)))
; 


;
; 


  (= 
    (write-old-seed $CommentChar) 
    ( (old-seed $Seed) 
      (set-det) 
      (format "~n~w RANDOM SEED: ~w~n" 
        (:: $CommentChar $Seed))))
; 

  (= write_old_seed True)
; 




;
; 

;
; 

;
; 



  (= 
    (random-include $L $U $R) 
    ( (integer $L) 
      (integer $U) 
      (set-det) 
      (is $U1 
        (+ $U 1)) 
      (random $L $U1 $R)))
; 

  (= 
    (random-include $L $U $R) 
    (random $L $U $R))
; 




;
; 

;
; 


  (= 
    (random $R $N) 
    ( (is $R1 
        (+ $R 1)) (random 1 $R1 $N)))
; 




;
; 

;
; 



  (= 
    (random-element $Set $Element) 
    ( (length $Set $Length) 
      (random $Length $R) 
      (nth $R $Set $Element)))
; 


;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 


;
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
    (random-permute Nil Nil) 
    (set-det))
; 

  (= 
    (random-permute $List1 
      (Cons  $Item $Rest)) 
    ( (random-select $Item $List1 $List) (random-permute $List $Rest)))
; 


;
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
    (random-bagof $A $B $C) 
    ( (bagof $A $B $C1) (random-permute $C1 $C)))
; 



  (= 
    (random-setof $A $B $C) 
    ( (setof $A $B $C1) (random-permute $C1 $C)))
; 



  (= 
    (random-findall $A $B $C) 
    ( (findall $A $B $C1) (random-permute $C1 $C)))
; 



;
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
    (random-arg $Term $Element) 
    ( (functor $Term $Args $Arity) 
      (random $Arity $R) 
      (arg $R $Term $Element)))
; 



;
; 

;
; 

;
; 



  (= 
    (random-success $Call) 
    ( (bagof $Call 
        (^ $Call 
          (call $Call)) $Calls) (random-element $Calls $Call)))
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
    (randomly-pair Nil $_ Nil) 
    (set-det))
; 

  (= 
    (randomly-pair $_ Nil Nil) 
    (set-det))
; 

  (= 
    (randomly-pair 
      (Cons  $A $As) $Set 
      (Cons  
        (= $A $Elt) $Pairs)) 
    ( (random-element $Set $Elt) 
      (extract $Elt $Set $Set1) 
      (randomly-pair $As $Set1 $Pairs)))
; 




;
; 

;
; 


  (= 
    (random-subsets 0 $_ $_ Nil) 
    (set-det))
; 

  (= 
    (random-subsets $N $Size $Set 
      (Cons  $Elt $Rest)) 
    ( (random-subset $Size $Set $Elt) 
      (is $N1 
        (- $N 1)) 
      (random-subsets $N1 $Size $Set $Rest)))
; 


;
; 

;
; 


  (= 
    (random-subset 0 $_ Nil) 
    (set-det))
; 

  (= 
    (random-subset $Size $Set 
      (Cons  $Elt $Rest)) 
    ( (random-element $Set $Elt) 
      (extract $Elt $Set $Set1) 
      (is $Size1 
        (- $Size 1)) 
      (random-subset $Size1 $Set1 $Rest)))
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
    (random-different-args $Term $Arg1 $Arg2) 
    ( (random-arg $Term $Arg1) (random-different-arg $Term $Arg1 $Arg2)))
; 




  (= 
    (random-different-arg $Term $ArgA $ArgB) 
    (random-different-arg $Term $ArgA $ArgA $ArgB))
; 


  (= 
    (random-different-arg $Term $ArgA $ArgB $ArgB) 
    ( (\== $ArgA $ArgB) (set-det)))
; 

  (= 
    (random-different-arg $Term $ArgA $ArgA $ArgB) 
    ( (random-arg $Term $Arg1) (random-different-arg $Term $ArgA $Arg1 $ArgB)))
; 



;
; 

;
; 

;
; 


  (= 
    (random-test $R $N $Ratio) 
    (random-test $R $N 0 0 100 $Ratio))
; 


;
; 

;
; 

;
; 

  (= 
    (random-test $R $N $SampleSize $Ratio) 
    (random-test $R $N 0 0 $SampleSize $Ratio))
; 


  (= 
    (random-test $R $N $Tried $Found $Tried $Ratio) 
    ( (is $Ratio 
        (/ $Found $Tried)) (set-det)))
; 

  (= 
    (random-test $R $N $Tried $Found $Total $Ratio) 
    ( (random $R $N1) 
      (det-if-then-else 
        (= $N $N1) 
        (is $Found1 
          (+ $Found 1)) 
        (= $Found1 $Found)) 
      (is $Tried1 
        (+ $Tried 1)) 
      (random-test $R $N $Tried1 $Found1 $Total $Ratio)))
; 

  

  (= 
    (sample-from-distribution $Dist $Choice) 
    ( (distribution $Dist $Options) 
      (random $R) 
      (in-prob-region $Options $R $Choice1) 
      (= $Choice1 $Choice)))
; 



  (= 
    (distribution  
      (distribution  $Choices) $Choices) True)
; 


;
; 


  (= 
    (in-prob-region 
      (:: (= $Choice $Prob)) $_ $Choice) 
    (set-det))
; 

  (= 
    (in-prob-region 
      (Cons  
        (= $C $P) $Choices) $Prob $C) 
    ( (< $Prob $P) (set-det)))
; 

  (= 
    (in-prob-region 
      (Cons  
        (= $C $P) $Choices) $Prob $Choice) 
    ( (is $PRest 
        (- $Prob $P)) (in-prob-region $Choices $PRest $Choice)))
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
    (sample-from-range $Range $Choice) 
    ( (range $Range $Min $Max) (random-include $Min $Max $Choice)))
; 



  (= 
    (range  
      (range  $Min $Max) $Min $Max) True)
; 




;
; 

;
; 

;
; 

;
; 

;
; 


;
; 

;
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
    (sample $Dist $Choice) 
    ( (pair-list $Choices $Weights $Dist) 
      (sum-list $Weights $Total) 
      (random 0.0 $Total $R) 
      (in-region $Dist $R $Choice1) 
      (= $Choice1 $Choice)))
; 


;
; 

;
; 

  (= 
    (sample 0 $_ Nil) 
    (set-det))
; 

  (= 
    (sample $N $Dist 
      (Cons  $S $Samples)) 
    ( (> $N 0) 
      (is $N1 
        (- $N 1)) 
      (sample $Dist $S) 
      (sample $N1 $Dist $Samples)))
; 


;
; 

;
; 

;
; 


  (= 
    (sample-subsets 0 $_ $_ Nil) 
    (set-det))
; 

  (= 
    (sample-subsets $N $Size $Dist 
      (Cons  $Elt $Rest)) 
    ( (sample $Size $Dist $Elt) 
      (is $N1 
        (- $N 1)) 
      (sample-subsets $N1 $Size $Dist $Rest)))
; 





  (= 
    (test-sample) 
    ( (repeat) 
      (sample 
        (:: 
          (- a 5) 
          (- b 10) 
          (- d 15)) $Choice) 
      (write $Choice) 
      (nl) 
      (fail)))
; 



;
; 

;
; 

;
; 



  (= 
    (randomize $N) 
    ( (format 'Using random seed #~p.~n' 
        (:: $N)) (randomize0 $N)))
; 



  (= 
    (randomize0 1) 
    (setrand (random 2260 5202 18078 -111865839)))
; 

  (= 
    (randomize0 2) 
    (setrand (random 1676 2152 14938 -111865839)))
; 

  (= 
    (randomize0 3) 
    (setrand (random 14918 9840 11226 -111865839)))
; 

  (= 
    (randomize0 4) 
    (setrand (random 11477 9180 488 -111865839)))
; 

  (= 
    (randomize0 5) 
    (setrand (random 27112 8989 12856 -111865839)))
; 

  (= 
    (randomize0 6) 
    (setrand (random 27949 24755 16306 -111865839)))
; 

  (= 
    (randomize0 7) 
    (setrand (random 3126 20129 24910 -111865839)))
; 

  (= 
    (randomize0 8) 
    (setrand (random 21946 18049 2077 -111865839)))
; 

  (= 
    (randomize0 9) 
    (setrand (random 26016 4946 13012 -111865839)))
; 

  (= 
    (randomize0 10) 
    (setrand (random 18553 19429 25736 -111865839)))
; 

  (= 
    (randomize0 test) 
    (setrand (random 1734 10872 10679 -111865839)))
; 


