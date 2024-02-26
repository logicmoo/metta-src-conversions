;                                           (convert-to-metta-file  reg_gram_1 $_450222 sre_dna/reg_gram_1.pl sre_dna/reg_gram_1.metta)

  (= (?-  (dynamic  (/  testset 2)))  True)

;
; Experiment: evolve a SRE to conform with this stoch. regular grammar
;
; (from Carrasco & Forcado 96)
;
; 
;
; S -> a S (0.2)
;
; S -> b A (0.8)
;
; A -> a B (0.7)
;
; A -> b S (0.3)
;
; B -> a A (0.4)
;
; B -> b B (0.1)
;
; B -> [] (0.5)
;
; 
;
; Fitness:
;
; - Mine expression K times, and compare distribution with test set.
;
; - maximum string length enforced
;
; - chi-square 2-bin test to compare distributions



  (= (evaluator $Expr $Fitness)  
    (testset $_ $TestSet)
    (gen-set-size-P $Size)
    (mine $Expr $Size $MineSet)
    (tabulate-set $MineSet 0 $_ $MineSet2)
    (chisquare-b $MineSet2 $TestSet $Size 0.0 $Fitness)
    (set-det))
; ;sre_mintestcnt_P(MC), ; new
; ; chisquare_2bins(MineSet2, TestSet, 0.0, Fitness),


  (= (mine $_ 0 Nil)  
    (set-det))
  (= (mine $Expr $K (Cons  $String $Rest))  
    (^^ $Expr 
      (raw-generate $String 0 $_))
    (is $K2 
      (- $K 1))
    (mine $Expr $K2 $Rest)
    (set-det))


  (= (normalize $_ Nil Nil)  
    (set-det))
  (= (normalize $Size (Cons  (, $A $C) $R) (Cons  (, $A $P) $R2))  
    (is $P 
      (/ $C $Size))
    (normalize $Size $R $R2)
    (set-det))


  (= (chisquare-b $_ Nil $_ $Fit $Fit)  
    (set-det))
  (= (chisquare-b $MineSet (Cons  (, $Test $Prob) $Rest) $Sum $FitSoFar $Fitness)  
    (member-remove 
      (, $Test $Count2) $MineSet $MineSet2)
    (is $X 
      (* $Prob $Sum))
    (is $T 
      (- $Count2 $X))
    (is $Fit2 
      (+ $FitSoFar 
        (/ 
          (* $T $T) $X)))
    (chisquare-b $MineSet2 $Rest $Sum $Fit2 $Fitness)
    (set-det))
  (= (chisquare-b $MineSet (Cons  (, $_ $Prob) $Rest) $Sum $FitSoFar $Fitness)  
    (is $Fit2 
      (+ $FitSoFar 
        (* $Prob $Sum)))
    (chisquare-b $MineSet $Rest $Sum $Fit2 $Fitness)
    (set-det))


  (= (member-remove $X (Cons  $X $Y) $Y)  
    (set-det))
  (= (member-remove $X (Cons  $Y $Z) (Cons  $Y $W))  
    (member-remove $X $Z $W))


  (= (count-and-remove $_ Nil Nil 0)  
    (set-det))
  (= (count-and-remove $A (Cons  $A $R) $S $Count)  
    (set-det)
    (count-and-remove $A $R $S $Count2)
    (is $Count 
      (+ $Count2 1)))
  (= (count-and-remove $A (Cons  $B $R) (Cons  $B $S) $Count)  
    (set-det)
    (count-and-remove $A $R $S $Count))

;
; This should be called once per GP generation.


  (= generate-testset  
    ( (or 
        (remove-atom  &self 
          (testset  $_ $_)) True) 
      (gen-set-size-P $Size) 
      (gen-set $Size $S) 
      (sre-mintestcnt-P $MC) 
      (tabulate-set $S $MC $Sum $T) 
      (normalize $Sum $T $T2) 
      (add-atom  &self 
        (testset  $Sum $T2)) 
      (set-det)))
; ; was 250


  (= (gen-set  0 ())  True)
  (= (gen-set $K (Cons  $S $R))  
    ( (> $K 0) repeat 
      (max-string-length-P $Max) 
      (gen-string s 0 $Max $S) 
      (is $K2 
        (- $K 1)) 
      (set-det) 
      (gen-set $K2 $R)))


  (= (gen-string $NonTerm $Len $Max $String)  
    (=< $Len $Max)
    (det-if-then-else 
      (production $NonTerm $Out $NextNonTerm) 
      (, 
        (is $Len2 
          (+ $Len 1)) 
        (gen-string $NextNonTerm $Len2 $Max $R) 
        (= $String 
          (Cons  $Out $R))) 
      (= $String Nil)))


;
; production(NonTerm, Output, NextNonTerm).


  (= (production s a s)  
    (maybe 0.2))
  (= (production  s b a)  True)
  (= (production a a b)  
    (maybe 0.7))
  (= (production  a b s)  True)
  (= (production b a a)  
    (maybe 0.4))
  (= (production b b b)  
    (maybe 0.167))

;
; pre-filter and count test set, rather than repeatedly process
;
; it during fitness evaluation of population. User can specify minimum
;
; count for processing.


  (= (tabulate-set  () $_ 0 ())  True)
  (= (tabulate-set (Cons  $A $R) $Min $Sum (Cons  (, $A $C2) $S))  
    (once (count-and-remove $A $R $R2 $C))
    (is $C2 
      (+ $C 1))
    (> $C2 $Min)
    (set-det)
    (tabulate-set $R2 $Min $Sum2 $S)
    (is $Sum 
      (+ $Sum2 $C2)))
  (= (tabulate-set (Cons  $_ $R) $Min $Sum $S)  
    (set-det)
    (tabulate-set $R $Min $Sum $S))


  !(multifile (/ chisquare 4))
  !(dynamic (/ chisquare 4))

  (= (temp-test $Fitness)  
    ( (generate-tree expr full 12 $_ $Tree1 $_) 
      (write 'Mining 1... ') 
      (mine $Tree1 500 $MineSet1) repeat 
      (generate-tree expr full 12 $_ $Tree2 $_) nl 
      (write 'Mining 2... ') 
      (mine $Tree2 500 $MineSet2) nl 
      (write 'chi square...') 
      (chisquare $MineSet1 $MineSet2 0.0 $Fitness) nl 
      (^^ $Tree1 
        (construct $Expr1)) 
      (^^ $Tree2 
        (construct $Expr2)) 
      (write 'Expr 1:') 
      (sre-pp $Expr1) 
      (write 'Expr 2:') 
      (sre-pp $Expr2)))


  (= (chisquare-2bins Nil Nil $Fit $Fit)  
    (set-det))
; /* test_chi(F) :- generate_testset, testset(_,A), generate_testset, testset(_,B), chisquare(A,B,0,F). */
  (= (chisquare-2bins (Cons  (, $_ $Count) $Rest) Nil $FitSoFar $Fit)  
    (is $Fit2 
      (+ $FitSoFar $Count))
    (chisquare-2bins $Rest Nil $Fit2 $Fit)
    (set-det))
  (= (chisquare-2bins $MineSet (Cons  (, $Test $Count) $Rest) $FitSoFar $Fitness)  
    (member-remove 
      (, $Test $Count2) $MineSet $MineSet2)
    (is $T 
      (- $Count2 $Count))
    (is $Fit2 
      (+ $FitSoFar 
        (/ 
          (* $T $T) 
          (+ $Count $Count2))))
    (chisquare-2bins $MineSet2 $Rest $Fit2 $Fitness)
    (set-det))
  (= (chisquare-2bins $MineSet (Cons  (, $_ $Count) $Rest) $FitSoFar $Fitness)  
    (is $Fit2 
      (+ $FitSoFar $Count))
    (chisquare-2bins $MineSet $Rest $Fit2 $Fitness)
    (set-det))


