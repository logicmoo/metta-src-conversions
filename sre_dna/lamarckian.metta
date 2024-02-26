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
    (lamarckian-evolution $Gen) 
    ( (lamarckian-P $Percent $K $_ $_) 
      (>= $Percent 1.0) 
      (writel (:: nl 'Lamarckian evolution...' nl)) 
      (population-size-P $_ $PopSize) 
      (num-list $PopSize $IDs) 
      (lamarck-loop $IDs 0 $FitImpr 0 $MaxImpr 0 $NumGain $K) 
      (add-symbol  &self 
        (gp_stats  $Gen $_ $_ $_ $_ $_ $_ 
          (lamarck  $FitImpr $MaxImpr $NumGain))) 
      (set-det)))
; 

  (= 
    (lamarckian-evolution $Gen) 
    ( (lamarckian-P $Percent $K $Select $_) 
      (< $Percent 1.0) 
      (population-size-P $_ $PopSize) 
      (is $N 
        (integer (* $Percent $PopSize))) 
      (writel (:: nl 'Lamarckian evolution...' nl)) 
      (get-unique-IDs $Select $N $PopSize Nil $IDs) 
      (lamarck-loop $IDs 0 $FitImpr 0 $MaxImpr 0 $NumGain $K) 
      (add-symbol  &self 
        (gp_stats  $Gen $_ $_ $_ $_ $_ $_ 
          (lamarck  $FitImpr $MaxImpr $NumGain))) 
      (set-det)))
; 


;
; 

;
; 



  (= 
    (get-unique-IDs $_ 0 $_ $IDs $IDs) 
    (set-det))
; 

  (= 
    (get-unique-IDs $Type $N $PopSize $SoFar $IDs) 
    ( (repeat) 
      (det-if-then-else 
        (= $Type random) 
        (my-random $PopSize $ID) 
        (tournament-select $Type $PopSize $ID $_)) 
      (not (member $ID $SoFar)) 
      (is $M 
        (- $N 1)) 
      (get-unique-IDs $Type $M $PopSize 
        (Cons  $ID $SoFar) $IDs) 
      (set-det)))
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
    (lamarck-loop Nil $FitImpr $FitImpr $MaxImpr $MaxImpr $NumGain $NumGain $_) 
    (set-det))
; 

  (= 
    (lamarck-loop 
      (Cons  $ID $Rest) $ImprSoFar $FitImpr $MaxSoFar $MaxImpr $NumSoFar $NumGain $Iter) 
    ( (individual $ID $Fit $Expr) 
      (hill-climb $Iter 
        (, $Fit $Expr) 
        (, $NewFit $NewExpr)) 
      (det-if-then-else 
        (or 
          (>= $NewFit $Fit) 
          (not (legal $NewExpr lamarck))) 
        (, 
          (writel -) 
          (= 
            (, $NewFitImpr $NewMaxImpr $NumSoFar2) 
            (, $ImprSoFar $MaxSoFar $NumSoFar))) 
        (, 
          (remove-symbol  &self 
            (individual  $ID $_ $_)) 
          (add-symbol  &self 
            (individual  $ID $NewFit $NewExpr)) 
          (is $NewFitImpr 
            (- 
              (+ $ImprSoFar $Fit) $NewFit)) 
          (is $NewMaxImpr 
            (max $MaxSoFar 
              (- $Fit $NewFit))) 
          (is $NumSoFar2 
            (+ $NumSoFar 1)) 
          (writel +))) 
      (lamarck-loop $Rest $NewFitImpr $FitImpr $NewMaxImpr $MaxImpr $NumSoFar2 $NumGain $Iter) 
      (set-det)))
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
    (hill-climb $K $Item $Item) 
    ( (=< $K 0) (set-det)))
; 

  (= 
    (hill-climb $K 
      (, $TopFit $TopExpr) $Soln) 
    ( (lamarckian-P $_ $_ $_ $PC) 
      (maybe $PC) 
      (population-size-P $_ $PopSize) 
      (tournament-select best $PopSize $_ $Expr2) 
      (crossover $TopExpr $Expr2 $NewExpr1 $NewExpr2) 
      (evaluator $NewExpr1 $NewFit1) 
      (evaluator $NewExpr2 $NewFit2) 
      (select-best 
        (, $NewFit1 $NewExpr1) 
        (, $TopFit $TopExpr) $BestSoFar1) 
      (select-best 
        (, $NewFit2 $NewExpr2) $BestSoFar1 $BestSoFar2) 
      (det-if-then-else 
        (or 
          (< $NewFit1 $TopFit) 
          (< $NewFit2 $TopFit)) 
        (= $K2 $K) 
        (is $K2 
          (- $K 2))) 
      (hill-climb $K2 $BestSoFar2 $Soln) 
      (set-det)))
; 

  (= 
    (hill-climb $K 
      (, $TopFit $TopExpr) $Soln) 
    ( (sre-mutation $TopExpr $NewExpr) 
      (evaluator $NewExpr $NewFit) 
      (select-best 
        (, $NewFit $NewExpr) 
        (, $TopFit $TopExpr) $BestSoFar) 
      (det-if-then-else 
        (< $NewFit $TopFit) 
        (= $K2 $K) 
        (is $K2 
          (- $K 1))) 
      (hill-climb $K2 $BestSoFar $Soln) 
      (set-det)))
; 

  (= 
    (hill-climb $K $BestSoFar $Soln) 
    ( (is $K2 
        (- $K 1)) 
      (hill-climb $K2 $BestSoFar $Soln) 
      (set-det)))
; 


;
; 



  (= 
    (select-best 
      (, $F1 $E1) 
      (, $F2 $_) 
      (, $F1 $E1)) 
    ( (=< $F1 $F2) (set-det)))
; 

  (= 
    (select_best  $_ $X $X) True)
; 




  (= 
    (sre-mutation $I $C) 
    (mutation $I $C))
; 


;
; 



  (= 
    (test-best-first $Iter $ID) 
    ( (population-size-P $_ $PopSize) 
      (tournament-select best $PopSize $ID $_) 
      (individual $ID $Fit $Expr) 
      (hill-climb $Iter 
        (, $Fit $Expr) 
        (, $NewFit $NewExpr)) 
      (writel (:: 'Initial: ' nl '  Fit = ' $Fit nl ' Expr = ' $Expr nl 'New: ' nl '  Fit = ' $NewFit nl ' Expr = ' $NewExpr nl)) 
      (set-det)))
; 

	

