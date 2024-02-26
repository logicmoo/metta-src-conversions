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
    (gp) 
    ( (clean-up-1) 
      (add-symbol  &self 
        (best_so_far  $_ $_ 1000 $_)) 
      (max-runs-P $MaxRuns $RunType $_) 
      (set-det) 
      (meta-run-loop 1 $MaxRuns $RunType) 
      (writel (:: '*** END ***' nl nl))))
; 



  (= 
    (meta-run-loop $Runs $MaxRuns $_) 
    ( (> $Runs $MaxRuns) 
      (set-det) 
      (best-so-far $Run $Gen $Fitness $Expr) 
      (writel (:: nl '--> Max run' $MaxRuns ' reached.' nl 'Best found in run ' $Run ' gen ' $Gen : nl '   Expr = ' $Expr nl '   Fitness = ' $Fitness nl nl)) 
      (writel (:: '--> Finished runs <--' nl nl)) 
      (set-det)))
; 

  (= 
    (meta-run-loop $Run $MaxRuns $RunType) 
    ( (=< $Run $MaxRuns) 
      (population-size-P $_ $PopSize) 
      (max-runs-P $_ $_ $MaxGen) 
      (writel (:: nl '---------------------  Run ' $Run  --------------------- nl)) 
      (since-last-datime total retract $Hour $Minute $Sec) 
      (since-last-datime generation retract $_ $_ $_) 
      (do-the-run 0 $MaxGen $PopSize) 
      (write 'Dumping stats... ') 
      (dump-stats $Run) 
      (write done) 
      (nl) 
      (set-best-so-far $Run) 
      (det-if-then-else 
        (, 
          (== $RunType solution) 
          (solved-run)) True 
        (, 
          (is $Run2 
            (+ $Run 1)) 
          (meta-run-loop $Run2 $MaxRuns $RunType)))))
; 



  (= 
    (do-the-run $Gen $MaxGen $_) 
    ( (> $Gen $MaxGen) (set-det)))
; 

  (= 
    (do-the-run $_ $_ $_) 
    ( (solved-run) (set-det)))
; 

  (= 
    (do-the-run 0 $MaxGen $PopSize) 
    ( (clean-up-2) 
      (add-symbol  &self 
        (best_in_run  $_ 1000 $_)) 
      (writel (:: nl '********* Generation ' 0 ********* nl)) 
      (evaluator-reset 0) 
      (genesis) 
      (set-best-in-run 0) 
      (print-tourn-stats 0) 
      (garbage-collect) 
      (set-det) 
      (do-the-run 1 $MaxGen $PopSize)))
; 

  (= 
    (do-the-run $Gen $MaxGen $PopSize) 
    ( (writel (:: nl '********* Generation ' $Gen ********* nl)) 
      (evaluator-reset $Gen) 
      (elite-migration 1 $StartSize) 
      (tournament-loop $StartSize $PopSize) 
      (rename-new-popn) 
      (det-if-then-else 
        (, 
          (lamarckian-P $P $_ $_ $_) 
          (> $P 0)) 
        (lamarckian-evolution $Gen) True) 
      (set-best-in-run $Gen) 
      (print-tourn-stats $Gen) 
      (is $Gen2 
        (+ $Gen 1)) 
      (garbage-collect) 
      (set-det) 
      (do-the-run $Gen2 $MaxGen $PopSize)))
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
    (tournament-loop $K $PopSize) 
    ( (> $K $PopSize) (set-det)))
; 

  (= 
    (tournament-loop $_ $_) 
    ( (solved-run) (set-det)))
; 

  (= 
    (tournament-loop $K $PopSize) 
    ( (prob-crossover-P $PC) 
      (maybe $PC) 
      (tournament-select best $PopSize $_ $Expr1) 
      (tournament-select best $PopSize $_ $Expr2) 
      (det-if-then-else 
        (crossover $Expr1 $Expr2 $NewExpr1 $NewExpr2) 
        (, 
          (add-child c $K $K2 $PopSize $NewExpr1) 
          (add-child c $K2 $K3 $PopSize $NewExpr2)) 
        (= $K $K3)) 
      (tournament-loop $K3 $PopSize)))
; 

  (= 
    (tournament-loop $K $PopSize) 
    ( (tournament-select best $PopSize $_ $Expr) 
      (det-if-then-else 
        (mutation $Expr $NewExpr) 
        (add-child m $K $K2 $PopSize $NewExpr) 
        (= $K $K2)) 
      (tournament-loop $K2 $PopSize)))
; 


;
; 

;
; 

;
; 



  (= 
    (tournament-select best $PopSize $ID $Expression) 
    ( (tournament-size-P $Num $_) 
      (select-random-IDs 0 $Num $PopSize Nil $IDs) 
      (select best $IDs $ID $Expression) 
      (set-det)))
; 

  (= 
    (tournament-select worst $PopSize $ID $Expression) 
    ( (tournament-size-P $_ $Num) 
      (select-random-IDs 0 $Num $PopSize Nil $IDs) 
      (select worst $IDs $ID $Expression) 
      (set-det)))
; 


;
; 

;
; 



  (= 
    (select-random-IDs $Size $Size $_ $Result $Result) 
    (set-det))
; 

  (= 
    (select-random-IDs $N $Size $PopSize $SoFar $Result) 
    ( (repeat) 
      (my-random $PopSize $K) 
      (not (member $K $SoFar)) 
      (is $N2 
        (+ $N 1)) 
      (select-random-IDs $N2 $Size $PopSize 
        (Cons  $K $SoFar) $Result)))
; 


;
; 

;
; 

;
; 



  (= 
    (select $Type 
      (Cons  $ID1 $Rest) $ID $Expression) 
    ( (individual $ID1 $Fit1 $_) (select2 $Type $Fit1 $ID1 $Rest $ID $Expression)))
; 



  (= 
    (select2 $_ $_ $ID Nil $ID $Expression) 
    ( (individual $ID $_ $Expression) (set-det)))
; 

  (= 
    (select2 $Type $Fit1 $_ 
      (Cons  $ID2 $Rest) $ID $Expression) 
    ( (individual $ID2 $Fit2 $_) 
      (or 
        (, 
          (== $Type best) 
          (< $Fit2 $Fit1)) 
        (, 
          (== $Type worst) 
          (> $Fit2 $Fit1))) 
      (set-det) 
      (select2 $Type $Fit2 $ID2 $Rest $ID $Expression)))
; 

  (= 
    (select2 $Type $Fit1 $ID1 
      (Cons  $_ $Rest) $ID $Expression) 
    (select2 $Type $Fit1 $ID1 $Rest $ID $Expression))
; 


;
; 

;
; 




  (= 
    (add-child $T $K $K2 $PopSize $Expr) 
    ( (det-if-then-else 
        (not (legal $Expr main)) 
        (= $K2 $K) 
        (, 
          (det-if-then-else 
            (eval-with-ID-P yes) 
            (evaluator $K $Expr $Fitness) 
            (evaluator $Expr $Fitness)) 
          (add-individual $PopSize $Fitness $Expr) 
          (writel $T) 
          (is $K2 
            (+ $K 1)))) (set-det)))
; 

		

  (= 
    (add-individual $_ $Fitness $NewExpr) 
    ( (gen-type-P separate) 
      (set-det) 
      (add-symbol  &self 
        (newindividual  $_ $Fitness $NewExpr))))
; 

  (= 
    (add-individual $PopSize $Fitness $NewExpr) 
    ( (tournament-select worst $PopSize $ID $_) 
      (remove-symbol  &self 
        (individual  $ID $_ $_)) 
      (add-symbol  &self 
        (individual  $ID $Fitness $NewExpr))))
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
    (legal $Expr $Flag) 
    ( (check-unique $Expr $Flag) 
      (check-depth $Expr) 
      (set-det)))
; 



  (= 
    (check-unique $_ $_) 
    ( (not (unique-population-P yes)) (set-det)))
; 

  (= 
    (check-unique $Expr main) 
    ( (gen-type-P separate) 
      (set-det) 
      (not (newindividual $_ $_ $Expr))))
; 

  (= 
    (check-unique $Expr $_) 
    (not (individual $_ $_ $Expr)))
; 


;
; 



  (= 
    (check-depth $Expr) 
    ( (max-depth-P $_ $MaxDepth) 
      (tree-depth $Expr $D) 
      (=< $D $MaxDepth) 
      (set-det)))
; 


;
; 



  (= 
    (solved-run) 
    ( (best-in-run $_ $BFitness $_) 
      (error-tolerance-P $Err) 
      (=< $BFitness $Err) 
      (set-det)))
; 

	

  (= 
    (clean-up-1) 
    ( (set-random-number-gen) 
      (remove-all-symbols  &self 
        (start_time  $_)) 
      (remove-all-symbols  &self 
        (best_so_far  $_ $_ $_ $_)) 
      (garbage-collect) 
      (set-det)))
; 



  (= 
    (clean-up-2) 
    ( (remove-all-symbols  &self 
        (best_in_run  $_ $_ $_)) 
      (remove-all-symbols  &self 
        (gp_stats  $_ $_ $_ $_ $_ $_ $_)) 
      (remove-all-symbols  &self 
        (individual  $_ $_ $_)) 
      (remove-all-symbols  &self 
        (newindividual  $_ $_ $_)) 
      (remove-all-symbols  &self 
        (popn_size  $_)) 
      (remove-all-symbols  &self 
        (popn_cnt  $_)) 
      (remove-all-symbols  &self 
        (temp  $_)) 
      (garbage-collect) 
      (set-det)))
; 



;
; 



  (= 
    (clean-up) 
    ( (clean-up-1) (clean-up-2)))
; 


;
; 

;
; 

;
; 



  (= 
    (evaluator-reset $_) 
    ( (evaluator-reset-P $_ no) (set-det)))
; 

  (= 
    (evaluator-reset $G) 
    ( (evaluator-reset-P $C $N) 
      (is 0 
        (mod $G $N)) 
      (call $C) 
      (set-det)))
; 

  (= 
    (evaluator_reset  $_) True)
; 


  (= 
    (rename-new-popn) 
    ( (gen-type-P separate) 
      (set-det) 
      (remove-all-symbols  &self 
        (individual  $_ $_ $_)) 
      (renumber-population)))
; 

  (= rename_new_popn True)
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
    (elite-migration $_ $StartSize) 
    ( (gen-type-P separate) 
      (elite-migrate-P $N $ReEval) 
      (> $N 0) 
      (set-det) 
      (setof 
        (, $V $K) 
        (^ $E 
          (individual $K $V $E)) $Set) 
      (first-K 0 $N $Set $Elite) 
      (copy-elite $Elite $ReEval) 
      (is $StartSize 
        (+ $N 1))))
; 

  (= 
    (elite-migration $K $K) 
    (set-det))
; 
 ;
; 



  (= 
    (copy-elite Nil $_) 
    (set-det))
; 

  (= 
    (copy-elite 
      (Cons  
        (, $V $K) $B) $ReEval) 
    ( (individual $K $_ $E) 
      (det-if-then-else 
        (= $ReEval yes) 
        (, 
          (det-if-then-else 
            (eval-with-ID-P yes) 
            (evaluator $K $E $V2) 
            (evaluator $E $V2)) 
          (write ?)) 
        (= $V $V2)) 
      (add-symbol  &self 
        (newindividual  $K $V2 $E)) 
      (set-det) 
      (copy-elite $B $ReEval)))
; 



  (= 
    (evaluator $K $E $V2) 
    (evaluator $E $V2))
; 


