;
;                               ------------------------------------------------

;
;                               January 1999

;
;                               Author: Brian Ross

;
;                               Dept. of Computer Science, Brock University

;
; 

;
;                                Fitness evaluation.

;
; 

;
;                                Evaluator/2 is user-supplied fitness function.

;
;                                It is then applied to initial population, which

;
;                                are reasserted with their fitness scores.

;
; 

;
;                                 The problem-specific evaluator should assign individuals a standardized

;
;                                 fitness value (lower score is better, 0 is perfect).

;
;                                 It's syntax must be: evaluator(Expr, Val)




  (= 
    (genesis) 
    ( (population-size-P $InitPopSize $PopSize) 
      (ramped-population $InitPopSize) 
      (nl) 
      (evalInitialPopn) 
      (writel (:: nl '*** Culling population' nl)) 
      (cull-population $InitPopSize $PopSize) 
      (collect-stats (- 0 culled)) 
      (set-det)))

;
;                                  following only used for initial population...



  (= 
    (evalInitialPopn) 
    ( (remove-atom  &self 
        (individual  $ID $Fitness $Expression)) 
      (det-if-then-else 
        (var $Fitness) 
        (det-if-then-else 
          (eval-with-ID-P yes) 
          (evaluator $ID $Expression $Fitness) 
          (evaluator $Expression $Fitness)) True) 
      (add-atom  &self 
        (individual  $ID $Fitness $Expression)) 
      (write ?) 
      (ttyflush) 
      (fail)))
;                                   ; only reevaluate if not scored


  (= 
    (evalInitialPopn) 
    (collect-stats (- 0 genesis)))


  (= 
    (cull-population $PopSize $PopSize) 
    (set-det))
  (= 
    (cull-population $InitPopSize $PopSize) 
    ( (< $InitPopSize $PopSize) 
      (set-det) 
      (writel (:: 'Error: init pop size ' $InitPopSize '< pop size' $PopSize nl)) 
      (fail)))
  (= 
    (cull-population $_ $PopSize) 
    ( (cull-method-P elite) 
      (set-det) 
      (write Culling...) 
      (nl) 
      (setof 
        (, $V $K $E) 
        (individual $K $V $E) $Set) 
      (first-K 0 $PopSize $Set $Set2) 
      (remove-all-atoms  &self 
        (individual  $_ $_ $_)) 
      (assert-elite $Set2)))
  (= 
    (cull-population $CurrPopSize $PopSize) 
    ( (tournament-select worst $CurrPopSize $ID $_) 
      (write x) 
      (ttyflush) 
      (remove-atom  &self 
        (individual  $ID $_ $_)) 
      (det-if-then-else 
        (\== $ID $CurrPopSize) 
        (, 
          (remove-atom  &self 
            (individual  $CurrPopSize $Fit $Expr)) 
          (add-atom  &self 
            (individual  $ID $Fit $Expr))) True) 
      (is $NewPopSize 
        (- $CurrPopSize 1)) 
      (set-det) 
      (cull-population $NewPopSize $PopSize)))

;
;                                      save best in run and best so far (session)



  (= 
    (set-best-in-run $Gen) 
    ( (bagof $V 
        (^ $E 
          (^ $ID 
            (individual $ID $V $E))) $L) 
      (min-list $L $Min) 
      (best-in-run $_ $BestSoFar $_) 
      (< $Min $BestSoFar) 
      (set-det) 
      (individual $_ $Min $Expression) 
      (remove-atom  &self 
        (best_in_run  $_ $_ $_)) 
      (add-atom  &self 
        (best_in_run  $Gen $Min $Expression))))
  (= 
    (set_best_in_run  $_) True)


  (= 
    (set-best-so-far $Run) 
    ( (best-in-run $Gen $Value $Expr) 
      (best-so-far $_ $_ $BV $_) 
      (< $Value $BV) 
      (set-det) 
      (remove-atom  &self 
        (best_so_far  $_ $_ $_ $_)) 
      (add-atom  &self 
        (best_so_far  $Run $Gen $Value $Expr))))
  (= 
    (set_best_so_far  $_) True)

;
;                                        assert_elite asserts individuals into population....



  (= 
    (assert-elite Nil) 
    (set-det))
  (= 
    (assert-elite (Cons  (, $V $K $E) $R)) 
    ( (add-atom  &self 
        (individual  $K $V $E)) 
      (assert-elite $R) 
      (set-det)))

