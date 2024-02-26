;
;                                ------------------------------------------------

;
;                                January 1999

;
;                                Author: Brian Ross

;
;                                Dept. of Computer Science, Brock University

;
; 

;
;                                 Lamarckian evolution: lamarckian_P(P, K, Select, PCross)

;
;                                 Performs Lamarckian evolution on P; of population, iterating

;
;                                 each K times using hill-climbing. Hill-climber uses mutation; it is

;
;                                 recommended that prob_terminal_mutation_P parameter be high, or else

;
;                                 internal mutation will not create good search performance.

;
;                                  Select can be tournament (best, worst) or random.

;
;                                  PCross is prob crossover (prob mutation = 1 - PCross).


;
;                                  lamarckian_evolution also asserts gp_stats with improvement gain obtained

;
;                                  First clause efficiently processes entire population.

;
;                                   Second case is if less than entire population to be used, in which case

;
;                                   selection must be performed.



  (= 
    (lamarckian-evolution $Gen) 
    ( (lamarckian-P $Percent $K $_ $_) 
      (>= $Percent 1.0) 
      (writel (:: nl 'Lamarckian evolution...' nl)) 
      (population-size-P $_ $PopSize) 
      (num-list $PopSize $IDs) 
      (lamarck-loop $IDs 0 $FitImpr 0 $MaxImpr 0 $NumGain $K) 
      (add-atom  &self 
        (gp_stats  $Gen $_ $_ $_ $_ $_ $_ 
          (lamarck  $FitImpr $MaxImpr $NumGain))) 
      (set-det)))
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
      (add-atom  &self 
        (gp_stats  $Gen $_ $_ $_ $_ $_ $_ 
          (lamarck  $FitImpr $MaxImpr $NumGain))) 
      (set-det)))

;
;                                    get_unique_IDs retrieves a list of N unique individual ID's,

;
;                                    selecting each one via Type (random or best/worst tournament selection).



  (= 
    (get-unique-IDs $_ 0 $_ $IDs $IDs) 
    (set-det))
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
;                                     ; in case number is repeated (member below)



;
;                                      lamark_loop(List, ImprSoFar, FitImpr, MaxSoFar, MaxImpr,

;
;                                     		NumSoFar, NumGain, Iter) does best-first Lamarckian evolution.

;
;                                      List = ordered list of individuals+Fitnesses

;
;                                      ImprSoFar, FitImr = Total fitness gain so far / final

;
;                                        MaxSoFar, MaxImpr = best fitness gain so far/final

;
;                                       NumSoFar, NumGain = number that have been changed so far/final

;
;                                       Iter = # iterations to do

;
; 

;
;                                       Note: even if no overall fitness gain achieved, if an altered expression

;
;                                        was found, it is asserted and treated like a gain: will improve genetic

;
;                                        diversity in population due to its syntactic variation.



  (= 
    (lamarck-loop Nil $FitImpr $FitImpr $MaxImpr $MaxImpr $NumGain $NumGain $_) 
    (set-det))
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
          (remove-atom  &self 
            (individual  $ID $_ $_)) 
          (add-atom  &self 
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
;                                         ; writel(['L ID=',ID,'* ']),

;                                         ; don't add



;
;                                         hill_climb(K, BestSoFar, Item) does hill-climbing search for

;
;                                         K iterations.  BestSoFar contains best expression obtained so far with

;
;                                          mutation,  and it and Item have (Fitness, Expression, Adf) structure.

;
;                                          Note: Failed mutation and repeated mutation is counted as an iteration

;
;                                                Also, improved hillclimbing step does not count as an iteration.



  (= 
    (hill-climb $K $Item $Item) 
    ( (=< $K 0) (set-det)))
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
;                                         ; crossover?


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
;                                         ; mutation?

;                                         ;K2 is K - 1,


  (= 
    (hill-climb $K $BestSoFar $Soln) 
    ( (is $K2 
        (- $K 1)) 
      (hill-climb $K2 $BestSoFar $Soln) 
      (set-det)))

;
;                                          select best of expression pairs



  (= 
    (select-best 
      (, $F1 $E1) 
      (, $F2 $_) 
      (, $F1 $E1)) 
    ( (=< $F1 $F2) (set-det)))
  (= 
    (select_best  $_ $X $X) True)



  (= 
    (sre-mutation $I $C) 
    (mutation $I $C))

;
;                                          some debugging code...



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
	

