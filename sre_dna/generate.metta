;                                           (convert_to_metta_file  generate $_59478 sre_dna/generate.pl sre_dna/generate.metta)
;
; ------------------------------------------------
;
; Jan 99
;
; Author: Brian Ross
;
; Dept. of Computer Science, Brock University
;
; 
;
; Random generation of CCS expressions.
;
; As in Koza, a ramped half-and-half approach used:
;
; 
;
; If depth = M, then equal spread of trees of approx. depth 2, 3, ..., M.
;
; Tree sizes not precise: list terms (restrict, relabel) can vary as much
;
; as +4 (or more?), because I'm avoiding backtracking for tree depth size.
;
; For each depth category, rougly equal split attempted between full
;
; and grow trees.
;
; Full tries to get full depth on all branches.
;
; Growth trees are irregularly shaped.
;
; The generation cycles between them and depth size. Cycling done to
;
; reduce duplicates that are common with grow tree generation.
;
; 
;
; ramped_population: create a population using ramped approach.
;
; Equal spread of trees using full and growth generation, and of size
;
; 2, 3, ..., MaxDepth.
;
; First, old population in individual/3 retracted.
;
; All expressions asserted into: individual(ID, Val, Expr).


  (= (ramped-population $PopSize)  
    ( (remove-all-atoms  &self 
        (individual  $_ $_ $_)) 
      (max-depth-P $MaxDepth $_) 
      (dctg-root-P $Root) 
      (setof $D 
        (^ $X 
          (^ $Y 
            (^ $Z 
              (with_self  
                (fast *) 
                (dctg-rule-info $Root $X $Y $D $Z))))) $L) 
      (max-list $L $MinDepth) 
      (populate $MinDepth $MaxDepth $MinDepth grow 0 $PopSize) 
      (number-population) 
      (set-det)))

;
; populate(D, MaxDepth, MinDepth, Type, CurrPopn, PopSize) loops until PopSize
;
; individuals created. Depth D goes between 2 and MaxDepth. Type toggles
;
; between grow and full.


  (= (populate $_ $_ $_ $_ $MaxPopn $MaxPopn)  
    (set-det))
  (= (populate $D $MaxD $MinD $Type $Popn $MaxPopn)  
    (> $D $MaxD)
    (set-det)
    (populate $MinD $MaxD $MinD $Type $Popn $MaxPopn))
  (= (populate $D $MaxD $MinD grow $Popn $MaxPopn)  
    (prob-grow-P $Pgrow)
    (maybe $Pgrow)
    (make-individual $D grow $Popn $Popn2)
    (set-det)
    (populate $D $MaxD $MinD full $Popn2 $MaxPopn))
; ; new! May/00: only Pgrow; chance of grow tree
  (= (populate $D $MaxD $MinD $Type $Popn $MaxPopn)  
    (make-individual $D full $Popn $Popn2)
    (is $D2 
      (+ $D 1))
    (set-det)
    (toggle-type $Type $Type2)
    (populate $D2 $MaxD $MinD $Type2 $Popn2 $MaxPopn))
; /* populate(D, MaxD, MinD, full, Popn, MaxPopn) :- make_individual(D, full, Popn, Popn2), D2 is D + 1, !, populate(D2, MaxD, MinD, grow, Popn2, MaxPopn). */
  (= (populate $D $MaxD $MinD $Type $Popn $MaxPopn)  
    (set-det)
    (populate $D $MaxD $MinD $Type $Popn $MaxPopn))
; ; new: June 11/99


  (= (toggle_type  grow full)  True)
  (= (toggle_type  full grow)  True)

;
; make_individual(Depth, Type, Popn, NewPopn)
;
; makes an individual of Type tree of Depth size.
;
; current Popn size updated to NewPopn, but might not change
;
; if expression rejected (not unique?)
;
; Each individual Expr is asserted into:
;
; individual(x, _, Expr, Expr2)
;
; where Expr is main body, Expr2 is adf expression ('0' if unused).
;
; ID and fitness will eventually replace first 2 fields.


  (= (make-individual $Depth $Type $Popn $NewPopn)  
    (dctg-root-P $Root)
    (user-args-P $UserArgs)
    (set-det)
    (generate-tree $Root $Type $Depth $UserArgs $Expr $_)
    (det-if-then-else 
      (, 
        (or 
          (unique-population-P yes) 
          (unique-population-P init)) 
        (individual $_ $_ $Expr)) 
      (= $NewPopn $Popn) 
      (, 
        (det-if-then-else 
          (== $Type full) 
          (writel f) 
          (writel g)) 
        (add-is-symbol  &self 
          (individual  x $_ $Expr)) 
        (is $NewPopn 
          (+ $Popn 1))))
    (set-det))
; ; last arg is list notn.

;
; consecutively numbers all the population with unique ID numbers


  (= (number-population)  
    ( (add-is-symbol  &self 
        (popn_cnt  0)) 
      (remove-is-symbol  &self 
        (individual  x $V $E)) 
      (remove-is-symbol  &self 
        (popn_cnt  $K)) 
      (is $K2 
        (+ $K 1)) 
      (add-is-symbol  &self 
        (popn_cnt  $K2)) 
      (add-is-symbol  &self 
        (individual  $K2 $V $E)) 
      (fail)))
  (= (number-population)  
    ( (remove-is-symbol  &self 
        (popn_cnt  $_)) (set-det)))

;
; consecutively renumbers all the new population with unique ID numbers


  (= (renumber-population)  
    ( (add-is-symbol  &self 
        (popn_cnt  0)) 
      (remove-is-symbol  &self 
        (newindividual  $_ $V $E)) 
      (remove-is-symbol  &self 
        (popn_cnt  $K)) 
      (is $K2 
        (+ $K 1)) 
      (add-is-symbol  &self 
        (popn_cnt  $K2)) 
      (add-is-symbol  &self 
        (individual  $K2 $V $E)) 
      (fail)))
  (= (renumber-population)  
    ( (remove-is-symbol  &self 
        (popn_cnt  $_)) (set-det)))



