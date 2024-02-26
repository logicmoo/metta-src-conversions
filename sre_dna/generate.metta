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
    (ramped-population $PopSize) 
    ( (remove-all-symbols  &self 
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


;
; 

;
; 

;
; 



  (= 
    (populate $_ $_ $_ $_ $MaxPopn $MaxPopn) 
    (set-det))
; 

  (= 
    (populate $D $MaxD $MinD $Type $Popn $MaxPopn) 
    ( (> $D $MaxD) 
      (set-det) 
      (populate $MinD $MaxD $MinD $Type $Popn $MaxPopn)))
; 

  (= 
    (populate $D $MaxD $MinD grow $Popn $MaxPopn) 
    ( (prob-grow-P $Pgrow) 
      (maybe $Pgrow) 
      (make-individual $D grow $Popn $Popn2) 
      (set-det) 
      (populate $D $MaxD $MinD full $Popn2 $MaxPopn)))
; 

  (= 
    (populate $D $MaxD $MinD $Type $Popn $MaxPopn) 
    ( (make-individual $D full $Popn $Popn2) 
      (is $D2 
        (+ $D 1)) 
      (set-det) 
      (toggle-type $Type $Type2) 
      (populate $D2 $MaxD $MinD $Type2 $Popn2 $MaxPopn)))
; 

  (= 
    (populate $D $MaxD $MinD $Type $Popn $MaxPopn) 
    ( (set-det) (populate $D $MaxD $MinD $Type $Popn $MaxPopn)))
; 



  (= 
    (toggle_type  grow full) True)
; 

  (= 
    (toggle_type  full grow) True)
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
    (make-individual $Depth $Type $Popn $NewPopn) 
    ( (dctg-root-P $Root) 
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
          (add-symbol  &self 
            (individual  x $_ $Expr)) 
          (is $NewPopn 
            (+ $Popn 1)))) 
      (set-det)))
; 


;
; 



  (= 
    (number-population) 
    ( (add-symbol  &self 
        (popn_cnt  0)) 
      (remove-symbol  &self 
        (individual  x $V $E)) 
      (remove-symbol  &self 
        (popn_cnt  $K)) 
      (is $K2 
        (+ $K 1)) 
      (add-symbol  &self 
        (popn_cnt  $K2)) 
      (add-symbol  &self 
        (individual  $K2 $V $E)) 
      (fail)))
; 

  (= 
    (number-population) 
    ( (remove-symbol  &self 
        (popn_cnt  $_)) (set-det)))
; 


;
; 



  (= 
    (renumber-population) 
    ( (add-symbol  &self 
        (popn_cnt  0)) 
      (remove-symbol  &self 
        (newindividual  $_ $V $E)) 
      (remove-symbol  &self 
        (popn_cnt  $K)) 
      (is $K2 
        (+ $K 1)) 
      (add-symbol  &self 
        (popn_cnt  $K2)) 
      (add-symbol  &self 
        (individual  $K2 $V $E)) 
      (fail)))
; 

  (= 
    (renumber-population) 
    ( (remove-symbol  &self 
        (popn_cnt  $_)) (set-det)))
; 




