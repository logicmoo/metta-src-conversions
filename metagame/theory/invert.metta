;                                           (convert-to-metta-file  invert $_197250 metagame/theory/invert.pl metagame/theory/invert.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;; invert.pl
;
; ;; How to invert game component defs from perspective of
;
; ;; other player.

;
; invert(Term,Player,InvertedTerm).
;
; If inverting for player, do nothing, otherwise flip.

  (= (invert $Term $Player $Term2)  
    (det-if-then-else 
      (= $Player player) 
      (= $Term $Term2) 
      (invert $Term $Term2)))


  (= (invert opponent player)  
    (set-det))
  (= (invert player opponent)  
    (set-det))
  (= (invert $Term $Term2)  
    (det-if-then-else 
      (atom $Term) 
      (= $Term $Term2) 
      (invert-struct $Term $Term2)))


;
; invert_struct(square(X1,Y1),square(X2,Y2)) :- !,

  (= (invert-struct (square $X1 $Y1) $Sq)  
    (set-det)
    (invert-square 
      (square $X1 $Y1) $Sq))
  (= (invert-struct (dir $X1 $Y1) $Dir)  
    (set-det)
    (invert-dir 
      (dir $X1 $Y1) $Dir))
  (= (invert-struct $Pred $PredOut)  
    (functor $Pred $F $N)
    (functor $PredOut $F $N)
    (invert-args $N $Pred $PredOut))


  (= (invert-args 0 $_ $_)  
    (set-det))
  (= (invert-args $N $Pred $PredOut)  
    (arg $N $Pred $A)
    (invert $A $A1)
    (arg $N $PredOut $A1)
    (is $N1 
      (- $N 1))
    (invert-args $N1 $Pred $PredOut))	



  (= (invert-square $Sq1 $Sq)  
    (current-board-size $XN $YN)
    (current-board-inversion $Inv)
    (invert-square-dim $Inv $XN $YN $Sq1 $Sq))


  (= (invert-square-dim diagonal $XN $YN (square $X1 $Y1) (square $X2 $Y2))  
    (is $X2 
      (+ 
        (- $XN $X1) 1))
    (is $Y2 
      (+ 
        (- $YN $Y1) 1)))
  (= (invert-square-dim forward $XN $YN (square $X1 $Y1) (square $X2 $Y2))  
    (= $X1 $X2)
    (is $Y2 
      (+ 
        (- $YN $Y1) 1)))



  (= (invert-dir (dir $X1 $Y1) (dir $X2 $Y2))  
    (current-board-inversion $Inv)
    (inv-negate-dir $Inv x $X1 $X2)
    (inv-negate-dir $Inv y $Y1 $Y2))


  (= (inv-negate-dir diagonal $Axis $X $XNeg)  
    (negates $X $XNeg))
  (= (inv-negate-dir forward $Axis $X1 $X2)  
    (det-if-then-else 
      (= $Axis y) 
      (negates $X1 $X2) 
      (= $X1 $X2))) 


