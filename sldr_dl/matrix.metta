
  !(use-module (library clpfd))
; 


;
; 



  (= 
    (const-add-const $X $Y $Z) 
    ( (is $Z 
        (+ $X $Y)) (set-det)))
; 



  (= 
    (vec-add-vec $X $Y $R) 
    ( (maplist const-add-const $X $Y $R) (set-det)))
; 



  (= 
    (mat-add-mat $X $Y $R) 
    ( (maplist vec-add-vec $X $Y $R) (set-det)))
; 


;
; 



  (= 
    (const-mult-const $X $Y $Z) 
    ( (is $Z 
        (* $X $Y)) (set-det)))
; 



  (= 
    (const-mult-vec $C $V $R) 
    ( (maplist 
        (const-mult-const $C) $V $R) (set-det)))
; 



  (= 
    (vec-mult-const $V $C $R) 
    ( (maplist 
        (const-mult-const $C) $V $R) (set-det)))
; 



  (= 
    (vec-mult-vec $X $Y $R) 
    ( (maplist const-mult-const $X $Y $R) (set-det)))
; 



  (= 
    (mat-mult-const $M $C $R) 
    ( (maplist 
        (const-mult-vec $C) $M $R) (set-det)))
; 



  (= 
    (mat-mult-vec $M $V $R) 
    ( (maplist 
        (vec-mult-vec $V) $M $T) 
      (maplist sumlist $T $R) 
      (set-det)))
; 



  (= 
    (mat-mult-mat $X $Y $R) 
    ( (transpose $Y $T) 
      (maplist 
        (mat-mult-vec $T) $X $R) 
      (set-det)))
; 



  (= 
    (mapmat $F $M $R) 
    (maplist 
      (mapmatsub $F) $M $R))
; 


  (= 
    (mapmatsub $F $V $R) 
    ( (maplist $F $V $R) (set-det)))
; 


