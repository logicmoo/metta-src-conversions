
  (= 
    (listdiff  $L () $L) True)
; 

  (= 
    (listdiff $L 
      (Cons  $H $T) $V) 
    ( (remove $H $L $L1) (listdiff $L1 $T $V)))
; 



  (= 
    (remove  $_ () ()) True)
; 

  (= 
    (remove $H 
      (Cons  $H $T) $L) 
    (remove $H $T $L))
; 

  (= 
    (remove $X 
      (Cons  $H $T) 
      (Cons  $H $L)) 
    ( (remove $X $T $L) (\= $X $H)))
; 



  (= 
    (select-two 
      (Cons  $H $T) $H $Y) 
    (member $Y $T))
; 

  (= 
    (select-two 
      (Cons  $H $T) $X $Y) 
    (select-two $T $X $Y))
; 



  (= 
    (forall $Goal $Condition) 
    ( (bagof0 $Condition $Goal $List) (forall1 $List)))
; 



  (= 
    (forall1  ()) True)
; 

  (= 
    (forall1 (Cons  $H $T)) 
    ( (call $H) (forall1 $T)))
; 



  (= 
    (bagof0 $T $G $L) 
    ( (bagof $T $G $L) (set-det)))
; 

  (= 
    (bagof0  $T $G ()) True)
; 



  (= 
    (setof0 $T $G $L) 
    ( (setof $T $G $L) (set-det)))
; 

  (= 
    (setof0  $T $G ()) True)
; 


;                                      (error
;                                        (syntax_error  operator_expected)
;                                        (file  index/utils.pl 37 4 521))



  (= 
    (add-if $X $Ys $Ys) 
    ( (member $X $Ys) (set-det)))
; 

  (= 
    (add_if  $X $Ys 
      (Cons  $X $Ys)) True)
; 



  (= 
    (flatten $Xs $Ys) 
    (flatten-dl $Xs Nil $Ys))
; 



  (= 
    (flatten-dl Nil $Ys $Ys) 
    (set-det))
; 

  (= 
    (flatten-dl 
      (Cons  $X $Xs) $Ys0 $Ys) 
    ( (set-det) 
      (flatten-dl $Xs $Ys0 $Ys1) 
      (flatten-dl $X $Ys1 $Ys)))
; 

  (= 
    (flatten_dl  $X $Xs 
      (Cons  $X $Xs)) True)
; 


