;
; 


  !(module td-basic 
    (:: 
      (/ distribute-vars 3) 
      (/ vars-of-type 3) 
      (/ enumerate-t 3) 
      (/ append-body 3)))
; 


;
; 

  !(use-module 
    (home argument-types) 
    (:: (/ type-sub 2)))
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
    (append-body 
      (= $H True) $B 
      (= $H $B)) 
    (set-det))
; 

  (= 
    (append-body 
      (= $H $B) $C 
      (= $H $B1)) 
    ( (set-det) (append-body $B $C $B1)))
; 

  (= 
    (append-body 
      (, $A $B) $C 
      (, $A $D)) 
    ( (set-det) (append-body $B $C $D)))
; 

  (= 
    (append_body  $A $B 
      (,  $A $B)) True)
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
    (distribute_vars  () $_ ()) True)
; 

  (= 
    (distribute-vars 
      (Cons  
        (with_self  $X $Tx) $R) $V 
      (Cons  
        (with_self  $X $Vx) $R1)) 
    ( (distribute-vars $R $V $R1) (vars-of-type $V $Tx $Vx)))
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
    (vars_of_type  () $_ 
      ($_)) True)
; 

  (= 
    (vars-of-type 
      (Cons  
        (with_self  $X $Tx) $R) $Ty $R2) 
    ( (vars-of-type $R $Ty $R1) (det-if-then-else (or (type-sub $Ty $Tx) (type-sub $Tx $Ty)) (= $R2 (Cons  $X $R1)) (= $R2 $R1))))
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
    (enumerate_t  () $PL $PL) True)
; 

  (= 
    (enumerate-t 
      (Cons  
        (with_self  $X $Vx) $R) $PL $PL2) 
    ( (enumerate-t $R $PL $PL1) (et $Vx $X $PL1 $PL2)))
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
    (et  () $_ $_ ()) True)
; 

  (= 
    (et 
      (Cons  $Y $R) $X $PL $PL3) 
    ( (et $R $X $PL $PL1) 
      (etx $PL $X $Y $PL2) 
      (append $PL2 $PL1 $PL3)))
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
    (etx  () $_ $_ ()) True)
; 

  (= 
    (etx 
      (Cons  $P $R) $X $Y $R2) 
    ( (etx $R $X $Y $R1) 
      (functor $P $F $N) 
      (functor $P1 $F $N) 
      (etx1 $N $P $P1 $X $Y) 
      (= $R2 
        (Cons  $P1 $R1))))
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
    (etx1 0 $_ $_ $_ $_) 
    (set-det))
; 

  (= 
    (etx1 $N $P $P1 $X $Y) 
    ( (is $N1 
        (- $N 1)) 
      (etx1 $N1 $P $P1 $X $Y) 
      (arg $N $P $Pn) 
      (det-if-then-else 
        (== $Pn $X) 
        (arg $N $P1 $Y) 
        (arg $N $P1 $Pn))))
; 





