;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (top) 
    ( (queens 8 $Qs) (fail)))
; 

  (= top True)
; 



  (= 
    (queens $N $Qs) 
    ( (range 1 $N $Ns) (queens $Ns Nil $Qs)))
; 


  (= 
    (queens  () $Qs $Qs) True)
; 

  (= 
    (queens $UnplacedQs $SafeQs $Qs) 
    ( (select $UnplacedQs $UnplacedQs1 $Q) 
      (not-attack $SafeQs $Q) 
      (queens $UnplacedQs1 
        (Cons  $Q $SafeQs) $Qs)))
; 



  (= 
    (not-attack $Xs $X) 
    (not-attack $Xs $X 1))
; 


  (= 
    (not-attack Nil $_ $_) 
    (set-det))
; 

  (= 
    (not-attack 
      (Cons  $Y $Ys) $X $N) 
    ( (=\= $X 
        (+ $Y $N)) 
      (=\= $X 
        (- $Y $N)) 
      (is $N1 
        (+ $N 1)) 
      (not-attack $Ys $X $N1)))
; 



  (= 
    (select  
      (Cons  $X $Xs) $Xs $X) True)
; 

  (= 
    (select 
      (Cons  $Y $Ys) 
      (Cons  $Y $Zs) $X) 
    (select $Ys $Zs $X))
; 



  (= 
    (range $N $N 
      (:: $N)) 
    (set-det))
; 

  (= 
    (range $M $N 
      (Cons  $M $Ns)) 
    ( (< $M $N) 
      (is $M1 
        (+ $M 1)) 
      (range $M1 $N $Ns)))
; 




