
;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    ( (odd $A) 
      (even $B) 
      (even $C) 
      (even $E) 
      (mult 
        (:: $C $B $A) $E 
        (Cons  $I 
          (Cons  $H 
            (Cons  $G 
              (Cons  $F $X))))) 
      (lefteven $F) 
      (odd $G) 
      (even $H) 
      (even $I) 
      (zero $X) 
      (lefteven $D) 
      (mult 
        (:: $C $B $A) $D 
        (Cons  $L 
          (Cons  $K 
            (Cons  $J $Y)))) 
      (lefteven $J) 
      (odd $K) 
      (even $L) 
      (zero $Y) 
      (sum 
        (:: $I $H $G $F) 
        (:: 0 $L $K $J) 
        (Cons  $P 
          (Cons  $O 
            (Cons  $N 
              (Cons  $M $Z))))) 
      (odd $M) 
      (odd $N) 
      (even $O) 
      (even $P) 
      (zero $Z)))
; 

	;
; 

	;
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
    (sum $AL $BL $CL) 
    (sum $AL $BL 0 $CL))
; 


  (= 
    (sum 
      (Cons  $A $AL) 
      (Cons  $B $BL) $Carry 
      (Cons  $C $CL)) 
    ( (set-det) 
      (is $X 
        (+ 
          (+ $A $B) $Carry)) 
      (is $C 
        (mod $X 10)) 
      (is $NewCarry 
        (// $X 10)) 
      (sum $AL $BL $NewCarry $CL)))
; 

  (= 
    (sum Nil $BL 0 $BL) 
    (set-det))
; 

  (= 
    (sum $AL Nil 0 $AL) 
    (set-det))
; 

  (= 
    (sum Nil 
      (Cons  $B $BL) $Carry 
      (Cons  $C $CL)) 
    ( (set-det) 
      (is $X 
        (+ $B $Carry)) 
      (is $NewCarry 
        (// $X 10)) 
      (is $C 
        (mod $X 10)) 
      (sum Nil $BL $NewCarry $CL)))
; 

  (= 
    (sum 
      (Cons  $A $AL) Nil $Carry 
      (Cons  $C $CL)) 
    ( (set-det) 
      (is $X 
        (+ $A $Carry)) 
      (is $NewCarry 
        (// $X 10)) 
      (is $C 
        (mod $X 10)) 
      (sum Nil $AL $NewCarry $CL)))
; 

  (= 
    (sum  () () $Carry 
      ($Carry)) True)
; 


;
; 


  (= 
    (mult $AL $D $BL) 
    (mult $AL $D 0 $BL))
; 


  (= 
    (mult 
      (Cons  $A $AL) $D $Carry 
      (Cons  $B $BL)) 
    ( (is $X 
        (+ 
          (* $A $D) $Carry)) 
      (is $B 
        (mod $X 10)) 
      (is $NewCarry 
        (// $X 10)) 
      (mult $AL $D $NewCarry $BL)))
; 

  (= 
    (mult Nil $_ $Carry 
      (:: $C $Cend)) 
    ( (is $C 
        (mod $Carry 10)) (is $Cend (// $Carry 10))))
; 



  (= 
    (zero  ()) True)
; 

  (= 
    (zero (Cons  0 $L)) 
    (zero $L))
; 



  (= 
    (odd  1) True)
; 

  (= 
    (odd  3) True)
; 

  (= 
    (odd  5) True)
; 

  (= 
    (odd  7) True)
; 

  (= 
    (odd  9) True)
; 



  (= 
    (even  0) True)
; 

  (= 
    (even  2) True)
; 

  (= 
    (even  4) True)
; 

  (= 
    (even  6) True)
; 

  (= 
    (even  8) True)
; 



  (= 
    (lefteven  2) True)
; 

  (= 
    (lefteven  4) True)
; 

  (= 
    (lefteven  6) True)
; 

  (= 
    (lefteven  8) True)
; 


