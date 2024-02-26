
;
; 




  (= 
    (member $X 
      (Cons  $Y $R)) 
    ( (member $X $R) (member $X (:: $X))))
; 

  (= 
    (member  $X 
      (Cons  $X $_)) True)
; 



  (= 
    (app  
      (x a) 
      (b c) 
      (x a b c)) True)
; 

  (= 
    (app  
      (a) 
      (b c) 
      (a b c)) True)
; 

  (= 
    (app  () 
      (b c) 
      (b c)) True)
; 



  (= 
    (ex  
      (app  
        (1 2) 
        (3) 
        (1 2 3)) +) True)
; 

  (= 
    (ex  
      (app  
        (x a) 
        (b c) 
        (x a b c)) +) True)
; 

  (= 
    (ex  
      (app  
        (a) 
        (b c) 
        (a b c)) +) True)
; 

  (= 
    (ex  
      (app  () () ()) +) True)
; 

  (= 
    (ex  
      (app  
        (p) () 
        (p)) +) True)
; 

  (= 
    (ex  
      (app  () 
        (u) 
        (u)) +) True)
; 

  (= 
    (ex  
      (app  () 
        (x y) 
        (x y)) +) True)
; 

  (= 
    (ex  
      (app  
        (r s) () 
        (r s)) +) True)
; 

  (= 
    (ex  
      (app  
        (g) 
        (d) 
        (g d)) +) True)
; 

  (= 
    (ex  
      (app  
        (9 8 7) () 
        (9 8 7)) +) True)
; 

  (= 
    (ex  
      (app  () 
        (6 5 4) 
        (6 5 4)) +) True)
; 

  (= 
    (ex  
      (app  
        (4 3 5) 
        (8) 
        (4 3 5 8)) +) True)
; 

  (= 
    (ex  
      (app  
        (r w) 
        (q t s f i) 
        (r w q t s f i)) +) True)
; 

  (= 
    (ex  
      (app  
        (j k l m) 
        (n o p q r) 
        (j k l m n o p q r)) +) True)
; 

  (= 
    (ex  
      (app  
        (r s t) 
        (q u v) 
        (t s r q u v)) -) True)
; 

  (= 
    (ex  
      (app  
        (s t) 
        (q u v) 
        (s r q u v)) -) True)
; 




  (= 
    (min $A 
      (Cons  $A $B)) 
    ( (min $C $B) (ge $E $F)))
; 


  (= 
    (p $X) 
    ( (q $X $V1) 
      (r $V1 $V2) 
      (q $V3) 
      (s $V3 $V1)))
; 




  (= 
    (column $X) 
    ( (brick $X) 
      (standing $X) 
      (is-on $X $Y) 
      (ground $Y)))
; 


  (= 
    (column $X) 
    ( (brick $X) 
      (standing $X) 
      (is-on $X $Y) 
      (column $Y)))
; 



  (= 
    (same-height $X $Y) 
    ( (ground $X) (ground $Y)))
; 


  (= 
    (same-height $X $Y) 
    ( (brick $X) 
      (standing $X) 
      (brick $Y) 
      (standing $Y) 
      (is-on $X $X1) 
      (is-on $Y $Y1) 
      (same-height $X1 $Y1)))
; 




  (= 
    (arch $X) 
    ( (part-of $A $X) 
      (part-of $B $X) 
      (part-of $C $X) 
      (is-on $A $B) 
      (is-on $A $C) 
      (is-on $B $D) 
      (is-on $C $E) 
      (ground $D) 
      (ground $E) 
      (left-of $B $C) 
      (does-not-touch $B $C) 
      (lying $A) 
      (wedge $A) 
      (standing $B) 
      (standing $C) 
      (brick $B) 
      (brick $C)))
; 





  (= 
    (ex  
      (p  a) +) True)
; 

  (= 
    (ex  
      (p  b) +) True)
; 

  (= 
    (ex  
      (p  c) -) True)
; 



  (= 
    (q  a qa) True)
; 

  (= 
    (q  b qb) True)
; 

  (= 
    (q  c qc) True)
; 


  (= 
    (r  qa x) True)
; 

  (= 
    (r  qb x) True)
; 

  (= 
    (r  qc x) True)
; 


  (= 
    (s  sa qa) True)
; 

  (= 
    (s  sb qb) True)
; 

  (= 
    (s  sc qc) True)
; 


  (= 
    (q  sa) True)
; 

  (= 
    (q  sb) True)
; 





; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 

