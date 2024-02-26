;
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


; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 

