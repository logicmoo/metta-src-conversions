

  (= 
    (lt $A $B) 
    (< $A $B))
; 


  (= 
    (type_restriction  
      (lt  $A $B) 
      ( (number  $A) (number  $B))) True)
; 




  (= 
    (ex  
      (merge  () () ()) +) True)
; 

  (= 
    (ex  
      (merge  () 
        (2) 
        (2)) +) True)
; 
 
  (= 
    (ex  
      (merge  () 
        (3 4) 
        (3 4)) +) True)
; 
 
  (= 
    (ex  
      (merge  
        (5) () 
        (5)) +) True)
; 
 
  (= 
    (ex  
      (merge  
        (2 3) () 
        (2 3)) +) True)
; 
 



  (= 
    (ex  
      (merge  
        (1) 
        (2) 
        (1 2)) +) True)
; 

  (= 
    (ex  
      (merge  
        (0 1) 
        (3 4) 
        (0 1 3 4)) +) True)
; 

  (= 
    (ex  
      (merge  
        (1) 
        (3 4) 
        (1 3 4)) +) True)
; 


  (= 
    (ex  
      (merge  
        (5) 
        (4) 
        (4 5)) +) True)
; 

  (= 
    (ex  
      (merge  
        (6 7) 
        (3 4) 
        (3 4 6 7)) +) True)
; 

  (= 
    (ex  
      (merge  
        (6 7) 
        (4) 
        (4 6 7)) +) True)
; 



  (= 
    (ex  
      (merge  
        (2 3 4 6) 
        (5 7) 
        (2 3 4 5 6 7)) +) True)
; 
 
  (= 
    (ex  
      (merge  
        (43 55 63) 
        (22 33 44 53) 
        (22 33 43 44 53 55 63)) +) True)
; 
 
  (= 
    (ex  
      (merge  
        (29 39 49 59) 
        (37 79 99) 
        (29 37 39 49 59 79 99)) +) True)
; 

  (= 
    (ex  
      (merge  
        (2) 
        (4 7) 
        (2 4 7)) +) True)
; 




  (= 
    (ex  
      (merge  () () 
        (3)) -) True)
; 

  (= 
    (ex  
      (merge  () 
        (2) 
        (1 2)) -) True)
; 
 
  (= 
    (ex  
      (merge  () 
        (3 4) 
        (4 3)) -) True)
; 
 
  (= 
    (ex  
      (merge  
        (5 6) () 
        (5)) -) True)
; 
 
  (= 
    (ex  
      (merge  
        (2 3) () 
        (1 2 3)) -) True)
; 
 



  (= 
    (ex  
      (merge  
        (1) 
        (2) 
        (2 1)) -) True)
; 

  (= 
    (ex  
      (merge  
        (0 1) 
        (3 4) 
        (0 1 4)) -) True)
; 

  (= 
    (ex  
      (merge  
        (1) 
        (3 4) 
        (4 1 1 3)) -) True)
; 


  (= 
    (ex  
      (merge  
        (5) 
        (1 4) 
        (4 1 5)) -) True)
; 

  (= 
    (ex  
      (merge  
        (6 7) 
        (3 4) 
        (3 6 4 7)) -) True)
; 

  (= 
    (ex  
      (merge  
        (6 7) 
        (4) 
        (6 7 4)) -) True)
; 


; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 

