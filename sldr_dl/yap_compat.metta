;
; 


  (= 
    (random $X) 
    (is $X random))
; 


  !(use-module (library random))
; 

  !(use-module (library lists))
; 



  (= 
    (round $X $Y) 
    (= $Y 
      (round $X)))
; 



