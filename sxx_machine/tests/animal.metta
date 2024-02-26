
  (= 
    (animal  goat) True)
; 

  (= 
    (animal  elephant) True)
; 

  (= 
    (animal $X) 
    ( (write 'please type one more animal name: ') (read $X)))
; 


