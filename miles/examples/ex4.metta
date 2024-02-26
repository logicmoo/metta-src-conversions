;
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
    (scene $X) 
    ( (part-of $X $Y) 
      (large $X) 
      (red $Y) 
      (part-of $X $Z) 
      (small $Z) 
      (green $Z) 
      (left-of $Y $Z)))
; 

  (= 
    (scene $X) 
    ( (part-of $X $Y) 
      (small $X) 
      (red $Y) 
      (part-of $X $Z) 
      (large $Z) 
      (green $Z) 
      (left-of $Y $Z)))
; 



;
; 


  (= 
    (pet $X) 
    (cat $X))
; 

  (= 
    (pet $X) 
    (dog $X))
; 


  (= 
    (small $X) 
    (cat $X))
; 



  (= 
    (cuddly-pet $X) 
    ( (small $X) 
      (fluffy $X) 
      (dog $X)))
; 

  (= 
    (cuddly-pet $X) 
    ( (fluffy $X) (cat $X)))
; 



  (= 
    (ex  
      (cuddly_pet  cathy_the_cat) +) True)
; 

  (= 
    (ex  
      (cuddly_pet  rosi_the_rabbit) +) True)
; 

  (= 
    (ex  
      (cuddly_pet  tom_the_turtle) -) True)
; 


;
; 


  (= 
    (has_wings  p) True)
; 


  (= 
    (has_beak  p) True)
; 


  (= 
    (has-wings $X) 
    (bird $X))
; 


  (= 
    (has-beak $X) 
    (bird $X))
; 


;
; 

;
; 

;
; 


;
; 

;
; 

;
; 



; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 

