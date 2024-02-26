
  (= 
    (classes  
      (yes no)) True)
; 


  (= 
    (attributes  
      (season weather temp work)) True)
; 


  (= 
    (example  1 yes 
      ( (= season spring) 
        (= weather sunny) 
        (= temp warm) 
        (= work few))) True)
; 

  (= 
    (example  2 yes 
      ( (= season summer) 
        (= weather cloudy) 
        (= temp hot) 
        (= work few))) True)
; 

  (= 
    (example  3 no 
      ( (= season summer) 
        (= weather rain) 
        (= temp cold) 
        (= work few))) True)
; 

  (= 
    (example  4 yes 
      ( (= season winter) 
        (= weather cloudy) 
        (= temp cold) 
        (= work plenty))) True)
; 

  (= 
    (example  5 no 
      ( (= season winter) 
        (= weather cloudy) 
        (= temp cold) 
        (= work few))) True)
; 

  (= 
    (example  6 yes 
      ( (= season autumn) 
        (= weather sunny) 
        (= temp hot) 
        (= work plenty))) True)
; 

  (= 
    (example  7 yes 
      ( (= season winter) 
        (= weather sunny) 
        (= temp warm) 
        (= work few))) True)
; 

  (= 
    (example  8 no 
      ( (= season autumn) 
        (= weather cloudy) 
        (= temp warm) 
        (= work few))) True)
; 

  (= 
    (example  9 no 
      ( (= season summer) 
        (= weather cloudy) 
        (= temp hot) 
        (= work plenty))) True)
; 

  (= 
    (example  10 no 
      ( (= season winter) 
        (= weather cloudy) 
        (= temp hot) 
        (= work few))) True)
; 


