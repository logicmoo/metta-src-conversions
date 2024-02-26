
  !(dynamic (/ rel 2))
; 

  !(dynamic (/ pos-tuple 1))
; 

  !(dynamic (/ neg-tuple 1))
; 



  (= 
    (rel  train 
      (direction hour minutes stop1)) True)
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (pos-tuple (train $Direction $Hour $Minutes $Stop1)) 
    (normaltrain $Direction $Hour $Minutes $Stop1))
; 

  (= 
    (pos-tuple (train $Direction $Hour $Minutes $Stop1)) 
    (specialtrain $Direction $Hour $Minutes $Stop1))
; 



  (= 
    (normaltrain $Direction $Hour $Minutes $Stop1) 
    ( (hour $Hour) (fasttrain $Direction $Minutes $Stop1)))
; 

  (= 
    (normaltrain $Direction $Hour $Minutes $Stop1) 
    ( (hour $Hour) (slowtrain $Direction $Minutes $Stop1)))
; 



  (= 
    (fasttrain $Direction $Minutes $Stop1) 
    ( (fasttrain $Direction $Minutes) (fasttrain-stop1 $Direction $Stop1)))
; 



  (= 
    (slowtrain $Direction $Minutes $Stop1) 
    ( (slowtrain $Direction $Minutes) (slowtrain-stop1 $Direction $Stop1)))
; 

  (= 
    (slowtrain $Direction $Minutes $Stop1) 
    ( (slowtrain $Direction $Minutes1) 
      (is $Minutes 
        (+ $Minutes1 30)) 
      (slowtrain-stop1 $Direction $Stop1)))
; 



  (= 
    (fasttrain  utrecht 8) True)
; 

  (= 
    (fasttrain  tilburg 10) True)
; 

  (= 
    (fasttrain  maastricht 10) True)
; 

  (= 
    (fasttrain  utrecht 25) True)
; 



  (= 
    (fasttrain_stop1  utrecht 
      (-  den bosch)) True)
; 

  (= 
    (fasttrain_stop1  tilburg tilburg) True)
; 

  (= 
    (fasttrain_stop1  maastricht weert) True)
; 



  (= 
    (slowtrain  utrecht 13) True)
; 

  (= 
    (slowtrain  tilburg 17) True)
; 



  (= 
    (slowtrain_stop1  utrecht 
      (-  eindhoven bkln)) True)
; 

  (= 
    (slowtrain_stop1  tilburg 
      (-  eindhoven bkln)) True)
; 



  (= 
    (specialtrain  utrecht 8 31 utrecht) True)
; 



  (= 
    (hour  8) True)
; 

  (= 
    (hour  9) True)
; 

;
; 

;
; 


