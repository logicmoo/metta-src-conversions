;
; 



  (= 
    (s 
      (:: hans trifft susi) Nil) 
    ( (pn 
        (:: hans trifft susi) 
        (:: trifft susi)) 
      (v-t 
        (:: trifft susi) 
        (:: susi)) 
      (pn 
        (:: susi) Nil)))
; 


  (= 
    (s 
      (:: martha schlaeft) Nil) 
    ( (pn 
        (:: martha schlaeft) 
        (:: schlaeft)) (v-i (:: schlaeft) Nil)))
; 




  (= 
    (vp $A $B) 
    ( (v-t $A $C) (np $C $B)))
; 

  (= 
    (vp 
      (:: sieht den mann) Nil) 
    ( (v-t 
        (:: sieht den mann) 
        (:: den mann)) 
      (det 
        (:: den mann) 
        (:: mann)) 
      (n 
        (:: mann) Nil)))
; 

  (= 
    (vp 
      (:: hilft karl) Nil) 
    ( (v-t 
        (:: hilft karl) 
        (:: karl)) (pn (:: karl) Nil)))
; 



  (= 
    (min1 $D 
      (Cons  
        (s $D) $E)) 
    (min1 $D $E))
; 

  (= 
    (min1 $F 
      (Cons  
        (s (s $F)) $G)) 
    (min1 $F $G))
; 


; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 

