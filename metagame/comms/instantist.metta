;
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
    (move $Role $Move $SIn $SOut) 
    ( (my-name $Me) 
      (format "~nPlayer ~p, as ~p, will select an INSTANT move.~n" 
        (:: $Me $Role)) 
      (instant-choose $Role $Move $SIn $SOut)))
; 


