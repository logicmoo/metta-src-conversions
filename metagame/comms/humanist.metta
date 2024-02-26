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
      (format "~nPlayer ~p, as ~p, wants your help to select his move.~n" 
        (:: $Me $Role)) 
      (human-choose $Role $Move $SIn $SOut)))
; 


