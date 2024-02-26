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
      (format "~nPlayer ~p, as ~p, will select a RANDOM move.~n" 
        (:: $Me $Role)) 
      (random-choose $Role $Move $SIn $SOut) 
      (verbosely-format "~nPlayer ~p, as ~p, has chosen RANDOM move: ~p.~n" 
        (:: $Me $Role $Move))))
; 


