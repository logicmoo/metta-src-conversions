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
    (forall $G1 $G) 
    (not (not (, (call $G1) (not (not (call $G2)))))))
; 



  (= 
    (undo $G) $G)
; 



  (= 
    (redo-each-call $EachSetup $Call $EachCleanup) 
    (setup-call-cleanup $EachSetup $Call $EachCleanup))
; 

  

  (= 
    (or 
      (if $IF $THEN) $ELSE) 
    ( (= $AtLeastOnce 
        (dotwo True)) (or (, (call $IF) (nb-setarg 1 $AtLeastOnce fail)) (or (= $AtLeastOnce (dotwo True)) $Else))))
; 



  (= 
    (one-of-or-else $If $Then $Else) 
    (or 
      (if $IF $THEN) $ELSE))
; 




;
; 


