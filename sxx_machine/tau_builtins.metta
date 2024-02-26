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


  !(op 1150 fx package)
; 


  (= 
    (package $X) 
    (nb-setval package $X))
; 



  !(package TauMachine.builtin-tau)
; 


;
; 

;
; 

;
; 



  !(public (/ undo 1))
; 



  (= 
    (undo $G) 
    (undo (call $G)))
; 


  (= 
    (undo1 $Term) 
    ( ($get-current-B $Cut) ($meta-call $Term user $Cut 0 interpret)))
; 
	


  (= 
    (redo-each-call $EachSetup $Call $EachCleanup) 
    (redo-each-call $EachSetup $Call $EachCleanup))
; 

  


  (= 
    (one-of-or-else $If $Then $Else) 
    (one-of-or-else $If $Then $Else))
; 




;
; 


