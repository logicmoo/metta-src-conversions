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
    (dl Nil) 
    (set-det))
; 

  (= 
    (dl (Cons  $F $Files)) 
    ( (set-det) 
      (dl $F) 
      (dl $Files)))
; 

  (= 
    (dl $Filename) 
    (dynamic-load $Filename))
; 



  (= 
    (dynamic-load $Filename) 
    ( (open $Filename read $Stream) 
      (format "Loading theory file: ~w~n" 
        (:: $Filename)) 
      (dynamic-load-stream $Stream) 
      (format "  Finished loading Theory file: ~w~n" 
        (:: $Filename)) 
      (close $Stream)))
; 



  (= 
    (dynamic-load-stream $Stream) 
    (det-if-then-else 
      (, 
        (read $Stream $Term) 
        (\== $Term end-of-file)) 
      (, 
        (process-term $Term) 
        (dynamic-load-stream $Stream)) True))
; 


;
; 


  (= 
    (process-term $Term) 
    (theory-assert $Term))
; 

	

