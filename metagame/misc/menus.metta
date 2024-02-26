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



  !(my-ensure-loaded (library aux))
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
    (process-command $String $Suffix $Args) 
    (det-if-then 
      (, 
        (append 
          (Cons  $Command1 $Options) 
          (:: .) $String) 
        (concat - $Suffix $Suff) 
        (concat $Command1 $Suff $Command) 
        (append 
          (Cons  $Command $Args) $Options $G) 
        (=.. $Goal $G) 
        (current-predicate $_ $Goal)) 
      (det-if-then-else 
        (call $Goal) True True)))
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



  (= 
    (menu-command $FormatString $FormatArgs $Suffix $Args) 
    ( (format $FormatString $FormatArgs) (menu-command $Suffix $Args)))
; 


  (= 
    (menu-command $FormatString $Suffix $Args) 
    (menu-command $FormatString Nil $Suffix $Args))
; 


  (= 
    (menu-command $Suffix $Args) 
    ( (read-keyboard-tokens $String) (process-command $String $Suffix $Args)))
; 




