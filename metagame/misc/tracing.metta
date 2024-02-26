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



  !(dynamic (/ %tracing 2))
; 


;
; 

;
; 

;
; 



  (= 
    (tracing $Module $Call) 
    (det-if-then-else 
      (tracing $Module) 
      (call $Call) True))
; 


;
; 

;
; 

;
; 


  (= 
    (tracing-format $Module $String $Args) 
    (det-if-then-else 
      (tracing $Module) 
      (format $String $Args) True))
; 


;
; 

;
; 


  (= 
    (set-tracing $Module $Status) 
    ( (remove-all-symbols  &self 
        ($tracing  $Module $Status1)) (add-symbol  &self ($tracing  $Module $Status))))
; 


;
; 

;
; 

;
; 

  (= 
    (set-tracing $Mod $Component $Status) 
    ( (functor $Module $Mod 1) 
      (arg 1 $Module $Component) 
      (set-tracing $Module $Status)))
; 



;
; 

;
; 

;
; 


  (= 
    (tracing $M) 
    ( (var $M) 
      (set-det) 
      (format "~nError in tracing: Variable Module~n" Nil)))
; 

  (= 
    (tracing (or $M1 $M2)) 
    (or 
      (, 
        (set-det) 
        (tracing $M1)) 
      (tracing $M2)))
; 

  (= 
    (tracing (, $M1 $M2)) 
    ( (set-det) 
      (tracing $M1) 
      (tracing $M2)))
; 

  (= 
    (tracing $Module) 
    (tracing-module $Module))
; 



  (= 
    (tracing-module $Module) 
    ($tracing $Module on))
; 



  (= 
    (traced-modules $Modules) 
    ( (setof $M 
        (tracing-module $M) $Modules) (set-det)))
; 

  (= 
    (traced_modules  ()) True)
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
    (trace-timing $Module $Call) 
    ( (tracing $Module) 
      (set-det) 
      (runtime $Call)))
; 

  (= 
    (trace-timing $_ $Call) 
    (call $Call))
; 



