
  (= 
    (ex2) 
    ( (exc2) 
      (derivation 
        (<- 
          (term (:: t1 t2)) 
          (& 
            (const t1) 
            (const t2))) app) 
      (derivation 
        (<- 
          (term (:: t1 t2)) 
          (& 
            (termv t1) 
            (termc t2))) rej) 
      (generate-discriminants 
        (term (:: t1 t2)) $_ $_)))
; 



  (= 
    (exc2) 
    ( (abolish :: 2) 
      (add-symbol  &self 
        (::  c1 
          (<-  
            (term  $X) 
            (&  
              (:=  
                (::  $X1 $X2) $X) 
              (&  
                (term  $X1) 
                (term  $X2)))))) 
      (add-symbol  &self 
        (::  c2 
          (<-  
            (term  $X) 
            (termc  $X)))) 
      (add-symbol  &self 
        (::  c3 
          (<-  
            (term  $X) 
            (termv  $X)))) 
      (add-symbol  &self 
        (::  c4 
          (<-  
            (termc  $X) 
            (&  
              (:=  
                (::  $X1 $X2) $X) 
              (&  
                (termc  $X1) 
                (termc  $X2)))))) 
      (add-symbol  &self 
        (::  c5 
          (<-  
            (termc  $X) 
            (const  $X)))) 
      (add-symbol  &self 
        (::  c6 
          (<-  
            (termv  $X) 
            (&  
              (:=  
                (::  $X1 $X2) $X) 
              (termv  $X1))))) 
      (add-symbol  &self 
        (::  c7 
          (<-  
            (termv  $X) 
            (&  
              (:=  
                (::  $X1 $X2) $X) 
              (termv  $X2))))) 
      (add-symbol  &self 
        (::  c8 
          (<-  
            (termv  $X) 
            (var  $X)))) 
      (add-symbol  &self 
        (:-  
          (::  c9 
            (<-  
              (:=  $X1 $X2) true)) 
          (:=  $X1 $X2)))))
; 



  (= 
    (?-  ex2) True)
; 


