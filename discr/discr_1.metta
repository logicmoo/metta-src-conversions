
  (= 
    (ex1) 
    ( (exc1) 
      (derivation 
        (<- q 
          (& u v)) app) 
      (derivation 
        (<- q 
          (& w v)) rej) 
      (generate-discriminants q $_ $_)))
; 



  (= 
    (exc1) 
    ( (abolish :: 2) 
      (add-symbol  &self 
        (::  c1 
          (<-  q 
            (&  s r)))) 
      (add-symbol  &self 
        (::  c2 
          (<-  s t))) 
      (add-symbol  &self 
        (::  c3 
          (<-  s w))) 
      (add-symbol  &self 
        (::  c4 
          (<-  t u))) 
      (add-symbol  &self 
        (::  c5 
          (<-  r v)))))
; 



  (= 
    (?-  ex1) True)
; 


