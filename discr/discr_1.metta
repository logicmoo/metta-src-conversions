;                                           (convert-to-metta-file  discr_1 $_220030 discr/discr_1.pl discr/discr_1.metta)

  (= ex1  
    (exc1 
      (derivation 
        (<- q 
          (& u v)) app) 
      (derivation 
        (<- q 
          (& w v)) rej) 
      (generate-discriminants q $_ $_)))


  (= exc1  
    ( (abolish :: 2) 
      (add-atom  &self 
        (::  c1 
          (<-  q 
            (&  s r)))) 
      (add-atom  &self 
        (::  c2 
          (<-  s t))) 
      (add-atom  &self 
        (::  c3 
          (<-  s w))) 
      (add-atom  &self 
        (::  c4 
          (<-  t u))) 
      (add-atom  &self 
        (::  c5 
          (<-  r v)))))


  (= (?-  ex1)  True)

