;                                           (convert_to_metta_file  discr_1 $_60876 discr/discr_1.pl discr/discr_1.metta)

  (= (ex1)  
    (exc1)
    (derivation 
      (<- q 
        (& u v)) app)
    (derivation 
      (<- q 
        (& w v)) rej)
    (generate-discriminants q $_ $_))


  (= (exc1)  
    ( (abolish :: 2) 
      (add-is-symbol  &self 
        (::  c1 
          (<-  q 
            (&  s r)))) 
      (add-is-symbol  &self 
        (::  c2 
          (<-  s t))) 
      (add-is-symbol  &self 
        (::  c3 
          (<-  s w))) 
      (add-is-symbol  &self 
        (::  c4 
          (<-  t u))) 
      (add-is-symbol  &self 
        (::  c5 
          (<-  r v)))))


  (= (?-  ex1)  True)

