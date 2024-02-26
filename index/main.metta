;                                           (convert-to-metta-file  main $_370140 index/main.pl index/main.metta)

  !(unknown $_ fail)
  !(no-style-check all)

  !(compile (library basics))
  !(compile (library lists))
  !(compile (library sets))
  !(compile (library not))
;
; :-compile(library(strings)).

  !(dynamic (/ switched-on 1))
  !(dynamic (/ level-set 2))


  (= (switched-on  cwa)  True)
  (= (switched-on  eval)  True)
  (= (switched-on  debug)  True)


  (= (level-set  acc (+  0.8))  True)
  (= (level-set  conf (+  2))  True)
  (= (level-set  split (/  0.5 0.1))  True)


  !((compile utils) 
    (compile char) 
    (compile eval) 
    (compile hooks) 
    (compile object) 
    (compile sort) 
    (compile decomp))
; /* modules */
  !((compile inter) (compile commands))


  (= run  
    (commands Nil Nil Nil))


  !(run *)

