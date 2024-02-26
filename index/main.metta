;                                           (convert_to_metta_file  main $_154800 index/main.pl index/main.metta)

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


  (= (switched_on  cwa)  True)
  (= (switched_on  eval)  True)
  (= (switched_on  debug)  True)


  (= (level_set  acc (+  0.8))  True)
  (= (level_set  conf (+  2))  True)
  (= (level_set  split (/  0.5 0.1))  True)


  !((compile utils) 
    (compile char) 
    (compile eval) 
    (compile hooks) 
    (compile object) 
    (compile sort) 
    (compile decomp))
; /* modules */
  !((compile inter) (compile commands))


  (= (run)  
    (commands Nil Nil Nil))


  !(run *)

