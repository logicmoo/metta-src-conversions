;                                           (convert-to-metta-file  yap_compat $_480996 sldr_dl/yap_compat.pl sldr_dl/yap_compat.metta)
;
; :- statistics( walltime, [X,Y]), XY is X+Y, srandom(X),!.

  (= (random $X)  
    (is $X random))

  !(use-module (library random))
  !(use-module (library lists))


  (= (round $X $Y)  
    (= $Y 
      (round $X)))
; /*flatten([], []). flatten([H|T], M) :- flatten(H, Hf), flatten(T, Tf), append(Hf, Tf, M).*/


