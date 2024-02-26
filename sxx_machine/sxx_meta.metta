;                                           (convert-to-metta-file  sxx_meta $_397876 sxx_machine/sxx_meta.pl sxx_machine/sxx_meta.metta)
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Builtin Predicates of MeTTa Cafe
;
; 
;
; Mutsunori Banbara (banbara@kobe-u.ac.jp)
;
; Naoyuki Tamura (tamura@kobe-u.ac.jp)
;
; Kobe University
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; :- op(1150, fx, (package)).
;
; package(X):- nb_setval(package,X).

;
; :-package('TauMachine.builtin_tau').

;
; :- public system_predicate/1.
;
; :- multifile(system_predicate/1).
;
; :- dynamic(system_predicate/1).



  (= (forall $G1 $G)  
    (not (not (, (call $G1) (not (not (call $G2)))))))
; /* system_predicate(nb_setarg(_,_,_)). system_predicate(setarg(_,_,_)). system_predicate(nb_get_attr(_,_,_)). system_predicate(nb_put_attr(_,_,_)). system_predicate(nb_get_attrs(_,_)). system_predicate(nb_put_attrs(_,_)). */
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ; Control constructs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ; :- public undo/1.
; /* '$builtin_meta_predicates'(undo, 1, [:]). '$builtin_meta_predicates'(undo1, 1, [:]). '$builtin_meta_predicates'(redo_each_call, 3, [:,:,:]). '$builtin_meta_predicates'(one_of_or_else, 3, [:,:,:]). */
; ;(IF -> THEN; _ELSE) :- call(IF), !, call(THEN). ;(_IF -> _THEN; ELSE) :- call(ELSE).


  (= (undo $G)  $G)


  (= (redo-each-call $EachSetup $Call $EachCleanup)  
    (setup-call-cleanup $EachSetup $Call $EachCleanup))
  

  (= (or (if $IF $THEN) $ELSE)  
    (= $AtLeastOnce 
      (dotwo True))
    (or 
      (, 
        (call $IF) 
        (nb-setarg 1 $AtLeastOnce fail)) 
      (or 
        (= $AtLeastOnce 
          (dotwo True)) $Else)))


  (= (one-of-or-else $If $Then $Else)  
    (or 
      (if $IF $THEN) $ELSE))



;
; END

