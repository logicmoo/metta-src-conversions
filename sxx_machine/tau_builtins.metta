;                                           (convert-to-metta-file  tau_builtins $_194264 sxx_machine/tau_builtins.pl sxx_machine/tau_builtins.metta)
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

  !(op 1150 fx package)

  (= (package $X)  
    (nb-setval package $X))


  !(package TauMachine.builtin-tau)

;
; :- public system_predicate/1.
;
; :- multifile(system_predicate/1).
;
; :- dynamic(system_predicate/1).


  !(public (/ undo 1))
; /* system_predicate(nb_setarg(_,_,_)). system_predicate(setarg(_,_,_)). system_predicate(nb_get_attr(_,_,_)). system_predicate(nb_put_attr(_,_,_)). system_predicate(nb_get_attrs(_,_)). system_predicate(nb_put_attrs(_,_)). */
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ; Control constructs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (= (undo $G)  
    (undo (call $G)))
; /* '$builtin_meta_predicates'(undo, 1, [:]). '$builtin_meta_predicates'(undo1, 1, [:]). '$builtin_meta_predicates'(redo_each_call, 3, [:,:,:]). '$builtin_meta_predicates'(one_of_or_else, 3, [:,:,:]). */

  (= (undo1 $Term)  
    (%get-current-B $Cut)
    (%meta-call $Term user $Cut 0 interpret))	


  (= (redo-each-call $EachSetup $Call $EachCleanup)  
    (redo-each-call $EachSetup $Call $EachCleanup))
  


  (= (one-of-or-else $If $Then $Else)  
    (one-of-or-else $If $Then $Else))



;
; END

