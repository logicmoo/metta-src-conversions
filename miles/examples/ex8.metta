;                                           (convert-to-metta-file  ex8 $_169394 miles/examples/ex8.pl miles/examples/ex8.metta)


  (= (ex  (reverse  () ()) +)  True)
  (= (ex  (reverse  (b) (b)) +)  True)
  (= (ex  (reverse  (d e) (e d)) +)  True)
  (= (ex  (reverse  (e) (e)) +)  True)

  (= (ex  (reverse  (1) (1)) +)  True)
  (= (ex  (reverse  (1 b) (b 1)) +)  True)
  (= (ex  (reverse  (a b) (b a)) +)  True)
  (= (ex  (reverse  (c d e) (e d c)) +)  True)
  (= (ex  (reverse  (c d) (d c)) +)  True)
  (= (ex  (reverse  (d) (d)) +)  True)

  (= (ex  (reverse  (1 b) (1)) -)  True)
  (= (ex  (reverse  (a b) (b 1)) -)  True)
  (= (ex  (reverse  (a b) (a b)) -)  True)
  (= (ex  (reverse  (c d e) (d c e)) -)  True)

;
; ;reverse([],[]).
;
; ;reverse([X|Y],Z):- reverse(Y,Z1).

; 
; 
; try:
; 
; | ?- clear_kb, do_full_kb('examples/ex8.pl').
; 
; :- init_kb('examples/ex8.pl').
; ; file "/tmp_mnt/home/stahl/edl/framework/miles/examples/ex8.pl" consulted.
; :- argument_types.
; ; yes
; :- clause_heads.
; ; yes
; :- complete_chk.
; ; yes
; :- correct_chk.
; ; no
; :- fp(Xmout1).
; ; yes
; ; resulting rules selected
; :- refinement_add_body_literal(18,Xmout1).
; ; yes
; :- eval_examples.
; ; yes
; :- complete_chk.
; ; yes
; :- correct_chk.
; ; no
; :- fp(Xmout1).
; ; yes
; ; resulting rules selected
; :- specialize_with_newpred(18,Xmout1).
; ; yes
; :- store_clause(newp5([e,d],[d,c],e,c),_,user,32).
; ; rule added.
; :- store_clause(newp5([d],[c],d,c),_,user,33).
; ; rule added.
; :- flatten_kb.
; ; yes
; :- absorb(32,33,Xmout1).
; ; yes
; ; rule 53 created.
; :- unflatten_kb.
; ; yes
; :- delete_clause(53).
; :- store_clause(newp5([E|A],[B|C],E,D) :-
; newp5(A,C,B,D),_,user,53).
; ; rule changed.
; :- clause_heads.
; ; yes
; :- delete_clause(32).
; :- delete_clause(33).
; :- delete_clause(55).
; :- delete_clause(56).
; :- delete_clause(57).
; ; selected rules deleted.
; :- eval_examples.
; ; yes
; :- complete_chk.
; ; yes
; :- correct_chk.
; ; yes
