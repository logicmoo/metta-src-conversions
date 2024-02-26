;                                           (convert-to-metta-file  bu_basics $_294070 miles/bu_basics.pl miles/bu_basics.metta)
;
; MODULE bu_basics EXPORTS

  !(module bu-basics 
    (:: 
      (/ addtolist 1) 
      (/ getlist 1) 
      (/ subs-build-clause 1) 
      (/ msg-build-long-clause 1) 
      (/ msg-build-heads 1) 
      (/ msg-build-body 1) 
      (/ annotate-redundancy 1) 
      (/ abs-process-proofs 2) 
      (/ abs-build-body 1) 
      (/ ident-process-proofs 2) 
      (/ ident-build-body 1) 
      (/ g1-build-clause 2) 
      (/ g1-process-proofs 2) 
      (/ idev-build-clause1 1) 
      (/ idev-build-clause 2) 
      (/ process-new-literals 2) 
      (/ sat-build-clause 3) 
      (/ head 3) 
      (/ body 3) 
      (/ assumption 3) 
      (/ assert-absorptions 2) 
      (/ assert-clause 1) 
      (/ assert-body 1) 
      (/ assert-body-unique 1) 
      (/ assert-body-randomly 1) 
      (/ cover-assert-assumptions 1) 
      (/ retract-body-literals 1) 
      (/ retract-literals 1) 
      (/ assert-literals 1) 
      (/ clear-mngr 0) 
      (/ reset-counts 0)))

;
; IMPORTS
  !(use-module 
    (home div-utils) 
    (:: (/ contains-duplicates 1)))
  !(use-module-if-exists 
    (library basics) 
    (:: (/ member 2)))
  !(use-module-if-exists 
    (library random) 
    (:: (/ maybe 0)))

;
; METAPREDICATES
;
; none

  !(dynamic (/ head 3))     ;
; head( Literal, {old,new}, Counter)
  !(dynamic (/ body 3))     ;
; body( Literal, {old,new}, Counter)
                       ;
; the second argument is used to check whether
                       ;
; saturation/idev resulted in a new literal at all.
                       ;
; the third argument is 0 for new literals that
                       ;
; resulted from sat/idev, and \= 0 for literals that
                       ;
; have been used for sat/idev
  !(dynamic (/ assumption 3))

;
; ***********************************************************************
;
; *
;
; * module: bu_utils.pl
;
; *
;
; * author: B.Jung, M.Mueller, I.Stahl, B.Tausend date:12/92
;
; *
;
; * changed:
;
; *
;
; * description: utilities for bottom-up operators
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************

;
; ***********************************************************************
;
; *
;
; * predicate: process_new_literals/2
;
; *
;
; * syntax: process_new_literals(+[H:Proof|_],-Flag)
;
; *
;
; * args: H = Lit/M where M is in {new_head,new_body}, or H = []
;
; * Proof = [[Lit,N],..,[],...] where N in {head,body}
;
; *
;
; * description: asserts all new heads and bodies of matched clauses
;
; * via head/3 and body/3
;
; * Flag = 1 if at least one existed
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (process-new-literals  () $_)  True)


  (= (process-new-literals (Cons  (with-self  Nil $Proof) $Rest) $Flag)  
    (process-new-literals $Rest $Flag))

  (= (process-new-literals (Cons  (with-self  (/ $L new-body) $Proof) $Rest) $Flag)  
    (body $L $_ $_)
    (set-det)
    (process-new-literals $Rest $Flag))

  (= (process-new-literals (Cons  (with-self  (/ $L new-head) $Proof) $Rest) $Flag)  
    (head $L $_ $_)
    (set-det)
    (process-new-literals $Rest $Flag))

  (= (process-new-literals (Cons  (with-self  (/ $L $_) $Proof) $Rest) $Flag)  
    (contains-duplicates $Proof)
    (set-det)
    (process-new-literals $Rest $Flag))
; ;;eigentlich ein Filter: jedem Literal
; ;;im Parent entspricht eines in der Resolvente

  (= (process-new-literals (Cons  (with-self  (/ $L new-head) $Proof) $Rest) 1)  
    ( (set-det) 
      (add-atom  &self 
        (head  $L new 0)) 
      (annotate $Proof) 
      (process-new-literals $Rest $_)))

  (= (process-new-literals (Cons  (with-self  (/ $L new-body) $Proof) $Rest) 1)  
    ( (add-atom  &self 
        (body  $L new 0)) 
      (annotate $Proof) 
      (process-new-literals $Rest $_)))

;
; for backtracking
  (= (process-new-literals (Cons  (with-self  (/ $L new-body) $Proof) $Rest) $Flag)  
    ( (remove-atom  &self 
        (body  $L new 0)) (process-new-literals $Rest $Flag)))


;
; ***********************************************************************
;
; *
;
; * predicate: annotate/1
;
; *
;
; * syntax: annotate(+Proofs)
;
; *
;
; * args: Proofs = [L1,..,Ln] where Li = [] or Li = [Lit,N] and N in {head,body}
;
; *
;
; * description: increments counter for each head/body literal in Proofs
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (annotate  ())  True)

  (= (annotate (Cons  (:: $L body) $Rest))  
    ( (remove-atom  &self 
        (body  $L $OldNew $I)) 
      (is $J 
        (+ $I 1)) 
      (add-atom  &self 
        (body  $L $OldNew $J)) 
      (annotate $Rest)))
  (= (annotate (Cons  (:: $L head) $Rest))  
    ( (remove-atom  &self 
        (head  $L $OldNew $I)) 
      (is $J 
        (+ $I 1)) 
      (add-atom  &self 
        (head  $L $OldNew $J)) 
      (annotate $Rest)))

  (= (annotate (Cons  Nil $Rest))  
    (annotate $Rest))



;
; ***********************************************************************
;
; *
;
; * predicate: abs_process_proofs/2
;
; *
;
; * syntax: abs_process_proofs(+Proofs,-Head)
;
; *
;
; * args: Proofs = [CL1,..,CLn] where CLi is a clause in list notation
;
; * Head: a head literal
;
; *
;
; * description: returns a head literal from one of the CLi, and retracts
;
; * the according body literals body(L,_,_) of CLi (destructive
;
; * absorption)
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (abs-process-proofs (Cons  (Cons  (with-self  $Head p) $_) $MoreHeads) $NewHead)  
    (body $Head $_ $_)
    (set-det)
    (abs-process-proofs $MoreHeads $NewHead))

  (= (abs-process-proofs (Cons  (Cons  $_ $Proof) $MoreHeads) $NewHead)  
    (contains-duplicates $Proof)
    (set-det)
    (abs-process-proofs $MoreHeads $NewHead))
; ;;eigentlich ein Filter: jedem Literal
; ;;im Parent entspricht eines in der Resolvente

  (= (abs-process-proofs (Cons  (Cons  (with-self  $Head p) $Proof) $MoreHeads) $Head)  
    (retract-body-literals $Proof))

;
; For Backtracking
  (= (abs-process-proofs (Cons  (Cons  (with-self  $Head p) $Proof) $MoreHeads) $NewHead)  
    (assert-body $Proof)
    (abs-process-proofs $MoreHeads $NewHead))



;
; ***********************************************************************
;
; *
;
; * predicate: ident_process_proofs/2
;
; *
;
; * syntax: ident_process_proofs(+[[H:Proof]|_],-Head)
;
; *
;
; * args: H = = Lit/M where M is in {new_head,new_body}, or H = []
;
; * Proof = [[Lit,N],..,[],...] where N in {head,body}
;
; * Head: a literal
;
; *
;
; * description: returns a head literal from one of the H:Proof, and retracts
;
; * the according literals from Proof from the kb
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (ident-process-proofs (Cons  (:: (with-self  (/ $Head new-head) $_)) $MoreHeads) $NewHead)  
    (head $Head $_ $_)
    (set-det)
    (ident-process-proofs $MoreHeads $NewHead))
; ;

  (= (ident-process-proofs (Cons  (:: (with-self  (/ $_ $_) $Proof)) $MoreHeads) $NewHead)  
    (contains-duplicates $Proof)
    (set-det)
    (ident-process-proofs $MoreHeads $NewHead))
; ;;eigentlich ein Filter: jedem Literal
; ;;im Parent entspricht eines in der Resolvente

  (= (ident-process-proofs (Cons  (:: (with-self  (/ $Head new-head) $Proof)) $MoreHeads) $Head)  
    (retract-literals $Proof))

;
; for backtracking
  (= (ident-process-proofs (Cons  (:: (with-self  (/ $Head new-head) $Proof)) $MoreHeads) $NewHead)  
    (assert-literals $Proof)
    (ident-process-proofs $MoreHeads $NewHead))
; ; write('new kb'),nl,subsume_mngr:show_heads,subsume_mngr:show_bodies,nl,



;
; ***********************************************************************
;
; *
;
; * predicate: g1_process_proofs/2
;
; *
;
; * syntax: g1_process_proofs(+[[H:Proof]|_],-Lit)
;
; *
;
; * args: H = = Lit/M where M is in {new_head,new_body}, or H = []
;
; * Proof = [[Lit,N],..,[],...] where N in {head,body}
;
; * Lit: a literal and its sign
;
; *
;
; * description: returns the resolution literal from one of the H:Proof, and retracts
;
; * the according literals from Proof from the kb
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (g1-process-proofs (Cons  (with-self  Nil $_) $R) $Lit)  
    (g1-process-proofs $R $Lit))

  (= (g1-process-proofs (Cons  (with-self  (/ $Head new-head) $_) $MoreHeads) $Lit)  
    (head $Head $_ $_)
    (set-det)
    (g1-process-proofs $MoreHeads $Lit))


  (= (g1-process-proofs (Cons  (with-self  (/ $Body new-body) $_) $MoreHeads) $Lit)  
    (body $Body $_ $_)
    (set-det)
    (g1-process-proofs $MoreHeads $Lit))

  (= (g1-process-proofs (Cons  (with-self  (/ $_ $_) $Proof) $MoreHeads) $Lit)  
    (contains-duplicates $Proof)
    (set-det)
    (g1-process-proofs $MoreHeads $Lit))
; ;;eigentlich ein Filter: jedem Literal
; ;;im Parent entspricht eines in der Resolvente

  (= (g1-process-proofs (Cons  (with-self  (/ $Lit $S0) $Proof) $MoreHeads) (with-self  $Lit $S))  
    (det-if-then-else 
      (= $S0 new-head) 
      (= $S p) 
      (= $S n))
    (retract-literals $Proof))

;
; for backtracking
  (= (g1-process-proofs (Cons  (with-self  (/ $_ $_) $Proof) $MoreHeads) $Lit)  
    (assert-literals $Proof)
    (g1-process-proofs $MoreHeads $Lit))
; ; write('new kb'),nl,subsume_mngr:show_heads,subsume_mngr:show_bodies,nl,


;
; ***********************************************************************
;
; *
;
; * predicate: assert_absorptions/2
;
; *
;
; * syntax: assert_assorptions(+[CL|_],-Flag)
;
; *
;
; * args: CL: clause in list notation
;
; *
;
; * description: asserts heads H of all absorbed clauses, if new, as
;
; * body(H,_,_). Flag=1 if at least one existed
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************



  (= (assert-absorptions  () $F)  True)
  (= (assert-absorptions (Cons  (Cons  (with-self  $Head p) $_) $MoreHeads) $F)  
    (body $Head $OldNew $Count)
    (set-det)
    (assert-absorptions $MoreHeads $F))

  (= (assert-absorptions (Cons  (Cons  (with-self  $Head p) $Proof) $MoreHeads) 1)  
    ( (add-atom  &self 
        (body  $Head new 0)) 
      (annotate-redundancy $Proof) 
      (assert-absorptions $MoreHeads 1)))

;
; For Backtracking
  (= (assert-absorptions (Cons  (Cons  (with-self  $Head p) $Proof) $MoreHeads) $_)  
    (remove-atom  &self 
      (body  $Head new 0)))


;
; ***********************************************************************
;
; *
;
; * predicate: annotate_redundancy/1
;
; *
;
; * syntax: annotate_redundancy(+Proof)
;
; *
;
; * args: Proof: clause in list notation
;
; *
;
; * description: increments counter for each body literal in Proof
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (annotate-redundancy  ())  True)
  (= (annotate-redundancy (Cons  (with-self  $L $_) $More))  
    ( (remove-atom  &self 
        (body  $L $OldNew $I)) 
      (is $J 
        (+ $I 1)) 
      (add-atom  &self 
        (body  $L $OldNew $J)) 
      (annotate-redundancy $More)))


;
; ***********************************************************************
;
; *
;
; * predicate: assert_body_randomly/1
;
; *
;
; * syntax: assert_body_randomly(+Clause_list)
;
; *
;
; * args: +Clause_list ... Clause in list notation
;
; *
;
; * description: assert body literals in random order
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (assert-body-randomly Nil)  
    (set-det))

  (= (assert-body-randomly (Cons  (with-self  $H p) $More))  
    (head $H $_ $_)
    (set-det)
    (assert-body-randomly $More))

  (= (assert-body-randomly (Cons  (with-self  $H p) $More))  
    ( (add-atom  &self 
        (head  $H $_ $_)) (assert-body-randomly $More)))

  (= (assert-body-randomly (Cons  (with-self  $L $_) $More))  
    (body $L $_ $_)
    (set-det)
    (assert-body-randomly $More))

  (= (assert-body-randomly (Cons  (with-self  $L $_) $More))  
    (maybe 
      (add-atom  &self 
        (body  $L old 0)) 
      (assert-body-randomly $More)))

  (= (assert-body-randomly (Cons  (with-self  $L $_) $More))  
    ( (add-atom  &self 
        (body  $L old 0)) (assert-body-randomly $More)))

;
; ***********************************************************************
;
; *
;
; * predicate: addtolist/1
;
; *
;
; * syntax: addtolist(+ToAdd)
;
; *
;
; * args: ToAdd .. Id or list of Id's
;
; *
;
; * description: asserts list of Id's
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (addtolist (Cons  $L $IST))  
    ( (remove-atom  &self 
        (id-list  $List1)) 
      (append $List1 
        (Cons  $L $IST) $List2) 
      (add-atom  &self 
        (id-list  $List2)) 
      (set-det)))
  (= (addtolist $Id)  
    ( (remove-atom  &self 
        (id-list  $List1)) 
      (add-atom  &self 
        (id-list  
          ([|]  $Id $List1))) 
      (set-det)))
  (= (addtolist (Cons  $L $IST))  
    ( (add-atom  &self 
        (id-list  
          ([|]  $L $IST))) (set-det)))
  (= (addtolist $Id)  
    ( (add-atom  &self 
        (id-list  
          ($Id))) (set-det)))


;
; ***********************************************************************
;
; *
;
; * predicate: getlist/1
;
; *
;
; * syntax: getlist(-ID_list)
;
; *
;
; * args:
;
; *
;
; * description: retracts the list of id's that has been asserted by
;
; * addtolist/1
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (getlist $List)  
    (remove-atom  &self 
      (id-list  $List)))


;
; ***********************************************************************
;
; *
;
; * predicate: cover_assert_assumptions
;
; *
;
; * syntax: cover_assert_assumptions(+Clause_list)
;
; *
;
; * args: Clause_list .. clause in list representation
;
; *
;
; * description: asserts for each literal L in Clause_list assumption(L,_,_)
;
; * in the kb
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (cover-assert-assumptions  ())  True)
  (= (cover-assert-assumptions (Cons  (with-self  $L $_) $More))  
    ( (add-atom  &self 
        (assumption  $L $_ $_)) (cover-assert-assumptions $More)))



;
; ***********************************************************************
;
; *
;
; * predicate: clear_mngr/0
;
; *
;
; * syntax:
;
; *
;
; * args:
;
; *
;
; * description: retracts all head/3 and body/3 within the knowledge base
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= clear-mngr  
    ( (remove-all-atoms  &self 
        (head  $_ $_ $_)) (remove-all-atoms  &self (body  $_ $_ $_))))


;
; ***********************************************************************
;
; *
;
; * predicate: retract_body_literals/1
;
; *
;
; * syntax: retract_body_literals(+CL)
;
; *
;
; * args: CL: clause in list notation
;
; *
;
; * description: retracts body(L,_,_) for each L:_ in CL
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************

 
  (= (retract-body-literals (Cons  (with-self  $L $_) $More))  
    ( (remove-atom  &self 
        (body  $L $_ $_)) (retract-body-literals $More)))
  (= (retract-body-literals  ())  True)


;
; ***********************************************************************
;
; *
;
; * predicate: retract_literals/1
;
; *
;
; * syntax: retract_literals(+[[Lit,N]|_])
;
; *
;
; * args: N in {head,body}
;
; *
;
; * description: retracts head(Lit,_,_)/body(Lit,_,_) for each [Lit,head]/
;
; * [Lit,body] in the input
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (retract-literals  ())  True)
  (= (retract-literals (Cons  (:: $L head) $Rest))  
    ( (remove-atom  &self 
        (head  $L $_ $_)) 
      (set-det) 
      (retract-literals $Rest)))
  (= (retract-literals (Cons  (:: $L body) $Rest))  
    ( (remove-atom  &self 
        (body  $L $_ $_)) 
      (set-det) 
      (retract-literals $Rest)))
  (= (retract-literals (Cons  (:: $_ $_) $Rest))  
    (retract-literals $Rest))
  (= (retract-literals (Cons  Nil $Rest))  
    (retract-literals $Rest))


;
; ***********************************************************************
;
; *
;
; * predicate: assert_literals/1
;
; *
;
; * syntax: assert_literals(+[[Lit,N]|_])
;
; *
;
; * args: N in {head,body}
;
; *
;
; * description: asserts each Lit with [Lit,N] in the input as N(Lit,_,_)
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (assert-literals  ())  True)
  (= (assert-literals (Cons  (:: $L head) $Rest))  
    ( (add-atom  &self 
        (head  $L $_ $_)) 
      (set-det) 
      (assert-literals $Rest)))
  (= (assert-literals (Cons  (:: $L body) $Rest))  
    ( (add-atom  &self 
        (body  $L $_ $_)) 
      (set-det) 
      (assert-literals $Rest)))
  (= (assert-literals (Cons  (:: $_ $_) $Rest))  
    (assert-literals $Rest))
  (= (assert-literals (Cons  Nil $Rest))  
    (assert-literals $Rest))


;
; ***********************************************************************
;
; *
;
; * predicate: assert_clause/1
;
; *
;
; * syntax: assert_clause(+CL)
;
; *
;
; * args: CL .. clause in list representation
;
; *
;
; * description: asserts positive literals L in CL as head(L,old,0),
;
; * negative and redundant literals as body(L,old,0).
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (assert-clause $C)  
    (assert-clause1 $C))
  (= (assert-clause $_)  
    (clear-mngr 
      (set-det) fail)) ;
; on backtracking

 
  (= (assert-clause1  ())  True)
  (= (assert-clause1 (Cons  $H $T))  
    (assert-clause1 $H)
    (assert-clause1 $T))
  (= (assert-clause1 (with-self  $H p))  
    (head $H $_ $_))
  (= (assert-clause1 (with-self  $H p))  
    (add-atom  &self 
      (head  $H old 0)))
  (= (assert-clause1 (with-self  $H $S))  
    (member $S 
      (:: n r))
    (body $H $_ $_))
  (= (assert-clause1 (with-self  $H $S))  
    ( (member $S 
        (:: n r)) (add-atom  &self (body  $H old 0))))


;
; ***********************************************************************
;
; *
;
; * predicate: assert_body/1
;
; *
;
; * syntax: assert_body(+CL)
;
; *
;
; * args: CL ... clause body in list representation (only negative and
;
; * redundant literals)
;
; *
;
; * description: asserts each literal L in CL as body(L,old,0)
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************

 
  (= (assert-body  ())  True)
  (= (assert-body (Cons  $H $T))  
    (assert-body $H)
    (assert-body $T))
  (= (assert-body (with-self  $H $S))  
    ( (member $S 
        (:: n r)) (add-atom  &self (body  $H old 0))))


;
; ***********************************************************************
;
; *
;
; * predicate: assert_body_unique/1
;
; *
;
; * syntax: assert_body_unique(+CL)
;
; *
;
; * args: CL ... clause body in list representation (only negative and
;
; * redundant literals)
;
; *
;
; * description: as assert_body/1, but tests whether body(L,_,_) is already
;
; * in the kb
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************

 
  (= (assert-body-unique  ())  True)
  (= (assert-body-unique (Cons  $H $T))  
    (assert-body-unique $H)
    (assert-body-unique $T))
  (= (assert-body-unique (with-self  $H $S))  
    (member $S 
      (:: n r))
    (body $H $_ $_))
  (= (assert-body-unique (with-self  $H $S))  
    ( (member $S 
        (:: n r)) (add-atom  &self (body  $H old 0))))



;
; ***********************************************************************
;
; *
;
; * predicate: reset_counts/0
;
; *
;
; * syntax:
;
; *
;
; * args:
;
; *
;
; * description: for each kb-entry head(Lit,_,Count) and body(Lit,_,Count),
;
; * Count is set to 0
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************

 
  (= reset-counts  
    ( (remove-atom  &self 
        (body  $L $O $_)) 
      (add-atom  &self 
        (body  $L $O 0)) fail))
  (= reset-counts  
    ( (remove-atom  &self 
        (head  $L $O $_)) 
      (add-atom  &self 
        (head  $L $O 0)) fail))
  (= reset_counts  True)

;
; **********************************************************************
;
; *
;
; * predicate: subs_build_clause/1
;
; *
;
; * syntax: subs_build_clause(-CL)
;
; *
;
; * args: CL ... Horn clause in list representation
;
; *
;
; * description: retracts one entry head(H,_,_) and all entries
;
; * body(L,_,_) and builds clause [H:p,..,L:p,...]
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (subs-build-clause (Cons  (with-self  $H p) $Body))  
    ( (remove-atom  &self 
        (head  $H $_ $_)) 
      (subs-build-clause $Body) 
      (set-det)))
  (= (subs-build-clause (Cons  (with-self  $L n) $Body))  
    ( (remove-atom  &self 
        (body  $L $_ $_)) (subs-build-clause $Body)))
  (= (subs-build-clause Nil)  
    (set-det))


;
; ***********************************************************************
;
; *
;
; * predicate: sat_build_clause/3
;
; *
;
; * syntax: sat_build_clause(+H,+B,-CL)
;
; *
;
; * args: H ... head
;
; * B ... list of body literals
;
; * CL ... Horn clause in list representation
;
; *
;
; * description: build clause CL = [H:p,...,L:M,....] for each L in B.
;
; * M is n, if body(L,_,_) is true, else M is r
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (sat-build-clause $H $B (Cons  (with-self  $H p) $B1))  
    (sat-build-body $B $B1))

  (= (sat-build-body  () ())  True)
  (= (sat-build-body (Cons  $L $B) (Cons  (with-self  $L n) $B1))  
    (body $L $_ 0)
    (set-det)
    (sat-build-body $B $B1))
; ; nonredundant literal
  (= (sat-build-body (Cons  $L $B) (Cons  (with-self  $L r) $B1))  
    (sat-build-body $B $B1))
; ; (probably) superfluos literal



;
; ***********************************************************************
;
; *
;
; * predicate: msg_build_long_clause/1
;
; *
;
; * syntax: msg_build_long_clause(-CL)
;
; *
;
; * args: CL ... a general clause in list representation
;
; *
;
; * description:
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************



  (= (msg-build-long-clause $Clause)  
    (msg-build-heads $Heads)
    (msg-build-body $Body)
    (append $Heads $Body $Clause)
    (set-det))


;
; ***********************************************************************
;
; *
;
; * predicate: msg_build_heads/1
;
; *
;
; * syntax: msg_build_heads(-CL)
;
; *
;
; * args: CL ... clause in list representation, consisting only of
;
; * positive literals
;
; *
;
; * description: collects all Literals L such that head(L,_,_) is in kb,
;
; * and returns CL = [...,L:p,....]
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (msg-build-heads (Cons  (with-self  $H p) $More))  
    ( (remove-atom  &self 
        (head  $H $F $I)) 
      (msg-build-heads $More) 
      (add-atom  &self 
        (head  $H $F $I))))
  (= (msg-build-heads Nil)  
    (set-det))


;
; ***********************************************************************
;
; *
;
; * predicate: msg_build_body/1
;
; *
;
; * syntax: msg_build_body(-CL)
;
; *
;
; * args: CL ... clause in list representation, consisting only of
;
; * negative and redundant literals
;
; *
;
; * description: collects all Literals L such that body(L,_,_) is in kb,
;
; * and returns CL = [...,L:M,....], M in {n,r}
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (msg-build-body (Cons  (with-self  $H $Sign) $More))  
    ( (remove-atom  &self 
        (body  $H $F $I)) 
      (det-if-then-else 
        (= $I 0) 
        (= $Sign n) 
        (= $Sign r)) 
      (msg-build-body $More) 
      (set-det) 
      (add-atom  &self 
        (body  $H $F $I))))

  (= (msg-build-body Nil)  
    (set-det))


;
; ***********************************************************************
;
; *
;
; * predicate: idev_build_clause1/1
;
; *
;
; * syntax: idev_build_clause1(-CL)
;
; *
;
; * args: CL .. Horn clause in list representation
;
; *
;
; * description:
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (idev-build-clause1 (Cons  (with-self  $H p) $More))  
    ( (remove-atom  &self 
        (head  $H $F 0)) 
      (set-det) 
      (idev-build-body $More) 
      (add-atom  &self 
        (head  $H $F 0))))
; ; the head is unique !

;
; ***********************************************************************
;
; *
;
; * predicate: idev_build_body/1
;
; *
;
; * syntax: idev_build_body(-CL)
;
; *
;
; * args: CL ... clause in list represenation, only negative and redundant literals
;
; *
;
; * description: collects all L such that body(L,_,_) is in kb
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (idev-build-body (Cons  (with-self  $H $Sign) $More))  
    ( (remove-atom  &self 
        (body  $H $F $I)) 
      (det-if-then-else 
        (= $I 0) 
        (= $Sign n) 
        (= $Sign r)) 
      (idev-build-body $More) 
      (set-det) 
      (add-atom  &self 
        (body  $H $F $I))))

  (= (idev-build-body Nil)  
    (set-det))



;
; ***********************************************************************
;
; *
;
; * predicate: idev_build_clause/2
;
; *
;
; * syntax: idev_build_clause(+H,-CL)
;
; *
;
; * args: CL .. clause in list representation
;
; * H ... preferred head of CL
;
; *
;
; * description: as idev_build_clause1/1, but with preferred head in CL
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (idev-build-clause $PrefHead (Cons  (with-self  $H p) $More))  
    (idev-build-head $PrefHead $H)
    (idev-build-body $More)
    (set-det))

;
; ***********************************************************************
;
; *
;
; * predicate: idev_build_head/2
;
; *
;
; * syntax: idev_build_head(+PrefH,-H)
;
; *
;
; * args: PrefH, H .. clause heads
;
; *
;
; * description: returns H such that head(H,_,_) in kb and PrefH and H
;
; * unifiable. If none exists, the first H with head(H,_,_) in kb
;
; * is returned.
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (idev-build-head $PrefHead $PrefHead)  
    ( (remove-atom  &self 
        (head  $PrefHead $F $N)) 
      (set-det) 
      (add-atom  &self 
        (head  $PrefHead $F $N))))

  (= (idev-build-head $_ $Head)  
    (head $Head $_ $_))
    ;
; retract( head(Head,F,N) ),
    ;
; !,
    ;
; asserta( head(H,F,N) )

 


;
; ***********************************************************************
;
; *
;
; * predicate: ident_build_body/1
;
; *
;
; * syntax: ident_build_body(-CL)
;
; *
;
; * args: CL ... clause in list notation, contains only negative literals
;
; *
;
; * description: CL = [...,L:n,...] for each L such that body(L,_,0) in kb
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (ident-build-body (Cons  (with-self  $L n) $Rest))  
    ( (remove-atom  &self 
        (body  $L $_ 0)) 
      (set-det) 
      (ident-build-body $Rest) 
      (add-atom  &self 
        (body  $L old 0))))    ;
; for backtracking
  (= (ident-build-body Nil)  
    (set-det))



;
; ***********************************************************************
;
; *
;
; * predicate: g1_build_clause/2
;
; *
;
; * syntax: g1_build_clause(+ResLit,-CL)
;
; *
;
; * args: CL ... Horn clause in list notation
;
; * ResLit ... the resolution literal
;
; *
;
; * description: CL is the second parent clause for the g1-operator
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (g1-build-clause (with-self  $L p) (Cons  (with-self  $L p) $Body))  
    (ident-build-body $Body)
    (set-det))
  (= (g1-build-clause (with-self  $L n) (Cons  (with-self  $H p) (Cons  (with-self  $L n) $Body)))  
    ( (ident-build-body $Body) 
      (remove-atom  &self 
        (head  $H $_ 0)) 
      (set-det) 
      (add-atom  &self 
        (head  $L old 0))))   


;
; ***********************************************************************
;
; *
;
; * predicate: abs_build_body/1
;
; *
;
; * syntax: abs_build_body(-CL)
;
; *
;
; * args: CL .. clause in list representation, contains only negative literals
;
; *
;
; * description: CL = [...,L:n,....] for each L such that body(L,_,_) in kb
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************



  (= (abs-build-body (Cons  (with-self  $L n) $Rest))  
    ( (remove-atom  &self 
        (body  $L $F $I)) 
      (set-det) 
      (abs-build-body $Rest) 
      (add-atom  &self 
        (body  $L $F $I))))
  (= (abs-build-body Nil)  
    (set-det))

