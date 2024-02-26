;                                           (convert-to-metta-file  lgg $_171590 miles/lgg.pl miles/lgg.metta)
;
; MODULE lgg EXPORTS

  !(module lgg 
    (:: 
      (/ subsumes-list 2) 
      (/ gen-msg 3) 
      (/ gen-msg 4) 
      (/ rlgg 3) 
      (/ rlgg 4) 
      (/ covered-clause 2) 
      (/ covered-clauses 4) 
      (/ reduce-complete 1) 
      (/ reduce-complete 2) 
      (/ reduce-approx 2) 
      (/ buildlgg 4) 
      (/ lgg 3) 
      (/ lgg 4) 
      (/ lgg 5) 
      (/ headed-lgg 3) 
      (/ headed-lgg 4) 
      (/ headed-lgg 5) 
      (/ nr-lgg 3) 
      (/ nr-lgg 4) 
      (/ nr-lgg 5) 
      (/ hnr-lgg 3) 
      (/ hnr-lgg 4) 
      (/ hnr-lgg 5) 
      (/ lgg-terms 3) 
      (/ lgg-terms 5) 
      (/ lgg-terms 7) 
      (/ set-lgg 2) 
      (/ gti 3) 
      (/ gti 5) 
      (/ lgti 3) 
      (/ lgti 5) 
      (/ lgti 6)))
; ; Buntine's most specific generalization
; ; " with given bound for saturation
; ; rllg
; ; rllg with given head literal of preference

;
; IMPORTS
  !(use-module 
    (home kb) 
    (:: 
      (/ get-clause 5) 
      (/ delete-clause 1) 
      (/ store-clause 4) 
      (/ get-example 3)))
  !(use-module 
    (home bu-basics) 
    (:: 
      (/ assert-body-randomly 1) 
      (/ clear-mngr 0) 
      (/ subs-build-clause 1) 
      (/ reset-counts 0) 
      (/ assert-clause 1) 
      (/ assert-body 1) 
      (/ body 3) 
      (/ head 3) 
      (/ cover-assert-assumptions 1) 
      (/ assumption 3) 
      (/ msg-build-long-clause 1) 
      (/ msg-build-heads 1) 
      (/ msg-build-body 1)))
  !(use-module 
    (home var-utils) 
    (:: 
      (/ skolemize 3) 
      (/ skolemize 4) 
      (/ deskolemize 3) 
      (/ clean-subst 3)))
  !(use-module 
    (home div-utils) 
    (:: 
      (/ extract-body 2) 
      (/ convert-to-horn-clause 3) 
      (/ effaceall 3) 
      (/ subset-chk 2) 
      (/ maximum 2) 
      (/ identical-member 2) 
      (/ identical-make-unique 2)))
  !(use-module 
    (home interpreter) 
    (:: 
      (/ prove1 2) 
      (/ prove5 2)))
  !(use-module 
    (home g1-ops) 
    (:: 
      (/ saturate 2) 
      (/ saturate 3) 
      (/ inv-derivate1 2)))
  !(use-module 
    (home filter) 
    (:: (/ truncate-unconnected 2)))
  !(use-module 
    (home evaluation) 
    (:: (/ complexity 2)))
  !(use-module-if-exists 
    (library sets) 
    (:: 
      (/ list-to-set 2) 
      (/ subtract 3)))
  !(use-module-if-exists 
    (library lists) 
    (:: 
      (/ is-list 1) 
      (/ subseq0 2) 
      (/ nth1 4)))
  !(use-module-if-exists 
    (library basics) 
    (:: (/ member 2)))
  !(use-module-if-exists 
    (library random) 
    (:: (/ maybe 0)))
  !(use-module-if-exists 
    (library not) 
    (:: (/ once 1)))
  !(use-module-if-exists 
    (library occurs) 
    (:: 
      (/ contains-var 2) 
      (/ free-of-var 2)))
  !(use-module-if-exists 
    (library subsumes) 
    (:: 
      (/ subsumes-chk 2) 
      (/ variant 2)))


;
; METAPREDICATES
;
; none


  !(dynamic (, (/ counter 1) (/ shortsubst 2) (/ iterate 1)))



;
; ***********************************************************************
;
; *
;
; * module: lgg.pl
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
; * description: - clause reduction
;
; * reduce clause with no repect to background
;
; * knowledge (Plotkin,1970)
;
; * reduce w.r.t. background knowledge (Buntine,1988)
;
; * Since reduction is in both cases NP-complete,
;
; * we provide 2 versions for each case:
;
; * a correct, but higly inefficient solution
;
; * & an approximation.
;
; * - clause_subsumption
;
; * - Buntine's most specific generalization under
;
; * generalized subsumption
;
; * - Plotkin's RLGG, with Muggleton's algorithm
;
; * - Plotkins least general generalisation (lgg)
;
; * under theta subsumption
;
; * - least general intersection lgti
;
; *
;
; * see also: Buntine,88; Muggleton,90; Plotkin, 70;
;
; * module lgg.pl for msg
;
; *
;
; ***********************************************************************



;
; ***********************************************************************
;
; *
;
; * predicates: reduce_complete/2
;
; *
;
; * syntax: reduce_complete(+Clause,-ReducedClause)
;
; *
;
; * args: Clause: input clause in list form
;
; * ReducedClause: reduced, minimal clause equivalent to Clause
;
; *
;
; * description: do not consider bg, i.e. reduction wrt theta subsumption
;
; *
;
; * example:
;
; *
;
; * peculiarities:
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************



  (= (reduce-complete $ID)  
    (get-clause $ID $_ $_ $C $_)
    (reduce-complete $C $D)
    (delete-clause $ID)
    (store-clause $_ $D reduce $ID)
    (set-det))


  (= (reduce-complete $Clause $Reduced)  
    (clear-mngr 
      (skolemize $Clause $S $SClause) 
      (assert-clause $SClause) 
      (reduce-complete1 $Clause $S $SReduced0) 
      (list-to-set $SReduced0 $SReduced) 
      (deskolemize $SReduced $S $Reduced) 
      (set-det)))        ;
; No backtracking allowed (solution is unique)


;
; ***********************************************************************
;
; *
;
; * predicate: reduce_complete1/3
;
; *
;
; * syntax: reduce_complete1(+CL,+Subst,-ReducedCL)
;
; *
;
; * args: CL,ReducedCL: clauses in list notation
;
; * Subst: skolem substitution
;
; *
;
; * description: reduces CL by matching it on the skolemized head and
;
; * body literals of the reduced clause in the kb
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


  (= (reduce-complete1 $C $S $RC)  
    (copy-term $C $ToShow)
    (prove1 $ToShow $SProof)
    (det-if-then-else 
      (reduce-complete-test $SProof) 
      (, clear-mngr 
        (list-to-set $SProof $SProof1) 
        (assert-clause $SProof1) 
        (copy-term $S $S1) 
        (deskolemize $SProof1 $S1 $Proof) 
        (reduce-complete1 $Proof $S $RC)) 
      (, reset-counts fail)))
; ; backtrack, find another proof
  (= (reduce-complete1 $_ $S $RC)  
    (set-det)
    (subs-build-clause $RC))
; ; there is no shorter proof, the minimal clause is found.



  (= (reduce-complete-test $P)  
    (findall 
      (with-self  $H p) 
      (head $H $_ $_) $HL)
    (setof 
      (with-self  $B $_) 
      (body $B $_ $_) $BL)
    (append $HL $BL $L)
    (list-to-set $L $L0)
    (list-to-set $P $P0)
    (subtract $L0 $P0 $D)
    (set-det)
    (\== $D Nil))



;
; ***********************************************************************
;
; *
;
; * predicate: reduce_approx/2
;
; *
;
; * syntax: reduce_approx(+Clause,-ReducedClause)
;
; *
;
; * args: Clause: input clause in list form
;
; * ReducedClause: reduced, minimal clause equivalent to Clause
;
; *
;
; * description: as reduce_complete except that the number of single
;
; * reduction steps is bound
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


  (= (reduce-approx $Clause $Reduced)  
    (clear-mngr 
      (copy-term $Clause $Copy) 
      (skolemize $Copy $S $SClause) 
      (assert-clause $SClause) 
      (reduce-approx1 $S 1 50) 
      (reduce-get-current-clause $SReduced0) 
      (list-to-set $SReduced0 $SReduced) 
      (deskolemize $SReduced $S $Reduced)))


  (= (reduce-approx1 $S $Counter $Bound)  
    ( (=< $Counter $Bound) 
      (copy-term $S $S1) 
      (reduce-get-current-clause $C) 
      (deskolemize $C $S1 $ToShow) 
      (prove1 $ToShow $SProof) clear-mngr 
      (assert-body-randomly $SProof) 
      (is $J 
        (+ $Counter 1)) 
      (reduce-approx1 $S $J $Bound)))

	
  (= (reduce-approx1  $_ $_ $_)  True)




;
; ***********************************************************************
;
; *
;
; * predicate: reduce_get_current_clause/1
;
; *
;
; * syntax: reduce_get_current_clause(-CL)
;
; *
;
; * args: CL: clause in list notation
;
; *
;
; * description: returns current skolemized clause that is stored via
;
; * head/3 and body/3 in the kb
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


  (= (reduce-get-current-clause $CL)  
    (findall 
      (with-self  $H p) 
      (head $H $_ $_) $Heads)
    (findall 
      (with-self  $L n) 
      (body $L $_ $_) $Body)
    (append $Heads $Body $CL))


;
; *********************************************************************
;
; *
;
; * predicate: covered_clause/2
;
; *
;
; * syntax: covered_clause(+RULES, +ID)
;
; *
;
; * args: RULES: list of kb references (integers)
;
; * ID: id of clause to be tested
;
; *
;
; * description: Test, if a clause is a specialization of RULES.
;
; *
;
; * example: let RULES refer to clauses
;
; * member(A,[A|B]).
;
; * member(A,[B|C]):- member(A,C).
;
; * then
;
; * member(3,[1,2,3]):- member(3,[2,3])
;
; * is covered by RULES
;
; *
;
; * see also:
;
; *
;
; ********************************************************************


  (= (covered-clause $RULES $ID)  
    (nonvar $RULES)
    (member $ID $RULES))


  (= (covered-clause $RULES $ID)  
    ( (nonvar $RULES) 
      (remove-all-atoms  &self 
        (assumption  $_ $_ $_)) 
      (get-clause $ID $_ $_ $Clause $_) 
      (skolemize $Clause $_ 
        (Cons  
          (with-self  $H p) $Body)) 
      (cover-assert-assumptions $Body) 
      (prove5 $H $RULES) 
      (set-det)))


;
; ****************************************************************
;
; *
;
; * predicate: covered_clauses/4
;
; *
;
; * syntax: covered_clauses(+RULES,+ToTest,-Covered,-Uncovered)
;
; *
;
; * args: RULES: list of kb references (integers)
;
; * ToTest: either label of kb entries (lgg, sat, ex ..)
;
; * or a list of kb references
;
; * description: test if RULES explain all example clauses
;
; * denoted by ToTest.
;
; * Or: RULES |= clauses(ToTest)
;
; *
;
; * example:
;
; *
;
; * peculiarities:
;
; *
;
; * see also:
;
; *
;
; ****************************************************************


  (= (covered-clauses $RULES $LABEL $Covered $Uncovered)  
    (nonvar $RULES)
    (atom $LABEL)
    (findall $ID 
      (get-clause $ID $_ $_ $_ $LABEL) $ToTest)
    (covering $RULES $ToTest Nil $Covered Nil $Uncovered))

  (= (covered-clauses $RULES $ToTest $Covered $Uncovered)  
    (nonvar $RULES)
    (is-list $ToTest)
    (covering $RULES $ToTest Nil $Covered Nil $Uncovered))



  (= (covering $RULES (Cons  $ID $ToTest) $Covered1 $Covered2 $Uncovered1 $Uncovered2)  
    ( (remove-all-atoms  &self 
        (assumption  $_ $_ $_)) 
      (get-clause $ID $_ $_ $Clause $_) 
      (not (or (member $ID $Covered1) (member $ID $Uncovered1))) 
      (skolemize $Clause $_ 
        (Cons  
          (with-self  $H p) $Body)) 
      (cover-assert-assumptions $Body) 
      (set-det) 
      (| 
        (det-if-then-else 
          (member $ID $RULES) 
          (, 
            (= $Covered3 
              (Cons  $ID $Covered1)) 
            (= $Uncovered3 $Uncovered1)) 
          (det-if-then 
            (prove5 $H $RULES) 
            (, 
              (= $Covered3 
                (Cons  $ID $Covered1)) 
              (= $Uncovered3 $Uncovered1)))) 
        (det-if-then otherwise 
          (, 
            (= $Covered3 $Covered1) 
            (= $Uncovered3 
              (Cons  $ID $Uncovered1))))) 
      (covering $RULES $ToTest $Covered3 $Covered2 $Uncovered3 $Uncovered2)))

  (= (covering $_ Nil $Covered $Covered $Uncovered $Uncovered)  
    (set-det))


;
; ***************************************************************
;
; *
;
; * predicates: gen_msg/3/4
;
; *
;
; * syntax: gen_msg(+ID1,+ID2,-ID)
;
; * gen_msg(+ID1,+ID2,-ID,+Bound)
;
; *
;
; * args: ID1,ID2,ID:integers, references to clauses in kb
;
; * Bound :integer, depth bound for saturation
;
; *
;
; * description: Approximation of Buntine's most specific
;
; * generalization. We saturate the 2 input clauses
;
; * & build the lgg over them.
;
; * Our procedure differs from Buntine's in that
;
; * saturation does not construct all generalizing
;
; * clauses under generalized subsumption: if some
;
; * head literal entailed by the body contains
;
; * unbound variables, saturation adds it to the
;
; * body as it is, whereas Buntine instead adds all
;
; * of its ground instances.
;
; *
;
; * example:
;
; *
;
; * peculiarities: the resulting clause is not yet reduced
;
; *
;
; * see also:
;
; *
;
; ***************************************************************


  (= (gen-msg $ID1 $ID2 $ID)  
    (get-clause $ID1 $H1 $_ $_ $_)
    (get-clause $ID2 $H2 $_ $_ $_)
    (functor $H1 $F $N)
    (functor $H2 $F $N)
    (saturate $ID1 $A)
    (saturate $ID2 $B)
    (lgti $A $B $ID)
    (delete-clause $A)
    (delete-clause $B))
; ; lgg(A,B,ID),
; ; changed !

  (= (gen-msg $ID1 $ID2 $ID $Bound)  
    (get-clause $ID1 $H1 $_ $C1 $_)
    (get-clause $ID2 $H2 $_ $C2 $_)
    (functor $H1 $F $N)
    (functor $H2 $F $N)
    (delete-clause $ID2)
    (saturate $ID1 $A $Bound)
    (delete-clause $ID1)
    (get-clause $A $_ $_ $C1sat $_)
    (delete-clause $A)
    (store-clause $_ $C2 genmsg $ID2)
    (saturate $ID2 $B $Bound)
    (store-clause $_ $C1sat sat $A)
    (delete-clause $ID2)
    (nr-lgg $A $B $ID)
    (delete-clause $A)
    (delete-clause $B))
; ; lgti(A,B,ID). ; changed !


;
; ***************************************************************
;
; *
;
; * predicate: rllg/3/4
;
; *
;
; * syntax: rllg(+ID1,+ID2,-ID)
;
; * rllg(+ID1,+ID2,+PrefHead,-ID)
;
; *
;
; * args: ID1,ID2,ID: integers, references to clauses in kb
;
; * PrefHead: MeTTa literal
;
; *
;
; * description: Plotkin's relative leat general generalization.
;
; * Implementation thru Muggleton's alg. :
;
; * Construct 2 inverse linear derivations,
;
; * then build lgg over them.
;
; *
;
; * If PrefHead is given & it is possible to find
;
; * a rllg who's head matches PrefHead, rllg will
;
; * construct this clause.
;
; *
;
; * example:
;
; *
;
; * peculiarities: no reduction yet
;
; *
;
; * see also:
;
; *
;
; ***************************************************************


  (= (rlgg $ID1 $ID2 $ID)  
    (rlgg $ID1 $ID2 $_ $ID))

  (= (rlgg $ID1 $ID2 $PrefHead $ID)  
    ( (or 
        (get-clause $ID1 $_ $_ $Clause1 $_) 
        (, 
          (get-example $ID1 $Ex1 +) 
          (= $Clause1 
            (:: (with-self  $Ex1 p))))) clear-mngr 
      (skolemize $Clause1 $S1 $C1) 
      (assert-clause $C1) 
      (inv-derivate1 $ID1 1) 
      (msg-build-long-clause $D1) 
      (deskolemize $D1 $S1 $A1) 
      (or 
        (get-clause $ID2 $_ $_ $Clause2 $_) 
        (, 
          (get-example $ID2 $Ex2 +) 
          (= $Clause2 
            (:: (with-self  $Ex2 p))))) clear-mngr 
      (skolemize $Clause2 $S2 $C2) 
      (assert-clause $C2) 
      (inv-derivate1 $ID2 1) 
      (msg-build-long-clause $D2) 
      (deskolemize $D2 $S2 $A2) 
      (lgg-gen-clause $A1 $A2 $D $_ Nil Nil $_ $_) 
      (set-det) 
      (convert-to-horn-clause $PrefHead $D $E) 
      (truncate-unconnected $E $F) 
      (store-clause $_ $F rlgg $ID)))
; ; thru backtracking different
; ; heads may be obtained


;
; ***********************************************************************
;
; *
;
; * predicate: subsumes_list/2
;
; *
;
; * syntax: subsumes_list(+General,+Specific)
;
; *
;
; * args: clauses in list notation
;
; *
;
; * description: checks for theta subsumption by list matching.
;
; * No proofs, no substitutions are returned.
;
; * General will not be instantiated.
;
; *
;
; * example:
;
; *
;
; * peculiarities:
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (subsumes-list $Gen $Spec)  
    (copy-term $Gen $Gen1)
    (copy-term $Spec $Spec1)
    (numbervars $Spec1 1 $_)
    (subsumes-list1 $Gen1 $Spec1))


  (= (subsumes-list1  () $_)  True)
  (= (subsumes-list1 (Cons  $L $Rest) $Spec)  
    (member $L $Spec)
    (subsumes-list1 $Rest $Spec))


;
; ********************************************************************************
;
; *
;
; * predicates: lgg_terms/3, lgg_terms/5, lgg_terms/7
;
; *
;
; * syntax: lgg_terms( + Term1, + Term2, - GenTerm )
;
; * lgg_terms( + Term1, + Term2, - GenTerm,
;
; * - Subst_Term1, - Subst_Term2 )
;
; * lgg_terms( + Term1, + Term2, - GenTerm,
;
; * - Subst1, - Subst2,
;
; * + Init_Subst1, + Init_Subst2 )
;
; *
;
; * args: Term1,Term2,GenTerm: MeTTa terms
;
; * Subst_termi: [Var/Fi,..] substitution such that Termi = GenTerm Subst_termi
;
; *
;
; * description: Plotkins least general generalisation wrt theta-subsumption
;
; * on terms
;
; *
;
; * example:
;
; *
;
; * peculiarities:
;
; *
;
; ************************************************************************


  (= (lgg-terms $Term1 $Term2 $GTerm)  
    (lgg-terms $Term1 $Term2 $GTerm $_ $_ Nil Nil))

  (= (lgg-terms $Term1 $Term2 $GTerm $Subst1 $Subst2)  
    (lgg-terms $Term1 $Term2 $GTerm $Subst1 $Subst2 Nil Nil))

  (= (lgg-terms $Term1 $Term2 $GTerm $S1 $S2 $Accu1 $Accu2)  
    (det-if-then-else 
      (, 
        (nonvar $Term1) 
        (nonvar $Term2) 
        (functor $Term1 $F $N) 
        (functor $Term2 $F $N)) 
      (, 
        (functor $GTerm $F $N) 
        (generalize-arguments $Term1 $Term2 $N $GTerm $Accu1 $Accu2 $S1 $S2)) 
      (det-if-then-else 
        (== $Term1 $Term2) 
        (, 
          (= $S1 $Accu1) 
          (= $S2 $Accu2) 
          (= $GTerm $Term1)) 
        (det-if-then-else 
          (substituted $Accu1 $Accu2 $Term1 $Term2 $Variable) 
          (, 
            (= $S1 $Accu1) 
            (= $S2 $Accu2) 
            (= $GTerm $Variable)) 
          (, 
            (= $S1 
              (Cons  
                (/ $NewVar $Term1) $Accu1)) 
            (= $S2 
              (Cons  
                (/ $NewVar $Term2) $Accu2)) 
            (= $GTerm $NewVar))))))
; ; if same functor, same arity
; ; then: build new term by generalizing arguments
; ; else if same instantiation
; ; then: no problem
; ; else if substitution has already been applied
; ; then: again, no problem
; ; else if neither is true
; ; then: substitute with a new variable

;
; generalize argument-terms (loop from position n to position 0)

  (= (generalize-arguments $_ $_ 0 $_ $Ac1 $Ac2 $Ac1 $Ac2)  
    (set-det))
  (= (generalize-arguments $Term1 $Term2 $N $GTerm $Ac1 $Ac2 $S1 $S2)  
    (arg $N $Term1 $ArgN1)
    (arg $N $Term2 $ArgN2)
    (arg $N $GTerm $ArgNG)
    (lgg-terms $ArgN1 $ArgN2 $ArgNG $Ac1new $Ac2new $Ac1 $Ac2)
    (is $K 
      (- $N 1))
    (generalize-arguments $Term1 $Term2 $K $GTerm $Ac1new $Ac2new $S1 $S2))

;
; test whether two terms at the same position have already a common generalised term

  (= (substituted (Cons  (/ $X $T1) $_) (Cons  (/ $X $T2) $_) $Term1 $Term2 $X)  
    (== $T1 $Term1)
    (== $T2 $Term2)
    (set-det))
  (= (substituted (Cons  $_ $Accu1) (Cons  $_ $Accu2) $Term1 $Term2 $X)  
    (substituted $Accu1 $Accu2 $Term1 $Term2 $X))


;
; ************************************************************************
;
; *
;
; * predicate: set_lgg/2
;
; *
;
; * syntax: set_lgg(+List_of_Terms,-GenTerm)
;
; *
;
; * args:
;
; *
;
; * description: lgg of a list of terms
;
; *
;
; * example:
;
; *
;
; * peculiarities:
;
; *
;
; * see also:
;
; *
;
; ************************************************************************


  (= (set-lgg (:: $L) $L)  
    (set-det))
  (= (set-lgg (Cons  $X (Cons  $Y $R)) $Lgg)  
    (lgg-terms $X $Y $Lgg0)
    (set-lgg 
      (Cons  $Lgg0 $R) $Lgg))



;
; ************************************************************************
;
; *
;
; * predicate: headed_lgg/3, headed_lgg/4
;
; *
;
; * syntax: headed_lgg(+ID1,+ID2,-IDG)
;
; * headed_lgg(+ID1,_ID2,-IDG,?Label)
;
; *
;
; * args: ID1,ID2,IDG: clauseIDs, Label: atom
;
; *
;
; * description: returns lgg of clauses ID1 ID2 in IDG, if both clauses
;
; * have a compatible head literal (i.e. same pred, same
;
; * arity). Fails else.
;
; * Default label is hlgg
;
; *
;
; * example:
;
; *
;
; * peculiarities:
;
; *
;
; * see also:
;
; *
;
; ************************************************************************


  (= (headed-lgg $Id1 $Id2 $IdG)  
    (headed-lgg $Id1 $Id2 $IdG hlgg))

  (= (headed-lgg $Id1 $Id2 $IdG $Label)  
    (det-if-then-else 
      (var $Label) 
      (= $Label hlgg) True)
    (hlgg1 $Id1 $Id2 $HG $BGlist)
    (reduce-complete 
      (Cons  
        (with-self  $HG p) $BGlist) 
      (Cons  
        (with-self  $HGr p) $BGlistred))
    (store-clause $_ 
      (Cons  
        (with-self  $HGr p) $BGlistred) $Label $IdG))
; ; reduce_irene([HG:p|BGlist],[HGr:p|BGlistred]),
; ; finally reduce lgg-body ; reduce_approx([HG:p|BGlist],[HGr:p|BGlistred]),; alternatively (more efficient)


  (= (hlgg1 $Id1 $Id2 $HG $BGlist)  
    (get-clause $Id1 $H1 $_ 
      (Cons  $_ $B1list) $_)
    (get-clause $Id2 $H2 $_ 
      (Cons  $_ $B2list) $_)
    (functor $H1 $F $N)
    (functor $H2 $F $N)
    (lgg-terms $H1 $H2 $HG $Subst1 $Subst2)
    (lgg-body $B1list $B2list $BGlist Nil $Subst1 $Subst2 $_ $_))
; ; if name + arity match for both heads
; ; generalize heads first,
; ; then generalize bodies.



;
; ************************************************************************
;
; *
;
; * predicate: hnr_lgg/3, hnr_lgg/4
;
; *
;
; * syntax: hnr_lgg(+ID1,+ID2,-IDG)
;
; * hnr_lgg(+ID1,_ID2,-IDG,?Label)
;
; *
;
; * args: ID1,ID2,IDG: clauseIDs, Label: atom
;
; *
;
; * description: same as headed_lgg, except that the resulting generalised
;
; * clause is NOT reduced. Default label is hnrlgg
;
; *
;
; * example:
;
; *
;
; * peculiarities:
;
; *
;
; * see also:
;
; *
;
; ************************************************************************


  (= (hnr-lgg $Id1 $Id2 $IdG)  
    (hnr-lgg $Id1 $Id2 $IdG hnrlgg))

  (= (hnr-lgg $Id1 $Id2 $IdG $Label)  
    (det-if-then-else 
      (var $Label) 
      (= $Label hnrlgg) True)
    (hlgg1 $Id1 $Id2 $HG $BGlist)
    (store-clause $_ 
      (Cons  
        (with-self  $HG p) $BGlist) $Label $IdG))


;
; ************************************************************************
;
; *
;
; * predicate: lgg/3, lgg/4
;
; *
;
; * syntax: lgg(+ID1,+ID2,-IDG)
;
; * lgg(+ID1,_ID2,-IDG,?Label)
;
; *
;
; * args: ID1,ID2,IDG: clauseIDs, Label: atom
;
; *
;
; * description: returns lgg of clauses ID1 ID2 in IDG. If both clauses
;
; * have no compatible head literal, the head literal of IDG
;
; * is set to 'true'.
;
; * Default label is lgg
;
; *
;
; * example:
;
; *
;
; * peculiarities:
;
; *
;
; * see also:
;
; *
;
; ************************************************************************


  (= (lgg $Id1 $Id2 $IdG)  
    (lgg $Id1 $Id2 $IdG lgg))

  (= (lgg $Id1 $Id2 $IdG $Label)  
    (det-if-then-else 
      (var $Label) 
      (= $Label lgg) True)
    (lgg1 $Id1 $Id2 $HG $BGlist)
    (reduce-complete 
      (Cons  
        (with-self  $HG p) $BGlist) 
      (Cons  
        (with-self  $HGr p) $BGlistred))
    (store-clause $_ 
      (Cons  
        (with-self  $HGr p) $BGlistred) $Label $IdG))
; ; reduce_irene([HG:p|BGlist],[HGr:p|BGlistred]),
; ; finally reduce lgg-body ; reduce_approx([HG:p|BGlist],[HGr:p|BGlistred]), ; alternatively (more efficient)



  (= (lgg1 $Id1 $Id2 $HG $BGlist)  
    (get-clause $Id1 $H1 $_ 
      (Cons  $_ $B1list) $_)
    (get-clause $Id2 $H2 $_ 
      (Cons  $_ $B2list) $_)
    (det-if-then-else 
      (, 
        (functor $H1 $F $N) 
        (functor $H2 $F $N)) 
      (lgg-terms $H1 $H2 $HG $Subst1 $Subst2) 
      (, 
        (= $HG True) 
        (= $Subst1 Nil) 
        (= $Subst2 Nil)))
    (lgg-body $B1list $B2list $BGlist Nil $Subst1 $Subst2 $_ $_))
; ; if name + arity match for both heads
; ; then generalize heads,
; ; else use 'true' as head;
; ; then generalize bodies.



;
; ************************************************************************
;
; *
;
; * predicate: nr_lgg/3, nr_lgg/4
;
; *
;
; * syntax: nr_lgg(+ID1,+ID2,-IDG)
;
; * nr_lgg(+ID1,_ID2,-IDG,?Label)
;
; *
;
; * args: ID1,ID2,IDG: clauseIDs, Label: atom
;
; *
;
; * description: same as lgg, except that the resulting generalised
;
; * clause is NOT reduced. Default label is nrlgg
;
; *
;
; * example:
;
; *
;
; * peculiarities:
;
; *
;
; * see also:
;
; *
;
; ************************************************************************


  (= (nr-lgg $Id1 $Id2 $IdG)  
    (nr-lgg $Id1 $Id2 $IdG nrlgg))

  (= (nr-lgg $Id1 $Id2 $IdG $Label)  
    (det-if-then-else 
      (var $Label) 
      (= $Label nrlgg) True)
    (lgg1 $Id1 $Id2 $HG $BGlist)
    (store-clause $_ 
      (Cons  
        (with-self  $HG p) $BGlist) $Label $IdG))


;
; ***********************************************************************
;
; *
;
; * predicate: buildlgg/4
;
; *
;
; * syntax: build_lgg(+IDs,+IID,-GID,+Label)
;
; *
;
; * args: IDs: list of clauseIDs,
;
; * IID,GID: clauseIDs
;
; * Label: atom
;
; *
;
; * description: returns in GID the ID of the lgg of all clauses in IDs. IID is
;
; * the ID of the intermediate result
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


  (= (buildlgg (:: $C1) $C2 $Clgg $L)  
    (lgg $C1 $C2 $Clgg $L)
    (set-det))

  (= (buildlgg (Cons  $C1 $CRest) $Clgg_old $Clgg_new2 $L)  
    (lgg $C1 $Clgg_old $Clgg_new1 tmp)
    (set-det)
    (det-if-then-else 
      (buildlgg $CRest $Clgg_new1 $Clgg_new2 $L) 
      (delete-clause $Clgg_new1) 
      (, 
        (delete-clause $Clgg_new1) fail)))

  (= (buildlgg Nil $O $N $L)  
    (get-clause $O $_ $_ $Cl $_)
    (store-clause $_ $Cl $L $N)
    (set-det))


;
; ************************************************************************
;
; *
;
; * predicate: lgg/5, nr_lgg/5
;
; *
;
; * syntax: (nr_)lgg(+CL1,+CL2,-CLG,-Subst1,-Subst2)
;
; *
;
; * args: CL1,CL2,CLG: clauses in list notation
;
; * Substi: Substitutions such that CLG Substi \subseteq CLi
;
; *
;
; * description: CLG is (non-reduced) lgg of clauses CL1 and CL2
;
; *
;
; * example:
;
; *
;
; * peculiarities:
;
; *
;
; * see also:
;
; *
;
; ************************************************************************


  (= (lgg $Clause1list $Clause2list (Cons  $HGr $BGlistred) $S1 $S2)  
    (nr-lgg $Clause1list $Clause2list 
      (Cons  $HG $BGlist) $SS1 $SS2)
    (reduce-complete 
      (Cons  $HG $BGlist) 
      (Cons  $HGr $BGlistred))
    (clean-subst 
      (Cons  $HGr $BGlistred) $SS1 $S1)
    (clean-subst 
      (Cons  $HGr $BGlistred) $SS2 $S2))
; ;Theta1, Theta2) :-
; ; reduce_irene([HG|BGlist],[HGr|BGlistred]).
;
; reduce_approx([HG|BGlist],[HGr|BGlistred]), ;;alternatively (more efficient)

	


  (= (nr-lgg (Cons  (with-self  $H1 p) $B1list) (Cons  (with-self  $H2 p) $B2list) (Cons  (with-self  $HG p) $BGlist) $Theta1 $Theta2)  
    (det-if-then-else 
      (, 
        (functor $H1 $F $N) 
        (functor $H2 $F $N)) 
      (lgg-terms $H1 $H2 $HG $Subst1 $Subst2) 
      (, 
        (= $HG True) 
        (= $Subst1 Nil) 
        (= $Subst2 Nil)))
    (lgg-body $B1list $B2list $BGlist Nil $Subst1 $Subst2 $Theta1 $Theta2))


;
; ************************************************************************
;
; *
;
; * predicate: headed_lgg/5, hnr_lgg/5
;
; *
;
; * syntax: headed_lgg(+CL1,+CL2,-CLG,-Subst1,-Subst2)
;
; * hnr_lgg(+CL1,+CL2,-CLG,-Subst1,-Subst2)
;
; *
;
; * args: CL1,CL2,CLG: clauses in list notation
;
; * Substi: Substitutions such that CLG Substi \subseteq CLi
;
; *
;
; * description: CLG is (non-reduced) headed lgg of clauses CL1 and CL2
;
; *
;
; * example:
;
; *
;
; * peculiarities:
;
; *
;
; * see also:
;
; *
;
; ************************************************************************


  (= (headed-lgg $Clause1list $Clause2list (Cons  $HGr $BGlistred) $S1 $S2)  
    (hnr-lgg $Clause1list $Clause2list 
      (Cons  $HG $BGlist) $SS1 $SS2)
    (reduce-complete 
      (Cons  $HG $BGlist) 
      (Cons  $HGr $BGlistred))
    (clean-subst 
      (Cons  $HGr $BGlistred) $SS1 $S1)
    (clean-subst 
      (Cons  $HGr $BGlistred) $SS2 $S2))
; ;;;Theta1, Theta2) :-
; ; reduce_irene([HG|BGlist],[HGr|BGlistred]).
;
; reduce_approx([HG|BGlist],[HGr|BGlistred]), ;;alternatively (more efficient)


  (= (hnr-lgg (Cons  (with-self  $H1 p) $B1list) (Cons  (with-self  $H2 p) $B2list) (Cons  (with-self  $HG p) $BGlist) $Theta1 $Theta2)  
    (functor $H1 $F $N)
    (functor $H2 $F $N)
    (lgg-terms $H1 $H2 $HG $Subst1 $Subst2)
    (lgg-body $B1list $B2list $BGlist Nil $Subst1 $Subst2 $Theta1 $Theta2))


;
; ************************************************************************
;
; *
;
; * predicate: lgg_body/8
;
; *
;
; * syntax: lgg_body(+CL1,+CL2,+CLAccu,-CLAccu,+Subst1,+Subst2,-Subst1,-Subst2)
;
; *
;
; * args: CL1,CL2,CLAccu: clauses bodiesin list representation
;
; * Subst..:Substitutions for the variables in CLAccu
;
; *
;
; * description: CLAccu is the non-reduced lgg of the clause bodies CL1 and
;
; * CL2. The generalisation of two redundant literals L1:r, L2:r is
;
; * marked as redundant LG:r.
;
; *
;
; * example:
;
; *
;
; * peculiarities:
;
; *
;
; * see also:
;
; *
;
; ************************************************************************


  (= (lgg-body Nil $_ $Accu $Accu $S1 $S2 $S1 $S2)  
    (set-det))
  (= (lgg-body $_ Nil $Accu $Accu $S1 $S2 $S1 $S2)  
    (set-det))
        
  (= (lgg-body (Cons  $Elem1 $R1) $List2 $Glist $AcGL $S1old $S2old $Theta1 $Theta2)  
    (generalize-elem $List2 $Elem1 $GLelem1 $S1old $S2old $S1new $S2new)
    (append $AcGL $GLelem1 $AcGLnew)
    (lgg-body $R1 $List2 $Glist $AcGLnew $S1new $S2new $Theta1 $Theta2))


;
; ***************************************************************
;
; *
;
; * predicate: lgg_gen_clause/8
;
; *
;
; * syntax: lgg_gen_clause(+CL1,+CL2,+CLAccu,-CLAccu,+Subst1,
;
; * +Subst2,-Subst1,-Subst2)
;
; *
;
; * args: CL1,CL2,CLAccu: general clauses in list representation
;
; * Subst..:Substitutions for the variables in CLAccu
;
; *
;
; * description: CLAccu is the lgg of CL1 and CL2 (non-reduced)
;
; * CL1, CL2 might be non Horn
;
; *
;
; * example:
;
; *
;
; * peculiarities:
;
; *
;
; * see also:
;
; *
;
; ***************************************************************


  (= (lgg-gen-clause Nil $_ $Accu $Accu $S1 $S2 $S1 $S2)  
    (set-det))
  (= (lgg-gen-clause $_ Nil $Accu $Accu $S1 $S2 $S1 $S2)  
    (set-det))
        
  (= (lgg-gen-clause (Cons  (with-self  $Elem1 $Sign1) $R1) $List2 $Glist $AcGL $S1old $S2old $Theta1 $Theta2)  
    (generalize-elem $List2 
      (with-self  $Elem1 $Sign1) $GLelem1 $S1old $S2old $S1new $S2new)
    (append $AcGL $GLelem1 $AcGLnew)
    (lgg-gen-clause $R1 $List2 $Glist $AcGLnew $S1new $S2new $Theta1 $Theta2))


;
; ***************************************************************
;
; *
;
; * predicate: generalize_elem/7
;
; *
;
; * syntax: generalize_elem(+CL,+Lit:Sign,-GCL,+Subst1,+Subst2,-Subst1,-Subst2)
;
; *
;
; * args: CL,GCL: clauses in list notation
;
; * GCL: clause in list notation
;
; * Lit:Sign: literal and sign (in {p,n,r})
;
; * Subst..: Sustitutions for the variables in GCL
;
; *
;
; * description: for each literal L in CL matching Lit:Sign (i.e. same functor, arity
;
; * and compatible sign), the lgg of L and Lit is added to GCL, and the
;
; * subtitutions are extended accordingly
;
; *
;
; * example:
;
; *
;
; * peculiarities:
;
; *
;
; * see also:
;
; *
;
; ***************************************************************


  (= (generalize-elem Nil $_ Nil $S1 $S2 $S1 $S2)  
    (set-det))

  (= (generalize-elem (Cons  (with-self  $Elem1 $Sign1) $R1) (with-self  $Literal $Sign2) (Cons  (with-self  $GTerm $Sign) $RestGL) $S10 $S20 $S12 $S22)  
    (functor $Literal $F $N)
    (functor $Elem1 $F $N)
    (lgg-terms $Literal $Elem1 $GTerm $S11 $S21 $S10 $S20)
    (det-if-then-else 
      (, 
        (== $Sign1 p) 
        (== $Sign2 p)) 
      (= $Sign p) 
      (det-if-then-else 
        (, 
          (== $Sign1 r) 
          (== $Sign2 r)) 
        (= $Sign r) 
        (det-if-then-else 
          (, 
            (== $Sign1 n) 
            (member $Sign2 
              (:: n r))) 
          (= $Sign n) 
          (det-if-then 
            (, 
              (== $Sign1 r) 
              (== $Sign2 n)) 
            (= $Sign n)))))
    (generalize-elem $R1 
      (with-self  $Literal $Sign2) $RestGL $S11 $S21 $S12 $S22)
    (set-det))

  (= (generalize-elem (Cons  $_ $R1) $Literal $RestGL $S1old $S2old $S1new $S2new)  
    (generalize-elem $R1 $Literal $RestGL $S1old $S2old $S1new $S2new))


;
; ************************************************************************
;
; *
;
; * predicate: gti/3, gti/5
;
; *
;
; * syntax: gti(+C1,+C2,-C), gti(+C1,+C2,-C,-S1,-S2)
;
; *
;
; * args: C1,C2,C: clauses in list notation
;
; * S1,S2: substitutions [ V1/Term1 , .... ]
;
; *
;
; * description: generalization thru intersection
;
; * least general intersection
;
; *
;
; * example:
;
; *
;
; * peculiarities: the resulting clause might be unconnected
;
; * ( see connectedness.pl )
;
; *
;
; * see also:
;
; *
;
; ************************************************************************



  (= (gti $C1 $C2 $C)  
    (gti $C1 $C2 $C $S1 $S2))


  (= (gti $Id1 $Id2 $ID $S1 $S2)  
    (integer $Id1)
    (integer $Id2)
    (set-det)
    (get-clause $Id1 $_ $_ $C1 $_)
    (get-clause $Id2 $_ $_ $C2 $_)
    (gti $C1 $C2 $C $S1 $S2)
    (store-clause $_ $C gti $ID))


  (= (gti (Cons  (with-self  $H1 p) $C1) (Cons  (with-self  $H2 p) $C2) (Cons  (with-self  $H p) $C) $S1 $S2)  
    (lgg-terms $H1 $H2 $H $Phi1 $Phi2)
    (gti $C1 $C2 $C $Phi1 $Phi2 $S1 $S2))




  (= (gti  () $_ () $S1 $S2 $S1 $S2)  True)

  (= (gti (Cons  (with-self  $L1 $Sign1) $Rest1) $C2 (Cons  (with-self  $L $Sign) $Rest) $Phi1 $Phi2 $S1 $S2)  
    (nth1 $N $C2 
      (with-self  $L2 $Sign2) $Rest2)
    (lgg-terms $L1 $L2 $L $Theta1 $Theta2 $Phi1 $Phi2)
    (nonvar $L)
    (| 
      (det-if-then 
        (, 
          (= $Sign1 r) 
          (= $Sign2 r)) 
        (= $Sign r)) 
      (det-if-then otherwise 
        (= $Sign n)))
    (gti $Rest1 $Rest2 $Rest $Theta1 $Theta2 $S1 $S2))
; ; subtract L2 from C2

  (= (gti (Cons  $_ $Rest1) $C2 $Rest $Phi1 $Phi2 $S1 $S2)  
    (gti $Rest1 $C2 $Rest $Phi1 $Phi2 $S1 $S2)) 




;
; ************************************************************************
;
; *
;
; * predicate: lgti/3, lgti/5, lgti/6
;
; *
;
; * syntax: lgti(+C1,+C2,-C,-S1,-S2),lgti(+C1,+C2,-C,-S1,-S2,+Bound)
;
; *
;
; * args: C1,C2,C,S1,S2: see gti above
;
; * Bound : pos. integer
;
; *
;
; * description: apply gti-operator Bound times ( default: Bound = 10 ).
;
; * Return the longest resulting clause & substitutions.
;
; *
;
; * example:
;
; *
;
; * peculiarities:
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (lgti $ID1 $ID2 $ID)  
    (integer $ID1)
    (integer $ID2)
    (set-det)
    (lgti $ID1 $ID2 $C $_ $_ 10)
    (or 
      (store-clause $_ $C lgti $ID) 
      (, 
        (delete-clause $ID) fail)))

  (= (lgti $C1 $C2 $C $S1 $S2)  
    (lgti $C1 $C2 $C $S1 $S2 10))



  (= (lgti $Id1 $Id2 $C $S1 $S2 $Bound)  
    (integer $Id1)
    (integer $Id2)
    (set-det)
    (get-clause $Id1 $_ $_ $C1 $_)
    (get-clause $Id2 $_ $_ $C2 $_)
    (lgti $C1 $C2 $C $S1 $S2 $Bound))


  (= (lgti $C1 $C2 $C $S1 $S2 $Bound)  
    (integer $Bound)
    (> $Bound 0)
    (init-chart $C1 $C2 $Bound)
    (set-det)
    (chart $_ $_ $_ $_ $_ $_)
    (findall $Comp 
      (chart $Comp $_ $_ $_ $_ $_) $Bag)
    (once (maximum $Bag $MaxComp))
    (once (remove-atom  &self (chart  $MaxComp $C $C1 $C2 $S1 $S2))))


  !(dynamic (/ chart 6))



  (= (init-chart $C1 $C2 $Bound)  
    ( (remove-all-atoms  &self 
        (chart-count  $_)) 
      (add-atom  &self 
        (chart-count  1)) 
      (remove-all-atoms  &self 
        (chart  $_ $_ $_ $_ $_ $_)) 
      (set-det) 
      (init-chart1 $C1 $C2 $Bound)))



  (= (init-chart1 $C1 $C2 $Bound)  
    (gti $C1 $C2 $C $S1 $S2)
    (once (, (complexity $C $Comp) (add-atom  &self (chart  $Comp $C $C1 $C2 $S1 $S2)) (remove-atom  &self (chart-count  $I)) (is $J (+ $I 1)) (add-atom  &self (chart-count  $J))))
    (> $J $Bound))
; ; no backtracking thru this

  (= (init-chart1 $_ $_ $_)  
    (set-det))  ;
; if there are less than Bound solutions.



