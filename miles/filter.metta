;                                           (convert_to_metta_file  filter $_456074 miles/filter.pl miles/filter.metta)
;
; MODULE filter EXPORTS

  !(module filter 
    (:: 
      (/ is-flat 1) 
      (/ is-unflat 1) 
      (/ connected-vars 4) 
      (/ truncate-unconnected 2) 
      (/ truncate-unconnected 1) 
      (/ is-weakly-generative 1) 
      (/ is-strongly-generative 1) 
      (/ is-connected 1) 
      (/ truncate 3) 
      (/ truncate-r 2) 
      (/ truncate-r 1) 
      (/ truncate-flat-r 1) 
      (/ truncate-unconnecting 1) 
      (/ truncate-unconnecting 2) 
      (/ truncate-strongly-generative 1) 
      (/ truncate-strongly-generative 2) 
      (/ truncate-neg-based 1) 
      (/ truncate-flat-neg-based 1) 
      (/ truncate-facts 1) 
      (/ truncate-j 2) 
      (/ noduplicate-symbols 1) 
      (/ noduplicate-symbol 2) 
      (/ select-var-sharing-lits 2) 
      (/ already-in 3)))

 
;
; IMPORTS
  !(use-module-if-exists 
    (library not) 
    (:: (/ once 1)))
  !(use-module-if-exists 
    (library sets) 
    (:: 
      (/ union 2) 
      (/ union 3) 
      (/ intersection 3) 
      (/ list-to-set 2) 
      (/ subtract 3) 
      (/ subset 2) 
      (/ select 3)))
  !(use-module-if-exists 
    (library strings) 
    (:: 
      (/ string-append 3) 
      (/ substring 4) 
      (/ midstring 6)))
  !(use-module-if-exists 
    (library basics) 
    (:: (/ member 2)))
  !(use-module-if-exists 
    (library lists) 
    (:: 
      (/ subseq 3) 
      (/ rev 2) 
      (/ last 2) 
      (/ nth1 4)))
  !(use-module-if-exists 
    (library occurs) 
    (:: (/ sub-term 2)))
  !(use-module-if-exists 
    (library arg) 
    (:: (/ genarg 3)))
  !(use-module-if-exists 
    (library subsumes) 
    (:: (/ subsumes-chk 2)))
  !(use-module 
    (home div-utils) 
    (:: (/ shares-var 2)))
  !(use-module 
    (home var-utils) 
    (:: 
      (/ vars 2) 
      (/ contains-vars 2) 
      (/ skolemize 3) 
      (/ deskolemize 3) 
      (/ skolems 2) 
      (/ flagged-contains-vars 3)))
  !(use-module 
    (home kb) 
    (:: 
      (/ get-clause 5) 
      (/ store-clause 4) 
      (/ delete-clause 1) 
      (/ get-fact 4) 
      (/ unflatten-kb 0)))
  !(use-module 
    (home flatten) 
    (:: 
      (/ flatten-clause 2) 
      (/ unflatten-clause 2)))
  !(use-module 
    (home evaluation) 
    (:: (/ correct-chk 0)))


;
; METAPREDICATES
;
; none

  !(dynamic (/ functional-mode 1))

;
; ***********************************************************************
;
; *
;
; * module: filter.pl
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
; * description: various filters useful for td- and bu-induction
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
; * predicate: already_in/3 (optional)
;
; *
;
; * syntax: already_in(+N,+P,+Y)
;
; *
;
; * args: N: arity of a literal P
;
; * P: literal
;
; * Y: term
;
; *
;
; * description: succeeds if Y is(==) already an argument of P
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


  (= (already-in 0 $_ $_)  
    (set-det)
    (fail))
  (= (already-in $N $P $Y)  
    (arg $N $P $Pn)
    (det-if-then-else 
      (== $Pn $Y) True 
      (, 
        (is $N1 
          (- $N 1)) 
        (already-in $N1 $P $Y))))


;
; ***********************************************************************
;
; *
;
; * predicate: select_var_sharing_lits/2
;
; *
;
; * syntax: select_var_sharing_lits(+List_of_Clauses,-List_of_Clauses)
;
; *
;
; * args:
;
; *
;
; * description: removes all clauses from List_of_Clauses the last
;
; * literal of which does not share a variable with
;
; * the rest of the clause -> to be used during td-refinement
;
; * (not to add unconnected body literals)
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


  (= (select_var_sharing_lits  () ())  True)
  (= (select-var-sharing-lits (Cons  $C $R) $R2)  
    (select-var-sharing-lits $R $R1)
    (det-if-then-else 
      (var-sharing-lit $C Nil) 
      (= $R2 
        (Cons  $C $R1)) 
      (= $R2 $R1)))


;
; ***********************************************************************
;
; *
;
; * predicate: var_sharing_lit/2
;
; *
;
; * syntax: var_sharing_lit(+Clause,+CAccu)
;
; *
;
; * args: CAccu contains all literals of clause except the last one
;
; *
;
; * description: succeeds if the last literal of clause shares (at least)
;
; * a variable with the rest of the clause.
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


  (= (var-sharing-lit (= $H  $B) Nil)  
    (set-det)
    (var-sharing-lit $B 
      (:: $H)))
  (= (var-sharing-lit (, $A $B) $C)  
    (set-det)
    (var-sharing-lit $B 
      (Cons  $A $C)))
  (= (var-sharing-lit $A $C)  
    (shares-var $A $C)
    (set-det))


;
; ***********************************************************************
;
; * predicate: noduplicate_atom/2
;
; *
;
; * syntax: noduplicate_atom(+P,+B)
;
; *
;
; * args: P: literal
;
; * B: clause body
;
; *
;
; * description: tests if P already occurs(==) in B
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


  (= (noduplicate-atom $P (, $A $B))  
    (set-det)
    (\== $P $A)
    (noduplicate-atom $P $B))
  (= (noduplicate-atom $P $A)  
    (\== $P $A))


;
; ***********************************************************************
;
; * predicate: noduplicate_atoms/1
;
; *
;
; * syntax: noduplicate_atoms(+Clause)
;
; *
;
; * args:
;
; *
;
; * description: tests whether Clause contains duplicate literals
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


  (= (noduplicate-atoms (= $A  $B))  
    (set-det)
    (noduplicate-atom $A $B)
    (noduplicate-atoms $B))
  (= (noduplicate-atoms (, $A $B))  
    (set-det)
    (noduplicate-atom $A $B)
    (noduplicate-atoms $B))
  (= (noduplicate_symbols  $_)  True)
   

;
; ***********************************************************************
;
; *
;
; * predicate: connected_vars/4
;
; *
;
; * syntax: connected_vars(+ClauseIn,-ClauseOut,-Connected,-Unconnected)
;
; *
;
; * args: ClauseIn,ClauseOut: clauses in list notation
;
; * Connected,Unconnected: list of variables
;
; *
;
; * description: returns connected & unconnected vars
;
; * A variable is connected ( to the head literal ) iff
;
; * - it appears in the head, or
;
; * - it appears in a literal with a connected variable
;
; * A literal is connected iff it's vars are.
;
; *
;
; * example:
;
; *
;
; * peculiarities: we make a copy of the input clause
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************

;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  miles/filter.pl 242 29 6845))



  (= (connected-skolems (Cons  (with_self  $Head (p)) $Body) $Connected $Unconnected)  
    (skolems $Head $Con)
    (= $Uncon Nil)
    (find-connected-skolems-in-body $Body $Con $Connected $Uncon $Unconnected_tupels)
    (union $Unconnected_tupels $Unconnected))


;
; ***********************************************************************
;
; *
;
; * predicate: find_connected_skolems_in_body/5
;
; *
;
; * syntax: find_connected_skolems_in_body(+Body_list,+Connected,-Connected,
;
; * +Unconnected_tuples,-Unconnected_tuples)
;
; *
;
; * args: Body_list .. clause body in list notation,
;
; * Connected .. connected skolem atoms
;
; * Unconnected_tuples .. lists of lists of unconnected skolem atoms
;
; *
;
; * description: separates connected and unconnected skolem atoms
;
; * (each corresponds to a variable) within the body
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


  (= (find-connected-skolems-in-body (Cons  (with_self  $L $_) $Rest) $C1 $C2 $U1 $U2)  
    (skolems $L $V1)
    (det-if-then-else 
      (, 
        (intersection $C1 $V1 $I) 
        (\== $I Nil)) 
      (, 
        (union $C1 $V1 $C4) 
        (connect $V1 $C4 $C3 $U1 $U3)) 
      (, 
        (= $C3 $C1) 
        (= $U3 
          (Cons  $V1 $U1))))
    (find-connected-skolems-in-body $Rest $C3 $C2 $U3 $U2))
  (= (find_connected_skolems_in_body  () $C $C $U $U)  True)


  (= (connect  $V $C $C () ())  True)
  (= (connect $Vars $C1 $C2 (Cons  $Tupel $More) $U2)  
    (member $Var $Vars)
    (member $Var $Tupel)
    (set-det)
    (union $C1 $Tupel $C3)
    (connect $Vars $C3 $C2 $More $U2))
  (= (connect $Vars $C1 $C2 (Cons  $Tupel $More) (Cons  $Tupel $U2))  
    (connect $Vars $C1 $C2 $More $U2))


;
; ***********************************************************************
;
; *
;
; * predicate: is_weakly_generative/1
;
; *
;
; * syntax: is_weakly_generative(+Clause)
;
; *
;
; * args: Clause: clause in list notation
;
; *
;
; * description: a clause is weakly generative if all vars of its head
;
; * appear also in another literal
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
; * see also: Muggleton,90
;
; *
;
; ***********************************************************************


  (= (is-weakly-generative (Cons  (with_self  $H (p)) $B))  
    (vars $H $Vars)
    (contains-vars $Vars $B))


;
; ***********************************************************************
;
; *
;
; * predicate: is_stronly_generative/1
;
; *
;
; * syntax: is_strongly_generative(+Clause)
;
; *
;
; * args: Clause: clausew in list notation
;
; *
;
; * description: a clause is strongly generative if every variable
;
; * appears in at least 2 literals
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
; * see also: Muggleton,90
;
; *
;
; ***********************************************************************


  (= (is-strongly-generative $Clause)  
    (findall $Flag 
      (, 
        (subseq $Clause 
          (:: $L) $Rest) 
        (vars $L $Vars) 
        (flagged-contains-vars $Vars $Rest $Flag)) $Flags)
    (not (member False $Flags))
    (set-det))


;
; ***********************************************************************
;
; *
;
; * predicate: is_connected/1
;
; *
;
; * syntax: is_connected(+Clause)
;
; *
;
; * args: Clause: clause in list notation
;
; *
;
; * description: a clause is connected if every variable is connected
;
; * to the head.
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
; * see also: Rouveirol,91
;
; *
;
; ***********************************************************************


  (= (is-connected $Clause)  
    (connected-vars $Clause $_ $_ Nil))


;
; ***********************************************************************
;
; *
;
; * predicate: is_flat/1, is_unflat/1
;
; *
;
; * syntax: is_flat(+Clause), is_unflat(Clause)
;
; *
;
; * args: clause in list notation [ head:p, b1:n, .. ]
;
; *
;
; * description: succeeds/fails if Clause contains no function symbols
;
; *
;
; * example:
;
; *
;
; * peculiarities: if input is not a clause in list form, is_flat/1
;
; * always succeeds.
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (is-flat $Clause)  
    (nonvar $Clause)
    (not (is-unflat $Clause)))


  (= (is-unflat (Cons  (with_self  $L $_) $Rest))  
    (is-unflat-literal $L))
  (= (is-unflat (Cons  $_ $Rest))  
    (is-unflat $Rest))                  


  (= (is-unflat-literal $L)  
    (sub-term $Subterm $L)
    (\== $Subterm $L)
    (nonvar $Subterm)
    (set-det))


;
; ***********************************************************************
;
; *
;
; * predicate: truncate/3
;
; *
;
; * syntax: truncate(+Strategy,+ClauseIn,-ClauseOut)
;
; *
;
; * args: Strategy: one of { r, unconnected, unconnecting,
;
; * strongly_generative}
;
; * ClauseIn,ClauseOut: integers (kb references)
;
; *
;
; * description: performs truncation operator on ClauseIn
;
; * using Strategy.
;
; *
;
; * The list of possible strategies is to be completed.
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


  (= (truncate $Strategy $In $Out)  
    (get-clause $In $_ $_ $C1 $_)
    (do-truncate $Strategy $C1 $C2)
    (store-clause $_ $C2 trunc $Out))


  (= (do-truncate r $C1 $C2)  
    (truncate-r $C1 $C2))
  (= (do-truncate unconnected $C1 $C2)  
    (truncate-unconnected $C1 $C2))
  (= (do-truncate strongly-generative $C1 $C2)  
    (truncate-strongly-generative $C1 $C2))
  (= (do-truncate unconnecting $C1 $C2)  
    (truncate-unconnecting $C1 $C2)) 


;
; ***********************************************************************
;
; *
;
; * predicate: truncate_r/2/1
;
; *
;
; * syntax: truncate_r(ClauseIn,ClauseOut)
;
; * truncate_r(ID)
;
; *
;
; * args: clauses in list notation
;
; *
;
; * description: drop all literals with label ':r',
;
; * i.e. drop all literals that were used in saturation.
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


  (= (truncate-r $ID)  
    (get-clause $ID $_ $_ $ClauseIn $_)
    (truncate-r $ClauseIn $ClauseOut)
    (delete-clause $ID)
    (store-clause $_ $ClauseOut trc $ID))

  (= (truncate-r $ClauseIn $ClauseOut)  
    (copy-term $ClauseIn $C)
    (do-truncate-r $C $ClauseOut))


  (= (do_truncate_r  () ())  True)
  (= (do-truncate-r (Cons  (with_self  $L (r)) $Rest) $Rest1)  
    (set-det)
    (do-truncate-r $Rest $Rest1))
  (= (do-truncate-r (Cons  (with_self  $L $S) $Rest) (Cons  (with_self  $L $S) $Rest1))  
    (set-det)
    (do-truncate-r $Rest $Rest1))


;
; ***********************************************************************
;
; *
;
; * predicate: truncate_flat_r/2/1
;
; *
;
; * syntax: truncate_flat_r(ClauseIn,ClauseOut)
;
; * truncate_flat_r(ID)
;
; *
;
; * args: clauses in list notation
;
; *
;
; * description: drop all true literals with label ':r',
;
; * i.e. drop all literals that were used in saturation,
;
; * but no literals with type information ( suffix '_p').
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


  (= (truncate-flat-r $ID)  
    (get-clause $ID $_ $_ $ClauseIn $_)
    (truncate-flat-r $ClauseIn $ClauseOut)
    (delete-clause $ID)
    (store-clause $_ $ClauseOut trc $ID))

  (= (truncate-flat-r $ClauseIn $ClauseOut)  
    (copy-term $ClauseIn $C)
    (do-truncate-flat-r $C $ClauseOut))


  (= (do_truncate_flat_r  () ())  True)
  (= (do-truncate-flat-r (Cons  (with_self  $L (r)) $Rest) $Rest1)  
    (functor $L $F $N)
    (functor $LC $F $N)
    (get-clause $_ $LC $_ $_ usr)
    (set-det)
    (do-truncate-flat-r $Rest $Rest1))
; ; L is bg predicate
  (= (do-truncate-flat-r (Cons  (with_self  $L $S) $Rest) (Cons  (with_self  $L $S) $Rest1))  
    (set-det)
    (do-truncate-flat-r $Rest $Rest1))


;
; ***********************************************************************
;
; *
;
; * predicate: truncate_unconnected/2
;
; *
;
; * syntax: truncate_unconnected(+ClauseIn, -ClauseOut)
;
; *
;
; * args: ClauseIn,ClauseOut: clauses in list notation
;
; *
;
; * description: truncate unconnected body literals (see above)
;
; *
;
; * example: let ClauseIn = [ min(A,[A|B]):p, min(C,B):n, ge(E,F):n ]
;
; * then ClauseOut = [ min(A,[A|B]):p, min(C,B):n ]
;
; * let ClauseIn = [p(X):n,q(X,V1):n,r(V1,V2):n,q(V3):n,s(V3,V1):n],
;
; * then ClauseOut = [p(X):n,q(X,V1):n,r(V1,V2):n,q(V3):n,s(V3,V1):n]
;
; * (in contrast to truncate_unconnecting)
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


  (= (truncate-unconnected $ID)  
    (get-clause $ID $_ $_ $C $_)
    (truncate-unconnected $C $D)
    (delete-clause $ID)
    (store-clause $_ $D trc-unconn $ID)
    (set-det))



  (= (truncate-unconnected $ClauseIn $ClauseOut)  
    (skolemize $ClauseIn $S $ClauseInS)
    (connected-skolems $ClauseInS $Con $Uncon)
    (= $ClauseInS 
      (Cons  
        (with_self  $Head 
          (p)) $Body))
    (truncate-unconnected1 $Body $Uncon $BodyOut)
    (= $ClauseOutS 
      (Cons  
        (with_self  $Head 
          (p)) $BodyOut))
    (deskolemize $ClauseOutS $S $ClauseOut))


  (= (truncate_unconnected1  () $_ ())  True)  ;
; no literals to drop
  (= (truncate-unconnected1 $B Nil $B)  
    (set-det))   ;
; all vars connected
  (= (truncate-unconnected1 (Cons  (with_self  $L $_) $More) $Uncon $BodyOut)  
    (det-if-then-else 
      (, 
        (skolems $L $SKs) 
        (member $A $SKs)) 
      (, 
        (member $A $Uncon) 
        (truncate-unconnected1 $More $Uncon $BodyOut)) fail)
    (set-det))
; ; either all or no vars in L are connected
; ; local cut
  (= (truncate-unconnected1 (Cons  (with_self  $L $S) $More) $Uncon (Cons  (with_self  $L $S) $BodyOut))  
    (truncate-unconnected1 $More $Uncon $BodyOut)
    (set-det))


;
; ***********************************************************************
;
; *
;
; * predicate: truncate_unconnecting/1/2
;
; *
;
; * syntax: truncate_unconnecting(ClauseIn,ClauseOut)
;
; *
;
; * args: clauses in list notation
;
; *
;
; * description: connectivity heuristics for truncation:
;
; * Rouveirol: Drop a body literal if all other literals remain
;
; * connected.
;
; * We added another constraint: the resulting clause must be
;
; * weakly generative.
;
; * Do not confuse the connectivity with the connectedness
;
; * heuristics !!
;
; *
;
; * example: truncate_unconnecting([p(X):n,q(X,V1):n,r(V1,V2):n,q(V3):n,s(V3,V1):n],
;
; * [p(X):n,q(X,V1):n,r(V1,V2):n, s(V3,V1):n],
;
; *
;
; * peculiarities:
;
; *
;
; * see also: Rouveirol '90; module connectedness.pl
;
; *
;
; ***********************************************************************


  (= (truncate-unconnecting $ID)  
    (get-clause $ID $_ $_ $C $_)
    (truncate-unconnecting $C $D)
    (delete-clause $ID)
    (store-clause $_ $D trc-unconn $ID)
    (set-det))

  (= (truncate-unconnecting $ClauseIn $ClauseOut)  
    (copy-term $ClauseIn 
      (Cons  
        (with_self  $H 
          (p)) $Body))
    (subseq $Body 
      (:: $L) $BodyOut)
    (= $ClauseOut1 
      (Cons  
        (with_self  $H 
          (p)) $BodyOut))
    (connected-vars $ClauseOut1 $ClauseOut $Con Nil)
    (is-weakly-generative $ClauseOut))
; ; no unconnected vars


;
; ***********************************************************************
;
; *
;
; * predicate: truncate_strongly_generative/2
;
; *
;
; * syntax: truncate_strongly_generative(+ClauseIn,-ClauseOut)
;
; *
;
; * args: clauses in list notation
;
; *
;
; * description: drop one body literal from a strongly generative
;
; * clause s.t. the resulting clause is also strongly
;
; * generative.
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


  (= (truncate-strongly-generative $ID)  
    (get-clause $ID $_ $_ $C $_)
    (truncate-strongly-generative $C $D)
    (delete-clause $ID)
    (store-clause $_ $D trc-unconn $ID)
    (set-det))

  (= (truncate-strongly-generative $ClauseIn $ClauseOut)  
    (copy-term $ClauseIn 
      (Cons  
        (with_self  $H 
          (p)) $Body))
    (subseq $Body 
      (:: $L) $BodyOut)
    (= $ClauseOut 
      (Cons  
        (with_self  $H 
          (p)) $BodyOut))
    (is-strongly-generative $ClauseOut))


;
; ***********************************************************************
;
; *
;
; * predicate: truncate_neg_based/1
;
; *
;
; * syntax: truncate_neg_based(+ID)
;
; *
;
; * args: integer, kb reference
;
; *
;
; * description: truncate as many literals as possible, s.t. the
;
; * resulting clause covers no negative examples.
;
; *
;
; * This only works on unflat clauses;
;
; * i.e. it only accounts for dropping condition.
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
; * see also : Muggleton's negative based reduction
;
; *
;
; ***********************************************************************


  (= (truncate-neg-based $ID)  
    (or 
      (get-clause $ID $_ $_ $C $Label) 
      (, 
        (store-clause $_ $C $Label $ID) 
        (fail)))
    (delete-clause $ID)
    (once (truncate-unconnected $C $D))
    (store-clause $_ $D trc $ID)
    (correct-chk)
    (= $D 
      (Cons  $H $Body))
    (length $Body $N)
    (truncate-neg-based1 $N $ID $H $Body)) 


  (= (truncate-neg-based1 0 $_ $_ $_)  
    (set-det))

  (= (truncate-neg-based1 $N $ID $H $Body)  
    (nth1 $N $Body $L $NewBody)
    (delete-clause $ID)
    (store-clause $_ 
      (Cons  $H $NewBody) trc $ID)
    (is $M 
      (- $N 1))
    (det-if-then-else 
      (, 
        (is-weakly-generative (Cons  $H $NewBody)) 
        (correct-chk)) 
      (truncate-neg-based1 $M $ID $H $NewBody) 
      (, 
        (delete-clause $ID) 
        (store-clause $_ 
          (Cons  $H $Body) trc $ID) 
        (truncate-neg-based1 $M $ID $H $Body))))


;
; ***********************************************************************
;
; *
;
; * predicate: truncate_flat_neg_based/1
;
; *
;
; * syntax: truncate_flat_neg_based(+ID)
;
; *
;
; * args: integer, kb reference
;
; *
;
; * description: truncate as many literals as possible, s.t. the
;
; * resulting clause covers no negative examples.
;
; *
;
; * As initial condition, the kb must be unflat.
;
; * The truncation is done on the flattened clause, so that
;
; * this accounts for the dropping rule & inverse subst.
;
; * On exiting, the kb is unflat.
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
; * see also : Muggleton's negative based reduction
;
; *
;
; ***********************************************************************


  (= (truncate-flat-neg-based $ID)  
    (get-clause $ID $_ $_ $C $Label)
    (flatten-clause $C $D)
    (truncate-unconnected $D $E)
    (delete-clause $ID)
    (store-clause $_ $E $Trc $ID)
    (set-det)
    (or 
      (, 
        (once correct-chk) 
        (= $D 
          (Cons  $H $Body)) 
        (truncate-flat-neg-based $ID $H Nil $Body)) 
      (, 
        (delete-clause $ID) 
        (store-clause $_ $C $Label $ID))))

  (= (truncate-flat-neg-based $ID $H $Nec Nil)  
    (delete-clause $ID)
    (truncate-unconnected 
      (Cons  $H $Nec) $D)
    (unflatten-clause $D $E)
    (store-clause $_ $E trc $ID)
    (set-det)
    (correct-chk))


  (= (truncate-flat-neg-based $ID $H $Nec (Cons  $L $Maybe))  
    (append $Nec $Maybe $C)
    (truncate-unconnected 
      (Cons  $H $C) $D)
    (unflatten-clause $D $E)
    (delete-clause $ID)
    (store-clause $_ $E trc $ID)
    (det-if-then-else correct-chk 
      (= $Nec1 $Nec) 
      (append $Nec 
        (:: $L) $Nec1))
    (truncate-flat-neg-based $ID $H $Nec1 $Maybe))


;
; ***********************************************************************
;
; *
;
; * predicate: truncate_facts/1
;
; *
;
; * syntax: truncate_facts(+ID)
;
; *
;
; * args: integer, kb reference
;
; *
;
; * description: truncate all body literals unifying with a kb fact
;
; * labeled 'usr'.
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


  (= (truncate-facts $ID)  
    (get-clause $ID $_ $_ 
      (Cons  $H $B) $_)
    (truncate-facts1 $B $BodyOut)
    (delete-clause $ID)
    (store-clause $_ 
      (Cons  $H $BodyOut) trc $ID))


  (= (truncate_facts1  () ())  True)
  (= (truncate-facts1 (Cons  (with_self  $L $S) $Rest) $BodyOut)  
    (truncate-facts1 $Rest $Rest1)
    (det-if-then-else 
      (, 
        (get-fact $_ $L1 $_ usr) 
        (subsumes-chk $L1 $L)) 
      (= $BodyOut $Rest1) 
      (= $BodyOut 
        (Cons  
          (with_self  $L $S) $Rest1))))


;
; ***********************************************************************
;
; *
;
; * predicate: truncate_j/2
;
; *
;
; * syntax: truncate_j(+ID,J)
;
; *
;
; * args: ID: kb reference
;
; * J : integer = number of allowed new variables per literal
;
; *
;
; * description: truncate all literals containing more than J variables
;
; * not appearing in the head of kb clause ID.
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
; * see also: truncate_j is remotely related to Muggleton's
;
; * ij-determination. We take i = 1.
;
; * More importantly, we cannot tell in our system, if a
;
; * literal is determinate or not, since we have no model.
;
; *
;
; ***********************************************************************


  (= (truncate-j $ID $J)  
    (get-clause $ID $_ $_ $C $_)
    (skolemize $C $S 
      (Cons  
        (with_self  $Head 
          (p)) $BodyIn))
    (skolems $Head $Vars)
    (do-truncate-j $J $Vars $BodyIn $BodyOutS)
    (deskolemize 
      (Cons  
        (with_self  $Head 
          (p)) $BodyOutS) $S $D)
    (truncate-unconnected $D $E)
    (delete-clause $ID)
    (store-clause $_ $E trc-j $ID))


  (= (do-truncate-j $J $Vars $BodyIn $BodyOut)  
    (findall $L 
      (, 
        (member $L $BodyIn) 
        (once (, (skolems $L $VarsL) (subtract $VarsL $Vars $NewVars) (length $NewVars $J1) (=< $J1 $J)))) $BodyOut))
                    

