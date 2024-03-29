;                                           (convert-to-metta-file  var_utils $_454808 miles/var_utils.pl miles/var_utils.metta)
;
; MODULE var_utils EXPORTS

  !(module var-utils 
    (:: 
      (/ inverse-substitute 2) 
      (/ inverse-substitute1 2) 
      (/ skolems 2) 
      (/ skolemize 3) 
      (/ skolemize 4) 
      (/ deskolemize 3) 
      (/ relevant-vars2 6) 
      (/ relevant-vars3 6) 
      (/ buildrelterms 6) 
      (/ contains-vars 2) 
      (/ flagged-contains-vars 3) 
      (/ vars 2) 
      (/ term-size 2) 
      (/ replace 4) 
      (/ inv-replace 4) 
      (/ terms 3) 
      (/ terms 4) 
      (/ only-vars 2) 
      (/ clause-terms 2) 
      (/ only-vars1 2) 
      (/ typed-only-vars1 2) 
      (/ exists-intersect 3) 
      (/ clean-subst 3) 
      (/ findargs 3) 
      (/ allarg 4)))

;
; IMPORTS
  !(use-module 
    (home div-utils) 
    (:: 
      (/ effaceall 3) 
      (/ genterm-test 2) 
      (/ identical-member 2) 
      (/ mysetof 3) 
      (/ clist-to-prolog 2) 
      (/ best 2) 
      (/ subterm-at-position 4) 
      (/ part-of-clause 2)))
  !(use-module 
    (home flatten) 
    (:: 
      (/ flatten-clause 2) 
      (/ unflatten-clause 2)))
  !(use-module 
    (home filter) 
    (:: (/ truncate-unconnected 2)))
  !(use-module 
    (home lgg) 
    (:: (/ lgg-terms 7))) 
  !(use-module-if-exists 
    (library basics) 
    (:: 
      (/ member 2) 
      (/ nonmember 2) 
      (/ memberchk 2)))
  !(use-module-if-exists 
    (library sets) 
    (:: 
      (/ union 3) 
      (/ subtract 3) 
      (/ list-to-set 2) 
      (/ intersection 3)))
  !(use-module-if-exists 
    (library strings) 
    (:: 
      (/ gensym 2) 
      (/ string-append 3) 
      (/ substring 4)))
  !(use-module-if-exists 
    (library occurs) 
    (:: 
      (/ sub-term 2) 
      (/ contains-var 2)))
  !(use-module-if-exists 
    (library subsumes) 
    (:: (/ variant 2)))


;
; METAPREDICATES
;
; none



;
; ***********************************************************************
;
; *
;
; * module: var_utils.pl
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
; * description: - utilities for variable and clause handling
;
; * - determine relevant variables for predicate invention.
;
; * - inverse_substitution
;
; * - skolemization is a special substitution
;
; * - replacement
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
; * predicate: vars/2
;
; *
;
; * syntax: vars(+Term,-Vars)
;
; *
;
; * args: Term: any MeTTa term
;
; * Vars: list of variables in Term
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
; * peculiarities: setof changed to mysetof (IS)
;
; *
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************
 

  (= (vars $Term $Vars)  
    (mysetof $V 
      (, 
        (sub-term $V $Term) 
        (var $V)) $Vars))


;
; ***********************************************************************
;
; *
;
; * predicate: clause_terms/2
;
; *
;
; * syntax: clause_terms(+Clause,-Termlist)
;
; *
;
; * args:
;
; *
;
; * description: returns list of all non-ground terms in Clause
;
; *
;
; * example: clause_terms((p(f(X),a,g(Y,b)):- r(f(X),Y)),
;
; * [f(X),X,g(Y,b),Y])
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


  (= (clause-terms (= $H  $B) $L)  
    (set-det)
    (functor $H $_ $N)
    (terms $N $H Nil $L0)
    (clause-terms $B $L0 $L))
  (= (clause-terms $H $L)  
    (functor $H $_ $N)
    (terms $N $H Nil $L))   

  (= (clause-terms (, $A $B) $L $L2)  
    (set-det)
    (clause-terms $A $L $L1)
    (clause-terms $B $L1 $L2))
  (= (clause-terms $A $L $L1)  
    (functor $A $_ $N)
    (terms $N $A $L $L1))


;
; ***********************************************************************
;
; *
;
; * predicate: terms/3,4
;
; *
;
; * syntax: terms(+Term,+Accu,-Accu)
;
; * terms(+Count,+Term,+Accu,-Accu)
;
; *
;
; * args:
;
; *
;
; * description: returns all non-ground subterms within Term
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


  (= (terms $V $L $L1)  
    (var $V)
    (set-det)
    (det-if-then-else 
      (identical-member $V $L) 
      (= $L1 $L) 
      (= $L1 
        (Cons  $V $L))))
  (= (terms $T $L $L1)  
    (functor $T $_ $N)
    (det-if-then-else 
      (or 
        (ground $T) 
        (identical-member $T $L)) 
      (= $L1 $L) 
      (terms $N $T 
        (Cons  $T $L) $L1)))

  (= (terms 0 $_ $L $L)  
    (set-det))
  (= (terms $N $T $L $L2)  
    (is $N1 
      (- $N 1))
    (terms $N1 $T $L $L1)
    (arg-quintus $N $T $Tn)
    (terms $Tn $L1 $L2))


;
; ***********************************************************************
;
; *
;
; * predicate: only_vars/2
;
; *
;
; * syntax: only_vars(+Term,-Varlist)
;
; *
;
; * args:
;
; *
;
; * description: returns all variables within Term
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


  (= (only-vars $T $L)  
    (terms $T Nil $L1)
    (only-vars1 $L1 $L))


  (= (only-vars1  () ())  True)
  (= (only-vars1 (Cons  $X $R) (Cons  $X $R1))  
    (var $X)
    (set-det)
    (only-vars1 $R $R1))
  (= (only-vars1 (Cons  $_ $R) $R1)  
    (only-vars1 $R $R1))


;
; ***********************************************************************
;
; *
;
; * predicate: typed_only_vars1/2
;
; *
;
; * syntax: typed_only_vars1(+TypedTermlist,-TypedVarlist)
;
; *
;
; * args: TypedTermlist: [T:typeT,...]
;
; * Vars: [Var:typeVar
;
; *
;
; * description: extracts each term T that is a variable
;
; * from a list TypedTermlist of terms with type definition
;
; *
;
; * example: only_vars2([X:type16,Y:type14,f(Z):type23],[X:type16,Y:type14])
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


  (= (typed-only-vars1  () ())  True)
  (= (typed-only-vars1 (Cons  (with-self  $X $T) $R) (Cons  (with-self  $X $T) $R1))  
    (var $X)
    (set-det)
    (typed-only-vars1 $R $R1))
  (= (typed-only-vars1 (Cons  $_ $R) $R1)  
    (typed-only-vars1 $R $R1))



;
; ***********************************************************************
;
; *
;
; * predicate: replace/3
;
; *
;
; * syntax: replace(+C1,+S1,-C2,-S2)
;
; *
;
; * args: C1, C2: clauses in list notation
;
; * S1, S2: replacements [ X / Term, .. ]
;
; * If all X's are variables, this is actually a
;
; * substitution, but we also allow other terms.
;
; *
;
; * description: C2 is a copy of C1 with S1 applied.
;
; * S2 is a copy of S1.
;
; *
;
; * example: replace( [p(A,B):p], [A/a], [p(a,D):p], [C/a]).
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


  (= (replace $C $S1 $D $S2)  
    (copy-term 
      (, $C $S1) 
      (, $E $S2))
    (do-replace $E $S2 $D))



  (= (do-replace  () $_ ())  True)

  (= (do-replace (Cons  $L $More) $S (Cons  $L1 $More1))  
    (do-replace1 $L $S $L1)
    (set-det)
    (do-replace $More $S $More1))


  (= (do-replace1 $T1 $S $T2)  
    (det-if-then-else 
      (, 
        (member 
          (/ $X $T) $S) 
        (== $X $T1)) 
      (= $T2 $T) 
      (det-if-then-else 
        (var $T1) 
        (= $T1 $T2) 
        (det-if-then 
          (functor $T1 $F $N) 
          (, 
            (functor $T2 $F $N) 
            (do-replace1 $N $T1 $T2 $S))))))

  (= (do-replace1  0 $_ $_ $_)  True)

  (= (do-replace1 $N $T1 $T2 $S)  
    (arg-quintus $N $T1 $A)
    (arg-quintus $N $T2 $B)
    (do-replace1 $A $S $B)
    (is $M 
      (- $N 1))
    (do-replace1 $M $T1 $T2 $S))


  (= (arg-quintus $N $C $E)  
    (compound $C)
    (arg $N $C $E))


;
; ***********************************************************************
;
; *
;
; * predicate: inv_replace/4
;
; *
;
; * syntax: inv_replace(+C1,+S1,-C2,-S2)
;
; *
;
; * args: C1, C2: clauses in list notation
;
; * S1, S2: replacements [ X / Term, .. ]
;
; *
;
; * description: C2 is a copy of C1 with term of S1 replaced by ass. vars
;
; * S2 is a copy of S1,s.t. vars(S2) in vars(C2).
;
; *
;
; * example: inv_replace( [p(a,B):p], [A/a], [p(C,D):p], [C/a]).
;
; *
;
; * peculiarities: this is not the inverse operation for replacement :
;
; * We don't distinguish the between places of terms.
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************



  (= (inv-replace $C $S1 $D $S2)  
    (copy-term 
      (, $C $S1) 
      (, $E $S2))
    (do-inv-replace $E $S2 $D))



  (= (do-inv-replace  () $_ ())  True)

  (= (do-inv-replace (Cons  $L $More) $S (Cons  $L1 $More1))  
    (do-inv-replace1 $L $S $L1)
    (do-inv-replace $More $S $More1))


  (= (do-inv-replace1 $T1 $S $T2)  
    (det-if-then-else 
      (var $T1) 
      (= $T1 $T2) 
      (det-if-then-else 
        (, 
          (member 
            (/ $X $T) $S) 
          (== $T $T1)) 
        (= $T2 $X) 
        (det-if-then 
          (functor $T1 $F $N) 
          (, 
            (functor $T2 $F $N) 
            (do-inv-replace1 $N $T1 $T2 $S))))))

  (= (do-inv-replace1  0 $_ $_ $_)  True)

  (= (do-inv-replace1 $N $T1 $T2 $S)  
    (arg-quintus $N $T1 $A)
    (arg-quintus $N $T2 $B)
    (do-inv-replace1 $A $S $B)
    (is $M 
      (- $N 1))
    (do-inv-replace1 $M $T1 $T2 $S))


  (= (do-inv-replace1 2 (:: $T) $T2 $S)  
    (arg-quintus 2 $T2 Nil)
    (do-inv-replace1 1 
      (:: $T) $T2 $S))


;
; ***********************************************************************
;
; *
;
; * predicate: term_size/2
;
; *
;
; * syntax: term_size(+Term,-Size)
;
; *
;
; * args:
;
; *
;
; * description: the folllowing code is a debugged copy from the Quintus library
;
; * 'termdepth'
;
; * term_size(+Term, ?Size) calculates the Size of a Term, defined
;
; * to be the number of constant and function symbol occurrences in it.
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


  (= (term-size $Term $Size)  
    (det-if-then-else 
      (var $Term) 
      (= $Size 0) 
      (, 
        (functor $Term $F $Arity) 
        (term-size $Arity $Term 1 $Size))))
; /* nonvar(Term) */
; ; Here was the bug
; ; "


  (= (term-size $N $NonVar $SoFar $Size)  
    (det-if-then-else 
      (=:= $N 0) 
      (is $Size $SoFar) 
      (, 
        (arg-quintus $N $NonVar $Arg) 
        (term-size $Arg $ArgSize) 
        (is $Accum 
          (+ $SoFar $ArgSize)) 
        (is $M 
          (- $N 1)) 
        (term-size $M $NonVar $Accum $Size))))


;
; ***************************************************************************
;
; *
;
; * predicate: contains_vars/2
;
; *
;
; * syntax: contains_vars(+Term,+Terms)
;
; *
;
; * args: Term: any MeTTa term
;
; * Vars: list of MeTTa terms (also variables)
;
; *
;
; * description: succeeds if all terms in Terms occur in Term
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
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (contains-vars  () $_)  True)
  (= (contains-vars (Cons  $V $Vars) $Term)  
    (contains-var $V $Term)
    (contains-vars $Vars $Term))


;
; ***************************************************************************
;
; *
;
; * predicate: flagged_contains_vars/3
;
; *
;
; * syntax: flagged_contains_vars(+Term,+Terms,-Flag)
;
; *
;
; * args: Term: any MeTTa term, Flag in {true,false}
;
; * Vars: list of MeTTa terms (also variables)
;
; *
;
; * description: returns true if all terms in Terms occur in Term, else false
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
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (flagged-contains-vars $Vars $Term True)  
    (contains-vars $Vars $Term)
    (set-det))
  (= (flagged-contains-vars  $Vars $Term false)  True)


;
; ***********************************************************************
;
; *
;
; * predicate: inverse_substitute/2
;
; *
;
; * syntax: inverse_substitute(+ClauseIn,-ClauseOut)
;
; *
;
; * args: clauses in list notation, i.e. [ head(A):p, b1(A):n, .. ]
;
; *
;
; * description: replace one term in ClauseIn by a variable.
;
; * Thru backtracking all solutions can be obtained.
;
; * Implementation: flatten Clause,
;
; * truncate one literal,
;
; * truncate unconnected literals,
;
; * unflatten Clause.
;
; *
;
; * example:
;
; *
;
; * peculiarities: Since identical terms are represented only once in
;
; * our flattening, we cannot tell between different
;
; * places the terms appear at.
;
; *
;
; * see also: Muggleton,1988
;
; *
;
; ***********************************************************************


  (= (inverse-substitute  $Clause $Clause)  True)       ;
; empty inverse substitution

  (= (inverse-substitute $ClauseIn $ClauseOut)  
    (flatten-clause $ClauseIn $C1)
    (remove-type-literal $C1 $C2)
    (truncate-unconnected $C2 $C3)
    (unflatten-clause $C3 $ClauseOut))


;
; ***********************************************************************
;
; *
;
; * predicate: remove_type_literal/2
;
; *
;
; * syntax: remove_type_literal(+CL,-CL1)
;
; *
;
; * args: CL,CL1: clause in list notation
;
; *
;
; * description: drop a "type literal" functor_p(...)
;
; * the next rules allow to perform inverse substitutions on several terms,
;
; * at the cost of an exploding search space.
;
; * A better strategy is to trunacte literals one by one and
;
; * only to truncate the promising clauses further.
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


  (= (remove-type-literal (Cons  (with-self  $L_p n) $More) $More)  
    (functor $L_p $F $_)
    (string-append $_ -p $F))
; ; drop this literal


  
  (= (remove-type-literal (Cons  (with-self  $L $S) $More) (Cons  (with-self  $L $S) $More1))  
    (remove-type-literal $More $More1))
; ; drop another literal 




;
; ***********************************************************************
;
; *
;
; * predicate: inverse_substitute1/2
;
; *
;
; * syntax: inverse_substitute1(+CL,-CL)
;
; *
;
; * args: CL,CL1: clauses in list notation
;
; *
;
; * description: this is an alternative approach without flattening
;
; * it replaces terms by variables.
;
; * (This does of course not work on flat clauses)
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


  (= (inverse-substitute1 $CLin $CLout)  
    (copy-term $CLin $CLin1)
    (clist-to-prolog $CLin1 $Clause)
    (mysetof 
      (with-self  $Sub $Pos) 
      (, 
        (subterm-at-position $Clause $Sub Nil $Pos) 
        (not (part-of-clause $Sub $Clause)) 
        (nonvar $Sub)) $Sublist)
    (isub1-list $Sublist $Sublist1)
    (best $Sublist1 
      (with-self  $T $Positions))
    (do-inverse-sub1 $T $Positions $_ $Clause $Clause1)
    (clist-to-prolog $CLout $Clause1)
    (not (variant $CLout $CLin)))
; ; this rule disallows variable renaming to constrain
; ; search space at the cost of incompleteness



  (= (isub1-list  () ())  True)
  (= (isub1-list (Cons  (with-self  $T $Pos) $R) (Cons  (with-self  $T (Cons  $Pos $Pos1)) $R2))  
    (isub1-l $T $R $R1 $Pos1)
    (isub1-list $R1 $R2))


  (= (isub1-l  $_ () () ())  True)
  (= (isub1-l $T (Cons  (with-self  $T1 $Pos) $R) $R2 $Pos1)  
    (isub1-l $T $R $R1 $Pos0)
    (det-if-then-else 
      (== $T $T1) 
      (, 
        (= $R2 $R1) 
        (= $Pos1 
          (Cons  $Pos $Pos0))) 
      (, 
        (= $R2 
          (Cons  
            (with-self  $T1 $Pos) $R1)) 
        (= $Pos1 $Pos0))))


;
; ***********************************************************************
;
; *
;
; * predicate: do_inverse_substitute1/5
;
; *
;
; * syntax: do_inverse_substitute(+Term,+Positions,+Var,+Clause,-Clause)
;
; *
;
; * args: Clause: MeTTa clause
;
; * Term: the term in Clause to be replaced with variable Var
;
; * Positions: list of positions of Term within Clause where it might
;
; * be replaced. A position is a list of numbers
;
; *
;
; * description: replaces Term by a Var
;
; * preference is to replace all occurrences of Term by Var;
;
; * thru backtracking, clauses may be obtained where
;
; * only some occurences of term are replaced.
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


  (= (do-inverse-sub1  $_ () $_ $Clause $Clause)  True)
  (= (do-inverse-sub1 $T (Cons  $P $R) $Var $Clause $Clause2)  
    (do-inverse-sub1 $T $R $Var $Clause $Clause1)
    (do-isub1 $T $P $Var $Clause1 $Clause2))
  (= (do-inverse-sub1 $T (Cons  $_ $R) $Var $Clause $Clause1)  
    (do-inverse-sub1 $T $R $Var $Clause $Clause1))


  (= (do-isub1  $_ () $Var $_ $Var)  True)
  (= (do-isub1 $T (Cons  $P $R) $V $C $C1)  
    (functor $C $F $N)
    (functor $C1 $F $N)
    (do-isub-copy $N $P $C $C1)
    (arg-quintus $P $C1 $C1p)
    (arg-quintus $P $C $Cp)
    (do-isub1 $T $R $V $Cp $C1p))


  (= (do-isub-copy 0 $_ $_ $_)  
    (set-det))
  (= (do-isub-copy $N $P $C $C1)  
    (is $N1 
      (- $N 1))
    (do-isub-copy $N1 $P $C $C1)
    (det-if-then-else 
      (== $N $P) True 
      (, 
        (arg-quintus $N $C $Cn) 
        (arg-quintus $N $C1 $Cn))))


;
; ***********************************************************************
;
; *
;
; * predicate: skolemize/3, deskolemize/3
;
; *
;
; * syntax: skolemize(+Term1,-Subst,-Term2)
;
; *
;
; * args: Term1,Term2: arbiraty MeTTa terms
;
; * Subst : substitution [ V1/t1, V2/t2, .. ]
;
; * where Vi are variables, ti skolem atoms
;
; *
;
; * description: skolemization is a special substitution: all variables
;
; * of Term1 are substituted by atoms. One keeps track of
;
; * the substitution thru Subst.
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
; * see also: Rouveirol,1991: ITOU.
;
; *
;
; ***********************************************************************


  (= (skolemize $T1 $S $T2)  
    (skolemize $T1 Nil $S $T2))

  (= (skolemize $T1 $S $S $Sk_Atom)  
    (var $T1)
    (already-skolem-covered $T1 $S $Sk_Atom)
    (set-det))
  (= (skolemize $Var $S (Cons  (/ $Var $Sk_Atom) $S) $Sk_Atom)  
    (var $Var)
    (set-det)
    (gensym sk-symbol $Sk_Atom))
  (= (skolemize $T1 $S1 $S2 $T2)  
    (functor $T1 $F $N)
    (functor $T2 $F $N)
    (skolemize $N $T1 $S1 $S2 $T2))
  (= (skolemize  0 $_ $S $S $_)  True)
  (= (skolemize $N $T $S1 $S2 $U)  
    (arg-quintus $N $T $Tn)
    (arg-quintus $N $U $Un)
    (skolemize $Tn $S1 $S3 $Un)
    (is $M 
      (- $N 1))
    (skolemize $M $T $S3 $S2 $U))


;
; ***********************************************************************
;
; *
;
; * predicate: already_skolem_covered/3
;
; *
;
; * syntax: already_skolem_covered(+Var,+Subst,-Skolem_atom)
;
; *
;
; * args:
;
; *
;
; * description: if Var has already been skolemized, i.e. Var:Skolem_atom in Subst,
;
; * the corresponding Skolem_atom is returned
;
; *
;
; * example:
;
; *
;
; * peculiarities: alter Name: already_covered
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (already-skolem-covered $Var (Cons  (/ $Var1 $Sk_Atom) $_) $Sk_Atom)  
    (== $Var $Var1)
    (set-det))

  (= (already-skolem-covered $Var (Cons  $_ $S) $Sk_Atom)  
    (already-skolem-covered $Var $S $Sk_Atom)) 


;
; ***********************************************************************
;
; *
;
; * predicate: deskolemize/3
;
; *
;
; * syntax: deskolemize(+Term1,+Subst,-Term2)
;
; *
;
; *
;
; * args: Term1,Term2: arbiraty MeTTa terms
;
; * Subst : substitution [ V1/t1, V2/t2, .. ]
;
; * where Vi are variables, ti skolem atoms
;
; * description: Deskolemization reverses skolemization, if the
;
; * skolem substitution is given as input.
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


  (= (deskolemize $Sk_Atom $S $Var)  
    (atom $Sk_Atom)
    (skolem-covered $Sk_Atom $S $Var)
    (set-det))
  (= (deskolemize $Atom $_ $Atom)  
    (atomic $Atom)
    (set-det))
  (= (deskolemize $Var $S $Var)  
    (var $Var)
    (set-det))
  (= (deskolemize $T1 $S $T2)  
    (functor $T1 $F $N)
    (functor $T2 $F $N)
    (deskolemize $N $T1 $S $T2))
  (= (deskolemize  0 $_ $_ $_)  True)
  (= (deskolemize $N $T $S $U)  
    (arg-quintus $N $T $Tn)
    (arg-quintus $N $U $Un)
    (deskolemize $Tn $S $Un)
    (is $M 
      (- $N 1))
    (deskolemize $M $T $S $U))


;
; ***********************************************************************
;
; *
;
; * predicate: skolem_covered/3
;
; *
;
; * syntax: skolem_covered(+Skolem_atom,+Subst,-Var)
;
; *
;
; * args:
;
; *
;
; * description: returns the variable that has been skolemized with Skolem_atom,
;
; * i.e. Var/Skolem_atom in Subst
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
; * see also: alter Name: covered
;
; *
;
; ***********************************************************************


  (= (skolem-covered $Sk_Atom (Cons  (/ $Var $Sk_Atom) $_) $Var)  
    (set-det))
  (= (skolem-covered $Sk_Atom (Cons  $_ $S) $Var)  
    (skolem-covered $Sk_Atom $S $Var))


;
; ***********************************************************************
;
; *
;
; * predicate: skolems/2
;
; *
;
; * syntax: skolems(+Term,-Skolems)
;
; *
;
; * args: Term: skolemized term, Skolems: all skolem atoms in Term
;
; *
;
; * description: returns skolem atoms occuring in Term
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


  (= (skolems $Term $Skolems)  
    (setof $Skolem 
      (^ $Len 
        (, 
          (sub-term $Skolem $Term) 
          (atom $Skolem) 
          (symbol-concat sk-symbol $Rest $Skolem) 
          (symbol-length $Rest $Len))) $Skolems)
    (set-det))
; ; q_substring(Skolem,sk_atom,0,Len)
  (= (skolems  $_ ())  True)



;
; ***********************************************************************
;
; *
;
; * predicate: relevant_vars2/6
;
; *
;
; * syntax: relevant_vars2(+C1,+C2,+Gen,+S1,+S2,-RelVars)
;
; *
;
; * args: C1,C2,Gen: clauses in list notation. C1,C2 at bottom of W.
;
; * S1,S2: substitutions [V1=T1, .. ].
;
; * RelVars: list of vars [ V1, V2, .. ]
;
; *
;
; * description: determine relevant vars with CIGOL heuristics.
;
; * A variable V in Gen is relevant if
;
; * one of the terms T1, T2 it is substituted by in S1, S2
;
; * contains a variable that also appears elsewhere
;
; * in S1 or S2.
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


  (= (relevant-vars2 $_ $_ $C $S1 $S2 $RelVars)  
    (vars $C $AllVars)
    (relevant-vars2 $AllVars $S1 $S2 $RelVars))


  (= (relevant-vars2  () $_ $_ ())  True)

  (= (relevant-vars2 (Cons  $V $MoreVars) $S1 $S2 (Cons  $V $RelVars))  
    (or 
      (, 
        (member 
          (/ $W $T1) $S1) 
        (== $V $W) 
        (member 
          (/ $X $T1a) $S1) 
        (\== $V $X) 
        (sub-term $Subterm1 $T1) 
        (var $Subterm1) 
        (contains-var $Subterm1 $T1a)) 
      (, 
        (member 
          (/ $W $T2) $S2) 
        (== $V $W) 
        (member 
          (/ $Y $T2a) $S2) 
        (\== $V $Y) 
        (sub-term $Subterm2 $T2) 
        (var $Subterm2) 
        (contains-var $Subterm2 $T2a)))
    (set-det)
    (relevant-vars2 $MoreVars $S1 $S2 $RelVars))

  (= (relevant-vars2 (Cons  $V $MoreVars) $S1 $S2 $RelVars)  
    (relevant-vars2 $MoreVars $S1 $S2 $RelVars))



;
; ***********************************************************************
;
; *
;
; * predicate: relevant_vars3/6
;
; *
;
; * syntax: relevant_vars3(+C1,+C2,+Gen,+S1,+S2,-RelVars)
;
; *
;
; * args: C1,C2,Gen: clauses in list notation
;
; * S1,S2: substititions
;
; * RelVars : set of relevant vars
;
; *
;
; * description: Gen is a common generaliztion of C1,C2 ,
;
; * s.t. S1(Gen) is a subsetof C1, and analogously for C2.
;
; *
;
; * A variable is relevant if
;
; * it appears in both Gen and ( C1 - Gen )
;
; * or Gen and ( C2 - Gen ).
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
; * see also: IRES,ITOU
;
; *
;
; ***********************************************************************


  (= (relevant-vars3 $C1 $C2 $Gen $S1 $S2 $Vars)  
    (skolemize 
      (, $C1 $C2 $Gen $S1 $S2) $Phi 
      (, $D1 $D2 $Gen1 $R1 $R2))
    (relevant-vars3a $D1 $Gen1 $R1 $Vars1)
    (relevant-vars3a $D2 $Gen1 $R2 $Vars2)
    (length $Vars1 $Len1)
    (length $Vars2 $Len2)
    (== $Len1 $Len2)
    (union $Vars1 $Vars2 $Vars0)
    (deskolemize $Vars0 $Phi $Vars))


  (= (relevant-vars3a $Spec $Gen $S $Skolems)  
    (replace $Gen $S $Gen1 $S)
    (subtract $Spec $Gen1 $Rest)
    (inv-replace $Rest $S $Rest1 $S)
    (skolems $Rest1 $Skolems1)
    (skolems $Gen $Skolems2)
    (intersection $Skolems1 $Skolems2 $Skolems))
; ;;changed Irene



;
; ***********************************************************************
;
; *
;
; * predicate: findargs/3
;
; *
;
; * syntax: findargs(+CL,+Accu,-Accu)
;
; *
;
; * args: CL: clause in list notation, Accu: arguments of the literals in CL
;
; *
;
; * description: find all arguments of the literals of a given clause
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


  (= (findargs Nil $Result $Result)  
    (set-det))
  (= (findargs (Cons  (with-self  $Lit1 $_) $Rest) $Accu $Result)  
    (functor $Lit1 $_ $N)
    (allarg $N $Lit1 Nil $Args)
    (union $Accu $Args $Newaccu)
    (findargs $Rest $Newaccu $Result))
; ; set operator


  (= (allarg 0 $_ $Accu $Accu)  
    (set-det))
  (= (allarg $N $Lit $Args $Result)  
    (arg-quintus $N $Lit $Arg1)
    (is $M 
      (- $N 1))
    (det-if-then-else 
      (nonmember $Arg1 $Args) 
      (allarg $M $Lit 
        (Cons  $Arg1 $Args) $Result) 
      (allarg $M $Lit $Args $Result)))


;
; ************************************************************************
;
; *
;
; * predicate: buildrelterms/6
;
; *
;
; * syntax: buildrelterms(+CL1,+CL2,+Clgg,+Subst1,+Subst2,-TermList)
;
; *
;
; * args: CL1, CL2, Clgg .. clauses in list representation
;
; * Subst1,Subst2 ... substitutions such that Clgg Subst1 = CL1
;
; * and Clgg Subst2 = CL2
;
; * TermList ... list of relevant terms for the new predicate
;
; *
;
; * description: determines the relevant terms for the new predicate
;
; * as described in R. Wirth's 1989 PhD thesis
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


  (= (buildrelterms $SpecC1 $SpecC2 $Gen $S1 $S2 $Terms)  
    (skolemize 
      (, $Gen $SpecC1 $SpecC2 $S1 $S2) $SS 
      (, $Gen1 $Spec1 $Spec2 $SS1 $SS2))
    (findterms $Gen1 $Spec1 $SS1 $Terms1)
    (findterms $Gen1 $Spec2 $SS2 $Terms2)
    (deskolemize 
      (, $Terms1 $Terms2) $SS 
      (, $T1 $T2))
    (general-terms $T1 $T2 $Terms $S1 $S2))
; ; Terms1 = RArgs1 != {}
; ; Terms2 = RArgs2 != {} ; union(Terms1,Terms2,TermsS), ; deskolemize(TermsS,SS,Terms). ; changed Irene



  (= (findterms $Gen $Spec $SS1 $RArgsG)  
    (replace $Gen $SS1 $Gen2 $_)
    (subtract $Spec $Gen2 $RestSpec)
    (subtract $Spec $RestSpec $SpecG)
    (findargs $SpecG Nil $ArgsG)
    (findargs $RestSpec Nil $ArgsR)
    (exists-intersect $ArgsG $ArgsR $RArgsG))
; ; RestSpec = Ci^r ( Spec - Gen )
; ; SpecG = Ci^g ;
; RArgs = relevant argument terms (not [])
;
; inv_replace(RArgsG0,SS1,RArgsG,SS1). ; changed (Irene)
	
;
; ************************************************************************
;
; *
;
; * predicate: general_terms/5
;
; *
;
; * syntax: general_terms(+T1,+T2,-TG,+Subst1,+Subst2)
;
; *
;
; * args: T1, T2 .. relevant terms in CL1, CL2 (cf. above)
;
; * Subst1, Subst2 .. substitutions (cf. above)
;
; * TG .. relevant terms in Clgg
;
; *
;
; * description: determines the relevant terms in Clgg that
;
; * correspond to the relevant terms in CL1 and CL2
;
; * here, inv_replace is used!!
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

;
; * general_terms (like a shotgun wedding between lgg for terms and inverse replacement...)

  (= (general-terms Nil Nil Nil $_ $_)  
    (set-det))

  (= (general-terms (Cons  $T1 $R1) Nil (Cons  $T $R3) $S1 $_)  
    (det-if-then-else 
      (genterm-test 
        (/ $T $T1) $S1) True 
      (inv-replace $T1 $S1 $T $_))
    (set-det)
    (general-terms $R1 Nil $R3 $S1 $_))

  (= (general-terms Nil (Cons  $T2 $R2) (Cons  $T $R3) $_ $S2)  
    (det-if-then-else 
      (genterm-test 
        (/ $T $T2) $S2) True 
      (inv-replace $T2 $S2 $T $_))
    (set-det)
    (general-terms Nil $R2 $R3 $_ $S2))

  (= (general-terms (Cons  $T1 $R1) $L2 (Cons  $T $R3) $S1 $S2)  
    (gen-term $T1 $L2 $L2Rest $T $S1 $S2)
    (set-det)
    (general-terms $R1 $L2Rest $R3 $S1 $S2))


  (= (gen-term $T1 $L2 $L2new $T $S1 $S2)  
    (nonvar $T1)
    (functor $T1 $F $N)
    (effaceall $T2 $L2 $L2new)
    (functor $T2 $F $N)
    (lgg-terms $T1 $T2 $T $_ $_ $S1 $S2))

  (= (gen-term $T1 $L2 $L2new $X $S1 $S2)  
    (effaceall $T2 $L2 $L2new)
    (genterm-test 
      (/ $X $T1) $S1)
    (genterm-test 
      (/ $Y $T2) $S2)
    (== $X $Y))

  (= (gen-term $T1 $L2 $L2 $T $S1 $_)  
    (det-if-then-else 
      (genterm-test 
        (/ $T $T1) $S1) True 
      (inv-replace $T1 $S1 $T $_)))


;
; ************************************************************************
;
; *
;
; * predicate: exists_intersect/3
;
; *
;
; * syntax: exists_intersect(+L1,+L2,-L)
;
; *
;
; * args: L1,L2,L: lists
;
; *
;
; * description: if nonempty intersection exists, succeeds and returns
;
; * intersection, fails else
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


  (= (exists-intersect $X $Y $Z)  
    (exi $X $Y $Z $_)
    (set-det))

  (= (exi Nil $_ Nil $Flag)  
    (== $Flag yes))
  (= (exi Nil $_ Nil $_)  
    ( (set-det) fail))
  (= (exi (Cons  $X $R) $Y (Cons  $X $Z) yes)  
    (memberchk $X $Y)
    (set-det)
    (exi $R $Y $Z yes))
  (= (exi (Cons  $_ $R) $Y $Z $Flag)  
    (exi $R $Y $Z $Flag))



;
; ************************************************************************
;
; *
;
; * predicate: clean_subst/3
;
; *
;
; * syntax: clean_subst(+CL,+Subst,-Subst)
;
; *
;
; * args: CL: clause in list notation, Subst: a substitution [X/Term,...]
;
; *
;
; * description: removes all X/T from Subst such that X does not occur in CL
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


  (= (clean-subst  $_ () ())  True)
  (= (clean-subst $CL (Cons  (/ $X $T) $R) $R2)  
    (clean-subst $CL $R $R1)
    (det-if-then-else 
      (contains-var $X $CL) 
      (= $R2 
        (Cons  
          (/ $X $T) $R1)) 
      (= $R2 $R1)))
