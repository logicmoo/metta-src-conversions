;                                           (convert_to_metta_file  tdref_it $_204458 miles/tdref_it.pl miles/tdref_it.metta)
;
; MODULE tdref_it EXPORTS

  !(module tdref-it 
    (:: 
      (/ refinement-unify-variables 3) 
      (/ refinement-unify-variables 2) 
      (/ refinement-instantiate-variables 3) 
      (/ refinement-instantiate-variables 2) 
      (/ refinement-add-body-literal 3) 
      (/ refinement-add-body-literal 2) 
      (/ refinement 2) 
      (/ possible-body-literals 3)))


;
; IMPORTS
  !(use-module 
    (home kb) 
    (:: 
      (/ get-clause 5) 
      (/ get-evaluation 2) 
      (/ get-predlist 1)))
  !(use-module 
    (home td-basic) 
    (:: 
      (/ distribute-vars 3) 
      (/ enumerate-t 3) 
      (/ append-body 3)))
  !(use-module 
    (home filter) 
    (:: 
      (/ noduplicate-symbols 1) 
      (/ noduplicate-symbol 2) 
      (/ select-var-sharing-lits 2)))
  !(use-module 
    (home div-utils) 
    (:: (/ mysetof 3)))
  !(use-module 
    (home var-utils) 
    (:: 
      (/ typed-only-vars1 2) 
      (/ clause-terms 2)))
  !(use-module 
    (home argument-types) 
    (:: 
      (/ compare-types 3) 
      (/ types-of 3)))
  !(use-module 
    (home show-utils) 
    (:: (/ write-list 1)))


;
; METAPREDICATES
;
; none


;
; ***********************************************************************
;
; *
;
; * module: tdref_it.pl
;
; *
;
; * author: I.Stahl date:12/92
;
; *
;
; * changed: comments BT; 11/10/92
;
; *
;
; * description: top-down refinement operators for Horn clauses
;
; * work iteratively
;
; *
;
; *
;
; * see also:
;
; *
;
; *
;
; ***********************************************************************


;
; ***********************************************************************
;
; *
;
; * predicates: refinement_unify_variables/2,
;
; * refinement_instantiate_variables/2,
;
; * refinement_add_body_literal/2
;
; *
;
; * syntax: refinement_...(+ClauseID,-(ClauseID,CL))
;
; *
;
; * args: ClauseID ... ID of the clause to be specialized
;
; * CL .... list of specialisations of Clause
;
; *
;
; * description: refines clause by unifying variables/instantiating
;
; * variables with terms/adding a body literal. Returns
;
; * list of specialised clauses
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


  (= (refinement-unify-variables $ID (, $ID $CL))  
    (get-clause $ID $H $B $_ $_)
    (= $C 
      (= $H  $B))
    (clause-terms $C $Terms)
    (types-of $Terms $C $TTerms)
    (refinement-unify-variables $C $TTerms $CL))


  (= (refinement-instantiate-variables $ID (, $ID $CL))  
    (get-clause $ID $H $B $_ $_)
    (= $C 
      (= $H  $B))
    (clause-terms $C $Terms)
    (types-of $Terms $C $TTerms)
    (refinement-instantiate-variables $C $TTerms $CL))


  (= (refinement-add-body-literal $ID (, $ID $CL))  
    (get-clause $ID $H $B $_ $_)
    (= $C 
      (= $H  $B))
    (clause-terms $C $Terms)
    (types-of $Terms $C $TTerms)
    (refinement-add-body-literal $C $TTerms $CL1)
    (select-var-sharing-lits $CL1 $CL))



;
; ***********************************************************************
;
; *
;
; * predicates: refinement_unify_variables/3,
;
; * refinement_instantiate_variables/3,
;
; * refinement_add_body_literal/3
;
; *
;
; * syntax: refinement_...(+Clause,+Terms,-CL)
;
; *
;
; * args: Clause ... the clause to be specialized
;
; * Terms ... the terms that shall be used in refinement
;
; * of the form [T:Type,...]
;
; * CL .... list of specialisations of Clause
;
; *
;
; * description: refines clause by unifying variables/instantiating
;
; * variables with terms/adding a body literal. Returns
;
; * list of specialised clauses
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


  (= (refinement-unify-variables $C $T $CL)  
    (typed-only-vars1 $T $Vars)
    (ref-unify-vars $C $Vars $Vars Nil $CL))


  (= (refinement-instantiate-variables $C $T $CL)  
    (typed-only-vars1 $T $Vars)
    (ref-instantiate-vars $C $Vars Nil $CL))


  (= (refinement-add-body-literal $C $T $CL)  
    (det-if-then-else 
      (= $C 
        (= $_  $_)) 
      (ref-add-body-literal $C $T Nil $CL) 
      (ref-add-body-literal 
        (= $C  True) $T Nil $CL)))



;
; ***********************************************************************
;
; *
;
; * predicates: possible_body_literals/3
;
; *
;
; * syntax: possible_body_literals(+Clause,+Terms,-LL)
;
; *
;
; * args: Clause ... the clause to be specialized
;
; * Terms ... the terms that shall be used in refinement
;
; * of the form [T:Type,...]
;
; * LL .... list of literals that might be added
;
; *
;
; * description: Returns the list of literals that might be used to refine
;
; * Clause
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


  (= (possible-body-literals $C $Terms $LL)  
    (get-predlist $Predlist)
    (enumerate-terms $Predlist $Terms $C Nil $LL))


;
; ***********************************************************************
;
; * predicate: ref_unify_vars/5
;
; * syntax: ref_unify_vars(+C,+[X:T|R],+V,+CL,-CL2)
;
; *
;
; * args: C: the clause to be refined
;
; * [X:T|R]: all variables with their types in C
;
; * V: all variables X:T in C
;
; *
;
; * CL: initial clause set
;
; * CL2: CL + copies of C where variables are unified
;
; *
;
; * description: unifies clause variables of the same type
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


  (= (ref_unify_vars  $_ () $_ $CL $CL)  True)
  (= (ref-unify-vars $C (Cons  $X $R) $V $CL $CL2)  
    (ref-unify-vars $C $R $V $CL $CL1)
    (ref-unify-vars1 $C $X $V $CL1 $CL2))


;
; ***********************************************************************
;
; *
;
; * predicate: ref_unify_vars1/5
;
; *
;
; * syntax: ref_unify_vars1(+C,+X:Tx,+[Y:Ty|R],+CL,-CL2)
;
; *
;
; * args: C: the clause to be refined
;
; * X:Tx: variable X of type Tx in C
;
; * [Y:Ty|R]: all variables Y:Ty in C
;
; *
;
; * CL: initial clause set
;
; * CL2: copies of the clause C where X is unified with each
;
; * matching Y
;
; *
;
; * description: makes copies of the clause C with unified variables
;
; * if the type of the variables is can be matched
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


  (= (ref_unify_vars1  $_ $_ () $CL $CL)  True)
  (= (ref-unify-vars1 $C (with_self  $X $Tx) (Cons  (with_self  $Y $Ty) $R) $CL $CL2)  
    (ref-unify-vars1 $C 
      (with_self  $X $Tx) $R $CL $CL1)
    (det-if-then-else 
      (, 
        (compare-types $Tx $Ty $_) 
        (@< $X $Y)) 
      (, 
        (copy-term 
          (, $C $X $Y) 
          (, $C1 $Z $Z)) 
        (det-if-then-else 
          (noduplicate-atoms $C1) 
          (= $CL2 
            (Cons  $C1 $CL1)) 
          (= $CL2 $CL1))) 
      (= $CL2 $CL1)))


;
; ***********************************************************************
;
; *
;
; * predicate: ref_instantiate_vars/4
;
; *
;
; * syntax: ref_instantiate_vars(+C,+[X:T|R]+CL,-CL2)
;
; *
;
; * args: C: the clause to be refined
;
; * [X:T|R]: variables X of type T in C
;
; *
;
; * CL: initial clause set
;
; * CL2: CL + copies of C where variables are instantiated
;
; *
;
; * description: instantiates variables by terms according to the
;
; * variables' type
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


  (= (ref_instantiate_vars  $_ () $CL $CL)  True)
  (= (ref-instantiate-vars $C (Cons  (with_self  $X $T) $R) $CL $CL2)  
    (ref-instantiate-vars $C $R $CL $CL1)
    (=.. $H 
      (:: $T $X))
    (=.. $H1 
      (:: $T $X1))
    (mysetof $H1 
      (^ $I 
        (^ $B 
          (^ $BL 
            (^ $Lab 
              (, 
                (get-clause $I $H1 $B $BL $Lab) 
                (nonvar $X1)))))) $HL)
    (ref-inst-vars $HL $H $C $HL1)
    (append $HL1 $CL1 $CL2))


  (= (ref_inst_vars  () $_ $_ ())  True)
  (= (ref-inst-vars (Cons  $H1 $R) $H $C $HL2)  
    (ref-inst-vars $R $H $C $HL1)
    (det-if-then-else 
      (, 
        (copy-term 
          (, $H $C) 
          (, $H1 $C1)) 
        (noduplicate-atoms $C1)) 
      (= $HL2 
        (Cons  $C1 $HL1)) 
      (= $HL2 $HL1)))


;
; ***********************************************************************
;
; *
;
; * predicate: ref_add_body_literal/4
;
; *
;
; * syntax: ref_add_body_literal(+C,+Terms,+CL,-CL1)
;
; *
;
; * args: C: the clause to be refined
;
; * [TT:T|R]: all terms TT and subterms with their types T in C
;
; *
;
; * CL: initial clause set
;
; * CL2: CL + copies of C with additional body literals
;
; *
;
; * description: adds a body literal to C by
;
; * - selecting all predicates with a type restriction contained in the kb
;
; * - enumerating literals where (at least one) argument of the new
;
; * literal is unfied with terms in the clause
;
; * - for each literal L: copy C and L, append the copied
;
; * literal to the copy of C and add the resulting clause to CL
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


  (= (ref-add-body-literal $C $Terms $CL $CL1)  
    (get-predlist $Predlist)
    (enumerate-terms $Predlist $Terms $C Nil $L)
    (add-to-bodies $L $C $CL $CL1))


;
; ***********************************************************************
;
; *
;
; * predicate: add_to_bodies/4
;
; *
;
; * syntax: add_to_bodies(+[Lit|R],+C,+CL,-CL1)
;
; *
;
; * args: [Lit|R]: a set of literals Lit to be added to the body
;
; * C: the clause to be refined
;
; *
;
; * CL: initial clause list
;
; * CL1: CL + refined clauses
;
; *
;
; * description: adds each literal to a copy of C, if it is not
;
; * duplicate in C
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


  (= (add_to_bodies  () $_ $CL $CL)  True)
  (= (add-to-bodies (Cons  $Pred $R) $C $CL $CL2)  
    (add-to-bodies $R $C $CL $CL1)
    (copy-term 
      (, $Pred $C) 
      (, $Pred1 
        (= $H1  $B1)))
    (det-if-then-else 
      (noduplicate-atom $Pred1 
        (, $H1 $B1)) 
      (, 
        (append-body 
          (= $H1  $B1) $Pred1 $C2) 
        (= $CL2 
          (Cons  $C2 $CL1))) 
      (= $CL2 $CL1)))


;
; ***********************************************************************
;
; *
;
; * predicate: enumerate_terms/5
;
; *
;
; * syntax: enumerate_terms(+[P:PVars|R],+Terms,+C,+L,-L2)
;
; *
;
; * args: [P:PVars|R]: predicate P & its variables PVars=[PV1:Tpv1,...]
;
; * Terms: all terms with types in C
;
; * C: clause to be refined
;
; * L: initial literal list
;
; *
;
; * L2: new literal list
;
; *
;
; * description: builds all literals from all predicates to be added by
;
; * - collecting all variables that could be unified with any variable of PVars
;
; * - building all literals with these variables replaced
;
; * - eliminating all those literals that already occur in C
;
; * - appending the literals to L2
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


  (= (enumerate_terms  () $_ $_ $L $L)  True)
  (= (enumerate-terms (Cons  (with_self  $P $PVars) $R) $V $C $L $L2)  
    (enumerate-terms $R $V $C $L $L1)
    (distribute-vars $PVars $V $PVars1)
    (enumerate-t $PVars1 
      (:: $P) $Plist)
    (append $Plist $L1 $L2))


;
; ***********************************************************************
;
; *
;
; * predicate: refinement/2
;
; *
;
; * syntax: refinement(+ID,-CL)
;
; *
;
; * args: ID: ID of a clause to be refined
;
; * CL: a list with refinements of the clause with ID
;
; *
;
; * description: shapiro's general refinement operator for a clause
;
; * with ID (all terms are eligible for refinement):
;
; * if there are covered positive examples:
;
; * - prepare for refinement:
;
; * a list of all terms and subterms augmented by their types
;
; * and a list of all variables in the clause with types
;
; * - refine clause by
;
; * - unifying variables within the clause
;
; * - instantiating variables within the clause to terms
;
; * - adding body literals. Only literals sharing at least a
;
; * variable with clause ID are allowed.
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


  (= (refinement $ID $CL)  
    (number $ID)
    (set-det)
    (get-clause $ID $H $B $_ $_)
    (get-evaluation $ID 
      (evaluation $_ $_ $Pos $_ $_ $_ $_ $_ $_))
    (det-if-then-else 
      (== $Pos Nil) 
      (= $CL Nil) 
      (, 
        (clause-terms 
          (= $H  $B) $Terms) 
        (types-of $Terms 
          (= $H  $B) $TTerms) 
        (refinement-unify-variables 
          (= $H  $B) $TTerms $CL0) 
        (refinement-instantiate-variables 
          (= $H  $B) $TTerms $CL1) 
        (refinement-add-body-literal 
          (= $H  $B) $TTerms $CL2) 
        (select-var-sharing-lits $CL2 $CL3) 
        (append $CL0 $CL1 $CL4) 
        (append $CL4 $CL3 $CL))))

  (= (refinement (= $H  $B) $CL)  
    (clause-terms 
      (= $H  $B) $Terms)
    (types-of $Terms 
      (= $H  $B) $TTerms)
    (refinement-unify-variables 
      (= $H  $B) $TTerms $CL0)
    (refinement-instantiate-variables 
      (= $H  $B) $TTerms $CL1)
    (refinement-add-body-literal 
      (= $H  $B) $TTerms $CL2)
    (select-var-sharing-lits $CL2 $CL3)
    (append $CL0 $CL1 $CL4)
    (append $CL4 $CL3 $CL))

