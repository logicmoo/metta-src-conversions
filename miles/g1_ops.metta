;                                           (convert_to_metta_file  g1_ops $_286080 miles/g1_ops.pl miles/g1_ops.metta)
;
; MODULE g1_ops EXPORTS

  !(module g1-ops 
    (:: 
      (/ saturate 2) 
      (/ saturate 3) 
      (/ elem-saturate 3) 
      (/ inv-derivate 2) 
      (/ inv-derivate 3) 
      (/ inv-derivate1 2) 
      (/ most-spec-v 3) 
      (/ absorb 3) 
      (/ identify 3) 
      (/ g1-op 3) 
      (/ g1-op 4) 
      (/ apply-g1 2)))
; ; saturate with default depth
; ; saturate with given maximum depth

;
; IMPORTS
  !(use-module 
    (home lgg) 
    (:: 
      (/ headed-lgg 3) 
      (/ headed-lgg 4)))
  !(use-module 
    (home kb) 
    (:: 
      (/ get-clause 5) 
      (/ store-clause 4) 
      (/ delete-clause 1) 
      (/ get-example 3)))
  !(use-module 
    (home var-utils) 
    (:: 
      (/ skolemize 3) 
      (/ skolemize 4) 
      (/ deskolemize 3)))
  !(use-module 
    (home div-utils) 
    (:: 
      (/ neg 2) 
      (/ buildpar2 3) 
      (/ efface 3)))
  !(use-module 
    (home bu-basics) 
    (:: 
      (/ addtolist 1) 
      (/ getlist 1) 
      (/ clear-mngr 0) 
      (/ assert-body 1) 
      (/ abs-process-proofs 2) 
      (/ abs-build-body 1) 
      (/ assert-clause 1) 
      (/ ident-process-proofs 2) 
      (/ g1-process-proofs 2) 
      (/ g1-build-clause 2) 
      (/ idev-build-clause 2) 
      (/ process-new-literals 2) 
      (/ sat-build-clause 3) 
      (/ assert-absorptions 2) 
      (/ body 3) 
      (/ ident-build-body 1)))
  !(use-module 
    (home show-utils) 
    (:: (/ show-bodies 0)))
  !(use-module 
    (home interpreter) 
    (:: 
      (/ prove3 2) 
      (/ prove4 3)))
  !(use-module-if-exists 
    (library basics) 
    (:: (/ member 2)))

;
; METAPREDICATES
;
; none


;
; ***********************************************************************
;
; *
;
; * module: g1_ops.pl
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
; *
;
; * description: - g1-Operator
;
; * Implementation of Ruediger Wirth's G1-operator for inverse
;
; * resolution corresponding to his 1989 PhD thesis.
;
; * - Absorption
;
; * All clauses induced by absorption are labelled "abs" in kb
;
; * - Rouveirol's saturation, with functions allowed as terms.
;
; * *Saturation: Maximum depth: given as input
;
; * default: 100 inverse resolution steps
;
; * *elementary saturation
;
; * - inverse derivate
;
; * Muggleton's inverse linear derivation, i.e. the
;
; * repeated application of the most specific v
;
; * (most specific absorption &most specific identification)
;
; * Induced clauses are marked invd
;
; * - identification
;
; * clauses induced by identification are labelled "idn" in kb
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


;
; ********************************************************************************;
;
; *
;
; * predicate: g1_op/3, g1_op/4
;
; *
;
; * syntax: g1_op ( +ResolventID, +Parent1ID, -Parent2ID )
;
; * g1_op ( +ResolventID, +Parent1ID, -Parent2ID, + Label )
;
; *
;
; * args: ResolventID, Parent1ID, Parent2ID .. clauseIDs
;
; * Label for Parent2ID (default: g11)
;
; *
;
; * description: given a resolvent and one parent clause, the second parent clause
;
; * is constructed
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
; ********************************************************************************


  (= (g1-op $Res $Par1 $Par2)  
    (g1-op $Res $Par1 $Par2 g11))
  (= (g1-op $Res $Par1 $Par2 $Label)  
    (det-if-then-else 
      (var $Label) 
      (= $Label g11) True)
    (get-clause $Res $_ $_ $Lres $_)
    (get-clause $Par1 $_ $_ $Lpar1 $_)
    (\== $Res $Par1)
    (clear-mngr)
    (skolemize $Lres $SS $LresSko)
    (assert-clause $LresSko)
    (findall 
      (with_self  $Uncovered $Proof) 
      (prove4 $Lpar1 $Uncovered $Proof) $Proofs)
    (g1-process-proofs $Proofs $Reslit)
    (g1-build-clause $Reslit $Lpar2Sko)
    (deskolemize $Lpar2Sko $SS $Lpar2)
    (store-clause $_ $Lpar2 $Label $Par2))
; ; clauses in list representation
; ; not a clause with itself
; ; skolemize resolvent
; ; build parent2
; ; deskolemize                 ;
; and store


;
; ********************************************************************************;
;
; *
;
; * predicate: extend_g1/2
;
; *
;
; * syntax: extend_g1(+Ai_ID,-A_Id)
;
; *
;
; * args: clauseIDs
;
; *
;
; * description: locates suitable V's that are already available in the kb
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
; ********************************************************************************;


  (= (extend-g1 $Ai_id $A_id)  
    (get-clause $Ai_id $_ $_ $Ai $_)
    (length $Ai $Li)
    (get-clause $Aj_id $_ $_ $Aj g11)
    (\== $Ai_id $Aj_id)
    (length $Aj $Li)
    (headed-lgg $Ai_id $Aj_id $A_id g1)
    (get-clause $A_id $_ $_ $A $_)
    (length $A $L)
    (det-if-then-else 
      (>= $L $Li) 
      (delete-clause $Aj_id) 
      (, 
        (delete-clause $A_id) 
        (fail))))
; ; heuristic


;
; ***********************************************************************
;
; *
;
; * predicate: apply_g1/2
;
; *
;
; * syntax: apply_g1( + NewClauseId, - List_of_ResultIds )
;
; *
;
; * args:
;
; *
;
; * description: One might want to use apply_g1/2 if a new clause of background
;
; * knowledge is added to the kb and the G1-operator is to be applied.
;
; * If there already is a suitable "V", it will be extended and the
;
; * lgg of two A's will be built.
;
; *
;
; * A
;
; * Bi / \ Bj
;
; * \ Ai Aj /
;
; * \ / \ /
;
; * Ci Cj
;
; *
;
; * If a clause A can be built as lgg of Ai and Aj (extend_g1/2),
;
; * Ai and Aj will be deleted.
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


  (= (apply-g1 $Clause $_)  
    (g1-op $_ $Clause $Par2)
    (findall $GenPar2 
      (extend-g1 $Par2 $GenPar2) $Bag)
    (det-if-then-else 
      (== $Bag Nil) 
      (addtolist $Par2) 
      (, 
        (delete-clause $Par2) 
        (addtolist $Bag)))
    (fail)
    (set-det))
; ; use new clause as parent1
  (= (apply-g1 $Clause $_)  
    (g1-op $Clause $_ $Par2)
    (findall $GenPar2 
      (extend-g1 $Par2 $GenPar2) $Bag)
    (det-if-then-else 
      (== $Bag Nil) 
      (addtolist $Par2) 
      (, 
        (delete-clause $Par2) 
        (addtolist $Bag)))
    (fail))
; ; use new clause as resolvent
  (= (apply-g1 $_ $List)  
    (getlist $List))


;
; ***********************************************************************
;
; *
;
; * predicate: absorb/3
;
; *
;
; * syntax: absorb(+ExID, ?Parent1ID, -NewID)
;
; *
;
; * args: ExID: ID of example clause
;
; * Parent1ID: id of known parent clause
;
; * NewID: ID of absorption of example clause
;
; *
;
; * description: apply one absorption step on input clause ExID;
;
; * if Parent1ID is given, it is tried to perform the
;
; * absorption step with it as known parent.
;
; * Otherwise absorption will be performed with the first
;
; * applicable background clause.
;
; * It is made sure that no 2 literals of a parent
;
; * clause abs_match the same literal in the resolvent.
;
; *
;
; * example:
;
; *
;
; * peculiarities: no inverse subsitution yet
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


;
; parent given

  (= (absorb $ExID $PID $NewID)  
    (nonvar $PID)
    (clear-mngr)
    (get-clause $ExID $_ $_ $Ex $_)
    (skolemize $Ex $S 
      (Cons  
        (with_self  $SHead 
          (p)) $SBody))
    (assert-body $SBody)
    (set-det)
    (abs-match1 $PID success $Proofs)
    (abs-process-proofs $Proofs $PHead)
    (abs-build-body $Body1)
    (append $Body1 
      (:: (with_self  $PHead (n))) $Body)
    (= $NewClauseS 
      (Cons  
        (with_self  $SHead 
          (p)) $Body))
    (deskolemize $NewClauseS $S $NewClause)
    (store-clause $_ $NewClause abs $NewID))



;
; parent not given
  (= (absorb $ExID $PID $NewID)  
    (var $PID)
    (clear-mngr)
    (get-clause $ExID $_ $_ $Ex $_)
    (skolemize $Ex $S 
      (Cons  
        (with_self  $SHead 
          (p)) $SBody))
    (assert-body $SBody)
    (set-det)
    (abs-match $ExID success $Proofs)
    (abs-process-proofs $Proofs $PHead)
    (abs-build-body $Body1)
    (append $Body1 
      (:: (with_self  $PHead (n))) $Body)
    (= $NewClauseS 
      (Cons  
        (with_self  $SHead 
          (p)) $Body))
    (deskolemize $NewClauseS $S $NewClause)
    (store-clause $_ $NewClause abs $NewID))


;
; ***********************************************************************
;
; *
;
; * predicate: abs_match/3
;
; *
;
; * syntax: abs_match(+ExID,-Mark,-Proofs)
;
; *
;
; * args: ExID: Id of the resolvent
;
; * Mark in {success,fail}
;
; * Proofs = [CL,...] where CL is a clause in list notation
;
; *
;
; * description: returns all (instantiated) clauses that can be embedded in the
;
; * skolemized example clause (stored in kb with head/3,body/3)
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


  (= (abs-match $ExID $Mark $Proofs)  
    (det-if-then-else 
      (findall $Proof 
        (abs-match0 $ExID $Proof) $Proofs) 
      (= $Mark success) 
      (, 
        (= $Proofs Nil) 
        (= $Mark fail))))
 

  (= (abs-match0 $ExId (Cons  (with_self  $Goal (p)) $Proof))  
    (get-clause $I $_ $_ 
      (Cons  
        (with_self  $Goal 
          (p)) $Body) usr)
    (\== $I $ExId)
    (prove3 $Body $Proof))


;
; ***********************************************************************
;
; *
;
; * predicate: abs_match1/3
;
; *
;
; * syntax: abs_match1(+PID,-Mark,-Proofs)
;
; *
;
; * args: PID: ID of a parent clause
;
; *
;
; * description: as abs_match, except for the fixed parent clause that
;
; * is embedded in the resolvent
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


  (= (abs-match1 $PID $Mark $Proofs)  
    (det-if-then-else 
      (findall $Proof 
        (abs-match1a $PID $Proof) $Proofs) 
      (= $Mark success) 
      (, 
        (= $Proofs Nil) 
        (= $Mark fail))))


  (= (abs-match1a $PID (Cons  (with_self  $Goal (p)) $Proof))  
    (get-clause $PID $_ $_ 
      (Cons  
        (with_self  $Goal 
          (p)) $Body) $_)
    (prove3 $Body $Proof))
 

;
; ***********************************************************************
;
; *
;
; * predicate: identify/3
;
; *
;
; * syntax: identify(+ExID, ?Parent1ID, -NewID)
;
; *
;
; * args: ExID: ID of example clause
;
; * Parent1ID: id of known parent clause
;
; * NewID: ID of identification of example clause
;
; *
;
; * description: apply one identification step on input clause ExID;
;
; * if Parent1ID is given, it is tried to perform the
;
; * identification step with it as known parent.
;
; * Otherwise identification will be performed with the
;
; * first applicable background clause.
;
; * It is made sure that no 2 literals of a parent
;
; * clause ident_match the same literal in the resolvent.
;
; *
;
; * example:
;
; *
;
; * peculiarities: no inverse subsitution yet
;
; * no backtraking
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************

;
; parent given

  (= (identify $ExID $PID $NewID)  
    (nonvar $PID)
    (clear-mngr)
    (get-clause $ExID $_ $_ $Ex $_)
    (skolemize $Ex $S $SClause)
    (assert-clause $SClause)
    (set-det)
    (ident-match1 $PID success $Proofs)
    (ident-process-proofs $Proofs $PHead)
    (ident-build-body $Body1)
    (= $NewClauseS 
      (Cons  
        (with_self  $PHead 
          (p)) $Body1))
    (deskolemize $NewClauseS $S $NewClause)
    (store-clause $_ $NewClause idn $NewID))



;
; parent not given
  (= (identify $ExID $PID $NewID)  
    (var $PID)
    (get-clause $ExID $_ $_ $Ex $_)
    (clear-mngr)
    (skolemize $Ex $S $SClause)
    (assert-clause $SClause)
    (ident-match $ExID success $Proofs)
    (ident-process-proofs $Proofs $PHead)
    (ident-build-body $Body1)
    (= $NewClauseS 
      (Cons  
        (with_self  $PHead 
          (p)) $Body1))
    (deskolemize $NewClauseS $S $NewClause)
    (store-clause $_ $NewClause idn $NewID))
; ; show_bodies,



;
; ***********************************************************************
;
; *
;
; * predicate: ident_match/3
;
; *
;
; * syntax: ident_match(+ExID,-Mark,-Proofs)
;
; *
;
; * args: ExID: ID of the resolvent
;
; * Mark in {success,fail}
;
; * Proofs = [P1,..,Pm], where Pi=[Uncovered:Proof] and
;
; * Uncovered = Lit/M (M in {new_head,new_body}), Proof = [[Lit,N],...]
;
; * (N in {head,body}
;
; *
;
; * description: matches clauses in kb against skolemized resolvent (stored
;
; * in kb with head/3,body/3), Uncovered is the resolution
;
; * literal that might be positive (new_head) or negative
;
; * (new_body)
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


  (= (ident-match $ExID $Mark $Proofs)  
    (det-if-then-else 
      (findall $Proof 
        (ident-match0 $ExID $Proof) $Proofs) 
      (= $Mark success) 
      (, 
        (= $Proofs Nil) 
        (= $Mark fail))))
 


  (= (ident-match0 $ExId (:: (with_self  $Uncovered $Proof)))  
    (get-clause $I $_ $_ $Clause usr)
    (\== $I $ExId)
    (prove4 $Clause $Uncovered $Proof))


;
; ***********************************************************************
;
; *
;
; * predicate: ident_match1/3
;
; *
;
; * syntax: ident_match1(+PID,-Mark,-Proofs)
;
; *
;
; * args: PID ...parentID, Mark,Proofs as for ident_match
;
; *
;
; * description: as ident_match, except for the parent clause to be matched
;
; * against the resolvent is given
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


  (= (ident-match1 $PID $Mark $Proofs)  
    (det-if-then-else 
      (findall $Proof 
        (ident-match1a $PID $Proof) $Proofs) 
      (= $Mark success) 
      (, 
        (= $Proofs Nil) 
        (= $Mark fail))))



  (= (ident-match1a $PID (:: (with_self  $Uncovered $Proof)))  
    (get-clause $PID $_ $_ $Clause $_)
    (prove4 $Clause $Uncovered $Proof))


;
; ***********************************************************************
;
; *
;
; * predicates: inv_derivate/2/3
;
; *
;
; * syntax: inv_derivate(+ExID,-NewID)
;
; * inv_derivate(+ExID,+PrefHead,-NewID)
;
; *
;
; * args: ExID : id of example
;
; * NewID: id of expanded example
;
; * PrefHead: a MeTTa literal
;
; *
;
; * description: Muggleton's inverse linear derivation
;
; * But:
;
; * while in intermediate stages several head literals
;
; * might appear simultanously, the result will always
;
; * be a Horn clause. As head literal we choose the
;
; * latest one derived in inv_derivate/2.
;
; * inv_derivate/3 takes as additional argument
;
; * a literal, which is interpreted as a preferred
;
; * head. If it is possible, inv_derivate/3 results
;
; * in a Horn clause where the head matches this
;
; * literal.
;
; * The operator is restricted to finding clauses at most
;
; * 100 inverse resolution steps away.
;
; *
;
; * example: inv_derivate(1, member(A,B), ID)
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


  (= (inv-derivate $ExID $NewID)  
    (clear-mngr)
    (or 
      (get-clause $ExID $_ $_ $CList $_) 
      (, 
        (get-example $ExID $Ex +) 
        (= $CList 
          (:: (with_self  $Ex (p))))))
    (skolemize $CList $S $CListS)
    (assert-clause $CListS)
    (inv-derivate1 $ExID 1)
    (idev-build-clause $_ $Clause1)
    (deskolemize $Clause1 $S $Clause)
    (store-clause $_ $Clause invd $NewID)
    (set-det))


  (= (inv-derivate $ExID $PrefHead $NewID)  
    (clear-mngr)
    (get-clause $ExID $_ $_ $CList $_)
    (skolemize $CList $S $CListS)
    (assert-clause $CListS)
    (inv-derivate1 $ExID 1)
    (idev-build-clause $PrefHead $Clause1)
    (deskolemize $Clause1 $S $Clause)
    (store-clause $_ $Clause invd $NewID)
    (set-det))


  (= (inv-derivate1 $ExID $I)  
    (setof 
      (with_self  $U $P) 
      (^ $ExID 
        (^ $Clause 
          (idev-match0 $ExID $Clause $U $P))) $Proofs)
    (process-new-literals $Proofs $Flag)
    (det-if-then-else 
      (, 
        (nonvar $Flag) 
        (< $I 100)) 
      (, 
        (is $J 
          (+ $I 1)) 
        (inv-derivate1 $ExID $J)) True))


;
; ***********************************************************************
;
; *
;
; * predicate: idev_match0/4
;
; *
;
; * syntax: idev_match0(+ExID,-Clause,-Uncovered,-Proof)
;
; *
;
; * args: ExID: ID of the resolvent
;
; * Clause: clause in list notation
;
; * Uncovered: Lit/M, where M in {new_head,new_body}
;
; * Proof: [[Lit,N],...] where N in {head,body}
;
; *
;
; * description: matches clause on skolemized resolvent (stored in kb
;
; * with head/3, body/3), and returns the instantiation
;
; * of clause and the resolution literal (Uncovered)
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


  (= (idev-match0 $ExID $Clause $Uncovered $Proof)  
    (get-clause $ID $_ $_ $Clause $_)
    (\== $ExID $ID)
    (prove4 $Clause $Uncovered $Proof))


;
; ***********************************************************************
;
; *
;
; * predicate: most_spec_v/3
;
; *
;
; * syntax: most_spec_v(+ExID, ?PID, -NewID)
;
; *
;
; * args: ExID: id of example (resolvent)
;
; * PID: id of known parent
;
; * NewID: id of new clause
;
; *
;
; * description:
;
; * Apply one most-spec-v operation to example with parent PID;
;
; * If PID is not given, take the first applicable clause
;
; * of bg as parent.
;
; * The most specific v comprises the most specific absorption
;
; * and the most specific identification.
;
; * Since we always want Horn clauses as a result, this operator
;
; * does not implement the most specific identification as
;
; * described by Muggleton: Instead of adding a second head
;
; * literal to the old clause, we replace the original head.
;
; * I.e. our most specific identification operator is destructive.
;
; * The most specific absorption remains nondestructive
;
; *
;
; * example:
;
; *
;
; * peculiarites:
;
; *
;
; * see also: inv_derivate/2 where multiple head literals are
;
; * allowed om intermediate stages.
;
; *
;
; ***********************************************************************


  (= (most-spec-v $ExID $PID $NewID)  
    (or 
      (get-clause $ExID $_ $_ $CList $_) 
      (, 
        (get-example $ExID $Ex +) 
        (= $CList 
          (:: (with_self  $Ex (p))))))
    (clear-mngr)
    (skolemize $CList $S $CListS)
    (assert-clause $CListS)
    (idev-match1 $ExID $_ $Uncovered $Proof $PID)
    (process-new-literals 
      (:: (with_self  $Uncovered $Proof)) $Flag)
    (nonvar $Flag)
    (idev-build-clause $_ $Clause1)
    (deskolemize $Clause1 $S $Clause)
    (store-clause $_ $Clause msv $NewID))
; ; Uncovered \== [],



;
; ***********************************************************************
;
; *
;
; * predicate: idev_match1/5
;
; *
;
; * syntax: idev_match1(+ExID,-Clause,-Uncovered,-Proof,-ID)
;
; *
;
; * args: ExID: ID of the resolvent, ID: ID of matched clause
;
; * Clause: clause in list notation
;
; * Uncovered: Lit/M, where M in {new_head,new_body}
;
; * Proof: [[Lit,N],...] where N in {head,body}
;
; *
;
; * description: is like idev_match0/4, but returns id of absorbed clause
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


  (= (idev-match1 $ExID $Clause $Uncovered $Proof $ID)  
    (get-clause $ID $_ $_ $Clause $_)
    (\== $ExID $ID)
    (prove4 $Clause $Uncovered $Proof))


;
; ***********************************************************************
;
; *
;
; * predicate: saturate/2,saturate/3
;
; *
;
; * syntax: saturate(+ExID, -NewID), saturate(+ExID,-NewID,+Bound)
;
; *
;
; * args: ExID: ID of example clause
;
; * NewID: ID of saturation of example clause
;
; *
;
; * description: apply elementary saturation w.r.t. background
;
; * clauses.
;
; * It is bounded by at most 100 iterations, if bound is not given
;
; * When checking the preconditions for firing one
;
; * absorption step,
;
; * it is made sure that no 2 literals of a parent
;
; * clause subsume the same literal in the resolvent.
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


  (= (saturate $ExID $GenID)  
    (saturate $ExID $GenID 100))

  (= (saturate $ExID $GenID $Bound)  
    (saturate1 $ExID $NewClause $Bound)
    (store-clause $_ $NewClause sat $GenID)
    (set-det))
; ; Rouveirol's theorem proving alg.
; ; write(NewClause),nl,      ;
; no backtracking
    


;
; ***********************************************************************
;
; *
;
; * predicate: saturate1/3
;
; *
;
; * syntax: saturate1(+ExID,-NewClause,+Bound)
;
; *
;
; * args: ExID .. ID of example clause, NewClause .. MeTTa clause in list notation,
;
; * Bound .. bound for interations
;
; *
;
; * description: saturates example clause w.r.t. background knowledge.
;
; * It is bounded by at most Bound interations.
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


  (= (saturate1 $ExID $NewClause $Bound)  
    (clear-mngr)
    (get-clause $ExID $H $B $T $_)
    (skolemize $T $S 
      (Cons  
        (with_self  $HS 
          (p)) $U))
    (assert-body $U)
    (saturate1a $HS 1 $Bound $S $S1)
    (bagof $A 
      (^ $M 
        (^ $I 
          (body $A $I $M))) $NewBody1)
    (sat-build-clause $H $NewBody1 $Clause1)
    (deskolemize $Clause1 $S1 $NewClause))
; ; Clause in list notation
        

;
; ***********************************************************************
;
; *
;
; * predicate: saturate1a/5
;
; *
;
; * syntax: saturate1a(+HS,+Count,+Bound,+Subst,-Subst)
;
; *
;
; * args: HS: skolemized head of the example clause,
;
; * Count,Bound: integers
;
; * Subst: skolem subtitutions
;
; *
;
; * description: while Count < Bound, all heads following from the saturated
;
; * clause so far (stored as body(Lit,_,_)) are asserted as
;
; * additional body-literals (via body/3)
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


  (= (saturate1a $HS $I $Bound $S1 $S2)  
    (sat-match $HS success $Proofs)
    (set-det)
    (skolemize $Proofs $S1 $S3 $Proofs1)
    (assert-absorptions $Proofs1 $Flag)
    (det-if-then-else 
      (, 
        (nonvar $Flag) 
        (< $I $Bound)) 
      (, 
        (is $J 
          (+ $I 1)) 
        (saturate1a $HS $J $Bound $S3 $S2)) 
      (= $S2 $S3)))


  (= (saturate1a $_ $_ $_ $S $S)  
    (set-det))


;
; ***********************************************************************
;
; *
;
; * predicate: sat_match/3
;
; *
;
; * syntax: sat_match(+HS,-M,-Proofs)
;
; *
;
; * args: HS: skolemized head of the example clause
;
; * Proofs = [CL,...] where each CL is a clause in list notation
;
; *
;
; * description: finds all possible proofs for all possible absorptions
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


  (= (sat-match $HS $Mark $Proofs)  
    (det-if-then-else 
      (setof $Proof 
        (^ $HS 
          (sat-match0 $HS $Proof)) $Proofs) 
      (= $Mark success) 
      (, 
        (= $Proofs Nil) 
        (= $Mark fail))))


  (= (sat-match0 $HS (Cons  (with_self  $Goal (p)) $ProofBody))  
    (get-clause $I $_ $_ 
      (Cons  
        (with_self  $Goal 
          (p)) $Body) $_)
    (prove3 $Body $ProofBody)
    (\== $Goal $HS))


;
; ***********************************************************************
;
; *
;
; * predicate: elem_saturate/3
;
; *
;
; * syntax: elem_saturate( +ExID, ?PID, -NewID)
;
; *
;
; * args: ExID: id of resolvent
;
; * PID : id of parent in bg
;
; * NewID: id of new parent
;
; *
;
; * description:
;
; * Add head of parent from bg to body of resolvent.
;
; * The Operator is identical to Muggleton's
;
; * most-specific-absorption.
;
; * When checking the preconditions for firing one
;
; * absorption step,
;
; * it is made sure that no 2 literals of a parent
;
; * clause subsume the same literal in the resolvent.
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

;
; parent given

  (= (elem-saturate $ExID $PID $NewID)  
    (nonvar $PID)
    (clear-mngr)
    (get-clause $ExID $_ $_ 
      (Cons  
        (with_self  $H 
          (p)) $B) $_)
    (skolemize $B $S $BS)
    (assert-body $BS)
    (get-clause $PID $_ $_ 
      (Cons  
        (with_self  $Goal 
          (p)) $Body) $_)
    (prove3 $Body $ProofBody)
    (assert-absorptions 
      (:: (Cons  (with_self  $Goal (p)) $ProofBody)) $Flag)
    (nonvar $Flag)
    (findall $L 
      (body $L $I $M) $NewBody)
    (sat-build-clause $H $NewBody $Clause1)
    (deskolemize $Clause1 $S $NewClause)
    (store-clause $_ $NewClause esat $NewID))


;
; parent not given
  (= (elem-saturate $ExID $PID $NewID)  
    (var $PID)
    (clear-mngr)
    (get-clause $ExID $_ $_ 
      (Cons  
        (with_self  $H 
          (p)) $B) $_)
    (skolemize $B $S $BS)
    (assert-body $BS)
    (sat-match1 $ExID 
      (Cons  
        (with_self  $Goal 
          (p)) $ProofBody) $PID)
    (assert-absorptions 
      (:: (Cons  (with_self  $Goal (p)) $ProofBody)) $Flag)
    (nonvar $Flag)
    (findall $L 
      (body $L $I $M) $NewBody)
    (sat-build-clause $H $NewBody $Clause1)
    (deskolemize $Clause1 $S $NewClause)
    (store-clause $_ $NewClause esat $NewID))


;
; ***********************************************************************
;
; *
;
; * predicate: sat_match1/3
;
; *
;
; * syntax: sat_match1(+ExID,-Proof,-ID)
;
; *
;
; * args: ExID,ID: clauseIDs, Proofs: clause in list notation
;
; *
;
; * description: is like sat_match0/2, but returns id of absorbed clause
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


  (= (sat-match1 $Ex (Cons  (with_self  $Goal (p)) $ProofBody) $I)  
    (get-clause $I $_ $_ 
      (Cons  
        (with_self  $Goal 
          (p)) $Body) $_)
    (\== $I $Ex)
    (prove3 $Body $ProofBody))


