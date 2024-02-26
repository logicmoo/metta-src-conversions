;                                           (convert_to_metta_file  g2_ops $_234856 miles/g2_ops.pl miles/g2_ops.metta)
;
; MODULE g2_ops EXPORTS

  !(module g2-ops 
    (:: 
      (/ intra-construct1 5) 
      (/ intra-construct1 6) 
      (/ intra-construct1 7) 
      (/ intra-construct2 5) 
      (/ intra-construct2 6) 
      (/ intra-construct2 7) 
      (/ g2-op 5) 
      (/ apply-g2 5) 
      (/ apply-g2 3) 
      (/ apply-g2 2)))
; ; ITOU-like
; ; give name of new pred
; ; give name & bound for common generalizations
; ; CIGOL-like
; ; give name of new pred
; ; give name & bound for common generalizations

;
; IMPORTS
  !(use-module 
    (home lgg) 
    (:: 
      (/ lgti 6) 
      (/ lgg 5) 
      (/ buildlgg 4) 
      (/ gti 5) 
      (/ lgti 5))) ;
; ;;diese f"ur lgti/6 ersetzen (ohne Bound)
  !(use-module 
    (home kb) 
    (:: 
      (/ get-clause 5) 
      (/ store-clause 4) 
      (/ delete-clause 1) 
      (/ delete-all 1)))
  !(use-module 
    (home var-utils) 
    (:: 
      (/ relevant-vars2 6) 
      (/ relevant-vars3 6) 
      (/ skolemize 3) 
      (/ deskolemize 3) 
      (/ replace 4) 
      (/ inv-replace 4) 
      (/ exists-intersect 3) 
      (/ findargs 3) 
      (/ allarg 4) 
      (/ buildrelterms 6)))
  !(use-module 
    (home div-utils) 
    (:: 
      (/ effaceall 3) 
      (/ genterm-test 2)))
  !(use-module 
    (home g1-ops) 
    (:: (/ g1-op 4)))
  !(use-module 
    (home environment) 
    (:: 
      (/ oracle 2) 
      (/ confirm 2) 
      (/ get-ci 2)))
  !(use-module 
    (home evaluation) 
    (:: (/ complexity 2)))
  !(use-module-if-exists 
    (library strings) 
    (:: (/ gensym 2)))
  !(use-module-if-exists 
    (library basics) 
    (:: (/ member 2)))
  !(use-module-if-exists 
    (library sets) 
    (:: (/ subtract 3)))
  !(use-module-if-exists 
    (library not) 
    (:: (/ once 1)))


;
; METAPREDICATES
;
; none


;
; ***********************************************************************
;
; *
;
; * module: g2_ops.pl
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
; * description: Intra-Construction, G2
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
; * predicate: intra_construct1/5/6/7
;
; *
;
; * syntax: intra_construct1(+C1,+C2,-A,-B1,-B2)
;
; * intra_construct1(+C1,+C2,-A,-B1,-B2,PName)
;
; * intra_construct1(+C1,+C2,-A,-B1,-B2,PName,Bound)
;
; *
;
; * args: C1,C2,A,B1,B2: references to clauses in kb
;
; * PName: atom - name of invented predicate
;
; * Bound: integer
;
; *
;
; * description: intra-construction where C1,C2 are at bottom of W,
;
; * A at the center top, B1,B2 at outside top positions.
;
; * S1(A) in C1, S2(A) in C2; the substitutions
;
; * between Bi & Ci are empty.
;
; * Uses an ITOU-like heursitics for determining relevant
;
; * variables for the new predicate
;
; * Our intra-construction will only work, if the two
;
; * input clauses require the same number of arguments
;
; * for the newly invented predicate.
;
; * This restriction is not part of the original
;
; * definition of intra-construction, but its at least
;
; * a very useful heuristics. If the restriction does not
;
; * fit your needs, change it in relevant_vars/3,
;
; * module "var_utils.pl".
;
; *
;
; * example:
;
; *
;
; * peculiarities: backtracking over lgti/6 until the same number
;
; * of arguments is reached.
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (intra-construct1 $IDC1 $IDC2 $IDA $IDB1 $IDB2)  
    (gensym new-pred $NewPred)
    (intra-construct1 $IDC1 $IDC2 $IDA $IDB1 $IDB2 $NewPred))


  (= (intra-construct1 $IDC1 $IDC2 $IDA $IDB1 $IDB2 $NewPred)  
    (intra-construct1 $IDC1 $IDC2 $IDA $IDB1 $IDB2 $NewPred 10)) 

  (= (intra-construct1 $IDC1 $IDC2 $IDA $IDB1 $IDB2 $NewPred $Bound)  
    (atom $NewPred)
    (get-clause $IDC1 $_ $_ $C1 $_)
    (get-clause $IDC2 $_ $_ $C2 $_)
    (set-det)
    (lgti $C1 $C2 $G $S1 $S2 $Bound)
    (once (, (relevant-vars3 $C1 $C2 $G $S1 $S2 $Vars) (=.. $NewLit (Cons  $NewPred $Vars)) (append $G (:: (with_self  $NewLit (n))) $A) (copy-term (, $C1 $G $S1 $NewLit) (, $C11 $G11 $S11 $NewLit11)) (skolemize (, $C11 $G11 $S11 $NewLit11) $Phi1 (, $C12 $G12 $S12 $NewLit12)) (replace $G12 $S12 $G13 $S12) (replace (:: $NewLit12) $S12 (:: $NewLit13) $S12) (subtract $C12 $G13 $B1BodyS) (= $B1S (Cons  (with_self  $NewLit13 (p)) $B1BodyS)) (deskolemize $B1S $Phi1 $B1) (copy-term (, $C2 $G $S2 $NewLit) (, $C21 $G21 $S21 $NewLit21)) (skolemize (, $C21 $G21 $S21 $NewLit21) $Phi2 (, $C22 $G22 $S22 $NewLit22)) (replace $G22 $S22 $G23 $S22) (replace (:: $NewLit22) $S22 (:: $NewLit23) $S22) (subtract $C22 $G23 $B2BodyS) (= $B2S (Cons  (with_self  $NewLit23 (p)) $B2BodyS)) (deskolemize $B2S $Phi2 $B2) (store-clause $_ $A ic $IDA) (store-clause $_ $B1 ic $IDB1) (store-clause $_ $B2 ic $IDB2))))
; ; G is common generalization
; ;;; ITOU - like
; ; build clause in center top
; ; build clause at top left
; ;
; ; build clause at top right        



;
; ***********************************************************************
;
; *
;
; * predicate: intra_construct2/5/6/7
;
; *
;
; * syntax: intra_construct2(+C1,+C2,-A,-B1,-B2)
;
; * intra_construct2(+C1,+C2,-A,-B1,-B2,PName)
;
; * intra_construct2(+C1,+C2,-A,-B1,-B2,PName,Bound)
;
; *
;
; * args: C1,C2,A,B1,B2: references to clauses in kb
;
; * PName: atom - name of invented predicate
;
; * Bound: integer
;
; *
;
; * description: intra-construction where C1,C2 are at bottom of W,
;
; * A at the center top, B1,B2 at outside top positions.
;
; * S1(A) in C1, S2(A) in C2; the substitutions
;
; * between Bi & Ci are empty.
;
; * Uses a CIGOL-like heursitics for determining relevant
;
; * variables for the new predicate
;
; *
;
; * example:
;
; *
;
; * peculiarities: backtracking over lgti/6 until the same number
;
; * of arguments is reached.
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (intra-construct2 $IDC1 $IDC2 $IDA $IDB1 $IDB2)  
    (gensym new-pred $NewPred)
    (intra-construct2 $IDC1 $IDC2 $IDA $IDB1 $IDB2 $NewPred))


  (= (intra-construct2 $IDC1 $IDC2 $IDA $IDB1 $IDB2 $NewPred)  
    (intra-construct2 $IDC1 $IDC2 $IDA $IDB1 $IDB2 $NewPred 10)) 

  (= (intra-construct2 $IDC1 $IDC2 $IDA $IDB1 $IDB2 $NewPred $Bound)  
    (atom $NewPred)
    (get-clause $IDC1 $_ $_ $C1 $_)
    (get-clause $IDC2 $_ $_ $C2 $_)
    (set-det)
    (lgti $C1 $C2 $G $S1 $S2 $Bound)
    (once (, (relevant-vars2 $C1 $C2 $G $S1 $S2 $Vars) (=.. $NewLit (Cons  $NewPred $Vars)) (append $G (:: (with_self  $NewLit (n))) $A) (copy-term (, $C1 $G $S1 $NewLit) (, $C11 $G11 $S11 $NewLit11)) (skolemize (, $C11 $G11 $S11 $NewLit11) $Phi1 (, $C12 $G12 $S12 $NewLit12)) (replace $G12 $S12 $G13 $S12) (replace (:: $NewLit12) $S12 (:: $NewLit13) $S12) (subtract $C12 $G13 $B1BodyS) (= $B1S (Cons  (with_self  $NewLit13 (p)) $B1BodyS)) (deskolemize $B1S $Phi1 $B1) (copy-term (, $C2 $G $S2 $NewLit) (, $C21 $G21 $S21 $NewLit21)) (skolemize (, $C21 $G21 $S21 $NewLit21) $Phi2 (, $C22 $G22 $S22 $NewLit22)) (replace $G22 $S22 $G23 $S22) (replace (:: $NewLit22) $S22 (:: $NewLit23) $S22) (subtract $C22 $G23 $B2BodyS) (= $B2S (Cons  (with_self  $NewLit23 (p)) $B2BodyS)) (deskolemize $B2S $Phi2 $B2) (store-clause $_ $A ic $IDA) (store-clause $_ $B1 ic $IDB1) (store-clause $_ $B2 ic $IDB2))))
; ; G is common generalization
; ;;; CIGOL - like
; ; build clause in center top
; ; build clause at top left
; ;
; ; build clause at top right        


;
; ********************************************************************************
;
; *
;
; * predicate: g2_op/5
;
; *
;
; * syntax: g2_op ( + C1_ID, + C2_ID, - A_ID, - B1_ID, - B2_ID)
;
; *
;
; * args: Ci_ID ... IDs of resolvent clauses C1 and C2 to be generalized
;
; * A_ID ... ID of common parent clause A
;
; * Bi_ID ... IDs of corresponding parent clauses B1 and B2
;
; *
;
; * description: Implementation of Ruediger Wirth's G2-operator for inverse
;
; * resolution corresponding to his 1989 PhD thesis.
;
; * We generalize the Ci using Plotkin's LGG, then build a new
;
; * predicate as resolution literal, find the argument terms for the
;
; * new predicate (in a heuristic manner) and finally build the Bi
;
; * using our well-known G1-operator.
;
; * The compression achieved is evaluated thru a simple, though quite
;
; * sophisticated complexity heuristic (cf. module 'complexity').
;
; * If the resulting clauses show some compression, the are passed
;
; * to the oracle for confirmation and the user gets a chance to
;
; * rename the new predicate.
;
; * Clauses which become obsolete during the process will be deleted.
;
; *
;
; * example:
;
; *
;
; * peculiarities: The procedure 'inv_replace' might yield unsatisfying results,
;
; * due to the possible ambiguity of inverse substitution.
;
; *
;
; * see also:
;
; *
;
; ********************************************************************************


  (= (g2-op $C1 $C2 $A $B1 $B2)  
    (get-clause $C1 $_ $_ $C1list $_)
    (get-clause $C2 $_ $_ $C2list $_)
    (lgg $C1list $C2list $Clgg $S1 $S2)
    (not-unary $Clgg)
    (buildrelterms $C1list $C2list $Clgg $S1 $S2 $Terms)
    (buildreslit $Terms $L)
    (buildparentA $Clgg $L $Alist)
    (store-clause $_ $Alist g2 $A)
    (det-if-then-else 
      (, 
        (g1-op $C1 $A $B1 g2g1) 
        (g1-op $C2 $A $B2 g2g1)) 
      (det-if-then-else 
        (compression-heuristic 
          (:: $A $B1 $B2) 
          (:: $C1 $C2)) 
        (, 
          (confirm 
            (:: $A $B1 $B2) $L) 
          (delete-all (:: $C1 $C2)) 
          (nl) 
          (write 'Resolvent clauses deleted.')) 
        (, 
          (nl) 
          (write 'G2: No compression achieved.') 
          (nl) 
          (fail))) 
      (delete-clause $A)))
; ; Clgg = A\{L}
; ; heuristic proc.
; ; Terms = List of Args for L
; ; L = resolution literal
; ; Alist = parentclause A
; ; Bi = parentclauses


;
; ********************************************************************************
;
; *
;
; * predicate: apply_g2/3 - tries to apply the G2-operator to a set of clauses Ci.
;
; * The output will be a kb reference of the common parent
;
; * clause A and a list of id's for parent clauses Bi.
;
; *
;
; * Bi A Bj
;
; * \ / \ /
;
; * \ / \ /
;
; * Ci Cj
;
; * apply_g2/2 - ORACLE is asked to enter the Id's of resolvent clauses
;
; * Ci one by one. This continues until oracle says 'stop'.
;
; * Doubles and answers which are not a number are ignored.
;
; * Finally apply_g2/3 is called.
;
; * apply_g2/5 - simply calls g2_op/5
;
; *
;
; * syntax: apply_g2( + CC, - A, -BB), apply_g2( - A, -BB),
;
; * apply_g2 ( + C1_ID, + C2_ID, - A_ID, - B1_ID, - B2_ID)
;
; *
;
; * args: CC ... Id-list of resolvent clauses Ci to be generalized
;
; * A ... Id of common parent clause A
;
; * BB ... Id-list of corresponding parent clauses Bi
;
; * C_ID, A_ID, B_ID .. as for g2_op/5
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
; * peculiarities:
;
; *
;
; * see also:
;
; *
;
; ********************************************************************************;


  (= (apply-g2 $C1 $C2 $A $B1 $B2)  
    (g2-op $C1 $C2 $A $B1 $B2)
    (set-det))


  (= (apply-g2 $A $BB)  
    (get-ci Nil $CC)
    (apply-g2 $CC $A $BB)
    (set-det))


  (= (apply-g2 $CC $A $BB)  
    (sort $CC $CCsort)
    (gensym new-p $N)
    (findall $Aij 
      (, 
        (member $Ci $CCsort) 
        (member $Cj $CCsort) 
        (< $Ci $Cj) 
        (g2-op-A $Ci $Cj $N $Aij)) $AA)
    (= $AA 
      (Cons  $A1 $An))
    (buildlgg $An $A1 $A g2)
    (delete-all $AA)
    (findall $Bi 
      (, 
        (member $Ci $CC) 
        (g1-op $Ci $A $Bi g2g1)) $BB)
    (length $CC $NoC)
    (det-if-then-else 
      (length $BB $NoC) 
      (det-if-then-else 
        (compression-heuristic 
          (Cons  $A $BB) $CC) 
        (, 
          (confirm 
            (Cons  $A $BB) $N) 
          (delete-all $CC) 
          (nl) 
          (write 'Resolvent clauses deleted.')) 
        (, 
          (nl) 
          (write 'G2: No compression achieved.') 
          (nl) 
          (fail))) 
      (, 
        (delete-all $BB) 
        (delete-clause $A) 
        (fail))))

	

  (= (g2-op-A $C1 $C2 $Name $A)  
    (get-clause $C1 $_ $_ $C1list $_)
    (get-clause $C2 $_ $_ $C2list $_)
    (lgg $C1list $C2list $Clgg $S1 $S2)
    (buildrelterms $C1list $C2list $Clgg $S1 $S2 $T)
    (not-unary $Clgg)
    (length $T $N)
    (functor $L $Name $N)
    (setargs $N $T $L)
    (buildparentA $Clgg $L $Alist)
    (store-clause $_ $Alist g2 $A))
; ; Clgg = A\{L}
; ; T = List of Args for L
; ; heuristic proc.
; ; Name = common for all L's
; ; L = resolution literal
; ; parentclause A



;
; ************************************************************************
;
; *
;
; * predicate: not_unary/1
;
; *
;
; * syntax: not_unary(+CL)
;
; *
;
; * args: CL .. clause in list representation
;
; *
;
; * description: fails, if CL is a unary clause or a unary goal
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


  (= (not-unary (:: (with_self  $_ (p))))  
    (nl)
    (write 'No compression achievable.')
    (set-det)
    (fail))
  (= (not-unary (:: (with_self  (True *) (p)) $_))  
    (nl)
    (write 'No compression achievable.')
    (set-det)
    (fail))
  (= (not_unary  $_)  True)
	

;
; ************************************************************************
;
; *
;
; * predicate: buildreslit/2
;
; *
;
; * syntax: buildreslit(+TermList,-Lit)
;
; *
;
; * args: TermList is the list of relevant argument terms, Lit is the resolution
;
; * literal (with a new predicate symbol)
;
; *
;
; * description: constructs the resolution literal
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


  (= (buildreslit $T $L)  
    (length $T $N)
    (gensym new-p $F)
    (functor $L $F $N)
    (setargs $N $T $L))


  (= (setargs 0 Nil $_)  
    (set-det))
  (= (setargs $N (Cons  $Arg1 $Rest) $L)  
    (arg $N $L $Arg1)
    (is $M 
      (- $N 1))
    (setargs $M $Rest $L))


;
; ************************************************************************
;
; *
;
; * predicate: buildparentA/3
;
; *
;
; * syntax: buildparentA(+A_L,+Lit,-AL)
;
; *
;
; * args: A_L ... A\{Lit} the lgg of C1 and C2
;
; * Lit ... the new predicate literal
;
; * AL ... A\{Lit} + {Lit}
;
; *
;
; * description: adds the new predicate literal Lit either as head or
;
; * as body literal to A_L
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


  (= (buildparentA (Cons  (with_self  (True *) (p)) $Rest) $L (Cons  (with_self  $L (p)) $Rest))  
    (set-det))
  (= (buildparentA $List $L $Alist)  
    (append $List 
      (:: (with_self  $L (n))) $Alist)
    (set-det))


;
; ************************************************************************
;
; *
;
; * predicate: compression_heuristic/2
;
; *
;
; * syntax: compression_heuristic(+NewIDs,+OldIDs)
;
; *
;
; * args: NewIDs, OldIDs ... clauseIDs
;
; *
;
; * description: succeeds if the size of the clauses NewIDs is smaller
;
; * than that of the clauses OldIDs
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


  (= (compression-heuristic $New_cl $Old_cl)  
    (complexity $Old_cl $Cold)
    (complexity $New_cl $Cnew)
    (det-if-then-else 
      (< $Cnew $Cold) True 
      (, 
        (delete-all $New_cl) 
        (fail))))

