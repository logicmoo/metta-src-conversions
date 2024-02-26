;                                           (convert_to_metta_file  newpred $_27198 miles/newpred.pl miles/newpred.metta)
;
; MODULE newpred EXPORTS
  !(module newpred 
    (:: 
      (/ specialize-with-newpred 5) 
      (/ specialize-with-newpred 7) 
      (/ specialize-with-newpred 2) 
      (/ is-newpred 1)))


;
; IMPORTS
  !(use-module 
    (home kb) 
    (:: 
      (/ get-clause 5) 
      (/ get-evaluation 2) 
      (/ delete-clause 1) 
      (/ store-clause 4) 
      (/ store-ex 3)))
  !(use-module 
    (home var-utils) 
    (:: (/ only-vars 2)))
  !(use-module 
    (home div-utils) 
    (:: (/ mysetof 3)))
  !(use-module 
    (home td-basic) 
    (:: (/ append-body 3)))
  !(use-module 
    (home interpreter) 
    (:: (/ prooftrees 3)))
  !(use-module 
    (home environment) 
    (:: (/ ask-for-ex 1)))
  !(use-module 
    (home argument-types) 
    (:: 
      (/ types-of 3) 
      (/ type-restriction 2)))
  !(use-module-if-exists 
    (library basics) 
    (:: (/ member 2)))
  !(use-module-if-exists 
    (library sets) 
    (:: (/ intersection 3)))
  !(use-module-if-exists 
    (library strings) 
    (:: (/ gensym 2)))

;
; METAPREDICATES
;
; none


;
; ***********************************************************************
;
; *
;
; * module: newpred.pl
;
; *
;
; * author: I.Stahl
;
; *
;
; * changed:
;
; *
;
; *
;
; * description:
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
; * predicate: specialize_with_newpred/1
;
; *
;
; * syntax: specialize_with_newpred(+ID)
;
; *
;
; * args: ID .. Clause ID
;
; *
;
; * description:
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (specialize-with-newpred $ID (, $ID $L))  
    (mysetof 
      (, $NC $Pos $Neg $TR) 
      (specialize-with-newpred $ID $NC $Pos $Neg $TR) $L))
    

;
; ***********************************************************************
;
; *
;
; * predicate: specialize_with_newpred/5
;
; *
;
; * syntax: specialize_with_newpred(+ID,-Newclause,-Pos,-Neg,-Typerestriction)
;
; *
;
; * args: ID .. Clause ID, Newclause.. specialized clause
;
; * Pos.. positive examples for the new predicate
;
; * Neg.. negative examples for the new predicate
;
; * Typerestriction.. type restriction for the new predicate
;
; *
;
; * description:
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************

  (= (specialize-with-newpred $ID $NC $P $N (type-restriction $Newp2 $TR))  
    (get-clause $ID $H $B $_ $_)
    (get-evaluation $ID 
      (evaluation $_ $_ $Pos $_ $Neg $_ $_ $_ $_))
    (det-if-then-else 
      (or 
        (= $Pos Nil) 
        (= $Neg Nil)) fail 
      (, 
        (only-vars 
          (, $H $B) $Vars) 
        (types-of $Vars 
          (= $H  $B) $TVars) 
        (clause-instances $Pos $ID $H $B $Vars $PV) 
        (clause-instances $Neg $ID $H $B $Vars $NV) 
        (reduce-newpred-args $Vars $Vars $PV $NV $Vars1 $P0 $N0) 
        (gensym newp $X) 
        (=.. $Newp 
          (Cons  $X $Vars1)) 
        (make-newp-ex $P0 $X $P) 
        (make-newp-ex $N0 $X $N) 
        (append-body 
          (= $H  $B) $Newp $NC) 
        (copy-term 
          (, $Vars1 $TVars $Newp) 
          (, $Vars2 $TVars2 $Newp2)) 
        (make-type-restriction $Vars2 $TVars2 $TR))))



;
; ***********************************************************************
;
; *
;
; * predicate: specialize_with_newpred/7
;
; *
;
; * syntax: specialize_with_newpred(+Clause,+CPos,+CNeg,-Newclause,
;
; * -Pos,-Neg,-Typerestriction)
;
; *
;
; * args: Clause.. clause to be specialised with new predicate
;
; * CPos,CNeg.. pos./neg. examples covered by the clause
;
; * Newclause.. specialized clause
;
; * Pos.. positive examples for the new predicate
;
; * Neg.. negative examples for the new predicate
;
; * Typerestriction.. type restriction for the new predicate
;
; *
;
; * description:
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************

  (= (specialize-with-newpred (= $H  $B) $Pos $Neg $NC $P $N (type-restriction $Newp2 $TR))  
    (only-vars 
      (, $H $B) $Vars)
    (types-of $Vars 
      (= $H  $B) $TVars)
    (clause-instances $Pos $ID $H $B $Vars $PV)
    (clause-instances $Neg $ID $H $B $Vars $NV)
    (reduce-newpred-args $Vars $Vars $PV $NV $Vars1 $P0 $N0)
    (gensym newp $X)
    (=.. $Newp 
      (Cons  $X $Vars1))
    (make-newp-ex $P0 $X $P)
    (make-newp-ex $N0 $X $N)
    (append-body 
      (= $H  $B) $Newp $NC)
    (copy-term 
      (, $Vars1 $TVars $Newp) 
      (, $Vars2 $TVars2 $Newp2))
    (make-type-restriction $Vars2 $TVars2 $TR))
 


;
; ***********************************************************************
;
; *
;
; * predicate: clause_instances/5
;
; *
;
; * syntax: clause_instances(+Covered,+ID,+Head,+Body,+Vars,-Varinstances)
;
; *
;
; * args: Covered.. examples covered by clause ID
;
; * ID .. clauseID
;
; * Head,Body.. of clause ID, Vars.. variables of clause ID
;
; * Varinstances.. instantiations of the clause variables according
;
; * to Covered. If Vars = [V1,..,Vn] and |Covered| = m, then
;
; * Varinstances = [[I11,..,I1n],..,[Im1,..,Imn]]
;
; *
;
; * description:
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (clause_instances  () $_ $_ $_ $_ ())  True)
  (= (clause-instances (Cons  (with_self  $ID $Ex) $R) $IDC $H $B $Vars (Cons  $Vars1 $R1))  
    (clause-instances $R $IDC $H $B $Vars $R1)
    (copy-term 
      (, $H $B $Vars) 
      (, $Ex $B1 $Vars1))
    (prooftrees $ID success $Proofs)
    (setof $PBody 
      (member 
        (:: $IDC $Ex $PBody) $Proofs) $Bodies)
    (body-instances $Bodies $B1))


  (= (body_instances  () $_)  True)
  (= (body-instances (Cons  $B $R) $B1)  
    (body-instances $R $B1)
    (body-inst $B $B1))


  (= (body-inst Nil True)  
    (set-det))
  (= (body-inst (Cons  (:: $_ $B $_) $R) (, $B $R1))  
    (set-det)
    (det-if-then-else 
      (not (ground $B)) 
      (ask-for-ex $B) True)
    (body-inst $R $R1))
  (= (body-inst (:: (:: $_ $B $_)) $B)  
    (det-if-then-else 
      (not (ground $B)) 
      (ask-for-ex $B) True))

   
;
; ***********************************************************************
;
; *
;
; * predicate: reduce_newpred_args/7
;
; *
;
; * syntax: reduce_newpred_args(+Vars,+Vars,+PCovered,+Ncovered,
;
; * -Vars,-PCovered,-NCovered)
;
; *
;
; * args: Vars.. argument variables of the new predicate, to be
;
; * reduced
;
; * PCovered,NCovered.. Instantiations of these argument
;
; * variables according to the Pos/Neg examples covered
;
; * by the clause
;
; *
;
; * description: discrimination based reduction
;
; *
;
; * see also: CHAMP/DBC
;
; *
;
; ***********************************************************************


  (= (reduce_newpred_args  () $Vars $PVars $NVars $Vars $PVars $NVars)  True)
  (= (reduce-newpred-args (Cons  $X $R) $Vars $P $N $Vars2 $P2 $N2)  
    (remove-arg $X $Vars $Vars1 $P $P1 $N $N1)
    (intersection $P1 $N1 Nil)
    (reduce-newpred-args $R $Vars1 $P1 $N1 $Vars2 $P2 $N2))
  (= (reduce-newpred-args (Cons  $_ $R) $Vars $P $N $Vars2 $P2 $N2)  
    (reduce-newpred-args $R $Vars $P $N $Vars2 $P2 $N2))


  (= (remove-arg $X $Vars $Vars1 $P $P1 $N $N1)  
    (rem-arg $X $Vars $Vars1 1 $Pos)
    (set-det)
    (rem-ins $P $Pos $P1)
    (rem-ins $N $Pos $N1))


  (= (rem-arg $X (Cons  $Y $R) $R $Pos $Pos)  
    (== $X $Y)
    (set-det))
  (= (rem-arg $X (Cons  $Y $R) (Cons  $Y $R1) $Pos $Pos1)  
    (is $Pos0 
      (+ $Pos 1))
    (rem-arg $X $R $R1 $Pos0 $Pos1))


  (= (rem_ins  () $_ ())  True)
  (= (rem-ins (Cons  $V $R) $Pos (Cons  $V1 $R1))  
    (rem-i $V 1 $Pos $V1)
    (rem-ins $R $Pos $R1))


  (= (rem-i (Cons  $_ $R) $P $P $R)  
    (set-det))
  (= (rem-i (Cons  $X $R) $P $P1 (Cons  $X $R1))  
    (is $P0 
      (+ $P 1))
    (rem-i $R $P0 $P1 $R1))

   
;
; ***********************************************************************
;
; *
;
; * predicate: make_newp_ex/3
;
; *
;
; * syntax: make_newp_ex(Varinstances,Newp_name,Newp_examples)
;
; *
;
; * args: Varinstances.. instantiations of the argument variables
;
; * [[I11,..,I1n],..,[Im1,..,Imn]]
;
; * Newp_examples [New_name(I11,..,I1n),..,Newp_name(Im1,..,Imn)]
;
; *
;
; * description:
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (make_newp_ex  () $_ ())  True)
  (= (make-newp-ex (Cons  $V $R) $X (Cons  $N $R1))  
    (=.. $N 
      (Cons  $X $V))
    (make-newp-ex $R $X $R1))


   
;
; ***********************************************************************
;
; *
;
; * predicate: make_type_restriction/4
;
; *
;
; * syntax: make_type_restriction(+Newpvars,+Typed_clause_vars,
;
; * -Type_restriction)
;
; *
;
; * args:
;
; *
;
; * description:
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (make_type_restriction  () $_ ())  True)
  (= (make-type-restriction (Cons  $X $R) $TVars (Cons  $T $R1))  
    (make-type-restriction $R $TVars $R1)
    (mtr $X $TVars $TN)
    (=.. $T 
      (:: $TN $X)))


  (= (mtr $X (Cons  (with_self  $Y $T) $_) $T)  
    (== $X $Y)
    (set-det))
  (= (mtr $X (Cons  $_ $R) $T)  
    (mtr $X $R $T))

   
;
; ***********************************************************************
;
; *
;
; * predicate: is_newpred/1
;
; *
;
; * syntax: is_newpred(+Pred_Name)
;
; *
;
; * args:
;
; *
;
; * description: checks whether Pred_Name is of the form 'newpXX'
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (is-newpred $Name)  
    (name $Name 
      (Cons  $N 
        (Cons  $E 
          (Cons  $W 
            (Cons  $P $_)))))
    (name newp 
      (:: $N $E $W $P)))
