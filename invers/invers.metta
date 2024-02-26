
  !(dynamic (/ flat 1))
; 

  !(dynamic (/ internal 1))
; 


;
; 

;
; 

;
; 



  (= 
    (dynamic $N $A) 
    (dynamic (/ $N $A)))
; 


;
; 

;
; 


;
; 

;
; 


;
; 

;
; 



  (= 
    (numbervars $T $Min $Max) 
    (numbervars $T %VAR $Min $Max))
; 



  !(:: (logic *))
; 



  (= 
    (absorption1 $Clause $Resolvent $InducedClause) 
    ( (split $Clause $ClauseHead $ClauseBody) 
      (split $Resolvent $ResolventHead $ResolventBody) 
      (step-1 $ClauseBody $ResolventBody $RestClauseBody $Subst_2) 
      (subst 
        (:: $ClauseHead) $Subst_2 
        (:: $NewClauseHead)) 
      (step-2 $NewClauseHead $RestClauseBody $IntermediaryBody) 
      (join $ResolventHead $IntermediaryBody $IntermediaryClause) 
      (copy $IntermediaryClause $InducedClause)))
; 

        

  (= 
    (split 
      (= $Head True) $Head Nil) 
    (set-det))
; 

  (= 
    (split 
      (= $Head $Body) $Head $BodyList) 
    ( (split $Body $_ $BodyList) (set-det)))
; 

  (= 
    (split 
      (, $Prem $Prems) $_ 
      (Cons  $Prem $RestPrems)) 
    ( (split $Prems $_ $RestPrems) (set-det)))
; 

  (= 
    (split  $Prem $_ 
      ($Prem)) True)
; 



  (= 
    (join $Head $PremList 
      (= $Head $Prems)) 
    (join $PremList $Prems))
; 


  (= 
    (join 
      (:: $Prem) $Prem) 
    (set-det))
; 

  (= 
    (join 
      (Cons  $Prem $Prems) 
      (, $Prem $RestPrems)) 
    (join $Prems $RestPrems))
; 



  (= 
    (step-1 $ClauseBody $ResolventBody $ResolventRest $Subst) 
    ( (choose-common-parts $ResolventBody $ClauseBody $Beta1 $Beta2) 
      (not (not (subset $Beta1 $ClauseBody))) 
      (substitution $ClauseBody $Beta1 $Subst) 
      (subtract-v $Beta1 $ResolventBody $ResolventRest)))
; 



  (= 
    (choose-common-parts $List1 $List2 $Sublist1 $Sublist2) 
    ( (findbag $MGT 
        (, 
          (member $Pred $List1) 
          (functor $Pred $N $A) 
          (functor $MGT $N $A)) $MGTS1) 
      (findbag $MGT 
        (, 
          (member $Pred $List2) 
          (functor $Pred $N $A) 
          (functor $MGT $N $A)) $MGTS2) 
      (set-det) 
      (dropping-condition-intersection $MGTS1 $MGTS2 $MGTS) 
      (copy $MGTS $Sublist1) 
      (copy $MGTS $Sublist2) 
      (subset $Sublist1 $List1) 
      (subset $Sublist2 $List2)))
; 



  (= 
    (dropping_condition_intersection  () $_ ()) True)
; 

  (= 
    (dropping-condition-intersection 
      (Cons  $X $R) $Y 
      (Cons  $X $Z)) 
    ( (member $X1 $Y) 
      (not (not (= $X $X1))) 
      (set-det) 
      (dropping-condition-delete $X $Y $Y1) 
      (dropping-condition-intersection $R $Y1 $Z)))
; 

  (= 
    (dropping-condition-intersection 
      (Cons  $X $R) $Y $Z) 
    (dropping-condition-intersection $R $Y $Z))
; 

     

  (= 
    (dropping-condition-delete $X 
      (Cons  $Y $Ys) $Ys) 
    ( (not (not (= $X $Y))) (set-det)))
; 

  (= 
    (dropping-condition-delete $X 
      (Cons  $Y $Ys) 
      (Cons  $Y $Zs)) 
    (dropping-condition-delete $X $Ys $Zs))
; 
 


  (= 
    (step-2 $ClauseHead $RestClauseBody $IntermediaryBody) 
    (union-v 
      (:: $ClauseHead) $RestClauseBody $IntermediaryBody))
; 

    

  (= 
    (absorption2 $Clause $Resolvent $InducedClause) 
    ( (flatten $Clause $ClauseHead $ClauseBody) 
      (flatten $Resolvent $ResolventHead $ResolventBody) 
      (step-1 $ClauseBody $ResolventBody $RestClauseBody $Subst_2) 
      (subst 
        (:: $ClauseHead) $Subst_2 
        (:: $NewClauseHead)) 
      (step-2 $NewClauseHead $RestClauseBody $IntermediaryBody) 
      (unflatten 
        (:: $ResolventHead) $IntermediaryBody $IntermediaryClause) 
      (copy $IntermediaryClause $InducedClause)))
; 



  (= 
    (flatten 
      (= $Conclusion $Conjunction) $NewConclusion $NewPremisses) 
    ( (flatten $Conclusion 
        (:: $NewConclusion) $FunctionPrems1 $Dictionary) 
      (flatten $Conjunction $Prems $FunctionPrems2 $Dictionary) 
      (union-v $FunctionPrems1 $FunctionPrems2 $FunctionPrems) 
      (union-v $FunctionPrems $Prems $NewPremisses) 
      (set-det)))
; 


  (= 
    (flatten 
      (, $Prem $Prems) $NewPremisses $FunctionPrems $Dictionary) 
    ( (flatten $Prem $NewPrem $FunctionPrems1 $Dictionary) 
      (flatten $Prems $NewPrems $FunctionPrems2 $Dictionary) 
      (union-v $NewPrem $NewPrems $NewPremisses) 
      (union-v $FunctionPrems1 $FunctionPrems2 $FunctionPrems) 
      (set-det)))
; 


  (= 
    (flatten  () () () $_) True)
; 

  (= 
    (flatten 
      (Cons  $Const $Terms) 
      (Cons  $Var $NewVars) $ResultPrems $Dictionary) 
    ( (atomic $Const) 
      (lookup $Const $Dictionary $Var) 
      (conc $Const -p $NewName) 
      (flat-assertion $NewName $Const) 
      (=.. $Pred 
        (:: $NewName $Var)) 
      (flatten $Terms $NewVars $NewPrems $Dictionary) 
      (set-det) 
      (union-v 
        (:: $Pred) $NewPrems $ResultPrems)))
; 

  (= 
    (flatten 
      (Cons  $Term $Terms) 
      (Cons  $Term $NewVars) $NewPrems $Dictionary) 
    ( (var $Term) 
      (set-det) 
      (flatten $Terms $NewVars $NewPrems $Dictionary)))
; 

  (= 
    (flatten 
      (Cons  $Term $Terms) 
      (Cons  $NewVar $NewVars) $NewPrems $Dictionary) 
    ( (=.. $Term 
        (Cons  $N $Args)) 
      (conc $N -p $NN) 
      (flatten $Args $NewTerms $Prems $Dictionary) 
      (append $NewTerms 
        (:: $NewVar) $NewArgs) 
      (=.. $NewTerm 
        (Cons  $NN $NewArgs)) 
      (flat-assertion $NN $NewTerms $N $NewTerms) 
      (flatten $Terms $NewVars $NP $Dictionary) 
      (set-det) 
      (union-v 
        (:: $NewTerm) $NP $ERG) 
      (union-v $Prems $ERG $NewPrems)))
; 


  (= 
    (flatten True Nil Nil $_) 
    (set-det))
; 

  (= 
    (flatten False Nil Nil $_) 
    (set-det))
; 

  (= 
    (flatten 
      (not $Pred) 
      (:: (not $NewPred)) $NewPrems $Dictionary) 
    ( (set-det) (flatten $Pred (:: $NewPred) $NewPrems $Dictionary)))
; 

  (= 
    (flatten $Pred 
      (:: $Pred) Nil $Dictionary) 
    ( (atomic $Pred) (set-det)))
; 

  (= 
    (flatten $Pred 
      (:: $NewPred) $NewPrems $Dictionary) 
    ( (set-det) 
      (=.. $Pred 
        (Cons  $N $Args)) 
      (flatten $Args $NewArgs $NewPrems $Dictionary) 
      (=.. $NewPred 
        (Cons  $N $NewArgs))))
; 



  (= 
    (flat-assertion $Name $Term) 
    ( (=.. $NewRelation 
        (:: $Name $Term)) 
      (copy $NewRelation $NR) 
      (skolemize 
        (:: $NR) 0 $_) 
      (get-symbols  &self 
        (= 
          (flat  $NR) true)) 
      (get-symbols  &self 
        (= 
          (flat  $NewRelation) true)) 
      (set-det)))
; 

  (= 
    (flat-assertion $Name $Term) 
    ( (=.. $NewRelation 
        (:: $Name $Term)) 
      (add-symbol  &self 
        (flat  $NewRelation)) 
      (set-det)))
; 


  (= 
    (flat-assertion $Name1 $Args $Name2 $Terms) 
    ( (=.. $OldTerm 
        (Cons  $Name2 $Terms)) 
      (append $Args 
        (:: $OldTerm) $NewArgs) 
      (=.. $NewRelation 
        (Cons  $Name1 $NewArgs)) 
      (copy $NewRelation $NR) 
      (skolemize 
        (:: $NR) 0 $_) 
      (get-symbols  &self 
        (= 
          (flat  $NR) true)) 
      (get-symbols  &self 
        (= 
          (flat  $NewRelation) true)) 
      (set-det)))
; 

  (= 
    (flat-assertion $Name1 $Args $Name2 $Terms) 
    ( (=.. $OldTerm 
        (Cons  $Name2 $Terms)) 
      (append $Args 
        (:: $OldTerm) $NewArgs) 
      (=.. $NewRelation 
        (Cons  $Name1 $NewArgs)) 
      (add-symbol  &self 
        (flat  $NewRelation))))
; 



  (= 
    (unflatten 
      (:: $Head) $BodyList 
      (= $Head $Body)) 
    ( (unflatten $BodyList $Body) (set-det)))
; 

    
  (= 
    (unflatten  () true) True)
; 

  (= 
    (unflatten 
      (:: $Prem) $Prem) 
    ( (functor $Prem $N $A) 
      (functor $P $N $A) 
      (not (get-symbols  &self (= (flat  $P) true))) 
      (set-det) 
      (not (get-symbols  &self (= (flat  $Prem) true))) 
      (set-det)))
; 

  (= 
    (unflatten 
      (Cons  $Prem $Prems) 
      (, $Prem $RestPrems)) 
    ( (functor $Prem $N $A) 
      (functor $P $N $A) 
      (not (get-symbols  &self (= (flat  $P) true))) 
      (set-det) 
      (not (get-symbols  &self (= (flat  $Prem) true))) 
      (unflatten $Prems $RestPrems) 
      (set-det)))
; 

  (= 
    (unflatten 
      (Cons  $Prem $Prems) $RestPrems) 
    ( (get-symbols  &self 
        (= 
          (flat  $Prem) true)) (unflatten $Prems $RestPrems)))
; 
    
	    

  (= 
    (intra-construction1 $Resolvent1 $Resolvent2 $InducedRules) 
    ( (split $Resolvent1 $Resolvent1Head $Resolvent1Body) 
      (split $Resolvent2 $Resolvent2Head $Resolvent2Body) 
      (step-1 $Resolvent1Head $Resolvent1Body $Resolvent2Head $Resolvent2Body $ClauseHead $Alpha $Resolvent1BodyRest $Resolvent2BodyRest $Subst1 $Subst2) 
      (step-2 $ClauseHead $Alpha $Resolvent1BodyRest $Subst1 $Resolvent2BodyRest $Subst2 $Vars) 
      (step-3-1 $ClauseHead $Vars $Alpha $Resolvent1BodyRest $Subst1 $Resolvent2BodyRest $Subst2 $InducedRules)))
; 



  (= 
    (step-1 $Resolvent1Head $Resolvent1Body $Resolvent2Head $Resolvent2Body 
      (:: $ClauseHead) $Alpha $Resolvent1BodyRest $Resolvent2BodyRest $Subst1 $Subst2) 
    ( (choose-common-parts $Resolvent1Body $Resolvent2Body $AlphaTheta1 $AlphaTheta2) 
      (length $AlphaTheta1 $X) 
      (length $AlphaTheta2 $X) 
      (> $X 0) 
      (subtract-v $AlphaTheta1 $Resolvent1Body $Resolvent1BodyRest) 
      (subtract-v $AlphaTheta2 $Resolvent2Body $Resolvent2BodyRest) 
      (lgg 
        (:: $Resolvent1Head $AlphaTheta1) 
        (:: $Resolvent2Head $AlphaTheta2) 
        (:: $ClauseHead $Alpha)) 
      (substitution 
        (:: $ClauseHead $Alpha) 
        (:: $Resolvent1Head $AlphaTheta1) $Subst1) 
      (substitution 
        (:: $ClauseHead $Alpha) 
        (:: $Resolvent2Head $AlphaTheta2) $Subst2)))
; 



  (= 
    (step-2 $ClauseHead $Alpha $Resolvent1BodyRest $Subst1 $Resolvent2BodyRest $Subst2 $Vars) 
    ( (varlist $Resolvent1BodyRest $RBR1Vars) 
      (varlist $Resolvent2BodyRest $RBR2Vars) 
      (varlist $ClauseHead $CHVars) 
      (varlist $Alpha $AVars) 
      (inv-subst $RBR1 $Subst1 $RBR1Vars) 
      (inv-subst $RBR2 $Subst2 $RBR2Vars) 
      (or 
        (= $CHVars Nil) 
        (, 
          (intersection-v $CHVars $AVars $XI) 
          (not (== $XI Nil)))) 
      (union-v $CHVars $AVars $Xs) 
      (union-v $RBR1 $RBR2 $RBRU) 
      (intersection-v $RBRU $Xs $Vars) 
      (set-det)))
; 



  (= 
    (step-3-1 
      (:: $ClauseHead) $Vars $Alpha $Resolvent1BodyRest $Subst1 $Resolvent2BodyRest $Subst2 
      (:: $R1 $R2 $R3)) 
    ( (gensym $Name P) 
      (=.. $X 
        (Cons  $Name $Vars)) 
      (subst 
        (:: $X) $Subst1 
        (:: $X1)) 
      (subst 
        (:: $X) $Subst2 
        (:: $X2)) 
      (join $X1 $Resolvent1BodyRest $QR1) 
      (join $X2 $Resolvent2BodyRest $QR2) 
      (copy $QR1 $NQR1) 
      (copy $QR2 $NQR2) 
      (numbervars $NQR1 0 $_) 
      (numbervars $NQR2 0 $_) 
      (nl) 
      (write ' Predicate invention') 
      (nl) 
      (nl) 
      (tab 3) 
      (write $NQR1) 
      (nl) 
      (tab 3) 
      (write $NQR2) 
      (nl) 
      (nl) 
      (write ' Has this predicate a meaning (y/n)? ') 
      (read $Answer) 
      (nl) 
      (or 
        (, 
          (= $Answer n) 
          (set-det) 
          (fail)) 
        (, 
          (= $Answer y) 
          (set-det))) 
      (write ' How shall I name the predicate ? ') 
      (read $NewName) 
      (nl) 
      (=.. $NP 
        (Cons  $NewName $Vars)) 
      (subst 
        (:: $NP) $Subst1 
        (:: $NP1)) 
      (subst 
        (:: $NP) $Subst2 
        (:: $NP2)) 
      (join $NP1 $Resolvent1BodyRest $R1) 
      (join $NP2 $Resolvent2BodyRest $R2) 
      (append 
        (:: $NP) $Alpha $ClauseBody) 
      (join $ClauseHead $ClauseBody $R3) 
      (set-det)))
; 

    

  (= 
    (intra-construction2 $Resolvent1 $Resolvent2 $InducedClauses) 
    ( (flatten $Resolvent1 $Resolvent1Head $Resolvent1Body) 
      (flatten $Resolvent2 $Resolvent2Head $Resolvent2Body) 
      (step-1 $Resolvent1Head $Resolvent1Body $Resolvent2Head $Resolvent2Body $ClauseHead $Alpha $Resolvent1BodyRest $Resolvent2BodyRest $Subst1 $Subst2) 
      (step-2 $ClauseHead $Alpha $Resolvent1BodyRest $Subst1 $Resolvent2BodyRest $Subst2 $Vars) 
      (step-3-2 $ClauseHead $Vars $Alpha $Resolvent1BodyRest $Subst1 $Resolvent2BodyRest $Subst2 $InducedClauses)))
; 

    

  (= 
    (step-3-2 $ClauseHead $Vars $Alpha $Resolvent1BodyRest $Subst1 $Resolvent2BodyRest $Subst2 
      (:: $R1 $R2 $R3)) 
    ( (gensym $Name P) 
      (=.. $X 
        (Cons  $Name $Vars)) 
      (subst 
        (:: $X) $Subst1 $X1) 
      (subst 
        (:: $X) $Subst2 $X2) 
      (unflatten $X1 $Resolvent1BodyRest $QR1) 
      (unflatten $X2 $Resolvent2BodyRest $QR2) 
      (copy $QR1 $NQR1) 
      (copy $QR2 $NQR2) 
      (numbervars $NQR1 0 $_) 
      (numbervars $NQR2 0 $_) 
      (nl) 
      (write ' Predicate invention') 
      (nl) 
      (nl) 
      (tab 3) 
      (write $NQR1) 
      (nl) 
      (tab 3) 
      (write $NQR2) 
      (nl) 
      (nl) 
      (write ' Has this predicate a meaning (y/n)? ') 
      (read $Answer) 
      (nl) 
      (or 
        (, 
          (= $Answer n) 
          (set-det) 
          (fail)) 
        (, 
          (= $Answer y) 
          (set-det))) 
      (write ' How shall I name the predicate ? ') 
      (read $NewName) 
      (nl) 
      (=.. $NP 
        (Cons  $NewName $Vars)) 
      (subst 
        (:: $NP) $Subst1 $NP1) 
      (subst 
        (:: $NP) $Subst2 $NP2) 
      (unflatten $NP1 $Resolvent1BodyRest $R1) 
      (unflatten $NP2 $Resolvent2BodyRest $R2) 
      (append 
        (:: $NP) $Alpha $ClauseBody) 
      (unflatten $ClauseHead $ClauseBody $R3) 
      (set-det)))
; 

    

  (= 
    (subst  () $_ ()) True)
; 

  (= 
    (subst 
      (Cons  $X $Xs) $Subst 
      (Cons  $Y $Ys)) 
    ( (member 
        (/ $U $V) $Subst) 
      (== $X $U) 
      (= $Y $V) 
      (subst $Xs $Subst $Ys) 
      (set-det)))
; 

  (= 
    (subst 
      (Cons  $Prem $Prems) $Subst 
      (Cons  $NewPrem $NewPrems)) 
    ( (=.. $Prem 
        (Cons  $F $Args)) 
      (subst $Args $Subst $NewArgs) 
      (=.. $NewPrem 
        (Cons  $F $NewArgs)) 
      (subst $Prems $Subst $NewPrems) 
      (set-det)))
; 

  (= 
    (subst 
      (Cons  $X $Xs) $Subst 
      (Cons  $X $Ys)) 
    (subst $Xs $Subst $Ys))
; 



  (= 
    (inv_subst  () $_ ()) True)
; 

  (= 
    (inv-subst 
      (Cons  $X $Xs) $Subst 
      (Cons  $Y $Ys)) 
    ( (member 
        (/ $U $V) $Subst) 
      (== $Y $V) 
      (= $X $U) 
      (inv-subst $Xs $Subst $Ys) 
      (set-det)))
; 

  (= 
    (inv-subst 
      (Cons  $Prem $Prems) $Subst 
      (Cons  $NewPrem $NewPrems)) 
    ( (=.. $NewPrem 
        (Cons  $F $NewArgs)) 
      (inv-subst $Args $Subst $NewArgs) 
      (=.. $Prem 
        (Cons  $F $Args)) 
      (inv-subst $Prems $Subst $NewPrems) 
      (set-det)))
; 

  (= 
    (inv-subst 
      (Cons  $X $Xs) $Subst 
      (Cons  $X $Ys)) 
    (inv-subst $Xs $Subst $Ys))
; 



  (= 
    (union_v  () $X $X) True)
; 

  (= 
    (union-v 
      (Cons  $X $R) $Y $Z) 
    ( (member $U $Y) 
      (== $U $X) 
      (set-det) 
      (union-v $R $Y $Z)))
; 

  (= 
    (union-v 
      (Cons  $X $R) $Y 
      (Cons  $X $Z)) 
    (union-v $R $Y $Z))
; 



  (= 
    (intersection_v  () $_ ()) True)
; 

  (= 
    (intersection-v 
      (Cons  $X $R) $Y 
      (Cons  $X $Z)) 
    ( (member $U $Y) 
      (== $U $X) 
      (set-det) 
      (intersection-v $R $Y $Z)))
; 

  (= 
    (intersection-v 
      (Cons  $X $R) $Y $Z) 
    (intersection-v $R $Y $Z))
; 



  (= 
    (subset  () ()) True)
; 

  (= 
    (subset 
      (Cons  $X $Subset) 
      (Cons  $X $List)) 
    (subset $Subset $List))
; 

  (= 
    (subset $Subset 
      (Cons  $_ $List)) 
    (subset $Subset $List))
; 

    

  (= 
    (subtract-v 
      (Cons  $X $Xs) $Ys $Zs) 
    ( (member $Y $Ys) 
      (== $X $Y) 
      (delete-v $X $Ys $Y1s) 
      (subtract-v $Xs $Y1s $Zs) 
      (set-det)))
; 

  (= 
    (subtract-v 
      (Cons  $X $Xs) $Ys $Zs) 
    (subtract-v $Xs $Ys $Zs))
; 

  (= 
    (subtract_v  () $Xs $Xs) True)
; 



  (= 
    (delete-v $X 
      (Cons  $Y $Ys) $Ys) 
    ( (== $X $Y) (set-det)))
; 

  (= 
    (delete-v $X 
      (Cons  $Y $Ys) 
      (Cons  $Y $Zs)) 
    (delete-v $X $Ys $Zs))
; 
 
    

  (= 
    (count $VAR $X) 
    ( (dynamic $VAR 1) 
      (=.. $P1 
        (:: $VAR $N)) 
      (remove-symbol  &self $P1) 
      (is $X 
        (+ $N 1)) 
      (=.. $P2 
        (:: $VAR $X)) 
      (add-symbol  &self $P2) 
      (set-det)))
; 

  (= 
    (count $VAR 1) 
    ( (dynamic $VAR 1) 
      (=.. $P 
        (:: $VAR 1)) 
      (add-symbol  &self $P)))
; 



  (= 
    (gensym $SYM $N) 
    ( (count $N $X) (conc $N $X $SYM)))
; 



  (= 
    (copy $A $B) 
    (or 
      (, 
        (add-symbol  &self 
          (internal  $A)) 
        (remove-symbol  &self 
          (internal  $B)) 
        (set-det)) 
      (, 
        (remove-symbol  &self 
          (internal  $_)) 
        (fail))))
; 



  (= 
    (lookup $Key 
      (d $Key $X $Left $Right) $Value) 
    ( (set-det) (= $X $Value)))
; 

  (= 
    (lookup $Key 
      (d $Key1 $X $Left $Right) $Value) 
    ( (@< $Key $Key1) (lookup $Key $Left $Value)))
; 

  (= 
    (lookup $Key 
      (d $Key1 $X $Left $Right) $Value) 
    ( (@> $Key $Key1) (lookup $Key $Right $Value)))
; 



  (= 
    (conc $STR1 $STR2 $STR3) 
    ( (nonvar $STR1) 
      (nonvar $STR2) 
      (name $STR1 $S1) 
      (name $STR2 $S2) 
      (append $S1 $S2 $S3) 
      (name $STR3 $S3)))
; 

  (= 
    (conc $STR1 $STR2 $STR3) 
    ( (nonvar $STR1) 
      (nonvar $STR3) 
      (name $STR1 $S1) 
      (name $STR3 $S3) 
      (append $S1 $S2 $S3) 
      (name $STR2 $S2)))
; 

  (= 
    (conc $STR1 $STR2 $STR3) 
    ( (nonvar $STR2) 
      (nonvar $STR3) 
      (name $STR2 $S2) 
      (name $STR3 $S3) 
      (append $S1 $S2 $S3) 
      (name $STR1 $S1)))
; 



  (= 
    (varlist $X $L) 
    ( (varlist $X Nil $L) (set-det)))
; 

   
  (= 
    (varlist $T $L $LO) 
    ( (nonvar $T) 
      (=.. $T 
        (Cons  $F $A)) 
      (set-det) 
      (varlist1 $A $L $LO)))
; 

  (= 
    (varlist $X $L $L) 
    ( (var $X) 
      (element-v $X $L) 
      (set-det)))
; 

  (= 
    (varlist $X $L 
      (Cons  $X $L)) 
    ( (var $X) (set-det)))
; 



  (= 
    (varlist1 
      (Cons  $T $A) $L $LO) 
    ( (varlist1 $A $L $L1) 
      (set-det) 
      (varlist $T $L1 $LO)))
; 

  (= 
    (varlist1  () $L $L) True)
; 



  (= 
    (element-v $Element1 
      (Cons  $Element2 $Tail)) 
    (== $Element1 $Element2))
; 

  (= 
    (element-v $Element 
      (Cons  $_ $Tail)) 
    (element-v $Element $Tail))
; 



  (= 
    (findbag $X $G $_) 
    ( (add-symbol  &self 
        (yk_found  mark)) 
      (call $G) 
      (add-symbol  &self 
        (yk_found  $X)) 
      (fail)))
; 
            
  (= 
    (findbag $_ $_ $L) 
    (yk-collect-found Nil $L))
; 
                     
                                               

  (= 
    (yk-collect-found $Acc $L) 
    ( (yk-getnext $X) (yk-collect-found (Cons  $X $Acc) $L)))
; 
      
  (= 
    (yk_collect_found  $X $X) True)
; 
                        
                                               
;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  invers/invers.pl 614 33 26398))



  (= 
    (help) 
    ( (write 'Load example calls with command: [Filename].') 
      (nl) 
      (write 'Run example with command: test1(X).') 
      (nl) 
      (write 'Run example with command: test2(X).') 
      (nl) 
      (write 'Run example with command: test3(X).') 
      (nl) 
      (write 'Run example with command: test4(X).') 
      (nl) 
      (write 'Run example with command: test5(X).') 
      (nl) 
      (write 'Run example with command: test6(X).') 
      (nl) 
      (write 'Run example with command: test7(X).') 
      (nl) 
      (write 'Run example with command: test8(X).') 
      (nl)))
; 



  !(help *)
; 


