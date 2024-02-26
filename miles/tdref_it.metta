;
; 


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



;
; 

  !(use-module 
    (home kb) 
    (:: 
      (/ get-clause 5) 
      (/ get-evaluation 2) 
      (/ get-predlist 1)))
; 

  !(use-module 
    (home td-basic) 
    (:: 
      (/ distribute-vars 3) 
      (/ enumerate-t 3) 
      (/ append-body 3)))
; 

  !(use-module 
    (home filter) 
    (:: 
      (/ noduplicate-symbols 1) 
      (/ noduplicate-symbol 2) 
      (/ select-var-sharing-lits 2)))
; 

  !(use-module 
    (home div-utils) 
    (:: (/ mysetof 3)))
; 

  !(use-module 
    (home var-utils) 
    (:: 
      (/ typed-only-vars1 2) 
      (/ clause-terms 2)))
; 

  !(use-module 
    (home argument-types) 
    (:: 
      (/ compare-types 3) 
      (/ types-of 3)))
; 

  !(use-module 
    (home show-utils) 
    (:: (/ write-list 1)))
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
; 

;
; 



  (= 
    (refinement-unify-variables $ID 
      (, $ID $CL)) 
    ( (get-clause $ID $H $B $_ $_) 
      (= $C 
        (= $H $B)) 
      (clause-terms $C $Terms) 
      (types-of $Terms $C $TTerms) 
      (refinement-unify-variables $C $TTerms $CL)))
; 



  (= 
    (refinement-instantiate-variables $ID 
      (, $ID $CL)) 
    ( (get-clause $ID $H $B $_ $_) 
      (= $C 
        (= $H $B)) 
      (clause-terms $C $Terms) 
      (types-of $Terms $C $TTerms) 
      (refinement-instantiate-variables $C $TTerms $CL)))
; 



  (= 
    (refinement-add-body-literal $ID 
      (, $ID $CL)) 
    ( (get-clause $ID $H $B $_ $_) 
      (= $C 
        (= $H $B)) 
      (clause-terms $C $Terms) 
      (types-of $Terms $C $TTerms) 
      (refinement-add-body-literal $C $TTerms $CL1) 
      (select-var-sharing-lits $CL1 $CL)))
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
    (refinement-unify-variables $C $T $CL) 
    ( (typed-only-vars1 $T $Vars) (ref-unify-vars $C $Vars $Vars Nil $CL)))
; 



  (= 
    (refinement-instantiate-variables $C $T $CL) 
    ( (typed-only-vars1 $T $Vars) (ref-instantiate-vars $C $Vars Nil $CL)))
; 



  (= 
    (refinement-add-body-literal $C $T $CL) 
    (det-if-then-else 
      (= $C 
        (= $_ $_)) 
      (ref-add-body-literal $C $T Nil $CL) 
      (ref-add-body-literal 
        (= $C True) $T Nil $CL)))
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

;
; 

;
; 



  (= 
    (possible-body-literals $C $Terms $LL) 
    ( (get-predlist $Predlist) (enumerate-terms $Predlist $Terms $C Nil $LL)))
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

;
; 



  (= 
    (ref_unify_vars  $_ () $_ $CL $CL) True)
; 

  (= 
    (ref-unify-vars $C 
      (Cons  $X $R) $V $CL $CL2) 
    ( (ref-unify-vars $C $R $V $CL $CL1) (ref-unify-vars1 $C $X $V $CL1 $CL2)))
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
    (ref_unify_vars1  $_ $_ () $CL $CL) True)
; 

  (= 
    (ref-unify-vars1 $C 
      (with_self  $X $Tx) 
      (Cons  
        (with_self  $Y $Ty) $R) $CL $CL2) 
    ( (ref-unify-vars1 $C 
        (with_self  $X $Tx) $R $CL $CL1) (det-if-then-else (, (compare-types $Tx $Ty $_) (@< $X $Y)) (, (copy-term (, $C $X $Y) (, $C1 $Z $Z)) (det-if-then-else (noduplicate-atoms $C1) (= $CL2 (Cons  $C1 $CL1)) (= $CL2 $CL1))) (= $CL2 $CL1))))
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

;
; 

;
; 

;
; 



  (= 
    (ref_instantiate_vars  $_ () $CL $CL) True)
; 

  (= 
    (ref-instantiate-vars $C 
      (Cons  
        (with_self  $X $T) $R) $CL $CL2) 
    ( (ref-instantiate-vars $C $R $CL $CL1) 
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
      (append $HL1 $CL1 $CL2)))
; 



  (= 
    (ref_inst_vars  () $_ $_ ()) True)
; 

  (= 
    (ref-inst-vars 
      (Cons  $H1 $R) $H $C $HL2) 
    ( (ref-inst-vars $R $H $C $HL1) (det-if-then-else (, (copy-term (, $H $C) (, $H1 $C1)) (noduplicate-atoms $C1)) (= $HL2 (Cons  $C1 $HL1)) (= $HL2 $HL1))))
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
; 



  (= 
    (ref-add-body-literal $C $Terms $CL $CL1) 
    ( (get-predlist $Predlist) 
      (enumerate-terms $Predlist $Terms $C Nil $L) 
      (add-to-bodies $L $C $CL $CL1)))
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

;
; 

;
; 

;
; 



  (= 
    (add_to_bodies  () $_ $CL $CL) True)
; 

  (= 
    (add-to-bodies 
      (Cons  $Pred $R) $C $CL $CL2) 
    ( (add-to-bodies $R $C $CL $CL1) 
      (copy-term 
        (, $Pred $C) 
        (, $Pred1 
          (= $H1 $B1))) 
      (det-if-then-else 
        (noduplicate-atom $Pred1 
          (, $H1 $B1)) 
        (, 
          (append-body 
            (= $H1 $B1) $Pred1 $C2) 
          (= $CL2 
            (Cons  $C2 $CL1))) 
        (= $CL2 $CL1))))
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
; 



  (= 
    (enumerate_terms  () $_ $_ $L $L) True)
; 

  (= 
    (enumerate-terms 
      (Cons  
        (with_self  $P $PVars) $R) $V $C $L $L2) 
    ( (enumerate-terms $R $V $C $L $L1) 
      (distribute-vars $PVars $V $PVars1) 
      (enumerate-t $PVars1 
        (:: $P) $Plist) 
      (append $Plist $L1 $L2)))
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
; 

;
; 

;
; 



  (= 
    (refinement $ID $CL) 
    ( (number $ID) 
      (set-det) 
      (get-clause $ID $H $B $_ $_) 
      (get-evaluation $ID 
        (evaluation $_ $_ $Pos $_ $_ $_ $_ $_ $_)) 
      (det-if-then-else 
        (== $Pos Nil) 
        (= $CL Nil) 
        (, 
          (clause-terms 
            (= $H $B) $Terms) 
          (types-of $Terms 
            (= $H $B) $TTerms) 
          (refinement-unify-variables 
            (= $H $B) $TTerms $CL0) 
          (refinement-instantiate-variables 
            (= $H $B) $TTerms $CL1) 
          (refinement-add-body-literal 
            (= $H $B) $TTerms $CL2) 
          (select-var-sharing-lits $CL2 $CL3) 
          (append $CL0 $CL1 $CL4) 
          (append $CL4 $CL3 $CL)))))
; 


  (= 
    (refinement 
      (= $H $B) $CL) 
    ( (clause-terms 
        (= $H $B) $Terms) 
      (types-of $Terms 
        (= $H $B) $TTerms) 
      (refinement-unify-variables 
        (= $H $B) $TTerms $CL0) 
      (refinement-instantiate-variables 
        (= $H $B) $TTerms $CL1) 
      (refinement-add-body-literal 
        (= $H $B) $TTerms $CL2) 
      (select-var-sharing-lits $CL2 $CL3) 
      (append $CL0 $CL1 $CL4) 
      (append $CL4 $CL3 $CL)))
; 


