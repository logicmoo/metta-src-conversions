;
; 


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
; 


;
; 

  !(use-module 
    (home lgg) 
    (:: 
      (/ headed-lgg 3) 
      (/ headed-lgg 4)))
; 

  !(use-module 
    (home kb) 
    (:: 
      (/ get-clause 5) 
      (/ store-clause 4) 
      (/ delete-clause 1) 
      (/ get-example 3)))
; 

  !(use-module 
    (home var-utils) 
    (:: 
      (/ skolemize 3) 
      (/ skolemize 4) 
      (/ deskolemize 3)))
; 

  !(use-module 
    (home div-utils) 
    (:: 
      (/ neg 2) 
      (/ buildpar2 3) 
      (/ efface 3)))
; 

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
; 

  !(use-module 
    (home show-utils) 
    (:: (/ show-bodies 0)))
; 

  !(use-module 
    (home interpreter) 
    (:: 
      (/ prove3 2) 
      (/ prove4 3)))
; 

  !(use-module-if-exists 
    (library basics) 
    (:: (/ member 2)))
; 


;
; 

;
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (g1-op $Res $Par1 $Par2) 
    (g1-op $Res $Par1 $Par2 g11))
; 

  (= 
    (g1-op $Res $Par1 $Par2 $Label) 
    ( (det-if-then-else 
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
      (store-clause $_ $Lpar2 $Label $Par2)))
; 
                 ;
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (extend-g1 $Ai_id $A_id) 
    ( (get-clause $Ai_id $_ $_ $Ai $_) 
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
          (fail)))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (apply-g1 $Clause $_) 
    ( (g1-op $_ $Clause $Par2) 
      (findall $GenPar2 
        (extend-g1 $Par2 $GenPar2) $Bag) 
      (det-if-then-else 
        (== $Bag Nil) 
        (addtolist $Par2) 
        (, 
          (delete-clause $Par2) 
          (addtolist $Bag))) 
      (fail) 
      (set-det)))
; 

  (= 
    (apply-g1 $Clause $_) 
    ( (g1-op $Clause $_ $Par2) 
      (findall $GenPar2 
        (extend-g1 $Par2 $GenPar2) $Bag) 
      (det-if-then-else 
        (== $Bag Nil) 
        (addtolist $Par2) 
        (, 
          (delete-clause $Par2) 
          (addtolist $Bag))) 
      (fail)))
; 

  (= 
    (apply-g1 $_ $List) 
    (getlist $List))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (absorb $ExID $PID $NewID) 
    ( (nonvar $PID) 
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
      (store-clause $_ $NewClause abs $NewID)))
; 




;
; 

  (= 
    (absorb $ExID $PID $NewID) 
    ( (var $PID) 
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
      (store-clause $_ $NewClause abs $NewID)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (abs-match $ExID $Mark $Proofs) 
    (det-if-then-else 
      (findall $Proof 
        (abs-match0 $ExID $Proof) $Proofs) 
      (= $Mark success) 
      (, 
        (= $Proofs Nil) 
        (= $Mark fail))))
; 

 

  (= 
    (abs-match0 $ExId 
      (Cons  
        (with_self  $Goal 
          (p)) $Proof)) 
    ( (get-clause $I $_ $_ 
        (Cons  
          (with_self  $Goal 
            (p)) $Body) usr) 
      (\== $I $ExId) 
      (prove3 $Body $Proof)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (abs-match1 $PID $Mark $Proofs) 
    (det-if-then-else 
      (findall $Proof 
        (abs-match1a $PID $Proof) $Proofs) 
      (= $Mark success) 
      (, 
        (= $Proofs Nil) 
        (= $Mark fail))))
; 



  (= 
    (abs-match1a $PID 
      (Cons  
        (with_self  $Goal 
          (p)) $Proof)) 
    ( (get-clause $PID $_ $_ 
        (Cons  
          (with_self  $Goal 
            (p)) $Body) $_) (prove3 $Body $Proof)))
; 

 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (identify $ExID $PID $NewID) 
    ( (nonvar $PID) 
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
      (store-clause $_ $NewClause idn $NewID)))
; 




;
; 

  (= 
    (identify $ExID $PID $NewID) 
    ( (var $PID) 
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
      (store-clause $_ $NewClause idn $NewID)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (ident-match $ExID $Mark $Proofs) 
    (det-if-then-else 
      (findall $Proof 
        (ident-match0 $ExID $Proof) $Proofs) 
      (= $Mark success) 
      (, 
        (= $Proofs Nil) 
        (= $Mark fail))))
; 

 


  (= 
    (ident-match0 $ExId 
      (:: (with_self  $Uncovered $Proof))) 
    ( (get-clause $I $_ $_ $Clause usr) 
      (\== $I $ExId) 
      (prove4 $Clause $Uncovered $Proof)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (ident-match1 $PID $Mark $Proofs) 
    (det-if-then-else 
      (findall $Proof 
        (ident-match1a $PID $Proof) $Proofs) 
      (= $Mark success) 
      (, 
        (= $Proofs Nil) 
        (= $Mark fail))))
; 




  (= 
    (ident-match1a $PID 
      (:: (with_self  $Uncovered $Proof))) 
    ( (get-clause $PID $_ $_ $Clause $_) (prove4 $Clause $Uncovered $Proof)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (inv-derivate $ExID $NewID) 
    ( (clear-mngr) 
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
      (set-det)))
; 



  (= 
    (inv-derivate $ExID $PrefHead $NewID) 
    ( (clear-mngr) 
      (get-clause $ExID $_ $_ $CList $_) 
      (skolemize $CList $S $CListS) 
      (assert-clause $CListS) 
      (inv-derivate1 $ExID 1) 
      (idev-build-clause $PrefHead $Clause1) 
      (deskolemize $Clause1 $S $Clause) 
      (store-clause $_ $Clause invd $NewID) 
      (set-det)))
; 



  (= 
    (inv-derivate1 $ExID $I) 
    ( (setof 
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
          (inv-derivate1 $ExID $J)) True)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (idev-match0 $ExID $Clause $Uncovered $Proof) 
    ( (get-clause $ID $_ $_ $Clause $_) 
      (\== $ExID $ID) 
      (prove4 $Clause $Uncovered $Proof)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (most-spec-v $ExID $PID $NewID) 
    ( (or 
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
      (store-clause $_ $Clause msv $NewID)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (idev-match1 $ExID $Clause $Uncovered $Proof $ID) 
    ( (get-clause $ID $_ $_ $Clause $_) 
      (\== $ExID $ID) 
      (prove4 $Clause $Uncovered $Proof)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (saturate $ExID $GenID) 
    (saturate $ExID $GenID 100))
; 


  (= 
    (saturate $ExID $GenID $Bound) 
    ( (saturate1 $ExID $NewClause $Bound) 
      (store-clause $_ $NewClause sat $GenID) 
      (set-det)))
; 
      ;
; 

    


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (saturate1 $ExID $NewClause $Bound) 
    ( (clear-mngr) 
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
      (deskolemize $Clause1 $S1 $NewClause)))
; 

        

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (saturate1a $HS $I $Bound $S1 $S2) 
    ( (sat-match $HS success $Proofs) 
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
        (= $S2 $S3))))
; 



  (= 
    (saturate1a $_ $_ $_ $S $S) 
    (set-det))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (sat-match $HS $Mark $Proofs) 
    (det-if-then-else 
      (setof $Proof 
        (^ $HS 
          (sat-match0 $HS $Proof)) $Proofs) 
      (= $Mark success) 
      (, 
        (= $Proofs Nil) 
        (= $Mark fail))))
; 



  (= 
    (sat-match0 $HS 
      (Cons  
        (with_self  $Goal 
          (p)) $ProofBody)) 
    ( (get-clause $I $_ $_ 
        (Cons  
          (with_self  $Goal 
            (p)) $Body) $_) 
      (prove3 $Body $ProofBody) 
      (\== $Goal $HS)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (elem-saturate $ExID $PID $NewID) 
    ( (nonvar $PID) 
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
      (store-clause $_ $NewClause esat $NewID)))
; 



;
; 

  (= 
    (elem-saturate $ExID $PID $NewID) 
    ( (var $PID) 
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
      (store-clause $_ $NewClause esat $NewID)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (sat-match1 $Ex 
      (Cons  
        (with_self  $Goal 
          (p)) $ProofBody) $I) 
    ( (get-clause $I $_ $_ 
        (Cons  
          (with_self  $Goal 
            (p)) $Body) $_) 
      (\== $I $Ex) 
      (prove3 $Body $ProofBody)))
; 



