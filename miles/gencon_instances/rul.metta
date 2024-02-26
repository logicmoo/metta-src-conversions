;
; 


  !(module rul 
    (:: (/ learn-rul 0)))
; 



;
; 

  !(use-module 
    (home kb) 
    (:: 
      (/ get-example 3) 
      (/ get-clause 5) 
      (/ get-evaluation 2) 
      (/ store-ex 3) 
      (/ delete-clause 1) 
      (/ store-clauses 2) 
      (/ store-clauses 3) 
      (/ store-clause 4) 
      (/ delete-all 1)))
; 

  !(use-module 
    (home argument-types) 
    (:: 
      (/ type-sub 2) 
      (/ type-equal 4) 
      (/ replace-t 4)))
; 
 
  !(use-module 
    (home gencon) 
    (:: (/ gilppi 12)))
; 
                    
  !(use-module 
    (home show-utils) 
    (:: (/ show-kb 0)))
; 
                     
  !(use-module 
    (home evaluation) 
    (:: 
      (/ eval-examples 0) 
      (/ covered-pos-examples 1)))
; 

  !(use-module 
    (home lgg) 
    (:: (/ set-lgg 2)))
; 

  !(use-module 
    (home div-utils) 
    (:: 
      (/ different-predicates 2) 
      (/ remove-v 3) 
      (/ mysetof 3)))
; 

  !(use-module 
    (home var-utils) 
    (:: (/ only-vars 2)))
; 

  !(use-module 
    (home td-basic) 
    (:: (/ append-body 3)))
; 

  !(use-module 
    (home newpred) 
    (:: (/ is-newpred 1)))
; 

  !(use-module-if-exists 
    (library basics) 
    (:: (/ member 2)))
; 

  !(use-module-if-exists 
    (library strings) 
    (:: (/ gensym 2)))
; 


;
; 

;
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (learn-rul) 
    (gilppi initialize stop-c quality-c update select add filter one-of spec gen l-newp output))
; 






  (= 
    (initialize (:: (with_self  $HL (active)))) 
    ( (mysetof $E 
        (^ $I 
          (get-example $I $E +)) $Elist) 
      (different-predicates $Elist $Elist1) 
      (initialize1 $Elist1 $HL)))
; 



  (= 
    (initialize1  () ()) True)
; 

  (= 
    (initialize1 
      (Cons  
        (Cons  $E $ER) $R) $HL) 
    ( (initialize1 $R $HL0) 
      (functor $E $T $_) 
      (mysetof $A 
        (^ $M 
          (, 
            (member $M 
              (Cons  $E $ER)) 
            (arg 1 $M $A))) $Alist) 
      (different-predicates $Alist $Alist1) 
      (initialize2 $Alist1 $T $HL1) 
      (append $HL1 $HL0 $HL)))
; 



  (= 
    (initialize2  () $_ ()) True)
; 

  (= 
    (initialize2 
      (Cons  $A $R) $T 
      (Cons  
        (= $H True) $R1)) 
    ( (set-lgg $A $A1) 
      (=.. $H 
        (:: $T $A1)) 
      (initialize2 $R $T $R1)))
; 




  (= 
    (stop_c  
      ($_)) True)
; 



  (= 
    (quality_c  ()) True)
; 

  (= 
    (quality-c (Cons  (= $H $B) $R)) 
    ( (only-vars $H $HV) 
      (only-vars $B $BV) 
      (remove-v $BV $HV Nil) 
      (quality-c $R)))
; 



  (= 
    (update  $L $L) True)
; 



  (= 
    (select $Partial_Sols $PS active $Partial_Sols1) 
    (select-active $Partial_Sols $PS $Partial_Sols1))
; 

  (= 
    (select $Partial_Sols $PS passive $Partial_Sols) 
    (select-passive $Partial_Sols $PS))
; 



  (= 
    (select_active  
      (Cons  
        (:  $PS active) $R) $PS 
      (Cons  
        (:  $PS passive) $R)) True)
; 

  (= 
    (select-active 
      (Cons  $P $R) $PS 
      (Cons  $P $R1)) 
    (select-active $R $PS $R1))
; 



  (= 
    (select-passive 
      (:: (with_self  $PS $_)) $PS) 
    (set-det))
; 

  (= 
    (select-passive 
      (Cons  
        (with_self  $PS $_) $R) $PS2) 
    ( (select-passive $R $PS1) (most-specific $PS $PS1 $PS2)))
; 



  (= 
    (most-specific $PS $PS1 $PS2) 
    (det-if-then-else 
      (more-specific $PS $PS1) 
      (= $PS2 $PS) 
      (= $PS2 $PS1)))
; 



  (= 
    (more-specific $Spec $Gen) 
    ( (copy-term 
        (, $Spec $Gen) 
        (, $Spec0 $Gen0)) 
      (normalize $Spec0 $Spec1) 
      (normalize $Gen0 $Gen1) 
      (rename-types $Gen1 $Spec1 $Spec2 $Tlist) 
      (store-clauses $Spec2 type $IDS) 
      (store-clauses $Gen1 type $IDG) 
      (append $IDS $IDG $IDA) 
      (det-if-then-else 
        (more-spec $Tlist) 
        (delete-all $IDA) 
        (, 
          (delete-all $IDA) 
          (set-det) 
          (fail)))))
; 




  (= 
    (more_spec  ()) True)
; 

  (= 
    (more-spec (Cons  (with_self  $Spec $Gen) $R)) 
    ( (type-sub $Gen $Spec) (more-spec $R)))
; 



  (= 
    (normalize  () ()) True)
; 

  (= 
    (normalize 
      (Cons  
        (= $H $B) $R) 
      (Cons  
        (= $H $B1) $R1)) 
    ( (normalize $R $R1) 
      (only-vars $H $HV) 
      (only-vars $B $BV) 
      (remove-v $BV $HV $RV) 
      (normalize $RV $B $B1)))
; 


  (= 
    (normalize 
      (:: $V) True 
      (all $V)) 
    (set-det))
; 

  (= 
    (normalize  () $B $B) True)
; 

  (= 
    (normalize 
      (Cons  $V $R) $B 
      (, 
        (all $V) $B1)) 
    (normalize $R $B $B1))
; 



  (= 
    (rename-types $Gen $Spec $Spec1 $Tlist) 
    ( (mysetof $Pred 
        (^ $H 
          (^ $B 
            (, 
              (member 
                (= $H $B) $Gen) 
              (functor $H $Pred 1)))) $Plist) 
      (rename-t $Plist $Tlist) 
      (transform-t $Spec $Spec1 $Tlist)))
; 



  (= 
    (rename_t  () ()) True)
; 

  (= 
    (rename-t 
      (Cons  $P $R) 
      (Cons  
        (with_self  $P1 $P) $R1)) 
    ( (rename-t $R $R1) (gensym $P $P1)))
; 



  (= 
    (transform_t  () () $_) True)
; 

  (= 
    (transform-t 
      (Cons  
        (= $H $B) $R) 
      (Cons  
        (= $H1 $B1) $R1) $Tlist) 
    ( (transform-t $R $R1 $Tlist) (transform-t1 (, $H $B) $Tlist (, $H1 $B1))))
; 



  (= 
    (transform-t1 
      (, $A $B) $Tlist 
      (, $A1 $B1)) 
    ( (set-det) 
      (transform-t1 $A $Tlist $A1) 
      (transform-t1 $B $Tlist $B1)))
; 

  (= 
    (transform-t1 True $_ True) 
    (set-det))
; 

  (= 
    (transform-t1 $A $Tlist $A1) 
    ( (=.. $A 
        (:: $Pred $Arg)) (det-if-then-else (member (with_self  $Pred1 $Pred) $Tlist) (=.. $A1 (:: $Pred1 $Arg)) (= $A1 $A))))
; 






  (= 
    (add $Partial_Sols $PSL $Partial_Sols1) 
    (append $PSL $Partial_Sols $Partial_Sols1))
; 



  (= 
    (filter  () ()) True)
; 

  (= 
    (filter 
      (Cons  $CL $R) 
      (Cons  $CL1 $R2)) 
    ( (filter $R $CL $R1 $CL1) (filter $R1 $R2)))
; 

  (= 
    (filter  () $CL () $CL) True)
; 

  (= 
    (filter 
      (Cons  
        (with_self  $CL1 $A) $R) 
      (with_self  $CL $B) $R1 $CL2) 
    (det-if-then-else 
      (more-specific $CL $CL1) 
      (det-if-then-else 
        (or 
          (== $B active) 
          (== $A passive)) 
        (filter $R 
          (with_self  $CL $B) $R1 $CL2) 
        (filter $R 
          (with_self  $CL1 $A) $R1 $CL2)) 
      (det-if-then-else 
        (more-specific $CL1 $CL) 
        (det-if-then-else 
          (or 
            (== $A active) 
            (== $B passive)) 
          (filter $R 
            (with_self  $CL1 $A) $R1 $CL2) 
          (filter $R 
            (with_self  $CL $B) $R1 $CL2)) 
        (, 
          (= $R1 
            (Cons  
              (with_self  $CL1 $A) $R0)) 
          (filter $R 
            (with_self  $CL $B) $R0 $CL2)))))
; 





  (= 
    (one-of $PS $M) 
    ( (store-clauses $PS hypo $IDL) 
      (eval-examples) 
      (mysetof 
        (with_self  $ID $P) 
        (get-example $ID $P +) $Pos) 
      (rem-other-covered $IDL $Pos $Pos1) 
      (delete-all $IDL) 
      (det-if-then-else 
        (= $Pos1 Nil) 
        (= $M spec) 
        (= $M gen))))
; 




  (= 
    (spec $PS $PSL) 
    ( (store-clauses $PS hypo $IDL) 
      (eval-examples) 
      (mysetof $P 
        (^ $ID0 
          (^ $H0 
            (^ $B0 
              (^ $CL0 
                (^ $L0 
                  (, 
                    (get-clause $ID0 $H0 $B0 $CL0 $L0) 
                    (functor $H0 $P 1))))))) $Predlist) 
      (spec $IDL $Predlist $PSL)))
; 


  (= 
    (spec Nil $_ Nil) 
    (mysetof 
      (= $H1 $B1) 
      (^ $ID1 
        (^ $CL 
          (, 
            (get-clause $ID1 $H1 $B1 $CL hypo) 
            (delete-clause $ID1)))) $_))
; 

  (= 
    (spec 
      (Cons  $ID $R) $Preds $PSL) 
    ( (get-clause $ID $H $B $_ $_) (det-if-then-else (specable $H $B $RV) (, (get-evaluation $ID (evaluation $_ $_ $Pos $_ $_ $_ $_ $_ $_)) (remove-other-covered $H $ID $Pos $Pos1) (delete-clause $ID) (spec-c $RV $H $B $Preds $ID $Pos1 $PSL0) (det-if-then-else (\== $PSL0 Nil) (, (= $PSL0 $PSL) (mysetof (= $H1 $B1) (^ $ID1 (^ $CL (, (get-clause $ID1 $H1 $B1 $CL hypo) (delete-clause $ID1)))) $_)) (, (store-clause (= $H $B) $_ hypo $ID) (spec $R $Preds $PSL)))) (spec $R $Preds $PSL))))
; 




  (= 
    (specable $H $B $RV) 
    ( (only-vars $H $HV) 
      (only-vars $B $BV) 
      (remove-v $BV $HV $RV) 
      (\== $RV Nil) 
      (set-det)))
; 



  (= 
    (remove-other-covered $H $ID $Pos $Pos1) 
    ( (functor $H $F $N) 
      (functor $H1 $F $N) 
      (mysetof $ID0 
        (^ $H1 
          (^ $B1 
            (^ $CL1 
              (get-clause $ID0 $H1 $B1 $CL1 hypo)))) $IDL0) 
      (remove-v 
        (:: $ID) $IDL0 $IDL) 
      (rem-other-covered $IDL $Pos $Pos1) 
      (set-det)))
; 



  (= 
    (rem_other_covered  () $Pos $Pos) True)
; 

  (= 
    (rem-other-covered 
      (Cons  $ID $R) $Pos $Pos2) 
    ( (rem-other-covered $R $Pos $Pos1) 
      (get-evaluation $ID 
        (evaluation $_ $_ $P $_ $_ $_ $_ $_ $_)) 
      (remove-v $P $Pos1 $Pos2)))
; 




  (= 
    (spec_c  () $_ $_ $_ $_ $_ ()) True)
; 

  (= 
    (spec-c 
      (Cons  $V $R) $H $B $Predlist $ID $Pos $PSL) 
    ( (spec-c $R $H $B $Predlist $ID $Pos $PSL0) 
      (spec-c1 $Predlist $V $H $B $ID $Pos $PSL1) 
      (append $PSL0 $PSL1 $PSL)))
; 



  (= 
    (spec_c1  () $_ $_ $_ $_ $_ ()) True)
; 

  (= 
    (spec-c1 
      (Cons  $Pred $R) $V $H $B $ID $Pos $PSL) 
    ( (copy-term 
        (, $V $H $B) 
        (, $V1 $H1 $B1)) 
      (=.. $Lit 
        (:: $Pred $V1)) 
      (append-body 
        (= $H1 $B1) $Lit $C) 
      (store-clause $C $_ hypo $ID) 
      (eval-examples) 
      (get-evaluation $ID 
        (evaluation $_ $_ $Pos1 $_ $_ $_ $_ $_ $_)) 
      (delete-clause $ID) 
      (det-if-then-else 
        (remove-v $Pos1 $Pos Nil) 
        (, 
          (mysetof 
            (= $H2 $B2) 
            (^ $ID2 
              (^ $CL 
                (get-clause $ID2 $H2 $B2 $CL hypo))) $RestPS) 
          (= $PSL 
            (Cons  
              (with_self  
                (Cons  $C $RestPS) 
                (active)) $PSL0))) 
        (= $PSL $PSL0)) 
      (spec-c1 $R $V $H $B $ID $Pos $PSL0)))
; 





  (= 
    (gen $PS 
      (:: (with_self  (Cons  (= $H True) $RestPS) (active)))) 
    ( (store-clauses $PS hypo) 
      (eval-examples) 
      (covered-pos-examples $Cov) 
      (get-clause $ID $H $_ $_ hypo) 
      (mysetof $IDE 
        (^ $H 
          (get-example $IDE $H +)) $PH) 
      (remove-v $Cov $PH $P1) 
      (\== $P1 Nil) 
      (delete-clause $ID) 
      (mysetof 
        (= $H2 $B2) 
        (^ $ID2 
          (^ $CL2 
            (, 
              (get-clause $ID2 $H2 $B2 $CL2 hypo) 
              (delete-clause $ID2)))) $RestPS)))
; 

   






  (= 
    (l-newp $PS 
      (:: (with_self  $Clist1 (active))) $_ $_ $_ $_ $_ $_ $_ $_ $_ $_ $_ $_) 
    ( (store-clauses $PS hypo) 
      (eval-examples) 
      (get-all-clauses $Clist) 
      (correct-with-newp $Clist $Clist1)))
; 



  (= 
    (get-all-clauses (Cons  (with_self  (= $H $B) (with_self  $Pos $RV)) $R)) 
    ( (get-clause $ID $H $B $_ hypo) 
      (get-evaluation $ID 
        (evaluation $_ $_ $Pos $_ $_ $_ $_ $_ $_)) 
      (only-vars $H $HV) 
      (only-vars $B $BV) 
      (remove-v $BV $HV $RV) 
      (delete-clause $ID) 
      (get-all-clauses $R)))
; 

  (= 
    (get_all_clauses  ()) True)
; 



  (= 
    (correct_with_newp  () ()) True)
; 

  (= 
    (correct-with-newp 
      (Cons  
        (with_self  
          (= $H $B) 
          (with_self  $_ Nil)) $R) 
      (Cons  
        (= $H $B) $R1)) 
    ( (set-det) (correct-with-newp $R $R1)))
; 

  (= 
    (correct-with-newp 
      (Cons  
        (with_self  
          (= $H $B) 
          (with_self  $Pos $RV)) $R) $R2) 
    ( (correct-with-newp $R $R1) 
      (c-with-newp $RV $B $B1 $Newps) 
      (instances $Newps $Pos $H $Elist) 
      (initialize1 $Elist $HL) 
      (append 
        (Cons  
          (= $H $B1) $R1) $HL $R2)))
; 



  (= 
    (c_with_newp  () $B $B ()) True)
; 

  (= 
    (c-with-newp 
      (:: $V) True $New 
      (:: $New)) 
    ( (set-det) 
      (gensym newp $Newp) 
      (=.. $New 
        (:: $Newp $V))))
; 

  (= 
    (c-with-newp 
      (Cons  $V $R) $B 
      (, $New $B1) 
      (Cons  $New $R1)) 
    ( (c-with-newp $R $B $B1 $R1) 
      (gensym newp $Newp) 
      (=.. $New 
        (:: $Newp $V))))
; 



  (= 
    (instances  () $_ $_ ()) True)
; 

  (= 
    (instances 
      (Cons  $New $R) $Pos $H 
      (Cons  $NewE $R1)) 
    ( (instances $R $Pos $H $R1) (mysetof $New (^ $I (^ $J (^ $H (, (member (with_self  $I $H) $Pos) (store-ex $New + $J))))) $NewE)))
; 



  (= 
    (output (:: $CL)) 
    ( (mysetof $PN 
        (^ $H 
          (^ $B 
            (^ $R 
              (, 
                (member 
                  (= $H $B) $CL) 
                (=.. $H 
                  (Cons  $PN $R)) 
                (is-newpred $PN))))) $Newpredlist) 
      (minimize-output $Newpredlist $CL $CL1) 
      (store-clauses $CL1 rul) 
      (show-kb)))
; 



  (= 
    (minimize_output  () $CL $CL) True)
; 

  (= 
    (minimize-output 
      (Cons  $P $R) $CL $CL2) 
    ( (findall $P1 
        (, 
          (member $P1 $R) 
          (type-equal $P $P1 
            (:: (with_self  $P $P1)) $CL)) $P1L) 
      (replace-t $CL $P1L $P $CL1) 
      (findall $I 
        (, 
          (member $NP $P1L) 
          (get-example $I $NPP $_) 
          (=.. $NPP 
            (Cons  $NP $_))) $IDL) 
      (delete-all $IDL) 
      (remove-v $P1L $R $R1) 
      (minimize-output $R1 $CL1 $CL2)))
; 


