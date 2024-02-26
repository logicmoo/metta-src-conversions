;
; 


  !(module constrained-clauses 
    (:: (/ learn-constrained 0)))
; 


;
; 

  !(meta-predicate (ccl-newp + + : : : : : : : : : : : :))
; 



;
; 

  !(use-module 
    (home kb) 
    (:: 
      (/ get-example 3) 
      (/ get-clause 5) 
      (/ get-evaluation 2) 
      (/ delete-clause 1) 
      (/ delete-example 1) 
      (/ store-clauses 2) 
      (/ store-clause 4) 
      (/ store-ex 3) 
      (/ delete-all 1) 
      (/ known 6)))
; 

  !(use-module 
    (home gencon) 
    (:: 
      (/ gilppi 12) 
      (/ gilppi 14)))
; 

  !(use-module 
    (home argument-types) 
    (:: 
      (/ types-of 3) 
      (/ type-restriction 2)))
; 
 
  !(use-module 
    (home clause-heads) 
    (:: (/ heads 1)))
; 
                    
  !(use-module 
    (home show-utils) 
    (:: 
      (/ show-kb 0) 
      (/ write-list 1)))
; 
                     
  !(use-module 
    (home evaluation) 
    (:: 
      (/ eval-examples 0) 
      (/ complete-chk 0) 
      (/ correct-chk 0) 
      (/ covered-pos-examples 1) 
      (/ clear-evaluation 0) 
      (/ covered-neg-examples 1) 
      (/ fp-hyp 1) 
      (/ change-evaluated 1)))
; 

  !(use-module 
    (home lgg) 
    (:: (/ set-lgg 2)))
; 

  !(use-module 
    (home div-utils) 
    (:: 
      (/ best 2) 
      (/ remove-v 3) 
      (/ mysetof 3) 
      (/ make-unique 2) 
      (/ body2list 2)))
; 

  !(use-module 
    (home var-utils) 
    (:: 
      (/ only-vars 2) 
      (/ clause-terms 2)))
; 

  !(use-module 
    (home tdref-it) 
    (:: (/ refinement-add-body-literal 3)))
; 

  !(use-module 
    (home newpred) 
    (:: (/ specialize-with-newpred 7)))
; 

  !(use-module 
    (home interpreter) 
    (:: (/ prooftrees 3)))
; 

  !(use-module-if-exists 
    (library basics) 
    (:: (/ member 2)))
; 

  !(use-module-if-exists 
    (library strings) 
    (:: (/ gensym 2)))
; 

  !(use-module-if-exists 
    (library sets) 
    (:: (/ subset 2)))
; 

  !(use-module-if-exists 
    (library subsumes) 
    (:: (/ variant 2)))
; 

;
; 

;
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (learn-constrained) 
    (gilppi ccinitialize ccstop-c ccquality-c ccupdate ccselect ccadd ccfilter ccone-of ccspec ccgen ccl-newp ccoutput))
; 




  (= 
    (ccinitialize (:: (, (with_self  $HL (with_self  (:: $S1 $S2) $HL)) (active)))) 
    ( (heads $HL) 
      (store-clauses $HL hypo) 
      (eval-examples) 
      (findall 
        (, $ID $H $B $CL $E) 
        (, 
          (get-clause $ID $H $B $CL hypo) 
          (get-evaluation $ID $E) 
          (delete-clause $ID)) $S1) 
      (findall 
        (, $ID1 $M1 $T1) 
        (remove-symbol  &self 
          (prooftrees  $ID1 $M1 $T1)) $S2)))
; 



  (= 
    (ccinitialize-newp (:: (, (with_self  $PS (with_self  (:: $S1 $S2) $PS)) (active)))) 
    ( (heads $HL) 
      (store-clauses $HL hypo) 
      (eval-examples) 
      (findall 
        (= $H0 $B0) 
        (get-clause $_ $H0 $B0 $_ hypo) $PS) 
      (findall 
        (, $ID $H $B $CL $E) 
        (, 
          (get-clause $ID $H $B $CL hypo) 
          (get-evaluation $ID $E) 
          (delete-clause $ID)) $S1) 
      (findall 
        (, $ID1 $M1 $T1) 
        (remove-symbol  &self 
          (prooftrees  $ID1 $M1 $T1)) $S2)))
; 



  (= 
    (ccstop_c  
      ($_)) True)
; 




  (= 
    (ccquality-c (with_self  $_ (with_self  (:: $SC $SP) $_))) 
    ( (sclauses $SC) 
      (sprooftrees $SP) 
      (det-if-then-else 
        (, 
          (complete-chk) 
          (correct-chk)) True fail)))
; 



  (= 
    (ccupdate  $L $L) True)
; 



  (= 
    (sclauses  ()) True)
; 

  (= 
    (sclauses (Cons  (, $ID $H $B $CL $E) $R)) 
    ( (sclauses $R) (add-symbol  &self (:  kb (known  $ID $H $B $CL hypo $E)))))
; 



  (= 
    (sprooftrees Nil) 
    (change-evaluated yes))
; 

  (= 
    (sprooftrees (Cons  (, $ID $M $T) $R)) 
    ( (sprooftrees $R) (add-symbol  &self (:  interpreter (prooftrees  $ID $M $T)))))
; 
   



  (= 
    (ccselect $Partial_Sols $PS active $Partial_Sols1) 
    (select-active $Partial_Sols $PS $Partial_Sols1))
; 

  (= 
    (ccselect $Partial_Sols $PS passive $Partial_Sols) 
    (select-passive $Partial_Sols $PS))
; 



  (= 
    (select_active  
      (Cons  
        (,  $PS active) $R) $PS 
      (Cons  
        (,  $PS passive) $R)) True)
; 

  (= 
    (select-active 
      (Cons  $P $R) $PS 
      (Cons  $P $R1)) 
    (select-active $R $PS $R1))
; 



  (= 
    (select-passive $Partial_Sols $PS) 
    ( (candidates $Partial_Sols Nil $Partial_Sols1) (best $Partial_Sols1 (- $_ $PS))))
; 



  (= 
    (candidates  () $PSS $PSS) True)
; 

  (= 
    (candidates 
      (Cons  
        (, 
          (with_self  $PS 
            (with_self  
              (:: $SC $SP) $Hist)) $_) $R) $PSS $PSS2) 
    ( (candidates $R $PSS $PSS1) 
      (sclauses $SC) 
      (sprooftrees $SP) 
      (det-if-then-else complete-chk 
        (, 
          (covered-neg-examples $N) 
          (length $N $NN) 
          (ccins 
            (- $NN 
              (with_self  $PS 
                (with_self  
                  (:: $SC $SP) $Hist))) $PSS1 $PSS2)) 
        (= $PSS1 $PSS2)) 
      (clear-evaluation) 
      (findall $ID 
        (, 
          (get-clause $ID $_ $_ $_ hypo) 
          (delete-clause $ID)) $_)))
; 



  (= 
    (ccins 
      (- $N $PS) 
      (Cons  
        (- $N1 $PS1) $R) 
      (Cons  
        (- $N1 $PS1) $R1)) 
    ( (> $N $N1) 
      (set-det) 
      (ccins 
        (- $N $PS) $R $R1)))
; 

  (= 
    (ccins  $X $L 
      (Cons  $X $L)) True)
; 



  (= 
    (ccadd $Partial_Sols $PSL $Partial_Sols1) 
    (append $Partial_Sols $PSL $Partial_Sols1))
; 



  (= 
    (ccfilter $Partial_Sols $Partial_Sols1) 
    (ccfilter $Partial_Sols Nil $Partial_Sols1))
; 


  (= 
    (ccfilter  () $Partial_Sols $Partial_Sols) True)
; 

  (= 
    (ccfilter 
      (Cons  $X $R) $Partial_Sols $Partial_Sols2) 
    ( (ccfilter1 $X $Partial_Sols $Partial_Sols1) (ccfilter $R $Partial_Sols1 $Partial_Sols2)))
; 



  (= 
    (ccfilter1  $X () 
      ($X)) True)
; 

  (= 
    (ccfilter1 
      (, 
        (with_self  $PS 
          (with_self  $DB $Hist)) $M) 
      (Cons  
        (, 
          (with_self  $PS1 
            (with_self  $DB1 $Hist1)) $M1) $R) 
      (Cons  
        (, 
          (with_self  $PS1 
            (with_self  $DB1 $Hist1)) $M1) $R1)) 
    (det-if-then-else 
      (, 
        (= $M $M1) 
        (clause-variants $PS $PS1)) 
      (= $R1 $R) 
      (ccfilter1 
        (, 
          (with_self  $PS 
            (with_self  $DB $Hist)) $M) $R $R1)))
; 



  (= 
    (ccone-of $_ $M) 
    (det-if-then-else complete-chk 
      (= $M spec) 
      (= $M gen)))
; 



  (= 
    (ccspec 
      (with_self  $_ 
        (with_self  $_ $Hist)) $PSL) 
    ( (ccspec1 $Hist $PSL) 
      (write-l $PSL) 
      (findall $ID 
        (get-clause $ID $_ $_ $_ hypo) $IDL) 
      (delete-all $IDL)))
; 



  (= 
    (ccspec1 $Hist $PSL) 
    ( (covered-neg-examples $NIDs) 
      (fp-hyp $OR) 
      (best $OR 
        (with_self  $I $_)) 
      (get-clause $I $H $B $_ hypo) 
      (get-evaluation $I 
        (evaluation $_ $_ $Pos $_ $_ $_ $_ $_ $_)) 
      (\== $Pos Nil) 
      (clause-terms 
        (= $H $B) $Terms) 
      (types-of $Terms 
        (= $H $B) $TTerms) 
      (refinement-add-body-literal 
        (= $H $B) $TTerms $CL) 
      (length $CL $CLl) 
      (nl) 
      (write 'no refs: ') 
      (write $CLl) 
      (nl) 
      (nl) 
      (check-refinements $CL $NIDs $I $Hist 
        (= $H $B) $PSL)))
; 



  (= 
    (check-refinements $CL $NIDs $I $Hist $C $PSL) 
    ( (delete-clause $I) 
      (clear-evaluation) 
      (check-refinements $CL $NIDs $I $Hist $PSL) 
      (store-clause $C $_ hypo $I) 
      (set-det)))
; 


  (= 
    (check_refinements  () $_ $_ $_ ()) True)
; 

  (= 
    (check-refinements 
      (Cons  $C $R) $NID $I $Hist $PSL2) 
    ( (det-if-then-else 
        (, 
          (constrained $C) 
          (not (clause-in $C $Hist $_))) 
        (, 
          (store-clause $C $_ hypo $I) 
          (eval-examples) 
          (get-evaluation $I 
            (evaluation $_ $_ $Pos $_ $_ $_ $_ $_ $_)) 
          (covered-neg-examples $NID1) 
          (det-if-then-else 
            (, 
              (genuine-subset $NID1 $NID) 
              (\== $Pos Nil)) 
            (, 
              (findall 
                (= $H $B) 
                (get-clause $ID $H $B $_ hypo) $PS) 
              (findall 
                (, $ID $H $B $CL $E) 
                (, 
                  (get-clause $ID $H $B $CL hypo) 
                  (get-evaluation $ID $E)) $S1) 
              (findall 
                (, $ID1 $M1 $T1) 
                (remove-symbol  &self 
                  (prooftrees  $ID1 $M1 $T1)) $S2) 
              (= $PSL2 
                (Cons  
                  (, 
                    (with_self  $PS 
                      (with_self  
                        (:: $S1 $S2) 
                        (Cons  $C $Hist))) 
                    (active)) $PSL1))) 
            (, 
              (= $PSL2 $PSL1) 
              (clear-evaluation))) 
          (delete-clause $I)) 
        (= $PSL2 $PSL1)) (check-refinements $R $NID $I $Hist $PSL1)))
; 




  (= 
    (constrained (= $H $B)) 
    ( (only-vars $H $HV) 
      (only-vars $B $BV) 
      (remove-v $HV $BV Nil)))
; 



  (= 
    (clause-in 
      (= $H True) 
      (Cons  
        (= $H1 $B1) $R) $R2) 
    ( (set-det) (det-if-then-else (, (variant $H $H1) (== $B1 True)) (= $R2 $R) (, (clause-in (= $H True) $R $R1) (= $R2 (Cons  (= $H1 $B1) $R1))))))
; 

  (= 
    (clause-in 
      (= $H $B) 
      (Cons  
        (= $H1 $B1) $R) $R2) 
    ( (body2list $B $BL) 
      (body2list $B1 $B1L) 
      (set-det) 
      (det-if-then-else 
        (, 
          (variant $H $H1) 
          (length $BL $N) 
          (length $B1L $N)) 
        (, 
          (= $H $H1) 
          (det-if-then-else 
            (c-in $BL $B1L) 
            (= $R2 $R) 
            (, 
              (clause-in 
                (= $H $B) $R $R1) 
              (= $R2 
                (Cons  
                  (= $H1 $B1) $R1))))) 
        (, 
          (clause-in 
            (= $H $B) $R $R1) 
          (= $R2 
            (Cons  
              (= $H1 $B1) $R1))))))
; 



  (= 
    (c_in  () ()) True)
; 

  (= 
    (c-in 
      (Cons  $L $R) $B) 
    ( (remove-v 
        (:: $L) $B $B1) 
      (set-det) 
      (c-in $R $B1)))
; 



  (= 
    (clause_variants  () ()) True)
; 

  (= 
    (clause-variants 
      (Cons  $C $R) $CL) 
    ( (clause-in $C $CL $CL1) (clause-variants $R $CL1)))
; 



  (= 
    (genuine-subset $L1 $L2) 
    ( (length $L1 $L1n) 
      (length $L2 $L2n) 
      (< $L1n $L2n) 
      (subset $L1 $L2)))
; 



  (= 
    (ccgen 
      (with_self  $_ 
        (with_self  $_ $Hist)) 
      (:: (, (with_self  $PS (with_self  (:: $S1 $S2) $Hist)) (active)))) 
    ( (covered-pos-examples $Cov) 
      (findall 
        (with_self  $ID1 $E1) 
        (, 
          (member $ID1 $Cov) 
          (get-example $ID1 $E1 $_)) $Cov1) 
      (get-clause $ID $H $B $_ hypo) 
      (mysetof 
        (with_self  $IDE $H) 
        (get-example $IDE $H +) $PH) 
      (remove-v $Cov1 $PH $P1) 
      (\== $P1 Nil) 
      (ccgen2 $ID $H $B $PH $B1) 
      (store-clause 
        (= $H $B1) $_ hypo $_) 
      (eval-examples) 
      (findall 
        (, $ID2 $H2 $B2 $CL2 $E2) 
        (, 
          (get-clause $ID2 $H2 $B2 $CL2 hypo) 
          (get-evaluation $ID2 $E2)) $S1) 
      (findall 
        (, $ID3 $M3 $T3) 
        (remove-symbol  &self 
          (prooftrees  $ID3 $M3 $T3)) $S2) 
      (findall 
        (= $H4 $B4) 
        (, 
          (get-clause $ID4 $H4 $B4 $_ hypo) 
          (delete-clause $ID4)) $PS) 
      (write-l (:: (, (with_self  $PS (with_self  (:: $S1 $S2) $Hist)) (active))))))
; 



  (= 
    (ccgen2 $ID $H $B $PH $B1) 
    ( (delete-clause $ID) 
      (body2list $B $BL) 
      (ccgen3 $BL $ID $H $PH $BL1) 
      (det-if-then-else 
        (== $BL1 Nil) 
        (= $B1 True) 
        (body2list $B1 $BL1)) 
      (store-clause 
        (= $H $B) $_ hypo $ID)))
; 



  (= 
    (ccgen3  () $_ $_ $_ ()) True)
; 

  (= 
    (ccgen3 
      (Cons  
        (with_self  $L $M) $R) $ID $H $PH $B) 
    ( (ccgen3 $R $ID $H $PH $B1) 
      (store-clause 
        (= $H $L) $_ hypo $ID) 
      (eval-examples) 
      (get-evaluation $ID 
        (evaluation $_ $_ $Pos $_ $_ $_ $_ $_ $_)) 
      (delete-clause $ID) 
      (det-if-then-else 
        (remove-v $Pos $PH Nil) 
        (= $B 
          (Cons  
            (with_self  $L $M) $B1)) 
        (= $B $B1))))
; 

   


  (= 
    (ccl-newp 
      (with_self  $_ 
        (with_self  $_ $Hist)) 
      (:: (, (with_self  $PS (with_self  (:: $S1 $S2) $Hist)) (active))) $Initialize $Stop_C $Quality_C $Update $Select $Add $Filter $One_of $Spec $Gen $L_newp $Output) 
    ( (fp-hyp $OR) 
      (best $OR 
        (with_self  $I $_)) 
      (get-clause $I $H $B $_ hypo) 
      (get-evaluation $I 
        (evaluation $_ $_ $Pos $_ $Neg $_ $_ $_ $_)) 
      (specialize-with-newpred 
        (= $H $B) $Pos $Neg $NC $NPos $NNeg $NType) 
      (delete-clause $I) 
      (delete-old-ex $Elist) 
      (store-newp-ex $NPos $NNeg $IDL0) 
      (make-unique $IDL0 $IDL) 
      (add-symbol  &self $NType) 
      (store-clause $NC $_ hypo $I) 
      (ccinitialize-newp $PSL) 
      (gilppi $PSL Nil $Initialize $Stop_C $Quality_C $Update $Select $Add $Filter $One_of $Spec $Gen $L_newp $Output) 
      (delete-all $IDL) 
      (store-old-ex $Elist) 
      (findall $ID0 
        (, 
          (get-clause $ID0 $H0 $B0 $_ constrained-clause) 
          (delete-clause $ID0) 
          (store-clause 
            (= $H0 $B0) $_ hypo $ID0)) $_) 
      (eval-examples) 
      (findall 
        (, $ID3 $H3 $B3 $CL3 $E3) 
        (, 
          (get-clause $ID3 $H3 $B3 $CL3 hypo) 
          (get-evaluation $ID3 $E3)) $S1) 
      (findall 
        (, $ID1 $M1 $T1) 
        (remove-symbol  &self 
          (prooftrees  $ID1 $M1 $T1)) $S2) 
      (findall 
        (= $H2 $B2) 
        (, 
          (get-clause $ID2 $H2 $B2 $_ hypo) 
          (delete-clause $ID2)) $PS)))
; 



  (= 
    (delete-old-ex (Cons  (ex $ID $F $M) $R)) 
    ( (get-example $ID $F $M) 
      (delete-example $ID) 
      (delete-old-ex $R)))
; 

  (= 
    (delete_old_ex  ()) True)
; 



  (= 
    (store_old_ex  ()) True)
; 

  (= 
    (store-old-ex (Cons  (ex $ID $F $M) $R)) 
    ( (store-ex $F $M $ID) (store-old-ex $R)))
; 



  (= 
    (store_newp_ex  () () ()) True)
; 

  (= 
    (store-newp-ex Nil 
      (Cons  $F $R) 
      (Cons  $ID $R1)) 
    ( (store-ex $F - $ID) (store-newp-ex Nil $R $R1)))
; 

  (= 
    (store-newp-ex 
      (Cons  $F $R) $L 
      (Cons  $ID $R1)) 
    ( (store-ex $F + $ID) (store-newp-ex $R $L $R1)))
; 



  (= 
    (ccoutput (:: (with_self  $CL (with_self  $_ $_)))) 
    ( (findall $ID 
        (get-clause $ID $_ $_ $_ hypo) $IDL) 
      (delete-all $IDL) 
      (store-clauses $CL constrained-clause) 
      (nl) 
      (nl) 
      (write 'gilppi completed..........') 
      (nl) 
      (nl) 
      (show-kb)))
; 




  (= 
    (write-l (Cons  (, (with_self  $CL (with_self  $_ $_)) $_) $R)) 
    ( (write-list $CL) 
      (nl) 
      (write-l $R)))
; 

  (= 
    (write_l  ()) True)
; 

