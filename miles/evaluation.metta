;
; 



  !(module evaluation 
    (:: 
      (/ covered-pos-examples 1) 
      (/ covered-neg-examples 1) 
      (/ all-covered-examples 1) 
      (/ complexity 2) 
      (/ clear-evaluation 0) 
      (/ evaluated 1) 
      (/ change-evaluated 1) 
      (/ eval-examples 0) 
      (/ eval-pos-examples 1) 
      (/ complete-chk 0) 
      (/ correct-chk 0) 
      (/ fp 1) 
      (/ fpo 1) 
      (/ fp-hyp 1) 
      (/ ip 1) 
      (/ herbrand-base-ff 1) 
      (/ ivonTunterE 1) 
      (/ ivonBundE 1) 
      (/ code-length 2) 
      (/ encoding-length-examples 1) 
      (/ encoding-length-clause 2)))
; 


;
; 

  !(use-module 
    (home div-utils) 
    (:: 
      (/ make-unique 2) 
      (/ insert-unique 4) 
      (/ sort-by-length 3) 
      (/ mysetof 3) 
      (/ remove 3) 
      (/ append-all 2) 
      (/ sum 2) 
      (/ identical-make-unique 2) 
      (/ best 2) 
      (/ remove-variant 3) 
      (/ make-unique 2) 
      (/ fak 2) 
      (/ fak1 3) 
      (/ nueberk 3) 
      (/ log2 2) 
      (/ log2nueberk 3) 
      (/ sum-of-logs 3)))
; 

  !(use-module 
    (home environment) 
    (:: (/ ask-for 1)))
; 

  !(use-module 
    (home var-utils) 
    (:: 
      (/ term-size 2) 
      (/ vars 2) 
      (/ skolemize 3)))
; 

  !(use-module 
    (home kb) 
    (:: 
      (/ get-example 3) 
      (/ ex 3) 
      (/ known 6) 
      (/ assertallz 1) 
      (/ get-predlist 1) 
      (/ get-evaluation 2) 
      (/ delete-example 1) 
      (/ delete-clause 1) 
      (/ get-clause 5)))
; 

  !(use-module 
    (home interpreter) 
    (:: 
      (/ prooftrees 3) 
      (/ solve-once 3) 
      (/ proof-close 2) 
      (/ solve 3) 
      (/ ip-part1 2) 
      (/ ip-part2 3)))
; 

  !(use-module-if-exists 
    (library basics) 
    (:: (/ member 2)))
; 

  !(use-module-if-exists 
    (library subsumes) 
    (:: 
      (/ subsumes-chk 2) 
      (/ variant 2)))
; 

  !(use-module-if-exists 
    (library occurs) 
    (:: (/ sub-term 2)))
; 

  !(use-module-if-exists 
    (library math) 
    (:: (/ pow 3)))
; 



;
; 

;
; 




  !(dynamic (/ evaluated 1))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (ip $UA_List) 
    ( (det-if-then-else 
        (evaluated no) eval-examples True) 
      (mysetof $E 
        (^ $I 
          (^ $Trees 
            (, 
              (get-example $I $E +) 
              (prooftrees $I fail $Trees)))) $Elist) 
      (set-det) 
      (ip-list $Elist Nil $UA_List1) 
      (make-unique $UA_List1 $UA_List)))
; 



  (= 
    (ip_list  () $L $L) True)
; 

  (= 
    (ip-list 
      (Cons  $E $R) $L $L2) 
    ( (ip0 $E $UAs) 
      (append $L $UAs $L1) 
      (ip-list $R $L1 $L2)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (ip0 $Goal $UAs) 
    ( (setof $Proof 
        (^ $Goal 
          (ip-part1 $Goal $Proof)) $Proofs0) 
      (append-all $Proofs0 $Proofs1) 
      (proof-close $Proofs1 $Proofs) 
      (set-det) 
      (ip-part2 $Proofs $Goal $UAs0) 
      (make-unique $UAs0 $UAs)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (fp $OR) 
    ( (det-if-then-else 
        (evaluated no) eval-examples True) 
      (bagof $TL 
        (^ $I 
          (^ $E 
            (^ $Trees 
              (^ $P 
                (, 
                  (get-example $I $E -) 
                  (prooftrees $I success $Trees) 
                  (member $P $Trees) 
                  (fp $P Nil $TL)))))) $TList) 
      (collect-indices $TList Nil $Indices) 
      (or-subsets $Indices $TList $OR_List) 
      (set-det) 
      (best $OR_List $OR)))
; 

  (= 
    (fp  ()) True)
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (fp 
      (:: sys $_ $_) $L $L) 
    (set-det))
; 

  (= 
    (fp 
      (:: $I $A $SG) $L $L2) 
    ( (fp-list $SG $L $L1) (insert-unique $I $A $L1 $L2)))
; 



  (= 
    (fp_list  () $L $L) True)
; 

  (= 
    (fp-list 
      (Cons  $G $R) $L $L2) 
    ( (fp-list $R $L $L1) (fp $G $L1 $L2)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (fp-hyp $OR) 
    ( (det-if-then-else 
        (evaluated no) eval-examples True) 
      (bagof $TL 
        (^ $I 
          (^ $E 
            (^ $Trees 
              (^ $P 
                (, 
                  (get-example $I $E -) 
                  (prooftrees $I success $Trees) 
                  (member $P $Trees) 
                  (fp-hyp $P Nil $TL)))))) $TList) 
      (collect-indices $TList Nil $Indices) 
      (or-subsets $Indices $TList $OR_List) 
      (set-det) 
      (best $OR_List $OR)))
; 

  (= 
    (fp_hyp  ()) True)
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (fp-hyp 
      (:: sys $_ $_) $L $L) 
    (set-det))
; 

  (= 
    (fp-hyp 
      (:: $I $A $SG) $L $L2) 
    ( (fp-hyp-list $SG $L $L1) (det-if-then-else (get-clause $I $_ $_ $_ hypo) (insert-unique $I $A $L1 $L2) (= $L2 $L1))))
; 



  (= 
    (fp_hyp_list  () $L $L) True)
; 

  (= 
    (fp-hyp-list 
      (Cons  $G $R) $L $L2) 
    ( (fp-hyp-list $R $L $L1) (fp-hyp $G $L1 $L2)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (fpo $OR) 
    ( (det-if-then-else 
        (evaluated no) eval-examples True) 
      (bagof $TL 
        (^ $I 
          (^ $E 
            (^ $Trees 
              (^ $P 
                (, 
                  (get-example $I $E -) 
                  (prooftrees $I success $Trees) 
                  (member $P $Trees) 
                  (fpo $P Nil $TL $_)))))) $TList) 
      (collect-indices $TList Nil $Indices) 
      (or-subsets $Indices $TList $OR_List) 
      (set-det) 
      (best $OR_List $OR)))
; 

  (= 
    (fpo  ()) True)
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (fpo 
      (:: sys $_ $_) $L $L ok) 
    (set-det))
; 

  (= 
    (fpo 
      (:: $I $A $SG) $L $L1 $X) 
    (det-if-then-else 
      (ask-for $A) 
      (fpo-list $SG $L $L1 $X) 
      (, 
        (insert-unique $I $A $L $L1) 
        (= $X not-ok))))
; 



  (= 
    (fpo_list  () $L $L ok) True)
; 

  (= 
    (fpo-list 
      (Cons  $G $R) $L $L2 $X) 
    ( (fpo $G $L $L1 $Xg) (det-if-then-else (= $Xg ok) (fpo-list $R $L1 $L2 $X) (= $X $Xg))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (collect_indices  () $L $L) True)
; 

  (= 
    (collect-indices 
      (Cons  $X $R) $L $L2) 
    ( (c-indices $X $L $L1) (collect-indices $R $L1 $L2)))
; 



  (= 
    (c_indices  () $L $L) True)
; 

  (= 
    (c-indices 
      (Cons  
        (with_self  $I $_) $R) $L $L2) 
    ( (c-indices $R $L $L1) (det-if-then-else (member $I $L1) (= $L2 $L1) (= $L2 (Cons  $I $L1)))))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (or-subsets $IX $TL $ORL) 
    ( (initialize-or-subsets $IX $IX $TL $TL1) 
      (or-all-subsets $TL1 Nil $TL2) 
      (sort-by-length $TL2 Nil $ORL)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (initialize_or_subsets  () $_ $_ ()) True)
; 

;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  miles/evaluation.pl 478 41 15079))



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (remove_conjuncts  $_ () () $A $A) True)
; 

  (= 
    (remove-conjuncts $I 
      (Cons  $X $R) $R1 $A $A2) 
    ( (member 
        (with_self  $I $E) $X) 
      (set-det) 
      (append $E $A $A0) 
      (identical-make-unique $A0 $A1) 
      (remove-conjuncts $I $R $R1 $A1 $A2)))
; 

  (= 
    (remove-conjuncts $I 
      (Cons  $X $R) 
      (Cons  $X $R1) $A $A1) 
    (remove-conjuncts $I $R $R1 $A $A1))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 


;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  miles/evaluation.pl 541 20 17217))

;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  miles/evaluation.pl 543 20 17286))


  (= 
    (or_all_subsets  () $L $L) True)
; 



  (= 
    (or_asubsets  () $_ $_ $_ ()) True)
; 

;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  miles/evaluation.pl 550 39 17474))



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (eval-pos-examples $Exlist) 
    ( (remove-all-symbols  &self 
        (prooftrees  $_ $_ $_)) (findall (with_self  $I $Fact) (, (ex $I $Fact +) (solve-once $Fact fail $_)) $Exlist)))
; 
        



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (eval-examples) 
    ( (evaluated yes) (set-det)))
; 


  (= 
    (eval-examples) 
    ( (remove-all-symbols  &self 
        (prooftrees  $_ $_ $_)) 
      (eval-examples1) 
      (set-det) 
      (change-evaluated yes)))
; 


  (= 
    (eval-examples1) 
    ( (ex $I $Fact $_) 
      (solve $Fact $M $Proofs) 
      (add-symbol  &self 
        (prooftrees  $I $M $Proofs)) 
      (fail)))
; 

  (= 
    (eval-examples1) 
    ( (bagof 
        (with_self  $I $Proofs) 
        (prooftrees $I success $Proofs) $Plist) 
      (findall 
        (known $J $H $B $Clist $L $_) 
        (, 
          (get-clause $J $H $B $Clist $L) 
          (delete-clause $J)) $Klist) 
      (compute-evaluation $Klist $Plist $Klist1) 
      (assertallz $Klist1) 
      (set-det)))
; 

  (= eval_examples1 True)
; 
               ;
; 



  (= 
    (clear-evaluation) 
    ( (remove-all-symbols  &self 
        (prooftrees  $_ $_ $_)) (change-evaluated no)))
; 



  (= 
    (change-evaluated $X) 
    ( (remove-all-symbols  &self 
        (evaluated  $_)) (add-symbol  &self (evaluated  $X))))
; 
 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (correct-chk) 
    (det-if-then-else 
      (evaluated no) 
      (det-if-then-else 
        (, 
          (ex $ID $Fact -) 
          (solve-once $Fact success $_)) fail True) 
      (det-if-then-else 
        (, 
          (ex $ID $Fact -) 
          (prooftrees $ID success $_)) fail True)))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (complete-chk) 
    (det-if-then-else 
      (evaluated no) 
      (det-if-then-else 
        (, 
          (ex $ID $Fact +) 
          (solve-once $Fact fail $_)) fail True) 
      (det-if-then-else 
        (, 
          (ex $ID $Fact +) 
          (prooftrees $ID fail $_)) fail True)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (compute_evaluation  () $_ ()) True)
; 

  (= 
    (compute-evaluation 
      (Cons  
        (known $I $H $B $Clist $O $_) $R) $Plist 
      (Cons  
        (known $I $H $B $Clist $O 
          (evaluation $RA $NPos $Pos $NNeg $Neg $UNPos $UPos $UNNeg $UNeg)) $R1)) 
    ( (compute-evaluation $R $Plist $R1) (compute-eval $Plist $I $RA $NPos $Pos $NNeg $Neg $UNPos $UPos $UNNeg $UNeg)))
; 




  (= 
    (compute_eval  () $_ 0 0 () 0 () 0 () 0 ()) True)
; 

  (= 
    (compute-eval 
      (Cons  
        (with_self  $I $Proofs) $R) $J $RA $NPos $Pos $NNeg $Neg $UNPos $UPos $UNNeg $UNeg) 
    ( (compute-eval $R $J $RA0 $NPos0 $Pos0 $NNeg0 $Neg0 $UNPos0 $UPos0 $UNNeg0 $UNeg0) (det-if-then-else (ex $I $_ -) (, (compute-eval $Proofs t $I $J $RA0 $RA $NNeg0 $NNeg $Neg0 $Neg $UNNeg0 $UNNeg $UNeg0 $UNeg) (= $NPos $NPos0) (= $Pos $Pos0) (= $UNPos $UNPos0) (= $UPos $UPos0)) (, (compute-eval $Proofs t $I $J $RA0 $RA $NPos0 $NPos $Pos0 $Pos $UNPos0 $UNPos $UPos0 $UPos) (= $NNeg $NNeg0) (= $Neg $Neg0) (= $UNNeg $UNNeg0) (= $UNeg $UNeg0)))))
; 


  (= 
    (compute_eval  () $_ $_ $_ $RA $RA $N $N $L $L $UN $UN $UL $UL) True)
; 

  (= 
    (compute-eval 
      (Cons  
        (:: $I $H $B) $R) $T $K $J $RA $RA3 $N $N3 $L $L3 $UN $UN3 $UL $UL3) 
    ( (compute-eval $R $T $K $J $RA $RA1 $N $N1 $L $L1 $UN $UN1 $UL $UL1) 
      (compute-eval $B b $K $J $RA1 $RA2 $N1 $N2 $L1 $L2 $UN1 $UN2 $UL1 $UL2) 
      (det-if-then-else 
        (== $I $J) 
        (, 
          (is $RA3 
            (+ $RA2 1)) 
          (det-if-then-else 
            (== $T t) 
            (det-if-then-else 
              (member 
                (with_self  $_ $H) $L2) 
              (, 
                (= $L3 $L2) 
                (= $N3 $N2) 
                (= $UN3 $UN2) 
                (= $UL3 $UL2)) 
              (, 
                (is $N3 
                  (+ $N2 1)) 
                (= $L3 
                  (Cons  
                    (with_self  $K $H) $L2)) 
                (= $UN3 $UN2) 
                (= $UL3 $UL2))) 
            (det-if-then-else 
              (member 
                (with_self  $_ $H) $UL2) 
              (, 
                (= $L3 $L2) 
                (= $N3 $N2) 
                (= $UN3 $UN2) 
                (= $UL3 $UL2)) 
              (, 
                (is $UN3 
                  (+ $UN2 1)) 
                (= $UL3 
                  (Cons  
                    (with_self  $K $H) $UL2)) 
                (= $N3 $N2) 
                (= $L3 $L2))))) 
        (, 
          (= $RA3 $RA2) 
          (= $L3 $L2) 
          (= $N3 $N2) 
          (= $UN3 $UN2) 
          (= $UL3 $UL2)))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (covered-pos-examples $Bag) 
    ( (det-if-then-else 
        (evaluated no) eval-examples True) 
      (findall $ID 
        (, 
          (get-example $ID $_ +) 
          (prooftrees $ID success $_)) $Bag) 
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



  (= 
    (covered-neg-examples $Bag) 
    ( (det-if-then-else 
        (evaluated no) eval-examples True) 
      (findall $ID 
        (, 
          (get-example $ID $_ -) 
          (prooftrees $ID success $_)) $Bag) 
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



  (= 
    (all-covered-examples $Bag) 
    ( (det-if-then-else 
        (evaluated no) eval-examples True) 
      (findall $ID 
        (, 
          (get-example $ID $_ $_) 
          (prooftrees $ID success $_)) $Bag) 
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
    (complexity $I $C) 
    ( (integer $I) 
      (get-clause $I $_ $_ $Clause $_) 
      (compute-complexity $Clause $C) 
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


  (= 
    (complexity $Clause $C) 
    ( (= $Clause 
        (Cons  
          (with_self  $H 
            (p)) $_)) 
      (compute-complexity $Clause $C) 
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

 
  (= 
    (complexity 
      (Cons  $ID $List) $C) 
    ( (integer $ID) 
      (findall $Com 
        (, 
          (member $I 
            (Cons  $ID $List)) 
          (get-clause $I $_ $_ $Clause $_) 
          (compute-complexity $Clause $Com)) $Bag) 
      (sum $Bag $C) 
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


  (= 
    (complexity $Term $Complexity) 
    ( (term-size $Term $Complexity) (set-det)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (complexity usr $C) 
    ( (findall $I 
        (, 
          (get-clause $_ $_ $_ $Clause usr) 
          (compute-complexity $Clause $I)) $Bag) 
      (sum $Bag $C) 
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


  (= 
    (complexity examples $C) 
    ( (findall $I 
        (, 
          (get-example $_ $Clause $_) 
          (compute-complexity $Clause $I)) $Bag) 
      (sum $Bag $C) 
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



  (= 
    (compute_complexity  () 0) True)
; 

  (= 
    (compute-complexity 
      (Cons  
        (with_self  $L $_) $More) $C) 
    ( (term-size $L $C1) 
      (compute-complexity $More $C2) 
      (is $C 
        (+ $C1 $C2))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (ivonTunterE $ITE) 
    ( (findall 
        (/ $P $A) 
        (, 
          (get-clause $_ $_ $_ $CL $_) 
          (member 
            (with_self  $F $_) $CL) 
          (functor $F $P $A)) $Predlist0) 
      (make-unique $Predlist0 $Predlist) 
      (max-arity $Predlist $A) 
      (length $Predlist $PT) 
      (findall $C 
        (, 
          (get-clause $_ $H $B $_ $_) 
          (sub-term $C 
            (, $H $B)) 
          (atomic $C) 
          (\== $C True)) $Clist0) 
      (make-unique $Clist0 $Clist) 
      (length $Clist $CT) 
      (findall 
        (/ $V $L2) 
        (, 
          (get-clause $_ $_ $_ $CL $_) 
          (length $CL $L1) 
          (is $L2 
            (- $L1 1)) 
          (vars $CL $VL) 
          (length $VL $V)) $LList) 
      (maxvars $LList $VT) 
      (max-arity $LList $L) 
      (findall $ID 
        (get-clause $ID $_ $_ $_ $_) $IDL) 
      (length $IDL $BT) 
      (ivonT $PT $CT $VT $A $L $BT $IT) 
      (ivonEunterT $Predlist $CT $IET) 
      (is $ITE 
        (+ $IT $IET))))
; 





  (= 
    (ivonT $PT0 $CT0 $VT0 $A0 $L0 $BT0 $IT) 
    ( (is $PT 
        (float $PT0)) 
      (is $CT 
        (float $CT0)) 
      (is $VT 
        (float $VT0)) 
      (is $A 
        (float $A0)) 
      (is $L 
        (float $L0)) 
      (is $BT 
        (float $BT0)) 
      (is $X1 
        (+ $CT $VT)) 
      (pow $X1 $A $X2) 
      (is $AT 
        (* $PT $X2)) 
      (nueberk $AT $L $X3) 
      (is $CLT 
        (* $AT $X3)) 
      (log2nueberk $CLT $BT $IT)))
; 



  (= 
    (ivonEunterT $Predlist $CT $IET) 
    ( (all-atoms $Predlist $CT $HT0) 
      (mTplus $CT $MTP0) 
      (is $HT 
        (float $HT0)) 
      (is $MTP 
        (float $MTP0)) 
      (is $MTM 
        (- $HT $MTP)) 
      (findall $P 
        (get-example $_ $P +) $PL) 
      (length $PL $PLN0) 
      (findall $N 
        (get-example $_ $N -) $NL) 
      (length $NL $NLN0) 
      (is $PLN 
        (float $PLN0)) 
      (is $NLN 
        (float $NLN0)) 
      (log2nueberk $MTP $PLN $LX) 
      (log2nueberk $MTM $NLN $LY) 
      (is $IET 
        (+ $LX $LY))))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (ivonBundE $IBE) 
    ( (findall 
        (/ $P $A) 
        (, 
          (get-clause $_ $_ $_ $CL $_) 
          (member 
            (with_self  $F $_) $CL) 
          (functor $F $P $A)) $Predlist00) 
      (findall 
        (/ $P1 $A1) 
        (, 
          (get-example $_ $F $_) 
          (functor $F $P1 $A1)) $Predlist01) 
      (append $Predlist00 $Predlist01 $Predlist0) 
      (make-unique $Predlist0 $Predlist) 
      (max-arity $Predlist $A) 
      (length $Predlist $PT) 
      (findall $C 
        (, 
          (get-clause $_ $H $B $_ $_) 
          (sub-term $C 
            (, $H $B)) 
          (atomic $C) 
          (\== $C True)) $Clist00) 
      (findall $C1 
        (, 
          (get-example $_ $H1 $_) 
          (sub-term $C1 $H1) 
          (atomic $C1) 
          (\== $C1 True)) $Clist01) 
      (append $Clist00 $Clist01 $Clist0) 
      (make-unique $Clist0 $Clist) 
      (length $Clist $CT) 
      (findall 
        (/ $V $L2) 
        (, 
          (get-clause $_ $_ $_ $CL $_) 
          (length $CL $L1) 
          (is $L2 
            (- $L1 1)) 
          (vars $CL $VL) 
          (length $VL $V)) $LList) 
      (maxvars $LList $VT) 
      (max-arity $LList $L) 
      (findall $ID 
        (get-clause $ID $_ $_ $_ $_) $IDL00) 
      (findall $ID1 
        (get-example $ID1 $_ $_) $IDL01) 
      (append $IDL00 $IDL01 $IDL) 
      (length $IDL $BT) 
      (ivonT $PT $CT $VT $A $L $BT $IBE)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (all_symbols  () $_ 0) True)
; 

  (= 
    (all-atoms 
      (Cons  
        (/ $_ $A0) $R) $CT0 $HT) 
    ( (all-atoms $R $CT0 $HT0) 
      (is $CT 
        (float $CT0)) 
      (is $A 
        (float $A0)) 
      (pow $CT $A $X) 
      (is $HT 
        (+ $HT0 $X))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (max-arity 
      (:: (/ $_ $A)) $A) 
    (set-det))
; 
   
  (= 
    (max-arity 
      (Cons  
        (/ $_ $A) $R) $C) 
    ( (max-arity $R $B) (maxi $A $B $C)))
; 



  (= 
    (maxvars 
      (:: (/ $A $_)) $A) 
    (set-det))
; 

  (= 
    (maxvars 
      (Cons  
        (/ $A $_) $R) $C) 
    ( (maxvars $R $B) (maxi $A $B $C)))
; 



  (= 
    (maxi $A $B $C) 
    (det-if-then-else 
      (>= $A $B) 
      (= $C $A) 
      (= $C $B)))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (herbrand-base-ff $M) 
    ( (findall $H 
        (get-clause $_ $H True $_ $_) $M00) 
      (reduce-hb $M00 $M0) 
      (findall $ID 
        (, 
          (get-clause $ID $_ $B $_ $_) 
          (\== $B True)) $IDlist) 
      (herbrand-base-ff $IDlist $M0 $M)))
; 


  (= 
    (herbrand-base-ff $IDlist $M $M2) 
    ( (herbrand-base-ff $IDlist $M $M $M1 $Mark) (det-if-then-else (== $Mark changed) (herbrand-base-ff $IDlist $M1 $M2) (= $M2 $M1))))
; 


  (= 
    (herbrand_base_ff  () $_ $M $M not_changed) True)
; 

  (= 
    (herbrand-base-ff 
      (Cons  $ID $R) $M $M1 $M4 $Mark) 
    ( (herbrand-base-ff $R $M $M1 $M2 $Mark0) 
      (get-clause $ID $H $B $_ $_) 
      (findall $H1 
        (match-body $H $B $M $H1) $HL) 
      (append $HL $M2 $M3) 
      (make-unique $M3 $M31) 
      (reduce-hb $M31 $M4) 
      (det-if-then-else 
        (remove-variant $M2 $M4 Nil) 
        (= $Mark $Mark0) 
        (= $Mark changed))))
; 



  (= 
    (match-body $H $B $M $H1) 
    ( (copy-term 
        (, $H $B) 
        (, $H1 $B1)) 
      (copy-term $M $M1) 
      (match-body $B1 $M1)))
; 


  (= 
    (match-body 
      (, $A $B) $M) 
    ( (set-det) 
      (member $A $M) 
      (match-body $B $M)))
; 

  (= 
    (match-body $A $M) 
    (member $A $M))
; 



  (= 
    (reduce-hb $L $L1) 
    (reduce-hb $L $L $L1))
; 

  (= 
    (reduce_hb  () $_ ()) True)
; 

  (= 
    (reduce-hb 
      (Cons  $H $R) $L $R2) 
    ( (reduce-hb $R $L $R1) (det-if-then-else (not (sub-contained-in $H $L)) (= $R2 (Cons  $H $R1)) (= $R2 $R1))))
; 



  (= 
    (sub-contained-in $H 
      (Cons  $H1 $R)) 
    (det-if-then-else 
      (, 
        (\== $H1 $H) 
        (subsumes-chk $H1 $H)) True 
      (sub-contained-in $H $R)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (mTplus $CT $MT) 
    ( (herbrand-base-ff $M) (hb-plus $M $CT $MT)))
; 



  (= 
    (hb_plus  () $_ 0) True)
; 

  (= 
    (hb-plus 
      (Cons  $T $R) $CT0 $MT) 
    ( (hb-plus $R $CT0 $MT1) 
      (vars $T $V) 
      (length $V $VN0) 
      (is $CT 
        (float $CT0)) 
      (is $VN 
        (float $VN0)) 
      (pow $CT $VN $X) 
      (is $MT 
        (+ $MT1 $X))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (code-length $S $L) 
    ( (skolemize $S $_ $S0) 
      (symbol-frequencies $S0 Nil $SymS) 
      (relative-frequencies $SymS 0 $N $SymS1) 
      (code-length1 $SymS1 $L0) 
      (is $L 
        (* $N $L0))))
; 



  (= 
    (code_length1  () 0) True)
; 

  (= 
    (code-length1 
      (Cons  $F $R) $L) 
    ( (code-length1 $R $L0) 
      (log2 $F $LF) 
      (is $L1 
        (* $F 
          (- $LF))) 
      (is $L 
        (+ $L0 $L1))))
; 



  (= 
    (relative_frequencies  () $N $N ()) True)
; 

  (= 
    (relative-frequencies 
      (Cons  
        (/ 
          (/ $_ $_) $M) $R) $N0 $N 
      (Cons  $RM $R1)) 
    ( (is $N1 
        (+ $N0 $M)) 
      (relative-frequencies $R $N1 $N $R1) 
      (is $RM 
        (/ $M $N))))
; 



  (= 
    (symbol-frequencies $X $L $L1) 
    ( (atomic $X) 
      (set-det) 
      (update-frequency-list $L $X 0 $L1)))
; 

  (= 
    (symbol-frequencies $X $L $L1) 
    ( (functor $X $F $N) 
      (update-frequency-list $L $F $N $L0) 
      (symbol-frequencies $N $X $L0 $L1)))
; 


  (= 
    (symbol-frequencies 0 $_ $L $L) 
    (set-det))
; 

  (= 
    (symbol-frequencies $N $X $L $L2) 
    ( (is $N1 
        (- $N 1)) 
      (symbol-frequencies $N1 $X $L $L1) 
      (arg $N $X $Xn) 
      (symbol-frequencies $Xn $L1 $L2)))
; 



  (= 
    (update_frequency_list  () $F $N 
      ( (/  
          (/  $F $N) 1))) True)
; 

  (= 
    (update-frequency-list 
      (Cons  
        (/ 
          (/ $F $N) $M) $R) $F $N 
      (Cons  
        (/ 
          (/ $F $N) $M1) $R)) 
    ( (set-det) (is $M1 (+ $M 1))))
; 

  (= 
    (update-frequency-list 
      (Cons  $X $R) $F $N 
      (Cons  $X $R1)) 
    (update-frequency-list $R $F $N $R1))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (encoding-length-examples $X) 
    ( (mysetof $ID 
        (^ $F 
          (get-example $ID $F +)) $PL) 
      (length $PL $PN) 
      (mysetof $ID1 
        (^ $F1 
          (get-example $ID1 $F1 -)) $NL) 
      (length $NL $NN) 
      (is $U 
        (+ $PN $NN)) 
      (log2 $U $LU) 
      (is $U1 
        (float $U)) 
      (is $PN1 
        (float $PN)) 
      (log2nueberk $U1 $PN1 $Y) 
      (is $X 
        (+ $LU $Y))))
; 



  (= 
    (encoding-length-clause $CL $EL) 
    ( (length $CL $N) 
      (is $N1 
        (float $N)) 
      (sum-of-logs 1.0 $N1 $LNF) 
      (encoding-length-lits $CL $Lits0) 
      (get-predlist $PList) 
      (length $PList $Preds) 
      (log2 $Preds $LPreds) 
      (is $Lits 
        (+ 
          (+ $Lits0 $N) 
          (* $N $LPreds))) 
      (is $EL 
        (/ $Lits $LNF))))
; 



  (= 
    (encoding-length-lits 
      (Cons  
        (with_self  $H 
          (p)) $R) $M) 
    ( (functor $H $_ $N) 
      (=.. $H 
        (Cons  $_ $Args)) 
      (log2 $N $LN) 
      (encoding-length-lits $R $Args $M1) 
      (is $M 
        (+ $M1 $LN))))
; 


  (= 
    (encoding-length-lits 
      (Cons  
        (with_self  $L $_) $R) $Args $M) 
    ( (length $Args $LA) 
      (log2 $LA $M0) 
      (=.. $L 
        (Cons  $_ $Args1)) 
      (append $Args1 $Args $Args2) 
      (identical-make-unique $Args2 $Args3) 
      (encoding-length-lits $R $Args3 $M1) 
      (is $M 
        (+ $M0 $M1))))
; 

  (= 
    (encoding_length_lits  () $_ 0) True)
; 
 
   


