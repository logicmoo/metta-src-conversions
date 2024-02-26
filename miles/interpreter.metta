
;
; 


  !(module interpreter 
    (:: 
      (/ solve 3) 
      (/ solve-once 3) 
      (/ failed-proof 1) 
      (/ prooftrees 3) 
      (/ proof-close 2) 
      (/ prove1 2) 
      (/ prove3 2) 
      (/ prove4 3) 
      (/ prove5 2) 
      (/ proof-path 4) 
      (/ set-proof-depth 0) 
      (/ t-interpreter 2) 
      (/ ip-part1 2) 
      (/ ip-part2 3)))
; 


;
; 

  !(use-module 
    (home kb) 
    (:: 
      (/ get-clause 5) 
      (/ interpretable-predicate 1)))
; 

  !(use-module 
    (home div-utils) 
    (:: 
      (/ insert-unique 3) 
      (/ identical-member 2) 
      (/ append-all 2) 
      (/ mysetof 3)))
; 

  !(use-module 
    (home bu-basics) 
    (:: 
      (/ head 3) 
      (/ body 3) 
      (/ assumption 3)))
; 

  !(use-module 
    (home environment) 
    (:: (/ satisfiable 1)))
; 

  !(use-module-if-exists 
    (library basics) 
    (:: (/ member 2)))
; 

  !(use-module-if-exists 
    (library unify) 
    (:: (/ unify 2)))
; 



;
; 

;
; 



  !(dynamic (, (/ failed-proof 1) (/ tag 1) (/ prooftrees 3) (/ depth-bound 1) (/ depth-exceeded 0) (/ depth-exceeded 3)))
; 
 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (ip-part1 $Goal $Proof) 
    ( (remove-all-symbols  &self 
        (tag  $_)) 
      (add-symbol  &self 
        (tag  $Goal)) 
      (gen-depth $D $Delta) 
      (ipp1 $Goal $D $Delta $Proof $Proof Nil)))
; 

 

  (= 
    (ipp1 True $D $Delta $Proof $Poi $_) 
    ( (set-det) (= $Poi Nil)))
; 


  (= 
    (ipp1 no-rule $_ $_ $_ $_ $_) 
    (set-det))
; 


  (= 
    (ipp1 
      (, $A $B) $D $Delta $Proof $Poi $Ancestors) 
    ( (set-det) 
      (= $Poi 
        (Cons  $PoiA $PoiB)) 
      (ipp1 $A $D $Delta $Proof 
        (:: $PoiA) $Ancestors) 
      (ipp1 $B $D $Delta $Proof $PoiB $Ancestors)))
; 

 
  (= 
    (ipp1 $A $D $Delta $Proof $Poi $Ancestors) 
    ( (interpretable-predicate $A) 
      (set-det) 
      (det-if-then-else 
        (> $D 0) True 
        (, 
          (add-symbol  &self 
            (tag  $A)) 
          (fail))) 
      (det-if-then-else 
        (identical-member $A $Ancestors) 
        (= $Poi 
          (:: (:: -1 $A looping))) 
        (, 
          (is $D1 
            (- $D 1)) 
          (ipp1-rule $D $Delta $Proof $Poi $I $A $B) 
          (= $Poi 
            (:: (:: $I $A $PoiB))) 
          (ipp1 $B $D1 $Delta $Proof $PoiB 
            (Cons  $A $Ancestors))))))
; 


  (= 
    (ipp1 $A $D $Delta $Proof $Poi $_) 
    (det-if-then-else 
      (call $A) 
      (= $Poi 
        (:: (:: sys $A Nil))) 
      (= $Poi 
        (:: (:: sys $A fail)))))
; 





  (= 
    (ipp1-rule $_ $_ $_ $_ $I $A $B) 
    (get-clause $I $A $B $_ $_))
; 

  (= 
    (ipp1-rule $D $Delta $Proof $Poi $_ $A no-rule) 
    (det-if-then-else 
      (get-clause $_ $A $_ $_ $_) fail 
      (= $Poi 
        (:: (:: -1 $A no-rules)))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (ip-part2 
      (Cons  $P $_) $Goal $UA) 
    (ipp2 $P Nil Nil $UA))
; 

  (= 
    (ip-part2 
      (Cons  $_ $R) $Goal $UA) 
    (ip-part2 $R $Goal $UA))
; 



  (= 
    (ipp2 
      (:: $I $H looping) $_ $L 
      (Cons  
        (with_self  $I $H) $L)) 
    (set-det))
; 

  (= 
    (ipp2 
      (:: sys $_ fail) 
      (Cons  
        (with_self  $I $A) $_) $L 
      (Cons  
        (with_self  $I $A) $L)) 
    (set-det))
; 

  (= 
    (ipp2 
      (:: $_ $_ Nil) $_ $L $L) 
    (set-det))
; 

  (= 
    (ipp2 
      (:: $_ $H no-rules) $_ $L 
      (Cons  
        (with_self  -1 $H) $L)) 
    (set-det))
; 

  (= 
    (ipp2 
      (:: $I $H $SG) $Ancestors $L $L1) 
    (det-if-then-else 
      (satisfiable $SG) 
      (ipp2-list $SG 
        (Cons  
          (with_self  $I $H) $Ancestors) $L $L1) 
      (= $L1 
        (Cons  
          (with_self  $I $H) $L))))
; 




  (= 
    (ipp2_list  () $_ $L $L) True)
; 

  (= 
    (ipp2-list 
      (Cons  $G $R) $A $L $L2) 
    ( (ipp2 $G $A $L $L1) (ipp2-list $R $A $L1 $L2)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (proof-path $Ex $P $T $Ts) 
    ( (copy-term 
        (, $P $T) 
        (, $Ex $T0)) (proof-path $T0 Nil $Ts)))
; 


  (= 
    (proof-path True $T $T) 
    (set-det))
; 

  (= 
    (proof-path 
      (, $A $B) $T $T2) 
    ( (set-det) 
      (proof-path $A $T $T1) 
      (proof-path $B $T1 $T2)))
; 


  (= 
    (proof-path $A $T $T) 
    ( (=.. $A 
        (Cons  all $_)) (set-det)))
; 

  (= 
    (proof-path $A $T $T) 
    ( (=.. $A 
        (Cons  $T1 $_)) 
      (or 
        (= $T1 is-symbol) 
        (or 
          (= $T1 symbolic) 
          (= $T1 number))) 
      (set-det) 
      (call $A)))
; 


  (= 
    (proof-path $A $T $T1) 
    ( (get-clause $I $A $B $_ type) 
      (proof-path $B $T $T0) 
      (insert-unique $I $T0 $T1)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (t-interpreter True $_) 
    (set-det))
; 

  (= 
    (t-interpreter 
      (, $A $B) $CL) 
    ( (set-det) 
      (t-interpreter $A $CL) 
      (t-interpreter $B $CL)))
; 

  (= 
    (t-interpreter $C $_) 
    ( (=.. $C 
        (Cons  $P $_)) 
      (or 
        (= $P is-symbol) 
        (or 
          (= $P number) 
          (= $P symbolic))) 
      (set-det) 
      (call $C)))
; 

  (= 
    (t-interpreter $C $CL) 
    ( (copy-term $CL $CL1) 
      (member 
        (= $C $B) $CL1) 
      (t-interpreter $B $CL)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (solve 
      (= $H $B) $Mark $Proofs) 
    (det-if-then-else 
      (ground $H) 
      (solve $B $Mark $Proofs) 
      (, 
        (set-det) 
        (fail))))
; 


  (= 
    (solve $Goal $Mark $Proofs) 
    ( (det-if-then-else 
        (setof $Proof 
          (^ $Goal 
            (solve0 $Goal $Proof)) $Proofs0) 
        (= $Mark success) 
        (, 
          (bagof $FProof 
            (failed-proof $FProof) $Proofs00) 
          (= $Mark fail) 
          (det-if-then-else depth-exceeded 
            (, 
              (setof $EProof 
                (^ $A 
                  (depth-exceeded $A $EProof 
                    (:: (:: -1 $A depth-exceeded)))) $EProofs0) 
              (append $EProofs0 $Proofs00 $Proofs0)) 
            (= $Proofs0 $Proofs00)))) 
      (append-all $Proofs0 $Proofs1) 
      (proof-close $Proofs1 $Proofs)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (solve-once 
      (= $H $B) $Mark $Proofs) 
    (det-if-then-else 
      (ground $H) 
      (solve-once $B $Mark $Proofs) 
      (, 
        (set-det) 
        (fail))))
; 


  (= 
    (solve-once $Goal $Mark $Proofs) 
    ( (det-if-then-else 
        (solve0 $Goal $Proof) 
        (, 
          (= $Proofs0 
            (:: $Proof)) 
          (= $Mark success)) 
        (, 
          (bagof $FProof 
            (failed-proof $FProof) $Proofs0) 
          (= $Mark fail))) 
      (append-all $Proofs0 $Proofs1) 
      (proof-close $Proofs1 $Proofs)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (proof-close $X Nil) 
    ( (var $X) (set-det)))
; 

  (= 
    (proof-close 
      (Cons  
        (:: $J $H $B1) $R1) 
      (Cons  
        (:: $J $H $B2) $R2)) 
    ( (proof-close $B1 $B2) (proof-close $R1 $R2)))
; 

  (= 
    (proof-close $X $X) 
    ( (atomic $X) (set-det)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (solve0 $Goal $Proof) 
    ( (remove-all-symbols  &self 
        (tag  $_)) 
      (remove-all-symbols  &self 
        (failed_proof  $_)) 
      (remove-all-symbols  &self 
        (depth_exceeded  $_ $_ $_)) 
      (remove-all-symbols  &self depth_exceeded) 
      (gen-depth $D $Delta) 
      (solve2 $Goal $D $Delta $Proof $Proof Nil)))
; 




  (= 
    (solve2 True $_ $_ $_ Nil $_) 
    (set-det))
; 


 
  (= 
    (solve2 
      (, $A $B) $D $Delta $Proof $Poi $Ancestors) 
    ( (set-det) 
      (= $Poi 
        (Cons  $PoiA $PoiB)) 
      (solve2 $A $D $Delta $Proof 
        (:: $PoiA) $Ancestors) 
      (solve2 $B $D $Delta $Proof $PoiB $Ancestors)))
; 

    
 
  (= 
    (solve2 $A $D $Delta $Proof $Poi $Ancestors) 
    ( (interpretable-predicate $A) 
      (set-det) 
      (det-if-then-else 
        (= $D 0) 
        (, 
          (add-symbol  &self 
            (tag  $A)) 
          (add-symbol  &self 
            (depth_exceeded  $A $Proof $Poi)) 
          (fail)) 
        (det-if-then-else 
          (identical-member $A $Ancestors) 
          (, 
            (= $Poi 
              (:: (:: -1 $A looping))) 
            (det-if-then-else 
              (< $D $Delta) 
              (add-symbol  &self 
                (failed_proof  $Proof)) True) 
            (fail)) 
          (, 
            (is $D1 
              (- $D 1)) 
            (solve-rule $D1 $Delta $Proof $Poi $I $A $B) 
            (= $Poi 
              (:: (:: $I $A $PoiB))) 
            (solve2 $B $D1 $Delta $Proof $PoiB 
              (Cons  $A $Ancestors)))))))
; 


  (= 
    (solve2 $A $D $Delta $Proof $Poi $_) 
    (det-if-then-else 
      (, 
        (predicate-property $A built-in) 
        (on-exception $_ 
          (call $A) fail)) 
      (= $Poi 
        (:: (:: sys $A Nil))) 
      (, 
        (= $Poi 
          (:: (:: sys $A fail))) 
        (det-if-then-else 
          (< $D $Delta) 
          (add-symbol  &self 
            (failed_proof  $Proof)) True) 
        (fail))))
; 



  (= 
    (solve2 $A $D $D $Proof $Poi $_) 
    ( (not depth-exceeded) 
      (= $Poi 
        (:: (:: -1 $A no-rules))) 
      (add-symbol  &self 
        (failed_proof  $Proof)) 
      (fail)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (solve-rule $_ $_ $_ $_ $I $A $B) 
    ( (functor $A $F $N) 
      (functor $A1 $F $N) 
      (get-clause $I $A1 $B $_ $_) 
      (unify $A $A1)))
; 

  (= 
    (solve-rule $D $Delta $Proof $Poi $_ $A $_) 
    ( (= $Poi 
        (:: (:: -1 $A no-more-rules))) 
      (det-if-then-else 
        (< $D $Delta) 
        (add-symbol  &self 
          (failed_proof  $Proof)) True) 
      (fail)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (gen-depth $D $Delta) 
    ( (depth-bound $N) (det-if-then-else (number $N) (det-if-then-else (>= $N 3) (gen-depth 3 $D 3 $Delta) (gen-depth $N $D $N $Delta)) (gen-depth 3 $D 3 $Delta))))
; 
           

 

  (= 
    (gen_depth  $D $D $Delta $Delta) True)
; 

 
  (= 
    (gen-depth $D0 $D $_ $Delta) 
    (det-if-then-else 
      (tag $_) 
      (, 
        (remove-all-symbols  &self 
          (tag  $_)) 
        (is $Delta1 
          (+ 
            (div $D0 2) 1)) 
        (is $D1 
          (+ $D0 $Delta1)) 
        (depth-bound $Max) 
        (det-if-then-else 
          (number $Max) 
          (det-if-then-else 
            (=< $D1 $Max) 
            (remove-all-symbols  &self 
              (depth_exceeded  $_ $_ $_)) 
            (, 
              (add-symbol  &self depth_exceeded) 
              (fail))) True) 
        (gen-depth $D1 $D $Delta1 $Delta)) fail))
; 



  (= 
    (set-proof-depth) 
    ( (nl) 
      (nl) 
      (write 'Speficy maximum depth for theorem prover (number or n for unbound proofs): ') 
      (read $N) 
      (remove-all-symbols  &self 
        (depth_bound  $_)) 
      (add-symbol  &self 
        (depth_bound  $N))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (prove1 $CL $SProof) 
    ( (ini-prove1 $CL $CL1) (prove1a $CL1 Nil $SProof)))
; 



  (= 
    (prove1a  () $SP $SP) True)
; 

;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  miles/interpreter.pl 618 26 17908))

;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  miles/interpreter.pl 619 13 17937))



  (= 
    (ini_prove1  () ()) True)
; 

;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  miles/interpreter.pl 634 20 18344))


;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  miles/interpreter.pl 636 16 18376))


  (= 
    (insert_prove1  $X $L 
      (Cons  $X $L)) True)
; 



  (= 
    (adapt_prove1  () $_ $_ ()) True)
; 

;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  miles/interpreter.pl 643 18 18567))

;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  miles/interpreter.pl 646 18 18664))



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (prove3 
      (Cons  $A $B) 
      (Cons  $ProofA $ProofB)) 
    ( (prove3 $A $ProofA) (prove3 $B $ProofB)))
; 

  (= 
    (prove3  () ()) True)
; 


  (= 
    (prove3 
      (with_self  $A 
        (n)) 
      (with_self  $A 
        (n))) 
    (body $A $_ $_))
; 

  (= 
    (prove3 
      (with_self  $A 
        (r)) 
      (with_self  $A 
        (r))) 
    (body $A $_ $_))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (prove4 
      (Cons  $H $More) $Uncovered 
      (Cons  $ProofH $ProofRest)) 
    ( (prove4 $H $Uncovered $ProofH) (prove4 $More $Uncovered $ProofRest)))
; 



  (= 
    (prove4 Nil Nil Nil) 
    (set-det))
; 

  (= 
    (prove4 Nil $_ Nil) 
    (set-det))
; 

  (= 
    (prove4 
      (with_self  $H 
        (n)) $_ 
      (:: $H body)) 
    (body $H $_ $_))
; 

  (= 
    (prove4 
      (with_self  $H 
        (r)) $_ 
      (:: $H body)) 
    (body $H $_ $_))
; 

  (= 
    (prove4 
      (with_self  $H 
        (p)) $_ 
      (:: $H head)) 
    (head $H $_ $_))
; 

  (= 
    (prove4 
      (with_self  $H 
        (n)) $Uncovered Nil) 
    ( (var $Uncovered) (= $Uncovered (/ $H new-head))))
; 

  (= 
    (prove4 
      (with_self  $H 
        (r)) $Uncovered Nil) 
    ( (var $Uncovered) (= $Uncovered (/ $H new-head))))
; 

  (= 
    (prove4 
      (with_self  $H 
        (p)) $Uncovered Nil) 
    ( (var $Uncovered) (= $Uncovered (/ $H new-body))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (prove5 $H $_) 
    (assumption $H $_ $_))
; 

  (= 
    (prove5  true $_) True)
; 

  (= 
    (prove5 $H $RULES) 
    ( (get-clause $ID $H True $_ $_) (member $ID $RULES)))
; 

  (= 
    (prove5 
      (, $L1 $L2) $RULES) 
    ( (prove5 $L1 $RULES) (prove5 $L2 $RULES)))
; 

  (= 
    (prove5 $H $RULES) 
    ( (get-clause $ID $H $B $_ $_) 
      (member $ID $RULES) 
      (prove5 $B $RULES)))
; 



