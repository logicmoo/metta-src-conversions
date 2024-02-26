;
; 


  !(module bu-basics 
    (:: 
      (/ addtolist 1) 
      (/ getlist 1) 
      (/ subs-build-clause 1) 
      (/ msg-build-long-clause 1) 
      (/ msg-build-heads 1) 
      (/ msg-build-body 1) 
      (/ annotate-redundancy 1) 
      (/ abs-process-proofs 2) 
      (/ abs-build-body 1) 
      (/ ident-process-proofs 2) 
      (/ ident-build-body 1) 
      (/ g1-build-clause 2) 
      (/ g1-process-proofs 2) 
      (/ idev-build-clause1 1) 
      (/ idev-build-clause 2) 
      (/ process-new-literals 2) 
      (/ sat-build-clause 3) 
      (/ head 3) 
      (/ body 3) 
      (/ assumption 3) 
      (/ assert-absorptions 2) 
      (/ assert-clause 1) 
      (/ assert-body 1) 
      (/ assert-body-unique 1) 
      (/ assert-body-randomly 1) 
      (/ cover-assert-assumptions 1) 
      (/ retract-body-literals 1) 
      (/ retract-literals 1) 
      (/ assert-literals 1) 
      (/ clear-mngr 0) 
      (/ reset-counts 0)))
; 


;
; 

  !(use-module 
    (home div-utils) 
    (:: (/ contains-duplicates 1)))
; 

  !(use-module-if-exists 
    (library basics) 
    (:: (/ member 2)))
; 

  !(use-module-if-exists 
    (library random) 
    (:: (/ maybe 0)))
; 


;
; 

;
; 


  !(dynamic (/ head 3))
; 
     ;
; 

  !(dynamic (/ body 3))
; 
     ;
; 

                       ;
; 

                       ;
; 

                       ;
; 

                       ;
; 

                       ;
; 

  !(dynamic (/ assumption 3))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (process_new_literals  () $_) True)
; 



  (= 
    (process-new-literals 
      (Cons  
        (with_self  Nil $Proof) $Rest) $Flag) 
    (process-new-literals $Rest $Flag))
; 


  (= 
    (process-new-literals 
      (Cons  
        (with_self  
          (/ $L new-body) $Proof) $Rest) $Flag) 
    ( (body $L $_ $_) 
      (set-det) 
      (process-new-literals $Rest $Flag)))
; 


  (= 
    (process-new-literals 
      (Cons  
        (with_self  
          (/ $L new-head) $Proof) $Rest) $Flag) 
    ( (head $L $_ $_) 
      (set-det) 
      (process-new-literals $Rest $Flag)))
; 


  (= 
    (process-new-literals 
      (Cons  
        (with_self  
          (/ $L $_) $Proof) $Rest) $Flag) 
    ( (contains-duplicates $Proof) 
      (set-det) 
      (process-new-literals $Rest $Flag)))
; 


  (= 
    (process-new-literals 
      (Cons  
        (with_self  
          (/ $L new-head) $Proof) $Rest) 1) 
    ( (set-det) 
      (add-symbol  &self 
        (head  $L new 0)) 
      (annotate $Proof) 
      (process-new-literals $Rest $_)))
; 


  (= 
    (process-new-literals 
      (Cons  
        (with_self  
          (/ $L new-body) $Proof) $Rest) 1) 
    ( (add-symbol  &self 
        (body  $L new 0)) 
      (annotate $Proof) 
      (process-new-literals $Rest $_)))
; 


;
; 

  (= 
    (process-new-literals 
      (Cons  
        (with_self  
          (/ $L new-body) $Proof) $Rest) $Flag) 
    ( (remove-symbol  &self 
        (body  $L new 0)) (process-new-literals $Rest $Flag)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (annotate  ()) True)
; 


  (= 
    (annotate (Cons  (:: $L body) $Rest)) 
    ( (remove-symbol  &self 
        (body  $L $OldNew $I)) 
      (is $J 
        (+ $I 1)) 
      (add-symbol  &self 
        (body  $L $OldNew $J)) 
      (annotate $Rest)))
; 

  (= 
    (annotate (Cons  (:: $L head) $Rest)) 
    ( (remove-symbol  &self 
        (head  $L $OldNew $I)) 
      (is $J 
        (+ $I 1)) 
      (add-symbol  &self 
        (head  $L $OldNew $J)) 
      (annotate $Rest)))
; 


  (= 
    (annotate (Cons  Nil $Rest)) 
    (annotate $Rest))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (abs-process-proofs 
      (Cons  
        (Cons  
          (with_self  $Head 
            (p)) $_) $MoreHeads) $NewHead) 
    ( (body $Head $_ $_) 
      (set-det) 
      (abs-process-proofs $MoreHeads $NewHead)))
; 


  (= 
    (abs-process-proofs 
      (Cons  
        (Cons  $_ $Proof) $MoreHeads) $NewHead) 
    ( (contains-duplicates $Proof) 
      (set-det) 
      (abs-process-proofs $MoreHeads $NewHead)))
; 


  (= 
    (abs-process-proofs 
      (Cons  
        (Cons  
          (with_self  $Head 
            (p)) $Proof) $MoreHeads) $Head) 
    (retract-body-literals $Proof))
; 


;
; 

  (= 
    (abs-process-proofs 
      (Cons  
        (Cons  
          (with_self  $Head 
            (p)) $Proof) $MoreHeads) $NewHead) 
    ( (assert-body $Proof) (abs-process-proofs $MoreHeads $NewHead)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (ident-process-proofs 
      (Cons  
        (:: (with_self  (/ $Head new-head) $_)) $MoreHeads) $NewHead) 
    ( (head $Head $_ $_) 
      (set-det) 
      (ident-process-proofs $MoreHeads $NewHead)))
; 


  (= 
    (ident-process-proofs 
      (Cons  
        (:: (with_self  (/ $_ $_) $Proof)) $MoreHeads) $NewHead) 
    ( (contains-duplicates $Proof) 
      (set-det) 
      (ident-process-proofs $MoreHeads $NewHead)))
; 


  (= 
    (ident-process-proofs 
      (Cons  
        (:: (with_self  (/ $Head new-head) $Proof)) $MoreHeads) $Head) 
    (retract-literals $Proof))
; 


;
; 

  (= 
    (ident-process-proofs 
      (Cons  
        (:: (with_self  (/ $Head new-head) $Proof)) $MoreHeads) $NewHead) 
    ( (assert-literals $Proof) (ident-process-proofs $MoreHeads $NewHead)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (g1-process-proofs 
      (Cons  
        (with_self  Nil $_) $R) $Lit) 
    (g1-process-proofs $R $Lit))
; 


  (= 
    (g1-process-proofs 
      (Cons  
        (with_self  
          (/ $Head new-head) $_) $MoreHeads) $Lit) 
    ( (head $Head $_ $_) 
      (set-det) 
      (g1-process-proofs $MoreHeads $Lit)))
; 



  (= 
    (g1-process-proofs 
      (Cons  
        (with_self  
          (/ $Body new-body) $_) $MoreHeads) $Lit) 
    ( (body $Body $_ $_) 
      (set-det) 
      (g1-process-proofs $MoreHeads $Lit)))
; 


  (= 
    (g1-process-proofs 
      (Cons  
        (with_self  
          (/ $_ $_) $Proof) $MoreHeads) $Lit) 
    ( (contains-duplicates $Proof) 
      (set-det) 
      (g1-process-proofs $MoreHeads $Lit)))
; 


  (= 
    (g1-process-proofs 
      (Cons  
        (with_self  
          (/ $Lit $S0) $Proof) $MoreHeads) 
      (with_self  $Lit $S)) 
    ( (det-if-then-else 
        (= $S0 new-head) 
        (= $S p) 
        (= $S n)) (retract-literals $Proof)))
; 


;
; 

  (= 
    (g1-process-proofs 
      (Cons  
        (with_self  
          (/ $_ $_) $Proof) $MoreHeads) $Lit) 
    ( (assert-literals $Proof) (g1-process-proofs $MoreHeads $Lit)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (assert_absorptions  () $F) True)
; 

  (= 
    (assert-absorptions 
      (Cons  
        (Cons  
          (with_self  $Head 
            (p)) $_) $MoreHeads) $F) 
    ( (body $Head $OldNew $Count) 
      (set-det) 
      (assert-absorptions $MoreHeads $F)))
; 


  (= 
    (assert-absorptions 
      (Cons  
        (Cons  
          (with_self  $Head 
            (p)) $Proof) $MoreHeads) 1) 
    ( (add-symbol  &self 
        (body  $Head new 0)) 
      (annotate-redundancy $Proof) 
      (assert-absorptions $MoreHeads 1)))
; 


;
; 

  (= 
    (assert-absorptions 
      (Cons  
        (Cons  
          (with_self  $Head 
            (p)) $Proof) $MoreHeads) $_) 
    (remove-symbol  &self 
      (body  $Head new 0)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (annotate_redundancy  ()) True)
; 

  (= 
    (annotate-redundancy (Cons  (with_self  $L $_) $More)) 
    ( (remove-symbol  &self 
        (body  $L $OldNew $I)) 
      (is $J 
        (+ $I 1)) 
      (add-symbol  &self 
        (body  $L $OldNew $J)) 
      (annotate-redundancy $More)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (assert-body-randomly Nil) 
    (set-det))
; 


  (= 
    (assert-body-randomly (Cons  (with_self  $H (p)) $More)) 
    ( (head $H $_ $_) 
      (set-det) 
      (assert-body-randomly $More)))
; 


  (= 
    (assert-body-randomly (Cons  (with_self  $H (p)) $More)) 
    ( (add-symbol  &self 
        (head  $H $_ $_)) (assert-body-randomly $More)))
; 


  (= 
    (assert-body-randomly (Cons  (with_self  $L $_) $More)) 
    ( (body $L $_ $_) 
      (set-det) 
      (assert-body-randomly $More)))
; 


  (= 
    (assert-body-randomly (Cons  (with_self  $L $_) $More)) 
    ( (maybe) 
      (add-symbol  &self 
        (body  $L old 0)) 
      (assert-body-randomly $More)))
; 


  (= 
    (assert-body-randomly (Cons  (with_self  $L $_) $More)) 
    ( (add-symbol  &self 
        (body  $L old 0)) (assert-body-randomly $More)))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (addtolist (Cons  $L $IST)) 
    ( (remove-symbol  &self 
        (id_list  $List1)) 
      (append $List1 
        (Cons  $L $IST) $List2) 
      (add-symbol  &self 
        (id_list  $List2)) 
      (set-det)))
; 

  (= 
    (addtolist $Id) 
    ( (remove-symbol  &self 
        (id_list  $List1)) 
      (add-symbol  &self 
        (id_list  
          (Cons  $Id $List1))) 
      (set-det)))
; 

  (= 
    (addtolist (Cons  $L $IST)) 
    ( (add-symbol  &self 
        (id_list  
          (Cons  $L $IST))) (set-det)))
; 

  (= 
    (addtolist $Id) 
    ( (add-symbol  &self 
        (id_list  
          ($Id))) (set-det)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (getlist $List) 
    (remove-symbol  &self 
      (id_list  $List)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (cover_assert_assumptions  ()) True)
; 

  (= 
    (cover-assert-assumptions (Cons  (with_self  $L $_) $More)) 
    ( (add-symbol  &self 
        (assumption  $L $_ $_)) (cover-assert-assumptions $More)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (clear-mngr) 
    ( (remove-all-symbols  &self 
        (head  $_ $_ $_)) (remove-all-symbols  &self (body  $_ $_ $_))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (retract-body-literals (Cons  (with_self  $L $_) $More)) 
    ( (remove-symbol  &self 
        (body  $L $_ $_)) (retract-body-literals $More)))
; 

  (= 
    (retract_body_literals  ()) True)
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (retract_literals  ()) True)
; 

  (= 
    (retract-literals (Cons  (:: $L head) $Rest)) 
    ( (remove-symbol  &self 
        (head  $L $_ $_)) 
      (set-det) 
      (retract-literals $Rest)))
; 

  (= 
    (retract-literals (Cons  (:: $L body) $Rest)) 
    ( (remove-symbol  &self 
        (body  $L $_ $_)) 
      (set-det) 
      (retract-literals $Rest)))
; 

  (= 
    (retract-literals (Cons  (:: $_ $_) $Rest)) 
    (retract-literals $Rest))
; 

  (= 
    (retract-literals (Cons  Nil $Rest)) 
    (retract-literals $Rest))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (assert_literals  ()) True)
; 

  (= 
    (assert-literals (Cons  (:: $L head) $Rest)) 
    ( (add-symbol  &self 
        (head  $L $_ $_)) 
      (set-det) 
      (assert-literals $Rest)))
; 

  (= 
    (assert-literals (Cons  (:: $L body) $Rest)) 
    ( (add-symbol  &self 
        (body  $L $_ $_)) 
      (set-det) 
      (assert-literals $Rest)))
; 

  (= 
    (assert-literals (Cons  (:: $_ $_) $Rest)) 
    (assert-literals $Rest))
; 

  (= 
    (assert-literals (Cons  Nil $Rest)) 
    (assert-literals $Rest))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (assert-clause $C) 
    (assert-clause1 $C))
; 

  (= 
    (assert-clause $_) 
    ( (clear-mngr) 
      (set-det) 
      (fail)))
; 
 ;
; 


 
  (= 
    (assert_clause1  ()) True)
; 

  (= 
    (assert-clause1 (Cons  $H $T)) 
    ( (assert-clause1 $H) (assert-clause1 $T)))
; 

  (= 
    (assert-clause1 (with_self  $H (p))) 
    (head $H $_ $_))
; 

  (= 
    (assert-clause1 (with_self  $H (p))) 
    (add-symbol  &self 
      (head  $H old 0)))
; 

  (= 
    (assert-clause1 (with_self  $H $S)) 
    ( (member $S 
        (:: n r)) (body $H $_ $_)))
; 

  (= 
    (assert-clause1 (with_self  $H $S)) 
    ( (member $S 
        (:: n r)) (add-symbol  &self (body  $H old 0))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (assert_body  ()) True)
; 

  (= 
    (assert-body (Cons  $H $T)) 
    ( (assert-body $H) (assert-body $T)))
; 

  (= 
    (assert-body (with_self  $H $S)) 
    ( (member $S 
        (:: n r)) (add-symbol  &self (body  $H old 0))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (assert_body_unique  ()) True)
; 

  (= 
    (assert-body-unique (Cons  $H $T)) 
    ( (assert-body-unique $H) (assert-body-unique $T)))
; 

  (= 
    (assert-body-unique (with_self  $H $S)) 
    ( (member $S 
        (:: n r)) (body $H $_ $_)))
; 

  (= 
    (assert-body-unique (with_self  $H $S)) 
    ( (member $S 
        (:: n r)) (add-symbol  &self (body  $H old 0))))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (reset-counts) 
    ( (remove-symbol  &self 
        (body  $L $O $_)) 
      (add-symbol  &self 
        (body  $L $O 0)) 
      (fail)))
; 

  (= 
    (reset-counts) 
    ( (remove-symbol  &self 
        (head  $L $O $_)) 
      (add-symbol  &self 
        (head  $L $O 0)) 
      (fail)))
; 

  (= reset_counts True)
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (subs-build-clause (Cons  (with_self  $H (p)) $Body)) 
    ( (remove-symbol  &self 
        (head  $H $_ $_)) 
      (subs-build-clause $Body) 
      (set-det)))
; 

  (= 
    (subs-build-clause (Cons  (with_self  $L (n)) $Body)) 
    ( (remove-symbol  &self 
        (body  $L $_ $_)) (subs-build-clause $Body)))
; 

  (= 
    (subs-build-clause Nil) 
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

;
; 

;
; 



  (= 
    (sat-build-clause $H $B 
      (Cons  
        (with_self  $H 
          (p)) $B1)) 
    (sat-build-body $B $B1))
; 


  (= 
    (sat_build_body  () ()) True)
; 

  (= 
    (sat-build-body 
      (Cons  $L $B) 
      (Cons  
        (with_self  $L 
          (n)) $B1)) 
    ( (body $L $_ 0) 
      (set-det) 
      (sat-build-body $B $B1)))
; 

  (= 
    (sat-build-body 
      (Cons  $L $B) 
      (Cons  
        (with_self  $L 
          (r)) $B1)) 
    (sat-build-body $B $B1))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (msg-build-long-clause $Clause) 
    ( (msg-build-heads $Heads) 
      (msg-build-body $Body) 
      (append $Heads $Body $Clause) 
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



  (= 
    (msg-build-heads (Cons  (with_self  $H (p)) $More)) 
    ( (remove-symbol  &self 
        (head  $H $F $I)) 
      (msg-build-heads $More) 
      (add-symbol  &self 
        (head  $H $F $I))))
; 

  (= 
    (msg-build-heads Nil) 
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

;
; 



  (= 
    (msg-build-body (Cons  (with_self  $H $Sign) $More)) 
    ( (remove-symbol  &self 
        (body  $H $F $I)) 
      (det-if-then-else 
        (= $I 0) 
        (= $Sign n) 
        (= $Sign r)) 
      (msg-build-body $More) 
      (set-det) 
      (add-symbol  &self 
        (body  $H $F $I))))
; 


  (= 
    (msg-build-body Nil) 
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



  (= 
    (idev-build-clause1 (Cons  (with_self  $H (p)) $More)) 
    ( (remove-symbol  &self 
        (head  $H $F 0)) 
      (set-det) 
      (idev-build-body $More) 
      (add-symbol  &self 
        (head  $H $F 0))))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (idev-build-body (Cons  (with_self  $H $Sign) $More)) 
    ( (remove-symbol  &self 
        (body  $H $F $I)) 
      (det-if-then-else 
        (= $I 0) 
        (= $Sign n) 
        (= $Sign r)) 
      (idev-build-body $More) 
      (set-det) 
      (add-symbol  &self 
        (body  $H $F $I))))
; 


  (= 
    (idev-build-body Nil) 
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
    (idev-build-clause $PrefHead 
      (Cons  
        (with_self  $H 
          (p)) $More)) 
    ( (idev-build-head $PrefHead $H) 
      (idev-build-body $More) 
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



  (= 
    (idev-build-head $PrefHead $PrefHead) 
    ( (remove-symbol  &self 
        (head  $PrefHead $F $N)) 
      (set-det) 
      (add-symbol  &self 
        (head  $PrefHead $F $N))))
; 


  (= 
    (idev-build-head $_ $Head) 
    (head $Head $_ $_))
; 

    ;
; 

    ;
; 

    ;
; 


 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (ident-build-body (Cons  (with_self  $L (n)) $Rest)) 
    ( (remove-symbol  &self 
        (body  $L $_ 0)) 
      (set-det) 
      (ident-build-body $Rest) 
      (add-symbol  &self 
        (body  $L old 0))))
; 
    ;
; 

  (= 
    (ident-build-body Nil) 
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
    (g1-build-clause 
      (with_self  $L 
        (p)) 
      (Cons  
        (with_self  $L 
          (p)) $Body)) 
    ( (ident-build-body $Body) (set-det)))
; 

  (= 
    (g1-build-clause 
      (with_self  $L 
        (n)) 
      (Cons  
        (with_self  $H 
          (p)) 
        (Cons  
          (with_self  $L 
            (n)) $Body))) 
    ( (ident-build-body $Body) 
      (remove-symbol  &self 
        (head  $H $_ 0)) 
      (set-det) 
      (add-symbol  &self 
        (head  $L old 0))))
; 
   


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (abs-build-body (Cons  (with_self  $L (n)) $Rest)) 
    ( (remove-symbol  &self 
        (body  $L $F $I)) 
      (set-det) 
      (abs-build-body $Rest) 
      (add-symbol  &self 
        (body  $L $F $I))))
; 

  (= 
    (abs-build-body Nil) 
    (set-det))
; 


