
  (= 
    (prove-1 (, $HEAD $REST)) 
    ( (set-det) 
      (prove-1 $HEAD) 
      (prove-1 $REST)))
; 

  (= 
    (prove-1 $FACT) 
    (get-symbols  &self 
      (= $FACT true)))
; 

  (= 
    (prove-1 $GOAL) 
    ( (get-symbols  &self 
        (= $GOAL $PREMISSES)) (prove-1 $PREMISSES)))
; 

  (= 
    (prove-1 $GOAL) 
    (call $GOAL))
; 



  (= 
    (prove-2 
      (, $HEAD $REST) $PROOF) 
    ( (set-det) 
      (prove-2 $HEAD $HEAD_PROOF) 
      (prove-2 $REST $REST_PROOF) 
      (append $HEAD_PROOF $REST_PROOF $PROOF)))
; 

  (= 
    (prove-2 $FACT 
      (:: $FACT)) 
    (get-symbols  &self 
      (= $FACT true)))
; 

  (= 
    (prove-2 $GOAL $PROOF) 
    ( (get-symbols  &self 
        (= $GOAL $PREMISSES)) 
      (not (== $PREMISSES True)) 
      (prove-2 $PREMISSES $PREM_PROOF) 
      (append 
        (:: $GOAL) 
        (:: $PREM_PROOF) $PROOF)))
; 

  (= 
    (prove-2 $GOAL 
      (:: $GOAL)) 
    (call $GOAL))
; 



  (= 
    (prove-3 
      (, $HEAD $REST) $PROOF) 
    ( (set-det) 
      (prove-3 $HEAD $HEAD_PROOF) 
      (prove-3 $REST $REST_PROOF) 
      (append $HEAD_PROOF $REST_PROOF $PROOF)))
; 

;                              (error
;                                (syntax_error  operator_expected)
;                                (file  ebg/ebg.pl 124 8 6850))

  (= 
    (prove-3 $GOAL $PROOF) 
    ( (get-symbols  &self 
        (= $GOAL $PREMISSES)) 
      (not (== $PREMISSES True)) 
      (prove-3 $PREMISSES $PREM_PROOF) 
      (append 
        (:: $GOAL) 
        (:: $PREM_PROOF) $PROOF)))
; 

  (= 
    (prove_3  $GOAL 
      ($GOAL)) True)
; 



  (= 
    (prove-4 
      (or $HEAD $REST) $PROOF) 
    ( (set-det) (or (prove-4 $HEAD $PROOF) (prove-4 $REST $PROOF))))
; 
 
  (= 
    (prove-4 
      (, $HEAD $REST) $PROOF) 
    ( (set-det) 
      (prove-4 $HEAD $HEAD_PROOF) 
      (prove-4 $REST $REST_PROOF) 
      (append $HEAD_PROOF $REST_PROOF $PROOF)))
; 

;                                 (error
;                                   (syntax_error  operator_expected)
;                                   (file  ebg/ebg.pl 155 8 8557))
 
;                                 (error
;                                   (syntax_error  operator_expected)
;                                   (file  ebg/ebg.pl 157 8 8632))

  (= 
    (prove_4  $GOAL 
      ($GOAL)) True)
; 



  (= 
    (prove-5 
      (, $HEAD $REST) 
      (Cons  $PROOF_HEAD $PROOF_REST) $LIST) 
    ( (set-det) 
      (prove-5 $HEAD $PROOF_HEAD $PROOF_LIST1) 
      (prove-5 $REST $PROOF_REST $PROOF_LIST2) 
      (append $PROOF_LIST1 $PROOF_LIST2 $LIST)))
; 

;                                   (error
;                                     (syntax_error  operator_expected)
;                                     (file  ebg/ebg.pl 187 8 10387))

  (= 
    (prove-5 $GOAL 
      (:: $GOAL $PROOF) $LIST) 
    ( (get-symbols  &self 
        (= $GOAL $PREMISSES)) 
      (not (== $PREMISSES True)) 
      (prove-5 $PREMISSES $PROOF $LIST)))
; 

  (= 
    (prove_5  $GOAL 
      ($GOAL) 
      ($GOAL)) True)
; 



  (= 
    (prove-6 
      (, $HEAD $REST) 
      (, $GEN_HEAD $GEN_REST) $LIST) 
    ( (set-det) 
      (prove-6 $HEAD $GEN_HEAD $LIST1) 
      (prove-6 $REST $GEN_REST $LIST2) 
      (append $LIST1 $LIST2 $LIST)))
; 

  (= 
    (prove-6 $GOAL $GEN_GOAL 
      (:: $GEN_GOAL)) 
    ( (operational $GOAL) (set-det)))
; 

;                                     (error
;                                       (syntax_error  operator_expected)
;                                       (file  ebg/ebg.pl 218 8 12142))



  (= 
    (listify  
      ($H) $H) True)
; 

  (= 
    (listify 
      (Cons  $H $R) 
      (, $H $S)) 
    (listify $R $S))
; 



  (= 
    (copy $TERM1 $TERM2) 
    ( (add-symbol  &self 
        (internal  $TERM1)) 
      (remove-symbol  &self 
        (internal  $TERM2)) 
      (set-det)))
; 



  (= 
    (operational $A) 
    (get-symbols  &self 
      (= $A true)))
; 

;                                        (error
;                                          (syntax_error  operator_expected)
;                                          (file  ebg/ebg.pl 277 8 15501))



  (= 
    (ebg $GOAL $RULE) 
    ( (functor $GOAL $F $N) 
      (functor $COPY $F $N) 
      (call $GOAL) 
      (prove-6 $GOAL $COPY $ZWERG1) 
      (listify $ZWERG1 $ZWERG2) 
      (= $RULE 
        (= $COPY $ZWERG2))))
; 



  (= 
    (help) 
    ( (write 'Load example theories and start EBG with command: [Filename].') (nl)))
; 



  !(help *)
; 


