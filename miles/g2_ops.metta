;
; 


  !(module g2-ops 
    (:: 
      (/ intra-construct1 5) 
      (/ intra-construct1 6) 
      (/ intra-construct1 7) 
      (/ intra-construct2 5) 
      (/ intra-construct2 6) 
      (/ intra-construct2 7) 
      (/ g2-op 5) 
      (/ apply-g2 5) 
      (/ apply-g2 3) 
      (/ apply-g2 2)))
; 


;
; 

  !(use-module 
    (home lgg) 
    (:: 
      (/ lgti 6) 
      (/ lgg 5) 
      (/ buildlgg 4) 
      (/ gti 5) 
      (/ lgti 5)))
; 
 ;
; 

  !(use-module 
    (home kb) 
    (:: 
      (/ get-clause 5) 
      (/ store-clause 4) 
      (/ delete-clause 1) 
      (/ delete-all 1)))
; 

  !(use-module 
    (home var-utils) 
    (:: 
      (/ relevant-vars2 6) 
      (/ relevant-vars3 6) 
      (/ skolemize 3) 
      (/ deskolemize 3) 
      (/ replace 4) 
      (/ inv-replace 4) 
      (/ exists-intersect 3) 
      (/ findargs 3) 
      (/ allarg 4) 
      (/ buildrelterms 6)))
; 

  !(use-module 
    (home div-utils) 
    (:: 
      (/ effaceall 3) 
      (/ genterm-test 2)))
; 

  !(use-module 
    (home g1-ops) 
    (:: (/ g1-op 4)))
; 

  !(use-module 
    (home environment) 
    (:: 
      (/ oracle 2) 
      (/ confirm 2) 
      (/ get-ci 2)))
; 

  !(use-module 
    (home evaluation) 
    (:: (/ complexity 2)))
; 

  !(use-module-if-exists 
    (library strings) 
    (:: (/ gensym 2)))
; 

  !(use-module-if-exists 
    (library basics) 
    (:: (/ member 2)))
; 

  !(use-module-if-exists 
    (library sets) 
    (:: (/ subtract 3)))
; 

  !(use-module-if-exists 
    (library not) 
    (:: (/ once 1)))
; 



;
; 

;
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (intra-construct1 $IDC1 $IDC2 $IDA $IDB1 $IDB2) 
    ( (gensym new-pred $NewPred) (intra-construct1 $IDC1 $IDC2 $IDA $IDB1 $IDB2 $NewPred)))
; 



  (= 
    (intra-construct1 $IDC1 $IDC2 $IDA $IDB1 $IDB2 $NewPred) 
    (intra-construct1 $IDC1 $IDC2 $IDA $IDB1 $IDB2 $NewPred 10))
; 
 

  (= 
    (intra-construct1 $IDC1 $IDC2 $IDA $IDB1 $IDB2 $NewPred $Bound) 
    ( (atom $NewPred) 
      (get-clause $IDC1 $_ $_ $C1 $_) 
      (get-clause $IDC2 $_ $_ $C2 $_) 
      (set-det) 
      (lgti $C1 $C2 $G $S1 $S2 $Bound) 
      (once (, (relevant-vars3 $C1 $C2 $G $S1 $S2 $Vars) (=.. $NewLit (Cons  $NewPred $Vars)) (append $G (:: (with_self  $NewLit (n))) $A) (copy-term (, $C1 $G $S1 $NewLit) (, $C11 $G11 $S11 $NewLit11)) (skolemize (, $C11 $G11 $S11 $NewLit11) $Phi1 (, $C12 $G12 $S12 $NewLit12)) (replace $G12 $S12 $G13 $S12) (replace (:: $NewLit12) $S12 (:: $NewLit13) $S12) (subtract $C12 $G13 $B1BodyS) (= $B1S (Cons  (with_self  $NewLit13 (p)) $B1BodyS)) (deskolemize $B1S $Phi1 $B1) (copy-term (, $C2 $G $S2 $NewLit) (, $C21 $G21 $S21 $NewLit21)) (skolemize (, $C21 $G21 $S21 $NewLit21) $Phi2 (, $C22 $G22 $S22 $NewLit22)) (replace $G22 $S22 $G23 $S22) (replace (:: $NewLit22) $S22 (:: $NewLit23) $S22) (subtract $C22 $G23 $B2BodyS) (= $B2S (Cons  (with_self  $NewLit23 (p)) $B2BodyS)) (deskolemize $B2S $Phi2 $B2) (store-clause $_ $A ic $IDA) (store-clause $_ $B1 ic $IDB1) (store-clause $_ $B2 ic $IDB2)))))
; 
        



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (intra-construct2 $IDC1 $IDC2 $IDA $IDB1 $IDB2) 
    ( (gensym new-pred $NewPred) (intra-construct2 $IDC1 $IDC2 $IDA $IDB1 $IDB2 $NewPred)))
; 



  (= 
    (intra-construct2 $IDC1 $IDC2 $IDA $IDB1 $IDB2 $NewPred) 
    (intra-construct2 $IDC1 $IDC2 $IDA $IDB1 $IDB2 $NewPred 10))
; 
 

  (= 
    (intra-construct2 $IDC1 $IDC2 $IDA $IDB1 $IDB2 $NewPred $Bound) 
    ( (atom $NewPred) 
      (get-clause $IDC1 $_ $_ $C1 $_) 
      (get-clause $IDC2 $_ $_ $C2 $_) 
      (set-det) 
      (lgti $C1 $C2 $G $S1 $S2 $Bound) 
      (once (, (relevant-vars2 $C1 $C2 $G $S1 $S2 $Vars) (=.. $NewLit (Cons  $NewPred $Vars)) (append $G (:: (with_self  $NewLit (n))) $A) (copy-term (, $C1 $G $S1 $NewLit) (, $C11 $G11 $S11 $NewLit11)) (skolemize (, $C11 $G11 $S11 $NewLit11) $Phi1 (, $C12 $G12 $S12 $NewLit12)) (replace $G12 $S12 $G13 $S12) (replace (:: $NewLit12) $S12 (:: $NewLit13) $S12) (subtract $C12 $G13 $B1BodyS) (= $B1S (Cons  (with_self  $NewLit13 (p)) $B1BodyS)) (deskolemize $B1S $Phi1 $B1) (copy-term (, $C2 $G $S2 $NewLit) (, $C21 $G21 $S21 $NewLit21)) (skolemize (, $C21 $G21 $S21 $NewLit21) $Phi2 (, $C22 $G22 $S22 $NewLit22)) (replace $G22 $S22 $G23 $S22) (replace (:: $NewLit22) $S22 (:: $NewLit23) $S22) (subtract $C22 $G23 $B2BodyS) (= $B2S (Cons  (with_self  $NewLit23 (p)) $B2BodyS)) (deskolemize $B2S $Phi2 $B2) (store-clause $_ $A ic $IDA) (store-clause $_ $B1 ic $IDB1) (store-clause $_ $B2 ic $IDB2)))))
; 
        


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (g2-op $C1 $C2 $A $B1 $B2) 
    ( (get-clause $C1 $_ $_ $C1list $_) 
      (get-clause $C2 $_ $_ $C2list $_) 
      (lgg $C1list $C2list $Clgg $S1 $S2) 
      (not-unary $Clgg) 
      (buildrelterms $C1list $C2list $Clgg $S1 $S2 $Terms) 
      (buildreslit $Terms $L) 
      (buildparentA $Clgg $L $Alist) 
      (store-clause $_ $Alist g2 $A) 
      (det-if-then-else 
        (, 
          (g1-op $C1 $A $B1 g2g1) 
          (g1-op $C2 $A $B2 g2g1)) 
        (det-if-then-else 
          (compression-heuristic 
            (:: $A $B1 $B2) 
            (:: $C1 $C2)) 
          (, 
            (confirm 
              (:: $A $B1 $B2) $L) 
            (delete-all (:: $C1 $C2)) 
            (nl) 
            (write 'Resolvent clauses deleted.')) 
          (, 
            (nl) 
            (write 'G2: No compression achieved.') 
            (nl) 
            (fail))) 
        (delete-clause $A))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (apply-g2 $C1 $C2 $A $B1 $B2) 
    ( (g2-op $C1 $C2 $A $B1 $B2) (set-det)))
; 



  (= 
    (apply-g2 $A $BB) 
    ( (get-ci Nil $CC) 
      (apply-g2 $CC $A $BB) 
      (set-det)))
; 



  (= 
    (apply-g2 $CC $A $BB) 
    ( (sort $CC $CCsort) 
      (gensym new-p $N) 
      (findall $Aij 
        (, 
          (member $Ci $CCsort) 
          (member $Cj $CCsort) 
          (< $Ci $Cj) 
          (g2-op-A $Ci $Cj $N $Aij)) $AA) 
      (= $AA 
        (Cons  $A1 $An)) 
      (buildlgg $An $A1 $A g2) 
      (delete-all $AA) 
      (findall $Bi 
        (, 
          (member $Ci $CC) 
          (g1-op $Ci $A $Bi g2g1)) $BB) 
      (length $CC $NoC) 
      (det-if-then-else 
        (length $BB $NoC) 
        (det-if-then-else 
          (compression-heuristic 
            (Cons  $A $BB) $CC) 
          (, 
            (confirm 
              (Cons  $A $BB) $N) 
            (delete-all $CC) 
            (nl) 
            (write 'Resolvent clauses deleted.')) 
          (, 
            (nl) 
            (write 'G2: No compression achieved.') 
            (nl) 
            (fail))) 
        (, 
          (delete-all $BB) 
          (delete-clause $A) 
          (fail)))))
; 


	

  (= 
    (g2-op-A $C1 $C2 $Name $A) 
    ( (get-clause $C1 $_ $_ $C1list $_) 
      (get-clause $C2 $_ $_ $C2list $_) 
      (lgg $C1list $C2list $Clgg $S1 $S2) 
      (buildrelterms $C1list $C2list $Clgg $S1 $S2 $T) 
      (not-unary $Clgg) 
      (length $T $N) 
      (functor $L $Name $N) 
      (setargs $N $T $L) 
      (buildparentA $Clgg $L $Alist) 
      (store-clause $_ $Alist g2 $A)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (not-unary (:: (with_self  $_ (p)))) 
    ( (nl) 
      (write 'No compression achievable.') 
      (set-det) 
      (fail)))
; 

  (= 
    (not-unary (:: (with_self  (True *) (p)) $_)) 
    ( (nl) 
      (write 'No compression achievable.') 
      (set-det) 
      (fail)))
; 

  (= 
    (not_unary  $_) True)
; 

	

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (buildreslit $T $L) 
    ( (length $T $N) 
      (gensym new-p $F) 
      (functor $L $F $N) 
      (setargs $N $T $L)))
; 



  (= 
    (setargs 0 Nil $_) 
    (set-det))
; 

  (= 
    (setargs $N 
      (Cons  $Arg1 $Rest) $L) 
    ( (arg $N $L $Arg1) 
      (is $M 
        (- $N 1)) 
      (setargs $M $Rest $L)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (buildparentA 
      (Cons  
        (with_self  
          (True *) 
          (p)) $Rest) $L 
      (Cons  
        (with_self  $L 
          (p)) $Rest)) 
    (set-det))
; 

  (= 
    (buildparentA $List $L $Alist) 
    ( (append $List 
        (:: (with_self  $L (n))) $Alist) (set-det)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (compression-heuristic $New_cl $Old_cl) 
    ( (complexity $Old_cl $Cold) 
      (complexity $New_cl $Cnew) 
      (det-if-then-else 
        (< $Cnew $Cold) True 
        (, 
          (delete-all $New_cl) 
          (fail)))))
; 


