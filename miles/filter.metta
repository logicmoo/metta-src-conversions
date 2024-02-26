;
; 


  !(module filter 
    (:: 
      (/ is-flat 1) 
      (/ is-unflat 1) 
      (/ connected-vars 4) 
      (/ truncate-unconnected 2) 
      (/ truncate-unconnected 1) 
      (/ is-weakly-generative 1) 
      (/ is-strongly-generative 1) 
      (/ is-connected 1) 
      (/ truncate 3) 
      (/ truncate-r 2) 
      (/ truncate-r 1) 
      (/ truncate-flat-r 1) 
      (/ truncate-unconnecting 1) 
      (/ truncate-unconnecting 2) 
      (/ truncate-strongly-generative 1) 
      (/ truncate-strongly-generative 2) 
      (/ truncate-neg-based 1) 
      (/ truncate-flat-neg-based 1) 
      (/ truncate-facts 1) 
      (/ truncate-j 2) 
      (/ noduplicate-symbols 1) 
      (/ noduplicate-symbol 2) 
      (/ select-var-sharing-lits 2) 
      (/ already-in 3)))
; 


 
;
; 

  !(use-module-if-exists 
    (library not) 
    (:: (/ once 1)))
; 

  !(use-module-if-exists 
    (library sets) 
    (:: 
      (/ union 2) 
      (/ union 3) 
      (/ intersection 3) 
      (/ list-to-set 2) 
      (/ subtract 3) 
      (/ subset 2) 
      (/ select 3)))
; 

  !(use-module-if-exists 
    (library strings) 
    (:: 
      (/ string-append 3) 
      (/ substring 4) 
      (/ midstring 6)))
; 

  !(use-module-if-exists 
    (library basics) 
    (:: (/ member 2)))
; 

  !(use-module-if-exists 
    (library lists) 
    (:: 
      (/ subseq 3) 
      (/ rev 2) 
      (/ last 2) 
      (/ nth1 4)))
; 

  !(use-module-if-exists 
    (library occurs) 
    (:: (/ sub-term 2)))
; 

  !(use-module-if-exists 
    (library arg) 
    (:: (/ genarg 3)))
; 

  !(use-module-if-exists 
    (library subsumes) 
    (:: (/ subsumes-chk 2)))
; 

  !(use-module 
    (home div-utils) 
    (:: (/ shares-var 2)))
; 

  !(use-module 
    (home var-utils) 
    (:: 
      (/ vars 2) 
      (/ contains-vars 2) 
      (/ skolemize 3) 
      (/ deskolemize 3) 
      (/ skolems 2) 
      (/ flagged-contains-vars 3)))
; 

  !(use-module 
    (home kb) 
    (:: 
      (/ get-clause 5) 
      (/ store-clause 4) 
      (/ delete-clause 1) 
      (/ get-fact 4) 
      (/ unflatten-kb 0)))
; 

  !(use-module 
    (home flatten) 
    (:: 
      (/ flatten-clause 2) 
      (/ unflatten-clause 2)))
; 

  !(use-module 
    (home evaluation) 
    (:: (/ correct-chk 0)))
; 



;
; 

;
; 


  !(dynamic (/ functional-mode 1))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (already-in 0 $_ $_) 
    ( (set-det) (fail)))
; 

  (= 
    (already-in $N $P $Y) 
    ( (arg $N $P $Pn) (det-if-then-else (== $Pn $Y) True (, (is $N1 (- $N 1)) (already-in $N1 $P $Y)))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (select_var_sharing_lits  () ()) True)
; 

  (= 
    (select-var-sharing-lits 
      (Cons  $C $R) $R2) 
    ( (select-var-sharing-lits $R $R1) (det-if-then-else (var-sharing-lit $C Nil) (= $R2 (Cons  $C $R1)) (= $R2 $R1))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (var-sharing-lit 
      (= $H $B) Nil) 
    ( (set-det) (var-sharing-lit $B (:: $H))))
; 

  (= 
    (var-sharing-lit 
      (, $A $B) $C) 
    ( (set-det) (var-sharing-lit $B (Cons  $A $C))))
; 

  (= 
    (var-sharing-lit $A $C) 
    ( (shares-var $A $C) (set-det)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (noduplicate-atom $P 
      (, $A $B)) 
    ( (set-det) 
      (\== $P $A) 
      (noduplicate-atom $P $B)))
; 

  (= 
    (noduplicate-atom $P $A) 
    (\== $P $A))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (noduplicate-atoms (= $A $B)) 
    ( (set-det) 
      (noduplicate-atom $A $B) 
      (noduplicate-atoms $B)))
; 

  (= 
    (noduplicate-atoms (, $A $B)) 
    ( (set-det) 
      (noduplicate-atom $A $B) 
      (noduplicate-atoms $B)))
; 

  (= 
    (noduplicate_symbols  $_) True)
; 

   

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
;                                             (file  miles/filter.pl 242 29 6845))



  (= 
    (connected-skolems 
      (Cons  
        (with_self  $Head 
          (p)) $Body) $Connected $Unconnected) 
    ( (skolems $Head $Con) 
      (= $Uncon Nil) 
      (find-connected-skolems-in-body $Body $Con $Connected $Uncon $Unconnected_tupels) 
      (union $Unconnected_tupels $Unconnected)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (find-connected-skolems-in-body 
      (Cons  
        (with_self  $L $_) $Rest) $C1 $C2 $U1 $U2) 
    ( (skolems $L $V1) 
      (det-if-then-else 
        (, 
          (intersection $C1 $V1 $I) 
          (\== $I Nil)) 
        (, 
          (union $C1 $V1 $C4) 
          (connect $V1 $C4 $C3 $U1 $U3)) 
        (, 
          (= $C3 $C1) 
          (= $U3 
            (Cons  $V1 $U1)))) 
      (find-connected-skolems-in-body $Rest $C3 $C2 $U3 $U2)))
; 

  (= 
    (find_connected_skolems_in_body  () $C $C $U $U) True)
; 



  (= 
    (connect  $V $C $C () ()) True)
; 

  (= 
    (connect $Vars $C1 $C2 
      (Cons  $Tupel $More) $U2) 
    ( (member $Var $Vars) 
      (member $Var $Tupel) 
      (set-det) 
      (union $C1 $Tupel $C3) 
      (connect $Vars $C3 $C2 $More $U2)))
; 

  (= 
    (connect $Vars $C1 $C2 
      (Cons  $Tupel $More) 
      (Cons  $Tupel $U2)) 
    (connect $Vars $C1 $C2 $More $U2))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (is-weakly-generative (Cons  (with_self  $H (p)) $B)) 
    ( (vars $H $Vars) (contains-vars $Vars $B)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (is-strongly-generative $Clause) 
    ( (findall $Flag 
        (, 
          (subseq $Clause 
            (:: $L) $Rest) 
          (vars $L $Vars) 
          (flagged-contains-vars $Vars $Rest $Flag)) $Flags) 
      (not (member False $Flags)) 
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



  (= 
    (is-connected $Clause) 
    (connected-vars $Clause $_ $_ Nil))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (is-flat $Clause) 
    ( (nonvar $Clause) (not (is-unflat $Clause))))
; 



  (= 
    (is-unflat (Cons  (with_self  $L $_) $Rest)) 
    (is-unflat-literal $L))
; 

  (= 
    (is-unflat (Cons  $_ $Rest)) 
    (is-unflat $Rest))
; 
                  


  (= 
    (is-unflat-literal $L) 
    ( (sub-term $Subterm $L) 
      (\== $Subterm $L) 
      (nonvar $Subterm) 
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

;
; 

;
; 



  (= 
    (truncate $Strategy $In $Out) 
    ( (get-clause $In $_ $_ $C1 $_) 
      (do-truncate $Strategy $C1 $C2) 
      (store-clause $_ $C2 trunc $Out)))
; 



  (= 
    (do-truncate r $C1 $C2) 
    (truncate-r $C1 $C2))
; 

  (= 
    (do-truncate unconnected $C1 $C2) 
    (truncate-unconnected $C1 $C2))
; 

  (= 
    (do-truncate strongly-generative $C1 $C2) 
    (truncate-strongly-generative $C1 $C2))
; 

  (= 
    (do-truncate unconnecting $C1 $C2) 
    (truncate-unconnecting $C1 $C2))
; 
 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (truncate-r $ID) 
    ( (get-clause $ID $_ $_ $ClauseIn $_) 
      (truncate-r $ClauseIn $ClauseOut) 
      (delete-clause $ID) 
      (store-clause $_ $ClauseOut trc $ID)))
; 


  (= 
    (truncate-r $ClauseIn $ClauseOut) 
    ( (copy-term $ClauseIn $C) (do-truncate-r $C $ClauseOut)))
; 



  (= 
    (do_truncate_r  () ()) True)
; 

  (= 
    (do-truncate-r 
      (Cons  
        (with_self  $L 
          (r)) $Rest) $Rest1) 
    ( (set-det) (do-truncate-r $Rest $Rest1)))
; 

  (= 
    (do-truncate-r 
      (Cons  
        (with_self  $L $S) $Rest) 
      (Cons  
        (with_self  $L $S) $Rest1)) 
    ( (set-det) (do-truncate-r $Rest $Rest1)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (truncate-flat-r $ID) 
    ( (get-clause $ID $_ $_ $ClauseIn $_) 
      (truncate-flat-r $ClauseIn $ClauseOut) 
      (delete-clause $ID) 
      (store-clause $_ $ClauseOut trc $ID)))
; 


  (= 
    (truncate-flat-r $ClauseIn $ClauseOut) 
    ( (copy-term $ClauseIn $C) (do-truncate-flat-r $C $ClauseOut)))
; 



  (= 
    (do_truncate_flat_r  () ()) True)
; 

  (= 
    (do-truncate-flat-r 
      (Cons  
        (with_self  $L 
          (r)) $Rest) $Rest1) 
    ( (functor $L $F $N) 
      (functor $LC $F $N) 
      (get-clause $_ $LC $_ $_ usr) 
      (set-det) 
      (do-truncate-flat-r $Rest $Rest1)))
; 

  (= 
    (do-truncate-flat-r 
      (Cons  
        (with_self  $L $S) $Rest) 
      (Cons  
        (with_self  $L $S) $Rest1)) 
    ( (set-det) (do-truncate-flat-r $Rest $Rest1)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (truncate-unconnected $ID) 
    ( (get-clause $ID $_ $_ $C $_) 
      (truncate-unconnected $C $D) 
      (delete-clause $ID) 
      (store-clause $_ $D trc-unconn $ID) 
      (set-det)))
; 




  (= 
    (truncate-unconnected $ClauseIn $ClauseOut) 
    ( (skolemize $ClauseIn $S $ClauseInS) 
      (connected-skolems $ClauseInS $Con $Uncon) 
      (= $ClauseInS 
        (Cons  
          (with_self  $Head 
            (p)) $Body)) 
      (truncate-unconnected1 $Body $Uncon $BodyOut) 
      (= $ClauseOutS 
        (Cons  
          (with_self  $Head 
            (p)) $BodyOut)) 
      (deskolemize $ClauseOutS $S $ClauseOut)))
; 



  (= 
    (truncate_unconnected1  () $_ ()) True)
; 
  ;
; 

  (= 
    (truncate-unconnected1 $B Nil $B) 
    (set-det))
; 
   ;
; 

  (= 
    (truncate-unconnected1 
      (Cons  
        (with_self  $L $_) $More) $Uncon $BodyOut) 
    ( (det-if-then-else 
        (, 
          (skolems $L $SKs) 
          (member $A $SKs)) 
        (, 
          (member $A $Uncon) 
          (truncate-unconnected1 $More $Uncon $BodyOut)) fail) (set-det)))
; 

  (= 
    (truncate-unconnected1 
      (Cons  
        (with_self  $L $S) $More) $Uncon 
      (Cons  
        (with_self  $L $S) $BodyOut)) 
    ( (truncate-unconnected1 $More $Uncon $BodyOut) (set-det)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (truncate-unconnecting $ID) 
    ( (get-clause $ID $_ $_ $C $_) 
      (truncate-unconnecting $C $D) 
      (delete-clause $ID) 
      (store-clause $_ $D trc-unconn $ID) 
      (set-det)))
; 


  (= 
    (truncate-unconnecting $ClauseIn $ClauseOut) 
    ( (copy-term $ClauseIn 
        (Cons  
          (with_self  $H 
            (p)) $Body)) 
      (subseq $Body 
        (:: $L) $BodyOut) 
      (= $ClauseOut1 
        (Cons  
          (with_self  $H 
            (p)) $BodyOut)) 
      (connected-vars $ClauseOut1 $ClauseOut $Con Nil) 
      (is-weakly-generative $ClauseOut)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (truncate-strongly-generative $ID) 
    ( (get-clause $ID $_ $_ $C $_) 
      (truncate-strongly-generative $C $D) 
      (delete-clause $ID) 
      (store-clause $_ $D trc-unconn $ID) 
      (set-det)))
; 


  (= 
    (truncate-strongly-generative $ClauseIn $ClauseOut) 
    ( (copy-term $ClauseIn 
        (Cons  
          (with_self  $H 
            (p)) $Body)) 
      (subseq $Body 
        (:: $L) $BodyOut) 
      (= $ClauseOut 
        (Cons  
          (with_self  $H 
            (p)) $BodyOut)) 
      (is-strongly-generative $ClauseOut)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (truncate-neg-based $ID) 
    ( (or 
        (get-clause $ID $_ $_ $C $Label) 
        (, 
          (store-clause $_ $C $Label $ID) 
          (fail))) 
      (delete-clause $ID) 
      (once (truncate-unconnected $C $D)) 
      (store-clause $_ $D trc $ID) 
      (correct-chk) 
      (= $D 
        (Cons  $H $Body)) 
      (length $Body $N) 
      (truncate-neg-based1 $N $ID $H $Body)))
; 
 


  (= 
    (truncate-neg-based1 0 $_ $_ $_) 
    (set-det))
; 


  (= 
    (truncate-neg-based1 $N $ID $H $Body) 
    ( (nth1 $N $Body $L $NewBody) 
      (delete-clause $ID) 
      (store-clause $_ 
        (Cons  $H $NewBody) trc $ID) 
      (is $M 
        (- $N 1)) 
      (det-if-then-else 
        (, 
          (is-weakly-generative (Cons  $H $NewBody)) 
          (correct-chk)) 
        (truncate-neg-based1 $M $ID $H $NewBody) 
        (, 
          (delete-clause $ID) 
          (store-clause $_ 
            (Cons  $H $Body) trc $ID) 
          (truncate-neg-based1 $M $ID $H $Body)))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (truncate-flat-neg-based $ID) 
    ( (get-clause $ID $_ $_ $C $Label) 
      (flatten-clause $C $D) 
      (truncate-unconnected $D $E) 
      (delete-clause $ID) 
      (store-clause $_ $E $Trc $ID) 
      (set-det) 
      (or 
        (, 
          (once correct-chk) 
          (= $D 
            (Cons  $H $Body)) 
          (truncate-flat-neg-based $ID $H Nil $Body)) 
        (, 
          (delete-clause $ID) 
          (store-clause $_ $C $Label $ID)))))
; 


  (= 
    (truncate-flat-neg-based $ID $H $Nec Nil) 
    ( (delete-clause $ID) 
      (truncate-unconnected 
        (Cons  $H $Nec) $D) 
      (unflatten-clause $D $E) 
      (store-clause $_ $E trc $ID) 
      (set-det) 
      (correct-chk)))
; 



  (= 
    (truncate-flat-neg-based $ID $H $Nec 
      (Cons  $L $Maybe)) 
    ( (append $Nec $Maybe $C) 
      (truncate-unconnected 
        (Cons  $H $C) $D) 
      (unflatten-clause $D $E) 
      (delete-clause $ID) 
      (store-clause $_ $E trc $ID) 
      (det-if-then-else correct-chk 
        (= $Nec1 $Nec) 
        (append $Nec 
          (:: $L) $Nec1)) 
      (truncate-flat-neg-based $ID $H $Nec1 $Maybe)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (truncate-facts $ID) 
    ( (get-clause $ID $_ $_ 
        (Cons  $H $B) $_) 
      (truncate-facts1 $B $BodyOut) 
      (delete-clause $ID) 
      (store-clause $_ 
        (Cons  $H $BodyOut) trc $ID)))
; 



  (= 
    (truncate_facts1  () ()) True)
; 

  (= 
    (truncate-facts1 
      (Cons  
        (with_self  $L $S) $Rest) $BodyOut) 
    ( (truncate-facts1 $Rest $Rest1) (det-if-then-else (, (get-fact $_ $L1 $_ usr) (subsumes-chk $L1 $L)) (= $BodyOut $Rest1) (= $BodyOut (Cons  (with_self  $L $S) $Rest1)))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (truncate-j $ID $J) 
    ( (get-clause $ID $_ $_ $C $_) 
      (skolemize $C $S 
        (Cons  
          (with_self  $Head 
            (p)) $BodyIn)) 
      (skolems $Head $Vars) 
      (do-truncate-j $J $Vars $BodyIn $BodyOutS) 
      (deskolemize 
        (Cons  
          (with_self  $Head 
            (p)) $BodyOutS) $S $D) 
      (truncate-unconnected $D $E) 
      (delete-clause $ID) 
      (store-clause $_ $E trc-j $ID)))
; 



  (= 
    (do-truncate-j $J $Vars $BodyIn $BodyOut) 
    (findall $L 
      (, 
        (member $L $BodyIn) 
        (once (, (skolems $L $VarsL) (subtract $VarsL $Vars $NewVars) (length $NewVars $J1) (=< $J1 $J)))) $BodyOut))
; 

                    

