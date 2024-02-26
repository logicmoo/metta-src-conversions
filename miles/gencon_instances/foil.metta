;                                           (convert_to_metta_file  foil $_163290 miles/gencon_instances/foil.pl miles/gencon_instances/foil.metta)
;
; MODULE foil EXPORTS


  !(module foil 
    (:: 
      (/ learn-foil 0) 
      (/ infogain 3)))


;
; IMPORTS

  !(use-module 
    (home kb) 
    (:: 
      (/ store-clause 4) 
      (/ get-evaluation 2) 
      (/ delete-clause 1) 
      (/ get-example 3) 
      (/ delete-example 1) 
      (/ get-clause 5) 
      (/ known 6) 
      (/ store-ex 3)))

  !(use-module 
    (home evaluation) 
    (:: 
      (/ eval-examples 0) 
      (/ encoding-length-examples 1) 
      (/ encoding-length-clause 2)))

  !(use-module 
    (home tdref-it) 
    (:: (/ refinement-add-body-literal 2)))

  !(use-module 
    (home div-utils) 
    (:: 
      (/ body2list 2) 
      (/ log2 2) 
      (/ log2nueberk 3) 
      (/ mysetof 3)))

  !(use-module 
    (home gencon) 
    (:: (/ gilppi 12)))

  !(use-module 
    (home show-utils) 
    (:: (/ show-kb 0)))



;
; METAPREDICATES
;
; none

;
; ************************************************************************
;
; *
;
; * module: foil.pl
;
; *
;
; * author: Irene Stahl date: 1. 7. 1993
;
; *
;
; * changed:
;
; *
;
; * description: simple implementation of infogain heuristic
;
; * foil as instantiation of the generic algorithm (25. 11. 93)
;
; *
;
; * see also:
;
; *
;
; ************************************************************************


  (= (learn-foil)  
    (gilppi initialize stop-c quality-c update select add filter one-of spec gen l-newp output))



  !(dynamic (/ el-ex 1))
  !(dynamic (/ total-ex 1))

  (= (initialize (:: (, (with_self  (:: $MGT) 0) (active))))  
    ( (mysetof 
        (, $P $N) 
        (^ $ID 
          (^ $L 
            (, 
              (get-example $ID $F $L) 
              (functor $F $P $N)))) 
        (:: (, $P1 $N1))) 
      (functor $MGT $P1 $N1) 
      (encoding-length-examples $X) 
      (add-is-symbol  &self 
        (el_ex  $X)) 
      (mysetof $ID1 
        (^ $F1 
          (^ $L1 
            (get-example $ID1 $F1 $L1))) $IDL) 
      (length $IDL $TE) 
      (add-is-symbol  &self 
        (total_ex  $TE))))



  (= (select  (Cons  (,  (:  $C $G) active) $R) $C active (Cons  (,  (:  $C $G) passive) $R))  True)
  (= (select (Cons  $X $R) $C active (Cons  $X $R1))  
    (select $R $C active $R1))



  (= (quality-c (:: $C))  
    (store-clause $C $_ usr $ID)
    (eval-examples)
    (get-clause $ID $H $B $CL $_)
    (get-evaluation $ID $E)
    (det-if-then-else 
      (arg 5 $E Nil) 
      (, 
        (arg 3 $E $Pos) 
        (remove-covered-ex $Pos)) 
      (, 
        (delete-clause $ID) 
        (add-is-symbol  &self 
          (known  $ID $H $B $CL hypo $E)) 
        (set-det) 
        (fail))))


  (= (remove_covered_ex  ())  True)
  (= (remove-covered-ex (Cons  (with_self  $ID $Fact) $R))  
    ( (delete-example $ID) 
      (add-is-symbol  &self 
        (saved_ex  $ID $Fact)) 
      (remove-covered-ex $R)))


  (= (update $_ (:: (, (with_self  (:: $MGT) 0) (active))))  
    (mysetof 
      (, $P $N) 
      (^ $ID 
        (, 
          (get-example $ID $F +) 
          (functor $F $P $N))) 
      (:: (, $P1 $N1)))
    (functor $MGT $P1 $N1)
    (set-det))
  (= (update  $_ ())  True)


  (= (one_of  $_ spec)  True)


  (= (spec (:: $C) $PSL)  
    (refinement-add-body-literal $C $CL)
    (infogain $CL $PSL))
   

  (= (add  $PS () $PS)  True)
  (= (add $PS (Cons  $X $R) $PS1)  
    (insert-by-gain $X $PS $PS0)
    (add $PS0 $R $PS1))


  (= (insert-by-gain (with_self  $C $G) (Cons  (, (with_self  $C1 $G1) $L) $R) (Cons  (, (with_self  $C1 $G1) $L) $R1))  
    (< $G $G1)
    (set-det)
    (insert-by-gain 
      (with_self  $C $G) $R $R1))
  (= (insert_by_gain  (:  $C $G) $L (Cons  (,  (:  ($C) $G) active) $L))  True)



  (= (filter  $L $L)  True)


  (= (stop-c $_)  
    (not (get-example $_ $_ +))
    (set-det))
  (= (stop-c $CL)  
    (stop-c1 $CL $N)
    (el-ex $X)
    (> $N $X))


  (= (stop_c1  () 0)  True)
  (= (stop-c1 (Cons  (:: (= $H  $B)) $R) $M)  
    (stop-c1 $R $M0)
    (body2list $B $BL)
    (encoding-length-clause 
      (Cons  
        (with_self  $H 
          (p)) $BL) $M1)
    (is $M 
      (+ $M0 $M1)))


  (= (output $_)  
    ( (remove-is-symbol  &self 
        (saved_ex  $ID $Fact)) 
      (store-ex $Fact + $ID) 
      (output $_)))
  (= (output $_)  
    ( (remove-all-atoms  &self 
        (total_ex  $_)) 
      (remove-all-atoms  &self 
        (el_ex  $_)) 
      (show-kb)))
   



;
; ************************************************************************
;
; *
;
; * predicate: info_gain/3
;
; *
;
; * syntax: infogain(+Clause,+Clause_refs,-CL)
;
; *
;
; * args: Clause.. MeTTa clause that is refined
;
; * Clause_refs .. list of MeTTa clauses, refinements of Clause
;
; * CL.. list of clauses with their gain: CL = [C1:Gain1,..,Cn:Gainn]
;
; *
;
; * description: Clause is an overgeneral clause, CL is the
;
; * list of refinements of this overgeneral clause,
;
; * CL = [C1,...,Cn]. infogain returns a list
;
; * CL = [C1:Gain_C1,..], where
;
; * Gain is the information gain of Ci in comparison
;
; * with the overgeneral clause
;
; *
;
; *************************************************************************



  (= (infogain $Clause $Ref_list $CL)  
    (store-clause $Clause $_ gain $ID)
    (eval-examples)
    (get-evaluation $ID 
      (evaluation $_ $Tip $_ $Tim $_ $_ $_ $_ $_))
    (delete-clause $ID)
    (is $OTi 
      (/ $Tip 
        (+ $Tip $Tim)))
    (log2 $OTi $LNOTi)
    (is $ITi 
      (- $LNOTi))
    (infogain1 $Ref_list $CL $ITi))


  (= (infogain1  () () $_)  True)
  (= (infogain1 (Cons  $C $R) $R2 $ITi)  
    (infogain1 $R $R1 $ITi)
    (store-clause $C $_ gain $ID)
    (eval-examples)
    (get-evaluation $ID 
      (evaluation $_ $Ti1p $_ $Ti1m $_ $_ $_ $_ $_))
    (delete-clause $ID)
    (det-if-then-else 
      (= $Ti1p 0) 
      (, 
        (copy-term $C $C1) 
        (numbervars $C1 0 $_) 
        (write 'refuted: ') 
        (write $C1) 
        (nl) 
        (nl) 
        (= $R2 $R1)) 
      (, 
        (is $OTi1 
          (/ $Ti1p 
            (+ $Ti1p $Ti1m))) 
        (log2 $OTi1 $LNOTi1) 
        (is $ITi1 
          (- $LNOTi1)) 
        (is $IG 
          (* $Ti1p 
            (- $ITi $ITi1))) 
        (copy-term $C $C1) 
        (numbervars $C1 0 $_) 
        (write 'refined clause: ') 
        (write $C1) 
        (write    ) 
        (write $IG) 
        (nl) 
        (nl) 
        (= $R2 
          (Cons  
            (with_self  $C $IG) $R1)))))


  (= (infogain $Ref_list $CL)  
    (get-clause $ID $_ $_ $_ hypo)
    (get-evaluation $ID 
      (evaluation $_ $Tip $_ $Tim $_ $_ $_ $_ $_))
    (delete-clause $ID)
    (is $OTi 
      (/ $Tip 
        (+ $Tip $Tim)))
    (log2 $OTi $LNOTi)
    (is $ITi 
      (- $LNOTi))
    (infogain2 $Ref_list $CL $ITi))



  (= (infogain2  () () $_)  True)
  (= (infogain2 (Cons  $C $R) $R2 $ITi)  
    (infogain2 $R $R1 $ITi)
    (store-clause $C $_ gain $ID)
    (eval-examples)
    (get-evaluation $ID 
      (evaluation $_ $Ti1p $_ $Ti1m $_ $_ $_ $_ $_))
    (encoding-length $Ti1p $X)
    (det-if-then-else 
      (= $C 
        (= $H  $B)) 
      (, 
        (body2list $B $BL) 
        (encoding-length-clause 
          (Cons  
            (with_self  $H 
              (p)) $BL) $XE)) 
      (encoding-length-clause 
        (:: (with_self  $C (p))) $XE))
    (delete-clause $ID)
    (det-if-then-else 
      (or 
        (= $Ti1p 0) 
        (> $XE $X)) 
      (, 
        (copy-term $C $C1) 
        (numbervars $C1 0 $_) 
        (write 'refuted: ') 
        (write $C1) 
        (nl) 
        (nl) 
        (= $R2 $R1)) 
      (, 
        (is $OTi1 
          (/ $Ti1p 
            (+ $Ti1p $Ti1m))) 
        (log2 $OTi1 $LNOTi1) 
        (is $ITi1 
          (- $LNOTi1)) 
        (is $IG 
          (* $Ti1p 
            (- $ITi $ITi1))) 
        (copy-term $C $C1) 
        (numbervars $C1 0 $_) 
        (write 'refined clause: ') 
        (write $C1) 
        (write    ) 
        (write $IG) 
        (nl) 
        (nl) 
        (= $R2 
          (Cons  
            (with_self  $C $IG) $R1)))))


  (= (encoding-length $PN $X)  
    (total-ex $U)
    (log2 $U $LU)
    (is $U1 
      (float $U))
    (is $PN1 
      (float $PN))
    (log2nueberk $U1 $PN1 $Y)
    (is $X 
      (+ $LU $Y)))

