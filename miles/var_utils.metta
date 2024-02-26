;
; 


  !(module var-utils 
    (:: 
      (/ inverse-substitute 2) 
      (/ inverse-substitute1 2) 
      (/ skolems 2) 
      (/ skolemize 3) 
      (/ skolemize 4) 
      (/ deskolemize 3) 
      (/ relevant-vars2 6) 
      (/ relevant-vars3 6) 
      (/ buildrelterms 6) 
      (/ contains-vars 2) 
      (/ flagged-contains-vars 3) 
      (/ vars 2) 
      (/ term-size 2) 
      (/ replace 4) 
      (/ inv-replace 4) 
      (/ terms 3) 
      (/ terms 4) 
      (/ only-vars 2) 
      (/ clause-terms 2) 
      (/ only-vars1 2) 
      (/ typed-only-vars1 2) 
      (/ exists-intersect 3) 
      (/ clean-subst 3) 
      (/ findargs 3) 
      (/ allarg 4)))
; 


;
; 

  !(use-module 
    (home div-utils) 
    (:: 
      (/ effaceall 3) 
      (/ genterm-test 2) 
      (/ identical-member 2) 
      (/ mysetof 3) 
      (/ clist-to-prolog 2) 
      (/ best 2) 
      (/ subterm-at-position 4) 
      (/ part-of-clause 2)))
; 

  !(use-module 
    (home flatten) 
    (:: 
      (/ flatten-clause 2) 
      (/ unflatten-clause 2)))
; 

  !(use-module 
    (home filter) 
    (:: (/ truncate-unconnected 2)))
; 

  !(use-module 
    (home lgg) 
    (:: (/ lgg-terms 7)))
; 
 
  !(use-module-if-exists 
    (library basics) 
    (:: 
      (/ member 2) 
      (/ nonmember 2) 
      (/ memberchk 2)))
; 

  !(use-module-if-exists 
    (library sets) 
    (:: 
      (/ union 3) 
      (/ subtract 3) 
      (/ list-to-set 2) 
      (/ intersection 3)))
; 

  !(use-module-if-exists 
    (library strings) 
    (:: 
      (/ gensym 2) 
      (/ string-append 3) 
      (/ substring 4)))
; 

  !(use-module-if-exists 
    (library occurs) 
    (:: 
      (/ sub-term 2) 
      (/ contains-var 2)))
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

;
; 

;
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (vars $Term $Vars) 
    (mysetof $V 
      (, 
        (sub-term $V $Term) 
        (var $V)) $Vars))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (clause-terms 
      (= $H $B) $L) 
    ( (set-det) 
      (functor $H $_ $N) 
      (terms $N $H Nil $L0) 
      (clause-terms $B $L0 $L)))
; 

  (= 
    (clause-terms $H $L) 
    ( (functor $H $_ $N) (terms $N $H Nil $L)))
; 
   

  (= 
    (clause-terms 
      (, $A $B) $L $L2) 
    ( (set-det) 
      (clause-terms $A $L $L1) 
      (clause-terms $B $L1 $L2)))
; 

  (= 
    (clause-terms $A $L $L1) 
    ( (functor $A $_ $N) (terms $N $A $L $L1)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (terms $V $L $L1) 
    ( (var $V) 
      (set-det) 
      (det-if-then-else 
        (identical-member $V $L) 
        (= $L1 $L) 
        (= $L1 
          (Cons  $V $L)))))
; 

  (= 
    (terms $T $L $L1) 
    ( (functor $T $_ $N) (det-if-then-else (or (ground $T) (identical-member $T $L)) (= $L1 $L) (terms $N $T (Cons  $T $L) $L1))))
; 


  (= 
    (terms 0 $_ $L $L) 
    (set-det))
; 

  (= 
    (terms $N $T $L $L2) 
    ( (is $N1 
        (- $N 1)) 
      (terms $N1 $T $L $L1) 
      (arg-quintus $N $T $Tn) 
      (terms $Tn $L1 $L2)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (only-vars $T $L) 
    ( (terms $T Nil $L1) (only-vars1 $L1 $L)))
; 



  (= 
    (only_vars1  () ()) True)
; 

  (= 
    (only-vars1 
      (Cons  $X $R) 
      (Cons  $X $R1)) 
    ( (var $X) 
      (set-det) 
      (only-vars1 $R $R1)))
; 

  (= 
    (only-vars1 
      (Cons  $_ $R) $R1) 
    (only-vars1 $R $R1))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (typed_only_vars1  () ()) True)
; 

  (= 
    (typed-only-vars1 
      (Cons  
        (with_self  $X $T) $R) 
      (Cons  
        (with_self  $X $T) $R1)) 
    ( (var $X) 
      (set-det) 
      (typed-only-vars1 $R $R1)))
; 

  (= 
    (typed-only-vars1 
      (Cons  $_ $R) $R1) 
    (typed-only-vars1 $R $R1))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (replace $C $S1 $D $S2) 
    ( (copy-term 
        (, $C $S1) 
        (, $E $S2)) (do-replace $E $S2 $D)))
; 




  (= 
    (do_replace  () $_ ()) True)
; 


  (= 
    (do-replace 
      (Cons  $L $More) $S 
      (Cons  $L1 $More1)) 
    ( (do-replace1 $L $S $L1) 
      (set-det) 
      (do-replace $More $S $More1)))
; 



  (= 
    (do-replace1 $T1 $S $T2) 
    (det-if-then-else 
      (, 
        (member 
          (/ $X $T) $S) 
        (== $X $T1)) 
      (= $T2 $T) 
      (det-if-then-else 
        (var $T1) 
        (= $T1 $T2) 
        (det-if-then 
          (functor $T1 $F $N) 
          (, 
            (functor $T2 $F $N) 
            (do-replace1 $N $T1 $T2 $S))))))
; 


  (= 
    (do_replace1  0 $_ $_ $_) True)
; 


  (= 
    (do-replace1 $N $T1 $T2 $S) 
    ( (arg-quintus $N $T1 $A) 
      (arg-quintus $N $T2 $B) 
      (do-replace1 $A $S $B) 
      (is $M 
        (- $N 1)) 
      (do-replace1 $M $T1 $T2 $S)))
; 



  (= 
    (arg-quintus $N $C $E) 
    ( (compound $C) (arg $N $C $E)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (inv-replace $C $S1 $D $S2) 
    ( (copy-term 
        (, $C $S1) 
        (, $E $S2)) (do-inv-replace $E $S2 $D)))
; 




  (= 
    (do_inv_replace  () $_ ()) True)
; 


  (= 
    (do-inv-replace 
      (Cons  $L $More) $S 
      (Cons  $L1 $More1)) 
    ( (do-inv-replace1 $L $S $L1) (do-inv-replace $More $S $More1)))
; 



  (= 
    (do-inv-replace1 $T1 $S $T2) 
    (det-if-then-else 
      (var $T1) 
      (= $T1 $T2) 
      (det-if-then-else 
        (, 
          (member 
            (/ $X $T) $S) 
          (== $T $T1)) 
        (= $T2 $X) 
        (det-if-then 
          (functor $T1 $F $N) 
          (, 
            (functor $T2 $F $N) 
            (do-inv-replace1 $N $T1 $T2 $S))))))
; 


  (= 
    (do_inv_replace1  0 $_ $_ $_) True)
; 


  (= 
    (do-inv-replace1 $N $T1 $T2 $S) 
    ( (arg-quintus $N $T1 $A) 
      (arg-quintus $N $T2 $B) 
      (do-inv-replace1 $A $S $B) 
      (is $M 
        (- $N 1)) 
      (do-inv-replace1 $M $T1 $T2 $S)))
; 



  (= 
    (do-inv-replace1 2 
      (:: $T) $T2 $S) 
    ( (arg-quintus 2 $T2 Nil) (do-inv-replace1 1 (:: $T) $T2 $S)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (term-size $Term $Size) 
    (det-if-then-else 
      (var $Term) 
      (= $Size 0) 
      (, 
        (functor $Term $F $Arity) 
        (term-size $Arity $Term 1 $Size))))
; 



  (= 
    (term-size $N $NonVar $SoFar $Size) 
    (det-if-then-else 
      (=:= $N 0) 
      (is $Size $SoFar) 
      (, 
        (arg-quintus $N $NonVar $Arg) 
        (term-size $Arg $ArgSize) 
        (is $Accum 
          (+ $SoFar $ArgSize)) 
        (is $M 
          (- $N 1)) 
        (term-size $M $NonVar $Accum $Size))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (contains_vars  () $_) True)
; 

  (= 
    (contains-vars 
      (Cons  $V $Vars) $Term) 
    ( (contains-var $V $Term) (contains-vars $Vars $Term)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (flagged-contains-vars $Vars $Term True) 
    ( (contains-vars $Vars $Term) (set-det)))
; 

  (= 
    (flagged_contains_vars  $Vars $Term false) True)
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (inverse_substitute  $Clause $Clause) True)
; 
       ;
; 


  (= 
    (inverse-substitute $ClauseIn $ClauseOut) 
    ( (flatten-clause $ClauseIn $C1) 
      (remove-type-literal $C1 $C2) 
      (truncate-unconnected $C2 $C3) 
      (unflatten-clause $C3 $ClauseOut)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (remove-type-literal 
      (Cons  
        (with_self  $L_p 
          (n)) $More) $More) 
    ( (functor $L_p $F $_) (string-append $_ -p $F)))
; 



  
  (= 
    (remove-type-literal 
      (Cons  
        (with_self  $L $S) $More) 
      (Cons  
        (with_self  $L $S) $More1)) 
    (remove-type-literal $More $More1))
; 
 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (inverse-substitute1 $CLin $CLout) 
    ( (copy-term $CLin $CLin1) 
      (clist-to-prolog $CLin1 $Clause) 
      (mysetof 
        (with_self  $Sub $Pos) 
        (, 
          (subterm-at-position $Clause $Sub Nil $Pos) 
          (not (part-of-clause $Sub $Clause)) 
          (nonvar $Sub)) $Sublist) 
      (isub1-list $Sublist $Sublist1) 
      (best $Sublist1 
        (with_self  $T $Positions)) 
      (do-inverse-sub1 $T $Positions $_ $Clause $Clause1) 
      (clist-to-prolog $CLout $Clause1) 
      (not (variant $CLout $CLin))))
; 




  (= 
    (isub1_list  () ()) True)
; 

  (= 
    (isub1-list 
      (Cons  
        (with_self  $T $Pos) $R) 
      (Cons  
        (with_self  $T 
          (Cons  $Pos $Pos1)) $R2)) 
    ( (isub1-l $T $R $R1 $Pos1) (isub1-list $R1 $R2)))
; 



  (= 
    (isub1_l  $_ () () ()) True)
; 

  (= 
    (isub1-l $T 
      (Cons  
        (with_self  $T1 $Pos) $R) $R2 $Pos1) 
    ( (isub1-l $T $R $R1 $Pos0) (det-if-then-else (== $T $T1) (, (= $R2 $R1) (= $Pos1 (Cons  $Pos $Pos0))) (, (= $R2 (Cons  (with_self  $T1 $Pos) $R1)) (= $Pos1 $Pos0)))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (do_inverse_sub1  $_ () $_ $Clause $Clause) True)
; 

  (= 
    (do-inverse-sub1 $T 
      (Cons  $P $R) $Var $Clause $Clause2) 
    ( (do-inverse-sub1 $T $R $Var $Clause $Clause1) (do-isub1 $T $P $Var $Clause1 $Clause2)))
; 

  (= 
    (do-inverse-sub1 $T 
      (Cons  $_ $R) $Var $Clause $Clause1) 
    (do-inverse-sub1 $T $R $Var $Clause $Clause1))
; 



  (= 
    (do_isub1  $_ () $Var $_ $Var) True)
; 

  (= 
    (do-isub1 $T 
      (Cons  $P $R) $V $C $C1) 
    ( (functor $C $F $N) 
      (functor $C1 $F $N) 
      (do-isub-copy $N $P $C $C1) 
      (arg-quintus $P $C1 $C1p) 
      (arg-quintus $P $C $Cp) 
      (do-isub1 $T $R $V $Cp $C1p)))
; 



  (= 
    (do-isub-copy 0 $_ $_ $_) 
    (set-det))
; 

  (= 
    (do-isub-copy $N $P $C $C1) 
    ( (is $N1 
        (- $N 1)) 
      (do-isub-copy $N1 $P $C $C1) 
      (det-if-then-else 
        (== $N $P) True 
        (, 
          (arg-quintus $N $C $Cn) 
          (arg-quintus $N $C1 $Cn)))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (skolemize $T1 $S $T2) 
    (skolemize $T1 Nil $S $T2))
; 


  (= 
    (skolemize $T1 $S $S $Sk_Atom) 
    ( (var $T1) 
      (already-skolem-covered $T1 $S $Sk_Atom) 
      (set-det)))
; 

  (= 
    (skolemize $Var $S 
      (Cons  
        (/ $Var $Sk_Atom) $S) $Sk_Atom) 
    ( (var $Var) 
      (set-det) 
      (gensym sk-symbol $Sk_Atom)))
; 

  (= 
    (skolemize $T1 $S1 $S2 $T2) 
    ( (functor $T1 $F $N) 
      (functor $T2 $F $N) 
      (skolemize $N $T1 $S1 $S2 $T2)))
; 

  (= 
    (skolemize  0 $_ $S $S $_) True)
; 

  (= 
    (skolemize $N $T $S1 $S2 $U) 
    ( (arg-quintus $N $T $Tn) 
      (arg-quintus $N $U $Un) 
      (skolemize $Tn $S1 $S3 $Un) 
      (is $M 
        (- $N 1)) 
      (skolemize $M $T $S3 $S2 $U)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (already-skolem-covered $Var 
      (Cons  
        (/ $Var1 $Sk_Atom) $_) $Sk_Atom) 
    ( (== $Var $Var1) (set-det)))
; 


  (= 
    (already-skolem-covered $Var 
      (Cons  $_ $S) $Sk_Atom) 
    (already-skolem-covered $Var $S $Sk_Atom))
; 
 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (deskolemize $Sk_Atom $S $Var) 
    ( (atom $Sk_Atom) 
      (skolem-covered $Sk_Atom $S $Var) 
      (set-det)))
; 

  (= 
    (deskolemize $Atom $_ $Atom) 
    ( (atomic $Atom) (set-det)))
; 

  (= 
    (deskolemize $Var $S $Var) 
    ( (var $Var) (set-det)))
; 

  (= 
    (deskolemize $T1 $S $T2) 
    ( (functor $T1 $F $N) 
      (functor $T2 $F $N) 
      (deskolemize $N $T1 $S $T2)))
; 

  (= 
    (deskolemize  0 $_ $_ $_) True)
; 

  (= 
    (deskolemize $N $T $S $U) 
    ( (arg-quintus $N $T $Tn) 
      (arg-quintus $N $U $Un) 
      (deskolemize $Tn $S $Un) 
      (is $M 
        (- $N 1)) 
      (deskolemize $M $T $S $U)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (skolem-covered $Sk_Atom 
      (Cons  
        (/ $Var $Sk_Atom) $_) $Var) 
    (set-det))
; 

  (= 
    (skolem-covered $Sk_Atom 
      (Cons  $_ $S) $Var) 
    (skolem-covered $Sk_Atom $S $Var))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (skolems $Term $Skolems) 
    ( (setof $Skolem 
        (^ $Len 
          (, 
            (sub-term $Skolem $Term) 
            (atom $Skolem) 
            (atom-concat sk-symbol $Rest $Skolem) 
            (atom-length $Rest $Len))) $Skolems) (set-det)))
; 

  (= 
    (skolems  $_ ()) True)
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (relevant-vars2 $_ $_ $C $S1 $S2 $RelVars) 
    ( (vars $C $AllVars) (relevant-vars2 $AllVars $S1 $S2 $RelVars)))
; 



  (= 
    (relevant_vars2  () $_ $_ ()) True)
; 


  (= 
    (relevant-vars2 
      (Cons  $V $MoreVars) $S1 $S2 
      (Cons  $V $RelVars)) 
    ( (or 
        (, 
          (member 
            (/ $W $T1) $S1) 
          (== $V $W) 
          (member 
            (/ $X $T1a) $S1) 
          (\== $V $X) 
          (sub-term $Subterm1 $T1) 
          (var $Subterm1) 
          (contains-var $Subterm1 $T1a)) 
        (, 
          (member 
            (/ $W $T2) $S2) 
          (== $V $W) 
          (member 
            (/ $Y $T2a) $S2) 
          (\== $V $Y) 
          (sub-term $Subterm2 $T2) 
          (var $Subterm2) 
          (contains-var $Subterm2 $T2a))) 
      (set-det) 
      (relevant-vars2 $MoreVars $S1 $S2 $RelVars)))
; 


  (= 
    (relevant-vars2 
      (Cons  $V $MoreVars) $S1 $S2 $RelVars) 
    (relevant-vars2 $MoreVars $S1 $S2 $RelVars))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (relevant-vars3 $C1 $C2 $Gen $S1 $S2 $Vars) 
    ( (skolemize 
        (, $C1 $C2 $Gen $S1 $S2) $Phi 
        (, $D1 $D2 $Gen1 $R1 $R2)) 
      (relevant-vars3a $D1 $Gen1 $R1 $Vars1) 
      (relevant-vars3a $D2 $Gen1 $R2 $Vars2) 
      (length $Vars1 $Len1) 
      (length $Vars2 $Len2) 
      (== $Len1 $Len2) 
      (union $Vars1 $Vars2 $Vars0) 
      (deskolemize $Vars0 $Phi $Vars)))
; 



  (= 
    (relevant-vars3a $Spec $Gen $S $Skolems) 
    ( (replace $Gen $S $Gen1 $S) 
      (subtract $Spec $Gen1 $Rest) 
      (inv-replace $Rest $S $Rest1 $S) 
      (skolems $Rest1 $Skolems1) 
      (skolems $Gen $Skolems2) 
      (intersection $Skolems1 $Skolems2 $Skolems)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (findargs Nil $Result $Result) 
    (set-det))
; 

  (= 
    (findargs 
      (Cons  
        (with_self  $Lit1 $_) $Rest) $Accu $Result) 
    ( (functor $Lit1 $_ $N) 
      (allarg $N $Lit1 Nil $Args) 
      (union $Accu $Args $Newaccu) 
      (findargs $Rest $Newaccu $Result)))
; 



  (= 
    (allarg 0 $_ $Accu $Accu) 
    (set-det))
; 

  (= 
    (allarg $N $Lit $Args $Result) 
    ( (arg-quintus $N $Lit $Arg1) 
      (is $M 
        (- $N 1)) 
      (det-if-then-else 
        (nonmember $Arg1 $Args) 
        (allarg $M $Lit 
          (Cons  $Arg1 $Args) $Result) 
        (allarg $M $Lit $Args $Result))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (buildrelterms $SpecC1 $SpecC2 $Gen $S1 $S2 $Terms) 
    ( (skolemize 
        (, $Gen $SpecC1 $SpecC2 $S1 $S2) $SS 
        (, $Gen1 $Spec1 $Spec2 $SS1 $SS2)) 
      (findterms $Gen1 $Spec1 $SS1 $Terms1) 
      (findterms $Gen1 $Spec2 $SS2 $Terms2) 
      (deskolemize 
        (, $Terms1 $Terms2) $SS 
        (, $T1 $T2)) 
      (general-terms $T1 $T2 $Terms $S1 $S2)))
; 




  (= 
    (findterms $Gen $Spec $SS1 $RArgsG) 
    ( (replace $Gen $SS1 $Gen2 $_) 
      (subtract $Spec $Gen2 $RestSpec) 
      (subtract $Spec $RestSpec $SpecG) 
      (findargs $SpecG Nil $ArgsG) 
      (findargs $RestSpec Nil $ArgsR) 
      (exists-intersect $ArgsG $ArgsR $RArgsG)))
; 
 ;
; 

;
; 

	
;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (general-terms Nil Nil Nil $_ $_) 
    (set-det))
; 


  (= 
    (general-terms 
      (Cons  $T1 $R1) Nil 
      (Cons  $T $R3) $S1 $_) 
    ( (det-if-then-else 
        (genterm-test 
          (/ $T $T1) $S1) True 
        (inv-replace $T1 $S1 $T $_)) 
      (set-det) 
      (general-terms $R1 Nil $R3 $S1 $_)))
; 


  (= 
    (general-terms Nil 
      (Cons  $T2 $R2) 
      (Cons  $T $R3) $_ $S2) 
    ( (det-if-then-else 
        (genterm-test 
          (/ $T $T2) $S2) True 
        (inv-replace $T2 $S2 $T $_)) 
      (set-det) 
      (general-terms Nil $R2 $R3 $_ $S2)))
; 


  (= 
    (general-terms 
      (Cons  $T1 $R1) $L2 
      (Cons  $T $R3) $S1 $S2) 
    ( (gen-term $T1 $L2 $L2Rest $T $S1 $S2) 
      (set-det) 
      (general-terms $R1 $L2Rest $R3 $S1 $S2)))
; 



  (= 
    (gen-term $T1 $L2 $L2new $T $S1 $S2) 
    ( (nonvar $T1) 
      (functor $T1 $F $N) 
      (effaceall $T2 $L2 $L2new) 
      (functor $T2 $F $N) 
      (lgg-terms $T1 $T2 $T $_ $_ $S1 $S2)))
; 


  (= 
    (gen-term $T1 $L2 $L2new $X $S1 $S2) 
    ( (effaceall $T2 $L2 $L2new) 
      (genterm-test 
        (/ $X $T1) $S1) 
      (genterm-test 
        (/ $Y $T2) $S2) 
      (== $X $Y)))
; 


  (= 
    (gen-term $T1 $L2 $L2 $T $S1 $_) 
    (det-if-then-else 
      (genterm-test 
        (/ $T $T1) $S1) True 
      (inv-replace $T1 $S1 $T $_)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (exists-intersect $X $Y $Z) 
    ( (exi $X $Y $Z $_) (set-det)))
; 


  (= 
    (exi Nil $_ Nil $Flag) 
    (== $Flag yes))
; 

  (= 
    (exi Nil $_ Nil $_) 
    ( (set-det) (fail)))
; 

  (= 
    (exi 
      (Cons  $X $R) $Y 
      (Cons  $X $Z) yes) 
    ( (memberchk $X $Y) 
      (set-det) 
      (exi $R $Y $Z yes)))
; 

  (= 
    (exi 
      (Cons  $_ $R) $Y $Z $Flag) 
    (exi $R $Y $Z $Flag))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (clean_subst  $_ () ()) True)
; 

  (= 
    (clean-subst $CL 
      (Cons  
        (/ $X $T) $R) $R2) 
    ( (clean-subst $CL $R $R1) (det-if-then-else (contains-var $X $CL) (= $R2 (Cons  (/ $X $T) $R1)) (= $R2 $R1))))
; 

