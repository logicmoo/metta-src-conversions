;
; 


  !(module argument-types 
    (:: 
      (/ argument-types 0) 
      (/ type-restriction 2) 
      (/ type-equal 2) 
      (/ type-equal 4) 
      (/ replace-t 4) 
      (/ type-sub 2) 
      (/ type-of 3) 
      (/ types-of 3) 
      (/ compare-types 3) 
      (/ define-type 0) 
      (/ verify-types 0)))
; 



;
; 

  !(use-module 
    (home kb) 
    (:: 
      (/ get-example 3) 
      (/ get-clause 5) 
      (/ store-clauses 2) 
      (/ store-clause 4) 
      (/ known 6) 
      (/ delete-clause 1)))
; 

  !(use-module 
    (home lgg) 
    (:: (/ set-lgg 2)))
; 

  !(use-module 
    (home div-utils) 
    (:: 
      (/ body2list 2) 
      (/ myforall 2) 
      (/ different-predicates 2) 
      (/ nth-arg 3) 
      (/ remove-v 3) 
      (/ make-unique 2) 
      (/ shares-var 2) 
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
    (home interpreter) 
    (:: (/ t-interpreter 2)))
; 

  !(use-module 
    (home show-utils) 
    (:: (/ show-kb-types 0)))
; 

  !(use-module-if-exists 
    (library subsumes) 
    (:: (/ variant 2)))
; 

  !(use-module-if-exists 
    (library occurs) 
    (:: (/ contains-var 2)))
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



  !(dynamic (/ type-restriction 2))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (argument-types) 
    ( (mysetof $E 
        (^ $I 
          (get-example $I $E +)) $Elist) 
      (different-predicates $Elist $Elist1) 
      (argument-types $Elist1) 
      (set-det)))
; 



  (= 
    (argument_types  ()) True)
; 

  (= 
    (argument-types (Cons  $E $R)) 
    ( (argument-types $R) (arg-types $E)))
; 



  (= 
    (arg-types (Cons  $E $R)) 
    ( (functor $E $P $N) 
      (functor $P1 $P $N) 
      (det-if-then-else 
        (type-restriction $P1 $_) True 
        (add-symbol  &self 
          (type_restriction  $P1 ()))) 
      (arg-types $N 
        (Cons  $E $R) $P $N)))
; 





;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (arg-types 0 $_ $_ $_) 
    (set-det))
; 

  (= 
    (arg-types $N $EL $P $M) 
    ( (is $N1 
        (- $N 1)) 
      (arg-types $N1 $EL $P $M) 
      (nth-arg $EL $N $S) 
      (gensym type $Type) 
      (arg-type $S Nil Nil $CL $Type) 
      (minimize-cl $CL $Type $Type1) 
      (adapt-type-restriction $M $P $N $Type1)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (arg-type $S $Ancestors $CL $CL2 $T) 
    ( (different-predicates $S $Slist) 
      (init-cl $Slist $T $CL0) 
      (append $CL $CL0 $CL1) 
      (refine-cl $Slist 
        (Cons  $T $Ancestors) $CL0 $CL1 $CL2)))
; 
 ;
; 



  (= 
    (init_cl  () $_ ()) True)
; 

  (= 
    (init-cl 
      (Cons  $EL $R) $T 
      (Cons  
        (= $T1 True) $R1)) 
    ( (init-cl $R $T $R1) 
      (set-lgg $EL $E) 
      (=.. $T1 
        (:: $T $E))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (refine_cl  () $_ $_ $CL $CL) True)
; 

  (= 
    (refine-cl 
      (Cons  $S $R) $A 
      (Cons  
        (= $Head $_) $R1) $CL $CL2) 
    ( (refine-cl $R $A $R1 $CL $CL1) 
      (arg 1 $Head $E) 
      (det-if-then-else 
        (var $E) 
        (test-var-instantiations $E $S $Head $A $CL1 $CL2) 
        (, 
          (functor $E $_ $N) 
          (ref-cl $N $E $S $Head $A $CL1 $CL2)))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (ref-cl 0 $_ $_ $_ $_ $CL $CL) 
    (set-det))
; 

  (= 
    (ref-cl $N $E $S $H $A $CL $CL2) 
    ( (is $N1 
        (- $N 1)) 
      (ref-cl $N1 $E $S $H $A $CL $CL1) 
      (arg $N $E $X) 
      (nth-arg $S $N $Sn) 
      (det-if-then-else 
        (var $X) 
        (test-var-instantiations $X $Sn $H $A $CL1 $CL2) 
        (, 
          (functor $X $_ $M) 
          (ref-cl $M $X $Sn $H $A $CL1 $CL2)))))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (test-var-instantiations $X $S $H $_ $CL $CL1) 
    ( (myforall $S is-symbol) 
      (set-det) 
      (=.. $Lit 
        (:: is-symbol $X)) 
      (add-literal $CL $H $Lit $CL1)))
; 


  (= 
    (test-var-instantiations $X $S $H $_ $CL $CL1) 
    ( (myforall $S number) 
      (set-det) 
      (=.. $Lit 
        (:: number $X)) 
      (add-literal $CL $H $Lit $CL1)))
; 


  (= 
    (test-var-instantiations $X $S $H $_ $CL $CL1) 
    ( (myforall $S symbolic) 
      (set-det) 
      (=.. $Lit 
        (:: symbolic $X)) 
      (add-literal $CL $H $Lit $CL1)))
; 


  (= 
    (test-var-instantiations $X $S $H $A $CL $CL1) 
    ( (test-ancestor $S $A $CL $APred) 
      (set-det) 
      (=.. $Lit 
        (:: $APred $X)) 
      (add-literal $CL $H $Lit $CL1)))
; 


  (= 
    (test-var-instantiations $X $S $H $A $CL $CL1) 
    ( (gensym type $T) 
      (=.. $Lit 
        (:: $T $X)) 
      (add-literal $CL $H $Lit $CL0) 
      (arg-type $S $A $CL0 $CL1 $T)))
; 




  (= 
    (test-ancestor $S 
      (Cons  $APred $_) $CL $APred) 
    ( (myforall-interpreted $S $APred $CL) (set-det)))
; 

  (= 
    (test-ancestor $S 
      (Cons  $_ $R) $CL $APred) 
    (test-ancestor $S $R $CL $APred))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (myforall_interpreted  () $_ $_) True)
; 

  (= 
    (myforall-interpreted 
      (Cons  $E $R) $Pred $CL) 
    ( (=.. $C 
        (:: $Pred $E)) 
      (t-interpreter $C $CL) 
      (myforall-interpreted $R $Pred $CL)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (add-literal 
      (Cons  
        (= $H $B) $R) $H1 $Lit 
      (Cons  
        (= $H 
          ($Lit $B)) $R)) 
    ( (== $H $H1) (set-det)))
; 

  (= 
    (add-literal 
      (Cons  $C $R) $H $Lit 
      (Cons  $C $R1)) 
    (add-literal $R $H $Lit $R1))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (adapt-type-restriction $M $P $N $T) 
    ( (functor $P1 $P $M) 
      (remove-symbol  &self 
        (type_restriction  $P1 $L)) 
      (arg $N $P1 $P1n) 
      (det-if-then-else 
        (, 
          (member $T1 $L) 
          (=.. $T1 
            (:: $T2 $X)) 
          (== $X $P1n)) 
        (, 
          (gensym type $Tnew) 
          (=.. $D 
            (:: $Tnew $P1n)) 
          (remove-v 
            (:: $T1) $L $L1) 
          (add-symbol  &self 
            (type_restriction  $P1 
              (Cons  $D $L1))) 
          (adapt-tr $Tnew $T $T2)) 
        (, 
          (=.. $D 
            (:: $T $P1n)) 
          (add-symbol  &self 
            (type_restriction  $P1 
              (Cons  $D $L)))))))
; 



  (= 
    (adapt-tr $Tnew $T1 $T2) 
    ( (functor $HT1 $T1 1) 
      (functor $HT2 $T2 1) 
      (mysetof 
        (= $HT1 $B1) 
        (^ $I 
          (^ $Clist 
            (get-clause $I $HT1 $B1 $Clist type))) $C1) 
      (mysetof 
        (= $HT2 $B2) 
        (^ $I 
          (^ $Clist 
            (get-clause $I $HT2 $B2 $Clist type))) $C2) 
      (append $C1 $C2 $C3) 
      (adapt-tr1 $C3 $Tnew $C4) 
      (make-unique $C4 $C5) 
      (store-clauses $C5 type)))
; 



  (= 
    (adapt_tr1  () $_ ()) True)
; 

  (= 
    (adapt-tr1 
      (Cons  
        (= $H $B) $R) $T 
      (Cons  
        (= $H1 $B) $R1)) 
    ( (adapt-tr1 $R $T $R1) 
      (=.. $H 
        (Cons  $_ $Arg)) 
      (=.. $H1 
        (Cons  $T $Arg))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (minimize-cl $CL $Type $Type1) 
    ( (mysetof 
        (= $H $B) 
        (^ $I 
          (^ $Clist 
            (get-clause $I $H $B $Clist type))) $Old_types) 
      (mysetof $T 
        (^ $H 
          (^ $B 
            (^ $R 
              (, 
                (member 
                  (= $H $B) $Old_types) 
                (=.. $H 
                  (Cons  $T $R)))))) $Oldt_names) 
      (mysetof $T 
        (^ $H 
          (^ $B 
            (^ $R 
              (, 
                (member 
                  (= $H $B) $CL) 
                (=.. $H 
                  (Cons  $T $R)))))) $Newt_names) 
      (append $Old_types $CL $Clauses) 
      (minimize-cl $Oldt_names $Newt_names $Clauses $CL $Type $Type1)))
; 


  (= 
    (minimize-cl Nil $Newt_names $Clauses $CL $Type $Type1) 
    (minim-cl $Newt_names $Clauses $CL $Type $Type1))
; 

  (= 
    (minimize-cl 
      (Cons  $T $R) $Newt_names $Clauses $CL $Type $Type2) 
    ( (mysetof $T1 
        (, 
          (member $T1 $Newt_names) 
          (type-equal $T $T1 
            (:: (with_self  $T $T1)) $Clauses)) $Tlist) 
      (replace-t $CL $Tlist $T $CL1) 
      (make-unique $CL1 $CL2) 
      (remove-v $Tlist $Newt_names $Newt_names1) 
      (det-if-then-else 
        (member $Type $Tlist) 
        (= $Type1 $T) 
        (= $Type1 $Type)) 
      (minimize-cl $R $Newt_names1 $Clauses $CL2 $Type1 $Type2)))
; 



  (= 
    (minim-cl Nil $_ $CL $Type $Type) 
    ( (min-cl $CL $CL1) (store-clauses $CL1 type)))
; 
 ;
; 

                            ;
; 

  (= 
    (minim-cl 
      (Cons  $T $R) $Clauses $CL $Type $Type2) 
    ( (mysetof $T1 
        (, 
          (member $T1 $R) 
          (type-equal $T $T1 
            (:: (with_self  $T $T1)) $Clauses)) $Tlist) 
      (replace-t $CL $Tlist $T $CL1) 
      (make-unique $CL1 $CL2) 
      (remove-v $Tlist $R $R1) 
      (det-if-then-else 
        (member $Type $Tlist) 
        (= $Type1 $T) 
        (= $Type1 $Type)) 
      (minim-cl $R1 $Clauses $CL2 $Type1 $Type2)))
; 



  (= 
    (min_cl  () ()) True)
; 

  (= 
    (min-cl 
      (Cons  
        (= $H $B) $R) 
      (Cons  
        (= $H $B1) $R1)) 
    ( (min-cl $R $R1) (min-cl1 $B $B1)))
; 



  (= 
    (min-cl1 $A $A) 
    (set-det))
; 

  (= 
    (min-cl1 True True) 
    (set-det))
; 

  (= 
    (min-cl1 
      (, $A $B) 
      (, $A $B1)) 
    (min-cl1 $B $B1))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (replace_t  () $_ $_ ()) True)
; 

  (= 
    (replace-t 
      (Cons  
        (= $H $_) $R) $Tlist $T $R1) 
    ( (=.. $H 
        (Cons  $T1 $_)) 
      (member $T1 $Tlist) 
      (set-det) 
      (replace-t $R $Tlist $T $R1)))
; 

  (= 
    (replace-t 
      (Cons  
        (= $H $B) $R) $Tlist $T 
      (Cons  
        (= $H $B1) $R1)) 
    ( (repl-t $B $Tlist $T $B1) (replace-t $R $Tlist $T $R1)))
; 



  (= 
    (repl-t 
      (, $A $B) $Tlist $T 
      (, $A1 $B1)) 
    ( (set-det) 
      (repl-t $A $Tlist $T $A1) 
      (repl-t $B $Tlist $T $B1)))
; 

  (= 
    (repl-t $A $Tlist $T $A1) 
    ( (=.. $A 
        (Cons  $T1 $R)) (det-if-then-else (member $T1 $Tlist) (=.. $A1 (Cons  $T $R)) (= $A1 $A))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (type-equal $T $T) 
    (set-det))
; 

  (= 
    (type-equal $T1 $T2) 
    ( (mysetof 
        (= $H1 $B1) 
        (^ $I 
          (^ $CL 
            (^ $R 
              (, 
                (get-clause $I $H1 $B1 $CL type) 
                (=.. $H1 
                  (Cons  $T1 $R)))))) $Clauses1) 
      (mysetof 
        (= $H2 $B2) 
        (^ $I 
          (^ $CL 
            (^ $R 
              (, 
                (get-clause $I $H2 $B2 $CL type) 
                (=.. $H2 
                  (Cons  $T2 $R)))))) $Clauses2) 
      (append $Clauses1 $Clauses2 $Clauses) 
      (type-equal $T1 $T2 
        (:: (with_self  $T1 $T2)) $Clauses)))
; 


  (= 
    (type-equal $T $T $_ $_) 
    (set-det))
; 

  (= 
    (type-equal $T1 $T2 $Ancestors $Clauses) 
    ( (mysetof 
        (= $H $B) 
        (^ $R 
          (, 
            (member 
              (= $H $B) $Clauses) 
            (=.. $H 
              (Cons  $T1 $R)))) $Clist1) 
      (mysetof 
        (= $H $B) 
        (^ $R 
          (, 
            (member 
              (= $H $B) $Clauses) 
            (=.. $H 
              (Cons  $T2 $R)))) $Clist2) 
      (compare-clauses $Clist1 $Clist2 $Clauses $Ancestors)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (compare_clauses  () () $_ $_) True)
; 

  (= 
    (compare-clauses 
      (Cons  
        (= $H1 $B1) $R) $CL2 $Clauses $Ancestors) 
    ( (find-variant-clause $CL2 $H1 $CL21 
        (= $H2 $B2)) 
      (arg 1 $H1 $E1) 
      (arg 1 $H2 $E2) 
      (comp-clauses $E1 $E2 $B1 $B2 $Clauses $Ancestors) 
      (compare-clauses $R $CL21 $Clauses $Ancestors)))
; 



  (= 
    (comp-clauses $E1 $E2 $B1 $B2 $C $A) 
    ( (var $E1) 
      (set-det) 
      (def-literal $E1 $B1 $L1) 
      (def-literal $E2 $B2 $L2) 
      (=.. $L1 
        (Cons  $T1 $_)) 
      (=.. $L2 
        (Cons  $T2 $_)) 
      (c-clauses $T1 $T2 $C $A)))
; 

  (= 
    (comp-clauses $E1 $E2 $B1 $B2 $C $A) 
    ( (functor $E1 $_ $N) (comp-clauses $N $E1 $E2 $B1 $B2 $C $A)))
; 

  (= 
    (comp-clauses 0 $_ $_ $_ $_ $_ $_) 
    (set-det))
; 

  (= 
    (comp-clauses $N $E1 $E2 $B1 $B2 $C $A) 
    ( (is $N1 
        (- $N 1)) 
      (comp-clauses $N1 $E1 $E2 $B1 $B2 $C $A) 
      (arg $N $E1 $E1n) 
      (arg $N $E2 $E2n) 
      (comp-clauses $E1n $E2n $B1 $B2 $C $A)))
; 



  (= 
    (c-clauses is-symbol $L $_ $_) 
    ( (set-det) (= $L is-symbol)))
; 

  (= 
    (c-clauses number $L $_ $_) 
    ( (set-det) (= $L number)))
; 

  (= 
    (c-clauses symbolic $L $_ $_) 
    ( (set-det) (= $L symbolic)))
; 

  (= 
    (c-clauses $_ $L2 $_ $_) 
    ( (or 
        (= $L2 is-symbol) 
        (or 
          (= $L2 number) 
          (= $L2 symbolic))) 
      (set-det) 
      (fail)))
; 

  (= 
    (c-clauses $T1 $T2 $C $A) 
    (det-if-then-else 
      (or 
        (member 
          (with_self  $T1 $T2) $A) 
        (member 
          (with_self  $T2 $T1) $A)) True 
      (type-equal $T1 $T2 
        (Cons  
          (with_self  $T1 $T2) $A) $C)))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (find-variant-clause 
      (Cons  
        (= $H2 $B2) $R) $H1 $R 
      (= $H2 $B2)) 
    ( (arg 1 $H1 $E1) 
      (arg 1 $H2 $E2) 
      (variant $E1 $E2)))
; 

  (= 
    (find-variant-clause 
      (Cons  $C $R) $H 
      (Cons  $C $R1) $C1) 
    (find-variant-clause $R $H $R1 $C1))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (def-literal $X 
      (, $A $B) $C) 
    ( (set-det) (det-if-then-else (contains-var $X $A) (= $C $A) (def-literal $X $B $C))))
; 

  (= 
    (def-literal $X $A $A) 
    ( (contains-var $X $A) (set-det)))
; 

  (= 
    (def_literal  $X $_ 
      (all  $X)) True)
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (type-sub $Gen 
      (= $H $B)) 
    (type-sub1 
      (:: (= $H $B)) $Gen Nil))
; 

  (= 
    (type-sub $Gen $Spec) 
    (type-sub $Gen $Spec 
      (:: (with_self  $Gen $Spec))))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (type-sub all $_ $_) 
    (set-det))
; 
 ;
; 

  (= 
    (type-sub $T1 all $_) 
    ( (set-det) (= $T1 all)))
; 
    
  (= 
    (type-sub $T $T1 $_) 
    ( (== $T $T1) (set-det)))
; 


  (= 
    (type-sub symbolic $T $_) 
    ( (set-det) (or (= $T is-symbol) (or (= $T symbolic) (or (= $T number) (, (functor $HT $T 1) (setof (, $HT $B) (^ $ID (^ $CL (get-clause $ID $HT $B $CL type))) $TL) (all-t-in $TL (:: is-symbol number symbolic))))))))
; 

  (= 
    (type-sub is-symbol $T $_) 
    ( (set-det) (or (= $T is-symbol) (, (functor $HT $T 1) (setof (, $HT $B) (^ $ID (^ $CL (get-clause $ID $HT $B $CL type))) $TL) (all-t-in $TL (:: is-symbol))))))
; 
 
  (= 
    (type-sub number $T $_) 
    ( (set-det) (or (= $T number) (, (functor $HT $T 1) (setof (, $HT $B) (^ $ID (^ $CL (get-clause $ID $HT $B $CL type))) $TL) (all-t-in $TL (:: number))))))
; 


  (= 
    (type-sub $T symbolic $_) 
    ( (set-det) (or (= $T symbolic) (or (= $T all) (, (functor $HT $T 1) (setof (, $HT $B) (^ $ID (^ $CL (get-clause $ID $HT $B $CL type))) $TL) (all-t-in $TL (:: symbolic)))))))
; 

  (= 
    (type-sub $T is-symbol $_) 
    ( (set-det) (or (= $T is-symbol) (or (= $T symbolic) (or (= $T all) (, (functor $HT $T 1) (setof (, $HT $B) (^ $ID (^ $CL (get-clause $ID $HT $B $CL type))) $TL) (all-t-in $TL (:: is-symbol symbolic))))))))
; 

  (= 
    (type-sub $T number $_) 
    ( (set-det) (or (= $T number) (or (= $T symbolic) (or (= $T all) (, (functor $HT $T 1) (setof (, $HT $B) (^ $ID (^ $CL (get-clause $ID $HT $B $CL type))) $TL) (all-t-in $TL (:: number symbolic))))))))
; 



  (= 
    (type-sub $TG $TS $A) 
    ( (functor $HTS $TS 1) 
      (mysetof 
        (= $HTS $BS) 
        (^ $ID 
          (^ $CL 
            (get-clause $ID $HTS $BS $CL type))) $CS) 
      (type-sub1 $CS $TG $A)))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (type_sub1  () $_ $_) True)
; 

  (= 
    (type-sub1 
      (Cons  
        (= $H $B) $R) 
      (= $HG $BG) $A) 
    ( (set-det) 
      (type-sub1 $R 
        (= $HG $BG) $A) 
      (set-det) 
      (=.. $H 
        (:: $_ $Es)) 
      (=.. $HG 
        (:: $_ $Es)) 
      (expand-to-type-def $BG $BG1) 
      (is-type-definition (= $H $B)) 
      (test-type-def $B) 
      (is-type-definition (= $HG $BG1)) 
      (test-type-def $BG1) 
      (only-vars $Es $EsV) 
      (type-sub2 $EsV $BG1 $B $A)))
; 

  (= 
    (type-sub1 
      (Cons  
        (= $H $B) $R) $TG $A) 
    ( (type-sub1 $R $TG $A) 
      (set-det) 
      (=.. $H 
        (:: $_ $Es)) 
      (=.. $HTG 
        (:: $TG $Es)) 
      (get-clause $_ $HTG $BG $_ type) 
      (expand-to-type-def $BG $BG1) 
      (is-type-definition (= $H $B)) 
      (test-type-def $B) 
      (is-type-definition (= $HTG $BG1)) 
      (test-type-def $BG1) 
      (only-vars $Es $EsV) 
      (type-sub2 $EsV $BG1 $B $A)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (type_sub2  () $_ $_ $_) True)
; 

  (= 
    (type-sub2 
      (Cons  $X $R) $BG1 $B $A) 
    ( (type-sub2 $R $BG1 $B $A) 
      (set-det) 
      (def-literal $X $BG1 $LG) 
      (def-literal $X $B $LS) 
      (=.. $LS 
        (Cons  $TS $_)) 
      (=.. $LG 
        (Cons  $TG $_)) 
      (det-if-then-else 
        (member 
          (with_self  $TG $TS) $A) True 
        (type-sub $TG $TS 
          (Cons  
            (with_self  $TG $TS) $A)))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (expand-to-type-def 
      (, $A $B) 
      (, $A $B1)) 
    ( (simple-td $A) (expand-to-type-def $B $B1)))
; 

  (= 
    (expand-to-type-def 
      (, $A $B) $B1) 
    ( (not (simple-td $A)) 
      (get-clause $_ $A $Lits $_ type) 
      (append-body $Lits $B $B0) 
      (expand-to-type-def $B0 $B1)))
; 

  (= 
    (expand-to-type-def $A $A) 
    (simple-td $A))
; 

  (= 
    (expand-to-type-def $A $A1) 
    ( (not (= $A (, $_ $_))) 
      (not (simple-td $A)) 
      (get-clause $_ $A $Lits $_ type) 
      (expand-to-type-def $Lits $A1)))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (simple-td True) 
    (set-det))
; 

  (= 
    (simple-td $A) 
    ( (=.. $A 
        (:: $_ $X)) (simple $X)))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (test-type-def (, $A $B)) 
    ( (set-det) 
      (test-type-def $A) 
      (test-type-def $B)))
; 

  (= 
    (test-type-def $A) 
    ( (=.. $A 
        (:: $T $X)) 
      (ground $X) 
      (or 
        (= $T is-symbol) 
        (or 
          (= $T symbolic) 
          (= $T number))) 
      (set-det) 
      (call $A)))
; 

  (= 
    (test_type_def  $_) True)
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (is-type-definition (= $H $B)) 
    ( (only-vars $H $Vars) (is-type-def $Vars $B)))
; 



  (= 
    (is_type_def  () $_) True)
; 

  (= 
    (is-type-def 
      (Cons  $X $R) $B) 
    ( (def-literal $X $B $Lit) 
      (set-det) 
      (=.. $Lit 
        (:: $_ $X1)) 
      (== $X $X1) 
      (is-type-def $R $B)))
; 



  (= 
    (all_t_in  () $_) True)
; 

  (= 
    (all-t-in 
      (Cons  
        (, $H $B) $R) $L) 
    ( (=.. $H 
        (:: $_ $X)) 
      (var $X) 
      (=.. $B 
        (:: $N $X)) 
      (member $N $L) 
      (all-t-in $R $L)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (type-of $V 
      (= $H $B) $Type) 
    ( (set-det) 
      (type-of $V $H $Type1) 
      (type-of $V $B $Type2) 
      (compare-types $Type1 $Type2 $Type)))
; 

  (= 
    (type-of $V 
      (, $A $B) $Type) 
    ( (set-det) 
      (type-of $V $A $Type1) 
      (type-of $V $B $Type2) 
      (compare-types $Type1 $Type2 $Type)))
; 

  (= 
    (type-of $T $Pred $Type) 
    (det-if-then-else 
      (type-restriction $Pred $Plist) 
      (, 
        (mysetof $Ts 
          (, 
            (member $Ts $Plist) 
            (contains-var $T $Ts)) $TsL) 
        (type-of1 $TsL $T $Type)) 
      (= $Type all)))
; 

  (= 
    (type_of  $_ true all) True)
; 



  (= 
    (type_of1  () $_ all) True)
; 

  (= 
    (type-of1 
      (Cons  $Ts $R) $T $Type) 
    ( (set-det) 
      (type-of1 $R $T $Type1) 
      (=.. $Ts 
        (:: $Type0 $T2)) 
      (det-if-then-else 
        (== $T2 $T) 
        (compare-types $Type1 $Type0 $Type) 
        (, 
          (get-clause $_ $Ts $_ 
            (Cons  $_ $CL) type) 
          (mysetof $Ts1 
            (, 
              (member 
                (with_self  $Ts1 $_) $CL) 
              (contains-var $T $Ts1)) $TsL) 
          (det-if-then-else 
            (= $TsL Nil) 
            (, 
              (mysetof $Ts2 
                (, 
                  (member $Ts2 $CL) 
                  (shares-var $T $Ts2)) $TsL1) 
              (=.. $H_int 
                (:: t-int $T)) 
              (with_self  
                (kb *) 
                (body2list $B_int $TsL1)) 
              (compare-types $Type1 
                (= $H_int $B_int) $Type)) 
            (, 
              (type-of1 $TsL $T $Type2) 
              (compare-types $Type1 $Type2 $Type)))))))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (types_of  () $_ ()) True)
; 

  (= 
    (types-of 
      (Cons  $V $R) $C 
      (Cons  
        (with_self  $V $T) $R1)) 
    ( (type-of $V $C $T) (types-of $R $C $R1)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (compare-types $Type1 $Type2 $Type) 
    (det-if-then-else 
      (type-sub $Type1 $Type2) 
      (= $Type $Type2) 
      (det-if-then-else 
        (type-sub $Type2 $Type1) 
        (= $Type $Type1) fail)))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (define-type) 
    ( (show-kb-types) (read-type-restriction)))
; 



  (= 
    (read-type-restriction) 
    ( (repeat) 
      (nl) 
      (write 'Please enter name and arity of the predicate p/n: ') 
      (det-if-then-else 
        (, 
          (read (/ $P $N)) 
          (atom $P) 
          (integer $N)) 
        (, 
          (functor $F $P $N) 
          (=.. $F 
            (Cons  $P $Args)) 
          (read-type-restriction $Args 1 $Alist) 
          (add-symbol  &self 
            (type_restriction  $F $Alist))) fail)))
; 


  (= 
    (read_type_restriction  () $_ ()) True)
; 

  (= 
    (read-type-restriction 
      (Cons  $V $R) $N 
      (Cons  $T $R1)) 
    ( (repeat) 
      (nl) 
      (write 'Please enter the type at argument position ') 
      (write $N) 
      (write  : ) 
      (det-if-then-else 
        (, 
          (read $TN) 
          (atom $TN)) 
        (det-if-then-else 
          (or 
            (, 
              (=.. $H 
                (:: $TN $_)) 
              (get-clause $_ $H $_ $_ type)) 
            (member $TN 
              (:: is-symbol number symbolic))) 
          (=.. $T 
            (:: $TN $V)) 
          (, 
            (read-type-definition $TN) 
            (=.. $T 
              (:: $TN $V)))) fail) 
      (is $N1 
        (+ $N 1)) 
      (read-type-restriction $R $N1 $R1)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (read-type-definition $TN) 
    ( (nl) 
      (write 'Type ') 
      (write $TN) 
      (write ' is undefined. Enter a definition (y/n)? ') 
      (read $A) 
      (det-if-then-else 
        (== $A y) 
        (read-type-def $TN) 
        (det-if-then-else 
          (== $A n) fail 
          (, 
            (nl) 
            (write 'Please enter y or n') 
            (read-type-definition $TN))))))
; 



  (= 
    (read-type-def $TN) 
    ( (nl) 
      (write 'Please enter the definition of ') 
      (write $TN) 
      (write ' in clausal form. Stop by entering stop.') 
      (nl) 
      (repeat) 
      (read $A) 
      (det-if-then-else 
        (or 
          (, 
            (= $A 
              (= $H $_)) 
            (=.. $H 
              (:: $_ $_))) 
          (=.. $A 
            (:: $_ $_))) 
        (, 
          (store-clause $A $_ type $_) 
          (nl) 
          (fail)) 
        (det-if-then-else 
          (== $A stop) True 
          (, 
            (nl) 
            (write 'Please enter a clause or stop') 
            (fail))))))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (verify-types) 
    ( (findall 
        (, $M $A) 
        (type-restriction $M $A) $TSet) (verify-types $TSet)))
; 


  (= 
    (verify_types  ()) True)
; 

  (= 
    (verify-types (Cons  (, $M $A) $R)) 
    ( (verify-types $R) (verify-types $A $M $A)))
; 


  (= 
    (verify_types  () $_ $_) True)
; 

  (= 
    (verify-types 
      (Cons  $H $R) $M $A) 
    ( (verify-types $R $M $A) 
      (=.. $H 
        (Cons  $T $_)) 
      (findall 
        (, $H1 $B1) 
        (, 
          (=.. $H1 
            (:: $T $_)) 
          (known $ID $H1 $B1 $CL $_ $E) 
          (delete-clause $ID) 
          (add-symbol  &self 
            (:  kb 
              (known  $ID $H1 $B1 $CL type $E)))) $Tlist) 
      (det-if-then-else 
        (or 
          (\== $Tlist Nil) 
          (member $T 
            (:: is-symbol number symbolic))) True 
        (, 
          (nl) 
          (write 'The type ') 
          (write $T) 
          (write ' is undefined in ') 
          (copy-term 
            (, $M $A) 
            (, $M1 $A1)) 
          (numbervars 
            (, $M1 $A1) 0 $_) 
          (write (type-restriction $M1 $A1)) 
          (nl) 
          (show-kb-types) 
          (repeat) 
          (nl) 
          (write 'Do you want to replace ') 
          (write $T) 
          (write ' in ') 
          (write (type-restriction $M1 $A1)) 
          (write ' (y/n)?') 
          (nl) 
          (read $An) 
          (det-if-then-else 
            (== $An y) 
            (, 
              (remove-symbol  &self 
                (type_restriction  $M $A)) 
              (repeat) 
              (nl) 
              (write 'Enter the name of the type replacing ') 
              (write $T) 
              (write : ) 
              (det-if-then-else 
                (, 
                  (read $T1) 
                  (atom $T1)) 
                (, 
                  (=.. $H1 
                    (:: $T1 $_)) 
                  (det-if-then-else 
                    (get-clause $_ $H1 $_ $_ $_) 
                    (, 
                      (vrt $A $T $T1 $A2) 
                      (add-symbol  &self 
                        (type_restriction  $M $A2))) 
                    (, 
                      (nl) 
                      (write 'The type ') 
                      (write $T1) 
                      (write ' is undefined.') 
                      (vrt1 $T1)))) fail)) 
            (det-if-then-else 
              (== $An n) 
              (, 
                (nl) 
                (write 'Then you have to define ') 
                (write $T) 
                (read-type-def $T)) 
              (, 
                (write 'Please enter y or n') 
                (fail))))))))
; 



  (= 
    (vrt  () $_ $_ ()) True)
; 

  (= 
    (vrt 
      (Cons  $H $R) $T $T1 
      (Cons  $H1 $R1)) 
    ( (vrt $R $T $T1 $R1) 
      (=.. $H 
        (:: $T2 $V)) 
      (det-if-then-else 
        (== $T2 $T) 
        (=.. $H1 
          (:: $T1 $V)) 
        (= $H1 $H))))
; 




  (= 
    (vrt1 $T1) 
    ( (nl) 
      (write 'Do you want to define it (y/n)? ') 
      (read $Bn) 
      (det-if-then-else 
        (== $Bn y) 
        (read-type-def $T1) 
        (det-if-then-else 
          (== $Bn n) fail 
          (, 
            (write 'Please enter y or n') 
            (vrt1 $T1))))))
; 

                            
                   

