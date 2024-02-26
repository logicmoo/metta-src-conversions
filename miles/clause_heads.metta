;
; 



  !(module clause-heads 
    (:: 
      (/ clause-heads 0) 
      (/ heads 1) 
      (/ heads 2) 
      (/ heads 3)))
; 


;
; 

  !(use-module 
    (home div-utils) 
    (:: 
      (/ mysetof 3) 
      (/ different-predicates 2) 
      (/ make-unique 2) 
      (/ variant-mem 2) 
      (/ split-examples 4) 
      (/ insert-unique 3) 
      (/ remove-v 3)))
; 

  !(use-module 
    (home var-utils) 
    (:: (/ terms 4)))
; 

  !(use-module 
    (home interpreter) 
    (:: (/ proof-path 4)))
; 

  !(use-module 
    (home kb) 
    (:: 
      (/ get-example 3) 
      (/ get-clause 5) 
      (/ store-clauses 2) 
      (/ delete-clause 1)))
; 

  !(use-module 
    (home argument-types) 
    (:: (/ type-restriction 2)))
; 

  !(use-module 
    (home lgg) 
    (:: (/ set-lgg 2)))
; 

  !(use-module 
    (home evaluation) 
    (:: (/ eval-examples 0)))
; 

  !(use-module-if-exists 
    (library subsumes) 
    (:: (/ subsumes-chk 2)))
; 

  !(use-module-if-exists 
    (library basics) 
    (:: (/ member 2)))
; 

  !(use-module-if-exists 
    (library lists) 
    (:: (/ rev 2)))
; 


;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (clause-heads) 
    ( (mysetof $E 
        (^ $I 
          (get-example $I $E +)) $Elist) 
      (different-predicates $Elist $Elist1) 
      (clause-heads $Elist1)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (heads $HL) 
    ( (clause-heads) (mysetof $Head (^ $ID (^ $Body (^ $CL (, (get-clause $ID $Head $Body $CL head) (delete-clause $ID))))) $HL)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (heads $P $N) 
    ( (functor $E $P $N) 
      (mysetof $E 
        (^ $I 
          (get-example $I $E +)) $Elist) 
      (clause-heads (:: $Elist))))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (heads $P $N $HL) 
    ( (functor $E $P $N) 
      (mysetof $E 
        (^ $I 
          (get-example $I $E +)) $Elist) 
      (clause-heads (:: $Elist)) 
      (functor $Head $P $N) 
      (mysetof $Head 
        (^ $ID 
          (^ $Body 
            (^ $CL 
              (, 
                (get-clause $ID $Head $Body $CL head) 
                (delete-clause $ID))))) $HL)))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (clause_heads  ()) True)
; 

  (= 
    (clause-heads (Cons  $EL $R)) 
    ( (clause-heads $R) 
      (clause-heads $EL $Heads) 
      (make-unique $Heads $Heads1) 
      (minimize-heads $Heads1 $EL $Heads2) 
      (store-clauses $Heads2 head) 
      (eval-examples)))
; 

   

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (clause-heads 
      (Cons  $E $R) $Heads) 
    ( (functor $E $P $N) 
      (functor $P1 $P $N) 
      (det-if-then-else 
        (type-restriction $P1 $Types) True 
        (, 
          (=.. $P1 
            (Cons  $_ $P1A)) 
          (trivial-tr $P1A $Types))) 
      (bases $N 
        (Cons  $E $R) $P1 $Types $B) 
      (remove-base-examples $B 
        (Cons  $E $R) $E1) 
      (split-examples-by-types $E1 $P1 $Types $Hlist) 
      (make-unique $Hlist $Hlist1) 
      (best-lgg $Hlist1 
        (Cons  $E $R) $B $Heads)))
; 



  (= 
    (trivial_tr  () ()) True)
; 

  (= 
    (trivial-tr 
      (Cons  $X $R) 
      (Cons  $T $R1)) 
    ( (trivial-tr $R $R1) (=.. $T (:: all $X))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (bases  0 $_ $_ $_ ()) True)
; 

  (= 
    (bases $N $E $P $Type $B) 
    ( (is $N1 
        (- $N 1)) 
      (bases $N1 $E $P $Type $B1) 
      (copy-term 
        (, $P $Type) 
        (, $P1 $Type1)) 
      (arg $N $P1 $P1n) 
      (member $T $Type1) 
      (=.. $T 
        (:: $_ $X)) 
      (== $X $P1n) 
      (mysetof $Base 
        (^ $I 
          (^ $CL 
            (^ $R 
              (, 
                (get-clause $I $T True $CL type) 
                (=.. $T 
                  (:: $R $Base)))))) $Bases) 
      (bases1 $Bases $N $E $B1 $B)))
; 



  (= 
    (bases1  () $_ $_ $B $B) True)
; 

  (= 
    (bases1 
      (Cons  $B $R) $N $E $B1 
      (Cons  $H $B2)) 
    ( (bases1 $R $N $E $B1 $B2) 
      (bases2 $E $N $B $Eb) 
      (set-lgg $Eb $H)))
; 



  (= 
    (bases2  () $_ $_ ()) True)
; 

  (= 
    (bases2 
      (Cons  $E $R) $N $B 
      (Cons  $E $R1)) 
    ( (arg $N $E $B) 
      (set-det) 
      (bases2 $R $N $B $R1)))
; 

  (= 
    (bases2 
      (Cons  $_ $R) $N $B $R1) 
    (bases2 $R $N $B $R1))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (split-examples-by-types $E $P $Types $Heads) 
    ( (split-examples-by-types $Types $P $E Nil $Elist) (construct-heads $Elist $Heads)))
; 


  (= 
    (split_examples_by_types  () $_ $_ $EL $EL) True)
; 

  (= 
    (split-examples-by-types 
      (Cons  $T $R) $P $E $EL $EL3) 
    ( (split-examples-by-types $R $P $E $EL $EL1) 
      (mysetof 
        (, $Ex $Ts) 
        (, 
          (member $Ex $E) 
          (proof-path $Ex $P $T $Ts)) $Elist0) 
      (split-example-list $Elist0 $EL2) 
      (append $EL1 $EL2 $EL3)))
; 




  (= 
    (split_example_list  () ()) True)
; 

  (= 
    (split-example-list 
      (Cons  
        (, $E $Ts) $R) 
      (Cons  
        (Cons  $E $EL) $R1)) 
    ( (split-elist $R $Ts $EL $R0) (split-example-list $R0 $R1)))
; 



  (= 
    (split_elist  () $_ () ()) True)
; 

  (= 
    (split-elist 
      (Cons  
        (, $E $Ts) $R) $Ts 
      (Cons  $E $R1) $R2) 
    (split-elist $R $Ts $R1 $R2))
; 

  (= 
    (split-elist 
      (Cons  $E $R) $Ts $R1 
      (Cons  $E $R2)) 
    (split-elist $R $Ts $R1 $R2))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (construct-heads 
      (Cons  $EL $R) 
      (Cons  
        (with_self  $H $Vars) $R1)) 
    ( (set-lgg $EL $H) 
      (functor $H $_ $N) 
      (terms $N $H Nil $Vars) 
      (construct-heads $R $R1)))
; 

  (= 
    (construct_heads  () ()) True)
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (best_lgg  () $_ $HL $HL) True)
; 

  (= 
    (best-lgg 
      (Cons  
        (with_self  $H $Vars) $R) $E $HL $HL1) 
    (det-if-then-else 
      (variant-mem $H $HL) 
      (best-lgg $R $E $HL $HL1) 
      (, 
        (try-to-unify $H $Vars $Vars $E Nil $Lp) 
        (append $Lp $R $R1) 
        (best-lgg $R1 $E 
          (Cons  $H $HL) $HL1))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (try_to_unify  $_ () $_ $_ $L $L) True)
; 

  (= 
    (try-to-unify $H 
      (Cons  $X $R) $V $E $L $L2) 
    ( (unify-vars $H $X $V $E $L $L1) (try-to-unify $H $R $V $E $L1 $L2)))
; 



  (= 
    (unify_vars  $_ $_ () $_ $L $L) True)
; 

  (= 
    (unify-vars $H $X 
      (Cons  $Y $R) $E $L $L2) 
    ( (copy-term 
        (, $H $X $Y) 
        (, $H1 $X1 $Y1)) 
      (unify-var $H1 $X1 $Y1 $E $L $L1) 
      (unify-vars $H $X $R $E $L1 $L2)))
; 



  (= 
    (unify-var $_ $X $Y $_ $L $L) 
    ( (== $X $Y) (set-det)))
; 

  (= 
    (unify-var $H $X $X $E $L0 $L1) 
    ( (set-det) 
      (split-examples $E $H $Pos $_) 
      (det-if-then-else 
        (\== $Pos Nil) 
        (, 
          (set-lgg $Pos $H1) 
          (functor $H1 $_ $N) 
          (terms $N $H1 Nil $Vars1) 
          (= $L1 
            (Cons  
              (with_self  $H1 $Vars1) $L0))) 
        (= $L1 $L0))))
; 

  (= 
    (unify_var  $_ $_ $_ $_ $L $L) True)
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (remove-base-examples $B 
      (Cons  $E $R) $R1) 
    ( (is-base-example $E $B) 
      (set-det) 
      (remove-base-examples $B $R $R1)))
; 

  (= 
    (remove-base-examples $B 
      (Cons  $E $R) 
      (Cons  $E $R1)) 
    (remove-base-examples $B $R $R1))
; 

  (= 
    (remove_base_examples  $_ () ()) True)
; 




  (= 
    (is-base-example $E 
      (Cons  $B $_)) 
    ( (subsumes-chk $B $E) (set-det)))
; 

  (= 
    (is-base-example $E 
      (Cons  $_ $R)) 
    (is-base-example $E $R))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (minimize-heads $H $EL $H4) 
    ( (sort-heads-theta $H $H1) 
      (remove-redundant $H1 $H1 $EL $H2) 
      (rev $H2 $H3) 
      (remove-redundant $H3 $H3 $EL $H4)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (sort_heads_theta  () ()) True)
; 

  (= 
    (sort-heads-theta 
      (Cons  $H $R) $L) 
    ( (sort-heads-theta $R $L1) (insert-heads-theta $L1 $H $L)))
; 



  (= 
    (insert-heads-theta 
      (Cons  $H1 $R) $H 
      (Cons  $H1 $R1)) 
    ( (subsumes-chk $H1 $H) 
      (set-det) 
      (insert-heads-theta $R $H $R1)))
; 

  (= 
    (insert_heads_theta  $L $H 
      (Cons  $H $L)) True)
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (remove-redundant 
      (Cons  $H $R) $HL $EL $HL1) 
    ( (remove-v 
        (:: $H) $HL $HL0) (det-if-then-else (heads-cover $HL0 $EL) (remove-redundant $R $HL0 $EL $HL1) (remove-redundant $R $HL $EL $HL1))))
; 

  (= 
    (remove_redundant  () $HL $_ $HL) True)
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (heads_cover  $_ ()) True)
; 

  (= 
    (heads-cover 
      (Cons  $H $R) $E) 
    ( (split-examples $E $H $_ $E1) (heads-cover $R $E1)))
; 


