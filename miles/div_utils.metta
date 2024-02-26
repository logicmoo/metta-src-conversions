;
; 



  !(module div-utils 
    (:: 
      (/ remove 3) 
      (/ sort-by-length 3) 
      (/ mysetof 3) 
      (/ sum 2) 
      (/ efface 3) 
      (/ buildpar2 3) 
      (/ neg 2) 
      (/ contains-duplicates 1) 
      (/ contains-identicals 1) 
      (/ identical-member 2) 
      (/ convert-to-horn-clause 3) 
      (/ extract-body 2) 
      (/ list-to-struct 2) 
      (/ clist-to-prolog 2) 
      (/ append-all 2) 
      (/ maximum 2) 
      (/ myforall 2) 
      (/ identical-make-unique 2) 
      (/ remove-v 3) 
      (/ remove-variant 3) 
      (/ make-unique 2) 
      (/ variant-mem 2) 
      (/ different-predicates 2) 
      (/ nth-arg 3) 
      (/ split-examples 4) 
      (/ shares-var 2) 
      (/ body2list 2) 
      (/ insert-unique 3) 
      (/ insert-unique 4) 
      (/ effaceall 3) 
      (/ genterm-test 2) 
      (/ subset-chk 2) 
      (/ best 2) 
      (/ subterm-at-position 4) 
      (/ part-of-clause 2) 
      (/ fak 2) 
      (/ fak1 3) 
      (/ nueberk 3) 
      (/ log2 2) 
      (/ log2nueberk 3) 
      (/ sum-of-logs 3)))
; 



;
; 

  !(meta-predicate (mysetof + : -))
; 



;
; 


  !(use-module-if-exists 
    (library basics) 
    (:: (/ member 2)))
; 

  !(use-module-if-exists 
    (library subsumes) 
    (:: (/ variant 2)))
; 

  !(use-module-if-exists 
    (library occurs) 
    (:: 
      (/ sub-term 2) 
      (/ contains-var 2)))
; 

  !(use-module-if-exists 
    (library lists) 
    (:: (/ rev 2)))
; 

  !(use-module-if-exists 
    (library math) 
    (:: (/ log 2)))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (remove  $_ () ()) True)
; 

  (= 
    (remove $I 
      (Cons  $I $R) $R) 
    (set-det))
; 

  (= 
    (remove $I 
      (Cons  $J $R) 
      (Cons  $J $R1)) 
    (remove $I $R $R1))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (sort_by_length  () $L $L) True)
; 

  (= 
    (sort-by-length 
      (Cons  $IXS $R) $L $L2) 
    ( (insert-by-length $IXS $L $L1) (sort-by-length $R $L1 $L2)))
; 



  (= 
    (insert-by-length $X 
      (Cons  $Y $R) 
      (Cons  $Y $R1)) 
    ( (length $X $N) 
      (length $Y $N1) 
      (> $N $N1) 
      (set-det) 
      (insert-by-length $X $R $R1)))
; 

  (= 
    (insert_by_length  $X $L 
      (Cons  $X $L)) True)
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (mysetof $A $B $C) 
    ( (setof $A $B $C) (set-det)))
; 

  (= 
    (mysetof  $_ $_ ()) True)
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (sum  
      ($I) $I) True)
; 

  (= 
    (sum 
      (Cons  $I $More) $C) 
    ( (sum $More $J) (is $C (+ $I $J))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (efface $A 
      (Cons  $A $L) $L) 
    (set-det))
; 

  (= 
    (efface $A 
      (Cons  $B $L) 
      (Cons  $B $M)) 
    (efface $A $L $M))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (effaceall  $A 
      (Cons  $A $L) $L) True)
; 

  (= 
    (effaceall $A 
      (Cons  $B $L) 
      (Cons  $B $M)) 
    (effaceall $A $L $M))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (best  
      (Cons  $X $_) $X) True)
; 

  (= 
    (best 
      (Cons  $_ $R) $X) 
    (best $R $X))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (buildpar2  
      (:  $Elem1 p) $List2 
      (Cons  
        (:  $Elem1 p) $List2)) True)
; 

  (= 
    (buildpar2 $ResLit $List2 $Parent2) 
    (append $List2 
      (:: $ResLit) $Parent2))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (neg  
      (:  $F p) 
      (:  $F n)) True)
; 

  (= 
    (neg  
      (:  $F n) 
      (:  $F p)) True)
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (contains-duplicates (Cons  $H $T)) 
    (member $H $T))
; 

  (= 
    (contains-duplicates (Cons  $_ $T)) 
    (contains-duplicates $T))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (contains-identicals (Cons  $H $T)) 
    (contains-var $H $T))
; 

  (= 
    (contains-identicals (Cons  $_ $T)) 
    (contains-identicals $T))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (identical-member $A 
      (Cons  $A1 $_)) 
    (== $A $A1))
; 
    
  (= 
    (identical-member $A 
      (Cons  $_ $R)) 
    (identical-member $A $R))
; 
 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (convert-to-horn-clause $PrefHead $GenClause $HornClause) 
    ( (extract-body $GenClause $Body) 
      (set-det) 
      (det-if-then-else 
        (member 
          (with_self  $PrefHead 
            (p)) $GenClause) 
        (= $Head $PrefHead) 
        (member 
          (with_self  $Head 
            (p)) $GenClause)) 
      (= $HornClause 
        (Cons  
          (with_self  $Head 
            (p)) $Body))))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (extract_body  () ()) True)
; 

  (= 
    (extract-body 
      (Cons  
        (with_self  $L 
          (n)) $Rest) 
      (Cons  
        (with_self  $L 
          (n)) $Rest1)) 
    (extract-body $Rest $Rest1))
; 

  (= 
    (extract-body 
      (Cons  
        (with_self  $L 
          (r)) $Rest) 
      (Cons  
        (with_self  $L 
          (r)) $Rest1)) 
    (extract-body $Rest $Rest1))
; 

  (= 
    (extract-body 
      (Cons  
        (with_self  $_ 
          (p)) $Rest) $Rest1) 
    (extract-body $Rest $Rest1))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (list-to-struct 
      (Cons  $A 
        (Cons  $B $Rest)) 
      (, $A $Rest1)) 
    (list-to-struct 
      (Cons  $B $Rest) $Rest1))
; 

  (= 
    (list_to_struct  
      ($A) $A) True)
; 

  (= 
    (list_to_struct  () true) True)
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (clist-to-prolog 
      (Cons  
        (with_self  $A 
          (p)) 
        (Cons  $B $Rest)) 
      (= $A $Rest1)) 
    ( (set-det) (clist-to-prolog (Cons  $B $Rest) $Rest1)))
; 

  (= 
    (clist-to-prolog 
      (:: (with_self  $A (p))) 
      (= $A True)) 
    (set-det))
; 

  (= 
    (clist-to-prolog 
      (Cons  
        (with_self  $A 
          (n)) 
        (Cons  $B $Rest)) 
      (, $A $Rest1)) 
    ( (set-det) (clist-to-prolog (Cons  $B $Rest) $Rest1)))
; 

  (= 
    (clist-to-prolog 
      (:: (with_self  $A (n))) $A) 
    (set-det))
; 

  (= 
    (clist-to-prolog 
      (Cons  
        (with_self  $A 
          (r)) 
        (Cons  $B $Rest)) 
      (, $A $Rest1)) 
    (clist-to-prolog 
      (Cons  $B $Rest) $Rest1))
; 

  (= 
    (clist_to_prolog  
      ( (:  $A r)) $A) True)
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (append_all  () ()) True)
; 

  (= 
    (append-all 
      (Cons  $P $R) $R2) 
    ( (append-all $R $R1) (append $P $R1 $R2)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (maximum  
      ($I) $I) True)
; 


  (= 
    (maximum 
      (Cons  $I $Rest) $I) 
    ( (maximum $Rest $J) 
      (>= $I $J) 
      (set-det)))
; 


  (= 
    (maximum 
      (Cons  $_ $Rest) $J) 
    ( (maximum $Rest $J) (set-det)))
; 
 

									
;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (myforall  () $_) True)
; 

  (= 
    (myforall 
      (Cons  $E $R) $Pred) 
    ( (=.. $C 
        (:: $Pred $E)) 
      (call $C) 
      (myforall $R $Pred)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (identical_make_unique  () ()) True)
; 

  (= 
    (identical-make-unique 
      (Cons  $X $R) $R1) 
    ( (contains-var $X $R) 
      (set-det) 
      (identical-make-unique $R $R1)))
; 
   
  (= 
    (identical-make-unique 
      (Cons  $X $R) 
      (Cons  $X $R1)) 
    (identical-make-unique $R $R1))
; 
 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (remove_v  $_ () ()) True)
; 

  (= 
    (remove-v $T 
      (Cons  $T1 $R) $R1) 
    ( (identical-member $T1 $T) 
      (set-det) 
      (remove-v $T $R $R1)))
; 

  (= 
    (remove-v $T 
      (Cons  $T1 $R) 
      (Cons  $T1 $R1)) 
    (remove-v $T $R $R1))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (remove_variant  $_ () ()) True)
; 

  (= 
    (remove-variant $T 
      (Cons  $T1 $R) $R1) 
    ( (variant-mem $T1 $T) 
      (set-det) 
      (remove-variant $T $R $R1)))
; 

  (= 
    (remove-variant $T 
      (Cons  $T1 $R) 
      (Cons  $T1 $R1)) 
    (remove-variant $T $R $R1))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (make_unique  () ()) True)
; 

  (= 
    (make-unique 
      (Cons  $X $R) $R1) 
    ( (variant-mem $X $R) 
      (set-det) 
      (make-unique $R $R1)))
; 

  (= 
    (make-unique 
      (Cons  $X $R) 
      (Cons  $X $R1)) 
    (make-unique $R $R1))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (variant-mem $T 
      (Cons  $T1 $_)) 
    ( (variant $T $T1) (set-det)))
; 

  (= 
    (variant-mem $T 
      (Cons  $_ $R)) 
    (variant-mem $T $R))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (different_predicates  () ()) True)
; 

  (= 
    (different-predicates 
      (Cons  $E $R) 
      (Cons  
        (Cons  $E $Es) $R2)) 
    ( (functor $E $F $N) 
      (diff-predicates $R $R1 $Es $F $N) 
      (different-predicates $R1 $R2)))
; 



  (= 
    (diff_predicates  () () () $_ $_) True)
; 

  (= 
    (diff-predicates 
      (Cons  $E $R) $R2 $Es2 $_ 0) 
    ( (set-det) 
      (diff-predicates $R $R1 $Es1 $_ 0) 
      (det-if-then-else 
        (functor $E $_ 0) 
        (, 
          (= $R2 $R1) 
          (= $Es2 
            (Cons  $E $Es1))) 
        (, 
          (= $R2 
            (Cons  $E $R1)) 
          (= $Es2 $Es1)))))
; 

  (= 
    (diff-predicates 
      (Cons  $E $R) $R2 $Es2 $F $N) 
    ( (diff-predicates $R $R1 $Es1 $F $N) (det-if-then-else (functor $E $F $N) (, (= $R2 $R1) (= $Es2 (Cons  $E $Es1))) (, (= $R2 (Cons  $E $R1)) (= $Es2 $Es1)))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (nth_arg  () $_ ()) True)
; 

  (= 
    (nth-arg 
      (Cons  $F $R) $N 
      (Cons  $Argn $R1)) 
    ( (arg $N $F $Argn) (nth-arg $R $N $R1)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (split-examples 
      (Cons  $E1 $R) $Lgg $P 
      (Cons  $E1 $M1)) 
    ( (not (= $E1 $Lgg)) 
      (split-examples $R $Lgg $P $M1) 
      (set-det)))
; 


  (= 
    (split-examples 
      (Cons  $E1 $R) $Lgg 
      (Cons  $E1 $P) $M1) 
    ( (split-examples $R $Lgg $P $M1) (set-det)))
; 


  (= 
    (split-examples Nil $_ Nil Nil) 
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
    (shares-var $T $Ts) 
    ( (sub-term $V $T) 
      (var $V) 
      (contains-var $V $Ts)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (body2list $B 
      (Cons  
        (with_self  $L1 
          (n)) $RestL)) 
    ( (functor $B , 2) 
      (arg 1 $B $L1) 
      (arg 2 $B $RestB) 
      (body2list $RestB $RestL)))
; 

  (= 
    (body2list $B 
      (:: (with_self  $B (n)))) 
    (set-det))
; 

  (= 
    (body2list $B 
      (Cons  
        (with_self  $L1 
          (r)) $RestL)) 
    ( (functor $B , 2) 
      (arg 1 $B $L1) 
      (arg 2 $B $RestB) 
      (body2list $RestB $RestL)))
; 

  (= 
    (body2list $B 
      (:: (with_self  $B (r)))) 
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
    (insert-unique $I 
      (Cons  $I $R) 
      (Cons  $I $R)) 
    (set-det))
; 

  (= 
    (insert-unique $I 
      (Cons  $J $R) 
      (Cons  $J $R1)) 
    ( (> $I $J) 
      (set-det) 
      (insert-unique $I $R $R1)))
; 

  (= 
    (insert_unique  $I $L 
      (Cons  $I $L)) True)
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (insert-unique $I $A Nil 
      (:: (with_self  $I (:: $A)))) 
    (set-det))
; 

  (= 
    (insert-unique $I $A 
      (Cons  
        (with_self  $I $A1) $R) 
      (Cons  
        (with_self  $I 
          (Cons  $A $A1)) $R)) 
    (set-det))
; 

  (= 
    (insert-unique $I $A 
      (Cons  $J $R) 
      (Cons  $J $R1)) 
    (insert-unique $I $A $R $R1))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (genterm-test 
      (/ $X $T1) 
      (Cons  
        (/ $X $T2) $_)) 
    ( (== $T1 $T2) (set-det)))
; 

  (= 
    (genterm-test $S 
      (Cons  $_ $Rest)) 
    (genterm-test $S $Rest))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (subset-chk Nil $_) 
    (set-det))
; 

  (= 
    (subset-chk 
      (Cons  $Elem1 $Rest1) $List2) 
    ( (identical-member $Elem1 $List2) 
      (set-det) 
      (subset-chk $Rest1 $List2)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (subterm-at-position $T $T $P $P1) 
    (rev $P $P1))
; 

  (= 
    (subterm-at-position $T $S $P $P1) 
    ( (nonvar $T) 
      (functor $T $_ $N) 
      (> $N 0) 
      (subterm-at-position $N $T $S $P $P1)))
; 


  (= 
    (subterm-at-position $N $T $S $P $P1) 
    ( (> $N 0) 
      (arg $N $T $Tn) 
      (subterm-at-position $Tn $S 
        (Cons  $N $P) $P1)))
; 

  (= 
    (subterm-at-position $N $T $S $P $P1) 
    ( (> $N 0) 
      (is $N1 
        (- $N 1)) 
      (subterm-at-position $N1 $T $S $P $P1)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (part-of-clause $S $B) 
    (== $S $B))
; 

  (= 
    (part-of-clause $S 
      (= $H $B)) 
    ( (set-det) (det-if-then-else (or (== $S $H) (== $S $B)) True (part-of-clause $S $B))))
; 

  (= 
    (part-of-clause $S 
      (, $H $B)) 
    ( (set-det) (det-if-then-else (or (== $S $H) (== $S $B)) True (part-of-clause $S $B))))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (fak $X 1) 
    ( (=:= $X 0) (set-det)))
; 

  (= 
    (fak $N $NF) 
    ( (is $N1 
        (- $N 1)) 
      (fak $N1 $N1F) 
      (is $NF 
        (* $N1F $N))))
; 



  (= 
    (fak1 $N $N 1) 
    (set-det))
; 

  (= 
    (fak1 $A $B $C) 
    ( (is $A1 
        (+ $A 1)) 
      (fak1 $A1 $B $C1) 
      (is $C 
        (* $C1 $A1))))
; 



  (= 
    (nueberk $N $K $NUK) 
    ( (is $NK 
        (- $N $K)) 
      (fak1 $NK $N $NKF) 
      (fak $K $KF) 
      (is $NUK 
        (/ $NKF $KF))))
; 



  (= 
    (log2 $X $LX) 
    ( (log $X $LNX) 
      (log 2 $LN2) 
      (is $LX 
        (/ $LNX $LN2))))
; 



  (= 
    (log2nueberk $_ 0.0 0.0) 
    (set-det))
; 

  (= 
    (log2nueberk $N 1.0 $LN) 
    ( (log2 $N $LN) (set-det)))
; 

  (= 
    (log2nueberk $N $N 0.0) 
    (set-det))
; 

  (= 
    (log2nueberk $N $K $L) 
    ( (is $N1 
        (+ 
          (- $N $K) 1)) 
      (sum-of-logs $N1 $N $L1) 
      (sum-of-logs 1.0 $K $L2) 
      (is $L 
        (- $L1 $L2))))
; 



  (= 
    (sum-of-logs $O $O $LO) 
    ( (log2 $O $LO) (set-det)))
; 

  (= 
    (sum-of-logs $U $O $L) 
    ( (< $U $O) 
      (set-det) 
      (is $U1 
        (+ $U 1)) 
      (sum-of-logs $U1 $O $L1) 
      (log2 $U $LU) 
      (is $L 
        (+ $L1 $LU))))
; 

  (= 
    (sum-of-logs $U $O $_) 
    ( (> $U $O) 
      (set-det) 
      (fail)))
; 

