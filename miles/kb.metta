;
; 


  !(module kb 
    (:: 
      (/ init-kb 1) 
      (/ init-kb 2) 
      (/ save-kb 1) 
      (/ consult-kb 1) 
      (/ gen-id 1) 
      (/ id-count 1) 
      (/ store-clause 4) 
      (/ store-clauses 2) 
      (/ store-clauses 3) 
      (/ store-ex 3) 
      (/ get-example 3) 
      (/ get-clause 5) 
      (/ get-fact 4) 
      (/ get-evaluation 2) 
      (/ clear-kb 0) 
      (/ delete-clause 1) 
      (/ delete-example 1) 
      (/ delete-all 1) 
      (/ random-ex 1) 
      (/ two-random-ex 2) 
      (/ two-random-uncovered-ex 2) 
      (/ two-random-ex-from-list 3) 
      (/ i-random-ex 2) 
      (/ shortest-clause 1) 
      (/ shortest-clause 2) 
      (/ two-shortest-clauses 2) 
      (/ two-shortest-clauses 3) 
      (/ shortest-ex 1) 
      (/ shortest-uncovered-ex 1) 
      (/ shortest-uncovered-ex 2) 
      (/ two-shortest-ex 2) 
      (/ two-shortest-uncovered-ex 2) 
      (/ all-shortest-ex 1) 
      (/ all-shortest-uncovered-ex 1) 
      (/ no-rules 0) 
      (/ no-pos-examples 0) 
      (/ no-neg-examples 0) 
      (/ no-examples 0) 
      (/ flatten-kb 0) 
      (/ flatten-rules 0) 
      (/ unflatten-kb 0) 
      (/ delete-covered-examples 0) 
      (/ get-predlist 1) 
      (/ rename 3) 
      (/ known 6) 
      (/ ex 3) 
      (/ assertallz 1) 
      (/ interpretable-predicate 1)))
; 


;
; 

  !(use-module 
    (home div-utils) 
    (:: 
      (/ body2list 2) 
      (/ mysetof 3)))
; 

  !(use-module 
    (home evaluation) 
    (:: 
      (/ eval-examples 0) 
      (/ complexity 2) 
      (/ evaluated 1) 
      (/ change-evaluated 1)))
; 

  !(use-module 
    (home argument-types) 
    (:: 
      (/ type-restriction 2) 
      (/ verify-types 0)))
; 

  !(use-module 
    (home flatten) 
    (:: 
      (/ flatten-clause 2) 
      (/ unflatten-clause 2)))
; 

  !(use-module 
    (home interpreter) 
    (:: (/ prooftrees 3)))
; 

  !(use-module-if-exists (library prompt))
; 

  !(use-module-if-exists 
    (library ask) 
    (:: (/ yesno 1)))
; 

  !(use-module-if-exists 
    (library basics) 
    (:: 
      (/ nonmember 2) 
      (/ member 2)))
; 

  !(use-module-if-exists 
    (library random) 
    (:: (/ random-select 3)))
; 


;
; 

;
; 



  !(dynamic (, (/ id-count 1) (/ ex 3) (/ known 6)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (gen-id $New) 
    ( (remove-symbol  &self 
        (id_count  $Old)) 
      (is $New 
        (+ $Old 1)) 
      (add-symbol  &self 
        (id_count  $New))))
; 

  (= 
    (gen-id 1) 
    (add-symbol  &self 
      (id_count  1)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (init-kb $Filename) 
    (init-kb $Filename usr))
; 


  (= 
    (init-kb $Filename $Origin) 
    ( (open $Filename read $S) 
      (repeat) 
      (read $S $Term) 
      (store-term $Term $Origin) 
      (close $S) 
      (set-det) 
      (eval-examples) 
      (verify-types)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (consult-kb $Filename) 
    ( (clear-kb) $Filename))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (save-kb $Filename) 
    (save-predicates 
      (:: 
        (/ known 6) 
        (/ ex 3) 
        (/ prooftrees 3) 
        (/ id-count 1) 
        (/ type-restriction 2) 
        (/ evaluated 1)) $Filename))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (clear-kb) 
    ( (remove-all-symbols  &self 
        (known  $_ $_ $_ $_ $_ $_)) 
      (remove-all-symbols  &self 
        (ex  $_ $_ $_)) 
      (remove-all-symbols  &self 
        (prooftrees  $_ $_ $_)) 
      (remove-all-symbols  &self 
        (id_count  $_)) 
      (remove-all-symbols  &self 
        (type_restriction  $_ $_)) 
      (remove-all-symbols  &self 
        (evaluated  $_))))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (store-term end-of-file $_) 
    (set-det))
; 

  (= 
    (store-term 
      (ex $F $C) $_) 
    ( (gen-id $ID) 
      (add-symbol  &self 
        (ex  $ID $F $C)) 
      (set-det) 
      (fail)))
; 

  (= 
    (store-term 
      (= $H $B) $O) 
    ( (body2list $B $L) 
      (gen-id $ID) 
      (add-symbol  &self 
        (known  $ID $H $B 
          (Cons  
            (:  $H p) $L) $O 
          (evaluation  0 0 () 0 () 0 () 0 ()))) 
      (set-det) 
      (fail)))
; 

  (= 
    (store-term 
      (type-restriction $M $A) $_) 
    ( (add-symbol  &self 
        (:  argument_types 
          (type_restriction  $M $A))) 
      (set-det) 
      (fail)))
; 

  (= 
    (store-term $H $O) 
    ( (gen-id $ID) 
      (add-symbol  &self 
        (known  $ID $H true 
          ( (:  $H p)) $O 
          (evaluation  0 0 () 0 () 0 () 0 ()))) 
      (set-det) 
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
    (store-clause $A $B $_ $ID) 
    (det-if-then-else 
      (, 
        (nonvar $ID) 
        (or 
          (known $ID $_ $_ $_ $_ $_) 
          (ex $ID $_ $_))) 
      (, 
        (set-det) 
        (fail)) 
      (det-if-then-else 
        (, 
          (var $A) 
          (var $B)) 
        (, 
          (set-det) 
          (fail)) fail)))
; 

  (= 
    (store-clause 
      (= $H $B) 
      (Cons  
        (with_self  $H 
          (p)) $L) $Label $ID) 
    ( (body2list $B $L) 
      (det-if-then-else 
        (var $ID) 
        (gen-id $ID) 
        (, 
          (id-count $Top) 
          (=< $ID $Top))) 
      (det-if-then-else 
        (var $Label) 
        (= $Label usr) True) 
      (add-symbol  &self 
        (known  $ID $H $B 
          (Cons  
            (:  $H p) $L) $Label 
          (evaluation  0 0 () 0 () 0 () 0 ()))) 
      (set-det) 
      (change-evaluated no)))
; 

  (= 
    (store-clause $H 
      (:: (with_self  $H (p))) $Label $ID) 
    ( (det-if-then-else 
        (var $ID) 
        (gen-id $ID) 
        (, 
          (id-count $Top) 
          (=< $ID $Top))) 
      (det-if-then-else 
        (var $Label) 
        (= $Label usr) True) 
      (add-symbol  &self 
        (known  $ID $H true 
          ( (:  $H p)) $Label 
          (evaluation  0 0 () 0 () 0 () 0 ()))) 
      (set-det) 
      (change-evaluated no)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (store_clauses  () $_) True)
; 

  (= 
    (store-clauses 
      (Cons  $C $R) $Label) 
    ( (store-clause $C $_ $Label $_) (store-clauses $R $Label)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (store_clauses  () $_ ()) True)
; 

  (= 
    (store-clauses 
      (Cons  $C $R) $Label 
      (Cons  $ID $R1)) 
    ( (store-clause $C $_ $Label $ID) (store-clauses $R $Label $R1)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (store-ex $F $Class $ID) 
    ( (ex $ID1 $F1 $Class1) 
      (== $F $F1) 
      (set-det) 
      (= $Class $Class1) 
      (= $ID $ID1)))
; 

  (= 
    (store-ex $F $_ $ID) 
    (det-if-then-else 
      (, 
        (nonvar $ID) 
        (or 
          (ex $ID $_ $_) 
          (known $ID $_ $_ $_ $_ $_))) 
      (, 
        (set-det) 
        (fail)) 
      (det-if-then-else 
        (var $F) 
        (, 
          (set-det) 
          (fail)) fail)))
; 

  (= 
    (store-ex $Fact $Class $ID) 
    ( (det-if-then-else 
        (var $ID) 
        (gen-id $ID) 
        (, 
          (id-count $Top) 
          (=< $ID $Top))) 
      (add-symbol  &self 
        (ex  $ID $Fact $Class)) 
      (set-det) 
      (change-evaluated no)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (get-example $ID $F $C) 
    (ex $ID $F $C))
; 


  (= 
    (get-clause $ID $H $B $L $O) 
    (known $ID $H $B $L $O $_))
; 
        

  (= 
    (get-fact $ID $F $L $O) 
    (known $ID $F True $L $O $_))
; 


  (= 
    (get-evaluation $ID $Eval) 
    (known $ID $_ $_ $_ $_ $Eval))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (delete-clause $ID) 
    ( (remove-symbol  &self 
        (known  $ID $_ $_ $_ $_ $_)) (change-evaluated no)))
; 


  (= 
    (delete-example $ID) 
    ( (remove-symbol  &self 
        (ex  $ID $_ $_)) (change-evaluated no)))
; 




  (= 
    (delete-all Nil) 
    (set-det))
; 
 
  (= 
    (delete-all (Cons  $Id1 $Rest)) 
    ( (delete-clause $Id1) 
      (set-det) 
      (delete-all $Rest)))
; 

  (= 
    (delete-all (Cons  $Id1 $Rest)) 
    ( (delete-example $Id1) 
      (set-det) 
      (delete-all $Rest)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (interpretable-predicate $A) 
    ( (functor $A $F $N) 
      (functor $A1 $F $N) 
      (or 
        (get-clause $_ $A1 $_ $_ $_) 
        (get-example $_ $A1 $_))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (assertallz  ()) True)
; 

  (= 
    (assertallz (Cons  $X $R)) 
    ( (add-symbol  &self $X) (assertallz $R)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (rename Nil $_ $_) 
    (set-det))
; 

  (= 
    (rename 
      (Cons  $Id1 $Rest) $Old $New) 
    ( (get-clause $Id1 $_ $_ $Clist $Label) 
      (rename-clause $Clist $NewClause $Old $New) 
      (delete-clause $Id1) 
      (store-clause $_ $NewClause $Label $Id1) 
      (set-det) 
      (rename $Rest $Old $New)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (rename-clause Nil Nil $_ $_) 
    (set-det))
; 

  (= 
    (rename-clause 
      (Cons  
        (with_self  $Lit $X) $Rest) 
      (Cons  
        (with_self  $NewLit $X) $NewRest) $Old $New) 
    ( (det-if-then-else 
        (=.. $Lit 
          (Cons  $Old $Args)) 
        (=.. $NewLit 
          (Cons  $New $Args)) 
        (= $NewLit $Lit)) 
      (set-det) 
      (rename-clause $Rest $NewRest $Old $New)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (random-ex $ID1) 
    ( (findall $ID 
        (get-example $ID $_ +) $Bag) (random-select $ID1 $Bag $_)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (two-random-ex $ID1 $ID2) 
    ( (findall $ID 
        (get-example $ID $_ +) $Bag) 
      (or 
        (random-select $ID1 $Bag $Residue) 
        (select $ID1 $Bag $Residue)) 
      (or 
        (random-select $ID2 $Residue $_) 
        (select $ID2 $Residue $_))))
; 

        ;
; 



  (= 
    (two-random-ex-from-list $List $ID1 $ID2) 
    ( (random-select $ID1 $List $Residue) (random-select $ID2 $Residue $_)))
; 



  (= 
    (two-random-uncovered-ex $ID1 $ID2) 
    ( (findall $ID 
        (, 
          (with_self  
            (kb *) 
            (prooftrees $ID fail $_)) 
          (get-example $ID $_ +)) $Uncovered) (two-random-ex-from-list $Uncovered $ID1 $ID2)))
; 
        


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (i-random-ex $I $Examples) 
    ( (> $I 0) 
      (findall $ID 
        (get-example $ID $_ +) $Bag) 
      (length $Bag $J) 
      (det-if-then-else 
        (=< $J $I) 
        (= $Examples $Bag) 
        (i-random-ex $I $Bag $Examples))))
; 

  (= 
    (i-random-ex 0 $_ Nil) 
    (set-det))
; 

  (= 
    (i-random-ex $N $Bag 
      (Cons  $ID $Rest)) 
    ( (random-select $ID $Bag $Residue) 
      (is $M 
        (- $N 1)) 
      (i-random-ex $M $Residue $Rest)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (shortest-clause (with_self  $ID1 $C1)) 
    (shortest-clause $_ 
      (with_self  $ID1 $C1)))
; 
 
        
  (= 
    (shortest-clause $Label 
      (with_self  $ID1 $C1)) 
    ( (findall 
        (with_self  $ID $C) 
        (, 
          (get-clause $ID $_ $_ $Clause $Label) 
          (complexity $Clause $C)) $Bag) (shortest $Bag (with_self  $ID1 $C1) $_)))
; 

       

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (two-shortest-clauses 
      (with_self  $ID1 $C1) 
      (with_self  $ID2 $C2)) 
    (two-shortest-clauses $_ 
      (with_self  $ID1 $C1) 
      (with_self  $ID2 $C2)))
; 


  (= 
    (two-shortest-clauses $Label 
      (with_self  $ID1 $C1) 
      (with_self  $ID2 $C2)) 
    ( (findall 
        (with_self  $ID $C) 
        (, 
          (get-clause $ID $_ $_ $Clause $Label) 
          (complexity $Clause $C)) $Bag) (two-shortest $Bag (with_self  $ID1 $C1) (with_self  $ID2 $C2))))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (shortest-ex (with_self  $ID1 $C1)) 
    ( (findall 
        (with_self  $ID $C) 
        (, 
          (get-example $ID $Ex +) 
          (complexity $Ex $C)) $Bag) (shortest $Bag (with_self  $ID1 $C1) $_)))
; 

       

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (two-shortest-ex 
      (with_self  $ID1 $C1) 
      (with_self  $ID2 $C2)) 
    ( (findall 
        (with_self  $ID $C) 
        (, 
          (get-example $ID $Ex +) 
          (complexity $Ex $C)) $Bag) (two-shortest $Bag (with_self  $ID1 $C1) (with_self  $ID2 $C2))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (shortest-uncovered-ex $ID1) 
    ( (findall 
        (with_self  $ID $C) 
        (, 
          (with_self  
            (kb *) 
            (prooftrees $ID fail $_)) 
          (get-example $ID $Ex +) 
          (complexity $Ex $C)) $Uncovered) (shortest $Uncovered (with_self  $ID1 $_) $Residue)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (shortest-uncovered-ex $Uncovered $ID1) 
    ( (add-complexities $Uncovered $Bag) (shortest $Bag (with_self  $ID1 $_) $Residue)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (two-shortest-uncovered-ex $ID1 $ID2) 
    ( (findall 
        (with_self  $ID $C) 
        (, 
          (with_self  
            (kb *) 
            (prooftrees $ID fail $_)) 
          (get-example $ID $Ex +) 
          (complexity $Ex $C)) $Uncovered) (two-shortest $Uncovered (with_self  $ID1 $_) (with_self  $ID2 $_))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (all-shortest-ex $Bag) 
    ( (shortest-ex (with_self  $_ $C1)) (findall $ID (, (get-example $ID $Ex +) (complexity $Ex $C1)) $Bag)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (all-shortest-uncovered-ex $Bag) 
    ( (findall 
        (with_self  $ID $C) 
        (, 
          (with_self  
            (kb *) 
            (prooftrees $ID fail $_)) 
          (get-example $ID $Ex +) 
          (complexity $Ex $C)) $Uncovered) 
      (shortest $Uncovered 
        (with_self  $_ $C1) $_) 
      (findall $ID2 
        (member 
          (with_self  $ID2 $C1) $Uncovered) $Bag)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (two-shortest $Bag 
      (with_self  $ID1 $C1) 
      (with_self  $ID2 $C2)) 
    ( (shortest $Bag 
        (with_self  $ID1 $C1) $Residue) 
      (shortest $Residue 
        (with_self  $ID2 $C2) $_) 
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
    (shortest  
      ( (:  $ID $C)) 
      (:  $ID $C) ()) True)
; 

  (= 
    (shortest 
      (Cons  
        (with_self  $ID1 $C1) $Rest) 
      (with_self  $ID $C) $Residue) 
    ( (shortest $Rest 
        (with_self  $ID2 $C2) $Residue2) 
      (| 
        (det-if-then 
          (< $C1 $C2) 
          (, 
            (= $ID $ID1) 
            (= $C $C1) 
            (= $Residue $Rest))) 
        (det-if-then otherwise 
          (, 
            (= $ID $ID2) 
            (= $C $C2) 
            (= $Residue 
              (Cons  
                (with_self  $ID1 $C1) $Residue2))))) 
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
    (add_complexities  () ()) True)
; 


  (= 
    (add-complexities 
      (Cons  
        (with_self  $ID $Ex) $More) 
      (Cons  
        (with_self  $ID $C) $MorePairs)) 
    ( (complexity $Ex $C) (add-complexities $More $MorePairs)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (no-rules) 
    (not (get-clause $_ $_ $_ $_ $_)))
; 



  (= 
    (no-pos-examples) 
    (not (get-example $_ $_ +)))
; 



  (= 
    (no-neg-examples) 
    (not (get-example $_ $_ -)))
; 



  (= 
    (no-examples) 
    (not (get-example $_ $_ $_)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (delete-covered-examples) 
    ( (findall $I 
        (, 
          (get-evaluation $I $Eval) 
          (arg 3 $Eval $CoveredEx) 
          (member 
            (with_self  $ID $_) $CoveredEx) 
          (delete-example $ID)) $_) (set-det)))
; 

                     

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
;                                             (file  miles/kb.pl 1086 19 32347))


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
;                                             (file  miles/kb.pl 1108 19 32926))



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
;                                             (file  miles/kb.pl 1133 19 33588))


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (store_flat_clauses  ()) True)
; 

;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  miles/kb.pl 1159 23 34514))



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (store_unflat_clauses  ()) True)
; 

;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  miles/kb.pl 1188 27 35402))

;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  miles/kb.pl 1191 25 35490))



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (get-predlist $Predlist) 
    ( (mysetof 
        (with_self  $P $N) 
        (^ $I 
          (^ $H 
            (^ $B 
              (^ $CL 
                (^ $L 
                  (, 
                    (get-clause $I $H $B $CL $L) 
                    (\== $L type) 
                    (functor $H $P $N))))))) $Plist) (get-pred $Plist $Predlist)))
; 



  (= 
    (get_pred  () ()) True)
; 

  (= 
    (get-pred 
      (Cons  
        (with_self  $Pred $N) $R) 
      (Cons  
        (with_self  $P $PVars) $R1)) 
    ( (get-pred $R $R1) 
      (functor $P $Pred $N) 
      (det-if-then-else 
        (type-restriction $P $Vars) 
        (adapt-v $Vars $PVars) 
        (, 
          (=.. $P 
            (Cons  $_ $Vars)) 
          (adapt-v1 $Vars $PVars)))))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (adapt_v  () ()) True)
; 

  (= 
    (adapt-v 
      (Cons  $T $R) 
      (Cons  
        (with_self  $X $Tx) $R1)) 
    ( (adapt-v $R $R1) (=.. $T (:: $Tx $X))))
; 



  (= 
    (adapt_v1  () ()) True)
; 

  (= 
    (adapt-v1 
      (Cons  $X $R) 
      (Cons  
        (with_self  $X 
          (all)) $R1)) 
    (adapt-v1 $R $R1))
; 



