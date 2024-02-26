  (= 
    (?-  
      (use_module  
        (library  lists))) True)
; 




  !(op 650 yfx ^^)
; 

  !(op 601 xfy :)
; 

  !(op 1150 xfx ::=)
; 

  !(op 1175 xfx <:>)
; 

  !(op 1150 xfx ::-)
; 




  (= 
    (guardedexpr-a 
      (node guardedexpr-a 
        (:: (:: a)) 8) $A $B) 
    (c $A a $B))
; 

  (= 
    (guardedexpr-a 
      (node guardedexpr-a 
        (:: 
          (:: a) $A) 9) $B $C) 
    ( (c $B a $D) (expr $A $D $C)))
; 



  (= 
    (char $A $B) 
    ( (integer $A) 
      (< $A 256) 
      (set-det) 
      (name $B 
        (:: $A))))
; 



  (= 
    (c  
      (Cons  $A $B) $A $B) True)
; 



  (= 
    (dctg_rule_info  guardedexpr_b 11 
      (guardedexpr_b  
        (node  $_ $_ 11) $_ $_) 3 nonterminal) True)
; 

  (= 
    (dctg_rule_info  guardedexpr_a 9 
      (guardedexpr_a  
        (node  $_ $_ 9) $_ $_) 3 nonterminal) True)
; 

  (= 
    (dctg_rule_info  noniter_expr 5 
      (noniter_expr  
        (node  $_ $_ 5) $_ $_) 3 nonterminal) True)
; 

  (= 
    (dctg_rule_info  expr 0 
      (expr  
        (node  $_ $_ 0) $_ $_) 3 nonterminal) True)
; 

  (= 
    (dctg_rule_info  expr 1 
      (expr  
        (node  $_ $_ 1) $_ $_) 2 nonterminal) True)
; 

  (= 
    (dctg_rule_info  noniter_expr 4 
      (noniter_expr  
        (node  $_ $_ 4) $_ $_) 2 nonterminal) True)
; 

  (= 
    (dctg_rule_info  iter_expr 6 
      (iter_expr  
        (node  $_ $_ 6) $_ $_) 2 nonterminal) True)
; 

  (= 
    (dctg_rule_info  iter_expr 7 
      (iter_expr  
        (node  $_ $_ 7) $_ $_) 2 nonterminal) True)
; 

  (= 
    (dctg_rule_info  probval 13 
      (probval  
        (node  $_ $_ 13) $_ $_) 1 terminal) True)
; 

  (= 
    (dctg_rule_info  intval 12 
      (intval  
        (node  $_ $_ 12) $_ $_) 1 terminal) True)
; 

  (= 
    (dctg_rule_info  guardedexpr_b 10 
      (guardedexpr_b  
        (node  $_ $_ 10) $_ $_) 1 terminal) True)
; 

  (= 
    (dctg_rule_info  guardedexpr_a 8 
      (guardedexpr_a  
        (node  $_ $_ 8) $_ $_) 1 terminal) True)
; 

  (= 
    (dctg_rule_info  noniter_expr 3 
      (noniter_expr  
        (node  $_ $_ 3) $_ $_) 1 terminal) True)
; 

  (= 
    (dctg_rule_info  noniter_expr 2 
      (noniter_expr  
        (node  $_ $_ 2) $_ $_) 1 terminal) True)
; 



  (= 
    (library_directory  'c:/program files/sicstus prolog/library') True)
; 



  (= 
    (sre-pp-l (:: $A)) 
    ( (sre-pp $A) (set-det)))
; 

  (= 
    (sre-pp-l (Cons  $A $B)) 
    ( (write [) 
      (sre-pp $A) 
      (write +) 
      (sre-pp-l $B) 
      (write ]) 
      (set-det)))
; 



  (= 
    (sre2b $A) 
    ( (generate-tree expr grow 8 $_ $B $_) 
      (^^ $B 
        (construct $C)) 
      (^^ $B 
        (recognize $A $D 1.0 $E)) 
      (nl) 
      (sre-pp $C) 
      (nl) 
      (write 'Prob = ') 
      (write $E) 
      (nl) 
      (write 'Leftover = ') 
      (write $D) 
      (nl)))
; 



  (= 
    (sre2c $A $B $C) 
    ( (repeat) 
      (or 
        (= $A full) 
        (= $A grow)) 
      (generate-tree expr $A 12 $_ $D $_) 
      (^^ $D 
        (construct $B)) 
      (nl) 
      (write $A) 
      (nl) 
      (sre-pp $B) 
      (nl) 
      (bagof $E 
        (^^ $D 
          (recognize $C Nil 1.0 $E)) $F) 
      (write 'Pr list: ') 
      (nl) 
      (writelist $F) 
      (nl)))
; 



  (= 
    (sre2 $A $B $C) 
    ( (repeat) 
      (or 
        (= $A full) 
        (= $A grow)) 
      (generate-tree expr $A 12 $_ $D $_) 
      (^^ $D 
        (construct $B)) 
      (nl) 
      (write $A) 
      (nl) 
      (sre-pp $B) 
      (nl) 
      (bagof 
        (, $E $F) 
        (^^ $D 
          (recognize $C $E 1.0 $F)) $G) 
      (write 'Recog list: ') 
      (nl) 
      (writelist $G) 
      (nl)))
; 



  (= 
    (sre-pp (* $A $B)) 
    ( (write () 
      (sre-pp $A) 
      (write )*) 
      (write $B) 
      (set-det)))
; 

  (= 
    (sre-pp (+ $A $B)) 
    ( (write () 
      (sre-pp $A) 
      (write )+) 
      (write $B) 
      (set-det)))
; 

  (= 
    (sre-pp (with_self  $A $B)) 
    ( (sre-pp $A) 
      (write :) 
      (sre-pp $B) 
      (set-det)))
; 

  (= 
    (sre-pp (Cons  $A $B)) 
    ( (sre-pp-l (Cons  $A $B)) (set-det)))
; 

  (= 
    (sre-pp (, $A $B)) 
    ( (write () 
      (sre-pp $A) 
      (write ,) 
      (write $B) 
      (write )) 
      (set-det)))
; 

  (= 
    (sre-pp $A) 
    (write $A))
; 



  (= 
    (select-kth-term 
      (:: $A) $_ $B $B $A) 
    (set-det))
; 

  (= 
    (select-kth-term 
      (Cons  $A $_) $B $C $C $A) 
    ( (>= $A $B) (set-det)))
; 

  (= 
    (select-kth-term 
      (Cons  $_ $A) $B $C $D $E) 
    ( (is $F 
        (+ $C 1)) (select-kth-term $A $B $F $D $E)))
; 



  (= 
    (sumlist  () () $A $A) True)
; 

  (= 
    (sumlist 
      (Cons  $A $B) 
      (Cons  $C $D) $E $F) 
    ( (is $C 
        (+ $E $A)) (sumlist $B $D $C $F)))
; 



  (= 
    (int_range  0 1000) True)
; 



  (= 
    (is-a-probability $A) 
    ( (float $A) (set-det)))
; 

  (= 
    (is-a-probability $A) 
    ( (with_self  
        (random *) 
        (random $B)) (is $A (/ (truncate (* $B 100)) 100))))
; 



  (= 
    (is-an-integer $A) 
    ( (integer $A) (set-det)))
; 

  (= 
    (is-an-integer $A) 
    ( (int-range $B $C) (with_self  (random *) (random $B $C $A))))
; 



  (= 
    (recognize-loop $_ $A Nil Nil $B $C) 
    ( (set-det) 
      (is $C 
        (* $B 
          (- 1.0 $A))) 
      (check-prob $C)))
; 

  (= 
    (recognize-loop $_ $A $B $B $C $D) 
    ( (is $D 
        (* $C 
          (- 1.0 $A))) (check-prob $D)))
; 

  (= 
    (recognize-loop $A $B $C $D $E $F) 
    ( (is $G 
        (* $E $B)) 
      (check-prob $G) 
      (^^ $A 
        (recognize $C $H $G $I)) 
      (not (= $C $H)) 
      (check-prob $I) 
      (recognize-loop $A $B $H $D $I $F)))
; 



  (= 
    (raw-gen-loop $A $B $C $D $E $F) 
    ( (< $E $C) 
      (maybe $B) 
      (^^ $A 
        (raw-generate $G $E $H)) 
      (raw-gen-loop $A $B $C $I $H $F) 
      (with_self  
        (lists *) 
        (append $G $I $D)) 
      (set-det)))
; 

  (= 
    (raw-gen-loop $_ $_ $_ Nil $A $A) 
    (set-det))
; 



  (= 
    (probval 
      (node probval 
        (:: (:: $A)) 13) $B $C) 
    ( (c $B $A $C) (is-a-probability $A)))
; 



  (= 
    (raw-select-term $A $B) 
    ( (sumlist $A $C 0 $D) 
      (with_self  
        (random *) 
        (random 0 $D $E)) 
      (select-kth-term $C $E 1 $B $_) 
      (set-det)))
; 



  (= 
    (guardedexpr-b 
      (node guardedexpr-b 
        (:: (:: b)) 10) $A $B) 
    (c $A b $B))
; 

  (= 
    (guardedexpr-b 
      (node guardedexpr-b 
        (:: 
          (:: b) $A) 11) $B $C) 
    ( (c $B b $D) (expr $A $D $C)))
; 



  (= 
    (intval 
      (node intval 
        (:: (:: $A)) 12) $B $C) 
    ( (c $B $A $C) (is-an-integer $A)))
; 



  (= 
    (noniter-expr 
      (node noniter-expr 
        (:: (:: a)) 2) $A $B) 
    (c $A a $B))
; 

  (= 
    (noniter-expr 
      (node noniter-expr 
        (:: (:: b)) 3) $A $B) 
    (c $A b $B))
; 

  (= 
    (noniter-expr 
      (node noniter-expr 
        (:: $A $B $C $D) 4) $E $F) 
    ( (guardedexpr-a $A $E $G) 
      (intval $B $G $H) 
      (guardedexpr-b $C $H $I) 
      (intval $D $I $F)))
; 

  (= 
    (noniter-expr 
      (node noniter-expr 
        (:: $A $B) 5) $C $D) 
    ( (expr $A $C $E) (expr $B $E $D)))
; 



  (= 
    (check-prob $A) 
    ( (min-grammar-prob-P $B) 
      (> $A $B) 
      (set-det)))
; 



  (= 
    (iter-expr 
      (node iter-expr 
        (:: $A $B) 6) $C $D) 
    ( (noniter-expr $A $C $E) (probval $B $E $D)))
; 

  (= 
    (iter-expr 
      (node iter-expr 
        (:: $A $B) 7) $C $D) 
    ( (noniter-expr $A $C $E) (probval $B $E $D)))
; 



  (= 
    (identify_type  () () ()) True)
; 

  (= 
    (identify-type 
      (Cons  $A $B) 
      (Cons  $A $C) $D) 
    ( (dctg-rule-info $_ $A $_ $_ terminal) 
      (set-det) 
      (identify-type $B $C $D)))
; 

  (= 
    (identify-type 
      (Cons  $A $B) $C 
      (Cons  $A $D)) 
    (identify-type $B $C $D))
; 



  (= 
    (get-rule-stuff $A $B) 
    ( (get-symbols  &self 
        (= 
          (semantic_rule  $B $_ $C $_) $_)) (=.. $C (Cons  $A $_))))
; 



  (= 
    (make-id-entries Nil) 
    (set-det))
; 

  (= 
    (make-id-entries (Cons  (, $A $B) $C)) 
    ( (add-symbol  &self 
        (dctg_id_table  $A $B $_ $_)) 
      (make-id-entries $C) 
      (set-det)))
; 



  (= 
    (make-rule-id-list2 $A $B) 
    ( (bagof $C 
        (get-rule-stuff $A $C) $D) (rem-dups $D $B)))
; 



  (= 
    (same-goal $A $B) 
    ( (=.. $A 
        (Cons  $C $_)) 
      (=.. $B 
        (Cons  $C $_)) 
      (set-det)))
; 



  (= 
    (abstract-member2 $A 
      (Cons  $B $_)) 
    (same-goal $A $B))
; 

  (= 
    (abstract-member2 $A 
      (Cons  $_ $B)) 
    (abstract-member2 $A $B))
; 



  (= 
    (goal-type $A $B $_ $C $D $E $C $D 
      (Cons  $A $E)) 
    ( (det-if-then-else 
        (= $B 
          (, $F $_)) True 
        (= $B $F)) 
      (or 
        (abstract-member2 $F $E) 
        (same-goal $A $F)) 
      (set-det)))
; 

  (= 
    (goal-type $A $B $C $D $E $F 
      (Cons  $A $D) $E $F) 
    ( (det-if-then-else 
        (= $B 
          (, $G $_)) True 
        (= $B $G)) 
      (or 
        (abstract-member2 $G $D) 
        (abstract-member2 $G $C)) 
      (set-det)))
; 

  (= 
    (goal-type $A 
      (, $_ $B) $C $D $E $F $G $H $I) 
    ( (set-det) (goal-type $A $B $C $D $E $F $G $H $I)))
; 

  (= 
    (goal_type  $A $_ $_ $B $C $D $B 
      (Cons  $A $C) $D) True)
; 



  (= 
    (user-override $A $B $C 
      (Cons  $A $B) $C) 
    ( (=.. $A 
        (Cons  $D $_)) 
      (dctg-override-P $E $_) 
      (with_self  
        (lists *) 
        (member $D $E)) 
      (set-det)))
; 

  (= 
    (user-override $A $B $C $B 
      (Cons  $A $C)) 
    ( (=.. $A 
        (Cons  $D $_)) 
      (dctg-override-P $_ $E) 
      (with_self  
        (lists *) 
        (member $D $E)) 
      (set-det)))
; 



  (= 
    (grammar-type-loop Nil $A $B $C $A $B $C) 
    (set-det))
; 

  (= 
    (grammar-type-loop 
      (Cons  $A $B) $C $D $E $F $G $H) 
    ( (user-override $A $D $E $I $J) (grammar-type-loop $B $C $I $J $F $G $H)))
; 

  (= 
    (grammar-type-loop 
      (Cons  $A $B) $C $D $E $F $G $H) 
    ( (copy-term $A $I) 
      (get-symbols  &self 
        (= $I $J)) 
      (goal-type $A $J $B $C $D $E $K $L $M) 
      (grammar-type-loop $B $K $L $M $F $G $H)))
; 



  (= 
    (find_minimum_depth  $_ () $A $A) True)
; 

  (= 
    (find-minimum-depth $A 
      (Cons  
        (, $B $C) $D) $E $F) 
    ( (=.. $B 
        (Cons  $A $_)) 
      (is $G 
        (min $C $E)) 
      (find-minimum-depth $A $D $G $F) 
      (set-det)))
; 

  (= 
    (find-minimum-depth $A 
      (Cons  $_ $B) $C $D) 
    ( (find-minimum-depth $A $B $C $D) (set-det)))
; 



  (= 
    (abstract-member $A 
      (Cons  
        (, $B $_) $_)) 
    (=.. $B 
      (Cons  $A $_)))
; 

  (= 
    (abstract-member $A 
      (Cons  $_ $B)) 
    (abstract-member $A $B))
; 



  (= 
    (find-min-depth $A 
      (Cons  
        (, $B $C) $_) $C) 
    ( (=.. $A 
        (Cons  $B $_)) (set-det)))
; 

  (= 
    (find-min-depth $A 
      (Cons  $_ $B) $C) 
    ( (find-min-depth $A $B $C) (set-det)))
; 



  (= 
    (is-a-rule-call $A) 
    ( (=.. $A 
        (Cons  $B $_)) 
      (dctg-id-table $B $_ $_ $_) 
      (set-det)))
; 



  (= 
    (find-min-depth-body 
      (, $A $B) $C $D $E) 
    ( (is-a-rule-call $A) 
      (set-det) 
      (find-min-depth $A $C $F) 
      (is $G 
        (max $F $D)) 
      (find-min-depth-body $B $C $G $E)))
; 

  (= 
    (find-min-depth-body 
      (, $_ $A) $B $C $D) 
    ( (set-det) (find-min-depth-body $A $B $C $D)))
; 

  (= 
    (find-min-depth-body $A $B $C $D) 
    ( (is-a-rule-call $A) 
      (set-det) 
      (find-min-depth $A $B $E) 
      (is $D 
        (max $E $C))))
; 

  (= 
    (find-min-depth-body $_ $_ $A $A) 
    (set-det))
; 



  (= 
    (find-rule-mins Nil $A $A) 
    (set-det))
; 

  (= 
    (find-rule-mins 
      (Cons  
        (, $A $B) $C) $D $E) 
    ( (=.. $A 
        (Cons  $F $_)) 
      (not (member (, $F $_) $D)) 
      (set-det) 
      (find-rule-mins $C 
        (Cons  
          (, $F $B) $D) $E)))
; 

  (= 
    (find-rule-mins 
      (Cons  $_ $A) $B $C) 
    (find-rule-mins $A $B $C))
; 



  (= 
    (process-rules Nil $A $_ $B $A $B) 
    (set-det))
; 

  (= 
    (process-rules 
      (Cons  $A $B) $C $D $E $F $G) 
    ( (copy-term $A $H) 
      (get-symbols  &self 
        (= $H $I)) 
      (find-min-depth-body $I $D 0 $J) 
      (set-det) 
      (is $K 
        (+ $J 1)) 
      (process-rules $B 
        (Cons  
          (, $A $K) $C) $D $E $F $G)))
; 

  (= 
    (process-rules 
      (Cons  $A $B) $C $D $E $F $G) 
    ( (set-det) (process-rules $B $C $D (Cons  $A $E) $F $G)))
; 



  (= 
    (set-rule-data Nil $_) 
    (set-det))
; 

  (= 
    (set-rule-data 
      (Cons  
        (, $A $B) $C) $D) 
    ( (=.. $A 
        (Cons  $E $F)) 
      (with_self  
        (lists *) 
        (append $_ 
          (:: 
            (node $_ $_ $G) $_ $_) $F)) 
      (det-if-then-else 
        (with_self  
          (lists *) 
          (member $A $D)) 
        (= $H terminal) 
        (= $H nonterminal)) 
      (add-symbol  &self 
        (dctg_rule_info  $E $G $A $B $H)) 
      (set-rule-data $C $D) 
      (set-det)))
; 



  (= 
    (grammar-type-top-loop $A $B $C $D) 
    ( (grammar-type-loop $A Nil $B $C $E $F $G) 
      (det-if-then-else 
        (, 
          (length $A $H) 
          (length $E $H)) 
        (= $F $D) 
        (grammar-type-top-loop $E $F $G $D)) 
      (set-det)))
; 



  (= 
    (grammar-depth-top-loop Nil $A $_ $A) 
    (set-det))
; 

  (= 
    (grammar-depth-top-loop $A $B $C $D) 
    ( (process-rules $A $B $C Nil $E $F) 
      (find-rule-mins $E $C $G) 
      (det-if-then-else 
        (, 
          (length $A $H) 
          (length $F $H)) 
        (, 
          (write 'Problem - ') 
          (write $H) 
          (write ' rules cannot terminate:') 
          (nl) 
          (writelist $F) 
          (nl) 
          (write 'these terminated - ') 
          (nl) 
          (writelist $E) 
          (nl) 
          (write 'These are mincalls - ') 
          (nl) 
          (writelist $G) 
          (nl) 
          (fail)) 
        (grammar-depth-top-loop $F $E $G $D)) 
      (set-det)))
; 



  (= 
    (clone-list Nil Nil) 
    (set-det))
; 

  (= 
    (clone-list 
      (Cons  $_ $A) 
      (Cons  $_ $B)) 
    ( (clone-list $A $B) (set-det)))
; 



  (= 
    (get-rule-name $A) 
    ( (get-symbols  &self 
        (= 
          (semantic_rule  $B $_ $C $_) $_)) 
      (=.. $C 
        (Cons  $D $E)) 
      (clone-list $E $F) 
      (with_self  
        (lists *) 
        (append $F 
          (:: 
            (node $_ $_ $B) $_ $_) $G)) 
      (=.. $A 
        (Cons  $D $G))))
; 



  (= 
    (dctg_id_table  expr 
      (0 1) () 
      (0 1)) True)
; 

  (= 
    (dctg_id_table  guardedexpr_a 
      (8 9) 
      (8) 
      (9)) True)
; 

  (= 
    (dctg_id_table  guardedexpr_b 
      (10 11) 
      (10) 
      (11)) True)
; 

  (= 
    (dctg_id_table  intval 
      (12) 
      (12) ()) True)
; 

  (= 
    (dctg_id_table  iter_expr 
      (6 7) () 
      (6 7)) True)
; 

  (= 
    (dctg_id_table  noniter_expr 
      (2 3 4 5) 
      (2 3) 
      (4 5)) True)
; 

  (= 
    (dctg_id_table  probval 
      (13) 
      (13) ()) True)
; 



  (= 
    (enhance-rule-id-list) 
    ( (remove-symbol  &self 
        (dctg_id_table  $A $B $_ $_)) 
      (identify-type $B $C $D) 
      (add-symbol  &self 
        (dctg_id_table  $A $B $C $D)) 
      (fail)))
; 

  (= enhance_rule_id_list True)
; 



  (= 
    (generate-rule-data) 
    ( (findall $A 
        (get-rule-name $A) $B) 
      (rem-dups $B $C) 
      (grammar-depth-top-loop $C Nil Nil $D) 
      (grammar-type-top-loop $C Nil Nil $E) 
      (set-rule-data $D $E) 
      (set-det)))
; 



  (= 
    (make-rule-id-list) 
    ( (findall 
        (, $A $B) 
        (make-rule-id-list2 $A $B) $C) 
      (make-id-entries $C) 
      (set-det)))
; 



  (= 
    (cleanup-grammar-data) 
    ( (remove-all-symbols  &self 
        (dctg_rule_info  $_ $_ $_ $_)) 
      (remove-all-symbols  &self 
        (dctg_id_table  $_ $_ $_ $_)) 
      (set-det)))
; 



  (= 
    (make-grammar-table) 
    ( (cleanup-grammar-data) 
      (make-rule-id-list) 
      (generate-rule-data) 
      (enhance-rule-id-list) 
      (set-det)))
; 



  (= 
    (file-search-path library $A) 
    (library-directory $A))
; 

  (= 
    (file-search-path system $A) 
    (prolog-flag host-type $A))
; 



  (= 
    (eval_with_ID_P  no) True)
; 



  (= 
    (negsetsize_P  30) True)
; 



  (= 
    (elite_migrate_P  0 no) True)
; 



  (= 
    (unique_guards_P  no) True)
; 



  (= 
    (min_skip_prob_P  0.0001) True)
; 



  (= 
    (min_grammar_prob_P  0.0001) True)
; 



  (= 
    (gen_set_size_P  1000) True)
; 



  (= 
    (sre_mintestcnt_P  2) True)
; 



  (= 
    (mutation_range_P  0.1) True)
; 



  (= 
    (dctg_override_P  () ()) True)
; 



  (= 
    (expr 
      (node expr 
        (:: $A) 0) $B $C) 
    (iter-expr $A $B $C))
; 

  (= 
    (expr 
      (node expr 
        (:: $A) 1) $B $C) 
    (noniter-expr $A $B $C))
; 



  (= 
    (dctg_root_P  expr) True)
; 



  (= 
    (user_args_P  ()) True)
; 



  (= 
    (reprod_verif_P  no) True)
; 



  (= 
    (evaluator_reset_P  generate_testset 100) True)
; 



  (= 
    (gen_type_P  steadystate) True)
; 



  (= 
    (popn_dump_P  no) True)
; 



  (= 
    (max_string_length_P  20) True)
; 



  (= 
    (rep_limit_P  2) True)
; 



  (= 
    (trace_limit_P  0 0) True)
; 



  (= 
    (unique_population_P  yes) True)
; 



  (= 
    (lamarckian_P  0.0 10 best 0.1) True)
; 



  (= 
    (tournament_size_P  4 4) True)
; 



  (= 
    (error_tolerance_P  0) True)
; 



  (= 
    (max_depth_P  10 17) True)
; 



  (= 
    (prob_terminal_mutation_P  0.75) True)
; 



  (= 
    (prob_internal_crossover_P  0.9) True)
; 



  (= 
    (reprod_P  3) True)
; 



  (= 
    (prob_crossover_P  0.9) True)
; 



  (= 
    (prob_grow_P  0.5) True)
; 



  (= 
    (max_runs_P  1 solution 3) True)
; 



  (= 
    (cull_method_P  elite) True)
; 



  (= 
    (population_size_P  75 50) True)
; 



  (= 
    (dctg_file_P  sre3.pl) True)
; 



  (= 
    (fitness_func_P  reg_gram_1) True)
; 



  (= 
    (wd_P  c:/research/sre_dna_fastX) True)
; 



  (= 
    (seed_P  random 
      (,  $_ 
        (,  $_ $_))) True)
; 



  (= 
    (rule_number  14) True)
; 



  (= 
    (semantic-rule 0 
      (construct $A) expr 
      (:: $B)) 
    ( (set-det) (^^ $B (construct $A))))
; 

  (= 
    (semantic-rule 0 
      (raw-generate $A $B $C) expr 
      (:: $D)) 
    ( (set-det) (^^ $D (raw-generate $A $B $C))))
; 

  (= 
    (semantic-rule 0 
      (recognize $A $B $C $D) expr 
      (:: $E)) 
    ( (set-det) 
      (check-prob $C) 
      (^^ $E 
        (recognize $A $B $C $D))))
; 

  (= 
    (semantic-rule 1 
      (construct $A) expr 
      (:: $B)) 
    ( (set-det) (^^ $B (construct $A))))
; 

  (= 
    (semantic-rule 1 
      (raw-generate $A $B $C) expr 
      (:: $D)) 
    ( (set-det) (^^ $D (raw-generate $A $B $C))))
; 

  (= 
    (semantic-rule 1 
      (recognize $A $B $C $D) expr 
      (:: $E)) 
    ( (set-det) 
      (check-prob $C) 
      (^^ $E 
        (recognize $A $B $C $D))))
; 

  (= 
    (semantic-rule 2 
      (construct a) noniter-expr 
      (:: (:: a))) 
    (set-det))
; 

  (= 
    (semantic-rule 2 
      (raw-generate 
        (:: a) $A $B) noniter-expr 
      (:: (:: a))) 
    ( (set-det) (is $B (+ $A 1))))
; 

  (= 
    (semantic-rule 2 
      (recognize 
        (Cons  a $A) $A $B $B) noniter-expr 
      (:: (:: a))) 
    ( (set-det) (check-prob $B)))
; 

  (= 
    (semantic-rule 3 
      (construct b) noniter-expr 
      (:: (:: b))) 
    (set-det))
; 

  (= 
    (semantic-rule 3 
      (raw-generate 
        (:: b) $A $B) noniter-expr 
      (:: (:: b))) 
    ( (set-det) (is $B (+ $A 1))))
; 

  (= 
    (semantic-rule 3 
      (recognize 
        (Cons  b $A) $A $B $B) noniter-expr 
      (:: (:: b))) 
    ( (set-det) (check-prob $B)))
; 

  (= 
    (semantic-rule 4 
      (construct (:: (, $A $B) (, $C $D))) noniter-expr 
      (:: $E $F $G $H)) 
    ( (set-det) 
      (^^ $E 
        (construct $A)) 
      (^^ $F 
        (construct $B)) 
      (^^ $G 
        (construct $C)) 
      (^^ $H 
        (construct $D))))
; 

  (= 
    (semantic-rule 4 
      (raw-generate $A $B $C) noniter-expr 
      (:: $D $E $F $G)) 
    ( (set-det) 
      (^^ $E 
        (construct $H)) 
      (^^ $G 
        (construct $I)) 
      (det-if-then-else 
        (raw-select-term 
          (:: $H $I) 1) 
        (^^ $D 
          (raw-generate $A $B $C)) 
        (^^ $F 
          (raw-generate $A $B $C)))))
; 

  (= 
    (semantic-rule 4 
      (recognize $A $B $C $D) noniter-expr 
      (:: $E $F $_ $G)) 
    ( (set-det) 
      (^^ $F 
        (construct $H)) 
      (^^ $G 
        (construct $I)) 
      (is $J 
        (/ 
          (* $C $H) 
          (+ $H $I))) 
      (check-prob $J) 
      (^^ $E 
        (recognize $A $B $J $D))))
; 

  (= 
    (semantic-rule 4 
      (recognize $A $B $C $D) noniter-expr 
      (:: $_ $E $F $G)) 
    ( (set-det) 
      (^^ $E 
        (construct $H)) 
      (^^ $G 
        (construct $I)) 
      (is $J 
        (/ 
          (* $C $I) 
          (+ $H $I))) 
      (check-prob $J) 
      (^^ $F 
        (recognize $A $B $J $D))))
; 

  (= 
    (semantic-rule 5 
      (construct (with_self  $A $B)) noniter-expr 
      (:: $C $D)) 
    ( (set-det) 
      (^^ $C 
        (construct $A)) 
      (^^ $D 
        (construct $B))))
; 

  (= 
    (semantic-rule 5 
      (raw-generate $A $B $C) noniter-expr 
      (:: $D $E)) 
    ( (set-det) 
      (^^ $D 
        (raw-generate $F $B $G)) 
      (^^ $E 
        (raw-generate $H $G $C)) 
      (with_self  
        (lists *) 
        (append $F $H $A))))
; 

  (= 
    (semantic-rule 5 
      (recognize $A $B $C $D) noniter-expr 
      (:: $E $F)) 
    ( (set-det) 
      (check-prob $C) 
      (^^ $E 
        (recognize $A $G $C $H)) 
      (check-prob $H) 
      (^^ $F 
        (recognize $G $B $H $D))))
; 

  (= 
    (semantic-rule 6 
      (construct (* $A $B)) iter-expr 
      (:: $C $D)) 
    ( (set-det) 
      (^^ $C 
        (construct $A)) 
      (^^ $D 
        (construct $B))))
; 

  (= 
    (semantic-rule 6 
      (raw-generate $A $B $C) iter-expr 
      (:: $D $E)) 
    ( (set-det) 
      (^^ $E 
        (construct $F)) 
      (max-string-length-P $G) 
      (raw-gen-loop $D $F $G $A $B $C)))
; 

  (= 
    (semantic-rule 6 
      (recognize $A $B $C $D) iter-expr 
      (:: $E $F)) 
    ( (set-det) 
      (check-prob $C) 
      (^^ $F 
        (construct $G)) 
      (recognize-loop $E $G $A $B $C $D)))
; 

  (= 
    (semantic-rule 7 
      (construct (+ $A $B)) iter-expr 
      (:: $C $D)) 
    ( (set-det) 
      (^^ $C 
        (construct $A)) 
      (^^ $D 
        (construct $B))))
; 

  (= 
    (semantic-rule 7 
      (raw-generate $A $B $C) iter-expr 
      (:: $D $E)) 
    ( (set-det) 
      (^^ $D 
        (raw-generate $F $B $G)) 
      (^^ $E 
        (construct $H)) 
      (max-string-length-P $I) 
      (raw-gen-loop $D $H $I $J $G $C) 
      (with_self  
        (lists *) 
        (append $F $J $A)) 
      (set-det)))
; 

  (= 
    (semantic-rule 7 
      (recognize $A $B $C $D) iter-expr 
      (:: $E $F)) 
    ( (set-det) 
      (check-prob $C) 
      (^^ $E 
        (recognize $A $G $C $H)) 
      (not (= $A $G)) 
      (check-prob $H) 
      (^^ $F 
        (construct $I)) 
      (recognize-loop $E $I $G $B $H $D)))
; 

  (= 
    (semantic-rule 8 
      (construct a) guardedexpr-a 
      (:: (:: a))) 
    (set-det))
; 

  (= 
    (semantic-rule 8 
      (raw-generate 
        (:: a) $A $B) guardedexpr-a 
      (:: (:: a))) 
    ( (set-det) (is $B (+ $A 1))))
; 

  (= 
    (semantic-rule 8 
      (recognize 
        (Cons  a $A) $A $B $B) guardedexpr-a 
      (:: (:: a))) 
    ( (set-det) (check-prob $B)))
; 

  (= 
    (semantic-rule 9 
      (construct (with_self  (a *) $A)) guardedexpr-a 
      (:: 
        (:: a) $B)) 
    ( (set-det) (^^ $B (construct $A))))
; 

  (= 
    (semantic-rule 9 
      (raw-generate 
        (Cons  a $A) $B $C) guardedexpr-a 
      (:: 
        (:: a) $D)) 
    ( (set-det) 
      (^^ $D 
        (raw-generate $A $B $E)) 
      (is $C 
        (+ $E 1))))
; 

  (= 
    (semantic-rule 9 
      (recognize 
        (Cons  a $A) $B $C $D) guardedexpr-a 
      (:: 
        (:: a) $E)) 
    ( (set-det) 
      (check-prob $C) 
      (^^ $E 
        (recognize $A $B $C $D))))
; 

  (= 
    (semantic-rule 10 
      (construct b) guardedexpr-b 
      (:: (:: b))) 
    (set-det))
; 

  (= 
    (semantic-rule 10 
      (raw-generate 
        (:: b) $A $B) guardedexpr-b 
      (:: (:: b))) 
    ( (set-det) (is $B (+ $A 1))))
; 

  (= 
    (semantic-rule 10 
      (recognize 
        (Cons  b $A) $A $B $B) guardedexpr-b 
      (:: (:: b))) 
    ( (set-det) (check-prob $B)))
; 

  (= 
    (semantic-rule 11 
      (construct (with_self  (b *) $A)) guardedexpr-b 
      (:: 
        (:: b) $B)) 
    ( (set-det) (^^ $B (construct $A))))
; 

  (= 
    (semantic-rule 11 
      (raw-generate 
        (Cons  b $A) $B $C) guardedexpr-b 
      (:: 
        (:: b) $D)) 
    ( (set-det) 
      (^^ $D 
        (raw-generate $A $B $E)) 
      (is $C 
        (+ $E 1))))
; 

  (= 
    (semantic-rule 11 
      (recognize 
        (Cons  b $A) $B $C $D) guardedexpr-b 
      (:: 
        (:: b) $E)) 
    ( (set-det) 
      (check-prob $C) 
      (^^ $E 
        (recognize $A $B $C $D))))
; 

  (= 
    (semantic-rule 12 
      (construct $A) intval 
      (:: (:: $A))) 
    (set-det))
; 

  (= 
    (semantic-rule 13 
      (construct $A) probval 
      (:: (:: $A))) 
    (set-det))
; 



  (= 
    (process $A) 
    ( (or 
        (= $A 
          (<:> $B $C)) 
        (= $A 
          (::= $B $C))) 
      (set-det) 
      (translate-rule $A $D) 
      (add-symbol  &self $D) 
      (set-det)))
; 

  (= 
    (process !$A) 
    ( (set-det) (call $A)))
; 

  (= 
    (process (= $A $B)) 
    ( (set-det) (add-symbol  &self (:-  $A $B))))
; 

  (= 
    (process $A) 
    (add-symbol  &self $A))
; 



  (= 
    (check-it $A) 
    ( (= $A end-of-file) (set-det)))
; 

  (= 
    (check-it $A) 
    ( (process $A) (fail)))
; 



  (= 
    (consume) 
    ( (repeat) 
      (read $A) 
      (check-it $A)))
; 



  (= 
    (grammar $A) 
    ( (seeing $B) 
      (see $A) 
      (consume) 
      (seen) 
      (see $B)))
; 



  (= 
    (add-extra-args $A $B $C) 
    ( (=.. $B $D) 
      (with_self  
        (lists *) 
        (append $D $A $E)) 
      (=.. $C $E)))
; 



  (= 
    (assert-semantic-rule $A $B $C 
      (, $D $E)) 
    ( (set-det) 
      (or 
        (= $D 
          (::- $F $G)) 
        (, 
          (= $F $D) 
          (= $G True))) 
      (add-symbol  &self 
        (:-  
          (semantic_rule  $A $F $B $C) 
          (,  ! $G))) 
      (assert-semantic-rule $A $B $C $E)))
; 

  (= 
    (assert-semantic-rule $A $B $C $D) 
    ( (or 
        (= $D 
          (::- $E $F)) 
        (, 
          (= $E $D) 
          (= $F True))) (add-symbol  &self (:-  (semantic_rule  $A $E $B $C) (,  ! $F)))))
; 



  (= 
    (prod-number $A) 
    ( (remove-symbol  &self 
        (rule_number  $A)) 
      (is $B 
        (+ $A 1)) 
      (add-symbol  &self 
        (rule_number  $B))))
; 



  (= 
    (tidy 
      (, $A $B $C) $D) 
    (tidy 
      (, $A $B $C) $D))
; 

  (= 
    (tidy 
      (, $A $B) 
      (, $C $D)) 
    ( (set-det) 
      (tidy $A $C) 
      (tidy $B $D)))
; 

  (= 
    (tidy $A $A) 
    (set-det))
; 



  (= 
    (t-rp 
      (set-det) $A $A $B $B 
      (set-det)) 
    (set-det))
; 

  (= 
    (t-rp Nil $A 
      (Cons  Nil $A) $B $C 
      (= $B $C)) 
    (set-det))
; 

  (= 
    (t-rp 
      (:: $A) $B 
      (Cons  
        (:: $C) $B) $D $E 
      (c $D $A $E)) 
    (char $A $C))
; 

  (= 
    (t-rp 
      (:: $A) $B 
      (Cons  
        (:: $A) $B) $C $D 
      (c $C $A $D)) 
    (set-det))
; 

  (= 
    (t-rp 
      (Cons  $A $B) $C 
      (Cons  
        (Cons  $D $E) $C) $F $G 
      (, 
        (c $F $A $H) $I)) 
    ( (char $A $D) (t-rp $B $C (Cons  $E $C) $H $G $I)))
; 

  (= 
    (t-rp 
      (Cons  $A $B) $C 
      (Cons  
        (Cons  $A $B) $C) $D $E 
      (, 
        (c $D $A $F) $G)) 
    ( (set-det) (t-rp $B $C (Cons  $B $C) $F $E $G)))
; 

  (= 
    (t-rp 
      {$A } $B $B $C $C $A) 
    (set-det))
; 

  (= 
    (t-rp 
      (, $A $B) $C $D $E $F 
      (, $G $H)) 
    ( (set-det) 
      (t-rp $A $C $I $E $J $G) 
      (t-rp $B $I $D $J $F $H)))
; 

  (= 
    (t-rp 
      (^^ $A $B) $C 
      (Cons  $B $C) $D $E $F) 
    (add-extra-args 
      (:: $B $D $E) $A $F))
; 

  (= 
    (t-rp $A $B 
      (Cons  $C $B) $D $E $F) 
    (add-extra-args 
      (:: $C $D $E) $A $F))
; 



  (= 
    (t-lp 
      (, $A $B) $C $D $E $F $G) 
    ( (with_self  
        (lists *) 
        (append $B $E $H)) 
      (prod-number $I) 
      (assert-semantic-rule $I $A $C $F) 
      (add-extra-args 
        (:: 
          (node $A $C $I) $D $H) $A $G)))
; 

  (= 
    (t-lp $A $B $C $D $E $F) 
    ( (prod-number $G) 
      (assert-semantic-rule $G $A $B $E) 
      (add-extra-args 
        (:: 
          (node $A $B $G) $C $D) $A $F)))
; 



  (= 
    (translate-rule 
      (<:> 
        (::= $A Nil) $B) $C) 
    ( (set-det) (t-lp $A Nil $D $D $B $C)))
; 

  (= 
    (translate-rule 
      (::= $A Nil) $B) 
    ( (set-det) (t-lp $A Nil $C $C Nil $B)))
; 

  (= 
    (translate-rule 
      (<:> 
        (::= $A $B) $C) 
      (= $D $E)) 
    ( (set-det) 
      (t-rp $B Nil $F $G $H $I) 
      (with_self  
        (lists *) 
        (reverse $F $J)) 
      (t-lp $A $J $G $H $C $D) 
      (tidy $I $E)))
; 

  (= 
    (translate-rule 
      (::= $A $B) 
      (= $C $D)) 
    (translate-rule 
      (<:> 
        (::= $A $B) Nil) 
      (= $C $D)))
; 



  (= 
    (^^ 
      (node $A $B $C) $D) 
    (semantic-rule $C $D $A $B))
; 



  (= 
    (sre $A $B $C $D) 
    ( (repeat) 
      (or 
        (= $A full) 
        (= $A grow)) 
      (generate-tree expr $A 12 $_ $E $_) 
      (^^ $E 
        (construct $B)) 
      (^^ $E 
        (raw-generate $C 0 $D)) 
      (nl) 
      (sre-pp $B) 
      (nl) 
      (write 'tree ') 
      (write $E) 
      (nl) 
      (tree-depth $E $F) 
      (write 'Depth = ') 
      (write $F) 
      (nl)))
; 



