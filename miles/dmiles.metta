
  !(use-module (library logicmoo-utils))
; 


  !(multifile (/ file-search-path 2))
; 

  !(dynamic (/ file-search-path 2))
; 



  (= 
    (ensure-loaded-if-exists $X) 
    (det-if-then-else 
      (exists-source $X) 
      (with_self  
        (system *) 
        (ensure-loaded $X)) 
      (dmsg (ensure-loaded-if-exists $X))))
; 


  (= 
    (use-module-if-exists $X) 
    (det-if-then-else 
      (exists-source $X) 
      (with_self  
        (system *) 
        (use-module $X)) 
      (dmsg (use-module-if-exists $X))))
; 

  (= 
    (use-module-if-exists $X $Y) 
    (det-if-then-else 
      (exists-source $X) 
      (with_self  
        (system *) 
        (use-module $X $Y)) 
      (dmsg (use-module-if-exists $X $Y))))
; 



  !((prolog-load-context directory $Dir) (add-symbol  &self (file_search_path  home $Dir)))
; 



  (= 
    (rev $A $B) 
    (rev $A $B Nil))
; 


  (= 
    (rev Nil $B $B) 
    (set-det))
; 

  (= 
    (rev 
      (Cons  $X $A) $B $C) 
    (rev $A $B 
      (Cons  $X $C)))
; 



  (= 
    (nonmember $Arg 
      (Cons  $Arg $_)) 
    ( (set-det) (fail)))
; 

  (= 
    (nonmember $Arg 
      (Cons  $_ $Tail)) 
    ( (set-det) (nonmember $Arg $Tail)))
; 

  (= 
    (nonmember  $_ ()) True)
; 



;
; 


  (= 
    (yesno $Question) 
    (yesno $Question no))
; 

  (= 
    (yesno $Question $Default) 
    ( (format '~N~w? (~w): ' 
        (:: $Question $Default)) 
      (get-single-char $YN) 
      (det-if-then-else 
        (= $YN 13) 
        (== $Default yes) 
        (member $YN 
          (:: 121 89)))))
; 

;
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
    (ask-chars $Label $S $E $Answer) 
    ( (repeat) 
      (format '~N~w: ?' 
        (:: $Label)) 
      (read-line-to-string-echo current-input $Answer) 
      (atom-length $Answer $Len) 
      (det-if-then-else 
        (between $S $E $Len) 
        (set-det) 
        (, 
          (format "~NPlease enter between ~w and ~w characters.~n" 
            (:: $S $E)) 
          (fail)))))
; 



  (= 
    (unify $X $Y) 
    (unify-with-occurs-check $X $Y))
; 



  !(use-module-if-exists (library ordsets))
; 



  (= 
    (union $X $Y) 
    (ord-union $X $Y))
; 

;
; 



  (= 
    ($list-skel $V) 
    ( (var $V) 
      (set-det) 
      (fail)))
; 

  (= 
    (%list_skel  ()) True)
; 

  (= 
    ($list-skel (Cons  $_ $L)) 
    ($list-skel $L))
; 


;
; 


  (= 
    (subseq $AB $A $B) 
    ( ($list-skel $AB) 
      (set-det) 
      ($subseq $AB $A $B)))
; 

  (= 
    (subseq $AB $A $B) 
    ( ($list-skel $A) 
      ($list-skel $B) 
      (set-det) 
      ($subseq $AB $A $B)))
; 

  (= 
    (subseq $AB $A $B) 
    (throw ('instantiation error' $AB $A)))
; 



  (= 
    ($subseq  () () ()) True)
; 

  (= 
    ($subseq 
      (Cons  $X $AB) $A 
      (Cons  $X $B)) 
    ($subseq $AB $A $B))
; 

  (= 
    ($subseq 
      (Cons  $X $AB) 
      (Cons  $X $A) $B) 
    ($subseq $AB $A $B))
; 



  (= 
    (subseq0 $AB $A) 
    ( ($list-skel $AB) 
      (set-det) 
      ($subseq $AB $A $_)))
; 

  (= 
    (subseq0 $AB $A) 
    (throw ('instantiation error' $AB $A)))
; 



  (= 
    (subseq1 $AB $A) 
    ( ($list-skel $AB) 
      (set-det) 
      ($subseq $AB $A $_) 
      (\== $A $AB)))
; 

  (= 
    (subseq1 $AB $A) 
    (throw ('instantiation error' $AB $A)))
; 




  (= 
    (string-append $A $B $C) 
    (string-concat $A $B $C))
; 



  (= 
    (save-predicates $List $Filename) 
    ( (tell $Filename) 
      (listing $List) 
      (told)))
; 


;
; 

;
; 






  (= 
    (prompt $X) 
    (format '~N~w ' 
      (:: $X)))
; 
 


  !(expects-dialect sicstus)
; 



  (= 
    (do-full-kb $KB) 
    ( (clear-kb) 
      (init-kb $KB) 
      (forall 
        (nth-clause do-full-kb1 $Index $_) 
        (do-full-kb $_ $Index))))
; 


  (= 
    (do-full-kb $KB $Index) 
    ( (nth-clause do-full-kb1 $Index $Ref) 
      (clause do-full-kb1 $Goal $Ref) 
      (do-full-kb $KB $Index $Goal)))
; 


  (= 
    (do-full-kb $KB $Index $Goal) 
    (once (, (ignore (, (nonvar $KB) (clear-kb) (init-kb $KB))) (format ~N=================== Nil) (format '~N======= Nth: ~w  ~p  =======' (:: $Index $Goal)) (format ~N===================~n Nil) (show-kb) (catch (do-full-call-each $Goal) $E (, (dumpST) (throw $E))) (show-kb) (format '~N==== DONE: ~w ========~n~n' (:: $Index)))))
; 



  (= 
    (do-full-call-each (, $G1 $G2)) 
    ( (set-det) 
      (do-full-call-each $G1) 
      (do-full-call-each $G2)))
; 

  (= 
    (do-full-call-each (det-if-then $G1 $G2)) 
    ( (not (not (, (wdmsg (do-call (det-if-then $G1 (= $G2 $Vars)))) (my-do-call $G1) (set-det) (term-variables $G2 $Vars) (my-do-call $G2) (set-det) (wdmsg (did-call (+ $Vars)))))) (set-det)))
; 

  (= 
    (do-full-call-each $G1) 
    ( (= $G2 True) 
      (not (not (, (wdmsg (do-call (det-if-then $G1 (= $G2 $Vars)))) (my-do-call $G1) (set-det) (term-variables $G2 $Vars) (my-do-call $G2) (set-det) (wdmsg (did-call (+ $Vars)))))) 
      (set-det)))
; 


;
; 


  (= 
    (my-do-call $G) 
    (notrace (ignore (catch $G $_ True))))
; 



  (= 
    (do-full-kb1) 
    ( (argument-types) 
      (show-kb) 
      (complete-chk) 
      (ip $A) 
      (clause-heads) 
      (eval-examples) 
      (show-kb) 
      (complete-chk) 
      (correct-chk) 
      (fp $A) 
      (refinement $ID $_) 
      (flatten-kb)))
; 
    ;
; 


  (= 
    (do-full-kb1) 
    (det-if-then 
      (intra-construct1 1 2 $A $B $C) 
      (det-if-then 
        (, 
          (show-clauses (:: 1 2 $A $B $C)) 
          (g2-op 1 2 $A $B $C) 
          (show-kb) 
          (identify 4 3 $J)) 
        (det-if-then 
          (, 
            (show-clause $J) 
            (identify 5 $I $J)) 
          (, 
            (show-clause $J) 
            (apply-g2 
              (:: 4 5 10) $A $BB))))))
; 


  (= 
    (do-full-kb1) 
    (det-if-then 
      (, 
        (intra-construct1 10 11 $A $B $C) 
        (show-clauses (:: 10 11 13 14 15)) 
        (g1-op 5 1 $I) 
        (g1-op 5 3 $I) 
        (absorb 5 1 $I) 
        (elem-saturate 5 1 $I) 
        (saturate 5 $I 10) 
        (most-spec-v 5 $I $J) 
        (inv-derivate 5 $J) 
        (show-kb) 
        (lgg 7 9 $J)) 
      (show-clause $J)))
; 


  (= 
    (do-full-kb1) 
    (det-if-then 
      (nr-lgg 7 9 $J) 
      (, 
        (show-clause $J) 
        (get-clause $J $_ $_ $CL $_) 
        (reduce-complete $CL $CL1) 
        (store-clause $_ $CL1 nrlgg $I) 
        (show-clause $I))))
; 


  (= 
    (do-full-kb1) 
    (det-if-then 
      (gen-msg 5 6 $J 10) 
      (det-if-then 
        (, 
          (show-clause $J) 
          (gti 8 9 $J)) 
        (show-clause $J))))
; 


  (= 
    (do-full-kb1) 
    (det-if-then 
      (rlgg 5 6 $J) 
      (show-clause $J)))
; 



  (= 
    (do-full-kb1) 
    (det-if-then 
      (lgg 1 2 $J) 
      (det-if-then 
        (, 
          (show-clause $J) 
          (nr-lgg 1 2 $J)) 
        (det-if-then 
          (, 
            (show-clause $J) 
            (lgg 3 4 $J)) 
          (det-if-then 
            (, 
              (show-clause $J) 
              (nr-lgg 3 4 $J)) 
            (det-if-then 
              (, 
                (show-clause $J) 
                (gti 3 4 $J)) 
              (det-if-then 
                (, 
                  (show-clause $J) 
                  (lgti 3 4 $C $_ $_) 
                  (lgg 8 9 $J)) 
                (det-if-then 
                  (, 
                    (show-clause $J) 
                    (rlgg 8 9 $J)) 
                  (det-if-then 
                    (, 
                      (show-clause $J) 
                      (rlgg 8 9 
                        (cuddly-pet $_) $J)) 
                    (det-if-then 
                      (, 
                        (show-clause $J) 
                        (gen-msg 8 9 $J)) 
                      (det-if-then 
                        (, 
                          (show-clause $J) 
                          (rlgg 10 11 $J)) 
                        (det-if-then 
                          (, 
                            (show-clause $J) 
                            (intra-construct1 14 15 $A $B $C)) 
                          (det-if-then 
                            (, 
                              (show-clauses (:: 14 15 $A $B $C)) 
                              (intra-construct2 16 17 $A $B $C)) 
                            (show-clauses (:: 16 17 $A $B $C)))))))))))))))
; 



;
; 

;
; 

;
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
    (do-full-kb1) 
    (det-if-then 
      (absorb $ID1 $ID2 $J) 
      (det-if-then 
        (, 
          (show-clause $J) 
          (elem-saturate $ID1 $ID2 $J1)) 
        (det-if-then 
          (, 
            (show-clause $J1) 
            (saturate $ID1 $J2 5)) 
          (, 
            (show-clause $J2) 
            (unflatten-kb) 
            (set-det))))))
; 



 

