
  (= 
    (substitution $Term1 $Term2 $Sub_List) 
    ( (implies $Term1 $Term2) 
      (substitute 
        (:: $Term1) 
        (:: $Term2) $Subst1) 
      (sort $Subst1 $Subst2) 
      (remove-id $Subst2 $Sub_List) 
      (set-det)))
; 

       
;                                 (error
;                                   (syntax_error  operator_expected)
;                                   (file  invers/logic.pl 95 8 5563))

     

  (= 
    (skolemize  () $N $N) True)
; 

  (= 
    (skolemize 
      (Cons  $Head $Tail) $N $M) 
    ( (= $Head 
        ($var $N)) 
      (skolemize $Tail 
        (s $N) $M) 
      (set-det)))
; 

  (= 
    (skolemize 
      (Cons  $Head $Tail) $N $M) 
    ( (=.. $Head 
        (Cons  $F $Tail1)) 
      (skolemize $Tail1 $N $O) 
      (skolemize $Tail $O $M)))
; 

     

  (= 
    (substitute Nil Nil Nil) 
    (set-det))
; 

  (= 
    (substitute 
      (Cons  $Head1 $Tail1) 
      (Cons  $Head2 $Tail2) 
      (Cons  
        (/ $Head1 $Head2) $Tail3)) 
    ( (var $Head1) 
      (substitute $Tail1 $Tail2 $Tail3) 
      (set-det)))
; 

  (= 
    (substitute 
      (Cons  $Head1 $Tail1) 
      (Cons  $Head2 $Tail2) $Subst) 
    ( (=.. $Head1 
        (Cons  $F1 $Tail11)) 
      (=.. $Head2 
        (Cons  $F1 $Tail22)) 
      (substitute $Tail11 $Tail22 $Tail33) 
      (substitute $Tail1 $Tail2 $Tail4) 
      (append $Tail33 $Tail4 $Subst) 
      (set-det)))
; 



  (= 
    (remove-id Nil Nil) 
    (set-det))
; 

  (= 
    (remove-id 
      (:: $X) 
      (:: $X)) 
    (set-det))
; 

  (= 
    (remove-id 
      (Cons  $A 
        (Cons  $B $Tail)) $List) 
    ( (== $A $B) 
      (remove-id 
        (Cons  $A $Tail) $List) 
      (set-det)))
; 

  (= 
    (remove-id 
      (Cons  $Head1 $Tail1) 
      (Cons  $Head1 $Tail2)) 
    ( (remove-id $Tail1 $Tail2) (set-det)))
; 



  (= 
    (lgg $Term1 $Term2 $Term3) 
    ( (lgg1 
        (:: $Term1) 
        (:: $Term2) 
        (:: $Term3) Nil $Subst) (set-det)))
; 

   

  (= 
    (lgg1  () () () $Subst $Subst) True)
; 

  (= 
    (lgg1 
      (Cons  $Head1 $Tail1) 
      (Cons  $Head2 $Tail2) 
      (Cons  $Head3 $Tail3) $Subst1 $Subst3) 
    ( (=.. $Head1 
        (Cons  $F $Tail11)) 
      (=.. $Head2 
        (Cons  $F $Tail22)) 
      (lgg1 $Tail11 $Tail22 $Tail33 $Subst1 $Subst2) 
      (=.. $Head3 
        (Cons  $F $Tail33)) 
      (lgg1 $Tail1 $Tail2 $Tail3 $Subst2 $Subst3) 
      (set-det)))
; 

  (= 
    (lgg1 
      (Cons  $Head1 $Tail1) 
      (Cons  $Head2 $Tail2) 
      (Cons  $Head3 $Tail3) $Subst1 $Subst2) 
    ( (subst-member 
        (/ $Head3 
          (, $Head1 $Head2)) $Subst1) 
      (lgg1 $Tail1 $Tail2 $Tail3 $Subst1 $Subst2) 
      (set-det)))
; 

  (= 
    (lgg1 
      (Cons  $Head1 $Tail1) 
      (Cons  $Head2 $Tail2) 
      (Cons  $Head3 $Tail3) $Subst1 $Subst2) 
    ( (lgg1 $Tail1 $Tail2 $Tail3 
        (Cons  
          (/ $Head3 
            (, $Head1 $Head2)) $Subst1) $Subst2) (set-det)))
; 



  (= 
    (subst-member 
      (/ $A $B) 
      (Cons  
        (/ $A $C) $_)) 
    ( (== $B $C) (set-det)))
; 

  (= 
    (subst-member $A 
      (Cons  $_ $B)) 
    ( (subst-member $A $B) (set-det)))
; 



  (= 
    (covers  () $_) True)
; 

  (= 
    (covers 
      (Cons  $H1 $T1) $P) 
    ( (member $C1 $P) 
      (copy $C1 
        (= $H1 $B1)) 
      (covers-body $B1 $P) 
      (covers $T1 $P)))
; 



  (= 
    (covers_body  true $_) True)
; 

  (= 
    (covers-body 
      (, $H $B) $P) 
    ( (set-det) 
      (covers 
        (:: $H) $P) 
      (covers-body $B $P)))
; 

  (= 
    (covers-body $H $P) 
    (covers 
      (:: $H) $P))
; 

    

  (= 
    (copy $A $B) 
    (or 
      (, 
        (add-symbol  &self 
          (yap_inst  $A)) 
        (remove-symbol  &self 
          (yap_inst  $B)) 
        (set-det)) 
      (, 
        (remove-symbol  &self 
          (yap_inst  $_)) 
        (fail))))
; 



  (= 
    (psubsumes $_ Nil) 
    (set-det))
; 

  (= 
    (psubsumes $P 
      (Cons  $C $T)) 
    ( (bsubsumes $P $C) 
      (psubsumes $P $T) 
      (set-det)))
; 


;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  invers/logic.pl 229 7 11814))

	     

  (= 
    (body-units True Nil) 
    (set-det))
; 

  (= 
    (body-units 
      (, $Head1 $Tail1) 
      (Cons  
        (= $Head1 True) $Tail2)) 
    ( (body-units $Tail1 $Tail2) (set-det)))
; 

  (= 
    (body_units  $Head 
      ( (:-  $Head true))) True)
; 



  (= 
    (goal_units  () ()) True)
; 

  (= 
    (goal-units 
      (Cons  $Head1 $Tail1) 
      (Cons  
        (= $Head1 True) $Tail2)) 
    ( (goal-units $Tail1 $Tail2) (set-det)))
; 



  (= 
    (p-subsumes $ClauseSet1 $Theory $ClauseSet2) 
    ( (append $ClauseSet1 $Theory $Program) (psubsumes $Program $ClauseSet2)))
; 

     

  (= 
    (p-equivalent $ClauseSet1 $Theory $ClauseSet2) 
    ( (append $ClauseSet1 $Theory $Program1) 
      (psubsumes $Program1 $ClauseSet2) 
      (append $ClauseSet2 $Theory $Program2) 
      (psubsumes $Program2 $ClauseSet1)))
; 

     		    


  (= 
    (help) 
    ( (write 'Load example calls with command: [Filename].') 
      (nl) 
      (write 'Call examples with: test1, test2, test3a, test3b,') 
      (nl) 
      (write '                    test4a, test4b, test4c, test4d, test5') 
      (nl)))
; 



  !(help *)
; 




