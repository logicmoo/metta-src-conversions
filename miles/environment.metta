;
; 



  !(module environment 
    (:: 
      (/ oracle 1) 
      (/ oracle 2) 
      (/ oracle 3) 
      (/ satisfiable 1) 
      (/ ask-for 1) 
      (/ ask-for-ex 1) 
      (/ confirm 2) 
      (/ get-ci 2)))
; 



;
; 

  !(use-module 
    (home kb) 
    (:: 
      (/ get-clause 5) 
      (/ delete-clause 1) 
      (/ store-clause 4) 
      (/ interpretable-predicate 1) 
      (/ get-example 3) 
      (/ store-ex 3) 
      (/ rename 3) 
      (/ delete-all 1)))
; 

  !(use-module 
    (home show-utils) 
    (:: 
      (/ show-clauses 1) 
      (/ show-names 0)))
; 

  !(use-module-if-exists 
    (library prompt) 
    (:: (/ prompt 1)))
; 

  !(use-module-if-exists 
    (library ask) 
    (:: 
      (/ yesno 1) 
      (/ yesno 2) 
      (/ ask-chars 4)))
; 

  !(use-module-if-exists 
    (library sets) 
    (:: (/ union 3)))
; 

  !(use-module-if-exists 
    (library subsumes) 
    (:: (/ subsumes-chk 2)))
; 


;
; 

;
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (satisfiable  ()) True)
; 

  (= 
    (satisfiable (Cons  (:: $_ $H $_) $R)) 
    ( (ask-for $H) (satisfiable $R)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (ask-for $Lit) 
    (det-if-then-else 
      (interpretable-predicate $Lit) 
      (det-if-then-else 
        (or 
          (get-clause $_ $Lit True $_ $_) 
          (get-example $_ $Lit +)) True 
        (det-if-then-else 
          (get-example $_ $Lit -) fail 
          (det-if-then-else 
            (ground $Lit) 
            (det-if-then-else 
              (oracle $Lit) 
              (store-ex $Lit + $_) 
              (, 
                (store-ex $Lit - $_) 
                (fail))) fail))) $Lit))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (ask-for-ex $Lit) 
    (det-if-then-else 
      (interpretable-predicate $Lit) 
      (det-if-then-else 
        (or 
          (get-clause $_ $Lit True $_ $_) 
          (get-example $_ $Lit +)) True 
        (det-if-then-else 
          (get-example $_ $Lit -) fail 
          (det-if-then-else 
            (oracle $Lit $Lit) 
            (store-ex $Lit + $_) fail))) $Lit))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (term-help) 
    (prompt 'Please enter a proper PROLOG-term followed by a full-stop and RETURN'))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (oracle $Lit) 
    ( (nl) 
      (prompt 'Is the following literal always true:') 
      (nl) 
      (nl) 
      (portray-clause $Lit) 
      (nl) 
      (set-det) 
      (yesno '> (y/n) ')))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (oracle 
      (Cons  $Id1 $Rest) $NegexId) 
    ( (nl) 
      (prompt 'Are the following clauses always true:') 
      (nl) 
      (show-clauses (Cons  $Id1 $Rest)) 
      (set-det) 
      (det-if-then-else 
        (yesno '> (y/n) ') True 
        (det-if-then 
          (yesno '> Would You like to give a counter-example' no) 
          (, 
            (repeat) 
            (prompt '> Please enter negative example as Prolog-term: ') 
            (read $Ex) 
            (det-if-then-else 
              (= $Ex h) 
              (, 
                (term-help) 
                (fail)) 
              (, 
                (store-ex $Ex - $NegexId) 
                (set-det))))))))
; 

           

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (oracle $Pname $Newname) 
    ( (functor $Pname $_ 0) 
      (nl) 
      (prompt 'How would You like to call predicate ') 
      (write $Pname) 
      (write  ?) 
      (set-det) 
      (repeat) 
      (ask-chars '> Please enter a name or "list" followed by RETURN' 1 40 $A1) 
      (atom-chars $A2 $A1) 
      (det-if-then-else 
        (== $A2 list) 
        (, 
          (prompt 'So far the following predicates have been defined in the knowledge-base:') 
          (nl) 
          (show-names) 
          (fail)) 
        (, 
          (= $Newname $A2) 
          (set-det)))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (oracle $Lit $InstLit) 
    ( (nl) 
      (prompt 'Is there a correct instance of the following literal:') 
      (nl) 
      (nl) 
      (portray-clause $Lit) 
      (nl) 
      (set-det) 
      (yesno '> (y/n) ') 
      (repeat) 
      (prompt '> Please enter an instance: ') 
      (read $InstLit) 
      (det-if-then-else 
        (= $InstLit h) 
        (, 
          (term-help) 
          (fail)) 
        (det-if-then-else 
          (subsumes-chk $Lit $InstLit) 
          (set-det) 
          (, 
            (prompt 'This is no instantiation of the literal!') 
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


  (= 
    (oracle $Question $Default $Answer) 
    ( (atom-chars $Question $Qlist) 
      (append 
        (:: 62 32) $Qlist $P1list) 
      (det-if-then-else 
        (var $Default) 
        (atom-chars $Prompt $P1list) 
        (, 
          (atom-chars $Default $Dlist) 
          (append $Dlist 
            (:: 93) $D1list) 
          (append $P1list 
            (Cons  32 
              (Cons  91 $D1list)) $P2list) 
          (atom-chars $Prompt $P2list))) 
      (nl) 
      (ask-chars $Prompt 0 255 $Alist) 
      (det-if-then-else 
        (, 
          (== $Alist Nil) 
          (nonvar $Default)) 
        (= $Answer $Default) 
        (atom-chars $Answer $Alist))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (confirm $Clause_ids $L) 
    ( (oracle $Clause_ids $Ex) 
      (var $Ex) 
      (functor $L $Oldname $_) 
      (oracle $Oldname $Newname) 
      (rename $Clause_ids $Oldname $Newname) 
      (set-det)))
; 


  (= 
    (confirm $Clause_ids $_) 
    ( (delete-all $Clause_ids) 
      (nl) 
      (write 'New clauses deleted.') 
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



  (= 
    (get-ci $Sofar $CC) 
    ( (oracle 'Please enter a resolvent ID followed by RETURN' stop $Answer) 
      (\== $Answer stop) 
      (atom-chars $Answer $Idc) 
      (det-if-then-else 
        (, 
          (number-chars $Id $Idc) 
          (union $Sofar 
            (:: $Id) $Sofarnew)) True 
        (= $Sofarnew $Sofar)) 
      (set-det) 
      (get-ci $Sofarnew $CC)))
; 

  (= 
    (get-ci $CC $CC) 
    (set-det))
; 
	

