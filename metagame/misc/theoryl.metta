;
; 

;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 


;
; 


;
; 


  !(op 1200 xfy ::)
; 



  (= 
    (theory-clause $G) 
    (theory-clause $G True $Id))
; 


  (= 
    (theory-clause $G $H) 
    (theory-clause $G $H $Id))
; 


;
; 


  (= 
    (theory-clause $G $H $Id) 
    ( (var $Id) 
      (set-det) 
      (theory-clause0 $G $H $Id)))
; 

  (= 
    (theory-clause $G $H 
      (ith-converse $I $Id)) 
    ( (set-det) 
      (theory-clause0 $H $Gs $Id) 
      (ith-and-member $I $G $Gs)))
; 

  (= 
    (theory-clause $G $H $Id) 
    ( (set-det) (theory-clause0 $G $H $Id)))
; 



  (= 
    (theory-clause0 $A $B $Id) 
    ( (:: $Id $C) (det-if-then-else (= (= $A $B) $C) True (det-if-then otherwise (= (= $A $B) (= $C True))))))
; 



  (= 
    (ith-and-member 0 $A 
      (, $A $_)) 
    (set-det))
; 

  (= 
    (ith-and-member 0 $A $A) 
    (set-det))
; 

  (= 
    (ith-and-member $I $A 
      (, $_ $B)) 
    ( (> $I 0) 
      (is $I1 
        (- $I 1)) 
      (ith-and-member $I1 $A $B)))
; 



  (= 
    (system-predicate $Goal) 
    ( (functor $Goal $F $N) 
      (functor $PredSpec $F $N) 
      (not (theory-clause $PredSpec $_ $_))))
; 




;
; 

;
; 



  !(dynamic (/ next-clause-id 1))
; 


  (= 
    (next_clause_id  0) True)
; 


;
; 

;
; 



  (= 
    (theory-assert $Clause) 
    (theory-assert $Clause $_))
; 



;
; 

;
; 


  (= 
    (theory-assert $Clause 
      (new $N)) 
    ( (clause-parts $Clause $G $H) 
      (remove-symbol  &self 
        (next_clause_id  $N)) 
      (is $N1 
        (+ $N 1)) 
      (add-symbol  &self 
        (next_clause_id  $N1)) 
      (add-symbol  &self 
        (::  
          (new  $N) 
          (:-  $G $H)))))
; 



;
; 

;
; 

;
; 



  (= 
    (theory-assert-az $Clause 
      (new $N) $AZ) 
    ( (clause-parts $Clause $G $H) 
      (remove-symbol  &self 
        (next_clause_id  $N)) 
      (is $N1 
        (+ $N 1)) 
      (add-symbol  &self 
        (next_clause_id  $N1)) 
      (assertaz $AZ 
        (:: 
          (new $N) 
          (= $G $H)))))
; 



  (= 
    (assertaz a $C) 
    (add-symbol  &self $C))
; 

  (= 
    (assertaz z $C) 
    (add-symbol  &self $C))
; 




  (= 
    (clause-parts 
      (= $G $H) $G $H) 
    (set-det))
; 

  (= 
    (clause-parts $G $G True) 
    (not (functor $G :- 2)))
; 


;
; 



  (= 
    (new_id  
      (new  $_)) True)
; 



;
; 



  (= 
    (theory-retract $Clause $Id) 
    ( (clause-parts $Clause $G $H) (remove-symbol  &self (::  $Id (:-  $G $H)))))
; 


  (= 
    (theory-retract $Clause) 
    (theory-retract $Clause $Id))
; 



  (= 
    (theory-clear) 
    (remove-all-symbols  &self 
      (::  $Id $Clause)))
; 




  (= 
    (theory-listing) 
    (whenever 
      (theory-clause $C) 
      (portray-clause $C)))
; 


