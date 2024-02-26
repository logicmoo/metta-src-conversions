;                                           (convert-to-metta-file  theoryl $_120994 metagame/misc/theoryl.pl metagame/misc/theoryl.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; 
;
; This file is thanks to William Cohen, who may reserve the
;
; copyright.
;
; ============================================================

;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ; theoryl.pl -- routines to manipulate the domain theory
;
; ;
;
; ; external routines:
;
; ; theory_clause(?Head,?Body,?Id) :- like Quintus clause/3
;
; ; theory_assert(+Clause,-Id) :- like Quintus assert/2
;
; ;
;
; ; syntax: domain theory is a set of labeled clauses of the form "tag::A:-B"
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; ; syntax for theory clauses

;
; :- current_op(P,T,(:-)),op(P,xfy,(::)).

  !(op 1200 xfy ::)


  (= (theory-clause $G)  
    (theory-clause $G True $Id))

  (= (theory-clause $G $H)  
    (theory-clause $G $H $Id))

;
; ; theory_clause(A,B,Id) :- clause A:-B with identifier Id is in the theory

  (= (theory-clause $G $H $Id)  
    (var $Id)
    (set-det)
    (theory-clause0 $G $H $Id))
  (= (theory-clause $G $H (ith-converse $I $Id))  
    (set-det)
    (theory-clause0 $H $Gs $Id)
    (ith-and-member $I $G $Gs))
  (= (theory-clause $G $H $Id)  
    (set-det)
    (theory-clause0 $G $H $Id))


  (= (theory-clause0 $A $B $Id)  
    (:: $Id $C)
    (det-if-then-else 
      (= 
        (= $A  $B) $C) True 
      (det-if-then otherwise 
        (= 
          (= $A  $B) 
          (= $C  True)))))


  (= (ith-and-member 0 $A (, $A $_))  
    (set-det))
  (= (ith-and-member 0 $A $A)  
    (set-det))
  (= (ith-and-member $I $A (, $_ $B))  
    (> $I 0)
    (is $I1 
      (- $I 1))
    (ith-and-member $I1 $A $B))


  (= (system-predicate $Goal)  
    (functor $Goal $F $N)
    (functor $PredSpec $F $N)
    (not (theory-clause $PredSpec $_ $_)))



;
; ; theory_assert(+Clause,?Id) :- create a new Horn clause
;
; ; of the form (G:-H) and asign it id Idnew


  !(dynamic (/ next-clause-id 1))

  (= (next-clause-id  0)  True)

;
; Default to assertz.
;
; theory_assert(Clause) :- theory_assert_az(Clause,_,z).


  (= (theory-assert $Clause)  
    (theory-assert $Clause $_))


;
; ; theory_assert[az](+Clause,?Id) :- create a new Horn clause
;
; ; of the form (G:-H) and asign it the id Idnew

  (= (theory-assert $Clause (new $N))  
    ( (clause-parts $Clause $G $H) 
      (remove-atom  &self 
        (next-clause-id  $N)) 
      (is $N1 
        (+ $N 1)) 
      (add-atom  &self 
        (next-clause-id  $N1)) 
      (add-atom  &self 
        (::  
          (new  $N) 
          (:-  $G $H)))))


;
; ; theory_assert_az(+Clause,?Id,AZ) :- create a new Horn clause
;
; ; of the form (G:-H) and asign it the id Idnew
;
; ; If AZ = a, asserta, if z, assertz.


  (= (theory-assert-az $Clause (new $N) $AZ)  
    ( (clause-parts $Clause $G $H) 
      (remove-atom  &self 
        (next-clause-id  $N)) 
      (is $N1 
        (+ $N 1)) 
      (add-atom  &self 
        (next-clause-id  $N1)) 
      (assertaz $AZ 
        (:: 
          (new $N) 
          (= $G  $H)))))


  (= (assertaz a $C)  
    (add-atom  &self $C))
  (= (assertaz z $C)  
    (add-atom  &self $C))



  (= (clause-parts (= $G  $H) $G $H)  
    (set-det))
  (= (clause-parts $G $G True)  
    (not (functor $G :- 2)))

;
; ; new_id(+Id) :- test if Id is recently asserted


  (= (new-id  (new  $_))  True)


;
; ; theory_retract(?(G:-H)) :- retract a Horn clause


  (= (theory-retract $Clause $Id)  
    ( (clause-parts $Clause $G $H) (remove-atom  &self (::  $Id (:-  $G $H)))))

  (= (theory-retract $Clause)  
    (theory-retract $Clause $Id))


  (= theory-clear  
    (remove-all-atoms  &self 
      (::  $Id $Clause)))



  (= theory-listing  
    (whenever 
      (theory-clause $C) 
      (portray-clause $C)))

