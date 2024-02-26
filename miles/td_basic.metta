;                                           (convert-to-metta-file  td_basic $_497458 miles/td_basic.pl miles/td_basic.metta)
;
; MODULE td_basic EXPORTS

  !(module td-basic 
    (:: 
      (/ distribute-vars 3) 
      (/ vars-of-type 3) 
      (/ enumerate-t 3) 
      (/ append-body 3)))

;
; IMPORTS
  !(use-module 
    (home argument-types) 
    (:: (/ type-sub 2)))

;
; METAPREDICATES
;
; none


;
; ***********************************************************************
;
; *
;
; * module: td_basic.pl
;
; *
;
; * author: I.Stahl date:12/92
;
; *
;
; * changed:
;
; *
;
; * description: basics for top-down induction
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************



;
; ***********************************************************************
;
; *
;
; * predicate: append_body/3
;
; *
;
; * syntax: append_body(+Clause,+Literal,-Clause1)
;
; *
;
; * args: Clause,Clause1.. MeTTa clauses
;
; *
;
; * description: adds Literal to the end of the body of Clause
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (append-body (= $H  True) $B (= $H  $B))  
    (set-det))
  (= (append-body (= $H  $B) $C (= $H  $B1))  
    (set-det)
    (append-body $B $C $B1))
  (= (append-body (, $A $B) $C (, $A $D))  
    (set-det)
    (append-body $B $C $D))
  (= (append-body  $A $B (,  $A $B))  True)


;
; ***********************************************************************
;
; *
;
; * predicate: distribute_vars/3
;
; *
;
; * syntax: distribute_vars(+PVars,+Terms,-DVars)
;
; *
;
; * args: PVars = [X:Tx|R]: terms X with types Tx in the new literal P
;
; * Terms: all terms with their types in the clause C to be refined
;
; *
;
; * DVars = [X:Vx,...]: for each X in PVars a list of all type-matching
;
; * variables Vx in Terms + an additional new variable
;
; *
;
; * description: computes DVars
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (distribute-vars  () $_ ())  True)
  (= (distribute-vars (Cons  (with-self  $X $Tx) $R) $V (Cons  (with-self  $X $Vx) $R1))  
    (distribute-vars $R $V $R1)
    (vars-of-type $V $Tx $Vx))


;
; ***********************************************************************
;
; *
;
; * predicate: vars_of_type/3
;
; *
;
; * syntax: vars_of_type(+Terms,+Ty,-R2)
;
; *
;
; * args: Terms= [X:Tx|_]: terms X with types Tx in the clause C to be refined
;
; * Ty: type Ty of an argument of the new literal
;
; * R2: a list of all terms in C matching type Ty
;
; *
;
; * description: adds a term X of Terms to R2 if type Ty subsumes type
;
; * of X or vice versa
;
; * and one new term (last element in R2)
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (vars-of-type  () $_ ($_))  True)
  (= (vars-of-type (Cons  (with-self  $X $Tx) $R) $Ty $R2)  
    (vars-of-type $R $Ty $R1)
    (det-if-then-else 
      (or 
        (type-sub $Ty $Tx) 
        (type-sub $Tx $Ty)) 
      (= $R2 
        (Cons  $X $R1)) 
      (= $R2 $R1)))


;
; ***********************************************************************
;
; *
;
; * predicate: enumerate_t/3
;
; *
;
; * syntax: enumerate_t(+DVars,+PL,-PL2)
;
; *
;
; * args: DVars = [X:Vx,...]: all variables X in the new literal
;
; * with their type-matching variables in C
;
; * PL: initial predicate list
;
; *
;
; * PL2: predicate list
;
; *
;
; * description: computes predicates P where variables in Vx are unified to X in P
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (enumerate-t  () $PL $PL)  True)
  (= (enumerate-t (Cons  (with-self  $X $Vx) $R) $PL $PL2)  
    (enumerate-t $R $PL $PL1)
    (et $Vx $X $PL1 $PL2))


;
; ***********************************************************************
;
; *
;
; * predicate: et/4
;
; *
;
; * syntax: et(+Vx,+X,+PL,-PL3)
;
; *
;
; * args: Vx: a list of variables to be unified with X
;
; * X: a term of P to be unified by a variable of Vx
;
; * PL: initial predicate list
;
; *
;
; * PL3: predicate list
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (et  () $_ $_ ())  True)
  (= (et (Cons  $Y $R) $X $PL $PL3)  
    (et $R $X $PL $PL1)
    (etx $PL $X $Y $PL2)
    (append $PL2 $PL1 $PL3))


;
; ***********************************************************************
;
; *
;
; * predicate: etx/4
;
; *
;
; * syntax: etx(PL,X,Y,R2)
;
; *
;
; * args: PL: literals of P
;
; * X: a term of P to be unified by Y
;
; * Y: a term of the clause C to be unfied with X
;
; *
;
; * R2: predicate list where X and Y are unified
;
; *
;
; * description: (if Y is not in args(P) then ) "unify" X and Y. This is
;
; * done by copying P and replacing X by Y.
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (etx  () $_ $_ ())  True)
  (= (etx (Cons  $P $R) $X $Y $R2)  
    (etx $R $X $Y $R1)
    (functor $P $F $N)
    (functor $P1 $F $N)
    (etx1 $N $P $P1 $X $Y)
    (= $R2 
      (Cons  $P1 $R1)))



;
; ***********************************************************************
;
; *
;
; * predicate: etx1/5
;
; *
;
; * syntax: etx1(+N,+P,-P1,+X,+Y)
;
; *
;
; * args: N: arity of literal P
;
; * P: literal to be added
;
; * X: variable to be replaced by Y
;
; * Y: variable
;
; *
;
; * description: replaces X by Y in the copy P1 of P
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (etx1 0 $_ $_ $_ $_)  
    (set-det))
  (= (etx1 $N $P $P1 $X $Y)  
    (is $N1 
      (- $N 1))
    (etx1 $N1 $P $P1 $X $Y)
    (arg $N $P $Pn)
    (det-if-then-else 
      (== $Pn $X) 
      (arg $N $P1 $Y) 
      (arg $N $P1 $Pn)))




