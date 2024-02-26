;
;                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                      Builtin Predicates of MeTTa Cafe

;
; 

;
;                      Mutsunori Banbara (banbara@kobe-u.ac.jp)

;
;                       Naoyuki Tamura (tamura@kobe-u.ac.jp)

;
;                       Kobe University

;
;                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  !(op 1150 fx package)

  (= 
    (package  $_) True)

  !(package TauMachine.builtin)

;
;                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                          Control constructs

;
;                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  !(public (, (/ True 0) (/ otherwise 0)))
  !(public (, (/ fail 0) (/ False 0)))
  !(public (/ (set-det) 0))
  !(public (/ ^ 2))
  !(public (/ , 2))
  !(public (/ or 2))
  !(public (/ -> 2))
  !(public (/ call 1))


  (= true True)

  (= otherwise True)


  (= fail 
    (empty))

  (= false 
    (empty))


  (= ! True)


  (= 
    (^ $_ $G) 
    (call $G))


  (= 
    (, $P $Q) 
    ( (call $P) (call $Q)))


  (= 
    (or $P $Q) 
    ( (\= $P 
        (det-if-then $_ $_)) (call $P)))
  (= 
    (or $P $Q) 
    ( (\= $Q 
        (det-if-then $_ $_)) (call $Q)))


  (= 
    (det-if-then $IF $THEN) 
    ( (call $IF) 
      (set-det) 
      (call $THEN)))


  (= 
    (det-if-then-else $IF $THEN $ELSE) 
    ( (call $IF) 
      (set-det) 
      (call $THEN)))
  (= 
    (det-if-then-else $IF $THEN $ELSE) 
    (call $ELSE))


  (= 
    (call $Term) 
    ( ($get-current-B $Cut) ($meta-call $Term user $Cut 0 interpret)))


  (= 
    ($meta-call $X $_ $_ $_ $_) 
    ( (var $X) 
      (set-det) 
      (illarg var 
        (call $X) 1)))
  (= 
    ($meta-call $X $_ $_ $_ $_) 
    ( (closure $X) 
      (set-det) 
      ($call-closure $X)))
  (= 
    ($meta-call True $_ $_ $_ $_) 
    (set-det))
  (= 
    ($meta-call trace $_ $_ $_ $_) 
    ( (set-det) (trace)))
  (= 
    ($meta-call debug $_ $_ $_ $_) 
    ( (set-det) (debug)))
  (= 
    ($meta-call notrace $_ $_ $_ $_) 
    ( (set-det) (notrace)))
  (= 
    ($meta-call nodebug $_ $_ $_ $_) 
    ( (set-det) (nodebug)))
  (= 
    ($meta-call 
      (spy $L) $_ $_ $_ $_) 
    ( (set-det) (spy $L)))
  (= 
    ($meta-call 
      (nospy $L) $_ $_ $_ $_) 
    ( (set-det) (nospy $L)))
  (= 
    ($meta-call nospyall $_ $_ $_ $_) 
    ( (set-det) (nospyall)))
  (= 
    ($meta-call 
      (leash $L) $_ $_ $_ $_) 
    ( (set-det) (leash $L)))
  (= 
    ($meta-call 
      (Cons  $X $Xs) $_ $_ $_ $_) 
    ( (set-det) (consult (Cons  $X $Xs))))
  (= 
    ($meta-call 
      (^ $_ $X) $P $Cut $Depth $Mode) 
    ( (set-det) ($meta-call $X $P $Cut $Depth $Mode)))
  (= 
    ($meta-call 
      (with_self  $P $X) $_ $Cut $Depth $Mode) 
    ( (set-det) ($meta-call $X $P $Cut $Depth $Mode)))
  (= 
    ($meta-call 
      (set-det) $_ no $_ $_) 
    ( (set-det) (illarg (context if cut) (set-det) 0)))
  (= 
    ($meta-call 
      (set-det) $_ $Cut $_ $_) 
    ( (set-det) ($cut $Cut)))
  (= 
    ($meta-call 
      (, $X $Y) $P $Cut $Depth $Mode) 
    ( (set-det) 
      ($meta-call $X $P $Cut $Depth $Mode) 
      ($meta-call $Y $P $Cut $Depth $Mode)))
  (= 
    ($meta-call 
      (det-if-then-else $X $Y $Z) $P $Cut $Depth $Mode) 
    ( (set-det) (det-if-then-else ($meta-call $X $P no $Depth $Mode) ($meta-call $Y $P $Cut $Depth $Mode) ($meta-call $Z $P $Cut $Depth $Mode))))
  (= 
    ($meta-call 
      (det-if-then $X $Y) $P $Cut $Depth $Mode) 
    ( (set-det) (det-if-then ($meta-call $X $P no $Depth $Mode) ($meta-call $Y $P $Cut $Depth $Mode))))
  (= 
    ($meta-call 
      (or $X $Y) $P $Cut $Depth $Mode) 
    ( (set-det) (or ($meta-call $X $P $Cut $Depth $Mode) ($meta-call $Y $P $Cut $Depth $Mode))))
  (= 
    ($meta-call 
      (not $X) $P $_ $Depth $Mode) 
    ( (set-det) (not ($meta-call $X $P no $Depth $Mode))))
  (= 
    ($meta-call 
      (findall $X $Y $Z) $P $Cut $Depth $Mode) 
    ( (set-det) (findall $X ($meta-call $Y $P $Cut $Depth $Mode) $Z)))
  (= 
    ($meta-call 
      (bagof $X $Y $Z) $P $Cut $Depth $Mode) 
    ( (set-det) (bagof $X ($meta-call $Y $P $Cut $Depth $Mode) $Z)))
  (= 
    ($meta-call 
      (setof $X $Y $Z) $P $Cut $Depth $Mode) 
    ( (set-det) (setof $X ($meta-call $Y $P $Cut $Depth $Mode) $Z)))
  (= 
    ($meta-call 
      (once $X) $P $Cut $Depth $Mode) 
    ( (set-det) (once ($meta-call $X $P $Cut $Depth $Mode))))
  (= 
    ($meta-call 
      (on-exception $X $Y $Z) $P $Cut $Depth $Mode) 
    ( (set-det) (on-exception $X ($meta-call $Y $P $Cut $Depth $Mode) ($meta-call $Z $P $Cut $Depth $Mode))))
  (= 
    ($meta-call 
      (catch $X $Y $Z) $P $Cut $Depth $Mode) 
    ( (set-det) (catch ($meta-call $X $P $Cut $Depth $Mode) $Y ($meta-call $Z $P $Cut $Depth $Mode))))
;
;                                         '$meta_call'(freeze(X,Y), P, Cut, Depth, Mode) :- !, ???

;
;                                         	freeze(X, '$meta_call'(Y, P, Cut, Depth, Mode)).

  (= 
    ($meta-call 
      (synchronized $X $Y) $P $Cut $Depth $Mode) 
    ( (set-det) (synchronized $X ($meta-call $Y $P $Cut $Depth $Mode))))
  (= 
    ($meta-call 
      (get-atoms  &self 
        (= $X $Y)) $P $_ $_ $_) 
    ( (set-det) (get-atoms  &self (= (:  $P $X) $Y))))
  (= 
    ($meta-call 
      (add-atom  &self $X) $P $_ $_ $_) 
    ( (set-det) (add-atom  &self (:  $P $X))))
  (= 
    ($meta-call 
      (add-atom  &self $X) $P $_ $_ $_) 
    ( (set-det) (add-atom  &self (:  $P $X))))
  (= 
    ($meta-call 
      (add-atom  &self $X) $P $_ $_ $_) 
    ( (set-det) (add-atom  &self (:  $P $X))))
  (= 
    ($meta-call 
      (remove-atom  &self $X) $P $_ $_ $_) 
    ( (set-det) (remove-atom  &self (:  $P $X))))
  (= 
    ($meta-call 
      (abolish $X) $P $_ $_ $_) 
    ( (set-det) (abolish (with_self  $P $X))))
  (= 
    ($meta-call 
      (remove-all-atoms  &self $X) $P $_ $_ $_) 
    ( (set-det) (remove-all-atoms  &self (:  $P $X))))
  (= 
    ($meta-call $X $P $_ $Depth $Mode) 
    ( (atom $P) 
      (callable $X) 
      (set-det) 
      ($meta-call $Mode $Depth $P $X)))
  (= 
    ($meta-call $X $P $_ $_ $_) 
    (illarg 
      (type callable) 
      (call (with_self  $P $X)) 1))

  (= 
    ($meta-call trace $Depth $P $X) 
    ( (set-det) 
      (functor $X $F $A) 
      ($trace-goal $X $P 
        (/ $F $A) $Depth)))
  (= 
    ($meta-call interpret $Depth $P $X) 
    ( (functor $X $F $A) ($call-internal $X $P (/ $F $A) $Depth interpret)))


  (= 
    ($call-internal $X $P $FA $Depth $Mode) 
    ( ($new-internal-database $P) 
      (hash-contains-key $P $FA) 
      (set-det) 
      ($get-current-B $Cut) 
      (is $Depth1 
        (+ $Depth 1)) 
      (get-atoms  &self 
        (= 
          (:  $P $X) $Body)) 
      ($meta-call $Body $P $Cut $Depth1 $Mode)))
  (= 
    ($call-internal $X $P $_ $_ $_) 
    ($call $P $X))



  !(public (, (/ catch 3) (/ throw 1)))
  !(public (/ on-exception 3))


  (= 
    (catch $Goal $Catch $Recovery) 
    (on-exception $Catch $Goal $Recovery))


  (= 
    (throw $Msg) 
    (raise-exception $Msg))


  (= 
    (on-exception $Catch $Goal $Recovery) 
    ( (callable $Goal) 
      (set-det) 
      ($on-exception $Catch $Goal $Recovery)))
  (= 
    (on-exception $Catch $Goal $Recovery) 
    (illarg 
      (type callable) 
      (on-exception $Catch $Goal $Recovery) 2))


  (= 
    ($on-exception $Catch $Goal $Recovery) 
    ( ($set-exception %none) 
      ($begin-exception $L) 
      (call $Goal) 
      ($end-exception $L)))
  (= 
    ($on-exception $Catch $Goal $Recovery) 
    ( ($get-exception $Msg) 
      (\== $Msg %none) 
      ($catch-and-throw $Msg $Catch $Recovery)))


  (= 
    ($catch-and-throw $Msg $Msg $Recovery) 
    ( (set-det) 
      ($set-exception %none) 
      (call $Recovery)))
  (= 
    ($catch-and-throw $Msg $_ $_) 
    (raise-exception $Msg))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          Term unification

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  !(public (, (/ = 2) (/ %unify 2)))
  !(public (, (/ \= 2) (/ %not-unifiable 2)))


  (= 
    (= $X $Y) 
    (= $X $Y))

  (= 
    ($unify $X $Y) 
    ($unify $X $Y))


  (= 
    (\= $X $Y) 
    (\= $X $Y))

  (= 
    ($not-unifiable $X $Y) 
    ($not-unifiable $X $Y))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          Type testing

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  !(public (, (/ var 1) (/ is-symbol 1) (/ integer 1) (/ long 1) (/ float 1) (/ symbolic 1) (/ compound 1) (/ nonvar 1) (/ number 1)))
  !(public (, (/ java 1) (/ java 2) (/ closure 1)))
  !(public (, (/ ground 1) (/ callable 1)))


  (= 
    (var $X) 
    (var $X))


  (= 
    (atom $X) 
    (atom $X))


  (= 
    (integer $X) 
    (integer $X))


  (= 
    (long $X) 
    (long $X))


  (= 
    (float $X) 
    (float $X))


  (= 
    (atomic $X) 
    (atomic $X))


  (= 
    (nonvar $X) 
    (nonvar $X))


  (= 
    (number $X) 
    (number $X))


  (= 
    (java $X) 
    (java $X))
  (= 
    (java $X $Y) 
    (java $X $Y))


  (= 
    (closure $X) 
    (closure $X))


  (= 
    (ground $X) 
    (ground $X))


  (= 
    (compound $X) 
    ( (nonvar $X) 
      (functor $X $_ $A) 
      (> $A 0)))


  (= 
    (callable $X) 
    ( (atom $X) (set-det)))
  (= 
    (callable $X) 
    ( (compound $X) (set-det)))
  (= 
    (callable $X) 
    (closure $X))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          Term comparison

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  !(public (, (/ == 2) (/ %equality-of-term 2)))
  !(public (, (/ \== 2) (/ %inequality-of-term 2)))
  !(public (, (/ @< 2) (/ %before 2)))
  !(public (, (/ @> 2) (/ %after 2)))
  !(public (, (/ @=< 2) (/ %not-after 2)))
  !(public (, (/ @>= 2) (/ %not-before 2)))
  !(public (, (/ ?= 2) (/ %identical-or-cannot-unify 2)))
  !(public (/ compare 3))
;
;                                          :- public sort/2.    witten in Java

;
;                                          :- public keysort/2. witten in Java

;
;                                          :- public merge/3.



  (= 
    (== $X $Y) 
    (== $X $Y))

  (= 
    ($equality-of-term $X $Y) 
    ($equality-of-term $X $Y))


  (= 
    (\== $X $Y) 
    (\== $X $Y))

  (= 
    ($inequality-of-term $X $Y) 
    ($inequality-of-term $X $Y))


  (= 
    (@< $X $Y) 
    (@< $X $Y))

  (= 
    ($before $X $Y) 
    ($before $X $Y))


  (= 
    (@> $X $Y) 
    (@> $X $Y))

  (= 
    ($after $X $Y) 
    ($after $X $Y))


  (= 
    (@=< $X $Y) 
    (@=< $X $Y))

  (= 
    ($not-after $X $Y) 
    ($not-after $X $Y))


  (= 
    (@>= $X $Y) 
    (@>= $X $Y))

  (= 
    ($not-before $X $Y) 
    ($not-before $X $Y))


  (= 
    (?= $X $Y) 
    (?= $X $Y))

  (= 
    ($identical-or-cannot-unify $X $Y) 
    ($identical-or-cannot-unify $X $Y))


  (= 
    (compare $Op $X $Y) 
    ( ($compare0 $Op0 $X $Y) ($map-compare-op $Op0 $Op)))


  (= 
    ($map-compare-op $Op0 $Op) 
    ( (=:= $Op0 0) 
      (set-det) 
      (= $Op =)))
  (= 
    ($map-compare-op $Op0 $Op) 
    ( (< $Op0 0) 
      (set-det) 
      (= $Op <)))
  (= 
    ($map-compare-op $Op0 $Op) 
    ( (> $Op0 0) 
      (set-det) 
      (= $Op >)))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          Term creation and decomposition

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                         :- public arg/3.     --> written in Java

;
;                                         :- public functor/3. --> written in Java


  !(public (/ =.. 2))
  !(public (/ copy-term 2))


  (= 
    (=.. $Term $List) 
    (=.. $Term $List))


  (= 
    (copy-term $X $Y) 
    (copy-term $X $Y))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          Arithmetic evaluation

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  !(public (/ is 2))
  !(public (, (/ %abs 2) (/ %asin 2) (/ %acos 2) (/ %atan 2)))
  !(public (, (/ %bitwise-conj 3) (/ %bitwise-disj 3) (/ %bitwise-exclusive-or 3) (/ %bitwise-neg 2)))
  !(public (, (/ %ceil 2) (/ %cos 2)))
  !(public (/ %degrees 2))
  !(public (/ %exp 2))
  !(public (, (/ %float 2) (/ %float-integer-part 2) (/ %float-fractional-part 2) (/ %float-quotient 3) (/ %floor 2)))
  !(public (/ %int-quotient 3))
  !(public (/ %log 2))
  !(public (, (/ %max 3) (/ %min 3) (/ %minus 3) (/ %mod 3) (/ %multi 3)))
  !(public (, (/ %plus 3) (/ %pow 3)))
  !(public (, (/ %radians 2) (/ %rint 2) (/ %round 2)))
  !(public (, (/ %shift-left 3) (/ %shift-right 3) (/ %sign 2) (/ %sin 2) (/ %sqrt 2)))
  !(public (, (/ %tan 2) (/ %truncate 2)))


  (= 
    (is $Z $Y) 
    (is $Z $Y))


  (= 
    ($abs $X $Y) 
    ($abs $X $Y))

  (= 
    ($asin $X $Y) 
    ($asin $X $Y))

  (= 
    ($acos $X $Y) 
    ($acos $X $Y))

  (= 
    ($atan $X $Y) 
    ($atan $X $Y))

  (= 
    ($bitwise-conj $X $Y $Z) 
    ($bitwise-conj $X $Y $Z))

  (= 
    ($bitwise-disj $X $Y $Z) 
    ($bitwise-disj $X $Y $Z))

  (= 
    ($bitwise-exclusive-or $X $Y $Z) 
    ($bitwise-exclusive-or $X $Y $Z))

  (= 
    ($bitwise-neg $X $Y) 
    ($bitwise-neg $X $Y))

  (= 
    ($ceil $X $Y) 
    ($ceil $X $Y))

  (= 
    ($cos $X $Y) 
    ($cos $X $Y))

  (= 
    ($degrees $X $Y) 
    ($degrees $X $Y))

  (= 
    ($exp $X $Y) 
    ($exp $X $Y))

  (= 
    ($float $X $Y) 
    ($float $X $Y))

  (= 
    ($float-integer-part $X $Y) 
    ($float-integer-part $X $Y))

  (= 
    ($float-fractional-part $X $Y) 
    ($float-fractional-part $X $Y))

  (= 
    ($float-quotient $X $Y $Z) 
    ($float-quotient $X $Y $Z))

  (= 
    ($floor $X $Y) 
    ($floor $X $Y))

  (= 
    ($int-quotient $X $Y $Z) 
    ($int-quotient $X $Y $Z))

  (= 
    ($log $X $Y) 
    ($log $X $Y))

  (= 
    ($max $X $Y $Z) 
    ($max $X $Y $Z))

  (= 
    ($min $X $Y $Z) 
    ($min $X $Y $Z))

  (= 
    ($minus $X $Y $Z) 
    ($minus $X $Y $Z))

  (= 
    ($mod $X $Y $Z) 
    ($mod $X $Y $Z))

  (= 
    ($multi $X $Y $Z) 
    ($multi $X $Y $Z))

  (= 
    ($plus $X $Y $Z) 
    ($plus $X $Y $Z))

  (= 
    ($pow $X $Y $Z) 
    ($pow $X $Y $Z))

  (= 
    ($radians $X $Y) 
    ($radians $X $Y))

  (= 
    ($rint $X $Y) 
    ($rint $X $Y))

  (= 
    ($round $X $Y) 
    ($round $X $Y))

  (= 
    ($shift-left $X $Y $Z) 
    ($shift-left $X $Y $Z))

  (= 
    ($shift-right $X $Y $Z) 
    ($shift-right $X $Y $Z))

  (= 
    ($sign $X $Y) 
    ($sign $X $Y))

  (= 
    ($sin $X $Y) 
    ($sin $X $Y))

  (= 
    ($sqrt $X $Y) 
    ($sqrt $X $Y))

  (= 
    ($tan $X $Y) 
    ($tan $X $Y))

  (= 
    ($truncate $X $Y) 
    ($truncate $X $Y))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          Arithmetic comparison

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  !(public (, (/ =:= 2) (/ %arith-equal 2)))
  !(public (, (/ =\= 2) (/ %arith-not-equal 2)))
  !(public (, (/ < 2) (/ %less-than 2)))
  !(public (, (/ =< 2) (/ %less-or-equal 2)))
  !(public (, (/ > 2) (/ %greater-than 2)))
  !(public (, (/ >= 2) (/ %greater-or-equal 2)))


  (= 
    (=:= $X $Y) 
    (=:= $X $Y))

  (= 
    ($arith-equal $X $Y) 
    ($arith-equal $X $Y))


  (= 
    (=\= $X $Y) 
    (=\= $X $Y))

  (= 
    ($arith-not-equal $X $Y) 
    ($arith-not-equal $X $Y))


  (= 
    (< $X $Y) 
    (< $X $Y))

  (= 
    ($less-than $X $Y) 
    ($less-than $X $Y))


  (= 
    (=< $X $Y) 
    (=< $X $Y))

  (= 
    ($less-or-equal $X $Y) 
    ($less-or-equal $X $Y))


  (= 
    (> $X $Y) 
    (> $X $Y))

  (= 
    ($greater-than $X $Y) 
    ($greater-than $X $Y))


  (= 
    (>= $X $Y) 
    (>= $X $Y))

  (= 
    ($greater-or-equal $X $Y) 
    ($greater-or-equal $X $Y))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          Clause retrieval and information

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  !(public (/ clause 2))
  !(public (/ initialization 2))
  !(public (/ %new-indexing-hash 3))


  (= 
    (get-atoms  &self 
      (= $Head $B)) 
    ( ($head-to-term $Head $H 
        (with_self  $P $PI) 
        (get-atoms  &self 
          (= $Head $B))) 
      ($new-internal-database $P) 
      ($check-procedure-permission 
        (with_self  $P $PI) access private-procedure 
        (get-atoms  &self 
          (= $Head $B))) 
      ($clause-internal $P $PI $H $Cl $_) 
      (copy-term $Cl 
        (= $H $B))))
;                                         ;(ground(Cl) -> Cl = (H :- B) ; copy_term(Cl, (H :- B))). ???



;
;                                          head --> term


  (= 
    ($head-to-term $H $T 
      (with_self  $Pkg 
        (/ $F $A)) $Goal) 
    ( ($head-to-term $H $T user $Pkg $Goal) (functor $T $F $A)))

  (= 
    ($head-to-term $H $_ $_ $_ $Goal) 
    ( (var $H) 
      (set-det) 
      (illarg var $Goal 1)))
  (= 
    ($head-to-term 
      (with_self  $P $H) $T $_ $Pkg $Goal) 
    ( (set-det) ($head-to-term $H $T $P $Pkg $Goal)))
  (= 
    ($head-to-term $H $H $Pkg $Pkg $_) 
    ( (callable $H) 
      (atom $Pkg) 
      (set-det)))
  (= 
    ($head-to-term $_ $_ $_ $_ $Goal) 
    (illarg 
      (type callable) $Goal 1))

;
;                                          creates an internal database for A if no exists.


  (= 
    ($new-internal-database $A) 
    ( (atom $A) 
      ($get-hash-manager $HM) 
      ($new-internal-database $HM $A)))

  (= 
    ($new-internal-database $HM $A) 
    ( (hash-contains-key $HM $A) (set-det)))
  (= 
    ($new-internal-database $_ $A) 
    ( (new-hash $_ 
        (:: (alias $A))) ($init-internal-database $A)))


  (= 
    ($init-internal-database $A) 
    ( ($compiled-predicate $A %init 0) 
      (findall $_ 
        (with_self  $A 
          (%init)) $_) 
      (set-det)))
  (= 
    ($init_internal_database  $_) True)

;
;                                          checks if the internal database of A exists.


  (= 
    ($defined-internal-database $A) 
    ( (atom $A) 
      ($get-hash-manager $HM) 
      (hash-contains-key $HM $A)))

;
;                                          repeatedly finds dynamic clauses.


  (= 
    ($clause-internal $P $PI $H $Cl $Ref) 
    ( (hash-contains-key $P $PI) 
      ($get-indices $P $PI $H $RevRefs) 
      ($get-instances $RevRefs $Cls_Refs) 
      ($clause-internal0 $Cls_Refs $Cl $Ref)))
;                                         ; ???

;                                         ;length(Cls_Refs,N),

;                                         ;'$fast_write'([clause_internal,N,for,P,PI]),nl,

;                                         ;




  (= 
    ($clause_internal0  () $_ $_) 
    (empty))
  (= 
    ($clause-internal0 
      (:: (, $Cl $Ref)) $Cl $Ref) 
    (set-det))
  (= 
    ($clause-internal0 $L $Cl $Ref) 
    ($builtin-member 
      (, $Cl $Ref) $L))


  (= 
    ($get-indices $P $PI $H $Refs) 
    ( ($new-indexing-hash $P $PI $IH) 
      ($calc-indexing-key $H $Key) 
      (det-if-then-else 
        (hash-contains-key $IH $Key) 
        (hash-get $IH $Key $Refs) 
        (hash-get $IH var $Refs))))

;
;                                          finds the indexing hashtable for P:PI. creates it if no exist.


  (= 
    ($new-indexing-hash $P $PI $IH) 
    ( (hash-contains-key $P $PI) 
      (set-det) 
      (hash-get $P $PI $IH)))
  (= 
    ($new-indexing-hash $P $PI $IH) 
    ( (new-hash $IH) 
      (hash-put $IH all Nil) 
      (hash-put $IH var Nil) 
      (hash-put $IH lis Nil) 
      (hash-put $IH str Nil) 
      (hash-put $P $PI $IH)))


  (= 
    ($calc-indexing-key $H all) 
    ( (atom $H) (set-det)))
  (= 
    ($calc-indexing-key $H $Key) 
    ( (arg 1 $H $A1) ($calc-indexing-key0 $A1 $Key)))


  (= 
    ($calc-indexing-key0 $A1 all) 
    ( (var $A1) (set-det)))
  (= 
    ($calc-indexing-key0 $A1 lis) 
    ( (= $A1 
        (Cons  $_ $_)) (set-det)))
  (= 
    ($calc-indexing-key0 $A1 str) 
    ( (compound $A1) (set-det)))
  (= 
    ($calc-indexing-key0 $A1 $Key) 
    ( (ground $A1) 
      (set-det) 
      ($term-hash $A1 $Key)))
  (= 
    ($calc-indexing-key0 $A1 $Key) 
    (illarg 
      (type term) 
      ($calc-indexing-key0 $A1 $Key) 1))

;
;                                          checks the permission of predicate P:F/A.


  (= 
    ($check-procedure-permission 
      (with_self  $P 
        (/ $F $A)) $Operation $ObjType $Goal) 
    ( (hash-contains-key $P 
        (/ $F $A)) (set-det)))
  (= 
    ($check-procedure-permission 
      (with_self  $P 
        (/ $F $A)) $Operation $ObjType $Goal) 
    ( ($compiled-predicate-or-builtin $P $F $A) 
      (set-det) 
      (illarg 
        (permission $Operation $ObjType 
          (with_self  $P 
            (/ $F $A)) $_) $Goal $_)))
  (= 
    ($check_procedure_permission  $_ $_ $_ $_) True)

;
;                                          initialize internal databases of given packages.


  (= 
    (initialization Nil $Goal) 
    ( (set-det) (once $Goal)))
  (= 
    (initialization 
      (Cons  $P $Ps) $Goal) 
    ( ($new-internal-database $P) (initialization $Ps $Goal)))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          Clause creation and destruction

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  !(public (/ assert 1))
  !(public (/ assertz 1))
  !(public (/ asserta 1))
  !(public (/ retract 1))
  !(public (/ abolish 1))
  !(public (/ retractall 1))


  (= 
    (add-atom  &self $T) 
    (add-atom  &self $T))


  (= 
    (add-atom  &self $T) 
    ( ($term-to-clause $T $Cl 
        (with_self  $P $PI) 
        (add-atom  &self $T)) 
      ($new-internal-database $P) 
      ($check-procedure-permission 
        (with_self  $P $PI) modify static-procedure 
        (add-atom  &self $T)) 
      (copy-term $Cl $NewCl) 
      ($insert $NewCl $Ref) 
      ($update-indexing $P $PI $Cl $Ref z) 
      (fail)))
;                                         ;'$fast_write'([intert,NewCl,Ref]), nl, ;???


  (= 
    (assertz  $_) True)


  (= 
    (add-atom  &self $T) 
    ( ($term-to-clause $T $Cl 
        (with_self  $P $PI) 
        (add-atom  &self $T)) 
      ($new-internal-database $P) 
      ($check-procedure-permission 
        (with_self  $P $PI) modify static-procedure 
        (add-atom  &self $T)) 
      (copy-term $Cl $NewCl) 
      ($insert $NewCl $Ref) 
      ($update-indexing $P $PI $Cl $Ref a) 
      (fail)))
;                                         ;'$fast_write'([insert,NewCl,Ref]), nl, ;???


  (= 
    (asserta  $_) True)


  (= 
    (abolish $T) 
    ( ($term-to-predicateindicator $T 
        (with_self  $P $PI) 
        (abolish $T)) 
      ($new-internal-database $P) 
      ($check-procedure-permission 
        (with_self  $P $PI) modify static-procedure 
        (abolish $T)) 
      ($new-indexing-hash $P $PI $IH) 
      (hash-get $IH all $Refs) 
      ($erase-all $Refs) 
      (hash-remove $P $PI) 
      (fail)))
;                                         ;'$fast_write'([erase_all,Refs]), nl, ;???


  (= 
    (abolish  $_) True)


  (= 
    (remove-atom  &self $Cl) 
    ( ($clause-to-term $Cl $T 
        (with_self  $P $PI) 
        (remove-atom  &self $Cl)) 
      ($new-internal-database $P) 
      ($check-procedure-permission 
        (with_self  $P $PI) access static-procedure 
        (remove-atom  &self $Cl)) 
      (= $T 
        (= $H $_)) 
      ($clause-internal $P $PI $H $Cl0 $Ref) 
      (copy-term $Cl0 $T) 
      ($erase $Ref) 
      ($rehash-indexing $P $PI $Ref)))
;                                         ;'$fast_write'([erase,Cl0,Ref]), nl, ;???




  (= 
    (remove-all-atoms  &self $Head) 
    ( ($head-to-term $Head $H 
        (with_self  $P $PI) 
        (remove-all-atoms  &self $Head)) 
      ($new-internal-database $P) 
      ($check-procedure-permission 
        (with_self  $P $PI) access static-procedure 
        (remove-all-atoms  &self $Head)) 
      ($clause-internal $P $PI $H $Cl $Ref) 
      (copy-term $Cl 
        (= $H $_)) 
      ($erase $Ref) 
      ($rehash-indexing $P $PI $Ref) 
      (fail)))
;                                         ;'$fast_write'([erase,Cl,Ref]), nl, ;???


  (= 
    (retractall  $_) True)

;
;                                          term --> clause (for assert)


  (= 
    ($term-to-clause $Cl0 $Cl 
      (with_self  $Pkg 
        (/ $F $A)) $Goal) 
    ( ($term-to-clause $Cl0 $Cl user $Pkg $Goal) 
      (= $Cl 
        (= $H $_)) 
      (functor $H $F $A)))

  (= 
    ($term-to-clause $Cl0 $_ $_ $_ $Goal) 
    ( (var $Cl0) 
      (set-det) 
      (illarg var $Goal 1)))
  (= 
    ($term-to-clause $_ $_ $Pkg0 $_ $Goal) 
    ( (var $Pkg0) 
      (set-det) 
      (illarg var $Goal 1)))
  (= 
    ($term-to-clause 
      (with_self  $P $Cl0) $Cl $_ $Pkg $Goal) 
    ( (set-det) ($term-to-clause $Cl0 $Cl $P $Pkg $Goal)))
  (= 
    ($term-to-clause $_ $_ $Pkg0 $_ $Goal) 
    ( (not (atom $Pkg0)) 
      (set-det) 
      (illarg 
        (type is-symbol) $Goal 1)))
  (= 
    ($term-to-clause 
      (= $H0 $B0) 
      (= $H $B) $Pkg $Pkg $Goal) 
    ( (set-det) 
      ($term-to-head $H0 $H $Pkg $Goal) 
      ($term-to-body $B0 $B $Pkg $Goal)))
  (= 
    ($term-to-clause $H0 
      (= $H True) $Pkg $Pkg $Goal) 
    ($term-to-head $H0 $H $Pkg $Goal))


  (= 
    ($term-to-head $H $H $_ $_) 
    ( (atom $H) (set-det)))
  (= 
    ($term-to-head $H $H $_ $_) 
    ( (compound $H) (set-det)))
  (= 
    ($term-to-head $_ $_ $_ $Goal) 
    (illarg 
      (type callable) $Goal 1))


  (= 
    ($term-to-body $B0 $B $Pkg $_) 
    ($localize-body $B0 $Pkg $B))


  (= 
    ($localize-body $G $P $G1) 
    ( (var $G) 
      (set-det) 
      ($localize-body 
        (call $G) $P $G1)))
  (= 
    ($localize-body 
      (with_self  $P $G) $_ $G1) 
    ( (set-det) ($localize-body $G $P $G1)))
  (= 
    ($localize-body 
      (, $X $Y) $P 
      (, $X1 $Y1)) 
    ( (set-det) 
      ($localize-body $X $P $X1) 
      ($localize-body $Y $P $Y1)))
  (= 
    ($localize-body 
      (det-if-then $X $Y) $P 
      (det-if-then $X1 $Y1)) 
    ( (set-det) 
      ($localize-body $X $P $X1) 
      ($localize-body $Y $P $Y1)))
  (= 
    ($localize-body 
      (or $X $Y) $P 
      (or $X1 $Y1)) 
    ( (set-det) 
      ($localize-body $X $P $X1) 
      ($localize-body $Y $P $Y1)))
  (= 
    ($localize-body $G $P $G1) 
    ( (functor $G $F $A) 
      ($builtin-meta-predicates $F $A $M) 
      (set-det) 
      (=.. $G 
        (Cons  $F $As)) 
      ($localize-args $M $As $P $As1) 
      (=.. $G1 
        (Cons  $F $As1))))
;                                         ;???


  (= 
    ($localize-body $G $P 
      (call (with_self  $P $G))) 
    ( (var $P) (set-det)))
  (= 
    ($localize-body $G user $G) 
    (set-det))
  (= 
    ($localize-body $G $_ $G) 
    ( (system-predicate $G) (set-det)))
  (= 
    ($localize_body  $G $P 
      (:  $P $G)) True)


  (= 
    ($localize-args Nil Nil $_ Nil) 
    (set-det))
  (= 
    ($localize-args 
      (Cons  : $Ms) 
      (Cons  $A $As) $P 
      (Cons  
        (with_self  $P $A) $As1)) 
    ( (or 
        (var $A) 
        (with_self  
          (\= $A $_) $_)) 
      (set-det) 
      ($localize-args $Ms $As $P $As1)))
  (= 
    ($localize-args 
      (Cons  $_ $Ms) 
      (Cons  $A $As) $P 
      (Cons  $A $As1)) 
    ($localize-args $Ms $As $P $As1))


  (= 
    ($builtin_meta_predicates  ^ 2 
      (? :)) True)
  (= 
    ($builtin_meta_predicates  call 1 
      (:)) True)
  (= 
    ($builtin_meta_predicates  once 1 
      (:)) True)
  (= 
    ($builtin_meta_predicates  \+ 1 
      (:)) True)
  (= 
    ($builtin_meta_predicates  findall 3 
      (? : ?)) True)
  (= 
    ($builtin_meta_predicates  setof 3 
      (? : ?)) True)
  (= 
    ($builtin_meta_predicates  bagof 3 
      (? : ?)) True)
  (= 
    ($builtin_meta_predicates  on_exception 3 
      (? : :)) True)
  (= 
    ($builtin_meta_predicates  catch 3 
      (: ? :)) True)
  (= 
    ($builtin_meta_predicates  synchronized 2 
      (? :)) True)
  (= 
    ($builtin_meta_predicates  freeze 2 
      (? :)) True)

;
;                                          clause --> term (for retract)


  (= 
    ($clause-to-term $Cl $T 
      (with_self  $Pkg 
        (/ $F $A)) $Goal) 
    ( ($clause-to-term $Cl $T user $Pkg $Goal) 
      (= $T 
        (= $H $_)) 
      (functor $H $F $A)))

  (= 
    ($clause-to-term $Cl $_ $_ $_ $Goal) 
    ( (var $Cl) 
      (set-det) 
      (illarg var $Goal 1)))
  (= 
    ($clause-to-term $_ $_ $Pkg $_ $Goal) 
    ( (var $Pkg) 
      (set-det) 
      (illarg var $Goal 1)))
  (= 
    ($clause-to-term 
      (with_self  $P $Cl) $T $_ $Pkg $Goal) 
    ( (set-det) ($clause-to-term $Cl $T $P $Pkg $Goal)))
  (= 
    ($clause-to-term $_ $_ $Pkg $_ $Goal) 
    ( (not (atom $Pkg)) 
      (set-det) 
      (illarg 
        (type is-symbol) $Goal 1)))
  (= 
    ($clause-to-term 
      (= $H0 $B) 
      (= $H $B) $Pkg $Pkg $Goal) 
    ( (set-det) ($head-to-term $H0 $H $_ $Goal)))
	;
;                                         '$body_to_term'(B0, B, Goal).

  (= 
    ($clause-to-term $H0 
      (= $H True) $Pkg $Pkg $Goal) 
    ($head-to-term $H0 $H $_ $Goal))

;
;                                          term --> predicate indicator (for abolish)


  (= 
    ($term-to-predicateindicator $T 
      (with_self  $Pkg $PI) $Goal) 
    ($term-to-predicateindicator $T $PI user $Pkg $Goal))

  (= 
    ($term-to-predicateindicator $T $_ $_ $_ $Goal) 
    ( (var $T) 
      (set-det) 
      (illarg var $Goal 1)))
  (= 
    ($term-to-predicateindicator $_ $_ $Pkg $_ $Goal) 
    ( (var $Pkg) 
      (set-det) 
      (illarg var $Goal 1)))
  (= 
    ($term-to-predicateindicator 
      (with_self  $P $T) $PI $_ $Pkg $Goal) 
    ( (set-det) ($term-to-predicateindicator $T $PI $P $Pkg $Goal)))
  (= 
    ($term-to-predicateindicator $T $_ $_ $_ $Goal) 
    ( (\= $T 
        (/ $_ $_)) 
      (set-det) 
      (illarg 
        (type predicate-indicator) $Goal 1)))
  (= 
    ($term-to-predicateindicator 
      (/ $F $_) $_ $_ $_ $Goal) 
    ( (not (atom $F)) 
      (set-det) 
      (illarg 
        (type is-symbol) $Goal 1)))
  (= 
    ($term-to-predicateindicator 
      (/ $_ $A) $_ $_ $_ $Goal) 
    ( (not (integer $A)) 
      (set-det) 
      (illarg 
        (type integer) $Goal 1)))
  (= 
    ($term_to_predicateindicator  $T $T $Pkg $Pkg $_) True)


  (= 
    ($update-indexing $P $PI $Cl $Ref $A_or_Z) 
    ( ($new-indexing-hash $P $PI $IH) 
      ($gen-indexing-keys $Cl $IH $Keys) 
      ($update-indexing-hash $A_or_Z $Keys $IH $Ref)))
;                                         ;'$fast_write'([update_indexing,P,PI,Cl,Ref,Keys]), nl, ;???




  (= 
    ($gen-indexing-keys 
      (= $H $_) $_ 
      (:: all)) 
    ( (atom $H) (set-det)))
  (= 
    ($gen-indexing-keys 
      (= $H $_) $IT $Keys) 
    ( (arg 1 $H $A1) ($gen-indexing-keys0 $A1 $IT $Keys)))


  (= 
    ($gen-indexing-keys0 $A1 $IT $Keys) 
    ( (var $A1) 
      (set-det) 
      (hash-keys $IT $Keys)))
  (= 
    ($gen-indexing-keys0 $A1 $_ 
      (:: all lis)) 
    ( (= $A1 
        (Cons  $_ $_)) (set-det)))
  (= 
    ($gen-indexing-keys0 $A1 $_ 
      (:: all str)) 
    ( (compound $A1) (set-det)))
  (= 
    ($gen-indexing-keys0 $A1 $IT 
      (:: all $Key)) 
    ( (ground $A1) 
      (set-det) 
      ($term-hash $A1 $Key) 
      (det-if-then-else 
        (hash-contains-key $IT $Key) True 
        (, 
          (hash-get $IT var $L) 
          (hash-put $IT $Key $L)))))
;                                         ; get the hash code of A1


  (= 
    ($gen-indexing-keys0 $A1 $IT $Keys) 
    (illarg 
      (type term) 
      ($gen-indexing-keys0 $A1 $IT $Keys) 1))


  (= 
    ($update-indexing-hash a $Keys $IH $Ref) 
    ( (set-det) ($hash-addz-all $Keys $IH $Ref)))
  (= 
    ($update-indexing-hash z $Keys $IH $Ref) 
    ( (set-det) ($hash-adda-all $Keys $IH $Ref)))


  (= 
    ($hash-adda-all Nil $_ $_) 
    (set-det))
  (= 
    ($hash-adda-all 
      (Cons  $K $Ks) $H $X) 
    ( ($hash-adda $H $K $X) ($hash-adda-all $Ks $H $X)))


  (= 
    ($hash-addz-all Nil $_ $_) 
    (set-det))
  (= 
    ($hash-addz-all 
      (Cons  $K $Ks) $H $X) 
    ( ($hash-addz $H $K $X) ($hash-addz-all $Ks $H $X)))


  (= 
    ($erase-all Nil) 
    (set-det))
  (= 
    ($erase-all (Cons  $R $Rs)) 
    ( ($erase $R) ($erase-all $Rs)))


  (= 
    ($rehash-indexing $P $PI $Ref) 
    ( ($new-indexing-hash $P $PI $IH) 
      (hash-keys $IH $Keys) 
      ($remove-index-all $Keys $IH $Ref)))
;                                         ;'$fast_write'([rehash_indexing,P,PI,Keys]), nl, ;???




  (= 
    ($remove-index-all Nil $_ $_) 
    (set-det))
  (= 
    ($remove-index-all 
      (Cons  $K $Ks) $IH $Ref) 
    ( ($hash-remove-first $IH $K $Ref) ($remove-index-all $Ks $IH $Ref)))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          All solutions

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  !(public (/ findall 3))
  !(public (/ bagof 3))
  !(public (/ setof 3))

;
;                                          findall/3


  (= 
    (findall $Template $Goal $Instances) 
    ( (callable $Goal) 
      (set-det) 
      (new-hash $H) 
      ($findall $H $Template $Goal $Instances)))
  (= 
    (findall $Template $Goal $Instances) 
    (illarg 
      (type callable) 
      (findall $Template $Goal $Instances) 2))


  (= 
    ($findall $H $Template $Goal $_) 
    ( (call $Goal) 
      (copy-term $Template $CT) 
      ($hash-addz $H %FINDALL $CT) 
      (fail)))
  (= 
    ($findall $H $_ $_ $Instances) 
    (hash-get $H %FINDALL $Instances))

;
;                                          bagof/3 & setof/3


  (= 
    (bagof $Template $Goal $Instances) 
    ( (callable $Goal) 
      (set-det) 
      ($bagof $Template $Goal $Instances)))
  (= 
    (bagof $Template $Goal $Instances) 
    (illarg 
      (type callable) 
      (bagof $Template $Goal $Instances) 2))


  (= 
    (setof $Template $Goal $Instances) 
    ( (callable $Goal) 
      (set-det) 
      ($bagof $Template $Goal $Instances0) 
      (sort $Instances0 $Instances)))
  (= 
    (setof $Template $Goal $Instances) 
    (illarg 
      (type callable) 
      (setof $Template $Goal $Instances) 2))


  (= 
    ($bagof $Template $Goal $Instances) 
    ( ($free-variables-set $Goal $Template $FV) 
      (\== $FV Nil) 
      (set-det) 
      (=.. $Witness 
        (Cons  %witness $FV)) 
      (findall 
        (+ $Witness $Template) $Goal $S) 
      ($bagof-instances $S $Witness $Instances0) 
      (= $Instances $Instances0)))
;                                         ;write('Goal = '), write(Goal), nl,

;                                         ;write('Free variables set = '), write(FV), nl,


  (= 
    ($bagof $Template $Goal $Instances) 
    ( (findall $Template $Goal $Instances) (\== $Instances Nil)))


  (= 
    ($bagof_instances  () $Witness $Instances) 
    (empty))
  (= 
    ($bagof-instances $S0 $Witness $Instances) 
    ( (= $S0 
        (Cons  
          (+ $W $T) $S)) 
      ($variants-subset $S $W $WT_list $T_list $S_next) 
      ($bagof-instances0 $S_next $Witness $Instances 
        (Cons  
          (+ $W $T) $WT_list) 
        (Cons  $T $T_list))))


  (= 
    ($bagof-instances0 $_ $Witness $Instances $WT_list $T_list) 
    ( ($unify-witness $WT_list $Witness) (= $Instances $T_list)))
  (= 
    ($bagof-instances0 $S_next $Witness $Instances $_ $_) 
    ($bagof-instances $S_next $Witness $Instances))


  (= 
    ($variants-subset Nil $W Nil Nil Nil) 
    (set-det))
  (= 
    ($variants-subset 
      (Cons  
        (+ $W0 $T0) $S) $W 
      (Cons  
        (+ $W0 $T0) $WT_list) 
      (Cons  $T0 $T_list) $S_next) 
    ( ($term-variant $W $W0) 
      (set-det) 
      ($variants-subset $S $W $WT_list $T_list $S_next)))
  (= 
    ($variants-subset 
      (Cons  $WT $S) $W $WT_list $T_list 
      (Cons  $WT $S_next)) 
    ($variants-subset $S $W $WT_list $T_list $S_next))


  (= 
    ($term-variant $X $Y) 
    ( (new-hash $Hash) ($term-variant $X $Y $Hash)))

  (= 
    ($term-variant $X $Y $Hash) 
    ( (var $X) 
      (set-det) 
      (det-if-then-else 
        (hash-contains-key $Hash $X) 
        (, 
          (hash-get $Hash $X $V) 
          (== $Y $V)) 
        (, 
          (var $Y) 
          (hash-put $Hash $X $Y)))))
  (= 
    ($term-variant $X $Y $_) 
    ( (ground $X) 
      (set-det) 
      (== $X $Y)))
  (= 
    ($term-variant $_ $Y $_) 
    ( (var $Y) 
      (set-det) 
      (fail)))
  (= 
    ($term-variant 
      (Cons  $X $Xs) 
      (Cons  $Y $Ys) $Hash) 
    ( (set-det) 
      ($term-variant $X $Y $Hash) 
      ($term-variant $Xs $Ys $Hash)))
  (= 
    ($term-variant $X $Y $Hash) 
    ( (=.. $X $Xs) 
      (=.. $Y $Ys) 
      ($term-variant $Xs $Ys $Hash)))


  (= 
    ($unify-witness Nil $_) 
    (set-det))
  (= 
    ($unify-witness 
      (Cons  
        (+ $W $_) $WT_list) $W) 
    ($unify-witness $WT_list $W))

;
;                                          Variable set of a term


  (= 
    ($variables-set $X $Vs) 
    ($variables-set $X Nil $Vs))

  (= 
    ($variables-set $X $Vs $Vs) 
    ( (var $X) 
      ($builtin-memq $X $Vs) 
      (set-det)))
  (= 
    ($variables-set $X $Vs 
      (Cons  $X $Vs)) 
    ( (var $X) (set-det)))
  (= 
    ($variables-set $X $Vs0 $Vs0) 
    ( (atomic $X) (set-det)))
  (= 
    ($variables-set 
      (Cons  $X $Xs) $Vs0 $Vs) 
    ( (set-det) 
      ($variables-set $X $Vs0 $Vs1) 
      ($variables-set $Xs $Vs1 $Vs)))
  (= 
    ($variables-set $X $Vs0 $Vs) 
    ( (=.. $X $Xs) ($variables-set $Xs $Vs0 $Vs)))


  (= 
    ($builtin-memq $X 
      (Cons  $Y $_)) 
    ( (== $X $Y) (set-det)))
  (= 
    ($builtin-memq $X 
      (Cons  $_ $Ys)) 
    ($builtin-memq $X $Ys))

;
;                                          Existential variables set of a term


  (= 
    ($existential-variables-set $X $Vs) 
    ($existential-variables-set $X Nil $Vs))

  (= 
    ($existential-variables-set $X $Vs $Vs) 
    ( (var $X) (set-det)))
  (= 
    ($existential-variables-set $X $Vs $Vs) 
    ( (atomic $X) (set-det)))
  (= 
    ($existential-variables-set 
      (with_self  $_ $X) $Vs0 $Vs) 
    ( (set-det) ($existential-variables-set $X $Vs0 $Vs)))
;
;                                         '$existential_variables_set'((X;Y), Vs0, Vs) :- !,

;
;                                         	'$existential_variables_set'(X, Vs0, Vs1),

;
;                                         	'$existential_variables_set'(Y, Vs1, Vs).

;
;                                         '$existential_variables_set'((X->Y), Vs0, Vs) :- !,

;
;                                         	'$existential_variables_set'(X, Vs0, Vs1),

;
;                                         	'$existential_variables_set'(Y, Vs1, Vs).

;
;                                         '$existential_variables_set'((X,Y), Vs0, Vs) :- !,

;
;                                         	'$existential_variables_set'(X, Vs0, Vs1),

;
;                                         	'$existential_variables_set'(Y, Vs1, Vs).

  (= 
    ($existential-variables-set 
      (^ $V $G) $Vs0 $Vs) 
    ( (set-det) 
      ($variables-set $V $Vs0 $Vs1) 
      ($existential-variables-set $G $Vs1 $Vs)))
  (= 
    ($existential-variables-set 
      ($meta-call $G $_ $_ $_ $_) $Vs0 $Vs) 
    ( (set-det) ($existential-variables-set $G $Vs0 $Vs)))
;                                         ;???


  (= 
    ($existential_variables_set  $_ $Vs $Vs) True)

;
;                                          Free variables set of a term


  (= 
    ($free-variables-set $T $V $FV) 
    ( ($variables-set $T $TV) 
      ($variables-set $V $VV) 
      ($existential-variables-set $T $VV $BV) 
      ($builtin-set-diff $TV $BV $FV) 
      (set-det)))


  (= 
    ($builtin-set-diff $L1 $L2 $L) 
    ( (sort $L1 $SL1) 
      (sort $L2 $SL2) 
      ($builtin-set-diff0 $SL1 $SL2 $L)))


  (= 
    ($builtin-set-diff0 Nil $_ Nil) 
    (set-det))
  (= 
    ($builtin-set-diff0 $L1 Nil $L1) 
    (set-det))
  (= 
    ($builtin-set-diff0 
      (Cons  $X $Xs) 
      (Cons  $Y $Ys) $L) 
    ( (== $X $Y) 
      (set-det) 
      ($builtin-set-diff0 $Xs $Ys $L)))
  (= 
    ($builtin-set-diff0 
      (Cons  $X $Xs) 
      (Cons  $Y $Ys) 
      (Cons  $X $L)) 
    ( (@< $X $Y) 
      (set-det) 
      ($builtin-set-diff0 $Xs 
        (Cons  $Y $Ys) $L)))
  (= 
    ($builtin-set-diff0 
      (Cons  $X $Xs) 
      (Cons  $Y $Ys) 
      (Cons  $Y $L)) 
    ($builtin-set-diff0 
      (Cons  $X $Xs) $Ys 
      (Cons  $Y $L)))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          Stream selection and control

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                         :- public current_input/1  (written in Java)

;
;                                         :- public current_output/1 (written in Java)

;
;                                         :- public set_input/1, set_output/1. (written in Java)

;
;                                         :- public open/4 (written in Java)


  !(public (/ open 3))
;
;                                         :- public close/2 (written in Java)

  !(public (/ close 1))
;
;                                         :- public flush_output/1.(written in Java)

  !(public (/ flush-output 0))
  !(public (/ stream-property 2))


  (= 
    (open $Source_sink $Mode $Stream) 
    (open $Source_sink $Mode $Stream Nil))


  (= 
    (close $S_or_a) 
    (close $S_or_a Nil))


  (= 
    (flush-output) 
    ( (current-output $S) (flush-output $S)))


  (= 
    (stream-property $Stream $Stream_property) 
    ( (var $Stream_property) 
      (set-det) 
      ($stream-property $Stream $Stream_property)))
  (= 
    (stream-property $Stream $Stream_property) 
    ( ($stream-property-specifier $Stream_property) 
      (set-det) 
      ($stream-property $Stream $Stream_property)))
  (= 
    (stream-property $Stream $Stream_property) 
    (illarg 
      (domain term stream-property) 
      (stream-property $Stream $Stream_property) 2))


  (= 
    ($stream-property $Stream $Stream_property) 
    ( (var $Stream) 
      (set-det) 
      ($get-stream-manager $SM) 
      (hash-map $SM $Map) 
      ($builtin-member 
        (, $Stream $Vs) $Map) 
      (java $Stream) 
      ($builtin-member $Stream_property $Vs)))
  (= 
    ($stream-property $Stream $Stream_property) 
    ( (java $Stream) 
      (set-det) 
      ($get-stream-manager $SM) 
      (hash-get $SM $Stream $Vs) 
      ($builtin-member $Stream_property $Vs)))
  (= 
    ($stream-property $Stream $Stream_property) 
    (illarg 
      (domain stream stream) 
      (stream-property $Stream $Stream_property) 1))


  (= 
    ($stream_property_specifier  input) True)
  (= 
    ($stream_property_specifier  output) True)
  (= 
    ($stream_property_specifier  
      (alias  $_)) True)
  (= 
    ($stream_property_specifier  
      (mode  $_)) True)
  (= 
    ($stream_property_specifier  
      (type  $_)) True)
  (= 
    ($stream_property_specifier  
      (file_name  $_)) True)

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          Character input/output

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                         :- public get_char/2, get_code/2.   (written in Java)

;
;                                         :- public peek_char/2, peek_code/2. (written in Java)

;
;                                         :- public put_char/2, put_code/2.   (written in Java)

;
;                                         :- public nl/0.                     (written in Java)



  !(public (, (/ get-char 1) (/ get-code 1)))
  !(public (, (/ peek-char 1) (/ peek-code 1)))
  !(public (, (/ put-char 1) (/ put-code 1)))
  !(public (/ nl 1))


  (= 
    (get-char $Char) 
    ( (current-input $S) (get-char $S $Char)))

  (= 
    (get-code $Code) 
    ( (current-input $S) (get-code $S $Code)))


  (= 
    (peek-char $Char) 
    ( (current-input $S) (peek-char $S $Char)))

  (= 
    (peek-code $Code) 
    ( (current-input $S) (peek-code $S $Code)))


  (= 
    (put-char $Char) 
    ( (current-output $S) (put-char $S $Char)))

  (= 
    (put-code $Code) 
    ( (current-output $S) (put-code $S $Code)))


  (= 
    (nl $S) 
    (put-char $S 
))


  !(public (, (/ get0 1) (/ get0 2)))
  !(public (/ get 1))
;
;                                         :- public get/2.  (written in Java)

  !(public (, (/ put 1) (/ put 2)))
  !(public (/ tab 1))
;
;                                         :- public tab/2.  (written in Java)

  !(public (/ skip 1))
;
;                                         :- public skip/2. (written in Java)



  (= 
    (get0 $Code) 
    ( (current-input $S) (get-code $S $Code)))
  (= 
    (get0 $S_or_a $Code) 
    (get-code $S_or_a $Code))


  (= 
    (get $Code) 
    ( (current-input $S) (get $S $Code)))


  (= 
    (put $Exp) 
    ( (current-output $S) (put $S $Exp)))
  (= 
    (put $S_or_a $Exp) 
    ( (is $Code $Exp) (put-code $S_or_a $Code)))


  (= 
    (tab $N) 
    ( (current-output $S) (tab $S $N)))


  (= 
    (skip $N) 
    ( (current-input $S) (skip $S $N)))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          Byte input/output

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  !(public (, (/ get-byte 1) (/ peek-byte 1) (/ put-byte 1)))
;
;                                         :- public get_byte/2.  ; written in java

;
;                                         :- public peek_byte/2. ; written in java

;
;                                         :- public put_byte/2.  ; written in java



  (= 
    (get-byte $Byte) 
    ( (current-input $S) (get-byte $S $Byte)))


  (= 
    (peek-byte $Byte) 
    ( (current-input $S) (peek-byte $S $Byte)))


  (= 
    (put-byte $Byte) 
    ( (current-output $S) (put-byte $S $Byte)))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          Term input/output (read)

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  !(public (, (/ read 1) (/ read 2)))
  !(public (, (/ read-with-variables 2) (/ read-with-variables 3)))
  !(public (/ read-line 1))
;
;                                         :- public read_line/2. (written in Java)

  !(dynamic (/ %tokens 1))


  (= 
    (read $X) 
    ( (current-input $S) (read $S $X)))

  (= 
    (read $S_or_a $X) 
    ( (read-tokens $S_or_a $Tokens $_) 
      (parse-tokens $X $Tokens) 
      (set-det)))


  (= 
    (read-with-variables $X $Vs) 
    ( (current-input $S) (read-with-variables $S $X $Vs)))

  (= 
    (read-with-variables $S_or_a $X $Vs) 
    ( (read-tokens $S_or_a $Tokens $Vs) 
      (parse-tokens $X $Tokens) 
      (set-det)))


  (= 
    (read-line $X) 
    ( (current-input $S) (read-line $S $X)))

;
;                                          read_token(S_or_a, Token) reads one token from the input,

;
;                                          and unifies Token with:

;
;                                            error(Atom),

;
;                                            end_of_file,

;
;                                            '.', ' ', '(', ')', '[', ']', '{', '}', ',', '|',

;
;                                            number(Integer_or_Float),

;
;                                            atom(Atom),

;
;                                            var(Atom),

;
;                                            string(CharCodeList)


;
;                                         read_token(Token) :- current_input(S), read_token(S, Token).



  (= 
    (read-token $S_or_a $Token) 
    ( ($read-token0 $S_or_a $Type $Token0) ($read-token1 (:: $Type) $Token0 $Token)))


  (= 
    ($read-token1 
      (:: -2) $T 
      (error $T)) 
    (set-det)) ;
;                                          error('message')

  (= 
    ($read-token1 "I" $T 
      (number $T)) 
    (set-det)) ;
;                                          number(intvalue)

  (= 
    ($read-token1 "L" $T 
      (number $T)) 
    (set-det)) ;
;                                          number(longvalue)

  (= 
    ($read-token1 "D" $T 
      (number $T)) 
    (set-det)) ;
;                                          number(floatvalue)

  (= 
    ($read-token1 "A" $T 
      (atom $T)) 
    (set-det)) ;
;                                          atom('name')

  (= 
    ($read-token1 "V" $T 
      (var $T)) 
    (set-det)) ;
;                                          var('name')

  (= 
    ($read-token1 "S" $T 
      (string $T)) 
    (set-det)) ;
;                                          string("chars")

  (= 
    ($read-token1 $_ $T $T) 
    (set-det)) ;
;                                          others


;
;                                          read_tokens(Tokens, Vs) reads tokens from the input

;
;                                          until full-stop-mark ('.') or end_of_file,

;
;                                          unifies Tokens with a list of tokens.

;
;                                          Token for a variable has a form of var(Name,Variable).

;
;                                          Vs is a list of Name=Variable pairs.


;
;                                         read_tokens(Tokens, Vs) :-

;
;                                         	current_input(Stream),

;
;                                         	'$read_tokens'(Stream, Tokens, Vs, []),

;
;                                         	!.



  (= 
    (read-tokens $Stream $Tokens $Vs) 
    ( ($read-tokens $Stream $Tokens $Vs Nil) (set-det)))


  (= 
    ($read-tokens $Stream $Tokens $Vs $VI) 
    ( (read-token $Stream $Token) ($read-tokens1 $Stream $Token $Tokens $Vs $VI)))


  (= 
    ($read-tokens1 $Stream 
      (error $Message) Nil $_ $_) 
    ( (set-det) 
      (write user-error '{SYNTAX ERROR}') 
      (nl user-error) 
      (write user-error ** ) 
      (write user-error $Message) 
      (write user-error  **) 
      (nl user-error) 
      (flush-output user-error) 
      ($read-tokens-until-fullstop $Stream) 
      (fail)))
  (= 
    ($read-tokens1 $Stream end-of-file 
      (:: end-of-file .) Nil $_) 
    (set-det))
  (= 
    ($read-tokens1 $Stream . 
      (:: .) Nil $_) 
    (set-det))
  (= 
    ($read-tokens1 $Stream 
      (var -) 
      (Cons  
        (var - $V) $Tokens) 
      (Cons  
        (= - $V) $Vs) $VI0) 
    ( (set-det) ($read-tokens $Stream $Tokens $Vs (Cons  (= - $V) $VI0))))
  (= 
    ($read-tokens1 $Stream 
      (var $Name) 
      (Cons  
        (var $Name $V) $Tokens) $Vs $VI) 
    ( ($mem-pair 
        (= $Name $V) $VI) 
      (set-det) 
      ($read-tokens $Stream $Tokens $Vs $VI)))
  (= 
    ($read-tokens1 $Stream 
      (var $Name) 
      (Cons  
        (var $Name $V) $Tokens) 
      (Cons  
        (= $Name $V) $Vs) $VI0) 
    ( (set-det) ($read-tokens $Stream $Tokens $Vs (Cons  (= $Name $V) $VI0))))
  (= 
    ($read-tokens1 $Stream $Token 
      (Cons  $Token $Tokens) $Vs $VI) 
    ($read-tokens $Stream $Tokens $Vs $VI))


  (= 
    ($mem-pair 
      (= $X1 $V1) 
      (Cons  
        (= $X2 $V2) $_)) 
    ( (== $X1 $X2) 
      (set-det) 
      (= $V1 $V2)))
  (= 
    ($mem-pair $X 
      (Cons  $_ $L)) 
    ($mem-pair $X $L))
;
;                                         '$mem_pair'(X, [_|L]) :- member(X, L).



  (= 
    ($read-tokens-until-fullstop $Stream) 
    ( (read-token $Stream $Token) ($read-tokens-until-fullstop $Stream $Token)))

  (= 
    ($read-tokens-until-fullstop $Stream end-of-file) 
    (set-det))
  (= 
    ($read-tokens-until-fullstop $Stream .) 
    (set-det))
  (= 
    ($read-tokens-until-fullstop $Stream $_) 
    ( (read-token $Stream $Token) ($read-tokens-until-fullstop $Stream $Token)))


  (= 
    (parse-tokens $X $Tokens) 
    ( (remove-all-atoms  &self 
        ($tokens  $_)) 
      (add-atom  &self 
        ($tokens  $Tokens)) 
      ($parse-tokens $X 1201 $Tokens 
        (:: .)) 
      (remove-atom  &self 
        ($tokens  $Tokens)) 
      (set-det)))

;
;                                          '$parse_tokens'(X, Prec) parses the input whose precedecence =< Prec.


  (= 
    (-->  
      ($parse_tokens  $X $Prec0) 
      (,  $parse_tokens_skip_spaces 
        (,  
          ($parse_tokens1  $Prec0 $X1 $Prec1) 
          (,  ! 
            (,  $parse_tokens_skip_spaces 
              (,  
                ($parse_tokens2  $Prec0 $X1 $Prec1 $X $Prec) !)))))) True)

  (= 
    (-->  
      ($parse_tokens1  $Prec0 $X1 $Prec1) 
      (,  
        ($parse_tokens_peep_next  $Next) 
        (,  
          { ($parse_tokens_is_starter  $Next) } 
          (,  ! 
            ($parse_tokens_before_op  $Prec0 $X1 $Prec1))))) True)
  (= 
    (-->  
      ($parse_tokens1  $_ $_ $_) 
      (,  
        ($parse_tokens_peep_next  $Next) 
        ($parse_tokens_error  
          ($Next cannot start an expression)))) True)

  (= 
    (-->  
      ($parse_tokens2  $Prec0 $X $Prec $X $Prec) 
      (,  
        ($parse_tokens_peep_next  $Next) 
        (,  
          { ($parse_tokens_is_terminator  $Next) } 
          (,  
            { (=<  $Prec $Prec0) } !)))) True)
  (= 
    (-->  
      ($parse_tokens2  $Prec0 $X1 $Prec1 $X $Prec) 
      (,  
        ($parse_tokens_peep_next  $Next) 
        (,  
          { ($parse_tokens_is_post_in_op  $Next) } 
          (,  ! 
            ($parse_tokens_post_in_ops  $Prec0 $X1 $Prec1 $X $Prec))))) True)
  (= 
    (-->  
      ($parse_tokens2  $_ $_ $_ $_ $_) 
      ($parse_tokens_error  
        (operator expected after expression))) True)

;
;                                          '$parse_tokens_before_op'(Prec0, X, Prec)

;
;                                          parses the input until infix or postfix operator,

;
;                                          and returns X and Prec

  (= 
    (-->  
      ($parse_tokens_before_op  $Prec0 $X $Prec) 
      (,  
        (' ') 
        (,  ! 
          ($parse_tokens_before_op  $Prec0 $X $Prec)))) True)
  (= 
    (-->  
      ($parse_tokens_before_op  $_ end_of_file 0) 
      (,  
        (end_of_file) !)) True)
  (= 
    (-->  
      ($parse_tokens_before_op  $_ $N 0) 
      (,  
        ( (number  $N)) !)) True)
  (= 
    (-->  
      ($parse_tokens_before_op  $_ $N 0) 
      (,  
        ( (atom  -)) 
        (,  
          ( (number  $N0)) 
          (,  ! 
            { (is  $N 
                (-  $N0)) })))) True)
  (= 
    (-->  
      ($parse_tokens_before_op  $_ $V 0) 
      (,  
        ( (var  $_ $V)) !)) True)
  (= 
    (-->  
      ($parse_tokens_before_op  $_ $S 0) 
      (,  
        ( (string  $S)) !)) True)
  (= 
    (-->  
      ($parse_tokens_before_op  $_ $X 0) 
      (,  
        (() 
        (,  ! 
          (,  
            ($parse_tokens  $X 1201) 
            ($parse_tokens_expect  )))))) True)
  (= 
    (-->  
      ($parse_tokens_before_op  $_ $X 0) 
      (,  
        ({) 
        (,  ! 
          (,  $parse_tokens_skip_spaces 
            ($parse_tokens_brace  $X))))) True)
  (= 
    (-->  
      ($parse_tokens_before_op  $_ $X 0) 
      (,  
        ([) 
        (,  ! 
          (,  $parse_tokens_skip_spaces 
            ($parse_tokens_list  $X))))) True)
  (= 
    (-->  
      ($parse_tokens_before_op  $_ $X 0) 
      (,  
        ( (atom  $F)) 
        (,  
          (() 
          (,  ! 
            (,  $parse_tokens_skip_spaces 
              (,  
                ($parse_tokens_args  $Args) 
                { (=..  $X 
                    (Cons  $F $Args)) })))))) True)
  (= 
    (-->  
      ($parse_tokens_before_op  $Prec0 $X $PrecOp) 
      (,  
        ( (atom  $F)) 
        (,  
          { (current_op  $PrecOp fx $F) } 
          (,  
            { (=<  $PrecOp $Prec0) } 
            (,  $parse_tokens_skip_spaces 
              (,  
                ($parse_tokens_peep_next  $Next) 
                (,  
                  { ($parse_tokens_is_starter  $Next) } 
                  (,  
                    { (\+  
                        ($parse_tokens_is_post_in_op  $Next)) } 
                    (,  ! 
                      (,  
                        { (is  $Prec1 
                            (-  $PrecOp 1)) } 
                        (,  
                          ($parse_tokens  $Arg $Prec1) 
                          (,  
                            { (functor  $X $F 1) } 
                            { (arg  1 $X $Arg) })))))))))))) True)
  (= 
    (-->  
      ($parse_tokens_before_op  $Prec0 $X $PrecOp) 
      (,  
        ( (atom  $F)) 
        (,  
          { (current_op  $PrecOp fy $F) } 
          (,  
            { (=<  $PrecOp $Prec0) } 
            (,  $parse_tokens_skip_spaces 
              (,  
                ($parse_tokens_peep_next  $Next) 
                (,  
                  { ($parse_tokens_is_starter  $Next) } 
                  (,  
                    { (\+  
                        ($parse_tokens_is_post_in_op  $Next)) } 
                    (,  ! 
                      (,  
                        ($parse_tokens  $Arg $PrecOp) 
                        (,  
                          { (functor  $X $F 1) } 
                          { (arg  1 $X $Arg) }))))))))))) True)
  (= 
    (-->  
      ($parse_tokens_before_op  $_ $A 0) 
      ( (atom  $A))) True)

  (= 
    (-->  
      ($parse_tokens_brace  {}) 
      (,  
        (}) !)) True)
  (= 
    (-->  
      ($parse_tokens_brace  $X) 
      (,  
        ($parse_tokens  $X1 1201) 
        (,  
          ($parse_tokens_expect  }) 
          { (= $X 
              {$X1 }) }))) True)

  (= 
    (-->  
      ($parse_tokens_list  []) 
      (,  
        (]) !)) True)
  (= 
    (-->  
      ($parse_tokens_list  
        (Cons  $X $Xs)) 
      (,  
        ($parse_tokens  $X 999) 
        (,  $parse_tokens_skip_spaces 
          ($parse_tokens_list_rest  $Xs)))) True)

  (= 
    (-->  
      ($parse_tokens_list_rest  $Xs) 
      (,  
        (|) 
        (,  ! 
          (,  
            ($parse_tokens  $Xs 999) 
            ($parse_tokens_expect  ]))))) True)
  (= 
    (-->  
      ($parse_tokens_list_rest  
        (Cons  $X $Xs)) 
      (,  
        (,) 
        (,  ! 
          (,  
            ($parse_tokens  $X 999) 
            (,  $parse_tokens_skip_spaces 
              ($parse_tokens_list_rest  $Xs)))))) True)
  (= 
    (-->  
      ($parse_tokens_list_rest  []) 
      ($parse_tokens_expect  ])) True)

  (= 
    (-->  
      ($parse_tokens_args  []) 
      (,  
        ()) !)) True)
  (= 
    (-->  
      ($parse_tokens_args  
        (Cons  $X $Xs)) 
      (,  
        ($parse_tokens  $X 999) 
        (,  $parse_tokens_skip_spaces 
          ($parse_tokens_args_rest  $Xs)))) True)

  (= 
    (-->  
      ($parse_tokens_args_rest  
        (Cons  $X $Xs)) 
      (,  
        (,) 
        (,  ! 
          (,  
            ($parse_tokens  $X 999) 
            (,  $parse_tokens_skip_spaces 
              ($parse_tokens_args_rest  $Xs)))))) True)
  (= 
    (-->  
      ($parse_tokens_args_rest  []) 
      ($parse_tokens_expect  ))) True)

;
;                                          '$parse_tokens_post_in_op'(Prec0, X1, Prec1, X, Prec)

;
;                                          parses the input beginning from infix or postfix operator,

;
;                                          and returns X and Prec

  (= 
    (-->  
      ($parse_tokens_post_in_ops  $Prec0 $X1 $Prec1 $X $Prec) 
      (,  $parse_tokens_skip_spaces 
        (,  
          ($Op) 
          (,  
            ($parse_tokens_op  $Op $Prec0 $X1 $Prec1 $X2 $Prec2) 
            ($parse_tokens_post_in_ops  $Prec0 $X2 $Prec2 $X $Prec))))) True)
  (= 
    (-->  
      ($parse_tokens_post_in_ops  $Prec0 $X $Prec $X $Prec) 
      { (=<  $Prec $Prec0) }) True)

  (= 
    (-->  
      ($parse_tokens_op  , $Prec0 $X1 $Prec1 $X $PrecOp) 
      (,  ! 
        ($parse_tokens_op  
          (atom  ,) $Prec0 $X1 $Prec1 $X $PrecOp))) True)
  (= 
    (-->  
      ($parse_tokens_op  | $Prec0 $X1 $Prec1 $X $PrecOp) 
      (,  ! 
        ($parse_tokens_op  
          (atom  ;) $Prec0 $X1 $Prec1 $X $PrecOp))) True)
  (= 
    (-->  
      ($parse_tokens_op  
        (atom  $Op) $Prec0 $X1 $Prec1 $X $PrecOp) 
      (,  
        { (current_op  $PrecOp xf $Op) } 
        (,  
          { (=<  $PrecOp $Prec0) } 
          (,  
            { (<  $Prec1 $PrecOp) } 
            (,  
              { (functor  $X $Op 1) } 
              { (arg  1 $X $X1) }))))) True)
  (= 
    (-->  
      ($parse_tokens_op  
        (atom  $Op) $Prec0 $X1 $Prec1 $X $PrecOp) 
      (,  
        { (current_op  $PrecOp yf $Op) } 
        (,  
          { (=<  $PrecOp $Prec0) } 
          (,  
            { (=<  $Prec1 $PrecOp) } 
            (,  
              { (functor  $X $Op 1) } 
              { (arg  1 $X $X1) }))))) True)
  (= 
    (-->  
      ($parse_tokens_op  
        (atom  $Op) $Prec0 $X1 $Prec1 $X $PrecOp) 
      (,  
        { (current_op  $PrecOp xfx $Op) } 
        (,  
          { (=<  $PrecOp $Prec0) } 
          (,  
            { (<  $Prec1 $PrecOp) } 
            (,  
              { (is  $Prec2 
                  (-  $PrecOp 1)) } 
              (,  
                ($parse_tokens  $X2 $Prec2) 
                (,  ! 
                  (,  
                    { (functor  $X $Op 2) } 
                    (,  
                      { (arg  1 $X $X1) } 
                      { (arg  2 $X $X2) }))))))))) True)
  (= 
    (-->  
      ($parse_tokens_op  
        (atom  $Op) $Prec0 $X1 $Prec1 $X $PrecOp) 
      (,  
        { (current_op  $PrecOp xfy $Op) } 
        (,  
          { (=<  $PrecOp $Prec0) } 
          (,  
            { (<  $Prec1 $PrecOp) } 
            (,  
              { (is  $Prec2 $PrecOp) } 
              (,  
                ($parse_tokens  $X2 $Prec2) 
                (,  ! 
                  (,  
                    { (functor  $X $Op 2) } 
                    (,  
                      { (arg  1 $X $X1) } 
                      { (arg  2 $X $X2) }))))))))) True)
  (= 
    (-->  
      ($parse_tokens_op  
        (atom  $Op) $Prec0 $X1 $Prec1 $X $PrecOp) 
      (,  
        { (current_op  $PrecOp yfx $Op) } 
        (,  
          { (=<  $PrecOp $Prec0) } 
          (,  
            { (=<  $Prec1 $PrecOp) } 
            (,  
              { (is  $Prec2 
                  (-  $PrecOp 1)) } 
              (,  
                ($parse_tokens  $X2 $Prec2) 
                (,  ! 
                  (,  
                    { (functor  $X $Op 2) } 
                    (,  
                      { (arg  1 $X $X1) } 
                      { (arg  2 $X $X2) }))))))))) True)


  (= 
    ($parse_tokens_is_starter  end_of_file) True)
  (= 
    ($parse_tokens_is_starter  () True)
  (= 
    ($parse_tokens_is_starter  [) True)
  (= 
    ($parse_tokens_is_starter  {) True)
  (= 
    ($parse_tokens_is_starter  
      (number  $_)) True)
  (= 
    ($parse_tokens_is_starter  
      (atom  $_)) True)
  (= 
    ($parse_tokens_is_starter  
      (var  $_ $_)) True)
  (= 
    ($parse_tokens_is_starter  
      (string  $_)) True)


  (= 
    ($parse_tokens_is_terminator  )) True)
  (= 
    ($parse_tokens_is_terminator  ]) True)
  (= 
    ($parse_tokens_is_terminator  }) True)
  (= 
    ($parse_tokens_is_terminator  .) True)


  (= 
    ($parse-tokens-is-post-in-op ,) 
    (set-det))
  (= 
    ($parse-tokens-is-post-in-op |) 
    (set-det))
  (= 
    ($parse-tokens-is-post-in-op (atom $Op)) 
    ( (current-op $_ $Type $Op) 
      ($parse-tokens-post-in-type $Type) 
      (set-det)))


  (= 
    ($parse_tokens_post_in_type  xfx) True)
  (= 
    ($parse_tokens_post_in_type  xfy) True)
  (= 
    ($parse_tokens_post_in_type  yfx) True)
  (= 
    ($parse_tokens_post_in_type  xf) True)
  (= 
    ($parse_tokens_post_in_type  yf) True)


  (= 
    (-->  
      ($parse_tokens_expect  $Token) 
      (,  $parse_tokens_skip_spaces 
        (,  
          ($Token) !))) True)
  (= 
    (-->  
      ($parse_tokens_expect  $Token) 
      ($parse_tokens_error  
        ($Token expected))) True)

  (= 
    (-->  $parse_tokens_skip_spaces 
      (,  
        (' ') 
        (,  ! $parse_tokens_skip_spaces))) True)
  (= 
    (-->  $parse_tokens_skip_spaces ()) True)


  (= 
    ($parse-tokens-peep-next $Next $S $S) 
    (= $S 
      (Cons  $Next $_)))


  (= 
    ($parse-tokens-error $Message $S0 $S) 
    ( (write user-error '{SYNTAX ERROR}') 
      (nl user-error) 
      (write user-error ** ) 
      ($parse-tokens-write-message user-error $Message) 
      (write user-error  **) 
      (nl user-error) 
      ($parse-tokens-error1 Nil $S0) 
      (get-atoms  &self 
        (= 
          ($tokens  $Tokens) $_)) 
      ($parse-tokens-error1 $Tokens $S0) 
      (flush-output user-error) 
      (fail)))


  (= 
    ($parse-tokens-error1 Nil $_) 
    (set-det))
  (= 
    ($parse-tokens-error1 $Tokens $S0) 
    ( (== $Tokens $S0) 
      (set-det) 
      (nl user-error) 
      (write user-error '** here **') 
      (nl user-error) 
      ($parse-tokens-error1 $Tokens Nil) 
      (nl user-error)))
  (= 
    ($parse-tokens-error1 
      (Cons  $Token $Tokens) $S0) 
    ( ($parse-tokens-error2 $Token) ($parse-tokens-error1 $Tokens $S0)))


  (= 
    ($parse-tokens-error2 (number $X)) 
    ( (set-det) (write $X)))
  (= 
    ($parse-tokens-error2 (atom $X)) 
    ( (set-det) (writeq $X)))
  (= 
    ($parse-tokens-error2 (var $X $_)) 
    ( (set-det) (write $X)))
  (= 
    ($parse-tokens-error2 (string $X)) 
    ( (set-det) 
      (write user-error ") 
      ($parse-tokens-write-string user-error $X) 
      (write user-error ")))
  (= 
    ($parse-tokens-error2 $X) 
    (write user-error $X))


  (= 
    ($parse_tokens_write_string  $_ ()) True)
  (= 
    ($parse-tokens-write-string $S 
      (Cons  $C $Cs)) 
    ( (= 
        (:: $C) "\"") 
      (set-det) 
      (put-code $S $C) 
      (put-code $S $C) 
      ($parse-tokens-write-string $S $Cs)))
  (= 
    ($parse-tokens-write-string $S 
      (Cons  $C $Cs)) 
    ( (put-code $S $C) ($parse-tokens-write-string $S $Cs)))


  (= 
    ($parse_tokens_write_message  $_ ()) True)
  (= 
    ($parse-tokens-write-message $S 
      (Cons  $X $Xs)) 
    ( (write $S $X) 
      (write $S ' ') 
      ($parse-tokens-write-message $S $Xs)))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          Term input/output (write)

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  !(public (, (/ write 1) (/ write 2)))
  !(public (, (/ writeq 1) (/ writeq 2)))
  !(public (, (/ write-canonical 1) (/ write-canonical 2)))
  !(public (, (/ write-term 2) (/ write-term 3)))


  (= 
    (write $Term) 
    ( (current-output $S) (write-term $S $Term (:: (numbervars True)))))

  (= 
    (write $S_or_a $Term) 
    (write-term $S_or_a $Term 
      (:: (numbervars True))))


  (= 
    (writeq $Term) 
    ( (current-output $S) (write-term $S $Term (:: (quoted True) (numbervars True)))))

  (= 
    (writeq $S_or_a $Term) 
    (write-term $S_or_a $Term 
      (:: 
        (quoted True) 
        (numbervars True))))


  (= 
    (write-canonical $Term) 
    ( (current-output $S) (write-term $S $Term (:: (quoted True) (ignore-ops True)))))

  (= 
    (write-canonical $S_or_a $Term) 
    (write-term $S_or_a $Term 
      (:: 
        (quoted True) 
        (ignore-ops True))))


  (= 
    (write-term $Term $Options) 
    ( (current-output $S) (write-term $S $Term $Options)))

  (= 
    (write-term $S_or_a $Term $Options) 
    ( ($write-term $S_or_a $Term $Options) (fail)))
  (= 
    (write_term  $_ $_ $_) True)


  (= 
    ($write-term $S_or_a $Term $Options) 
    ( ($write-term0 $Term 1200 punct $_ $Options $S_or_a) (set-det)))


  (= 
    ($write-term0 $Term $Prec $Type0 alpha $_ $S_or_a) 
    ( (var $Term) 
      (set-det) 
      ($write-space-if-needed $Type0 alpha $S_or_a) 
      ($fast-write $S_or_a $Term)))
  (= 
    ($write-term0 $Term $Prec $Type0 alpha $_ $S_or_a) 
    ( (java $Term) 
      (set-det) 
      ($write-space-if-needed $Type0 alpha $S_or_a) 
      ($fast-write $S_or_a $Term)))
  (= 
    ($write-term0 $Term $Prec $Type0 alpha $Style $S_or_a) 
    ( (= $Term $VN) 
      (integer $VN) 
      (>= $VN 0) 
      ($builtin-member 
        (numbervars True) $Style) 
      (set-det) 
      ($write-space-if-needed $Type0 alpha $S_or_a) 
      ($write-VAR $VN $S_or_a)))
  (= 
    ($write-term0 $Term $Prec $Type0 alpha $_ $S_or_a) 
    ( (number $Term) 
      (< $Term 0) 
      (set-det) 
      ($write-space-if-needed $Type0 symbol $S_or_a) 
      ($fast-write $S_or_a $Term)))
  (= 
    ($write-term0 $Term $Prec $Type0 alpha $_ $S_or_a) 
    ( (number $Term) 
      (set-det) 
      ($write-space-if-needed $Type0 alpha $S_or_a) 
      ($fast-write $S_or_a $Term)))
;
;                                         '$write_term0'(Term, Prec, Type0, punct, _, S_or_a) :-

;
;                                         	atom(Term),

;
;                                         	current_op(PrecOp, OpType, Term),

;
;                                         	(OpType = fx ; OpType = fy),

;
;                                         	PrecOp =< Prec,

;
;                                         	!,

;
;                                         	'$write_space_if_needed'(Type0, punct, S_or_a),

;
;                                         	put_char(S_or_a, '('),

;
;                                         	'$write_atom'(Term, punct, _, _, S_or_a),

;
;                                         	put_char(S_or_a, ')').

  (= 
    ($write-term0 $Term $Prec $Type0 $Type $Style $S_or_a) 
    ( (atom $Term) 
      (set-det) 
      ($write-atom $Term $Type0 $Type $Style $S_or_a)))
  (= 
    ($write-term0 $Term $Prec $Type0 $Type $Style $S_or_a) 
    ( (not ($builtin-member (ignore-ops True) $Style)) 
      ($write-is-operator $Term $Op $Args $OpType) 
      (set-det) 
      ($write-term-op $Op $OpType $Args $Prec $Type0 $Type $Style $S_or_a)))
  (= 
    ($write-term0 $Term $Prec $Type0 punct $Style $S_or_a) 
    ( (= $Term 
        (Cons  $_ $_)) 
      (not ($builtin-member (ignore-ops True) $Style)) 
      (set-det) 
      ($write-space-if-needed $Type0 punct $S_or_a) 
      (put-char $S_or_a [) 
      ($write-term-list-args $Term punct $_ $Style $S_or_a) 
      (put-char $S_or_a ])))
  (= 
    ($write-term0 $Term $Prec $Type0 $Type $Style $S_or_a) 
    ( (= $Term 
        {$Term1 }) 
      (not ($builtin-member (ignore-ops True) $Style)) 
      (set-det) 
      ($write-space-if-needed $Type0 punct $S_or_a) 
      (put-char $S_or_a {) 
      ($write-term0 $Term1 1200 punct $_ $Style $S_or_a) 
      (put-char $S_or_a })))
  (= 
    ($write-term0 $Term $Prec $Type0 punct $Style $S_or_a) 
    ( (=.. $Term 
        (Cons  $F $Args)) 
      ($write-atom $F $Type0 $_ $Style $S_or_a) 
      (put-char $S_or_a () 
      ($write-term-args $Args punct $_ $Style $S_or_a) 
      (put-char $S_or_a ))))


  (= 
    ($write-space-if-needed punct $_ $_) 
    (set-det))
  (= 
    ($write-space-if-needed $X $X $S_or_a) 
    ( (set-det) (put-char $S_or_a ' ')))
  (= 
    ($write-space-if-needed other alpha $S_or_a) 
    ( (set-det) (put-char $S_or_a ' ')))
  (= 
    ($write_space_if_needed  $_ $_ $_) True)


  (= 
    ($write-VAR $VN $S_or_a) 
    ( (< $VN 26) 
      (set-det) 
      (is $Letter 
        (+ 
          (mod $VN 26) "A")) 
      (put-code $S_or_a $Letter)))
  (= 
    ($write-VAR $VN $S_or_a) 
    ( (is $Letter 
        (+ 
          (mod $VN 26) "A")) 
      (put-code $S_or_a $Letter) 
      (is $Rest 
        (// $VN 26)) 
      ($fast-write $S_or_a $Rest)))


  (= 
    ($write-atom $Atom $Type0 $Type $Style $S_or_a) 
    ( ($builtin-member 
        (quoted True) $Style) 
      (set-det) 
      ($atom-type $Atom $Type) 
      ($write-space-if-needed $Type0 $Type $S_or_a) 
      ($fast-writeq $S_or_a $Atom)))
  (= 
    ($write-atom $Atom $Type0 $Type $_ $S_or_a) 
    ( ($atom-type $Atom $Type) 
      ($write-space-if-needed $Type0 $Type $S_or_a) 
      ($fast-write $S_or_a $Atom)))


  (= 
    ($atom-type $X alpha) 
    ( ($atom-type0 $X 0) (set-det)))
  (= 
    ($atom-type $X symbol) 
    ( ($atom-type0 $X 1) (set-det)))
  (= 
    ($atom-type $X punct) 
    ( ($atom-type0 $X 2) (set-det)))
  (= 
    ($atom-type $X other) 
    ( ($atom-type0 $X 3) (set-det)))


  (= 
    ($write-is-operator $Term $Op $Args $OpType) 
    ( (functor $Term $Op $Arity) 
      ($write-op-type $Arity $OpType) 
      (current-op $_ $OpType $Op) 
      (=.. $Term 
        (Cons  $_ $Args)) 
      (set-det)))


  (= 
    ($write_op_type  1 fx) True)
  (= 
    ($write_op_type  1 fy) True)
  (= 
    ($write_op_type  1 xf) True)
  (= 
    ($write_op_type  1 yf) True)
  (= 
    ($write_op_type  2 xfx) True)
  (= 
    ($write_op_type  2 xfy) True)
  (= 
    ($write_op_type  2 yfx) True)


  (= 
    ($write-term-op $Op $OpType $Args $Prec $Type0 punct $Style $S_or_a) 
    ( (current-op $PrecOp $OpType $Op) 
      (> $PrecOp $Prec) 
      (set-det) 
      ($write-space-if-needed $Type0 punct $S_or_a) 
      (put-char $S_or_a () 
      ($write-term-op1 $Op $OpType $Args $PrecOp punct $_ $Style $S_or_a) 
      (put-char $S_or_a ))))
  (= 
    ($write-term-op $Op $OpType $Args $Prec $Type0 $Type $Style $S_or_a) 
    ( (current-op $PrecOp $OpType $Op) ($write-term-op1 $Op $OpType $Args $PrecOp $Type0 $Type $Style $S_or_a)))


  (= 
    ($write-term-op1 $Op fx 
      (:: $A1) $PrecOp $Type0 $Type $Style $S_or_a) 
    ( (set-det) 
      ($write-atom $Op $Type0 $Type1 $Style $S_or_a) 
      (is $Prec1 
        (- $PrecOp 1)) 
      ($write-term0 $A1 $Prec1 $Type1 $Type $Style $S_or_a)))
  (= 
    ($write-term-op1 $Op fy 
      (:: $A1) $PrecOp $Type0 $Type $Style $S_or_a) 
    ( (set-det) 
      ($write-atom $Op $Type0 $Type1 $Style $S_or_a) 
      (is $Prec1 $PrecOp) 
      ($write-term0 $A1 $Prec1 $Type1 $Type $Style $S_or_a)))
  (= 
    ($write-term-op1 $Op xf 
      (:: $A1) $PrecOp $Type0 $Type $Style $S_or_a) 
    ( (set-det) 
      (is $Prec1 
        (- $PrecOp 1)) 
      ($write-term0 $A1 $Prec1 $Type0 $Type1 $Style $S_or_a) 
      ($write-atom $Op $Type1 $Type $Style $S_or_a)))
  (= 
    ($write-term-op1 $Op yf 
      (:: $A1) $PrecOp $Type0 $Type $Style $S_or_a) 
    ( (set-det) 
      (is $Prec1 $PrecOp) 
      ($write-term0 $A1 $Prec1 $Type0 $Type1 $Style $S_or_a) 
      ($write-atom $Op $Type1 $Type $Style $S_or_a)))
  (= 
    ($write-term-op1 $Op xfx 
      (:: $A1 $A2) $PrecOp $Type0 $Type $Style $S_or_a) 
    ( (set-det) 
      (is $Prec1 
        (- $PrecOp 1)) 
      (is $Prec2 
        (- $PrecOp 1)) 
      ($write-term0 $A1 $Prec1 $Type0 $Type1 $Style $S_or_a) 
      ($write-term-infix-op $Op $Type1 $Type2 $Style $S_or_a) 
      ($write-term0 $A2 $Prec2 $Type2 $Type $Style $S_or_a)))
  (= 
    ($write-term-op1 $Op xfy 
      (:: $A1 $A2) $PrecOp $Type0 $Type $Style $S_or_a) 
    ( (set-det) 
      (is $Prec1 
        (- $PrecOp 1)) 
      (is $Prec2 $PrecOp) 
      ($write-term0 $A1 $Prec1 $Type0 $Type1 $Style $S_or_a) 
      ($write-term-infix-op $Op $Type1 $Type2 $Style $S_or_a) 
      ($write-term0 $A2 $Prec2 $Type2 $Type $Style $S_or_a)))
  (= 
    ($write-term-op1 $Op yfx 
      (:: $A1 $A2) $PrecOp $Type0 $Type $Style $S_or_a) 
    ( (set-det) 
      (is $Prec1 $PrecOp) 
      (is $Prec2 
        (- $PrecOp 1)) 
      ($write-term0 $A1 $Prec1 $Type0 $Type1 $Style $S_or_a) 
      ($write-term-infix-op $Op $Type1 $Type2 $Style $S_or_a) 
      ($write-term0 $A2 $Prec2 $Type2 $Type $Style $S_or_a)))


  (= 
    ($write-term-infix-op , $Type0 punct $_ $S_or_a) 
    ( (set-det) 
      ($write-space-if-needed $Type0 punct $S_or_a) 
      (put-char $S_or_a ,)))
  (= 
    ($write-term-infix-op $Op $Type0 $Type $Style $S_or_a) 
    ($write-atom $Op $Type0 $Type $Style $S_or_a))


  (= 
    ($write-term-list-args 
      (Cons  $A $As) $Type0 $Type $Style $S_or_a) 
    ( (nonvar $As) 
      (= $As 
        (Cons  $_ $_)) 
      (set-det) 
      ($write-term0 $A 999 $Type0 $Type1 $Style $S_or_a) 
      ($write-space-if-needed $Type1 punct $S_or_a) 
      (put-char $S_or_a ,) 
      ($write-term-list-args $As punct $Type $Style $S_or_a)))

  (= 
    ($write-term-list-args 
      (Cons  $A $As) $Type0 $Type $Style $S_or_a) 
    ( (nonvar $As) 
      (= $As Nil) 
      (set-det) 
      ($write-term0 $A 999 $Type0 $Type $Style $S_or_a)))

  (= 
    ($write-term-list-args 
      (Cons  $A $As) $Type0 $Type $Style $S_or_a) 
    ( ($write-term0 $A 999 $Type0 $Type1 $Style $S_or_a) 
      ($write-space-if-needed $Type1 punct $S_or_a) 
      (put-char $S_or_a |) 
      ($write-term0 $As 999 punct $Type $Style $S_or_a)))


  (= 
    ($write-term-args Nil $Type $Type $_ $_) 
    (set-det))
  (= 
    ($write-term-args 
      (:: $A) $Type0 $Type $Style $S_or_a) 
    ( (set-det) ($write-term0 $A 999 $Type0 $Type $Style $S_or_a)))
  (= 
    ($write-term-args 
      (Cons  $A $As) $Type0 $Type $Style $S_or_a) 
    ( (set-det) 
      ($write-term0 $A 999 $Type0 $Type1 $Style $S_or_a) 
      ($write-space-if-needed $Type1 punct $S_or_a) 
      (put-char $S_or_a ,) 
      ($write-term-args $As punct $Type $Style $S_or_a)))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          Term input/output (others)

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  !(public (/ op 3))
  !(public (/ current-op 3))
  !(dynamic (/ %current-operator 3))


  (= 
    (op $Priority $Op_specifier $Operator) 
    ( (integer $Priority) 
      (=< 0 $Priority) 
      (=< $Priority 1200) 
      (set-det) 
      ($op1 $Priority $Op_specifier $Operator)))
  (= 
    (op $Priority $Op_specifier $Operator) 
    (illarg 
      (domain integer 
        (- 0 1200)) 
      (op $Priority $Op_specifier $Operator) 1))


  (= 
    ($op1 $Priority $Op_specifier $Operator) 
    ( (nonvar $Op_specifier) 
      ($op-specifier $Op_specifier $_) 
      (set-det) 
      ($op2 $Priority $Op_specifier $Operator)))
  (= 
    ($op1 $Priority $Op_specifier $Operator) 
    ( (findall $X 
        ($op-specifier $X $_) $Domain) (illarg (domain term $Domain) (op $Priority $Op_specifier $Operator) 2)))


  (= 
    ($op2 $Priority $Op_specifier $Operator) 
    ( (atom $Operator) 
      (set-det) 
      ($add-operators 
        (:: $Operator) $Priority $Op_specifier)))
  (= 
    ($op2 $Priority $Op_specifier $Operator) 
    ( ($op-atom-list $Operator $Atoms) 
      (set-det) 
      ($add-operators $Atoms $Priority $Op_specifier)))
  (= 
    ($op2 $Priority $Op_specifier $Operator) 
    (illarg 
      (type (list is-symbol)) 
      (op $Priority $Op_specifier $Operator) 3))


  (= 
    ($add-operators Nil $_ $_) 
    (set-det))
  (= 
    ($add-operators 
      (Cons  $A $As) $Priority $Op_specifier) 
    ( ($add-op $A $Priority $Op_specifier) ($add-operators $As $Priority $Op_specifier)))


  (= 
    ($add-op , $Priority $Op_specifier) 
    ( (set-det) (illarg (permission modify operator , $_) (op $Priority $Op_specifier ,) 3)))
  (= 
    ($add-op $A $_ $Op_specifier) 
    ( (get-atoms  &self 
        (= 
          ($current_operator  $_ $Op_specifier0 $A) $_)) 
      ($op-specifier $Op_specifier $Class) 
      ($op-specifier $Op_specifier0 $Class0) 
      (= $Class $Class0) 
      (remove-atom  &self 
        ($current_operator  $_ $Op_specifier0 $A)) 
      (fail)))
  (= 
    ($add-op $_ 0 $_) 
    (set-det))
  (= 
    ($add-op $A $Priority $Op_specifier) 
    (add-atom  &self 
      ($current_operator  $Priority $Op_specifier $A)))


  (= 
    ($op_specifier  fx prefix) True)
  (= 
    ($op_specifier  fy prefix) True)
  (= 
    ($op_specifier  xfx infix) True)
  (= 
    ($op_specifier  xfy infix) True)
  (= 
    ($op_specifier  yfx infix) True)
  (= 
    ($op_specifier  xf postfix) True)
  (= 
    ($op_specifier  yf postfix) True)


  (= 
    ($op-atom-list $X $_) 
    ( (var $X) 
      (set-det) 
      (fail)))
  (= 
    ($op-atom-list Nil Nil) 
    (set-det))
  (= 
    ($op-atom-list 
      (Cons  $X $Xs) 
      (Cons  $X $As)) 
    ( (atom $X) 
      (set-det) 
      ($op-atom-list $Xs $As)))


  (= 
    (current-op $Priority $Op_specifier $Operator) 
    (get-atoms  &self 
      (= 
        ($current_operator  $Priority $Op_specifier $Operator) $_)))


  (= 
    ($current_operator  1200 xfx :-) True)
  (= 
    ($current_operator  1200 xfx -->) True)
  (= 
    ($current_operator  1200 fx :-) True)
  (= 
    ($current_operator  1200 fx ?-) True)
  (= 
    ($current_operator  1150 fx package) True)
  (= 
    ($current_operator  1150 fx import) True)
  (= 
    ($current_operator  1150 fx include) True)
  (= 
    ($current_operator  1150 fx include_resource) True)
  (= 
    ($current_operator  1150 fx constant) True)
  (= 
    ($current_operator  1150 fx public) True)
  (= 
    ($current_operator  1150 fx dynamic) True)
  (= 
    ($current_operator  1150 fx meta_predicate) True)
  (= 
    ($current_operator  1150 fx mode) True)
  (= 
    ($current_operator  1150 fx multifile) True)
  (= 
    ($current_operator  1150 fx block) True)
  (= 
    ($current_operator  1150 fx ifdef) True)
  (= 
    ($current_operator  1150 fx ifndef) True)
  (= 
    ($current_operator  1150 fx domain) True)
  (= 
    ($current_operator  1150 fx database) True)
  (= 
    ($current_operator  1100 xfy ;) True)
  (= 
    ($current_operator  1050 xfy ->) True)
  (= 
    ($current_operator  1000 xfy ,) True)
  (= 
    ($current_operator  900 fy \+) True)
  (= 
    ($current_operator  700 xfx =) True)
  (= 
    ($current_operator  700 xfx \=) True)
  (= 
    ($current_operator  700 xfx ==) True)
  (= 
    ($current_operator  700 xfx \==) True)
  (= 
    ($current_operator  700 xfx @<) True)
  (= 
    ($current_operator  700 xfx @>) True)
  (= 
    ($current_operator  700 xfx @=<) True)
  (= 
    ($current_operator  700 xfx @>=) True)
  (= 
    ($current_operator  700 xfx =..) True)
  (= 
    ($current_operator  700 xfx is) True)
  (= 
    ($current_operator  700 xfx =:=) True)
  (= 
    ($current_operator  700 xfx =\=) True)
  (= 
    ($current_operator  700 xfx <) True)
  (= 
    ($current_operator  700 xfx >) True)
  (= 
    ($current_operator  700 xfx =<) True)
  (= 
    ($current_operator  700 xfx >=) True)
  (= 
    ($current_operator  550 xfy :) True)
  (= 
    ($current_operator  500 yfx +) True)
  (= 
    ($current_operator  500 yfx -) True)
  (= 
    ($current_operator  500 yfx #) True)
  (= 
    ($current_operator  500 yfx /\) True)
  (= 
    ($current_operator  500 yfx \/) True)
  (= 
    ($current_operator  500 fx +) True)
  (= 
    ($current_operator  400 yfx *) True)
  (= 
    ($current_operator  400 yfx /) True)
  (= 
    ($current_operator  400 yfx //) True)
  (= 
    ($current_operator  400 yfx mod) True)
  (= 
    ($current_operator  400 yfx rem) True)
  (= 
    ($current_operator  400 yfx <<) True)
  (= 
    ($current_operator  400 yfx >>) True)
  (= 
    ($current_operator  300 xfx ~) True)
  (= 
    ($current_operator  200 xfx **) True)
  (= 
    ($current_operator  200 xfy ^) True)
  (= 
    ($current_operator  200 fy \) True)
  (= 
    ($current_operator  200 fy -) True)

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          Logic and control

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  !(public (/ \+ 1))
  !(public (/ once 1))
  !(public (/ repeat 0))


  (= 
    (not $G) 
    ( (call $G) 
      (set-det) 
      (fail)))
  (= 
    (\+  $_) True)


  (= repeat True)
  (= 
    (repeat) 
    (repeat))


  (= 
    (once $G) 
    ( (call $G) (set-det)))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          Atomic term processing

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                         :- public atom_length/2.                  written in Java

;
;                                         :- public atom_concat/3.                  written in Java


  !(public (/ sub-symbol 5))
;
;                                         :- public atom_chars/2, atom_codes/2.     written in Java

;
;                                         :- public char_code/2.                    written in Java

;
;                                         :- public number_chars/2, number_codes/2. written in Java

  !(public (/ name 2))
;
;                                         :- public regex_compile/2.                written in Java

;
;                                         :- public regex_match/3.                  written in Java

  !(public (/ regex-matches 3))
  !(public (/ regex-matches 2))


  (= 
    (sub-atom $Atom $Before $Length $After $Sub_atom) 
    ( (atom-concat $AtomL $X $Atom) 
      (atom-length $AtomL $Before) 
      (atom-concat $Sub_atom $AtomR $X) 
      (atom-length $Sub_atom $Length) 
      (atom-length $AtomR $After)))


  (= 
    (name $Constant $Chars) 
    ( (nonvar $Constant) (det-if-then-else (number $Constant) (number-codes $Constant $Chars) (det-if-then-else (atomic $Constant) (atom-codes $Constant $Chars) (illarg (type symbolic) (name $Constant $Chars) 1)))))
  (= 
    (name $Constant $Chars) 
    ( (var $Constant) (det-if-then-else (number-codes $Constant0 $Chars) (= $Constant $Constant0) (det-if-then-else (atom-codes $Constant0 $Chars) (= $Constant $Constant0) (illarg (type (list char)) (name $Constant $Chars) 2)))))


  (= 
    (regex-matches $_ Nil $_) 
    ( (set-det) (fail)))
  (= 
    (regex-matches $Pattern $List $Result) 
    ( (= $List 
        (Cons  $_ $_)) 
      (set-det) 
      (regex-list $Pattern $List $Result)))
  (= 
    (regex-matches $Pattern $String $Result) 
    ( (atom $String) 
      (regex-compile $Pattern $Matcher) 
      (regex-match $Matcher $String $Result)))

  (= 
    (regex-matches $Pattern $String) 
    (once (regex-matches $Pattern $String $_)))


  (= 
    (regex-list $Pattern 
      (Cons  $H $_) $Result) 
    (regex-matches $Pattern $H $Result))
  (= 
    (regex-list $Pattern 
      (Cons  $_ $Ls) $Result) 
    (regex-list $Pattern $Ls $Result))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          Implementation defined hooks

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  !(public (/ set-prolog-flag 2))
  !(public (/ current-prolog-flag 2))


  (= 
    (set-prolog-flag $Flag $Value) 
    ( (var $Flag) 
      (set-det) 
      (illarg var 
        (set-prolog-flag $Flag $Value) 1)))
  (= 
    (set-prolog-flag $Flag $Value) 
    ( (var $Value) 
      (set-det) 
      (illarg var 
        (set-prolog-flag $Flag $Value) 2)))
  (= 
    (set-prolog-flag $Flag $Value) 
    ( (atom $Flag) 
      (set-det) 
      ($set-prolog-flag0 $Flag $Value)))
  (= 
    (set-prolog-flag $Flag $Value) 
    (illarg 
      (type is-symbol) 
      (set-prolog-flag $Flag $Value) 1))


  (= 
    ($set-prolog-flag0 $Flag $Value) 
    ( ($prolog-impl-flag $Flag $Mode 
        (changeable $YN)) 
      (set-det) 
      ($set-prolog-flag0 $YN $Flag $Value $Mode)))
  (= 
    ($set-prolog-flag0 $Flag $Value) 
    (illarg 
      (domain is-symbol prolog-flag) 
      (set-prolog-flag $Flag $Value) 1))

  (= 
    ($set-prolog-flag0 no $Flag $Value $_) 
    ( (set-det) (illarg (permission modify flag $Flag $_) (set-prolog-flag $Flag $Value) $_)))
  (= 
    ($set-prolog-flag0 $_ $Flag $Value $Mode) 
    ( ($builtin-member $Value $Mode) 
      (set-det) 
      ($set-prolog-impl-flag $Flag $Value)))
  (= 
    ($set-prolog-flag0 $_ $Flag $Value $_) 
    (illarg 
      (domain is-symbol flag-value) 
      (set-prolog-flag $Flag $Value) 2))


  (= 
    (current-prolog-flag $Flag $Term) 
    ( (var $Flag) 
      (set-det) 
      ($prolog-impl-flag $Flag $_ $_) 
      ($get-prolog-impl-flag $Flag $Term)))
  (= 
    (current-prolog-flag $Flag $Term) 
    ( (atom $Flag) 
      (set-det) 
      (det-if-then-else 
        ($prolog-impl-flag $Flag $_ $_) 
        ($get-prolog-impl-flag $Flag $Term) 
        (illarg 
          (domain is-symbol prolog-flag) 
          (current-prolog-flag $Flag $Term) 1))))
  (= 
    (current-prolog-flag $Flag $Term) 
    (illarg 
      (type is-symbol) 
      (current-prolog-flag $Flag $Term) 1))

;
;                                          '$MeTTa_impl_flag'(bounded,     _, changeable(no)).


  (= 
    ($prolog_impl_flag  max_integer $_ 
      (changeable  no)) True)
  (= 
    ($prolog_impl_flag  min_integer $_ 
      (changeable  no)) True)
;
;                                          '$MeTTa_impl_flag'(integer_rounding_function, [down,toward_zero], changeable(no)).

;
;                                          '$MeTTa_impl_flag'(char_conversion, [on,off], changeable(no)).

  (= 
    ($prolog_impl_flag  debug 
      (on off) 
      (changeable  yes)) True)
  (= 
    ($prolog_impl_flag  max_arity $_ 
      (changeable  no)) True)
  (= 
    ($prolog_impl_flag  unknown 
      (error fail warning) 
      (changeable  yes)) True)
  (= 
    ($prolog_impl_flag  double_quotes 
      (chars codes atom) 
      (changeable  no)) True)
  (= 
    ($prolog_impl_flag  print_stack_trace 
      (on off) 
      (changeable  yes)) True)


  !(public (/ halt 0))
  !(public (/ abort 0))


  (= 
    (halt) 
    (halt 0))

  (= 
    (abort) 
    (raise-exception 'Execution aborted'))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          DCG

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  !(public (, (/ C 3) (/ expand-term 2)))


  (= 
    (C  
      (Cons  $X $S) $X $S) True)


  (= 
    (expand-term $Dcg $Cl) 
    ( (var $Dcg) 
      (set-det) 
      (= $Dcg $Cl)))
  (= 
    (expand-term $Dcg $Cl) 
    ( ($dcg-expansion $Dcg $Cl0) 
      (set-det) 
      (= $Cl0 $Cl)))
  (= 
    (expand_term  $Dcg $Dcg) True)


  (= 
    ($dcg-expansion $Dcg $Cl) 
    ( (var $Dcg) 
      (set-det) 
      (= $Dcg $Cl)))
  (= 
    ($dcg-expansion 
      (--> $Head $B) 
      (= $H1 
        ($G1 $G2))) 
    ( (nonvar $Head) 
      (= $Head 
        (, $H $List)) 
      (= $List 
        (Cons  $_ $_)) 
      (set-det) 
      ($dcg-translation-atom $H $H1 $S0 $S1) 
      ($dcg-translation $B $G1 $S0 $S) 
      ($dcg-translation $List $G2 $S1 $S)))
  (= 
    ($dcg-expansion 
      (--> $H $B) 
      (= $H1 $B1)) 
    ( ($dcg-translation-atom $H $H1 $S0 $S) ($dcg-translation $B $B1 $S0 $S)))


  (= 
    ($dcg-translation-atom $X 
      (phrase $X $S0 $S) $S0 $S) 
    ( (var $X) (set-det)))
  (= 
    ($dcg-translation-atom 
      (with_self  $M $X) 
      (with_self  $M $X1) $S0 $S) 
    ( (set-det) ($dcg-translation-atom $X $X1 $S0 $S)))
  (= 
    ($dcg-translation-atom $X $X1 $S0 $S) 
    ( (=.. $X 
        (Cons  $F $As)) 
      ($builtin-append $As 
        (:: $S0 $S) $As1) 
      (=.. $X1 
        (Cons  $F $As1))))


  (= 
    ($dcg-translation $X $Y $S0 $S) 
    ( ($dcg-trans $X $Y0 $T $S0 $S) ($dcg-trans0 $Y0 $Y $T $S0 $S)))


  (= 
    ($dcg-trans0 $Y $Y $T $S0 $T) 
    ( (\== $T $S0) (set-det)))
  (= 
    ($dcg-trans0 $Y0 $Y $T $_ $S) 
    ($dcg-concat $Y0 
      (= $S $T) $Y))


  (= 
    ($dcg-concat $X $Y $Z) 
    ( (== $X True) 
      (set-det) 
      (= $Z $Y)))
  (= 
    ($dcg-concat $X $Y $Z) 
    ( (== $Y True) 
      (set-det) 
      (= $Z $X)))
  (= 
    ($dcg_concat  $X $Y 
      (,  $X $Y)) True)


  (= 
    ($dcg-trans $X $X1 $S $S0 $S) 
    ( (var $X) 
      (set-det) 
      ($dcg-translation-atom $X $X1 $S0 $S)))
  (= 
    ($dcg-trans 
      (with_self  $M $X) 
      (with_self  $M $Y) $T $S0 $S) 
    ( (set-det) ($dcg-trans $X $Y $T $S0 $S)))
  (= 
    ($dcg-trans Nil True $S0 $S0 $_) 
    (set-det))
  (= 
    ($dcg-trans 
      (Cons  $X $Y) $Z $T $S0 $S) 
    ( (set-det) 
      ($dcg-trans $Y $Y1 $T $S1 $S) 
      ($dcg-concat 
        (C $S0 $X $S1) $Y1 $Z)))
  (= 
    ($dcg-trans 
      (not $X) 
      (det-if-then-else $X1 fail 
        (= $S $S0)) $S $S0 $S) 
    ( (set-det) ($dcg-trans $X $X1 $S1 $S0 $S1)))
  (= 
    ($dcg-trans 
      (, $X $Y) $Z $T $S0 $S) 
    ( (set-det) 
      ($dcg-trans $X $X1 $S1 $S0 $S1) 
      ($dcg-trans $Y $Y1 $T $S1 $S) 
      ($dcg-concat $X1 $Y1 $Z)))
  (= 
    ($dcg-trans 
      (det-if-then $X $Y) 
      (det-if-then $X1 $Y1) $T $S0 $S) 
    ( (set-det) 
      ($dcg-trans $X $X1 $S1 $S0 $S1) 
      ($dcg-trans $Y $Y1 $T $S1 $S)))
  (= 
    ($dcg-trans 
      (or $X $Y) 
      (or $X1 $Y1) $S $S0 $S) 
    ( (set-det) 
      ($dcg-translation $X $X1 $S0 $S) 
      ($dcg-translation $Y $Y1 $S0 $S)))
  (= 
    ($dcg-trans 
      (set-det) 
      (set-det) $S0 $S0 $_) 
    (set-det))
  (= 
    ($dcg-trans 
      {$G } 
      (call $G) $S0 $S0 $_) 
    ( (var $G) (set-det)))
  (= 
    ($dcg-trans 
      {$G } $G $S0 $S0 $_) 
    (set-det))
  (= 
    ($dcg-trans $X $X1 $S $S0 $S) 
    ($dcg-translation-atom $X $X1 $S0 $S))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          Hash creation and control

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  !(public (/ new-hash 1))
  !(public (/ hash-map 2))
  !(public (/ hash-exists 1))


  (= 
    (new-hash $Hash) 
    (new-hash $Hash Nil))


  (= 
    (hash-map $H_or_a $List) 
    ( (hash-keys $H_or_a $Ks0) 
      (sort $Ks0 $Ks) 
      (hash-map $Ks $List $H_or_a)))

  (= 
    (hash-map Nil Nil $_) 
    (set-det))
  (= 
    (hash-map 
      (Cons  $K $Ks) 
      (Cons  
        (, $K $V) $Ls) $H_or_a) 
    ( (hash-get $H_or_a $K $V) (hash-map $Ks $Ls $H_or_a)))


  (= 
    (hash-exists $Alias) 
    ( (atom $Alias) 
      ($get-hash-manager $HM) 
      (hash-contains-key $HM $Alias)))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          Java interoperation

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                         :- public java_constructor0/2.            (written in Java)

;
;                                         :- public java_declared_constructor0/2.   (written in Java)

;
;                                         :- public java_method0/3.                 (written in Java)

;
;                                         :- public java_declared_method0/3.        (written in Java)

;
;                                         :- public java_get_field0/3.              (written in Java)

;
;                                         :- public java_get_declared_field0/3.     (written in Java)

;
;                                         :- public java_set_field0/3.              (written in Java)

;
;                                         :- public java_set_declared_field0/3.     (written in Java)

;
;                                         :- public java_conversion/2.              (written in Java)


  !(public (/ java-constructor 2))
  !(public (/ java-declared-constructor 2))
  !(public (/ java-method 3))
  !(public (/ java-declared-method 3))
  !(public (/ java-get-field 3))
  !(public (/ java-get-declared-field 3))
  !(public (/ java-set-field 3))
  !(public (/ java-set-declared-field 3))
  !(public (/ synchronized 2))


  (= 
    (java-constructor $Constr $Instance) 
    ( (=.. $Constr 
        (Cons  $F $As)) 
      (builtin-java-convert-args $As $As1) 
      (=.. $Constr1 
        (Cons  $F $As1)) 
      (java-constructor0 $Constr1 $Instance1) 
      (= $Instance $Instance1)))


  (= 
    (java-declared-constructor $Constr $Instance) 
    ( (=.. $Constr 
        (Cons  $F $As)) 
      (builtin-java-convert-args $As $As1) 
      (=.. $Constr1 
        (Cons  $F $As1)) 
      (java-declared-constructor0 $Constr1 $Instance1) 
      (= $Instance $Instance1)))


  (= 
    (java-method $Class_or_Instance $Method $Value) 
    ( (=.. $Method 
        (Cons  $F $As)) 
      (builtin-java-convert-args $As $As1) 
      (=.. $Method1 
        (Cons  $F $As1)) 
      (java-method0 $Class_or_Instance $Method1 $Value1) 
      (java-conversion $Value2 $Value1) 
      (= $Value $Value2)))


  (= 
    (java-declared-method $Class_or_Instance $Method $Value) 
    ( (=.. $Method 
        (Cons  $F $As)) 
      (builtin-java-convert-args $As $As1) 
      (=.. $Method1 
        (Cons  $F $As1)) 
      (java-declared-method0 $Class_or_Instance $Method1 $Value1) 
      (java-conversion $Value2 $Value1) 
      (= $Value $Value2)))


  (= 
    (java-get-field $Class_or_Instance $Field $Value) 
    ( (java-get-field0 $Class_or_Instance $Field $Value1) 
      (java-conversion $Value2 $Value1) 
      (= $Value $Value2)))


  (= 
    (java-get-declared-field $Class_or_Instance $Field $Value) 
    ( (java-get-declared-field0 $Class_or_Instance $Field $Value1) 
      (java-conversion $Value2 $Value1) 
      (= $Value $Value2)))


  (= 
    (java-set-field $Class_or_Instance $Field $Value) 
    ( (java-conversion $Value $Value1) (java-set-field0 $Class_or_Instance $Field $Value1)))


  (= 
    (java-set-declared-field $Class_or_Instance $Field $Value) 
    ( (java-conversion $Value $Value1) (java-set-declared-field0 $Class_or_Instance $Field $Value1)))


  (= 
    (builtin-java-convert-args Nil Nil) 
    (set-det))
  (= 
    (builtin-java-convert-args 
      (Cons  $X $Xs) 
      (Cons  $Y $Ys)) 
    ( (java-conversion $X $Y) (builtin-java-convert-args $Xs $Ys)))


  (= 
    (synchronized $Object $Goal) 
    ( ($begin-sync $Object $Ref) 
      (call $Goal) 
      ($end-sync $Ref)))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          MeTTa interpreter

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  !(op 1170 xfx :-)
  !(op 1170 xfx -->)
  !(op 1170 fx :-)
  !(op 1170 fx ?-)

  !(op 1150 fx package)
  !(op 1150 fx import)
  !(op 1150 fx public)
  !(op 1150 fx dynamic)
  !(op 1150 fx meta-predicate)
  !(op 1150 fx mode)
  !(op 1150 fx multifile)
  !(op 1150 fx block)

  !(public (/ cafeteria 0))
  !(public (/ consult 1))
  !(public (/ consult-stream 1))
  !(public (, (/ trace 0) (/ notrace 0)))
  !(public (, (/ debug 0) (/ nodebug 0)))
  !(public (/ leash 1))
  !(public (, (/ spy 1) (/ nospy 1) (/ nospyall 0)))
  !(public (/ listing 0))
  !(public (/ listing 1))

  !(dynamic (/ %current-leash 1))
  !(dynamic (/ %current-spypoint 3))
  !(dynamic (/ %leap-flag 1))
  !(dynamic (/ %consulted-file 1))
  !(dynamic (/ %consulted-import 2))
  !(dynamic (/ %consulted-package 1))
  !(dynamic (/ %consulted-predicate 3))

;
;                                         ;; Main


  (= 
    (cafeteria) 
    ( (%cafeteria-init) 
      (repeat) 
      (%toplvel-loop) 
      (on-exception $Msg 
        ($cafeteria $Goal) 
        (print-message error $Msg)) 
      (== $Goal end-of-file) 
      (set-det) 
      (nl) 
      ($fast-write bye) 
      (nl)))


  (= 
    (%cafeteria-init) 
    ( (remove-all-atoms  &self 
        ($leap_flag  $_)) 
      (remove-all-atoms  &self 
        ($current_leash  $_)) 
      (remove-all-atoms  &self 
        ($current_spypoint  $_ $_ $_)) 
      (remove-all-atoms  &self 
        ($consulted_file  $_)) 
      (remove-all-atoms  &self 
        ($consulted_package  $_)) 
      (remove-all-atoms  &self 
        ($consulted_predicate  $_ $_ $_)) 
      (add-atom  &self 
        ($leap_flag  no)) 
      (add-atom  &self 
        ($current_leash  call)) 
      (add-atom  &self 
        ($current_leash  exit)) 
      (add-atom  &self 
        ($current_leash  redo)) 
      (add-atom  &self 
        ($current_leash  fail)) 
      (set-det)))


  (= 
    (%toplvel-loop) 
    ( (current-prolog-flag debug $Mode) 
      (det-if-then-else 
        (== $Mode off) True 
        (print-message info 
          (:: debug))) 
      ($fast-write | ?- ) 
      (flush-output)))


  (= 
    ($cafeteria $Goal) 
    ( (read-with-variables $Goal $Vars) ($process-order $Goal $Vars)))


  (= 
    ($process-order $G $_) 
    ( (var $G) 
      (set-det) 
      (illarg var 
        (?- $G) 1)))
  (= 
    ($process-order end-of-file $_) 
    (set-det))
  (= 
    ($process-order 
      (Cons  $File $Files) $_) 
    ( (set-det) (consult (Cons  $File $Files))))
  (= 
    ($process-order $G $Vars) 
    ( (current-prolog-flag debug $Mode) 
      (det-if-then-else 
        (== $Mode off) 
        (call $G) 
        ($trace-goal $G)) 
      (nl) 
      ($rm-redundant-vars $Vars $Vars1) 
      ($give-answers-with-prompt $Vars1) 
      (set-det) 
      ($fast-write yes) 
      (nl)))
  (= 
    ($process-order $_ $_) 
    ( (nl) 
      ($fast-write no) 
      (nl)))


  (= 
    ($rm-redundant-vars Nil Nil) 
    (set-det))
  (= 
    ($rm-redundant-vars 
      (Cons  
        (= - $_) $Xs) $Vs) 
    ( (set-det) ($rm-redundant-vars $Xs $Vs)))
  (= 
    ($rm-redundant-vars 
      (Cons  $X $Xs) 
      (Cons  $X $Vs)) 
    ($rm-redundant-vars $Xs $Vs))


  (= 
    ($give-answers-with-prompt Nil) 
    (set-det))
  (= 
    ($give-answers-with-prompt $Vs) 
    ( ($give-an-answer $Vs) 
      ($fast-write  ? ) 
      (flush-output) 
      (read-line $Str) 
      (\== $Str ";") 
      (nl)))


  (= 
    ($give-an-answer Nil) 
    ( (set-det) ($fast-write True)))
  (= 
    ($give-an-answer (:: $X)) 
    ( (set-det) ('$print-an answer' $X)))
  (= 
    ($give-an-answer (Cons  $X $Xs)) 
    ( ('$print-an answer' $X) 
      ($fast-write ,) 
      (nl) 
      ($give-an-answer $Xs)))


  (= 
    ('$print-an answer' (= $N $V)) 
    ( (write $N) 
      ($fast-write  = ) 
      (writeq $V)))

;
;                                         ;; Read Program


  (= 
    (consult $Files) 
    ( (var $Files) 
      (set-det) 
      (illarg var 
        (consult $Files) 1)))
  (= 
    (consult Nil) 
    (set-det))
  (= 
    (consult (Cons  $File $Files)) 
    ( (set-det) 
      (consult $File) 
      (consult $Files)))
  (= 
    (consult $File) 
    ( (atom $File) 
      (set-det) 
      ($consult $File)))


  (= 
    ($consult $F) 
    ( ($prolog-file-name $F $PF) 
      (open $PF read $In) 
      (stream-property $In 
        (file-name $File)) 
      (print-message info 
        (:: consulting $File ...)) 
      (statistics runtime $_) 
      (consult-stream $File $In) 
      (statistics runtime 
        (:: $_ $T)) 
      (print-message info 
        (:: $File consulted $T msec)) 
      (close $In)))


  (= 
    (consult-stream $File $In) 
    ( ($consult-init $File) 
      (repeat) 
      (read $In $Cl) 
      ($consult-clause $Cl) 
      (== $Cl end-of-file) 
      (set-det)))


  (= 
    ($prolog-file-name $File $File) 
    ( (sub-atom $File $_ $_ $After .) 
      (> $After 0) 
      (set-det)))
  (= 
    ($prolog-file-name $File0 $File) 
    (atom-concat $File0 .pl $File))


  (= 
    ($consult-init $File) 
    ( (remove-all-atoms  &self 
        ($consulted_file  $_)) 
      (remove-all-atoms  &self 
        ($consulted_package  $_)) 
      (remove-all-atoms  &self 
        ($consulted_import  $File $_)) 
      (remove-atom  &self 
        ($consulted_predicate  $P $PI $File)) 
      (abolish (with_self  $P $PI)) 
      (fail)))
  (= 
    ($consult-init $File) 
    ( (add-atom  &self 
        ($consulted_file  $File)) (add-atom  &self ($consulted_package  user))))


  (= 
    ($consult-clause end-of-file) 
    (set-det))
  (= 
    ($consult-clause !(module $P $_)) 
    ( (set-det) ($assert-consulted-package $P)))
  (= 
    ($consult-clause !(package $P)) 
    ( (set-det) ($assert-consulted-package $P)))
  (= 
    ($consult-clause !(import $P)) 
    ( (set-det) ($assert-consulted-import $P)))
  (= 
    ($consult-clause !(dynamic $_)) 
    (set-det))
  (= 
    ($consult-clause !(public $_)) 
    (set-det))
  (= 
    ($consult-clause !(meta-predicate $_)) 
    (set-det))
  (= 
    ($consult-clause !(mode $_)) 
    (set-det))
  (= 
    ($consult-clause !(multifile $_)) 
    (set-det))
  (= 
    ($consult-clause !(block $_)) 
    (set-det))
  (= 
    ($consult-clause !$G) 
    ( (set-det) 
      (get-atoms  &self 
        (= 
          ($consulted_package  $P) $_)) 
      (once (with_self  $P $G))))
  (= 
    ($consult-clause $Clause0) 
    ( ($consult-preprocess $Clause0 $Clause) ($consult-cls $Clause)))


  (= 
    ($assert-consulted-package $P) 
    ( (get-atoms  &self 
        (= 
          ($consulted_package  $P) $_)) (set-det)))
  (= 
    ($assert-consulted-package $P) 
    ( (remove-all-atoms  &self 
        ($consulted_package  $_)) (add-atom  &self ($consulted_package  $P))))


  (= 
    ($assert-consulted-import $P) 
    ( (get-atoms  &self 
        (= 
          ($consulted_file  $File) $_)) (add-atom  &self ($consulted_import  $File $P))))


  (= 
    ($consult-preprocess $Clause0 $Clause) 
    (expand-term $Clause0 $Clause))


  (= 
    ($consult-cls (= $H $G)) 
    ( (set-det) ($assert-consulted-clause (= $H $G))))
  (= 
    ($consult-cls $H) 
    ($assert-consulted-clause (= $H True)))


  (= 
    ($assert-consulted-clause $Clause) 
    ( (= $Clause 
        (= $H $_)) 
      (functor $H $F $A) 
      (get-atoms  &self 
        (= 
          ($consulted_file  $File) $_)) 
      (get-atoms  &self 
        (= 
          ($consulted_package  $P) $_)) 
      (add-atom  &self 
        (:  $P $Clause)) 
      (add-atom  &self 
        ($consulted_predicate  $P 
          (/  $F $A) $File)) 
      (set-det)))

;
;                                         ;; Trace


  (= 
    (trace) 
    ( (current-prolog-flag debug on) (set-det)))
  (= 
    (trace) 
    ( (set-prolog-flag debug on) 
      (%trace-init) 
      ($fast-write '{Small debugger is switch on}') 
      (nl) 
      (set-det)))


  (= 
    (%trace-init) 
    ( (remove-all-atoms  &self 
        ($leap_flag  $_)) 
      (remove-all-atoms  &self 
        ($current_leash  $_)) 
      (remove-all-atoms  &self 
        ($current_spypoint  $_ $_ $_)) 
      (add-atom  &self 
        ($leap_flag  no)) 
      (add-atom  &self 
        ($current_leash  call)) 
      (add-atom  &self 
        ($current_leash  exit)) 
      (add-atom  &self 
        ($current_leash  redo)) 
      (add-atom  &self 
        ($current_leash  fail)) 
      (set-det)))


  (= 
    (notrace) 
    ( (current-prolog-flag debug off) (set-det)))
  (= 
    (notrace) 
    ( (set-prolog-flag debug off) 
      ($fast-write '{Small debugger is switch off}') 
      (nl) 
      (set-det)))


  (= 
    (debug) 
    (trace))

  (= 
    (nodebug) 
    (notrace))

;
;                                         ;; Spy-Points


  (= 
    (spy $T) 
    ( ($term-to-predicateindicator $T $PI 
        (spy $T)) 
      (trace) 
      ($assert-spypoint $PI) 
      ($set-debug-flag leap yes) 
      (set-det)))


  (= 
    ($assert-spypoint (with_self  $P (/ $F $A))) 
    ( (get-atoms  &self 
        (= 
          ($current_spypoint  $P $F $A) $_)) 
      (print-message info 
        (:: spypoint 
          (with_self  $P 
            (/ $F $A)) is already added)) 
      (set-det)))
  (= 
    ($assert-spypoint (with_self  $P (/ $F $A))) 
    ( (get-atoms  &self 
        (= 
          ($consulted_predicate  $P 
            (/  $F $A) $_) $_)) 
      (add-atom  &self 
        ($current_spypoint  $P $F $A)) 
      (print-message info 
        (:: spypoint 
          (with_self  $P 
            (/ $F $A)) is added)) 
      (set-det)))
  (= 
    ($assert-spypoint (with_self  $P (/ $F $A))) 
    (print-message warning 
      (:: no matching predicate for spy 
        (with_self  $P 
          (/ $F $A)))))


  (= 
    (nospy $T) 
    ( ($term-to-predicateindicator $T $PI 
        (nospy $T)) 
      ($retract-spypoint $PI) 
      ($set-debug-flag leap no) 
      (set-det)))


  (= 
    ($retract-spypoint (with_self  $P (/ $F $A))) 
    ( (remove-atom  &self 
        ($current_spypoint  $P $F $A)) 
      (print-message info 
        (:: spypoint 
          (with_self  $P 
            (/ $F $A)) is removed)) 
      (set-det)))
  (= 
    ($retract_spypoint  $_) True)


  (= 
    (nospyall) 
    ( (remove-all-atoms  &self 
        ($current_spypoint  $_ $_ $_)) ($set-debug-flag leap no)))

;
;                                         ;; Leash


  (= 
    (leash $L) 
    ( (nonvar $L) 
      ($leash $L) 
      (set-det)))
  (= 
    (leash $L) 
    (illarg 
      (type leash-specifier) 
      (leash $L) 1))


  (= 
    ($leash Nil) 
    ( (set-det) 
      (remove-all-atoms  &self 
        ($current_leash  $_)) 
      (print-message info 
        (:: no leashing))))
  (= 
    ($leash $Ms) 
    ( (remove-all-atoms  &self 
        ($current_leash  $_)) 
      ($assert-leash $Ms) 
      (print-message info 
        (:: leashing stopping on $Ms))))


  (= 
    ($assert-leash Nil) 
    (set-det))
  (= 
    ($assert-leash (Cons  $X $Xs)) 
    ( ($leash-specifier $X) 
      (add-atom  &self 
        ($current_leash  $X)) 
      ($assert-leash $Xs)))


  (= 
    ($leash_specifier  call) True)
  (= 
    ($leash_specifier  exit) True)
  (= 
    ($leash_specifier  redo) True)
  (= 
    ($leash_specifier  fail) True)
;
;                                         '$leash_specifier'(exception).


;
;                                         ;; Trace a Goal


  (= 
    ($trace-goal $Term) 
    ( ($set-debug-flag leap no) 
      ($get-current-B $Cut) 
      ($meta-call $Term user $Cut 0 trace)))

  (= 
    ($trace-goal $X $P $FA $Depth) 
    ( (print-procedure-box call $X $P $FA $Depth) 
      ($call-internal $X $P $FA $Depth trace) 
      (print-procedure-box exit $X $P $FA $Depth) 
      (redo-procedure-box $X $P $FA $Depth)))
  (= 
    ($trace-goal $X $P $FA $Depth) 
    ( (print-procedure-box fail $X $P $FA $Depth) (fail)))


  (= 
    (print-procedure-box $Mode $G $P 
      (/ $F $A) $Depth) 
    ( (get-atoms  &self 
        (= 
          ($current_spypoint  $P $F $A) $_)) 
      (set-det) 
      ($builtin-message (:: + $Depth $Mode : (with_self  $P $G))) 
      ($read-blocked (print-procedure-box $Mode $G $P (/ $F $A) $Depth))))
  (= 
    (print-procedure-box $Mode $G $P $FA $Depth) 
    ( (get-atoms  &self 
        (= 
          ($leap_flag  no) $_)) 
      (set-det) 
      ($builtin-message (:: ' ' $Depth $Mode : (with_self  $P $G))) 
      (det-if-then-else 
        (get-atoms  &self 
          (= 
            ($current_leash  $Mode) $_)) 
        ($read-blocked (print-procedure-box $Mode $G $P $FA $Depth)) nl)))
  (= 
    (print_procedure_box  $_ $_ $_ $_ $_) True)


  (= 
    (redo_procedure_box  $_ $_ $_ $_) True)
  (= 
    (redo-procedure-box $X $P $FA $Depth) 
    ( (print-procedure-box redo $X $P $FA $Depth) (fail)))


  (= 
    ($read-blocked $G) 
    ( ($fast-write  ? ) 
      (flush-output) 
      (read-line $C) 
      (det-if-then-else 
        (== $C Nil) 
        (= $DOP 99) 
        (= $C 
          (Cons  $DOP $_))) 
      ($debug-option $DOP $G)))


  (= 
    ($debug-option 97 $_) 
    ( (set-det) 
      (notrace) 
      (abort)))               ;
;                                          a for abort

  (= 
    ($debug-option 99 $_) 
    ( (set-det) ($set-debug-flag leap no)))  ;
;                                          c for creep

  (= 
    ($debug-option 108 $_) 
    ( (set-det) ($set-debug-flag leap yes))) ;
;                                          l for leap

  (= 
    ($debug-option 43 
      (print-procedure-box $Mode $G $P $FA $Depth)) 
    ( (set-det) 
      (spy (with_self  $P $FA)) 
      (call (print-procedure-box $Mode $G $P $FA $Depth))))
;                                         ; + for spy this


  (= 
    ($debug-option 45 
      (print-procedure-box $Mode $G $P $FA $Depth)) 
    ( (set-det) 
      (nospy (with_self  $P $FA)) 
      (call (print-procedure-box $Mode $G $P $FA $Depth))))
;                                         ; - for nospy this


  (= 
    ($debug-option 63 $G) 
    ( (set-det) 
      (%show-debug-option) 
      (call $G)))
  (= 
    ($debug-option 104 $G) 
    ( (set-det) 
      (%show-debug-option) 
      (call $G)))
  (= 
    ($debug_option  $_ $_) True)


  (= 
    (%show-debug-option) 
    ( (tab 4) 
      ($fast-write 'Debuggin options:') 
      (nl) 
      (tab 4) 
      ($fast-write 'a      abort') 
      (nl) 
      (tab 4) 
      ($fast-write 'RET    creep') 
      (nl) 
      (tab 4) 
      ($fast-write 'c      creep') 
      (nl) 
      (tab 4) 
      ($fast-write 'l      leap') 
      (nl) 
      (tab 4) 
      ($fast-write '+      spy this') 
      (nl) 
      (tab 4) 
      ($fast-write '-      nospy this') 
      (nl) 
      (tab 4) 
      ($fast-write '?      help') 
      (nl) 
      (tab 4) 
      ($fast-write 'h      help') 
      (nl)))


  (= 
    ($set-debug-flag leap $Flag) 
    ( (get-atoms  &self 
        (= 
          ($leap_flag  $Flag) $_)) (set-det)))
  (= 
    ($set-debug-flag leap $Flag) 
    ( (remove-all-atoms  &self 
        ($leap_flag  $_)) (add-atom  &self ($leap_flag  $Flag))))

;
;                                         ;; Listing


  (= 
    (listing) 
    ($listing $_ user))

  (= 
    (listing $T) 
    ( (var $T) 
      (set-det) 
      (illarg var 
        (listing $T) 1)))
  (= 
    (listing $P) 
    ( (atom $P) 
      (set-det) 
      ($listing $_ $P)))
  (= 
    (listing (/ $F $A)) 
    ( (set-det) ($listing (/ $F $A) user)))
  (= 
    (listing (with_self  $P $PI)) 
    ( (atom $P) 
      (set-det) 
      ($listing $PI $P)))
  (= 
    (listing $T) 
    (illarg 
      (type predicate-indicator) 
      (listing $T) 1))


  (= 
    ($listing $PI $P) 
    ( (var $PI) 
      (set-det) 
      ($listing-dynamic-clause $P $_)))
  (= 
    ($listing 
      (/ $F $A) $P) 
    ( (atom $F) 
      (integer $A) 
      (set-det) 
      ($listing-dynamic-clause $P 
        (/ $F $A))))
  (= 
    ($listing $PI $P) 
    (illarg 
      (type predicate-indicator) 
      (listing (with_self  $P $PI)) 1))


  (= 
    ($listing-dynamic-clause $P $PI) 
    ( ($new-internal-database $P) 
      (hash-keys $P $Keys) 
      ($builtin-member $PI $Keys) 
      (= $PI 
        (/ $F $A)) 
      (functor $H $F $A) 
      ($clause-internal $P $PI $H $Cl $_) 
      ($write-dynamic-clause $P $Cl) 
      (fail)))
  (= 
    ($listing_dynamic_clause  $_ $_) True)


  (= 
    ($write-dynamic-clause $_ $Cl) 
    ( (var $Cl) 
      (set-det) 
      (fail)))
  (= 
    ($write-dynamic-clause $P 
      (= $H True)) 
    ( (set-det) 
      (numbervars $H 0 $_) 
      ($write-dynamic-head $P $H) 
      (write .) 
      (nl)))
  (= 
    ($write-dynamic-clause $P 
      (= $H $B)) 
    ( (set-det) 
      (numbervars 
        (= $H $B) 0 $_) 
      ($write-dynamic-head $P $H) 
      (write  :-) 
      (nl) 
      ($write-dynamic-body $B 8) 
      (write .) 
      (nl)))


  (= 
    ($write-dynamic-head user $H) 
    ( (set-det) (writeq $H)))
  (= 
    ($write-dynamic-head $P $H) 
    ( (write $P) 
      (write :) 
      (writeq $H)))


  (= 
    ($write-dynamic-body 
      (, $G1 $G2) $N) 
    ( (set-det) 
      ($write-dynamic-body $G1 $N) 
      (write ,) 
      (nl) 
      ($write-dynamic-body $G2 $N)))
  (= 
    ($write-dynamic-body 
      (or $G1 $G2) $N) 
    ( (set-det) 
      (is $N1 
        (+ $N 4)) 
      (tab $N) 
      (write () 
      (nl) 
      ($write-dynamic-body $G1 $N1) 
      (nl) 
      (tab $N) 
      (write or) 
      (nl) 
      ($write-dynamic-body $G2 $N1) 
      (nl) 
      (tab $N) 
      (write ))))
  (= 
    ($write-dynamic-body 
      (det-if-then $G1 $G2) $N) 
    ( (set-det) 
      (is $N1 
        (+ $N 4)) 
      (tab $N) 
      (write () 
      (nl) 
      ($write-dynamic-body $G1 $N1) 
      (nl) 
      (tab $N) 
      (write ->) 
      (nl) 
      ($write-dynamic-body $G2 $N1) 
      (nl) 
      (tab $N) 
      (write ))))
  (= 
    ($write-dynamic-body $B $N) 
    ( (tab $N) (writeq $B)))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          Misc

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  !(public (/ reverse 2))
  !(public (/ length 2))
  !(public (/ numbervars 3))
  !(public (/ statistics 2))

;
;                                         reverse(Xs, Zs) :- reverse(Xs, [], Zs).

;
;                                         reverse([], Zs, Zs).

;
;                                         reverse([X|Xs], Tmp, Zs) :- reverse(Xs, [X|Tmp], Zs).



  (= 
    (length $L $N) 
    ( (var $N) 
      (set-det) 
      ($length $L 0 $N)))
  (= 
    (length $L $N) 
    ($length0 $L 0 $N))


  (= 
    ($length  () $I $I) True)
  (= 
    ($length 
      (Cons  $_ $L) $I0 $I) 
    ( (is $I1 
        (+ $I0 1)) ($length $L $I1 $I)))


  (= 
    ($length0 Nil $I $I) 
    (set-det))
  (= 
    ($length0 
      (Cons  $_ $L) $I0 $I) 
    ( (< $I0 $I) 
      (is $I1 
        (+ $I0 1)) 
      ($length0 $L $I1 $I)))


  (= 
    (numbervars $X $VI $VN) 
    ( (integer $VI) 
      (>= $VI 0) 
      (set-det) 
      ($numbervars $X $VI $VN)))


  (= 
    ($numbervars $X $VI $VN) 
    ( (var $X) 
      (set-det) 
      (= $X $VI) 
      (is $VN 
        (+ $VI 1))))
;                                         ; This structure is checked in write


  (= 
    ($numbervars $X $VI $VI) 
    ( (atomic $X) (set-det)))
  (= 
    ($numbervars $X $VI $VI) 
    ( (java $X) (set-det)))
  (= 
    ($numbervars $X $VI $VN) 
    ( (functor $X $_ $N) ($numbervars-str 1 $N $X $VI $VN)))


  (= 
    ($numbervars-str $I $I $X $VI $VN) 
    ( (set-det) 
      (arg $I $X $A) 
      ($numbervars $A $VI $VN)))
  (= 
    ($numbervars-str $I $N $X $VI $VN) 
    ( (arg $I $X $A) 
      ($numbervars $A $VI $VN1) 
      (is $I1 
        (+ $I 1)) 
      ($numbervars-str $I1 $N $X $VN1 $VN)))


  (= 
    (statistics $Key $Value) 
    ( (nonvar $Key) 
      ($statistics-mode $Key) 
      (set-det) 
      ($statistics $Key $Value)))
  (= 
    (statistics $Key $Value) 
    ( (findall $M 
        ($statistics-mode $M) $Domain) (illarg (domain is-symbol $Domain) (statistics $Key $Value) 1)))


  (= 
    ($statistics_mode  runtime) True)
  (= 
    ($statistics_mode  trail) True)
  (= 
    ($statistics_mode  choice) True)


  (= 
    (print-message $Type $Message) 
    ( (var $Type) 
      (set-det) 
      (illarg var 
        (print-message $Type $Message) 1)))
  (= 
    (print-message error $Message) 
    ( (set-det) ($error-message $Message)))
  (= 
    (print-message info $Message) 
    ( (set-det) 
      ($fast-write {) 
      ($builtin-message $Message) 
      ($fast-write }) 
      (nl)))
  (= 
    (print-message warning $Message) 
    ( (set-det) 
      ($fast-write '{WARNING: ') 
      ($builtin-message $Message) 
      ($fast-write }) 
      (nl)))


  (= 
    ($error-message (instantiation-error $Goal 0)) 
    ( (set-det) 
      ($fast-write user-error '{INSTANTIATION ERROR: ') 
      ($write-goal user-error $Goal) 
      ($fast-write user-error }) 
      (nl user-error) 
      (flush-output user-error)))
  (= 
    ($error-message (instantiation-error $Goal $ArgNo)) 
    ( (set-det) 
      ($fast-write user-error '{INSTANTIATION ERROR: ') 
      ($write-goal user-error $Goal) 
      ($fast-write user-error ' - arg ') 
      ($fast-write user-error $ArgNo) 
      ($fast-write user-error }) 
      (nl user-error) 
      (flush-output user-error)))
  (= 
    ($error-message (type-error $Goal $ArgNo $Type $Culprit)) 
    ( (set-det) 
      ($fast-write user-error '{TYPE ERROR: ') 
      ($write-goal user-error $Goal) 
      ($fast-write user-error ' - arg ') 
      ($fast-write user-error $ArgNo) 
      ($fast-write user-error ': expected ') 
      ($fast-write user-error $Type) 
      ($fast-write user-error ', found ') 
      (write user-error $Culprit) 
      ($fast-write user-error }) 
      (nl user-error) 
      (flush-output user-error)))
  (= 
    ($error-message (domain-error $Goal $ArgNo $Domain $Culprit)) 
    ( (set-det) 
      ($fast-write user-error '{DOMAIN ERROR: ') 
      ($write-goal user-error $Goal) 
      ($fast-write user-error ' - arg ') 
      ($fast-write user-error $ArgNo) 
      ($fast-write user-error ': expected ') 
      ($fast-write user-error $Domain) 
      ($fast-write user-error ', found ') 
      (write user-error $Culprit) 
      ($fast-write user-error }) 
      (nl user-error) 
      (flush-output user-error)))
  (= 
    ($error-message (existence-error $Goal 0 $ObjType $Culprit $Message)) 
    ( (set-det) 
      ($fast-write user-error '{EXISTENCE ERROR: ') 
      ($fast-write user-error $ObjType) 
      ($fast-write user-error ' ') 
      (write user-error $Culprit) 
      ($fast-write user-error ' does not exist') 
      ($fast-write user-error }) 
      (nl user-error) 
      (flush-output user-error)))
  (= 
    ($error-message (existence-error $Goal $ArgNo $ObjType $Culprit $Message)) 
    ( (set-det) 
      ($fast-write user-error '{EXISTENCE ERROR: ') 
      ($write-goal user-error $Goal) 
      ($fast-write user-error ' - arg ') 
      ($fast-write user-error $ArgNo) 
      ($fast-write user-error : ) 
      ($fast-write user-error $ObjType) 
      ($fast-write user-error ' ') 
      (write user-error $Culprit) 
      ($fast-write user-error ' does not exist') 
      ($fast-write user-error }) 
      (nl user-error) 
      (flush-output user-error)))
  (= 
    ($error-message (permission-error $Goal $Operation $ObjType $Culprit $Message)) 
    ( (set-det) 
      ($fast-write user-error '{PERMISSION ERROR: ') 
      ($write-goal user-error $Goal) 
      ($fast-write user-error ' - can not ') 
      ($fast-write user-error $Operation) 
      ($fast-write user-error ' ') 
      ($fast-write user-error $ObjType) 
      ($fast-write user-error ' ') 
      (write user-error $Culprit) 
      ($fast-write user-error : ) 
      ($fast-write user-error $Message) 
      ($fast-write user-error }) 
      (nl user-error) 
      (flush-output user-error)))
  (= 
    ($error-message (representation-error $Goal $ArgNo $Flag)) 
    ( (set-det) 
      ($fast-write user-error '{REPRESENTATION ERROR: ') 
      ($write-goal user-error $Goal) 
      ($fast-write user-error ' - arg ') 
      ($fast-write user-error $ArgNo) 
      ($fast-write user-error ': limit of ') 
      ($fast-write user-error $Flag) 
      ($fast-write user-error ' is breached') 
      ($fast-write user-error }) 
      (nl user-error) 
      (flush-output user-error)))
  (= 
    ($error-message (evaluation-error $Goal $ArgNo $Type)) 
    ( (set-det) 
      ($fast-write user-error '{EVALUATION ERROR: ') 
      ($write-goal user-error $Goal) 
      ($fast-write user-error ' - arg ') 
      ($fast-write user-error $ArgNo) 
      ($fast-write user-error ', found ') 
      ($fast-write user-error $Type) 
      ($fast-write user-error }) 
      (nl user-error) 
      (flush-output user-error)))
  (= 
    ($error-message (syntax-error $Goal $ArgNo $Type $Culprit $Message)) 
    ( (set-det) 
      ($fast-write user-error '{SYNTAX ERROR: ') 
      ($write-goal user-error $Goal) 
      ($fast-write user-error ' - arg ') 
      ($fast-write user-error $ArgNo) 
      ($fast-write user-error ': expected ') 
      ($fast-write user-error $Type) 
      ($fast-write user-error ', found ') 
      (write user-error $Culprit) 
      ($fast-write user-error }) 
      (nl user-error) 
      (flush-output user-error)))
  (= 
    ($error-message (system-error $Message)) 
    ( (set-det) 
      ($fast-write user-error '{SYSTEM ERROR: ') 
      (write user-error $Message) 
      ($fast-write user-error }) 
      (nl user-error) 
      (flush-output user-error)))
  (= 
    ($error-message (internal-error $Message)) 
    ( (set-det) 
      ($fast-write user-error '{INTERNAL ERROR: ') 
      (write user-error $Message) 
      ($fast-write user-error }) 
      (nl user-error) 
      (flush-output user-error)))
  (= 
    ($error-message (java-error $Goal $ArgNo $Exception)) 
    ( (set-det) 
      ($fast-write user-error '{JAVA ERROR: ') 
      ($write-goal user-error $Goal) 
      ($fast-write user-error ' - arg ') 
      ($fast-write user-error $ArgNo) 
      ($fast-write user-error ', found ') 
      ($write-goal user-error $Exception) 
      ($fast-write user-error }) 
      (nl user-error) 
      ($print-stack-trace $Exception) 
      (flush-output user-error)))
  (= 
    ($error-message $Message) 
    ( ($fast-write user-error {) 
      (write user-error $Message) 
      ($fast-write user-error }) 
      (nl user-error) 
      (flush-output user-error)))


  (= 
    ($write-goal $S $Goal) 
    ( (java $Goal) 
      (set-det) 
      ($write-toString $S $Goal)))
  (= 
    ($write-goal $S $Goal) 
    (write $S $Goal))


  (= 
    (illarg $Msg $Goal $ArgNo) 
    ( (var $Msg) 
      (set-det) 
      (illarg var $Goal $ArgNo)))
  (= 
    (illarg var $Goal $ArgNo) 
    (raise-exception (instantiation-error $Goal $ArgNo)))
  (= 
    (illarg 
      (type $Type) $Goal $ArgNo) 
    ( (arg $ArgNo $Goal $Arg) 
      (det-if-then-else 
        (nonvar $Arg) 
        (= $Error 
          (type-error $Goal $ArgNo $Type $Arg)) 
        (= $Error 
          (instantiation-error $Goal $ArgNo))) 
      (raise-exception $Error)))
  (= 
    (illarg 
      (domain $Type $ExpDomain) $Goal $ArgNo) 
    ( (arg $ArgNo $Goal $Arg) 
      (det-if-then-else 
        ($match-type $Type $Arg) 
        (= $Error 
          (domain-error $Goal $ArgNo $ExpDomain $Arg)) 
        (det-if-then-else 
          (nonvar $Arg) 
          (= $Error 
            (type-error $Goal $ArgNo $Type $Arg)) 
          (= $Error 
            (instantiation-error $Goal $ArgNo)))) 
      (raise-exception $Error)))
  (= 
    (illarg 
      (existence $ObjType $Culprit $Message) $Goal $ArgNo) 
    (raise-exception (existence-error $Goal $ArgNo $ObjType $Culprit $Message)))
  (= 
    (illarg 
      (permission $Operation $ObjType $Culprit $Message) $Goal $_) 
    (raise-exception (permission-error $Goal $Operation $ObjType $Culprit $Message)))
  (= 
    (illarg 
      (representation $Flag) $Goal $ArgNo) 
    (raise-exception (representation-error $Goal $ArgNo $Flag)))
  (= 
    (illarg 
      (evaluation $Type) $Goal $ArgNo) 
    (raise-exception (evaluation-error $Goal $ArgNo $Type)))
  (= 
    (illarg 
      (syntax $Type $Culprit $Message) $Goal $ArgNo) 
    (raise-exception (syntax-error $Goal $ArgNo $Type $Culprit $Message)))
  (= 
    (illarg 
      (system $Message) $_ $_) 
    (raise-exception (system-error $Message)))
  (= 
    (illarg 
      (internal $Message) $_ $_) 
    (raise-exception (internal-error $Message)))
  (= 
    (illarg 
      (java $Exception) $Goal $ArgNo) 
    (raise-exception (java-error $Goal $ArgNo $Exception)))
  (= 
    (illarg $Msg $_ $_) 
    (raise-exception $Msg))


  (= 
    ($match_type  term $_) True)
  (= 
    ($match-type variable $X) 
    (var $X))
  (= 
    ($match-type is-symbol $X) 
    (atom $X))
  (= 
    ($match-type symbolic $X) 
    (atomic $X))
  (= 
    ($match-type byte $X) 
    ( (integer $X) 
      (=< 0 $X) 
      (=< $X 255)))
  (= 
    ($match-type in-byte $X) 
    ( (integer $X) 
      (=< -1 $X) 
      (=< $X 255)))
  (= 
    ($match-type character $X) 
    ( (atom $X) (atom-length $X 1)))
  (= 
    ($match-type in-character $X) 
    (or 
      (== $X end-of-file) 
      ($match-type character $X)))
  (= 
    ($match-type number $X) 
    (number $X))
  (= 
    ($match-type integer $X) 
    (integer $X))
  (= 
    ($match-type long $X) 
    (long $X))
  (= 
    ($match-type float $X) 
    (float $X))
  (= 
    ($match-type callable $X) 
    (callable $X))
  (= 
    ($match-type compound $X) 
    (compound $X))
  (= 
    ($match-type list $X) 
    ( (nonvar $X) (or (= $X Nil) (= $X (Cons  $_ $_)))))
  (= 
    ($match-type java $X) 
    (java $X))
  (= 
    ($match-type stream $X) 
    (or 
      (java $X java.io.PushbackReader) 
      (java $X java.io.PrintWriter)))
  (= 
    ($match-type stream-or-alias $X) 
    (or 
      (atom $X) 
      ($match-type stream $X)))
  (= 
    ($match-type hash $X) 
    (java $X com.googlecode.prolog-cafe.lang.HashtableOfTerm))
  (= 
    ($match-type hash-or-alias $X) 
    (or 
      (atom $X) 
      ($match-type hash $X)))
  (= 
    ($match-type predicate-indicator $X) 
    ( (nonvar $X) 
      (with_self  
        (= $X $P) 
        (/ $F $A)) 
      (atom $P) 
      (atom $F) 
      (integer $A)))
;
;                                         '$match_type'(evaluable,    X).

;
;                                         '$match_type'('convertible to java',  X).


;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          ISO thread synchronization

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  !(public (/ with-mutex 2))


  (= 
    (with-mutex $M $G) 
    ( (not (atom $M)) 
      (not (java $M)) 
      (set-det) 
      (illarg 
        (type is-symbol) 
        (with-mutex $M $G) 1)))
  (= 
    (with-mutex $M $G) 
    ( (var $G) 
      (set-det) 
      (illarg var 
        (with-mutex $M $G) 2)))
  (= 
    (with-mutex $M $G) 
    ( (not (callable $G)) 
      (set-det) 
      (illarg 
        (type callable) 
        (with-mutex $M $G) 2)))
  (= 
    (with-mutex $M $G) 
    ( (mutex-lock-bt $M) 
      (call $G) 
      (set-det) 
      (mutex-unlock $M)))
;                                         ; if it fails or throws exception, mutex is unlocked automatically due to mutex_lock_bt



;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          Utilities

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (= 
    ($builtin_append  () $Zs $Zs) True)
  (= 
    ($builtin-append 
      (Cons  $X $Xs) $Ys 
      (Cons  $X $Zs)) 
    ($builtin-append $Xs $Ys $Zs))

;
;                                         '$builtin_member'(X, [X|_]).

;
;                                         '$builtin_member'(X, [_|L]) :- '$builtin_member'(X, L).



  (= 
    ($builtin-message Nil) 
    (set-det))
  (= 
    ($builtin-message (:: $M)) 
    ( (set-det) (write $M)))
  (= 
    ($builtin-message (Cons  $M $Ms)) 
    ( (write $M) 
      ($fast-write ' ') 
      ($builtin-message $Ms)))


  (= 
    ($member-in-reverse $X 
      (Cons  $_ $L)) 
    ($member-in-reverse $X $L))
  (= 
    ($member_in_reverse  $X 
      (Cons  $X $_)) True)
;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                          END



