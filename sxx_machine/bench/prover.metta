;                                           (convert_to_metta_file  prover $_504364 sxx_machine/bench/prover.pl sxx_machine/bench/prover.metta)
;
; generated: 30 October 1989
;
; option(s):
;
; 
;
; prover
;
; 
;
; Richard A. O'Keefe
;
; 
;
; MeTTa theorem prover
;
; 
;
; from "MeTTa Compared with Lisp?," SIGPLAN Notices, v. 18 #5, May 1983

;
; op/3 directives


  (= (top)  
    (prover))



  !(op 950 xfy #)	;
; disjunction
  !(op 850 xfy &)	;
; conjunction
  !(op 500 fx +)	;
; assertion
  !(op 500 fx -)	;
; denial


  (= (prover)  
    (problem $_ $P $C)
    (implies $P $C)
    (fail))
  (= prover  True)

;
; problem set


  (= (problem  1 (-  a) (+  a))  True)

  (= (problem  2 (+  a) (&  (-  a) (-  a)))  True)

  (= (problem  3 (-  a) (#  (+  to_be) (-  to_be)))  True)

  (= (problem  4 (&  (-  a) (-  a)) (-  a))  True)

  (= (problem  5 (-  a) (#  (+  b) (-  a)))  True)

  (= (problem  6 (&  (-  a) (-  b)) (&  (-  b) (-  a)))  True)

  (= (problem  7 (-  a) (#  (-  b) (&  (+  b) (-  a))))  True)

  (= (problem  8 (#  (-  a) (#  (-  b) (+  c))) (#  (-  b) (#  (-  a) (+  c))))  True)

  (= (problem  9 (#  (-  a) (+  b)) (#  (&  (+  b) (-  c)) (#  (-  a) (+  c))))  True)

  (= (problem  10 (&  (#  (-  a) (+  c)) (#  (-  b) (+  c))) (#  (&  (-  a) (-  b)) (+  c)))  True)

;
; MeTTa theorem prover


  (= (implies $Premise $Conclusion)  
    (opposite $Conclusion $Denial)
    (add-conjunction $Premise $Denial 
      (fs Nil Nil Nil Nil)))


  (= (opposite (& $F0 $G0) (# $F1 $G1))  
    (set-det)
    (opposite $F0 $F1)
    (opposite $G0 $G1))
  (= (opposite (# $F1 $G1) (& $F0 $G0))  
    (set-det)
    (opposite $F1 $F0)
    (opposite $G1 $G0))
  (= (opposite (+ $Atom) (- $Atom))  
    (set-det))
  (= (opposite  (-  $Atom) (+  $Atom))  True)


  (= (add-conjunction $F $G $Set)  
    (expand $F $Set $Mid)
    (expand $G $Mid $New)
    (refute $New))


  (= (expand $_ refuted refuted)  
    (set-det))
  (= (expand (& $F $G) (fs $D $_ $_ $_) refuted)  
    (includes $D 
      (& $F $G))
    (set-det))
  (= (expand (& $F $G) (fs $D $C $P $N) (fs $D $C $P $N))  
    (includes $C 
      (& $F $G))
    (set-det))
  (= (expand (& $F $G) (fs $D $C $P $N) $New)  
    (set-det)
    (expand $F 
      (fs $D 
        (Cons  
          (& $F $G) $C) $P $N) $Mid)
    (expand $G $Mid $New))
  (= (expand (# $F $G) (fs $D $C $P $N) $Set)  
    (set-det)
    (opposite 
      (# $F $G) $Conj)
    (extend $Conj $D $C $D1 
      (fs $D1 $C $P $N) $Set))
  (= (expand (+ $Atom) (fs $D $C $P $N) $Set)  
    (set-det)
    (extend $Atom $P $N $P1 
      (fs $D $C $P1 $N) $Set))
  (= (expand (- $Atom) (fs $D $C $P $N) $Set)  
    (extend $Atom $N $P $N1 
      (fs $D $C $P $N1) $Set))


  (= (includes (Cons  $Head $_) $Head)  
    (set-det))
  (= (includes (Cons  $_ $Tail) $This)  
    (includes $Tail $This))


  (= (extend $Exp $_ $Neg $_ $_ refuted)  
    (includes $Neg $Exp)
    (set-det))
  (= (extend $Exp $Pos $_ $Pos $Set $Set)  
    (includes $Pos $Exp)
    (set-det))
  (= (extend  $Exp $Pos $_ (Cons  $Exp $Pos) $Set $Set)  True)


  (= (refute refuted)  
    (set-det))
  (= (refute (fs (Cons  (& $F1 $G1) $D) $C $P $N))  
    (opposite $F1 $F0)
    (opposite $G1 $G0)
    (= $Set 
      (fs $D $C $P $N))
    (add-conjunction $F0 $G1 $Set)
    (add-conjunction $F0 $G0 $Set)
    (add-conjunction $F1 $G0 $Set))

