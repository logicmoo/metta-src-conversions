;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (top) 
    (prover))
; 




  !(op 950 xfy #)
; 
	;
; 

  !(op 850 xfy &)
; 
	;
; 

  !(op 500 fx +)
; 
	;
; 

  !(op 500 fx -)
; 
	;
; 



  (= 
    (prover) 
    ( (problem $_ $P $C) 
      (implies $P $C) 
      (fail)))
; 

  (= prover True)
; 


;
; 



  (= 
    (problem  1 
      (-  a) 
      (+  a)) True)
; 


  (= 
    (problem  2 
      (+  a) 
      (&  
        (-  a) 
        (-  a))) True)
; 


;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  tests/features/convertor/sxx_machine/bench/prover.pl 33 24 500))


  (= 
    (problem  4 
      (&  
        (-  a) 
        (-  a)) 
      (-  a)) True)
; 


;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  tests/features/convertor/sxx_machine/bench/prover.pl 37 20 558))


  (= 
    (problem  6 
      (&  
        (-  a) 
        (-  b)) 
      (&  
        (-  b) 
        (-  a))) True)
; 


  (= 
    (problem  7 
      (-  a) 
      (#  
        (-  b) 
        (&  
          (+  b) 
          (-  a)))) True)
; 


;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  tests/features/convertor/sxx_machine/bench/prover.pl 43 22 653))


;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  tests/features/convertor/sxx_machine/bench/prover.pl 45 16 693))


;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  tests/features/convertor/sxx_machine/bench/prover.pl 47 18 741))


;
; 



  (= 
    (implies $Premise $Conclusion) 
    ( (opposite $Conclusion $Denial) (add-conjunction $Premise $Denial (fs Nil Nil Nil Nil))))
; 



  (= 
    (opposite 
      (& $F0 $G0) 
      (# $F1 $G1)) 
    ( (set-det) 
      (opposite $F0 $F1) 
      (opposite $G0 $G1)))
; 

  (= 
    (opposite 
      (# $F1 $G1) 
      (& $F0 $G0)) 
    ( (set-det) 
      (opposite $F1 $F0) 
      (opposite $G1 $G0)))
; 

  (= 
    (opposite 
      (+ $Atom) 
      (- $Atom)) 
    (set-det))
; 

  (= 
    (opposite  
      (-  $Atom) 
      (+  $Atom)) True)
; 



  (= 
    (add-conjunction $F $G $Set) 
    ( (expand $F $Set $Mid) 
      (expand $G $Mid $New) 
      (refute $New)))
; 



  (= 
    (expand $_ refuted refuted) 
    (set-det))
; 

  (= 
    (expand 
      (& $F $G) 
      (fs $D $_ $_ $_) refuted) 
    ( (includes $D 
        (& $F $G)) (set-det)))
; 

  (= 
    (expand 
      (& $F $G) 
      (fs $D $C $P $N) 
      (fs $D $C $P $N)) 
    ( (includes $C 
        (& $F $G)) (set-det)))
; 

  (= 
    (expand 
      (& $F $G) 
      (fs $D $C $P $N) $New) 
    ( (set-det) 
      (expand $F 
        (fs $D 
          (Cons  
            (& $F $G) $C) $P $N) $Mid) 
      (expand $G $Mid $New)))
; 

  (= 
    (expand 
      (# $F $G) 
      (fs $D $C $P $N) $Set) 
    ( (set-det) 
      (opposite 
        (# $F $G) $Conj) 
      (extend $Conj $D $C $D1 
        (fs $D1 $C $P $N) $Set)))
; 

  (= 
    (expand 
      (+ $Atom) 
      (fs $D $C $P $N) $Set) 
    ( (set-det) (extend $Atom $P $N $P1 (fs $D $C $P1 $N) $Set)))
; 

  (= 
    (expand 
      (- $Atom) 
      (fs $D $C $P $N) $Set) 
    (extend $Atom $N $P $N1 
      (fs $D $C $P $N1) $Set))
; 



  (= 
    (includes 
      (Cons  $Head $_) $Head) 
    (set-det))
; 

  (= 
    (includes 
      (Cons  $_ $Tail) $This) 
    (includes $Tail $This))
; 



  (= 
    (extend $Exp $_ $Neg $_ $_ refuted) 
    ( (includes $Neg $Exp) (set-det)))
; 

  (= 
    (extend $Exp $Pos $_ $Pos $Set $Set) 
    ( (includes $Pos $Exp) (set-det)))
; 

  (= 
    (extend  $Exp $Pos $_ 
      (Cons  $Exp $Pos) $Set $Set) True)
; 



  (= 
    (refute refuted) 
    (set-det))
; 

  (= 
    (refute (fs (Cons  (& $F1 $G1) $D) $C $P $N)) 
    ( (opposite $F1 $F0) 
      (opposite $G1 $G0) 
      (= $Set 
        (fs $D $C $P $N)) 
      (add-conjunction $F0 $G1 $Set) 
      (add-conjunction $F0 $G0 $Set) 
      (add-conjunction $F1 $G0 $Set)))
; 

