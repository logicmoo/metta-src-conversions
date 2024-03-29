;                                           (convert-to-metta-file  unify $_284082 sxx_machine/bench/unify.pl sxx_machine/bench/unify.metta)
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (C) 1990 Regents of the University of California.
;
; All rights reserved. This program may be freely used and modified for
;
; non-commercial purposes provided this copyright notice is kept unchanged.
;
; Written by Peter Van Roy as a part of the Aquarius project.
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; Benchmark based on part of Aquarius MeTTa compiler
;
; Compiling unification into abstract machine code.


  (= top  
    (main $X))
;
; , write(X), nl.


  (= (main $Size)  
    (u $X 
      (:: 1 $Y) 
      (:: $X) $Code)
    (size $Code 0 $Size))

;
; Unify variable X with term T and write the result:

  (= (u $X $T $In $Code)  
    (unify $X $T $In $_ $Code Nil))
 
;
; Unify the variable X with the term T, given that
;
; In = set of variables initialized before the unification.
;
; Returns the intermediate code for the unification and
;
; Out = set of variables initialized after the unification.

  (= (-->  (unify  $X $T $In $Out) (,  {(\+  (myin  $X $In)) } (,  ! (uninit  $X $T $In $Out))))  True)
  (= (-->  (unify  $X $T $In $Out) (,  {(myin  $X $In) } (,  ! (init  $X $T $In $Out nonlast $_))))  True)

;
; **** Uninit assumes X has not yet been initialized:
  (= (-->  (uninit  $X $T $In $Out) (,  {(my-compound  $T) } (,  ! (,  ((move  (^  $Tag h) $X)) (,  {(termtag  $T $Tag) } (,  (unify-block  nonlast $T $_ $In $Mid $_) {(incl  $X $Mid $Out) }))))))  True)
  (= (-->  (uninit  $X $T $In $Out) (,  {(atomic  $T) } (,  ! (,  ((move  (^  tatm $T) $X)) {(incl  $X $In $Out) }))))  True)
  (= (-->  (uninit  $X $T $In $Out) (,  {(var  $T) } (,  ! (unify-var  $X $T $In $Out))))  True)

;
; **** Init assumes X has already been initialized:
  (= (-->  (init  $X $T $In $Out $Last $LLbls) (,  {(nonvar  $T) } (,  ! (,  {(termtag  $T $Tag) } (,  ((deref  $X) (switch  $Tag $X ([|]  (trail  $X) $Write) $Read fail)) (,  {(unify-writemode  $X $T $In $Last $LLbls $Write ()) } {(unify-readmode  $X $T $In $Out $LLbls $Read ()) }))))))  True)
  (= (-->  (init  $X $T $In $Out $_ $_) (,  {(var  $T) } (,  ! (unify-var  $X $T $In $Out))))  True)

;
; **** Unifying two variables together:
  (= (-->  (unify-var  $X $Y $In $In) (,  {(,  (myin  $X $In) (myin  $Y $In)) } (,  ! ((unify  $X $Y fail)))))  True)
  (= (-->  (unify-var  $X $Y $In $Out) (,  {(,  (myin  $X $In) (\+  (myin  $Y $In))) } (,  ! (,  ((move  $X $Y)) {(incl  $Y $In $Out) }))))  True)
  (= (-->  (unify-var  $X $Y $In $Out) (,  {(,  (\+  (myin  $X $In)) (myin  $Y $In)) } (,  ! (,  ((move  $Y $X)) {(incl  $X $In $Out) }))))  True)
  (= (-->  (unify-var  $X $Y $In $Out) (,  {(,  (\+  (myin  $X $In)) (\+  (myin  $Y $In))) } (,  ! (,  ((move  (^  tvar h) $X) (move  (^  tvar h) $Y) (add  1 h) (move  $Y ((-  h 1)))) {(,  (incl  $X $In $Mid) (incl  $Y $Mid $Out)) }))))  True)

;
; **** Unify_readmode assumes X is a dereferenced nonvariable
;
; at run-time and T is a nonvariable at compile-time.
  (= (-->  (unify-readmode  $X $T $In $Out $LLbls) (,  {(structure  $T) } (,  ! (,  ((equal  ($X) (^  tatm (/  $F $N)) fail)) (,  {(functor  $T $F $N) } (unify-args  1 $N $T $In $Out 0 $X $LLbls))))))  True)
  (= (-->  (unify-readmode  $X $T $In $Out $LLbls) (,  {(cons  $T) } (,  ! (unify-args  1 2 $T $In $Out -1 $X $LLbls))))  True)
  (= (-->  (unify-readmode  $X $T $In $In $_) (,  {(atomic  $T) } (,  ! ((equal  $X (^  tatm $T) fail)))))  True)

  (= (-->  (unify-args  $I $N $_ $In $In $_ $_ $_) (,  {(>  $I $N) } !))  True)
  (= (-->  (unify-args  $I $N $T $In $Out $D $X ([|]  $_ $LLbls)) (,  {(= $I  $N) } (,  ! (unify-arg  $I $T $In $Out $D $X last $LLbls))))  True) 
  (= (-->  (unify-args  $I $N $T $In $Out $D $X $LLbls) (,  {(<  $I $N) } (,  ! (,  (unify-arg  $I $T $In $Mid $D $X nonlast $_) (,  {(is  $I1 (+  $I 1)) } (unify-args  $I1 $N $T $Mid $Out $D $X $LLbls))))))  True)

  (= (-->  (unify-arg  $I $T $In $Out $D $X $Last $LLbls) (,  ((move  ((+  $X $ID)) $Y)) (,  {(,  (is  $ID (+  $I $D)) (,  (incl  $Y $In $Mid) (arg  $I $T $A))) } (init  $Y $A $Mid $Out $Last $LLbls))))  True)



;
; **** Unify_writemode assumes X is a dereferenced unbound
;
; variable at run-time and T is a nonvariable at compile-time.
  (= (-->  (unify-writemode  $X $T $In $Last $LLbls) (,  {(my-compound  $T) } (,  ! (,  ((move  (^  $Tag h) ($X))) (,  {(termtag  $T $Tag) } (unify-block  $Last $T $_ $In $_ $LLbls))))))  True)
  (= (-->  (unify-writemode  $X $T $_ $_ $_) (,  {(atomic  $T) } (,  ! ((move  (^  tatm $T) ($X))))))  True)


;
; **** Generate a minimal sequence of moves to create T on the heap:
  (= (-->  (unify-block  last $T $Size $In $In ([|]  $Lbl $_)) (,  ! (,  ((add  $Size h) (jump  $Lbl)) {(size  $T 0 $Size) })))  True)
  (= (-->  (unify-block  nonlast $T $Size $In $Out ([|]  $_ $LLbls)) (,  ! (,  ((add  $Size h)) (,  {(,  (size  $T 0 $Size) (is  $Offset (-  $Size))) } (block  $T $Offset 0 $In $Out $LLbls)))))  True)

  (= (-->  (block  $T $Inf $Outf $In $Out $LLbls) (,  {(structure  $T) } (,  ! (,  ((move  (^  tatm (/  $F $N)) ((+  h $Inf)))) (,  {(,  (functor  $T $F $N) (,  (is  $Midf (+  (+  $Inf $N) 1)) (is  $S (+  $Inf 1)))) } (,  (make-slots  1 $N $T $S $Offsets $In $Mid) (block-args  1 $N $T $Midf $Outf $Offsets $Mid $Out $LLbls)))))))  True)
  (= (-->  (block  $T $Inf $Outf $In $Out $LLbls) (,  {(cons  $T) } (,  ! (,  {(is  $Midf (+  $Inf 2)) } (,  (make-slots  1 2 $T $Inf $Offsets $In $Mid) (block-args  1 2 $T $Midf $Outf $Offsets $Mid $Out $LLbls))))))  True)
  (= (-->  (block  $T $Inf $Inf $In $In ()) (,  {(atomic  $T) } !))  True)
  (= (-->  (block  $T $Inf $Inf $In $In ()) (,  {(var  $T) } !))  True)

  (= (-->  (block-args  $I $N $_ $Inf $Inf () $In $In ()) (,  {(>  $I $N) } !))  True)
  (= (-->  (block-args  $I $N $T $Inf $Outf ($Inf) $In $Out ([|]  $Lbl $LLbls)) (,  {(= $I  $N) } (,  ! (,  ((label  $Lbl)) (,  {(arg  $I $T $A) } (block  $A $Inf $Outf $In $Out $LLbls))))))  True)
  (= (-->  (block-args  $I $N $T $Inf $Outf ([|]  $Inf $Offsets) $In $Out $LLbls) (,  {(<  $I $N) } (,  ! (,  {(arg  $I $T $A) } (,  (block  $A $Inf $Midf $In $Mid $_) (,  {(is  $I1 (+  $I 1)) } (block-args  $I1 $N $T $Midf $Outf $Offsets $Mid $Out $LLbls)))))))  True)

  (= (-->  (make-slots  $I $N $_ $_ () $In $In) (,  {(>  $I $N) } !))  True)
  (= (-->  (make-slots  $I $N $T $S ([|]  $Off $Offsets) $In $Out) (,  {(=<  $I $N) } (,  ! (,  {(arg  $I $T $A) } (,  (init-var  $A $S $In) (,  {(,  (incl  $A $In $Mid) (make-word  $A $Off $Word)) } (,  ((move  $Word ((+  h $S)))) (,  {(,  (is  $S1 (+  $S 1)) (is  $I1 (+  $I 1))) } (make-slots  $I1 $N $T $S1 $Offsets $Mid $Out)))))))))  True)


;
; Initialize first-time variables in write mode:
  (= (-->  (init-var  $V $I $In) (,  {(,  (var  $V) (\+  (myin  $V $In))) } (,  ! ((move  (^  tvar (+  h $I)) $V)))))  True)
  (= (-->  (init-var  $V $_ $In) (,  {(,  (var  $V) (myin  $V $In)) } !))  True)
  (= (-->  (init-var  $V $_ $_) (,  {(nonvar  $V) } !))  True)


  (= (make-word $C $Off (^ $Tag (+ h $Off)))  
    (my-compound $C)
    (set-det)
    (termtag $C $Tag))
  (= (make-word $V $_ $V)  
    (var $V)
    (set-det))
  (= (make-word $A $_ (^ tatm $A))  
    (atomic $A)
    (set-det))

;
; Calculate the size of T on the heap:

  (= (-->  (size  $T) (,  {(structure  $T) } (,  ! (,  {(functor  $T $_ $N) } (,  (add  1) (,  (add  $N) (size-args  1 $N $T)))))))  True)
  (= (-->  (size  $T) (,  {(cons  $T) } (,  ! (,  (add  2) (size-args  1 2 $T)))))  True)
  (= (-->  (size  $T) (,  {(atomic  $T) } !))  True)
  (= (-->  (size  $T) (,  {(var  $T) } !))  True)

  (= (-->  (size-args  $I $N $_) (,  {(>  $I $N) } !))  True)
  (= (-->  (size-args  $I $N $T) (,  {(=<  $I $N) } (,  ! (,  {(arg  $I $T $A) } (,  (size  $A) (,  {(is  $I1 (+  $I 1)) } (size-args  $I1 $N $T)))))))  True)

;
; **** Utility routines:


  (= (add $I $X $Y)  
    (is $Y 
      (+ $X $I)))


  (= (myin $A (Cons  $B $S))  
    (compare $Order $A $B)
    (in-2 $Order $A $S))


  (= (in-2  = $_ $_)  True)
  (= (in-2 > $A $S)  
    (myin $A $S))


  (= (incl $A $S1 $S)  
    (incl-2 $S1 $A $S))


  (= (incl-2  () $A ($A))  True)
  (= (incl-2 (Cons  $B $S1) $A $S)  
    (compare $Order $A $B)
    (incl-3 $Order $A $B $S1 $S))


  (= (incl-3  < $A $B $S1 ([|]  $A ([|]  $B $S1)))  True)
  (= (incl-3  = $_ $B $S1 ([|]  $B $S1))  True)
  (= (incl-3 > $A $B $S1 (Cons  $B $S))  
    (incl-2 $S1 $A $S))


  (= (my-compound $X)  
    (nonvar $X)
    (not (atomic $X)))

  (= (cons $X)  
    (nonvar $X)
    (= $X 
      (Cons  $_ $_)))

  (= (structure $X)  
    (my-compound $X)
    (not (= $X (Cons  $_ $_))))


  (= (termtag $T tstr)  
    (structure $T))
  (= (termtag $T tlst)  
    (cons $T))
  (= (termtag $T tatm)  
    (atomic $T))
  (= (termtag $T tvar)  
    (var $T))





