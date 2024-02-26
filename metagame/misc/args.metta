;                                           (convert-to-metta-file  args $_74358 metagame/misc/args.pl metagame/misc/args.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;; args.pl
;
; ;; Some more general routines for term-manipulation

;
; SAME_FUNCTOR(?T1,?T2)
;
; Succeeds when T1 and T2 share the same functor.
;
; Fails unless one of T1,T2 is non-var.

  (= (same-functor $T1 $T2)  
    (det-if-then-else 
      (nonvar $T1) 
      (, 
        (functor $T1 $F $A) 
        (functor $T2 $F $A)) 
      (det-if-then-else 
        (nonvar $T2) 
        (, 
          (functor $T2 $F $A) 
          (functor $T1 $F $A)) 
        (, 
          (format "Error in same_functor: Both terms variables!~n" Nil) fail))))

;
; SAME_FUNCTOR(?T1,?T2,?A)
;
; Succeeds when T1 and T2 share the same functor,
;
; and both have arity A.
;
; Fails unless one of T1,T2 is non-var.
;
; (So not as general as Quintus's library pred.
  (= (same-functor $T1 $T2 $A)  
    (det-if-then-else 
      (nonvar $T1) 
      (, 
        (functor $T1 $F $A) 
        (functor $T2 $F $A)) 
      (det-if-then-else 
        (nonvar $T2) 
        (, 
          (functor $T2 $F $A) 
          (functor $T1 $F $A)) 
        (, 
          (format "Error in same_functor: Both terms variables!~n" Nil) fail))))


;
; SAME_ARG(+N,?T1,?T2)
;
; Succeeds when T1 and T2, both non-var, have the same arg N.

  (= (same-arg $N $T1 $T2)  
    (arg $N $T1 $Item)
    (arg $N $T2 $Item))

;
; SAME_ARG(+N,?T1,?T2,+Item)
;
; Succeeds when T1 and T2, both non-var, have the same Item as arg N.
  (= (same-arg $N $T1 $T2 $Item)  
    (arg $N $T1 $Item)
    (arg $N $T2 $Item))


;
; CORRESPONDING_ARG(N,T1,Item1,T2,Item2)
;
; Item1 and Item2 are the Nth args in T1 and T2,
;
; respectively.
;
; Not as general as in Quintus.

  (= (corresponding-arg $N $T1 $Item1 $T2 $Item2)  
    (arg $N $T1 $Item1)
    (arg $N $T2 $Item2))


;
; (The following routines were borroowed from Quintus)

;
; genarg(?N, +Term, ?Item)
;
; like arg(N,Term,Item), but will generate N if necessary.


  (= (genarg $N $Term $Arg)  
    (integer $N)
    (nonvar $Term)
    (set-det)
    (arg $N $Term $Arg))
  (= (genarg $N $Term $Arg)  
    (var $N)
    (nonvar $Term)
    (set-det)
    (functor $Term $_ $Arity)
    (genarg $Arity $Term $Arg $N))

  (= (genarg 1 $Term $Arg 1)  
    (set-det)
    (arg 1 $Term $Arg))
  (= (genarg $N $Term $Arg $N)  
    (arg $N $Term $Arg))
  (= (genarg $K $Term $Arg $N)  
    (> $K 1)
    (is $J 
      (- $K 1))
    (genarg $J $Term $Arg $N))


;
; path_arg(Path, Term, SubTerm)
;
; This routine replaces two predicates in the old Dec-10 MeTTa
;
; library: patharg/3 and position/3. It does everything they did,
;
; and reports errors as well.


  (= (path-arg $Path $Term $SubTerm)  
    (var $Term)
    (set-det)
    (det-if-then-else 
      (== $Path Nil) 
      (= $SubTerm $Term) 
      (det-if-then-else 
        (not (= $Path (Cons  $_ $_))) fail 
        (, 
          (format user-error '~N! Instantiation fault in argument ~w of ~q/~w~n! Goal: ~p~n' 
            (:: 2 path-arg 3 
              (path-arg $Path $Term $SubTerm))) fail))))
  (= (path-arg  () $Term $Term)  True)
  (= (path-arg (Cons  $Head $Tail) $Term $SubTerm)  
    (det-if-then-else 
      (integer $Head) 
      (, 
        (arg $Head $Term $Arg) 
        (path-arg $Tail $Arg $SubTerm)) 
      (det-if-then-else 
        (var $Head) 
        (, 
          (functor $Term $_ $Arity) 
          (genarg $Arity $Term $Arg $Head) 
          (path-arg $Tail $Arg $SubTerm)) 
        (, 
          (format user-error '~N! Type failure in argument ~w of ~q/~w~n! Goal: ~p~n' 
            (:: 1 path-arg 3 
              (path-arg 
                (Cons  $Head $Tail) $Term $SubTerm))) fail))))
; /* otherwise */


