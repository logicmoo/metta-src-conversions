;
; 

;
; 

;
; 

;
; 


;
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
    (same-functor $T1 $T2) 
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
          (format "Error in same_functor: Both terms variables!~n" Nil) 
          (fail)))))
; 


;
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
    (same-functor $T1 $T2 $A) 
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
          (format "Error in same_functor: Both terms variables!~n" Nil) 
          (fail)))))
; 



;
; 

;
; 


  (= 
    (same-arg $N $T1 $T2) 
    ( (arg $N $T1 $Item) (arg $N $T2 $Item)))
; 


;
; 

;
; 

  (= 
    (same-arg $N $T1 $T2 $Item) 
    ( (arg $N $T1 $Item) (arg $N $T2 $Item)))
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
    (corresponding-arg $N $T1 $Item1 $T2 $Item2) 
    ( (arg $N $T1 $Item1) (arg $N $T2 $Item2)))
; 



;
; 


;
; 

;
; 



  (= 
    (genarg $N $Term $Arg) 
    ( (integer $N) 
      (nonvar $Term) 
      (set-det) 
      (arg $N $Term $Arg)))
; 

  (= 
    (genarg $N $Term $Arg) 
    ( (var $N) 
      (nonvar $Term) 
      (set-det) 
      (functor $Term $_ $Arity) 
      (genarg $Arity $Term $Arg $N)))
; 


  (= 
    (genarg 1 $Term $Arg 1) 
    ( (set-det) (arg 1 $Term $Arg)))
; 

  (= 
    (genarg $N $Term $Arg $N) 
    (arg $N $Term $Arg))
; 

  (= 
    (genarg $K $Term $Arg $N) 
    ( (> $K 1) 
      (is $J 
        (- $K 1)) 
      (genarg $J $Term $Arg $N)))
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
    (path-arg $Path $Term $SubTerm) 
    ( (var $Term) 
      (set-det) 
      (det-if-then-else 
        (== $Path Nil) 
        (= $SubTerm $Term) 
        (det-if-then-else 
          (not (= $Path (Cons  $_ $_))) fail 
          (, 
            (format user-error '~N! Instantiation fault in argument ~w of ~q/~w~n! Goal: ~p~n' 
              (:: 2 path-arg 3 
                (path-arg $Path $Term $SubTerm))) 
            (fail))))))
; 

  (= 
    (path_arg  () $Term $Term) True)
; 

  (= 
    (path-arg 
      (Cons  $Head $Tail) $Term $SubTerm) 
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
                (Cons  $Head $Tail) $Term $SubTerm))) 
          (fail)))))
; 



