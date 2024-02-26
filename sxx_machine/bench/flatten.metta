;
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
    ( (eliminate-disjunctions 
        (:: (= (a $A $B $C) (or (b $A) (c $C)))) $X $Y Nil) (inst-vars (, $X $Y))))
; 

;
; 

	;
; 

;
; 

  (= top True)
; 

;
; 



  (= 
    (eliminate-disjunctions $OneProc $NewProc $NewClauses $Link) 
    ( (gather-disj $OneProc $NewProc $Disj Nil) (treat-disj $Disj $NewClauses $Link)))
; 



  (= 
    (gather_disj  () () $Link $Link) True)
; 

  (= 
    (gather-disj 
      (Cons  $C $Cs) $NewProc $Disj $Link) 
    ( (extract-disj $C $NewC $Disj $Rest) 
      (= $NewProc 
        (Cons  $NewC $NewCs)) 
      (gather-disj $Cs $NewCs $Rest $Link)))
; 


;
; 

;
; 

;
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
    (extract-disj $C 
      (= $Head $NewBody) $Disj $Link) 
    ( (= $C 
        (= $Head $Body)) 
      (set-det) 
      (= $CtrIn 0) 
      (extract-disj $Body $NewBody $Disj $Link $C $CtrIn $CtrOut)))
; 

  (= 
    (extract_disj  $Head $Head $Link $Link) True)
; 


  (= 
    (extract-disj 
      (, $C1 $C2) 
      (, $NewC1 $NewC2) $Disj $Link $C $CtrIn $CtrOut) 
    ( (extract-disj $C1 $NewC1 $Disj $Link1 $C $CtrIn $Ctr) (extract-disj $C2 $NewC2 $Link1 $Link $C $Ctr $CtrOut)))
; 


  (= 
    (extract-disj $Goal $X $Disj $Link $C $CtrIn $CtrOut) 
    ( (is-disj $Goal $NewGoal) 
      (set-det) 
      (= $Disj 
        (Cons  
          (disj $NewGoal $CtrIn $X $C) $Link)) 
      (is $CtrOut 
        (+ $CtrIn 1))))
; 

  (= 
    (extract_disj  $Goal $Goal $Link $Link $_ $CtrIn $CtrIn) True)
; 



  (= 
    (is-disj 
      (det-if-then-else $C1 $C2 $C3) 
      (or 
        (, $C1 
          (set-det) $C2) $C3)) 
    (set-det))
; 

  (= 
    (is_disj  
      (;  $C1 $C2) 
      (;  $C1 $C2)) True)
; 

  (= 
    (is_disj  
      (not  $C) 
      (;  
        (,  $C 
          (,  ! fail)) true)) True)
; 

  (= 
    (is_disj  
      (\+  $C) 
      (;  
        (,  $C 
          (,  ! fail)) true)) True)
; 

  (= 
    (is_disj  
      (\=  $C1 $C2) 
      (;  
        (,  
          (= $C1 $C2) 
          (,  ! fail)) true)) True)
; 


;
; 

;
; 

;
; 

;
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
    (treat_disj  () $Link $Link) True)
; 

  (= 
    (treat-disj 
      (Cons  
        (disj 
          (or $A $B) $N $X $C) $Disjs) $DummyClauses $Link) 
    ( (find-vars 
        (or $A $B) $Vars) 
      (find-vars $C $CVars) 
      (intersect-vars $Vars $CVars $Args) 
      (make-dummy-name $N $Name) 
      (=.. $X 
        (Cons  $Name $Args)) 
      (make-dummy-clauses 
        (or $A $B) $X $DummyClauses $Rest) 
      (treat-disj $Disjs $Rest $Link)))
; 



  (= 
    (make-dummy-clauses 
      (or $A $B) $X 
      (Cons  $NewC $Cs) $Link) 
    ( (set-det) 
      (copy 
        (= $X $A) $NewC) 
      (make-dummy-clauses $B $X $Cs $Link)))
; 

  (= 
    (make-dummy-clauses $A $X 
      (Cons  $NewC $Link) $Link) 
    (copy 
      (= $X $A) $NewC))
; 



  (= 
    (find-vars $X $Y) 
    ( (find-vars $X $Y $Link) (= $Link Nil)))
; 


  (= 
    (find-vars $Var 
      (Cons  $Var $Link) $Link) 
    ( (var $Var) (set-det)))
; 

  (= 
    (find-vars $Cst $Link $Link) 
    ( (atomic $Cst) (set-det)))
; 

  (= 
    (find-vars 
      (Cons  $T $Ts) $Vars $NewLink) 
    ( (set-det) 
      (find-vars $T $Vars $Link) 
      (find-vars $Ts $Link $NewLink)))
; 

  (= 
    (find-vars $Term $Vars $Link) 
    ( (=.. $Term 
        (Cons  $_ $Args)) (find-vars $Args $Vars $Link)))
; 



  (= 
    (intersect-vars $V1 $V2 $Out) 
    ( (sort-vars $V1 $Sorted1) 
      (sort-vars $V2 $Sorted2) 
      (intersect-sorted-vars $Sorted1 $Sorted2 $Out)))
; 



  (= 
    (make-dummy-name $N $Name) 
    ( (name -dummy- $L1) 
      (name $N $L2) 
      (append $L1 $L2 $L) 
      (name $Name $L)))
; 



  (= 
    (append  () $L $L) True)
; 

  (= 
    (append 
      (Cons  $H $L1) $L2 
      (Cons  $H $Res)) 
    (append $L1 $L2 $Res))
; 


;
; 


  (= 
    (copy $Term1 $Term2) 
    ( (varset $Term1 $Set) 
      (make-sym $Set $Sym) 
      (copy2 $Term1 $Term2 $Sym) 
      (set-det)))
; 



  (= 
    (copy2 $V1 $V2 $Sym) 
    ( (var $V1) 
      (set-det) 
      (retrieve-sym $V1 $Sym $V2)))
; 

  (= 
    (copy2 $X1 $X2 $Sym) 
    ( (nonvar $X1) 
      (set-det) 
      (functor $X1 $Name $Arity) 
      (functor $X2 $Name $Arity) 
      (copy2 $X1 $X2 $Sym 1 $Arity)))
; 


  (= 
    (copy2 $X1 $X2 $Sym $N $Arity) 
    ( (> $N $Arity) (set-det)))
; 

  (= 
    (copy2 $X1 $X2 $Sym $N $Arity) 
    ( (=< $N $Arity) 
      (set-det) 
      (arg $N $X1 $Arg1) 
      (arg $N $X2 $Arg2) 
      (copy2 $Arg1 $Arg2 $Sym) 
      (is $N1 
        (+ $N 1)) 
      (copy2 $X1 $X2 $Sym $N1 $Arity)))
; 



  (= 
    (retrieve-sym $V 
      (Cons  
        (p $W $X) $Sym) $X) 
    ( (== $V $W) (set-det)))
; 

  (= 
    (retrieve-sym $V 
      (Cons  $_ $Sym) $X) 
    (retrieve-sym $V $Sym $X))
; 



  (= 
    (make_sym  () ()) True)
; 

  (= 
    (make-sym 
      (Cons  $V $L) 
      (Cons  
        (p $V $_) $S)) 
    (make-sym $L $S))
; 


;
; 


  (= 
    (varset $Term $VarSet) 
    ( (varbag $Term $VB) (sort $VB $VarSet)))
; 


  (= 
    (varbag $Term $VarBag) 
    (varbag $Term $VarBag Nil))
; 



  (= 
    (-->  
      (varbag  $Var) 
      (,  
        { (var  $Var) } 
        (,  ! 
          ($Var)))) True)
; 

  (= 
    (-->  
      (varbag  $Str) 
      (,  
        { (,  
            (nonvar  $Str) 
            (,  ! 
              (functor  $Str $_ $Arity))) } 
        (varbag  $Str 1 $Arity))) True)
; 


  (= 
    (-->  
      (varbag  $Str $N $Arity) 
      (,  
        { (>  $N $Arity) } !)) True)
; 

  (= 
    (-->  
      (varbag  $Str $N $Arity) 
      (,  
        { (=<  $N $Arity) } 
        (,  ! 
          (,  
            { (arg  $N $Str $Arg) } 
            (,  
              (varbag  $Arg) 
              (,  
                { (is  $N1 
                    (+  $N 1)) } 
                (varbag  $Str $N1 $Arity))))))) True)
; 



  (= 
    (inst-vars $Term) 
    ( (varset $Term $Vars) 
      (= 
        (:: $A) 
        (:: 65)) 
      (inst-vars-list $Vars $A)))
; 



  (= 
    (inst_vars_list  () $_) True)
; 

  (= 
    (inst-vars-list 
      (Cons  $T $L) $N) 
    ( (name $T 
        (:: $N)) 
      (is $N1 
        (+ $N 1)) 
      (inst-vars-list $L $N1)))
; 



  (= 
    (sort-vars $V $Out) 
    (sort-vars $V $Out Nil))
; 

  (= 
    (sort_vars  () $Link $Link) True)
; 

  (= 
    (sort-vars 
      (Cons  $V $Vs) $Result $Link) 
    ( (split-vars $Vs $V $Smaller $Bigger) 
      (sort-vars $Smaller $Result 
        (Cons  $V $SLink)) 
      (sort-vars $Bigger $SLink $Link)))
; 



  (= 
    (intersect-sorted-vars Nil $_ Nil) 
    (set-det))
; 

  (= 
    (intersect_sorted_vars  $_ () ()) True)
; 

  (= 
    (intersect-sorted-vars 
      (Cons  $X $Xs) 
      (Cons  $Y $Ys) 
      (Cons  $X $Rs)) 
    ( (== $X $Y) 
      (set-det) 
      (intersect-sorted-vars $Xs $Ys $Rs)))
; 

  (= 
    (intersect-sorted-vars 
      (Cons  $X $Xs) 
      (Cons  $Y $Ys) $Rs) 
    ( (@< $X $Y) 
      (set-det) 
      (intersect-sorted-vars $Xs 
        (Cons  $Y $Ys) $Rs)))
; 

  (= 
    (intersect-sorted-vars 
      (Cons  $X $Xs) 
      (Cons  $Y $Ys) $Rs) 
    ( (@> $X $Y) 
      (set-det) 
      (intersect-sorted-vars 
        (Cons  $X $Xs) $Ys $Rs)))
; 




  (= 
    (split_vars  () $_ () ()) True)
; 

  (= 
    (split-vars 
      (Cons  $V $Vs) $A 
      (Cons  $V $Ss) $Bs) 
    ( (@< $V $A) 
      (set-det) 
      (split-vars $Vs $A $Ss $Bs)))
; 

  (= 
    (split-vars 
      (Cons  $V $Vs) $A $Ss $Bs) 
    ( (== $V $A) 
      (set-det) 
      (split-vars $Vs $A $Ss $Bs)))
; 

  (= 
    (split-vars 
      (Cons  $V $Vs) $A $Ss 
      (Cons  $V $Bs)) 
    ( (@> $V $A) 
      (set-det) 
      (split-vars $Vs $A $Ss $Bs)))
; 


