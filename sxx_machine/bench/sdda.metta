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


  (= 
    (top) 
    (do-sdda test $A $B $C))
; 


;
; 

;
; 

;
; 


  (= 
    (do-sdda $FileName $ExitModes $BackList $PredList) 
    ( (read-procedures $Procs $ExitModes $Entries) (entry-exit-modes-list $Procs $ExitModes $Entries)))
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
    (read-procedures 
      (Cons  
        (Cons  
          (/ a 2) 
          (Cons  
            (a $109 $110) 
            (Cons  
              (a $148 $148) $184))) 
        (Cons  
          (Cons  
            (/ c 3) 
            (Cons  
              (= 
                (c $191 $192 $193) 
                (a $191 $192)) $238)) $239)) $68 
      (Cons  
        (c $76 $77 $78) $102)) 
    (set-det))
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
    (entry-exit-modes-list $_ $_ $Entries) 
    (var $Entries))
; 

  (= 
    (entry-exit-modes-list $ProcList $Known 
      (Cons  $Entry $Entries)) 
    ( (=.. $Entry 
        (Cons  $Functor $Act)) 
      (length $Act $Arity) 
      (proc-exit-mode $ProcList $Known Nil 
        (/ $Functor $Arity) $Act $_) 
      (entry-exit-modes-list $ProcList $Known $Entries)))
; 


;
; 

;
; 


  (= 
    (proc-exit-mode $_ $_ $_ 
      (/ $Functor $Arity) $Act $Exit) 
    (built-in 
      (/ $Functor $Arity) $Act $Exit))
; 
		    ;
; 

  (= 
    (proc-exit-mode $_ $Known $_ 
      (/ $Functor $Arity) $Act $Exit) 
    (look-up-act 
      (:: 
        (/ $Functor $Arity) $Act $Exit) $Known))
; 
       ;
; 

  (= 
    (proc-exit-mode $ProcList $Known $Invocations 
      (/ $Functor $Arity) $Act $Exit) 
    ( (umember 
        (Cons  
          (/ $Functor $Arity) $Clauses) $ProcList) 
      (dup $Clauses $ClausesCopy) 
      (clause-exit-modes-list $ProcList $Known $Invocations $ClausesCopy $Act $Exits) 
      (det-if-then-else 
        (= $Exits Nil) fail True) 
      (worst-case $Exits $Exit) 
      (dup $Act $ActCopy) 
      (add-to-list 
        (:: 
          (/ $Functor $Arity) $ActCopy $Exit) $Known)))
; 
   ;
; 

  (= 
    (proc-exit-mode $_ $Known $_ 
      (/ $Functor $Arity) $Act $Exit) 
    ( (=.. $Activation 
        (Cons  $Functor $Act)) 
      (all-shared $Act $Exit) 
      (add-to-list 
        (:: 
          (/ $Functor $Arity) $Act $Exit) $Known)))
; 


;
; 


  (= 
    (clause-exit-modes-list $_ $_ $_ $Clauses $_ Nil) 
    ( (var $Clauses) (set-det)))
; 
			       ;
; 

  (= 
    (clause-exit-modes-list $ProcList $Known $Invocations 
      (Cons  $Clause $Clauses) $Act $Exits) 
    ( (eqmember 
        (:: $Clause $Act) $Invocations) (clause-exit-modes-list $ProcList $Known $Invocations $Clauses $Act $Exits)))
; 
		;
; 

  (= 
    (clause-exit-modes-list $ProcList $Known $Invocations 
      (Cons  $Clause $Clauses) $Act 
      (Cons  $Exit $Exits)) 
    ( (dup $Act $Exit) 
      (clause-exit-mode $ProcList $Known 
        (Cons  
          (:: $Clause $Act) $Invocations) $Clause $Exit) 
      (clause-exit-modes-list $ProcList $Known $Invocations $Clauses $Act $Exits)))
; 

  (= 
    (clause-exit-modes-list $ProcList $Known $Invocations 
      (Cons  $Clause $Clauses) $Act $Exits) 
    (clause-exit-modes-list $ProcList $Known $Invocations $Clauses $Act $Exits))
; 
   

;
; 


  (= 
    (clause-exit-mode $ProcList $Known $Invocations $Clause $Act) 
    ( (or 
        (= $Clause 
          (= $Head $Body)) 
        (, 
          (= $Clause $Head) 
          (= $Body True))) 
      (=.. $Head 
        (Cons  $_ $Args)) 
      (unify $Args $Act) 
      (body-exit-mode $ProcList $Known $Invocations $Body)))
; 
 	;
; 



  (= 
    (body-exit-mode $ProcList $Known $Invocations 
      (, $Goal $Goals)) 
    ( (body-exit-mode $ProcList $Known $Invocations $Goal) (body-exit-mode $ProcList $Known $Invocations $Goals)))
; 
	;
; 

  (= 
    (body-exit-mode $ProcList $Known $Invocation $Goal) 
    ( (functor $Goal $Functor $Arity) 
      (=.. $Goal 
        (Cons  $Functor $Act)) 
      (proc-exit-mode $ProcList $Known $Invocation 
        (/ $Functor $Arity) $Act $Exit) 
      (unify $Act $Exit)))
; 
	

;
; 

;
; 


  (= 
    (unify $Left $Left) 
    (set-det))
; 
				;
; 

  (= 
    (unify $Left g) 
    ( (atomic $Left) 
      (set-det) 
      (not (= $Left Nil))))
; 

  (= 
    (unify g $Right) 
    ( (atomic $Right) 
      (set-det) 
      (not (= $Right Nil))))
; 

  (= 
    (unify 
      (Cons  $LeftHead $LeftTail) 
      (Cons  $RightHead $RightTail)) 
    ( (set-det) 
      (unify $LeftHead $RightHead) 
      (unify $LeftTail $RightTail)))
; 

  (= 
    (unify $Left $Right) 
    ( (=.. $Left 
        (Cons  $Functor $LeftArgs)) 
      (=.. $Right 
        (Cons  $Functor $RightArgs)) 
      (unify $LeftArgs $RightArgs)))
; 

	
;
; 

;
; 


  (= 
    (equiv $Left $Right) 
    (equiv $Left $Right $_))
; 

  (= 
    (equiv $Left $Right $_) 
    ( (== $Left $Right) (set-det)))
; 
			
  (= 
    (equiv g $Right $_) 
    ( (atomic $Right) 
      (set-det) 
      (not (= $Right Nil))))
; 
			
  (= 
    (equiv $Left g $_) 
    ( (atomic $Left) 
      (set-det) 
      (not (= $Left Nil))))
; 
			
  (= 
    (equiv $Left $Right $Bindings) 
    ( (var $Left) 
      (set-det) 
      (var $Right) 
      (equiv-vars $Left $Right $Bindings)))
; 

  (= 
    (equiv $Left $Right $Bindings) 
    ( (var $Right) 
      (set-det) 
      (var $Left) 
      (equiv-vars $Left $Right $Bindings)))
; 

  (= 
    (equiv 
      (Cons  $LeftHead $LeftTail) 
      (Cons  $RightHead $RightTail) $Bindings) 
    ( (set-det) 
      (equiv $LeftHead $RightHead $Bindings) 
      (equiv $LeftTail $RightTail $Bindings)))
; 

  (= 
    (equiv $Left $Right $Bindings) 
    ( (=.. $Left 
        (Cons  $Functor $LeftArgs)) 
      (=.. $Right 
        (Cons  $Functor $RightArgs)) 
      (equiv $LeftArgs $RightArgs $Bindings)))
; 



  (= 
    (equiv-vars $Left $Right $Bindings) 
    ( (var $Bindings) 
      (set-det) 
      (= $Bindings 
        (Cons  
          (:: $Left $Right) $_))))
; 

  (= 
    (equiv-vars $Left $Right 
      (Cons  
        (:: $AnyVar $AnyBinding) $_)) 
    ( (== $Left $AnyVar) 
      (set-det) 
      (== $Right $AnyBinding)))
; 

  (= 
    (equiv-vars $Left $Right 
      (Cons  
        (:: $AnyVar $AnyBinding) $_)) 
    ( (== $Right $AnyBinding) 
      (set-det) 
      (== $Left $AnyVar)))
; 

  (= 
    (equiv-vars $Left $Right 
      (Cons  $_ $Bindings)) 
    (equiv-vars $Left $Right $Bindings))
; 


;
; 

;
; 


  (= 
    (dup $Orig $Copy) 
    (dup $Orig $Copy $_))
; 

  (= 
    (dup $Orig $Copy $Bindings) 
    ( (var $Orig) 
      (set-det) 
      (dup-var $Orig $Copy $Bindings)))
; 

  (= 
    (dup $Orig $Orig $_) 
    ( (atomic $Orig) (set-det)))
; 

  (= 
    (dup 
      (Cons  $OrigHead $OrigTail) 
      (Cons  $CopyHead $CopyTail) $Bindings) 
    ( (set-det) 
      (dup $OrigHead $CopyHead $Bindings) 
      (dup $OrigTail $CopyTail $Bindings)))
; 

  (= 
    (dup $Orig $Copy $Bindings) 
    ( (=.. $Orig 
        (Cons  $Functor $OrigArgs)) 
      (dup $OrigArgs $CopyArgs $Bindings) 
      (=.. $Copy 
        (Cons  $Functor $CopyArgs))))
; 



  (= 
    (dup-var $Orig $Copy $Bindings) 
    ( (var $Bindings) 
      (set-det) 
      (= $Bindings 
        (Cons  
          (:: $Orig $Copy) $_))))
; 

  (= 
    (dup-var $Orig $Copy 
      (Cons  
        (:: $AnyVar $Copy) $_)) 
    ( (== $Orig $AnyVar) (set-det)))
; 

  (= 
    (dup-var $Orig $Copy 
      (Cons  $_ $Bindings)) 
    (dup-var $Orig $Copy $Bindings))
; 


;
; 



  (= 
    (built_in  
      (/  true 0) () ()) True)
; 
			;
; 

  (= 
    (built_in  
      (/  fail 0) () ()) True)
; 
			;
; 

  (= 
    (built-in 
      (/ = 2) 
      (:: $X $Y) 
      (:: g g)) 
    (or 
      (atomic $X) 
      (atomic $Y)))
; 
 		;
; 

  (= 
    (built_in  
      (/  = 2) 
      ($X $Y) 
      ($X $X)) True)
; 
		;
; 

  (= 
    (built_in  
      (/  + 2) 
      ($X $Y) 
      ($X $Y)) True)
; 
		;
; 

  (= 
    (built_in  
      (/  - 2) 
      ($X $Y) 
      ($X $Y)) True)
; 
		;
; 

  (= 
    (built_in  
      (/  * 2) 
      ($X $Y) 
      ($X $Y)) True)
; 
		;
; 

  (= 
    (built_in  
      (/  / 2) 
      ($X $Y) 
      ($X $Y)) True)
; 
		;
; 

  (= 
    (built_in  
      (/  >= 2) 
      ($X $Y) 
      ($X $Y)) True)
; 
		;
; 

  (= 
    (built_in  
      (/  < 2) 
      ($X $Y) 
      ($X $Y)) True)
; 
		;
; 

  (= 
    (built_in  
      (/  is 2) 
      ($X $Y) 
      (g $Y)) True)
; 
			;
; 


;
; 



  (= 
    (worst_case  () $_) True)
; 
				;
; 

  (= 
    (worst-case 
      (Cons  $Exit $Exits) $Worst) 
    ( (unify $Exit $Worst) (worst-case $Exits $Worst)))
; 



  (= 
    (look-up-act $_ $Known) 
    ( (var $Known) 
      (set-det) 
      (fail)))
; 
    
  (= 
    (look-up-act 
      (:: 
        (/ $Functor $Arity) $Act $Exit) 
      (Cons  
        (:: 
          (/ $Functor $Arity) $KnownAct $Exit) $_)) 
    (equiv $Act $KnownAct))
; 
    
  (= 
    (look-up-act 
      (:: 
        (/ $Functor $Arity) $Act $Exit) 
      (Cons  $_ $Known)) 
    (look-up-act 
      (:: 
        (/ $Functor $Arity) $Act $Exit) $Known))
; 



  (= 
    (all-shared $Act $Exit) 
    ( (unify $Act $_ $VarModesList) 
      (bind-all $_ $VarModesList) 
      (unify $Act $Exit $VarModesList)))
; 



  (= 
    (bind-all $_ $VarModesList) 
    (var $VarModesList))
; 

  (= 
    (bind-all $Mode 
      (Cons  
        (:: $Var $Mode) $VarModesList)) 
    ( (var $Mode) (bind-all $Mode $VarModesList)))
; 

  (= 
    (bind-all $Mode 
      (Cons  
        (:: $_ $_) $VarModesList)) 
    (bind-all $Mode $VarModesList))
; 



;
; 


  (= 
    (add-to-list $Element $List) 
    ( (var $List) (= $List (Cons  $Element $_))))
; 

  (= 
    (add-to-list $Element 
      (Cons  $_ $List)) 
    (add-to-list $Element $List))
; 


;
; 


  (= 
    (umember $_ $List) 
    ( (var $List) 
      (set-det) 
      (fail)))
; 

  (= 
    (umember  $Element 
      (Cons  $Element $_)) True)
; 

  (= 
    (umember $Element 
      (Cons  $_ $Tail)) 
    (umember $Element $Tail))
; 


;
; 


  (= 
    (sumember $_ $List) 
    ( (var $List) 
      (set-det) 
      (fail)))
; 

  (= 
    (sumember $Element 
      (Cons  $AnyElement $_)) 
    (== $Element $AnyElement))
; 

  (= 
    (sumember $Element 
      (Cons  $_ $Tail)) 
    (sumember $Element $Tail))
; 


;
; 


  (= 
    (member  $X 
      (Cons  $X $_)) True)
; 

  (= 
    (member $X 
      (Cons  $_ $T)) 
    (member $X $T))
; 


;
; 


  (= 
    (smember $X 
      (Cons  $Y $_)) 
    (== $X $Y))
; 

  (= 
    (smember $X 
      (Cons  $_ $T)) 
    (smember $X $T))
; 


;
; 


  (= 
    (eqmember $X 
      (Cons  $Y $_)) 
    (equiv $X $Y))
; 

  (= 
    (eqmember $X 
      (Cons  $_ $T)) 
    (eqmember $X $T))
; 


;
; 


  (= 
    (concat  () $L $L) True)
; 

  (= 
    (concat 
      (Cons  $X $L1) $L2 
      (Cons  $X $L3)) 
    (concat $L1 $L2 $L3))
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

	
  (= nl True)
; 
				

  (= 
    (write-list2 
      (Cons  $H $T) $NextName $NewNextName) 
    ( (name-vars $H $NextName $TempNextName) 
      (write $H) 
      (det-if-then-else 
        (nonvar $T) 
        (, 
          (write ,) 
          (nl) 
          (write ' ') 
          (write-list2 $T $TempNextName $NewNextName)) 
        (= $NewNextName $TempNextName))))
; 



  (= 
    (name-vars $Term $NextName $NewNextName) 
    ( (var $Term) 
      (set-det) 
      (make-name $NextName $Term) 
      (is $NewNextName 
        (+ $NextName 1))))
; 

  (= 
    (name-vars $Term $NextName $NextName) 
    ( (atom $Term) (set-det)))
; 

  (= 
    (name-vars 
      (Cons  $TermHead $TermTail) $NextName $NewNextName) 
    ( (set-det) 
      (name-vars $TermHead $NextName $TempNextName) 
      (name-vars $TermTail $TempNextName $NewNextName)))
; 

  (= 
    (name-vars $Term $NextName $NewNextName) 
    ( (=.. $Term 
        (Cons  $_ $TermArgs)) (name-vars $TermArgs $NextName $NewNextName)))
; 



  (= 
    (make-name $IntName $Variable) 
    ( (is $Count 
        (// $IntName 26)) 
      (is $NewIntName 
        (+ 
          (mod $IntName 26) "A")) 
      (build-name $Count $NewIntName $Name) 
      (name $Variable $Name)))
; 



  (= 
    (build-name 0 $IntName 
      (:: $IntName)) 
    (set-det))
; 

  (= 
    (build-name $Count $IntName 
      (Cons  $IntName $Rest)) 
    ( (> $Count 0) 
      (is $NewCount 
        (- $Count 1)) 
      (build-name $NewCount $IntName $Rest)))
; 



