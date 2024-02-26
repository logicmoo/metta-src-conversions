;                                           (convert-to-metta-file  sdda $_151926 sxx_machine/bench/sdda.pl sxx_machine/bench/sdda.metta)
;
; Sdda3 5-Oct-86
;
; For use on simulator

;
; ; To do: (look for ';;')
;
; ; recursion - keep list of call procedures, ignore recursive calls
;
; ; problem: doesn't work for typical procedure working on a list,
;
; ; since the list is smaller (different) each time.
;
; ; possible optimization: "recognize" base case & skip to it
;
; ; follow atoms, g is 'any atom', all others unique, does it work?
;
; ; stats - write heapused, cputime to files (as comments)
;
; ; worst_case - handle ground terms (copy unify, modify atomic)
;
; ; handle disjunction - needs worst_case
;
; ; add cuts where possible to save space
;
; ; fill in rest of built-ins
;
; ; how to handle op?
;
; ; Handle assert/retract? call? (If given ground terms- ok, vars- no)
;
; ; must have ground functor, definite number of args!

;
; Front end for simulator use

  (= top  
    (do-sdda test $A $B $C))

;
; Does the sdda on FileName, instantiates Exitmodes to list of exit modes,
;
; ExitModes structure: [[Funtor/Arity, Activation, Exit], ... ],
;
; e.g. [[a/2, [g,X], [g,g]]

  (= (do-sdda $FileName $ExitModes $BackList $PredList)  
    (read-procedures $Procs $ExitModes $Entries)
    (entry-exit-modes-list $Procs $ExitModes $Entries))
; ;;see(FileName),
; ; collect all procedures
; ;;seen, ; write('Procedures '), nl, write_list(Procs), nl, ; write('Entry points '), nl, write_list(Entries), nl, ; (nonvar(ExitModes) -> ; Don't mention there ; (write('Declared exit modes '), nl, ; aren't any ; write_list(ExitModes), nl) ; ; true),
;
; write('Exit modes '), nl, write_list(ExitModes), nl.

;
; ;; !!! Hard code in read for test:
;
; sdda_entry(c(A,B,C)).
;
; a(X, Y).
;
; a(X, X).
;
; c(A,B,C) :- a(A,B).


  (= (read-procedures (Cons  (Cons  (/ a 2) (Cons  (a $109 $110) (Cons  (a $148 $148) $184))) (Cons  (Cons  (/ c 3) (Cons  (= (c $191 $192 $193)  
    (a $191 $192)) $238)) $239)) $68 (Cons  (c $76 $77 $78) $102))  
    (set-det))

;
; For each entry point in Entries do sdda, building Known, an unbound-tail list
;
; Known structure: [[Name/Arity, ActivationModes, ExitModes], ...|_],
;
; where ActivationModes and ExitModes are lists of variables and the atom 'g'.
;
; 'g' represents a ground element and variables represent equivalence classes.

  (= (entry-exit-modes-list $_ $_ $Entries)  
    (var $Entries))
; ; Done
  (= (entry-exit-modes-list $ProcList $Known (Cons  $Entry $Entries))  
    (=.. $Entry 
      (Cons  $Functor $Act))
    (length $Act $Arity)
    (proc-exit-mode $ProcList $Known Nil 
      (/ $Functor $Arity) $Act $_)
    (entry-exit-modes-list $ProcList $Known $Entries))
; ; Get functor/arity & activation
; ; from entry declaration
; ; No invoc.

;
; Do sdda on procedure Functor/Arity, given activation mode Act. Instantiates
;
; Known to known exit modes and Act to exit modes for Functor/Arity under Act

  (= (proc-exit-mode $_ $_ $_ (/ $Functor $Arity) $Act $Exit)  
    (built-in 
      (/ $Functor $Arity) $Act $Exit))		    ;
; This is a built-in
  (= (proc-exit-mode $_ $Known $_ (/ $Functor $Arity) $Act $Exit)  
    (look-up-act 
      (:: 
        (/ $Functor $Arity) $Act $Exit) $Known))       ;
; Already did this
  (= (proc-exit-mode $ProcList $Known $Invocations (/ $Functor $Arity) $Act $Exit)  
    (umember 
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
        (/ $Functor $Arity) $ActCopy $Exit) $Known))
; ; Look up definition
; ; Don't munge original
; ; didn't find any => fail
; ; assume the worst
; ; Need copy because Body   ;
; binds Act & Exit
  (= (proc-exit-mode $_ $Known $_ (/ $Functor $Arity) $Act $Exit)  
    (=.. $Activation 
      (Cons  $Functor $Act))
    (all-shared $Act $Exit)
    (add-to-list 
      (:: 
        (/ $Functor $Arity) $Act $Exit) $Known))
; ; write('No such procedure at compile time '),
; ; write(Activation), nl,
; ; return worst possible - all shared

;
; Analyze all clauses for this procedure, instantiate Exits to all exit modes

  (= (clause-exit-modes-list $_ $_ $_ $Clauses $_ Nil)  
    (var $Clauses)
    (set-det))			       ;
; No more clauses => done
  (= (clause-exit-modes-list $ProcList $Known $Invocations (Cons  $Clause $Clauses) $Act $Exits)  
    (eqmember 
      (:: $Clause $Act) $Invocations)
    (clause-exit-modes-list $ProcList $Known $Invocations $Clauses $Act $Exits))
; ; This is a recursive ; write('skipping clause exit mode for '), ; write(Clause), write(' '), write(Act), nl,
; ; call, ignore		;
; it
  (= (clause-exit-modes-list $ProcList $Known $Invocations (Cons  $Clause $Clauses) $Act (Cons  $Exit $Exits))  
    (dup $Act $Exit)
    (clause-exit-mode $ProcList $Known 
      (Cons  
        (:: $Clause $Act) $Invocations) $Clause $Exit)
    (clause-exit-modes-list $ProcList $Known $Invocations $Clauses $Act $Exits))
; ; We'll bind Exit
; ; Record invocation
  (= (clause-exit-modes-list $ProcList $Known $Invocations (Cons  $Clause $Clauses) $Act $Exits)  
    (clause-exit-modes-list $ProcList $Known $Invocations $Clauses $Act $Exits))
; ; Unify failed   

;
; Given activation modes for this clause, return its exit modes

  (= (clause-exit-mode $ProcList $Known $Invocations $Clause $Act)  
    (or 
      (= $Clause 
        (= $Head  $Body)) 
      (, 
        (= $Clause $Head) 
        (= $Body True)))
    (=.. $Head 
      (Cons  $_ $Args))
    (unify $Args $Act)
    (body-exit-mode $ProcList $Known $Invocations $Body))
; ; Decompose it
; ; Bind the head
; ; to activation 	;
; do the body


  (= (body-exit-mode $ProcList $Known $Invocations (, $Goal $Goals))  
    (body-exit-mode $ProcList $Known $Invocations $Goal)
    (body-exit-mode $ProcList $Known $Invocations $Goals))
; ; Conjunction
; ; Do 1st	;
; & rest
  (= (body-exit-mode $ProcList $Known $Invocation $Goal)  
    (functor $Goal $Functor $Arity)
    (=.. $Goal 
      (Cons  $Functor $Act))
    (proc-exit-mode $ProcList $Known $Invocation 
      (/ $Functor $Arity) $Act $Exit)
    (unify $Act $Exit))	

;
; Unifies Left and Right with the special case that the atom 'g' matches
;
; any atom (except [])

  (= (unify $Left $Left)  
    (set-det))				;
; Try standard unify first
  (= (unify $Left g)  
    (atomic $Left)
    (set-det)
    (not (= $Left Nil)))
; ; else, is it special case
  (= (unify g $Right)  
    (atomic $Right)
    (set-det)
    (not (= $Right Nil)))
  (= (unify (Cons  $LeftHead $LeftTail) (Cons  $RightHead $RightTail))  
    (set-det)
    (unify $LeftHead $RightHead)
    (unify $LeftTail $RightTail))
; ; or list
  (= (unify $Left $Right)  
    (=.. $Left 
      (Cons  $Functor $LeftArgs))
    (=.. $Right 
      (Cons  $Functor $RightArgs))
    (unify $LeftArgs $RightArgs))
; ; or structure
	
;
; Succeed if Left and Right are equivalent, i.e. they are the exact same
;
; with variables renamed

  (= (equiv $Left $Right)  
    (equiv $Left $Right $_))
  (= (equiv $Left $Right $_)  
    (== $Left $Right)
    (set-det))			
  (= (equiv g $Right $_)  
    (atomic $Right)
    (set-det)
    (not (= $Right Nil)))			
  (= (equiv $Left g $_)  
    (atomic $Left)
    (set-det)
    (not (= $Left Nil)))			
  (= (equiv $Left $Right $Bindings)  
    (var $Left)
    (set-det)
    (var $Right)
    (equiv-vars $Left $Right $Bindings))
  (= (equiv $Left $Right $Bindings)  
    (var $Right)
    (set-det)
    (var $Left)
    (equiv-vars $Left $Right $Bindings))
  (= (equiv (Cons  $LeftHead $LeftTail) (Cons  $RightHead $RightTail) $Bindings)  
    (set-det)
    (equiv $LeftHead $RightHead $Bindings)
    (equiv $LeftTail $RightTail $Bindings))
  (= (equiv $Left $Right $Bindings)  
    (=.. $Left 
      (Cons  $Functor $LeftArgs))
    (=.. $Right 
      (Cons  $Functor $RightArgs))
    (equiv $LeftArgs $RightArgs $Bindings))


  (= (equiv-vars $Left $Right $Bindings)  
    (var $Bindings)
    (set-det)
    (= $Bindings 
      (Cons  
        (:: $Left $Right) $_)))
  (= (equiv-vars $Left $Right (Cons  (:: $AnyVar $AnyBinding) $_))  
    (== $Left $AnyVar)
    (set-det)
    (== $Right $AnyBinding))
  (= (equiv-vars $Left $Right (Cons  (:: $AnyVar $AnyBinding) $_))  
    (== $Right $AnyBinding)
    (set-det)
    (== $Left $AnyVar))
  (= (equiv-vars $Left $Right (Cons  $_ $Bindings))  
    (equiv-vars $Left $Right $Bindings))

;
; Make a copy of Orig with new vars. Copy must be a variable.
;
; E.g. dup([A,s(A,B),[B,C]], New) binds New to [X,s(X,Y),[Y,Z]]

  (= (dup $Orig $Copy)  
    (dup $Orig $Copy $_))
  (= (dup $Orig $Copy $Bindings)  
    (var $Orig)
    (set-det)
    (dup-var $Orig $Copy $Bindings))
  (= (dup $Orig $Orig $_)  
    (atomic $Orig)
    (set-det))
; ; Atoms, including []
  (= (dup (Cons  $OrigHead $OrigTail) (Cons  $CopyHead $CopyTail) $Bindings)  
    (set-det)
    (dup $OrigHead $CopyHead $Bindings)
    (dup $OrigTail $CopyTail $Bindings))
  (= (dup $Orig $Copy $Bindings)  
    (=.. $Orig 
      (Cons  $Functor $OrigArgs))
    (dup $OrigArgs $CopyArgs $Bindings)
    (=.. $Copy 
      (Cons  $Functor $CopyArgs)))


  (= (dup-var $Orig $Copy $Bindings)  
    (var $Bindings)
    (set-det)
    (= $Bindings 
      (Cons  
        (:: $Orig $Copy) $_)))
  (= (dup-var $Orig $Copy (Cons  (:: $AnyVar $Copy) $_))  
    (== $Orig $AnyVar)
    (set-det))
  (= (dup-var $Orig $Copy (Cons  $_ $Bindings))  
    (dup-var $Orig $Copy $Bindings))

;
; ----- Built-ins ----- ;


  (= (built-in  (/  true 0) () ())  True)			;
; No change
  (= (built-in  (/  fail 0) () ())  True)			;
; No change
  (= (built-in (/ = 2) (:: $X $Y) (:: g g))  
    (or 
      (atomic $X) 
      (atomic $Y))) 		;
; Ground both if either atomic
  (= (built-in  (/  = 2) ($X $Y) ($X $X))  True)		;
; else bind them
  (= (built-in  (/  + 2) ($X $Y) ($X $Y))  True)		;
; No change
  (= (built-in  (/  - 2) ($X $Y) ($X $Y))  True)		;
; No change
  (= (built-in  (/  * 2) ($X $Y) ($X $Y))  True)		;
; No change
  (= (built-in  (/  / 2) ($X $Y) ($X $Y))  True)		;
; No change
  (= (built-in  (/  >= 2) ($X $Y) ($X $Y))  True)		;
; No change
  (= (built-in  (/  < 2) ($X $Y) ($X $Y))  True)		;
; No change
  (= (built-in  (/  is 2) ($X $Y) (g $Y))  True)			;
; Ground result

;
; ----- Utilities ----- ;


  (= (worst-case  () $_)  True)				;
; ; Doesn't work if any Exits
  (= (worst-case (Cons  $Exit $Exits) $Worst)  
    (unify $Exit $Worst)
    (worst-case $Exits $Worst))
; ;; fail to match, e.g.
; ;; [[s(1)], [f(1)]].


  (= (look-up-act $_ $Known)  
    ( (var $Known) 
      (set-det) fail))    
  (= (look-up-act (:: (/ $Functor $Arity) $Act $Exit) (Cons  (:: (/ $Functor $Arity) $KnownAct $Exit) $_))  
    (equiv $Act $KnownAct))    
  (= (look-up-act (:: (/ $Functor $Arity) $Act $Exit) (Cons  $_ $Known))  
    (look-up-act 
      (:: 
        (/ $Functor $Arity) $Act $Exit) $Known))


  (= (all-shared $Act $Exit)  
    (unify $Act $_ $VarModesList)
    (bind-all $_ $VarModesList)
    (unify $Act $Exit $VarModesList))
; ;; Wrong


  (= (bind-all $_ $VarModesList)  
    (var $VarModesList))
  (= (bind-all $Mode (Cons  (:: $Var $Mode) $VarModesList))  
    (var $Mode)
    (bind-all $Mode $VarModesList))
  (= (bind-all $Mode (Cons  (:: $_ $_) $VarModesList))  
    (bind-all $Mode $VarModesList))


;
; Adds Element to the tail of List, an unbound-tail list

  (= (add-to-list $Element $List)  
    (var $List)
    (= $List 
      (Cons  $Element $_)))
  (= (add-to-list $Element (Cons  $_ $List))  
    (add-to-list $Element $List))

;
; Membership relation for unbound-tail lists

  (= (umember $_ $List)  
    ( (var $List) 
      (set-det) fail))
  (= (umember  $Element ([|]  $Element $_))  True)
  (= (umember $Element (Cons  $_ $Tail))  
    (umember $Element $Tail))

;
; Strict membership relation for unbound-tail lists

  (= (sumember $_ $List)  
    ( (var $List) 
      (set-det) fail))
  (= (sumember $Element (Cons  $AnyElement $_))  
    (== $Element $AnyElement))
  (= (sumember $Element (Cons  $_ $Tail))  
    (sumember $Element $Tail))

;
; Membership relation for standard nil-tail lists

  (= (member  $X ([|]  $X $_))  True)
  (= (member $X (Cons  $_ $T))  
    (member $X $T))

;
; Strict membership relation for standard nil-tail lists

  (= (smember $X (Cons  $Y $_))  
    (== $X $Y))
  (= (smember $X (Cons  $_ $T))  
    (smember $X $T))

;
; Equiv membership relation for standard nil-tail lists

  (= (eqmember $X (Cons  $Y $_))  
    (equiv $X $Y))
  (= (eqmember $X (Cons  $_ $T))  
    (eqmember $X $T))

;
; Our old favorite

  (= (concat  () $L $L)  True)
  (= (concat (Cons  $X $L1) $L2 (Cons  $X $L3))  
    (concat $L1 $L2 $L3))

;
; Pretty prints unbound-tail lists -- dies on NIL tail lists
;
; write_list(List) :-
;
; dup(List, NewList),
;
; (var(NewList) -> (name_vars(NewList, 0, _));
;
; write(NewList)) ;
;
; (write('['),
;
; write_list2(NewList, 0, _),
;
; write('|_].'))), ; write('].') to write nil tails
	
  (= nl  True)				

  (= (write-list2 (Cons  $H $T) $NextName $NewNextName)  
    (name-vars $H $NextName $TempNextName)
    (write $H)
    (det-if-then-else 
      (nonvar $T) 
      (, 
        (write ,) nl 
        (write ' ') 
        (write-list2 $T $TempNextName $NewNextName)) 
      (= $NewNextName $TempNextName)))


  (= (name-vars $Term $NextName $NewNextName)  
    (var $Term)
    (set-det)
    (make-name $NextName $Term)
    (is $NewNextName 
      (+ $NextName 1)))
  (= (name-vars $Term $NextName $NextName)  
    (atom $Term)
    (set-det))
  (= (name-vars (Cons  $TermHead $TermTail) $NextName $NewNextName)  
    (set-det)
    (name-vars $TermHead $NextName $TempNextName)
    (name-vars $TermTail $TempNextName $NewNextName))
  (= (name-vars $Term $NextName $NewNextName)  
    (=.. $Term 
      (Cons  $_ $TermArgs))
    (name-vars $TermArgs $NextName $NewNextName))


  (= (make-name $IntName $Variable)  
    (is $Count 
      (// $IntName 26))
    (is $NewIntName 
      (+ 
        (mod $IntName 26) "A"))
    (build-name $Count $NewIntName $Name)
    (name $Variable $Name))


  (= (build-name 0 $IntName (:: $IntName))  
    (set-det))
  (= (build-name $Count $IntName (Cons  $IntName $Rest))  
    (> $Count 0)
    (is $NewCount 
      (- $Count 1))
    (build-name $NewCount $IntName $Rest))


