;                                           (convert-to-metta-file  statify_theory $_34076 metagame/state/statify_theory.pl metagame/state/statify_theory.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;; statify_theory.pl
;
; ;; To transform a domain theory with true, add, and del statements
;
; ;; into one with a threaded StateIn, and StateOut pair.

  !(my-ensure-loaded (library aux))
  !(my-ensure-loaded (library stat))

;
; ----------------------------------------
;
; The following are defined in sysdev.pl

;
; THEORY_FILES(Files)

;
; DYNAMIC_PREDS_FILE(File)

;
; ----------------------------------------


;
; COMPILE_AND_LOAD_PLAYER
;
; This command does the state compilation on all the player theory files,
;
; and then loads them.
;
; Not nec. to call: load_compiled_play_theory/0 here,
;
; as we compile-load each file immediately after writing it.
;
; 

  (= compile-and-load-player  state-compile-player)
	

;
; STATE_COMPILE_PLAYER
;
; Compile the dynamic pred declarations into a theory file.
;
; Then index them for efficient state representation.
;
; Then load all theory files as data, and perform the
;
; state compilation on the theory.
;
; Finally, clear the loaded theory files, as after this
;
; we'll only use the compiled MeTTa versions.
;
; (Systems which want the theory around after that should load
;
; it again.)
;
; 

  (= state-compile-player  
    (compile-dynamic-preds index-dynamic-preds player-theory-load player-theory-compile theory-clear))


  (= player-theory-load  
    ( (format "Loading Theory: MetaGame~n" Nil) theory-clear 
      (player-theory-files $Files) 
      (dl $Files)))


  (= player-theory-compile  
    ( (format "Compiling Theory: MetaGame~n" Nil) stativity-analysis compile-player-files))


  (= compile-player-files  
    (theory-files $Files)
    (whenever 
      (member $File $Files) 
      (thcomp $File)))


  (= (player-compiled-files $CompFiles)  
    (theory-files $Files)
    (bagof $CompFile 
      (^ $File 
        (, 
          (member $File $Files) 
          (theory-statname $File $CompFile))) $CompFiles))  


  (= (player-theory-files $CompFiles)  
    (theory-files $Files)
    (bagof $CompFile 
      (^ $File 
        (, 
          (member $File $Files) 
          (theory-filename $File $CompFile))) $CompFiles))  



;
; LOAD_COMPILED_PLAY_THEORY
;
; Loads all the compiled player theory files.
;
; If they have been state_compiled already, and none have changed,
;
; this is the only call nec. to load them.
;
; (Thus, someone just using the player need only know this command
;
; once he has compiled the first time.
;
; 

  (= load-compiled-play-theory  
    (player-compiled-files $Fs)
    (whenever 
      (member $F $Fs) 
      (compile $F)))



  (= (theory-filename $FileRoot $CompFileName)  
    (sys-suffixed-filename $FileRoot prolog $File)
    (theory-directory $TheoryDir)
    (concat-list 
      (:: $TheoryDir / $File) $CompFileName))


  (= (theory-statname $FileRoot $CompFileName)  
    (sys-suffixed-filename $FileRoot state-compile $File)
    (theory-directory $TheoryDir)
    (concat-list 
      (:: $TheoryDir / $File) $CompFileName))



  (= (thcomp $FileRoot)  
    (theory-filename $FileRoot $FileIn)
    (theory-statname $FileRoot $FileOut)
    (state-compile-file $FileIn $FileOut)
    (compile $FileOut))


  (= (state-compile-file $FileIn $FileOut)  
    ( (format "Compiling theory file: ~w~n" 
        (:: $FileIn)) 
      (see $FileIn) 
      (tell $FileOut) compile-clauses 
      (set-det) seen told 
      (format "Wrote compiled file: ~w~n" 
        (:: $FileOut))))
; ; don't know where nondet is.


  (= compile-clauses  
    (det-if-then-else 
      (, 
        (read $ClauseIn) 
        (\== $ClauseIn end-of-file)) 
      (, 
        (compile-clause $ClauseIn $ClauseOut) 
        (portray-clause $ClauseOut) compile-clauses) True))



  (= (compile-clause $CIn $COut)  
    (clause-parts $CIn $HIn $BIn)
    (thread-clause $HIn $BIn $COut))


  (= (thread-clause $HeadIn $BodyIn $ClauseOut)  
    (add-state $HeadIn $SIn $SOut $HeadOut)
    (thread $BodyIn $SIn $SOut $BodyOut)
    (clause-parts $ClauseOut $HeadOut $BodyOut))



  (= (add-state $GoalIn $Stativity $GoalOut)  
    (add-state $GoalIn $SIn $SOut $Stativity $GoalOut))
; /* A goal G should be threaded with SIn/SOut if: a. It is an Add/DEL literal (thread SIn,SOut). b. It is a True literal (thread SIn). c. There is an interpreted pred: G :- Body, and the body should be threaded. A body B1,B2, ... should be threaded if one of its Bi should be. */

  (= (add-state $GoalIn $SIn $SOut $GoalOut)  
    (stat $GoalIn $Stat)
    (add-state $GoalIn $SIn $SOut $Stat $GoalOut))


;
; ADD_STATE(Goalin,SIn,SOut,Stativity,GoalOut)
;
; add_state(foo(a,b),A,B,0,GoalOut) --> GoalOut = foo(a,b).
;
; add_state(foo(a,b),A,B,1,GoalOut) --> GoalOut = foo(a,b,A).
;
; add_state(foo(a,b),A,B,2,GoalOut) --> GoalOut = foo(a,b,A,B).
  (= (add-state  $GoalIn $SIn $SIn 0 $GoalIn)  True)
  (= (add-state $GoalIn $SIn $SIn 1 $GoalOut)  
    (thread-in-state $GoalIn $SIn $GoalOut))
  (= (add-state $GoalIn $SIn $SOut 2 $GoalOut)  
    (thread-in-state $GoalIn $SIn $G1)
    (thread-in-state $G1 $SOut $GoalOut))


  (= (thread-in-state $GoalIn $SIn $GoalOut)  
    (increase-term-arity $GoalIn $SIn $GoalOut))



;
; ================================================================================
;
; Compiling Dynamic Predicates.
;
; ================================================================================

;
; Declarations of dynamic predicates.
;
; These are as they appear in the game theory definitions.
;
; 
;
; Ex.: move_count(L) ==> move_count(L,S) :- true_in(move_count(L),S).
;
; 

  (= (dynamic-predicates  ((control  $Player) (on  $Piece $Square) (moved-onto  $Piece $Sq) (move-count  $L) (stage  $Stage) (effects  $Effect $Captures) (in-hand  $Piece $Player) (opponent-promotes  $OldPiece $Sq)))  True)
; ; empty(_Square),
; ; effect(_Effect),
; ; captured(_Piece,_Sq), ; opponent_displaces,



  (= compile-dynamic-preds  
    (dynamic-preds-file $F)
    (compile-dynamics $F))



  (= (compile-dynamics $Dest)  
    ( (format "~nCompiling Dynamic Predicates to theory file: ~w~n" 
        (:: $Dest)) 
      (tell $Dest) 
      (dynamic-predicates $Preds) 
      (compile-preds $Preds) told))


  (= (compile-preds  ())  True)
  (= (compile-preds (Cons  $H $T))  
    (make-state $H)
    (compile-preds $T))


  (= (make-state $Pred)  
    (statify $Pred $Clause)
    (portray-clause $Clause))



  (= (statify $PredIn $Clause)  
    (= $Clause 
      (= $PredIn  
        (true $PredIn))))


