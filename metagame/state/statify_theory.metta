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


  !(my-ensure-loaded (library aux))
; 

  !(my-ensure-loaded (library stat))
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
    (compile-and-load-player) 
    (state-compile-player))
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
    (state-compile-player) 
    ( (compile-dynamic-preds) 
      (index-dynamic-preds) 
      (player-theory-load) 
      (player-theory-compile) 
      (theory-clear)))
; 



  (= 
    (player-theory-load) 
    ( (format "Loading Theory: MetaGame~n" Nil) 
      (theory-clear) 
      (player-theory-files $Files) 
      (dl $Files)))
; 



  (= 
    (player-theory-compile) 
    ( (format "Compiling Theory: MetaGame~n" Nil) 
      (stativity-analysis) 
      (compile-player-files)))
; 



  (= 
    (compile-player-files) 
    ( (theory-files $Files) (whenever (member $File $Files) (thcomp $File))))
; 



  (= 
    (player-compiled-files $CompFiles) 
    ( (theory-files $Files) (bagof $CompFile (^ $File (, (member $File $Files) (theory-statname $File $CompFile))) $CompFiles)))
; 
  


  (= 
    (player-theory-files $CompFiles) 
    ( (theory-files $Files) (bagof $CompFile (^ $File (, (member $File $Files) (theory-filename $File $CompFile))) $CompFiles)))
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
    (load-compiled-play-theory) 
    ( (player-compiled-files $Fs) (whenever (member $F $Fs) (compile $F))))
; 




  (= 
    (theory-filename $FileRoot $CompFileName) 
    ( (sys-suffixed-filename $FileRoot prolog $File) 
      (theory-directory $TheoryDir) 
      (concat-list 
        (:: $TheoryDir / $File) $CompFileName)))
; 



  (= 
    (theory-statname $FileRoot $CompFileName) 
    ( (sys-suffixed-filename $FileRoot state-compile $File) 
      (theory-directory $TheoryDir) 
      (concat-list 
        (:: $TheoryDir / $File) $CompFileName)))
; 




  (= 
    (thcomp $FileRoot) 
    ( (theory-filename $FileRoot $FileIn) 
      (theory-statname $FileRoot $FileOut) 
      (state-compile-file $FileIn $FileOut) 
      (compile $FileOut)))
; 



  (= 
    (state-compile-file $FileIn $FileOut) 
    ( (format "Compiling theory file: ~w~n" 
        (:: $FileIn)) 
      (see $FileIn) 
      (tell $FileOut) 
      (compile-clauses) 
      (set-det) 
      (seen) 
      (told) 
      (format "Wrote compiled file: ~w~n" 
        (:: $FileOut))))
; 



  (= 
    (compile-clauses) 
    (det-if-then-else 
      (, 
        (read $ClauseIn) 
        (\== $ClauseIn end-of-file)) 
      (, 
        (compile-clause $ClauseIn $ClauseOut) 
        (portray-clause $ClauseOut) 
        (compile-clauses)) True))
; 




  (= 
    (compile-clause $CIn $COut) 
    ( (clause-parts $CIn $HIn $BIn) (thread-clause $HIn $BIn $COut)))
; 



  (= 
    (thread-clause $HeadIn $BodyIn $ClauseOut) 
    ( (add-state $HeadIn $SIn $SOut $HeadOut) 
      (thread $BodyIn $SIn $SOut $BodyOut) 
      (clause-parts $ClauseOut $HeadOut $BodyOut)))
; 




  (= 
    (add-state $GoalIn $Stativity $GoalOut) 
    (add-state $GoalIn $SIn $SOut $Stativity $GoalOut))
; 


  (= 
    (add-state $GoalIn $SIn $SOut $GoalOut) 
    ( (stat $GoalIn $Stat) (add-state $GoalIn $SIn $SOut $Stat $GoalOut)))
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
    (add_state  $GoalIn $SIn $SIn 0 $GoalIn) True)
; 

  (= 
    (add-state $GoalIn $SIn $SIn 1 $GoalOut) 
    (thread-in-state $GoalIn $SIn $GoalOut))
; 

  (= 
    (add-state $GoalIn $SIn $SOut 2 $GoalOut) 
    ( (thread-in-state $GoalIn $SIn $G1) (thread-in-state $G1 $SOut $GoalOut)))
; 



  (= 
    (thread-in-state $GoalIn $SIn $GoalOut) 
    (increase-term-arity $GoalIn $SIn $GoalOut))
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
    (dynamic_predicates  
      ( (control  $Player) 
        (on  $Piece $Square) 
        (moved_onto  $Piece $Sq) 
        (move_count  $L) 
        (stage  $Stage) 
        (effects  $Effect $Captures) 
        (in_hand  $Piece $Player) 
        (opponent_promotes  $OldPiece $Sq))) True)
; 




  (= 
    (compile-dynamic-preds) 
    ( (dynamic-preds-file $F) (compile-dynamics $F)))
; 




  (= 
    (compile-dynamics $Dest) 
    ( (format "~nCompiling Dynamic Predicates to theory file: ~w~n" 
        (:: $Dest)) 
      (tell $Dest) 
      (dynamic-predicates $Preds) 
      (compile-preds $Preds) 
      (told)))
; 



  (= 
    (compile_preds  ()) True)
; 

  (= 
    (compile-preds (Cons  $H $T)) 
    ( (make-state $H) (compile-preds $T)))
; 



  (= 
    (make-state $Pred) 
    ( (statify $Pred $Clause) (portray-clause $Clause)))
; 




  (= 
    (statify $PredIn $Clause) 
    (= $Clause 
      (= $PredIn 
        (true $PredIn))))
; 



