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



  !(my-ensure-loaded (library shells))
; 

  !(my-ensure-loaded (library menus))
; 




;
; 

;
; 

;
; 


  (= 
    (ask-move $Move $SIn $SOut) 
    ( (format "~nEnter a move or command (help  gives  more information)~n" Nil) 
      (read-keyboard-tokens $String) 
      (process-move-or-command $String $Move $SIn $S1) 
      (set-det) 
      (really-get-move $Move $SIn $S1 $SOut)))
; 

  (= 
    (ask-move $Move $SIn $SOut) 
    ( (control $Player $SIn) 
      (format "~n~p failed to select a legal move. Please try again!~n" 
        (:: $Player)) 
      (format "~n ('display.' to redisplay board)~n" Nil) 
      (ask-move $Move $SIn $SOut)))
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
    (really-get-move $Move $SIn $S1 $S1) 
    ( (nonvar $Move) 
      (nonvar $S1) 
      (set-det)))
; 

  (= 
    (really-get-move $Move $SIn $S1 $SOut) 
    ( (nonvar $S1) 
      (set-det) 
      (ask-move $Move $S1 $SOut)))
; 
	
  (= 
    (really-get-move $Move $SIn $S1 $SOut) 
    (ask-move $Move $SIn $SOut))
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
    (process-move-or-command $MoveString $Move $SIn $SOut) 
    (process-command $MoveString com 
      (:: $Move $SIn $SOut)))
; 

  (= 
    (process-move-or-command $MoveString $Move $SIn $SOut) 
    ( (nl) 
      (set-parsing-mode) 
      (move-notation $Move $MoveString) 
      (set-det) 
      (verbosely-format "~nTrying Move: ~p~n" 
        (:: $Move)) 
      (select-move $Move $SIn $SOut)))
; 

  (= 
    (process-move-or-command $MoveString $Move $SIn $SOut) 
    ( (format "Attempting to find a completion for move: " Nil) 
      (print-tokens $MoveString) 
      (nl) 
      (parameter completions on) 
      (choose-completed-move $MoveString $Move $SIn $SOut)))
; 




;
; 

;
; 


  (= 
    (select-move $Move $SIn $SOut) 
    ( (legal $Move $SIn $SOut) 
      (format "~nAfter:~n" Nil) 
      (print-state $SOut) 
      (print-notation $Move) 
      (ask-accept-choice $Answer) 
      (det-if-then-else 
        (= $Answer abort) 
        (, 
          (set-det) 
          (fail)) 
        (= $Answer yes))))
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
    (printing-choose $Method $Move $SIn $SOut) 
    ( (=.. $Chooser 
        (:: $Method $Move $SIn $SOut)) (selecting-choice (, (timing $Chooser) (format "~nAfter:~n" Nil) (print-state $SOut) (print-notation $Move)))))
; 


;
; 

;
; 

;
; 


  (= 
    (random-printing-choose $Method $Move $SIn $SOut) 
    ( (=.. $Chooser 
        (:: $Method $Move $SIn $SOut)) (selecting-choice (, (timing (random-success $Chooser)) (format "~nAfter:~n" Nil) (print-state $SOut) (print-notation $Move)))))
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
    (compatible $Op $Tokens) 
    ( (with-alpha-squares (, (move-notation $Op $FullToks) (mesh $Completion $Tokens $FullToks))) (verbosely-format "~nTokens: ~w~nCompletion: ~w~nFull: ~w~n" (:: $Tokens $Completion $FullToks))))
; 
 



  (= 
    (completed-move $Tokens $Move $SIn $SOut) 
    ( (legal $Move $SIn $SOut) 
      (compatible $Move $Tokens) 
      (acceptable $Move $SIn $SOut)))
; 



  (= 
    (acceptable $Move $SIn $SOut) 
    (det-if-then-else 
      (parameter safety on) 
      (check-safe-move $Move $SIn $SOut) True))
; 




  (= 
    (choose-completed-move $Tokens $Move $SIn $SOut) 
    ( (= $Chooser 
        (completed-move $Tokens $Move $SIn $SOut)) (selecting-choice (, (timing $Chooser) (format "~nAfter:~n" Nil) (print-choice $Move $SIn $SOut)))))
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
    (print-choice $Move $SIn $SOut) 
    ( (current-output $O) 
      (flush-output $O) 
      (tracing-play state 
        (print-state $SOut)) 
      (tracing-play move 
        (print-notation $Move)) 
      (linebreak)))
; 



  (= 
    (linebreak) 
    (format "\n===============================================================\n" Nil))
; 



  (= 
    (print-notation $Op) 
    ( (set-printing-mode) 
      (with-alpha-squares (move-notation $Op $Notate)) 
      (set-det) 
      (format "Notated Move played: ~n" Nil) 
      (print-tokens $Notate) 
      (nl) 
      (nl) 
      (verbosely-format "~n~nFull Move played: ~p~n" 
        (:: $Op))))
; 

  (= 
    (print-notation $Op) 
    (format "Move played: ~p~n" 
      (:: $Op)))
; 




  (= 
    (print-move $Move) 
    ( (set-printing-mode) 
      (with-alpha-squares (move-notation $Move $Notate)) 
      (set-det) 
      (print-tokens $Notate) 
      (nl)))
; 

  (= 
    (print-move $Move) 
    (format "Move: ~p~n" 
      (:: $Move)))
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
    (selecting-choice $Goal) 
    ( (call $Goal) 
      (ask-accept-choice $Answer) 
      (det-if-then-else 
        (= $Answer abort) 
        (, 
          (set-det) 
          (fail)) 
        (, 
          (= $Answer yes) 
          (set-det)))))
; 





  (= 
    (ask-accept-choice yes) 
    ( (parameter confirm-choices off) (set-det)))
; 

  (= 
    (ask-accept-choice $Answer) 
    ( (menu-command "~nAccept this choice? (yes, next, abort, help)~n" accept 
        (:: $Answer)) (set-det)))
; 

  (= 
    (ask-accept-choice $Answer) 
    ( (format "~n~p failed to answer appropriately. Please try again!~n" 
        (:: You)) (ask-accept-choice $Answer)))
; 


;
; 

;
; 

;
; 



  (= 
    (yes-accept yes) 
    (format "~nChoice accepted.~n" Nil))
; 


  (= 
    (y-accept yes) 
    (yes-accept yes))
; 



  (= 
    (next-accept next) 
    (format "~nTrying next choice.~n" Nil))
; 


  (= 
    (no-accept next) 
    (next-accept next))
; 


  (= 
    (n-accept next) 
    (next-accept next))
; 




  (= 
    (abort-accept abort) 
    (format "~nAttempt aborted!~n" Nil))
; 



  (= 
    (help-accept $Answer) 
    ( (help-accept) (ask-accept-choice $Answer)))
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
    (select-com $Move $SIn $SOut) 
    (select-move $Move $SIn $SOut))
; 



  (= 
    (display-com $Move $SIn $_) 
    ( (format "~nCurrent State:  ~n" Nil) (print-state $SIn)))
; 



  (= 
    (restart-com $_ $_ $_) 
    ( (format "~nRestarting ...~n" Nil) (metagame)))
; 



  (= 
    (quit-com $_ $_ $_) 
    (print-quit))
; 




  (= 
    (prolog-com $_ $_ $_) 
    (print-abort))
; 



  (= 
    (abort-com $_ $_ $_) 
    (print-abort))
; 



  (= 
    (verbose-com $_ $_ $_) 
    (set-verbose))
; 



  (= 
    (quiet-com $_ $_ $_) 
    (set-quiet))
; 




  (= 
    (break-com $_ $_ $_) 
    (format "~nBreak command not implemented." Nil))
; 



  (= 
    (set-com $_ $_ $_ $P $V) 
    (set-parameter $P $V))
; 


  (= 
    (set-com $_ $_ $_) 
    (show-parameters))
; 


;
; 



  (= 
    (setg-com $_ $_ $_ $P $V) 
    (add-global $P $V))
; 



  (= 
    (showg-com $_ $_ $_) 
    (showg))
; 




  (= 
    (randomize-com $_ $_ $_ $N) 
    (randomize $N))
; 



  (= 
    (pieces-com $_ $_ $_) 
    (show-piece-names))
; 



  (= 
    (show-piece-names) 
    ( (player-current-game $Game) 
      (game-piece-names $Game $Names) 
      (format "\nThe current game has the following pieces: \n~p\nNote that pieces are displayed on the board by the their first letter. \n" 
        (:: $Names))))
; 



  (= 
    (game-piece-names $Game $Names) 
    (setof $Name 
      (game-piece-name $Game $Name) $Names))
; 



  (= 
    (define-com $_ $_ $_ $PieceName) 
    (show-piece-definition $PieceName))
; 



  (= 
    (show-piece-definition $PieceName) 
    ( (player-current-game $G) 
      (game-piece-def $G $PieceName $Def) 
      (set-det) 
      (set-printing-mode) 
      (piece-def $Def $String Nil) 
      (set-parsing-mode) 
      (format "~nPiece <~p> is defined as follows: ~n~n" 
        (:: $PieceName)) 
      (print-tokens $String) 
      (nl) 
      (nl)))
; 

  (= 
    (show-piece-definition $PieceName) 
    (format "~nSorry, Piece <~p> is not defined in this game.~n~n" 
      (:: $PieceName)))
; 




  (= 
    (rules-com $_ $_ $_) 
    (show-rules))
; 



  (= 
    (board-com $_ $_ $_) 
    (show-board))
; 



  (= 
    (goals-com $_ $_ $_) 
    (show-game-goals))
; 





  (= 
    (cd-com $_ $_ $_ $Dir) 
    (cd-print $Dir))
; 




  (= 
    (pwd-com $_ $_ $_) 
    (pwd-print))
; 



  (= 
    (ls-com $_ $_ $_) 
    (ls))
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
    (access-com $Move $SIn $SOut) 
    ( (format "\nEnter a goal of the form:\n\tSIn^SOut^goal(...,SIn,...,SOut).\nCalling this goal will bind the current state to SOut,\nand return you to the move selection menu.\nTo accept the current state as your move, use the command: accept.\n\n" Nil) 
      (read (^ $SIn (^ $SOut $Goal))) 
      (call $Goal)))
; 


;
; 

;
; 


  (= 
    (call-com $_ $SIn $SOut) 
    ( (get-state-goal $Goal) (call-state-goal $Goal $SIn $SOut)))
; 



  (= 
    (get-state-goal $Goal) 
    ( (format "\nEntering a goal of the form:\n\tgoal(a,b,...)\nWill result in the following 'statified' goal being called:\n\tgoal(a,b,...,SIn,SOut)\nIf successful, this goal will bind the current state to SOut,\nand return you to the move selection menu.\nTo accept the current state as your move, use the command: accept.\n\n" Nil) (read $Goal)))
; 



  (= 
    (call-state-goal $Goal $SIn $SOut) 
    ( (=.. $Goal 
        (Cons  $H $Args)) 
      (append $Args 
        (:: $SIn $SOut) $NewArgs) 
      (=.. $SGoal 
        (Cons  $H $NewArgs)) 
      (det-if-then-else 
        (current-predicate $_ $SGoal) 
        (call $SGoal) 
        (, 
          (format "*** Unknown_goal: ~w~n" 
            (:: $SGoal)) 
          (fail)))))
; 




;
; 

;
; 


  (= 
    (restore-com $_ $SIn $SOut $Name) 
    (restore-state $Name $SOut))
; 

	
	
;
; 


  (= 
    (accept-com accept $SIn $SIn) 
    (format "Current state accepted as chosen move.~n" Nil))
; 


;
; 


  (= 
    (checkpoint-com $_ $SIn $_) 
    (checkpoint-state $SIn))
; 


;
; 

  (= 
    (checkpoint-com $_ $SIn $_ $Name) 
    (checkpoint-state $Name $SIn))
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
    (query-com $_ $SIn $_ goal) 
    (query-goal $SIn))
; 


  (= 
    (query-com $_ $SIn $_ material) 
    (query-material $SIn))
; 

  (= 
    (query-com $_ $SIn $_ mobility) 
    (query-mobility $SIn))
; 


  (= 
    (query-com $_ $SIn $_ goal $Player) 
    (query-goal $SIn $Player))
; 

  (= 
    (query-com $_ $SIn $_ material $Player) 
    (query-material $SIn $Player))
; 

  (= 
    (query-com $_ $SIn $_ mobility $Player) 
    (query-mobility $SIn $Player))
; 




  (= 
    (query-goal $SIn) 
    ( (format "Checking goals ...~n" Nil) (det-if-then-else (timing (game-outcome $O $SIn)) (format "Outcome is ~p~n" (:: $O)) (format "Game is not over yet.~n" Nil))))
; 



  (= 
    (query-goal $SIn $Player) 
    ( (format "Checking goals for <~p> ...~n" 
        (:: $Player)) (det-if-then-else (timing (goal-achieved $Player $SIn)) (format "<~p> has achieved a goal!~n" (:: $Player)) (format "<~p> has not achieved a goal.~n" (:: $Player)))))
; 


;
; 


  (= 
    (goal-achieved $P $S) 
    (goal-achieved $P $_ $S $_))
; 



  (= 
    (query-goal-slow-com $_ $SIn $_) 
    (query-goal-slow $SIn))
; 


  (= 
    (query-goal-slow $SIn) 
    ( (format "Checking goals ...~n" Nil) (det-if-then-else (timing (game-outcome player $SIn)) (format "<~p> has achieved a goal!~n" (:: player)) (format "Player has not won yet.~n" Nil))))
; 




  (= 
    (query-material $SIn) 
    ( (query-material $SIn player) (query-material $SIn opponent)))
; 


  (= 
    (query-material $S $Player) 
    ( (format "Counting material for <~p> ...~n" 
        (:: $Player)) 
      (timing (material $Player $S $Mat)) 
      (format "<~p> has material count of ~p~n" 
        (:: $Player $Mat))))
; 




  (= 
    (query-mobility $SIn) 
    ( (query-mobility $SIn player) (query-mobility $SIn opponent)))
; 


  (= 
    (query-mobility $S $Player) 
    ( (format "Counting mobility for <~p> ...~n" 
        (:: $Player)) 
      (timing (mobility $Player $S $Mat)) 
      (format "<~p> has mobility count of ~p~n" 
        (:: $Player $Mat))))
; 



;
; 

;
; 

;
; 



  (= 
    (trace-com $_ $_ $_ $Module) 
    (set-tracing $Module on))
; 

  (= 
    (trace-com $_ $_ $_ $Module $Component) 
    (set-tracing $Module $Component on))
; 



  (= 
    (untrace-com $_ $_ $_ $Module) 
    (set-tracing $Module off))
; 

  (= 
    (untrace-com $_ $_ $_ $Module $Component) 
    (set-tracing $Module $Component off))
; 



  (= 
    (list-tracing-com $_ $_ $_) 
    (list-tracing))
; 



  (= 
    (list-tracing) 
    ( (traced-modules $M) (format "The following modules are being traced: ~n~p~n" (:: $M))))
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
    (pass-com pass $SIn $SOut) 
    (pass-move $SIn $SOut))
; 




  (= 
    (random-com $Move $SIn $SOut) 
    (printing-choose random-move $Move $SIn $SOut))
; 




  (= 
    (random1-com $Move $SIn $SOut) 
    (random-printing-choose legal $Move $SIn $SOut))
; 




  (= 
    (instant-com $Move $SIn $SOut) 
    (printing-choose instant-move $Move $SIn $SOut))
; 




  (= 
    (victor-com $Move $SIn $SOut) 
    (printing-choose victor-move $Move $SIn $SOut))
; 




  (= 
    (endgame-com $Move $SIn $SOut) 
    ( (timing (endgame-move $Move $SIn $SOut)) (select-move $Move $SIn $SOut)))
; 




  (= 
    (cautious-com $Move $SIn $SOut) 
    (printing-choose cautious-move $Move $SIn $SOut))
; 



  (= 
    (random-aggressive-com $Move $SIn $SOut) 
    (printing-choose random-aggressive-move $Move $SIn $SOut))
; 




  (= 
    (mate-com $Move $SIn $SOut) 
    ( (timing (mate-move $Move $SIn $SOut)) (select-move $Move $SIn $SOut)))
; 



  (= 
    (threaten-com $Move $SIn $SOut) 
    ( (timing (threaten-move $Move $SIn $SOut)) (select-move $Move $SIn $SOut)))
; 



  (= 
    (enough-rope-com $Move $SIn $SOut) 
    ( (timing (enough-rope-move $Move $SIn $SOut)) (select-move $Move $SIn $SOut)))
; 




  (= 
    (help-clock) 
    (format "\n============================================================================\nGame Clock Routines\n-------------------\nclock adjust <color> <msec> : increments time player has used this game\nclock reset: resets time used by each player to 0.\nclock print: prints time used and remaining for each player.\nclock unlimit: disables the clock by giving both players unlimited time.\n\nclock: short form for clock print. \n\nExample:\n     clock adjust white 2000.     : adds 2 secs to time white used.\n     clock adjust black '-2500'.  : subtracts 2.5 secs from time black used.\n============================================================================\n" Nil))
; 



  (= 
    (clock-com $_ $_ $_ adjust $Color $Time) 
    ( (player-color $Player $Color) (adjust-player-clock $Player $Time)))
; 


  (= 
    (clock-com $_ $_ $_) 
    (print-clock))
; 

  (= 
    (clock-com $_ $_ $_ print) 
    (print-clock))
; 


  (= 
    (clock-com $_ $_ $_ reset) 
    (reset-clock))
; 


  (= 
    (clock-com $_ $_ $_ unlimit) 
    (clock-unlimit))
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



  !(my-ensure-loaded (library tracing))
; 



  (= 
    (tracing-play $Type $Call) 
    (det-if-then-else 
      (tracing (play $Type)) 
      (call $Call) True))
; 


;
; 


  (= 
    (tracing-play-format $Type $String $Args) 
    (det-if-then-else 
      (tracing (play $Type)) 
      (format $String $Args) True))
; 



  (= 
    (tracing-play-timing $Type $Call) 
    (trace-timing 
      (play $Type) $Call))
; 



  (= 
    (set-play-verbosity $Level $Status) 
    (set-tracing 
      (play $Level) $Status))
; 



  (= 
    (silent-play) 
    (all-play off))
; 


  (= 
    (loud-play) 
    (all-play on))
; 



  (= 
    (all-play $Status) 
    ( (set-play-verbosity state $Status) 
      (set-play-verbosity move $Status) 
      (set-play-verbosity clock $Status)))
; 
 


  (= 
    (trace-play-state) 
    (set-play-verbosity state on))
; 
 

  (= 
    (trace-play-move) 
    (set-play-verbosity move on))
; 
 

  (= 
    (trace-play-clock) 
    (set-play-verbosity clock on))
; 
 


  !(loud-play *)
; 

;
; 




