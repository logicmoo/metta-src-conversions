;                                           (convert_to_metta_file  controller $_422396 metagame/play/controller.pl metagame/play/controller.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; controller.pl


;
; ================================================================================

;
; ========================================
;
; GET_CURRENT_GAME
;
; ========================================
;
; External hook, which should (at least)
;
; ensure that the internal representation of the current game
;
; to be played is saved in the predicates:
;
; player_current_game/1
;
; opponent_current_game/1
;
; This can be achieved by calling the predicate:
;
; file_make_test_game/1
;
; with a file in which a game is stored.

;
; ========================================
;
; GET_PLAYERS(-White,-Black)
;
; ========================================
;
; An external hook, which returns names attached
;
; to the two players.

;
; ========================================
;
; GET_RANDOM_ASSIGNMENT(-Assignments)
;
; ========================================
;
; External hook to determine random assignments
;
; when necessary.

;
; ================================================================================
;
; CHOOSE(+Name,+Role,+SIn,-SOut)
;
; ================================================================================
;
; Contains a clause for each named decision method (or player),
;
; when playing in a particular role (player or opponent),
;
; to update the present state by selecting a move.

;
; ================================================================================
;
; SHOULD_CONTINUE(+State)
;
; ================================================================================
;
; External hook to determine whether to continue
;
; controlling the game.
;
; Should return true if the player wants to continue.

;
; ================================================================================
;
; TERMINATE_GAME(+FinalState)
;
; ================================================================================
;
; Hook to call to determine what to do when the game has ended.

;
; ================================================================================


;
; GET_IF_RANDOM_ASSIGNMENT
;
; If the game requires a random setup,
;
; then call the HOOK: get_random_assignment(A)
;
; to get an assignment, and record it.

  (= (get-if-random-assignment)  
    (current-random-setup-game)
    (set-det)
    (get-random-assignment $Assignment)
    (set-random-assignment $Assignment))
  (= get_if_random_assignment  True)

;
; Used by file parse.pl

  (= (set-random-assignment $Assignment)  
    ( (remove-all-atoms  &self 
        (random_assignment  $_)) (add-is-symbol  &self (random_assignment  $Assignment))))


;
; ==================
;
; START_CONTROLLER
;
; ==================
;
; Decide what game will be played, and who will play each color.
;
; Then start the controller with those players played their colors.
;
; This is top level for playing a particular contest
;
; of a particular game.


  (= (start)  
    (start-controller))


  (= (start-top $PosName)  
    (checkpoint $PosName $SIn)
    (set-current-pos-name $PosName)
    (start-controller $SIn))




  (= (start-controller)  
    (get-current-game)
    (get-initial-state $SIn)
    (initialize-history $SIn)
    (start-controller $SIn))  


  (= (get-initial-state $SIn)  
    (get-if-random-assignment)
    (start-game $SIn))
	

;
; ==================================
;
; START_CONTROLLER(SIn)
;
; ==================================
;
; Resets the clock at the start of each game.
;
; Starts the game (initialize board, etc),
;
; then has the players play it in the main control loop.
;
; Finds out which game it is playing.
;
; Gets a random assignment if necessary for that game.

  (= (start-controller $SIn)  
    (format "~nInitial Position:~n" Nil)
    (print-state $SIn)
    (linebreak)
    (reset-clock)
    (controller $SIn $_)
    (set-det))
; ; initialize_checkpoints(SIn),
; ; new



  (= (start-game $Init)  
    (new-state $State)
    (start-game $State $Init))


;
; CONTROLLER(SIn,SOut)
;
; Either the game ends, or we have the current player
;
; make a move, and then continue.
;
; 
;
; The clock is only printed if tracing(play(clock)).
;
; To (un)set this, use: '(un)trace play clock'.

  (= (controller $SIn $SIn)  
    (game-ends-in-outcome $SIn $Outcome)
    (set-det)
    (record-game-outcome $Outcome)
    (checkpoint-state final $SIn))
  (= (controller $SIn $SOut)  
    (tracing-play clock print-clock)
    (play-in-control $SIn $S1)
    (should-continue $S1)
    (set-det)
    (cleanup-state $S1 $S2)
    (controller $S2 $SOut))
; ; new hook
  (= (controller $SIn $SIn)  
    (format "The game has been halted prematurely!!" Nil))



;
; PLAY_IN_CONTROL(SIn,SOut)
;
; Initialize parameters for the player to move,
;
; and call the appropriate choice method.
;
; 

  (= (play-in-control $SIn $SOut)  
    (control $Role $SIn)
    (initialize-player-move $Role)
    (role-chooser $Role $Chooser)
    (choose-or-resign $Chooser $Role $SIn $SOut))
; ;

;
; CHOOSE_OR_RESIGN(Chooser,Role,SIn,-SOut)
;
; 
;
; Possibly add resignation as a kind of move later.
;
; For now, failing to choose a legal move is resignation.
;
; Now uses real-time instead of runtime. This means human
;
; players get timed correctly also!

  (= (choose-or-resign $Chooser $Role $SIn $SOut)  
    (realtime-success 
      (choose $Chooser $Role $Move $SIn $SOut) $Time)
    (set-det)
    (adjust-player-clock $Role $Time)
    (add-state-to-history $Move $SOut)
    (restore-parameters))
; ; realtime_success(choose(Chooser,Role,SIn,SOut),Time), !,
  (= (choose-or-resign $Chooser $Role $SIn $_)  
    (opposite-role $Role $OppRole)
    (role-chooser $OppRole $OppChooser)
    (format "~n~p, as ~p, failed to select a legal move.~nThus, ~p, as ~p, is declared the Winner!~n" 
      (:: $Chooser $Role $OppChooser $OppRole))
    (checkpoint-state final $SIn)
    (restore-parameters)
    (fail))
	

;
; ================================================================================
;
; SHOULD_CONTINUE
;
; ================================================================================
;
; A hook to controller.
;
; Should return true if the player wants to continue the game.
;
; Querying is disabled when parameter CONTINUOUS = yes.


  (= (continuous)  
    (parameter continuous yes))


  (= (set-continuous)  
    (set-parameter continuous yes))

  (= (set-stepping)  
    (set-parameter continuous no))


  (= (should-continue $SIn)  
    (continuous)
    (set-det))
  (= (should-continue $SIn)  
    (ask-continue y))


  (= (ask-continue $Answer)  
    (ask-ynp Continue $Answer1)
    (set-det)
    (= $Answer1 $Answer))


  (= (ask-ynp $Query $Answer)  
    (format "~a? (y or n)~n" 
      (:: $Query))
    (read $Answer1)
    (ynp $Answer1 $Answer 
      (ask-ynp $Query $Answer)))

;
; =============================================================================
;
; PRE-MOVE INITIALIZATION
;
; -----------------------
;
; 
;
; role_chooser(Role,Chooser):
;
; Moves for player role Role (player, opponent)
;
; are chosen by choice method Chooser. This is set by the
;
; "player <color> chooser" command from the interface.
;
; 
;
; role_file(Role,File):
;
; File contains info about pre-move initiliazations for Role.
;
; File is either <none>, a filename to be loaded, or a list of
;
; parameters. This list can be modified using the command template:
;
; "set <color> <parameter> <value>".
;
; 

;
; INITIALIZE_PLAYER_MOVE(Role)
;
; Either load the initialization file for Player,
;
; or override to the player-specific parameters.
;
; If there is no info, do nothing.
;
; Before changing parameters, the old version is saved,
;
; and will be restored at the end of this move.
;
; This ensures that no player can modify the parameters
;
; for the opponent.

  (= (initialize-player-move $Role)  
    (save-parameters)
    (role-file $Role $File)
    (det-if-then-else 
      (member $File 
        (:: none Nil)) True 
      (load-player-eval $File)))

;
; Assert a list of parameters only if that's what is defined for
;
; player_file.

  (= (load-player-eval (Cons  $P $Ps))  
    (set-det)
    (restore-parameters (Cons  $P $Ps)))
  (= (load-player-eval $Name)  
    (load-player-eval-file $Name))
	


  (= (set-com $_ $_ $_ $C $P $V)  
    (set-color-parameter $C $P $V))


  (= (set-top $C $P $V)  
    (set-color-parameter $C $P $V))


  (= (unset-top $C)  
    (color-player $C $Role)
    (clear-player-parameters $Role))


  (= (set-color-parameter $Color $P $V)  
    (color-player $Color $Role)
    (set-player-parameter $Role $P $V))


  (= (set-player-parameter $Role $Param $Val)  
    (role-file $Role $List1)
    (set-assoc $List1 $Param $Val $New)
    (set-role-file $Role $New))
	

  (= (clear-player-parameters $Role)  
    (set-role-file $Role none))




  (= (load-player-eval-file $Name)  
    (find-eval-file $Name $File)
    (save-parameters)
    (compile $File))
	
;
; -------------------------------------------------------------------------

  (= (role-chooser $Role $Chooser)  
    (player-method-parameter $Role $Param)
    (parameter $Param $Chooser))


  (= (player_method_parameter  player player_method)  True)
  (= (player_method_parameter  opponent opponent_method)  True)


  (= (role-file $Role $File)  
    (player-file-parameter $Role $Param)
    (parameter $Param $File))


  (= (set-role-file $Role $File)  
    (player-file-parameter $Role $Param)
    (set-parameter $Param $File))



  (= (player_file_parameter  player player_file)  True)
  (= (player_file_parameter  opponent opponent_file)  True)

;
; Human mode is operative when one of the players is a human
;
; chooser. This hook determines when questions and confirmations
;
; should be made from the local console. Otherwise the controller
;
; assumes it has the correct information to begin with.

  (= (human-mode)  
    (det-if-then 
      (role-chooser $Role human) True)) 

;
; ============================================================================
;
; Game Clock Routines
;
; -------------------
;
; adjust_player_clock: increments time player has used this game
;
; reset_clock: resets time used by each player to 0.
;
; print_clock: prints time used and remaining for each player.
;
; game_time_left: returns time left for a player in this game.
;
; time_out_outcome: returns game outcome if timeout for at least one player.
;
; ============================================================================

;
; ADJUST_PLAYER_CLOCK(+Player,+Time)
;
; Adds Time units to players total elapsed time this game.
;
; This makes available the predicate:
;
; 
;
; time_used(?Player,?T) : Player has used T units of time this game.
;
; 

  (= (adjust-player-clock $Player $Time)  
    ( (remove-is-symbol  &self 
        (time_used  $Player $TOld)) 
      (is $TNew 
        (+ $TOld $Time)) 
      (add-is-symbol  &self 
        (time_used  $Player $TNew))))

;
; RESET_CLOCK
;
; Sets both players' elapsed times to 0 (to be used at the start of each game).

  (= (reset-clock)  
    (reset-player-clock player)
    (reset-player-clock opponent))


  (= (reset-player-clock $Player)  
    ( (remove-all-atoms  &self 
        (time_used  $Player $_)) (add-is-symbol  &self (time_used  $Player 0))))


;
; PRINT_CLOCK
;
; Prints time used and left for each player.

  (= (print-clock)  
    (format "~*c~n" 
      (:: 40 45))
    (format "Clock times (in seconds):~n" Nil)
    (print-player-clock player)
    (print-player-clock opponent)
    (format "~*c~n" 
      (:: 40 45)))


  (= (print-player-clock $Player)  
    (time-used $Player $Used)
    (game-time-left $Player $Left)
    (format "<~p>: \t~3d used, \t~3d left~n" 
      (:: $Player $Used $Left)))
	
;
; TIME_OUT_OUTCOME(?Outcome)
;
; Outcome is DRAW if both players out of time, the player
;
; who still has time left if only one is out, and fails
;
; if both still have time.

  (= (time-out-outcome $Outcome)  
    (player-time-out player $PTime)
    (player-time-out opponent $OTime)
    (time-out-outcome $PTime $OTime $Outcome))

  (= (time-out-outcome yes yes draw)  
    (set-det)
    (format "Both players are out of time!~n" Nil))
  (= (time-out-outcome no yes player)  
    (set-det)
    (format "<~p> is out of time~n" 
      (:: opponent)))
  (= (time-out-outcome yes no opponent)  
    (set-det)
    (format "<~p> is out of time~n" 
      (:: player)))


;
; PLAYER_TIME_OUT(Player,Out)
;
; 
;
; A player is out of time if he has no time remaining.

  (= (player-time-out $Player $Out)  
    (game-time-left $Player $Time)
    (det-if-then-else 
      (=< $Time 0) 
      (= $Out yes) 
      (= $Out no)))



;
; GAME_TIME_LEFT(?Player,-Time)
;
; Time is the amount of time Player has left in the current game.

  (= (game-time-left $Player $Time)  
    (game-time-limit $Limit)
    (time-used $Player $Used)
    (is $Time 
      (max 
        (- $Limit $Used) 0)))


  (= (game-time-limit $Limit)  
    (parameter game-time-limit $Limit))
	
;
; Nullify time limits for both players.

  (= (clock-unlimit)  
    (set-parameter game-time-limit 99999999)
    (set-parameter move-time-limit 99999999)
    (set-parameter move-horizon 1)
    (reset-clock))


;
; =============================================================================


  (= (game-ends-in-outcome $SIn $Outcome)  
    (game-over $SIn)
    (set-det)
    (game-outcome $Outcome $SIn))
  (= (game-ends-in-outcome $SIn $Outcome)  
    (time-out-outcome $Outcome))

;
; game_over and game_outcome gets compiled to have 2 args, as it
;
; thinks the check for no legal moves actually might affect state.


  (= (game-over $SIn)  
    (game-over $SIn $_))


  (= (game-outcome $O $S)  
    (game-outcome $O $S $_))




  !(dynamic (/ recorded-game-outcome 1))
;
; ========================================
;
; RECORDED_GAME_OUTCOME(?Outcome)
;
; ========================================
;
; The last game ended in Outcome (player, opponent, or draw)
;
; 


;
; RECORD_GAME_OUTCOME(O)
;
; Prints the outcome of the game.
;
; Makes available the predicate:
;
; recorded_game_outcome/1.

  (= (record-game-outcome $O)  
    ( (remove-all-atoms  &self 
        (recorded_game_outcome  $O)) 
      (add-is-symbol  &self 
        (recorded_game_outcome  $O)) 
      (write-outcome $O)))



  (= (write-outcome draw)  
    (format "~n~nThe game is over. Ends in a draw!~n" Nil)
    (set-det))
  (= (write-outcome $Player)  
    (format "~n~nThe game is over.  ~p wins!~n" 
      (:: $Player)))

;
; cleanup_state(Old,New)
;
; This provides a hook for different state representations
;
; to purge temporary data from their state, if necessary.

  (= (cleanup_state  $S $S)  True)


