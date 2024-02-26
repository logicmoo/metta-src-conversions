;                                           (convert_to_metta_file  local $_435814 metagame/play/local.pl metagame/play/local.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;; local.pl
;
; ;; Control hooks for using the console to play two humans or programs against
;
; ;; each other locally (i.e. without using internet communication).
;
; ;;
;
; ;; This file contains the parts of the interface which are specific to this
;
; ;; mode of play only. Thus, for a local match, we find the players and the game
;
; ;; by querying the user, whereas in a remote match, each player receives this
;
; ;; info from the referee, and the referee sends it.


;
; ========================================
;
; GET_PLAYERS(White,Black)
;
; ========================================

;
; If already have a game to play, use it.
;
; Else query user.


  (= (get-players $White $Black)  
    (get-player-color player $White)
    (get-player-color opponent $Black)
    (query-accept-players $White $Black)
    (set-det))
  (= (get-players $_ $_)  
    (format "\nPlease use 'player' to select the players.\n'help player' gives more info, and a list of available players.\n" Nil)
    (fail))



  (= (get-player-color $Color $Player)  
    (player-method-parameter $Color $Param)
    (parameter $Param $Player))



  (= (query-accept-players $White $Black)  
    (human-mode)
    (set-det)
    (format "\n~p as ~p vs. ~p as ~p?\n(y to accept, n to select new players): " 
      (:: $White player $Black opponent))
    (read $Answer)
    (= $Answer y))
  (= (query-accept-players $White $Black)  
    (format "Playing ~p as ~p vs. ~p as ~p?" 
      (:: $White player $Black opponent)))


;
; ========================================
;
; GET_CURRENT_GAME
;
; ========================================
;
; Provides external hook, storing the internal representation
;
; of the current game (after choosing it)
;
; in the predicates:
;
; player_current_game/1
;
; opponent_current_game/1

;
; If already have a game to play, use it.
;
; Else query user.

  (= (get-current-game)  
    (player-current-game $Game)
    (query-use-current-game $Game)
    (set-det))
  (= (get-current-game)  
    (format "\nPlease use 'game' to select a game.\n'games_library' will show you a list of available games.\n'generate <file> will generate a new game as <file>.game.\n" Nil)
    (fail))

;
; File saved as [path/]GameName.game

  (= (query-use-current-game $Game)  
    (human-mode)
    (set-det)
    (game-name $Game $Name)
    (format "~nPlay game ~w? (y to play, n to choose another): " 
      (:: $Name))
    (read $Answer)
    (= $Answer y))
  (= (query-use-current-game $Game)  
    (game-name $Game $Name)
    (format "~nPlaying game ~w" 
      (:: $Name)))


;
; ========================================
;
; GET_RANDOM_ASSIGNMENT(Assignments)
;
; ========================================
;
; By default, generates a random assignment.
;
; If assignment_method(ask), then queries user.


  (= (get-random-assignment $Assignment)  
    (game-assignments $Game $As)
    (assignment-decision $As random $PieceNames $Squares)
    (produce-assignment $PieceNames $Squares $Assignment))



  (= (produce-assignment $PieceNames $Squares $Assignment)  
    (parameter assignment-method ask)
    (set-det)
    (ask-random-assignment $PieceNames $Squares $Assignment))
  (= (produce-assignment $PieceNames $Squares $Assignment)  
    (generate-random-assignment $PieceNames $Squares $Assignment))
	


  (= (generate-random-assignment $PieceNames $Squares $Assignment)  
    (format "~nGenerating Random Assignment~n" Nil)
    (assign-pieces-to-squares $PieceNames $Squares $Assignment))  ;
; defined in generator

;
; The ['.'|Rest] is because the assignment_defs doesn't end in a period,
;
; while the end of string requires it.
;
; 

  (= (ask-random-assignment $PieceNames $Squares $Assignment)  
    (format "~nPlease assign ~w to ~w~nExample syntax: piece1 at {(1,1)} piece2 at {(2,2) (3,3)}.~n(C-d on new line to end)~n|: " 
      (:: $PieceNames $Squares))
    (read-tokens $AssignmentString)
    (nl)
    (set-parsing-mode)
    (assignments $Assignment $AssignmentString 
      (Cons  . $Rest)))

  (= (ask-random-assignment $PieceNames $Squares $Assignment)  
    (format "~nPlease assign ~w to ~w~n('.' for help)~n|: " 
      (:: $PieceNames $Squares))
    (read-keyboard-tokens $AssignmentString)
    (help-or-process-assign $AssignmentString $PieceNames $Squares $Assignment))


  (= (help-or-process-assign (:: .) $PieceNames $Squares $Assignment)  
    (format "~nExample syntax:~npiece1 at {(1,1)} piece2 at {(2,2) (3,3)}.~n" Nil)
    (ask-random-assignment $PieceNames $Squares $Assignment))

  (= (help-or-process-assign $AssignmentString $_ $_ $Assignment)  
    (nl)
    (set-parsing-mode)
    (assignments $Assignment $AssignmentString 
      (Cons  . $Rest)))

      

  (= (choose $Name $Role $Move $SIn $SOut)  
    (concat $Name -choose $NameChoose)
    (=.. $Goal 
      (:: $NameChoose $Role $Move $SIn $SOut))
    (call $Goal))
; /* ;================================================================================ ; TERMINATE_GAME(SIn) ;================================================================================ ; Saves final state for debugging purposes. ; May not want to print checkpoint info every time. terminate_game(SIn) :- ask_ynp('Would you like to play another game',Answer), !, play_again_if(Answer). play_again_if(y) :- !, metagame. play_again_if(_). */
; ;================================================================================ ; Move Selection methods ;================================================================================
; ; CHOOSE(Chooser,Role,Move,SIn,SOut) ; CHOOSER is an atom. ; Calls the procedure CHOOSER_choose(Role,Move,SIn,SOut). ; ; Currently define choice methods are: ; HUMAN ; INSTANT ; RANDOM ; THREATEN ; CAUTIOUS ; QUICK_CAUTIOUS ; VARIABLE ; SMARTER ; RANDOM_AGGRESSIVE



  (= (variable-choose $Role $Move $SIn $SOut)  
    (player-method-parameter $Role $Param)
    (parameter $Param $Chooser)
    (choose $Chooser $Role $Move $SIn $SOut))


