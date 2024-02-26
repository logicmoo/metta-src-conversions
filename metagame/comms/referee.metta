;                                           (convert_to_metta_file  referee $_103860 metagame/comms/referee.pl metagame/comms/referee.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;; referee.pl
;
; ;;
	

  !(prolog-flag redefine-warnings $_ off)

;
; ================================================================================
;
; REFEREE
;
; ================================================================================
;
; 
;
; REFEREE MeTTa process will be called with command like:
;
; start_sicstus_shell(any,
;
; [server_name, ServerAddr,
;
; server_port, Port,
;
; player1_name, Player1,
;
; player2_name, Player2,
;
; file,'~/MeTTa/play/referee']).

;
; Redefine metagame here, so we don't give menu and instead just start referee.

  (= (metagame)  
    (start-ref))


  (= (start-ref)  
    (find-server $Server $Port)
    (find-players $Player1 $Player2)
    (referee $Server $Port $Player1 $Player2))


  (= (find-server $S $P)  
    (parameter server-name $S)
    (parameter server-port $P))



  (= (find-players $Player1 $Player2)  
    (parameter player1-name $Player1)
    (parameter player2-name $Player2))


	

  (= (referee $ServerAddr $Port $Player1 $Player2)  
    ( (use-module (library linda/client)) 
      (port-number $Port $PortNum) 
      (linda-client-wait (with_self  $ServerAddr $PortNum)) 
      (greet-players $Player1 $Player2) 
      (format "~nI'm your referee!~n" Nil) 
      (add-is-symbol  &self 
        (players  $Player1 $Player2)) 
      (start-controller)))


  (= (greet-players $P1 $P2)  
    (format "~nHello, player1: ~w, player2: ~w~n" 
      (:: $P1 $P2)))


;
; ================================================================================
	

;
; Play a number of contests between these
;
; players, for a given game definition.


  (= (referee-game-contests $P1 $P2 $G 0)  
    (set-det)
    (record-statistics))
  (= (referee-game-contests $P1 $P2 $G $Contests)  
    (> $Contests 0)
    (referee-contest $P1 $P2 $G)
    (is $C1 
      (- $Contests 1))
    (referee-game-contests $P1 $P2 $G $C1))
	

;
; For now, shut off everything. Eventually, can play
;
; role in more complicated tournament.


  (= (close-match $P1 $P2)  
    (close-players $P1 $P2)
    (close-server)
    (record-statistics)
    (close-client))
	

  (= (close-server)  
    (with_self  
      (linda *) 
      (linda-call halt)))


;
; ===========================================================================
;
; CALL_FOR_PLAYERS(Player^Call)
;
; ===========================================================================
;
; Call is a goal, possibly requiring a variable Player.
;
; The goal will be called, instantiated in turn for both players.


  (= (call-for-players $PlayerCall)  
    (players $Player1 $Player2)
    (call-for-players $Player1 $Player2 $PlayerCall))

  (= (call-for-players $Player1 $Player2 $PlayerCall)  
    (call-for-player $Player1 $PlayerCall)
    (call-for-player $Player2 $PlayerCall))


  (= (call-for-player $Name (^ $Player $Call))  
    (verify (, (= $Player $Name) (call $Call))))

;
; OUT_TO_PLAYERS(Pattern)
;
; Send the same message to both players, personally addressed.

  (= (out-to-players $Pattern)  
    (call-for-players (^ $P (out-personal $P $Pattern))))


;
; Sends a personal message, of the form:
;
; message(Player,Pattern)

  (= (out-personal $Player $Pattern)  
    (out1 (message $Player $Pattern)))


  (= (out1 $Pattern)  
    (format "~nSending pattern: ~w~n" 
      (:: $Pattern))
    (out $Pattern))


;
; ================================================================================
;
; Hooks to standard controller script
;
; ================================================================================

;
; ========================================
;
; GET_PLAYERS(White,Black)
;
; ========================================

  (= (get-players $White $Black)  
    (next-players $White $Black)
    (set-players $White $Black))


;
; NEXT_PLAYERS(White,Black)
;
; Could be determined from an external source, like a tournament
;
; director, based on the current results of the tournament.
;
; For now, just alternate colors each game.
;
; players(White,Black) is initially saved upon entry above.

  (= (next-players $White $Black)  
    ( (remove-is-symbol  &self 
        (players  $Black $White)) (add-is-symbol  &self (players  $White $Black))))
	

;
; Sends to each player the information about the roles
;
; both players are playing.

  (= (set-players $White $Black)  
    (out-to-players (players $White $Black)))


;
; ========================================
;
; GET_CURRENT_GAME
;
; ========================================

;
; Provides external hook.
;
; Selects next game, and loads it as the current game.
;
; Then send the name of the game to the players.
;
; We assume the players have already been sent the full
;
; definitions of the games with these names.
;
; Probably this could be done over email.


  (= (get-current-game)  
    (select-next-game $GameName)
    (load-game $GameName)
    (send-game-name-to-players $GameName))


  (= (send-game-name-to-players $GameName)  
    (out-to-players (game-name $GameName)))

;
; SELECT_NEXT_GAME(GameName).
;
; Could be determined from an external source, like a tournament
;
; director, based on the current results of the tournament.
;
; Could also be determined from a file.
;
; For the time being, to demonstrate the functionality,
;
; we'll have them play chess, checkers, shogi, and turncoat_chess,
;
; and then tell them the match is over.


  (= (select-next-game $GameName)  
    (next-alternate-game $GameName)
    (set-det))
  (= (select_next_game  chess)  True)


  (= (next-alternate-game $GameName)  
    (player-current-game-name $OldGame)
    (set-det)
    (game-follows $OldGame $GameName))
	

  (= (player-current-game-name $Name)  
    (player-current-game $G)
    (game-name $G $Name))


  (= (game_follows  chess checkers)  True)
  (= (game_follows  checkers shogi)  True)
  (= (game_follows  shogi turncoat_chess)  True)


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
; Here, the ref. generates a random assignment,
;
; and sends the string representation to each of the players.

  (= (get-random-assignment $Assignment)  
    (game-assignments $Game $As)
    (assignment-decision $As random $PieceNames $Squares)
    (generate-random-assignment $PieceNames $Squares $Assignment)
    (send-assignment $Assignment))


;
; assign_pieces_to_squares is defined in generator

  (= (generate-random-assignment $PieceNames $Squares $Assignment)  
    (format "~nGenerating Random Assignment~n" Nil)
    (assign-pieces-to-squares $PieceNames $Squares $Assignment)) 


;
; Ex:
;
; ?- send_assignment([piece1=[square(2,2)],piece2=[square(1,1)]]).
;
; piece1 at { ( 2 , 2 ) }
;
; piece2 at { ( 1 , 1 ) } .
;
; Uses command call_for_players, to send copy to each player.
;
; Send by name?
;
; assignments_string/2 defined in game grammar file grammar.pl

  (= (send-assignment $Assignment)  
    (assignments-string $Assignment $AssignmentString)
    (format "~nSending random assignment: ~w~n" 
      (:: $Assignment))
    (out-to-players (init-state $AssignmentString)))
	


;
; ================================================================================
;
; TERMINATE_GAME(+FinalState)
;
; ================================================================================
;
; Hook called when the game has ended.


  (= (terminate-game $SIn)  
    (format "~nThe game has finished~n" Nil)
    (process-results $SIn)
    (restart-or-end))

;
; How results are processed to be defined later.

  (= (process_results  $SIn)  True)


  (= (restart-or-end)  
    (det-if-then-else 
      (next-alternate-game $GameName) restart goodbye-players))
	

  (= (restart)  
    (out-to-players (reset new))
    (start-controller))


  (= (goodbye-players)  
    (out-to-players (reset end))
    (format "~nTournament is finished. Bye!~n" Nil))



;
; ================================================================================
;
; SHOULD_CONTINUE(SIn)
;
; ================================================================================
;
; Another hook to controller.
;
; Should return true if the player wants to continue the game.



  (= (should-continue $SIn)  
    (continuous)
    (set-det)
    (not-abort))
  (= (should-continue $SIn)  
    (ask-continue y))

;
; So someone else (like ref. or human)
;
; can emergency abort by connecting to the server also.

  (= (not-abort)  
    (rd-noblock abort)
    (set-det)
    (format "~nUser chose to abort!~n")
    (fail))
  (= not_abort  True)

;
; ================================================================================
;
; Move Selection methods
;
; ================================================================================

;
; CHOOSE(Chooser,Role,SIn,SOut)
;
; Contains a clause for each decision method (or player)
;
; Currently:
;
; HUMAN
;
; COMPUTER
;
; RANDOM


  (= (choose $Player $Role $SIn $SOut)  
    (observe-choice $Player $Role $SIn $SOut))

;
; If move is legal, then say it is, so the other player can process it.
;
; (This is one attempt to avoid problems when both trying to rd/in
;
; at the same time, so only 1 gets it).
;
; Note we receive a move in string representation, and parse it
;
; and find a legal interpretation in the current position.
;
; If the move isn't legal, we crash at the moment.

  (= (observe-choice $Player $Role $SIn $SOut)  
    (format "~nPlayer ~w, as ~w, must select his move.~n" 
      (:: $Player $Role))
    (observe-move $Player $MoveString)
    (nl)
    (format "Observed: ~s" 
      (:: $MoveString))
    (nl)
    (find-correct-interpretation $Player $Role $MoveString $SIn $SOut)
    (other-player $Player $Other)
    (out-personal $Other legal)
    (format "~nPlayer ~w, as ~w, has selected his move.~n" 
      (:: $Player $Role)))
	

  (= (find-correct-interpretation $Player $Role $MoveString $SIn $SOut)  
    (move-notation-string $Move $MoveString)
    (check-legality $Player $Role $Move $SIn $SOut)
    (set-det))
  (= (find-correct-interpretation $Player $Role $MoveString $SIn $SOut)  
    ( (format "~nCouldn't interpret the move, ~s, of player ~w, as ~w~n" 
        (:: $MoveString $Player $Role)) 
      (add-is-symbol  &self 
        (checkpoints  $Player $Role $MoveString $SIn $SOut)) 
      (break)))
 

  (= (check-legality $Player $Role $Move $SIn $SOut)  
    (format "~nVerifying legality of move by Player ~w, as ~w:~w~n" 
      (:: $Player $Role $Move))
    (legal $Move $SIn $SOut)
    (format "~nMove ~w  passed legality check!~n" 
      (:: $Move))
    (print-state $SOut)
    (print-notation $Move))
	   



  (= (observe-choice $Player $Role $SIn $SOut)  
    (format "~nPlayer ~w, as ~w, selected an illegal move!!!~n" 
      (:: $Player $Role))
    (set-det)
    (fail))


  (= (observe-move $Player $Move)  
    (rd-wait (moved $Player $Move)))


  (= (role_player  player $White $_ $White)  True)
  (= (role_player  opponent $_ $Black $Black)  True)


  (= (other-player $Player $Other)  
    (players $Player $Other))
  (= (other-player $Player $Other)  
    (players $Other $Player))



