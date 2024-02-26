;                                           (convert_to_metta_file  player $_380830 metagame/comms/player.pl metagame/comms/player.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;; player.pl
;
; ;;



  !(prolog-flag redefine-warnings $_ off)


;
; ================================================================================
;
; PLAYER
;
; ================================================================================
;
; 
;
; PLAYER MeTTa process will be called with command like:
;
; start_sicstus_shell(any,
;
; [ server_name, ServerAddr,
;
; server_port, Port,
;
; local_name, Player,
;
; file, MoveFile,
;
; file,'~/MeTTa/play/player'
;
; ]).
;
; The MoveFile should define the search method which will be used when
;
; this player is to move.


;
; Redefine metagame here, so we don't give menu and instead just start player.

  (= (metagame)  
    (start-player))


  (= (start-player)  
    ( (find-my-name $Name) 
      (add-is-symbol  &self 
        (my_name  $Name)) 
      (find-server $Addr $Port) 
      (play $Addr $Port $Name)))


  (= (find-my-name $N)  
    (parameter player1-name $N))


  (= (find-server $S $P)  
    (parameter server-name $S)
    (parameter server-port $P))


  (= (play $ServerAddr $Port $Player)  
    (use-module (library linda/client))
    (port-number $Port $PortNum)
    (format "Server address is ~q:~q~n" 
      (:: $ServerAddr $PortNum))
    (linda-client-wait (with_self  $ServerAddr $PortNum))
    (greet-ref $Player)
    (start-controller))


  (= (greet-ref $Player)  
    (format "~nHello, I'm player: ~w" 
      (:: $Player)))





;
; These are the interface routines for a player to receive and send
;
; information to a controller, when playing against an opponent.


;
; Waits for a personal message, of the form:
;
; message(<Me>,Pattern)

  (= (in-wait-personal $Pattern)  
    (my-name $Me)
    (in-wait (message $Me $Pattern)))




;
; GET_MOVE(Player,Move)
;
; If communicating directly (not via ref) don't need
;
; above transmit, just one puts in, other takes out, and
;
; vice versa.
;
; Someone must say the move is legal, before I'll get it.
;
; If not ref, the other player himself can do so.
;
; This gets the string notation, which must later be parsed
;
; into an internal representation denoting the legal move.
;
; moved(player,move) is the only message not personally addressed.

  (= (get-move $Player $Move)  
    (in-wait-personal legal)
    (in-wait (moved $Player $Move)))

;
; COMMUNICATE_MOVE(Player,Move)
;
; Sends the move, in string notation.
;
; Doesn't need to be personal, as it mentions the
;
; player and so won't be intercepted by anyone not involved.
;
; And the other player won't see it till the ref. has said it is
;
; legal.

  (= (communicate-move $Player $Move)  
    (out (moved $Player $Move)))

;
; RECEIVE_PLAYERS(White,Black)

  (= (receive-players $White $Black)  
    (in-wait-personal (players $White $Black)))


  (= (process-game $GameName)  
    (atom $GameName)
    (set-det)
    (file-make-test-game $GameName))
  (= (process-game $GameString)  
    (string-make-test-game $GameString))
	

;
; ================================================================================
;
; Hooks to standard controller script
;
; ================================================================================


  (= (get-players $White $Black)  
    ( (receive-players $White $Black) 
      (my-name $Me) 
      (find-opponent $White $Black $Me $Opponent) 
      (remove-all-atoms  &self 
        (my_opponent  $_)) 
      (add-is-symbol  &self 
        (my_opponent  $Opponent))))


  (= (find_opponent  $White $Black $White $Black)  True)
  (= (find_opponent  $White $Black $Black $White)  True)


;
; ========================================
;
; GET_CURRENT_GAME
;
; ========================================
;
; Receive the name of the next game to be played.
;
; Then load it as the current game.


  (= (get-current-game)  
    (receive-game-name $GameName)
    (load-game $GameName))


  (= (receive-game-name $G)  
    (in-wait-personal (game-name $G)))


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
; Gets the random assignment string from the referee,
;
; then parses it.

  (= (get-random-assignment $Assignment)  
    (format "~nRequesting Random Assignment~n" Nil)
    (get-init-state $AssignmentString)
    (assignments-string $Assignment $AssignmentString)
    (format "~nReceived random assignment: ~w~n" 
      (:: $Assignment)))
	

;
; GET_INIT_STATE(-Assignment)
;
; Gets the string representing the initial state assignment
;
; (for random-setup games).

  (= (get-init-state $Assignment)  
    (in-wait-personal (init-state $Assignment)))


;
; ================================================================================
;
; TERMINATE_GAME(+FinalState)
;
; ================================================================================
;
; Hook called when the game has ended.



  (= (terminate-game $SIn)  
    (format "~nI'm finished playing the game.~n" Nil)
    (analyze-game $SIn)
    (restart-or-end))

;
; To be defined by player's own programs.

  (= (analyze_game  $_)  True)
	


  (= (restart-or-end)  
    (in-wait-personal (reset $R))
    (set-det)
    (restart-if $R))


  (= (restart-if new)  
    (format "~nStarting again!~n" Nil)
    (start-controller))
  (= (restart-if end)  
    (format "~nTournament is finished. Bye!~n" Nil)
    (close-player))


  (= close_player  True)  

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
;
; 
;
; Some routines here defined in local.pl


  (= (should-continue $_)  
    (continuous)
    (set-det)
    (not-abort))
  (= (should-continue $_)  
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
; Contains a clause for whether local player is moving or not.
;
; Currently:
;
; If ME, calls MOVE as loaded in designated file.
;
; If NOT ME, gets move from remote opponent.
;
; Note my_name was saved above.


  (= (choose $Me $Role $SIn $SOut)  
    (my-name $Me)
    (set-det)
    (my-choice $Role $SIn $SOut))
  (= (choose $Other $Role $SIn $SOut)  
    (other-choice $Role $SIn $SOut))




;
; ================================================================================
;
; MOVE(Role,Move,SIn,SOut)
;
; ================================================================================
;
; A hook into a file which is loaded by this player to determine how to
;
; move. The result is the move that the player will choose.
;
; By splitting this up, the PLAYER script can be used by all different
;
; programs to play remotely, just as the CONTROLLER script is used
;
; by all participants.
;
; Some choices of files: humanist, randomist, instantist.
;
; ================================================================================


;
; MY_CHOICE(Role,SIn,SOut)
;
; What to do when it is my turn.
;
; Uses move/4 defined in accompanying file to select a
;
; move, returning the internal representation.
;
; Then translate this into the appropriate grammatical string
;
; representation, and communicate this to the other player.

  (= (my-choice $Role $SIn $SOut)  
    (my-name $Me)
    (move $Role $Move $SIn $SOut)
    (move-notation-string $Move $MoveString)
    (communicate-move $Me $MoveString))


;
; OTHER_CHOICE(Role,SIn,SOut)
;
; What to do when it's the other player's turn.
;
; Get his move, parse it, and find the interpretation which is
;
; legal (assumed unambiguous w.r.t. current position).

  (= (other-choice $Role $SIn $SOut)  
    (my-opponent $Player)
    (get-move $Player $MoveString)
    (nl)
    (format "Received: ~s" 
      (:: $MoveString))
    (nl)
    (find-correct-interpretation $Player $Role $MoveString $SIn $SOut))
; ; nl, format(MoveString,[]), nl,



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



