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

	

  !(prolog-flag redefine-warnings $_ off)
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
    (metagame) 
    (start-ref))
; 



  (= 
    (start-ref) 
    ( (find-server $Server $Port) 
      (find-players $Player1 $Player2) 
      (referee $Server $Port $Player1 $Player2)))
; 



  (= 
    (find-server $S $P) 
    ( (parameter server-name $S) (parameter server-port $P)))
; 




  (= 
    (find-players $Player1 $Player2) 
    ( (parameter player1-name $Player1) (parameter player2-name $Player2)))
; 



	

  (= 
    (referee $ServerAddr $Port $Player1 $Player2) 
    ( (use-module (library linda/client)) 
      (port-number $Port $PortNum) 
      (linda-client-wait (with_self  $ServerAddr $PortNum)) 
      (greet-players $Player1 $Player2) 
      (format "~nI'm your referee!~n" Nil) 
      (add-symbol  &self 
        (players  $Player1 $Player2)) 
      (start-controller)))
; 



  (= 
    (greet-players $P1 $P2) 
    (format "~nHello, player1: ~w, player2: ~w~n" 
      (:: $P1 $P2)))
; 



;
; 

	

;
; 

;
; 



  (= 
    (referee-game-contests $P1 $P2 $G 0) 
    ( (set-det) (record-statistics)))
; 

  (= 
    (referee-game-contests $P1 $P2 $G $Contests) 
    ( (> $Contests 0) 
      (referee-contest $P1 $P2 $G) 
      (is $C1 
        (- $Contests 1)) 
      (referee-game-contests $P1 $P2 $G $C1)))
; 

	

;
; 

;
; 



  (= 
    (close-match $P1 $P2) 
    ( (close-players $P1 $P2) 
      (close-server) 
      (record-statistics) 
      (close-client)))
; 

	

  (= 
    (close-server) 
    (with_self  
      (linda *) 
      (linda-call halt)))
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
    (call-for-players $PlayerCall) 
    ( (players $Player1 $Player2) (call-for-players $Player1 $Player2 $PlayerCall)))
; 


  (= 
    (call-for-players $Player1 $Player2 $PlayerCall) 
    ( (call-for-player $Player1 $PlayerCall) (call-for-player $Player2 $PlayerCall)))
; 



  (= 
    (call-for-player $Name 
      (^ $Player $Call)) 
    (verify (, (= $Player $Name) (call $Call))))
; 


;
; 

;
; 


  (= 
    (out-to-players $Pattern) 
    (call-for-players (^ $P (out-personal $P $Pattern))))
; 



;
; 

;
; 


  (= 
    (out-personal $Player $Pattern) 
    (out1 (message $Player $Pattern)))
; 



  (= 
    (out1 $Pattern) 
    ( (format "~nSending pattern: ~w~n" 
        (:: $Pattern)) (out $Pattern)))
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
    (get-players $White $Black) 
    ( (next-players $White $Black) (set-players $White $Black)))
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
    (next-players $White $Black) 
    ( (remove-symbol  &self 
        (players  $Black $White)) (add-symbol  &self (players  $White $Black))))
; 

	

;
; 

;
; 


  (= 
    (set-players $White $Black) 
    (out-to-players (players $White $Black)))
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
    (get-current-game) 
    ( (select-next-game $GameName) 
      (load-game $GameName) 
      (send-game-name-to-players $GameName)))
; 



  (= 
    (send-game-name-to-players $GameName) 
    (out-to-players (game-name $GameName)))
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
    (select-next-game $GameName) 
    ( (next-alternate-game $GameName) (set-det)))
; 

  (= 
    (select_next_game  chess) True)
; 



  (= 
    (next-alternate-game $GameName) 
    ( (player-current-game-name $OldGame) 
      (set-det) 
      (game-follows $OldGame $GameName)))
; 

	

  (= 
    (player-current-game-name $Name) 
    ( (player-current-game $G) (game-name $G $Name)))
; 



  (= 
    (game_follows  chess checkers) True)
; 

  (= 
    (game_follows  checkers shogi) True)
; 

  (= 
    (game_follows  shogi turncoat_chess) True)
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
    (get-random-assignment $Assignment) 
    ( (game-assignments $Game $As) 
      (assignment-decision $As random $PieceNames $Squares) 
      (generate-random-assignment $PieceNames $Squares $Assignment) 
      (send-assignment $Assignment)))
; 



;
; 


  (= 
    (generate-random-assignment $PieceNames $Squares $Assignment) 
    ( (format "~nGenerating Random Assignment~n" Nil) (assign-pieces-to-squares $PieceNames $Squares $Assignment)))
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
    (send-assignment $Assignment) 
    ( (assignments-string $Assignment $AssignmentString) 
      (format "~nSending random assignment: ~w~n" 
        (:: $Assignment)) 
      (out-to-players (init-state $AssignmentString))))
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
    (terminate-game $SIn) 
    ( (format "~nThe game has finished~n" Nil) 
      (process-results $SIn) 
      (restart-or-end)))
; 


;
; 


  (= 
    (process_results  $SIn) True)
; 



  (= 
    (restart-or-end) 
    (det-if-then-else 
      (next-alternate-game $GameName) restart goodbye-players))
; 

	

  (= 
    (restart) 
    ( (out-to-players (reset new)) (start-controller)))
; 



  (= 
    (goodbye-players) 
    ( (out-to-players (reset end)) (format "~nTournament is finished. Bye!~n" Nil)))
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
    (should-continue $SIn) 
    ( (continuous) 
      (set-det) 
      (not-abort)))
; 

  (= 
    (should-continue $SIn) 
    (ask-continue y))
; 


;
; 

;
; 


  (= 
    (not-abort) 
    ( (rd-noblock abort) 
      (set-det) 
      (format "~nUser chose to abort!~n") 
      (fail)))
; 

  (= not_abort True)
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
    (choose $Player $Role $SIn $SOut) 
    (observe-choice $Player $Role $SIn $SOut))
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
    (observe-choice $Player $Role $SIn $SOut) 
    ( (format "~nPlayer ~w, as ~w, must select his move.~n" 
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
        (:: $Player $Role))))
; 

	

  (= 
    (find-correct-interpretation $Player $Role $MoveString $SIn $SOut) 
    ( (move-notation-string $Move $MoveString) 
      (check-legality $Player $Role $Move $SIn $SOut) 
      (set-det)))
; 

  (= 
    (find-correct-interpretation $Player $Role $MoveString $SIn $SOut) 
    ( (format "~nCouldn't interpret the move, ~s, of player ~w, as ~w~n" 
        (:: $MoveString $Player $Role)) 
      (add-symbol  &self 
        (checkpoints  $Player $Role $MoveString $SIn $SOut)) 
      (break)))
; 

 

  (= 
    (check-legality $Player $Role $Move $SIn $SOut) 
    ( (format "~nVerifying legality of move by Player ~w, as ~w:~w~n" 
        (:: $Player $Role $Move)) 
      (legal $Move $SIn $SOut) 
      (format "~nMove ~w  passed legality check!~n" 
        (:: $Move)) 
      (print-state $SOut) 
      (print-notation $Move)))
; 

	   



  (= 
    (observe-choice $Player $Role $SIn $SOut) 
    ( (format "~nPlayer ~w, as ~w, selected an illegal move!!!~n" 
        (:: $Player $Role)) 
      (set-det) 
      (fail)))
; 



  (= 
    (observe-move $Player $Move) 
    (rd-wait (moved $Player $Move)))
; 



  (= 
    (role_player  player $White $_ $White) True)
; 

  (= 
    (role_player  opponent $_ $Black $Black) True)
; 



  (= 
    (other-player $Player $Other) 
    (players $Player $Other))
; 

  (= 
    (other-player $Player $Other) 
    (players $Other $Player))
; 




