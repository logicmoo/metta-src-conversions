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

;
; 

;
; 



;
; 


  (= 
    (metagame) 
    (start-player))
; 



  (= 
    (start-player) 
    ( (find-my-name $Name) 
      (add-symbol  &self 
        (my_name  $Name)) 
      (find-server $Addr $Port) 
      (play $Addr $Port $Name)))
; 



  (= 
    (find-my-name $N) 
    (parameter player1-name $N))
; 



  (= 
    (find-server $S $P) 
    ( (parameter server-name $S) (parameter server-port $P)))
; 



  (= 
    (play $ServerAddr $Port $Player) 
    ( (use-module (library linda/client)) 
      (port-number $Port $PortNum) 
      (format "Server address is ~q:~q~n" 
        (:: $ServerAddr $PortNum)) 
      (linda-client-wait (with_self  $ServerAddr $PortNum)) 
      (greet-ref $Player) 
      (start-controller)))
; 



  (= 
    (greet-ref $Player) 
    (format "~nHello, I'm player: ~w" 
      (:: $Player)))
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
    (in-wait-personal $Pattern) 
    ( (my-name $Me) (in-wait (message $Me $Pattern))))
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
    (get-move $Player $Move) 
    ( (in-wait-personal legal) (in-wait (moved $Player $Move))))
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
    (communicate-move $Player $Move) 
    (out (moved $Player $Move)))
; 


;
; 


  (= 
    (receive-players $White $Black) 
    (in-wait-personal (players $White $Black)))
; 



  (= 
    (process-game $GameName) 
    ( (atom $GameName) 
      (set-det) 
      (file-make-test-game $GameName)))
; 

  (= 
    (process-game $GameString) 
    (string-make-test-game $GameString))
; 

	

;
; 

;
; 

;
; 



  (= 
    (get-players $White $Black) 
    ( (receive-players $White $Black) 
      (my-name $Me) 
      (find-opponent $White $Black $Me $Opponent) 
      (remove-all-symbols  &self 
        (my_opponent  $_)) 
      (add-symbol  &self 
        (my_opponent  $Opponent))))
; 



  (= 
    (find_opponent  $White $Black $White $Black) True)
; 

  (= 
    (find_opponent  $White $Black $Black $White) True)
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
    ( (receive-game-name $GameName) (load-game $GameName)))
; 



  (= 
    (receive-game-name $G) 
    (in-wait-personal (game-name $G)))
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
    ( (format "~nRequesting Random Assignment~n" Nil) 
      (get-init-state $AssignmentString) 
      (assignments-string $Assignment $AssignmentString) 
      (format "~nReceived random assignment: ~w~n" 
        (:: $Assignment))))
; 

	

;
; 

;
; 

;
; 


  (= 
    (get-init-state $Assignment) 
    (in-wait-personal (init-state $Assignment)))
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
    ( (format "~nI'm finished playing the game.~n" Nil) 
      (analyze-game $SIn) 
      (restart-or-end)))
; 


;
; 


  (= 
    (analyze_game  $_) True)
; 

	


  (= 
    (restart-or-end) 
    ( (in-wait-personal (reset $R)) 
      (set-det) 
      (restart-if $R)))
; 



  (= 
    (restart-if new) 
    ( (format "~nStarting again!~n" Nil) (start-controller)))
; 

  (= 
    (restart-if end) 
    ( (format "~nTournament is finished. Bye!~n" Nil) (close-player)))
; 



  (= close_player True)
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
    (should-continue $_) 
    ( (continuous) 
      (set-det) 
      (not-abort)))
; 

  (= 
    (should-continue $_) 
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
    (choose $Me $Role $SIn $SOut) 
    ( (my-name $Me) 
      (set-det) 
      (my-choice $Role $SIn $SOut)))
; 

  (= 
    (choose $Other $Role $SIn $SOut) 
    (other-choice $Role $SIn $SOut))
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
    (my-choice $Role $SIn $SOut) 
    ( (my-name $Me) 
      (move $Role $Move $SIn $SOut) 
      (move-notation-string $Move $MoveString) 
      (communicate-move $Me $MoveString)))
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
    (other-choice $Role $SIn $SOut) 
    ( (my-opponent $Player) 
      (get-move $Player $MoveString) 
      (nl) 
      (format "Received: ~s" 
        (:: $MoveString)) 
      (nl) 
      (find-correct-interpretation $Player $Role $MoveString $SIn $SOut)))
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




