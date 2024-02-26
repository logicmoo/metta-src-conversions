;
; 

;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



;
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
    ( (get-player-color player $White) 
      (get-player-color opponent $Black) 
      (query-accept-players $White $Black) 
      (set-det)))
; 

  (= 
    (get-players $_ $_) 
    ( (format "\nPlease use 'player' to select the players.\n'help player' gives more info, and a list of available players.\n" Nil) (fail)))
; 




  (= 
    (get-player-color $Color $Player) 
    ( (player-method-parameter $Color $Param) (parameter $Param $Player)))
; 




  (= 
    (query-accept-players $White $Black) 
    ( (human-mode) 
      (set-det) 
      (format "\n~p as ~p vs. ~p as ~p?\n(y to accept, n to select new players): " 
        (:: $White player $Black opponent)) 
      (read $Answer) 
      (= $Answer y)))
; 

  (= 
    (query-accept-players $White $Black) 
    (format "Playing ~p as ~p vs. ~p as ~p?" 
      (:: $White player $Black opponent)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    ( (player-current-game $Game) 
      (query-use-current-game $Game) 
      (set-det)))
; 

  (= 
    (get-current-game) 
    ( (format "\nPlease use 'game' to select a game.\n'games_library' will show you a list of available games.\n'generate <file> will generate a new game as <file>.game.\n" Nil) (fail)))
; 


;
; 


  (= 
    (query-use-current-game $Game) 
    ( (human-mode) 
      (set-det) 
      (game-name $Game $Name) 
      (format "~nPlay game ~w? (y to play, n to choose another): " 
        (:: $Name)) 
      (read $Answer) 
      (= $Answer y)))
; 

  (= 
    (query-use-current-game $Game) 
    ( (game-name $Game $Name) (format "~nPlaying game ~w" (:: $Name))))
; 



;
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
      (produce-assignment $PieceNames $Squares $Assignment)))
; 




  (= 
    (produce-assignment $PieceNames $Squares $Assignment) 
    ( (parameter assignment-method ask) 
      (set-det) 
      (ask-random-assignment $PieceNames $Squares $Assignment)))
; 

  (= 
    (produce-assignment $PieceNames $Squares $Assignment) 
    (generate-random-assignment $PieceNames $Squares $Assignment))
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


  (= 
    (ask-random-assignment $PieceNames $Squares $Assignment) 
    ( (format "~nPlease assign ~w to ~w~nExample syntax: piece1 at {(1,1)} piece2 at {(2,2) (3,3)}.~n(C-d on new line to end)~n|: " 
        (:: $PieceNames $Squares)) 
      (read-tokens $AssignmentString) 
      (nl) 
      (set-parsing-mode) 
      (assignments $Assignment $AssignmentString 
        (Cons  . $Rest))))
; 


  (= 
    (ask-random-assignment $PieceNames $Squares $Assignment) 
    ( (format "~nPlease assign ~w to ~w~n('.' for help)~n|: " 
        (:: $PieceNames $Squares)) 
      (read-keyboard-tokens $AssignmentString) 
      (help-or-process-assign $AssignmentString $PieceNames $Squares $Assignment)))
; 



  (= 
    (help-or-process-assign 
      (:: .) $PieceNames $Squares $Assignment) 
    ( (format "~nExample syntax:~npiece1 at {(1,1)} piece2 at {(2,2) (3,3)}.~n" Nil) (ask-random-assignment $PieceNames $Squares $Assignment)))
; 


  (= 
    (help-or-process-assign $AssignmentString $_ $_ $Assignment) 
    ( (nl) 
      (set-parsing-mode) 
      (assignments $Assignment $AssignmentString 
        (Cons  . $Rest))))
; 


      

  (= 
    (choose $Name $Role $Move $SIn $SOut) 
    ( (concat $Name -choose $NameChoose) 
      (=.. $Goal 
        (:: $NameChoose $Role $Move $SIn $SOut)) 
      (call $Goal)))
; 




  (= 
    (variable-choose $Role $Move $SIn $SOut) 
    ( (player-method-parameter $Role $Param) 
      (parameter $Param $Chooser) 
      (choose $Chooser $Role $Move $SIn $SOut)))
; 



