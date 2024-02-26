;
; 

;
; 

;
; 

;
; 


;
; 



;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 


;
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
    (get-if-random-assignment) 
    ( (current-random-setup-game) 
      (set-det) 
      (get-random-assignment $Assignment) 
      (set-random-assignment $Assignment)))
; 

  (= get_if_random_assignment True)
; 


;
; 


  (= 
    (set-random-assignment $Assignment) 
    ( (remove-all-symbols  &self 
        (random_assignment  $_)) (add-symbol  &self (random_assignment  $Assignment))))
; 



;
; 

;
; 

;
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
    (start) 
    (start-controller))
; 



  (= 
    (start-top $PosName) 
    ( (checkpoint $PosName $SIn) 
      (set-current-pos-name $PosName) 
      (start-controller $SIn)))
; 





  (= 
    (start-controller) 
    ( (get-current-game) 
      (get-initial-state $SIn) 
      (initialize-history $SIn) 
      (start-controller $SIn)))
; 
  


  (= 
    (get-initial-state $SIn) 
    ( (get-if-random-assignment) (start-game $SIn)))
; 

	

;
; 

;
; 

;
; 

;
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
    (start-controller $SIn) 
    ( (format "~nInitial Position:~n" Nil) 
      (print-state $SIn) 
      (linebreak) 
      (reset-clock) 
      (controller $SIn $_) 
      (set-det)))
; 




  (= 
    (start-game $Init) 
    ( (new-state $State) (start-game $State $Init)))
; 



;
; 

;
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
    (controller $SIn $SIn) 
    ( (game-ends-in-outcome $SIn $Outcome) 
      (set-det) 
      (record-game-outcome $Outcome) 
      (checkpoint-state final $SIn)))
; 

  (= 
    (controller $SIn $SOut) 
    ( (tracing-play clock print-clock) 
      (play-in-control $SIn $S1) 
      (should-continue $S1) 
      (set-det) 
      (cleanup-state $S1 $S2) 
      (controller $S2 $SOut)))
; 

  (= 
    (controller $SIn $SIn) 
    (format "The game has been halted prematurely!!" Nil))
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
    (play-in-control $SIn $SOut) 
    ( (control $Role $SIn) 
      (initialize-player-move $Role) 
      (role-chooser $Role $Chooser) 
      (choose-or-resign $Chooser $Role $SIn $SOut)))
; 


;
; 

;
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
    (choose-or-resign $Chooser $Role $SIn $SOut) 
    ( (realtime-success 
        (choose $Chooser $Role $Move $SIn $SOut) $Time) 
      (set-det) 
      (adjust-player-clock $Role $Time) 
      (add-state-to-history $Move $SOut) 
      (restore-parameters)))
; 

  (= 
    (choose-or-resign $Chooser $Role $SIn $_) 
    ( (opposite-role $Role $OppRole) 
      (role-chooser $OppRole $OppChooser) 
      (format "~n~p, as ~p, failed to select a legal move.~nThus, ~p, as ~p, is declared the Winner!~n" 
        (:: $Chooser $Role $OppChooser $OppRole)) 
      (checkpoint-state final $SIn) 
      (restore-parameters) 
      (fail)))
; 

	

;
; 

;
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
    (continuous) 
    (parameter continuous yes))
; 



  (= 
    (set-continuous) 
    (set-parameter continuous yes))
; 


  (= 
    (set-stepping) 
    (set-parameter continuous no))
; 



  (= 
    (should-continue $SIn) 
    ( (continuous) (set-det)))
; 

  (= 
    (should-continue $SIn) 
    (ask-continue y))
; 



  (= 
    (ask-continue $Answer) 
    ( (ask-ynp Continue $Answer1) 
      (set-det) 
      (= $Answer1 $Answer)))
; 



  (= 
    (ask-ynp $Query $Answer) 
    ( (format "~a? (y or n)~n" 
        (:: $Query)) 
      (read $Answer1) 
      (ynp $Answer1 $Answer 
        (ask-ynp $Query $Answer))))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
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
    (initialize-player-move $Role) 
    ( (save-parameters) 
      (role-file $Role $File) 
      (det-if-then-else 
        (member $File 
          (:: none Nil)) True 
        (load-player-eval $File))))
; 


;
; 

;
; 


  (= 
    (load-player-eval (Cons  $P $Ps)) 
    ( (set-det) (restore-parameters (Cons  $P $Ps))))
; 

  (= 
    (load-player-eval $Name) 
    (load-player-eval-file $Name))
; 

	


  (= 
    (set-com $_ $_ $_ $C $P $V) 
    (set-color-parameter $C $P $V))
; 



  (= 
    (set-top $C $P $V) 
    (set-color-parameter $C $P $V))
; 



  (= 
    (unset-top $C) 
    ( (color-player $C $Role) (clear-player-parameters $Role)))
; 



  (= 
    (set-color-parameter $Color $P $V) 
    ( (color-player $Color $Role) (set-player-parameter $Role $P $V)))
; 



  (= 
    (set-player-parameter $Role $Param $Val) 
    ( (role-file $Role $List1) 
      (set-assoc $List1 $Param $Val $New) 
      (set-role-file $Role $New)))
; 

	

  (= 
    (clear-player-parameters $Role) 
    (set-role-file $Role none))
; 





  (= 
    (load-player-eval-file $Name) 
    ( (find-eval-file $Name $File) 
      (save-parameters) 
      (compile $File)))
; 

	
;
; 


  (= 
    (role-chooser $Role $Chooser) 
    ( (player-method-parameter $Role $Param) (parameter $Param $Chooser)))
; 



  (= 
    (player_method_parameter  player player_method) True)
; 

  (= 
    (player_method_parameter  opponent opponent_method) True)
; 



  (= 
    (role-file $Role $File) 
    ( (player-file-parameter $Role $Param) (parameter $Param $File)))
; 



  (= 
    (set-role-file $Role $File) 
    ( (player-file-parameter $Role $Param) (set-parameter $Param $File)))
; 




  (= 
    (player_file_parameter  player player_file) True)
; 

  (= 
    (player_file_parameter  opponent opponent_file) True)
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
    (human-mode) 
    (det-if-then 
      (role-chooser $Role human) True))
; 
 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 


;
; 

;
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
    (adjust-player-clock $Player $Time) 
    ( (remove-symbol  &self 
        (time_used  $Player $TOld)) 
      (is $TNew 
        (+ $TOld $Time)) 
      (add-symbol  &self 
        (time_used  $Player $TNew))))
; 


;
; 

;
; 


  (= 
    (reset-clock) 
    ( (reset-player-clock player) (reset-player-clock opponent)))
; 



  (= 
    (reset-player-clock $Player) 
    ( (remove-all-symbols  &self 
        (time_used  $Player $_)) (add-symbol  &self (time_used  $Player 0))))
; 



;
; 

;
; 


  (= 
    (print-clock) 
    ( (format "~*c~n" 
        (:: 40 45)) 
      (format "Clock times (in seconds):~n" Nil) 
      (print-player-clock player) 
      (print-player-clock opponent) 
      (format "~*c~n" 
        (:: 40 45))))
; 



  (= 
    (print-player-clock $Player) 
    ( (time-used $Player $Used) 
      (game-time-left $Player $Left) 
      (format "<~p>: \t~3d used, \t~3d left~n" 
        (:: $Player $Used $Left))))
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
    (time-out-outcome $Outcome) 
    ( (player-time-out player $PTime) 
      (player-time-out opponent $OTime) 
      (time-out-outcome $PTime $OTime $Outcome)))
; 


  (= 
    (time-out-outcome yes yes draw) 
    ( (set-det) (format "Both players are out of time!~n" Nil)))
; 

  (= 
    (time-out-outcome no yes player) 
    ( (set-det) (format "<~p> is out of time~n" (:: opponent))))
; 

  (= 
    (time-out-outcome yes no opponent) 
    ( (set-det) (format "<~p> is out of time~n" (:: player))))
; 



;
; 

;
; 

;
; 


  (= 
    (player-time-out $Player $Out) 
    ( (game-time-left $Player $Time) (det-if-then-else (=< $Time 0) (= $Out yes) (= $Out no))))
; 




;
; 

;
; 


  (= 
    (game-time-left $Player $Time) 
    ( (game-time-limit $Limit) 
      (time-used $Player $Used) 
      (is $Time 
        (max 
          (- $Limit $Used) 0))))
; 



  (= 
    (game-time-limit $Limit) 
    (parameter game-time-limit $Limit))
; 

	
;
; 


  (= 
    (clock-unlimit) 
    ( (set-parameter game-time-limit 99999999) 
      (set-parameter move-time-limit 99999999) 
      (set-parameter move-horizon 1) 
      (reset-clock)))
; 



;
; 



  (= 
    (game-ends-in-outcome $SIn $Outcome) 
    ( (game-over $SIn) 
      (set-det) 
      (game-outcome $Outcome $SIn)))
; 

  (= 
    (game-ends-in-outcome $SIn $Outcome) 
    (time-out-outcome $Outcome))
; 


;
; 

;
; 



  (= 
    (game-over $SIn) 
    (game-over $SIn $_))
; 



  (= 
    (game-outcome $O $S) 
    (game-outcome $O $S $_))
; 





  !(dynamic (/ recorded-game-outcome 1))
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (record-game-outcome $O) 
    ( (remove-all-symbols  &self 
        (recorded_game_outcome  $O)) 
      (add-symbol  &self 
        (recorded_game_outcome  $O)) 
      (write-outcome $O)))
; 




  (= 
    (write-outcome draw) 
    ( (format "~n~nThe game is over. Ends in a draw!~n" Nil) (set-det)))
; 

  (= 
    (write-outcome $Player) 
    (format "~n~nThe game is over.  ~p wins!~n" 
      (:: $Player)))
; 


;
; 

;
; 

;
; 


  (= 
    (cleanup_state  $S $S) True)
; 



