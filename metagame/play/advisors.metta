;
; 

;
; 

;
; 

;
; 


;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 


;
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
    (human-choose $Player $Move $SIn $SOut) 
    ( (control $Player $SIn) (ask-move $Move $SIn $SOut)))
; 


	
;
; 

;
; 

;
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
    (threaten-choose $Role $Move $SIn $SOut) 
    ( (det-if-then 
        (move-count $N $SIn) 
        (=< $N 1)) 
      (set-det) 
      (random-choose $Role $Move $SIn $SOut)))
; 

  (= 
    (threaten-choose $_ $Move $SIn $SOut) 
    ( (timing (threaten-move $Move $SIn $SOut)) 
      (set-det) 
      (print-choice $Move $SIn $SOut)))
; 

  (= 
    (threaten-choose $Role $Move $SIn $SOut) 
    (random-choose $Role $Move $SIn $SOut))
; 



;
; 

;
; 

;
; 


  (= 
    (instant-choose $Player $Move $SIn $SOut) 
    ( (control $Player $SIn) 
      (format "~nThe Instant Moves:~n" Nil) 
      (instant-move $Move $SIn $SOut) 
      (print-choice $Move $SIn $SOut)))
; 

	
;
; 


  (= 
    (instant-move $Move $SIn $SOut) 
    ( (legal $Move $SIn $SOut) (set-det)))
; 



;
; 

;
; 

;
; 


  (= 
    (random-choose $Player $Move $SIn $SOut) 
    ( (control $Player $SIn) 
      (timing (random-move $Move $SIn $SOut)) 
      (set-det) 
      (print-choice $Move $SIn $SOut)))
; 


;
; 


  (= 
    (random-move $M $SIn $SOut) 
    (random-success (legal $M $SIn $SOut)))
; 




;
; 

;
; 

;
; 


  (= 
    (cautious-choose $Player $Move $SIn $SOut) 
    ( (timing (cautious-move $Move $SIn $SOut)) 
      (set-det) 
      (print-choice $Move $SIn $SOut)))
; 



;
; 

;
; 



  (= 
    (cautious-move $Move $SIn $SOut) 
    ( (safe-move $Move $SIn $SOut) (not (victor-move $M2 $SOut $_))))
; 



;
; 



  (= 
    (safe-move $Move $SIn $SOut) 
    ( (control $Player $SIn) 
      (opposite-role $Player $Opponent) 
      (legal $Move $SIn $SOut) 
      (not (game-outcome $Opponent $SOut))))
; 


;
; 

;
; 


  (= 
    (check-safe-move $Move $SIn $SOut) 
    ( (control $Player $SIn) 
      (opposite-role $Player $Opponent) 
      (not (game-outcome $Opponent $SOut))))
; 





;
; 

;
; 

;
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
    (random-aggressive-choose $Player $Move $SIn $SOut) 
    ( (timing (random-aggressive-move $Move $SIn $SOut)) 
      (set-det) 
      (print-choice $Move $SIn $SOut)))
; 




  (= 
    (random-aggressive-move $Move $SIn $SOut) 
    ( (victor-move $Move $SIn $SOut) (set-det)))
; 

  (= 
    (random-aggressive-move $Move $SIn $SOut) 
    (random-cautious-move $Move $SIn $SOut))
; 



;
; 

;
; 

;
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
    (random-cautious-choose $Player $Move $SIn $SOut) 
    ( (timing (random-cautious-move $Move $SIn $SOut)) 
      (set-det) 
      (print-choice $Move $SIn $SOut)))
; 



  (= 
    (random-cautious-move $Move $SIn $SOut) 
    ( (legal-moves $Moves $SIn) 
      (random-non-losing $Moves $Move $SIn $SOut) 
      (set-det)))
; 

  (= 
    (random-cautious-move $Move $SIn $SOut) 
    ( (print-resign-notice) (random-move $Move $SIn $SOut)))
; 



  (= 
    (print-resign-notice) 
    (format "\nA cautious player would resign now ... \nbut perhaps my opponent won't see it!\n" Nil))
; 



  (= 
    (print-forced-notice) 
    (format "Forced choice: only 1 legal move~n" Nil))
; 



  (= 
    (print-rushed-notice) 
    (format "Rushed choice: no time to think!~n" Nil))
; 



  (= 
    (print-forced-or-lost-notice) 
    (format "Forced choice: only 1 legal move (or all others lose!)~n" Nil))
; 



  (= 
    (random-non-losing 
      (:: $Move) $Move $SIn $SOut) 
    ( (set-det) 
      (print-forced-or-lost-notice) 
      (legal $Move $SIn $SOut)))
; 

  (= 
    (random-non-losing $Moves $Move $SIn $SOut) 
    ( (random-select $Move1 $Moves $RestMoves) (nl-or-next $Move1 $RestMoves $Move $SIn $SOut)))
; 



  (= 
    (nl-or-next $Move $_ $Move $SIn $SOut) 
    ( (cautious-move $Move $SIn $SOut) (set-det)))
; 

  (= 
    (nl-or-next $_ $Moves $Move $SIn $SOut) 
    ( (random-non-losing $Moves $Move $SIn $SOut) (set-det)))
; 



  (= 
    (legal-moves $Moves $SIn) 
    (setof $Move 
      (^ $S1 
        (legal $Move $SIn $S1)) $Moves))
; 


;
; 


;
; 

;
; 



  (= 
    (pass-move $SIn $SOut) 
    ( (control $P1 $SIn) 
      (transfer-control $_ $SIn $SOut) 
      (control $P2 $SOut) 
      (format "Passing, control has now transferred from ~p to ~p~n" 
        (:: $P1 $P2))))
; 




;
; 


  (= 
    (victor-move $Move $SIn $SOut) 
    ( (control $Player $SIn) 
      (legal $Move $SIn $SOut) 
      (game-outcome $Player $SOut)))
; 


;
; 


  (= 
    (endgame-move $Move $SIn $SOut) 
    ( (legal $Move $SIn $SOut) (game-over $SOut)))
; 



;
; 

;
; 

;
; 


  (= 
    (mate-move $Move $SIn $SOut) 
    ( (legal $Move $SIn $SOut) (not (cautious-move $M2 $SOut $_))))
; 



;
; 

;
; 



  (= 
    (threaten-move $Move $SIn $SOut) 
    ( (legal $Move $SIn $SOut) (det-if-then (transfer-control $_ $SOut $S1) (victor-move $M2 $S1 $_))))
; 



;
; 

;
; 



  (= 
    (enough-rope-move $Move $SIn $SOut) 
    ( (legal $Move $SIn $SOut) 
      (legal $M2 $SOut $S1) 
      (victor-move $M3 $S1 $_)))
; 



