;                                           (convert-to-metta-file  advisors $_486236 metagame/play/advisors.pl metagame/play/advisors.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;; advisors.pl
;
; ;; Provides move filters and corresponding choice methods (players).

;
; ================================================================================
;
; Choice Methods
;
; ================================================================================
;
; Here are a few built-in choice methods which can be called for different players.
;
; See the file local.pl for how they are used with the local interface,
;
; or the file randomist.pl for an example how they can serve as the basis for
;
; remote players.
;
; 
;
; The move generators (or filters) on which they are based are defined here also.

;
; ================================
;
; HUMAN_CHOOSE(Player,Move,SIn,SOut)
;
; ================================
;
; Calls the nice user a nice interface for selecting moves.
;
; 

  (= (human-choose $Player $Move $SIn $SOut)  
    (control $Player $SIn)
    (ask-move $Move $SIn $SOut))

	
;
; ================================
;
; THREATEN_CHOOSE(Player,Move,SIn,SOut)
;
; ================================
;
; Choose randomly at start, as checking for stalemate
;
; at start of game takes so long.
;
; After start, play threats if have them, else random.
;
; 

  (= (threaten-choose $Role $Move $SIn $SOut)  
    (det-if-then 
      (move-count $N $SIn) 
      (=< $N 1))
    (set-det)
    (random-choose $Role $Move $SIn $SOut))
  (= (threaten-choose $_ $Move $SIn $SOut)  
    (timing (threaten-move $Move $SIn $SOut))
    (set-det)
    (print-choice $Move $SIn $SOut))
  (= (threaten-choose $Role $Move $SIn $SOut)  
    (random-choose $Role $Move $SIn $SOut))


;
; ================================
;
; INSTANT_CHOOSE(Player,Move,SIn,SOut)
;
; ================================

  (= (instant-choose $Player $Move $SIn $SOut)  
    (control $Player $SIn)
    (format "~nThe Instant Moves:~n" Nil)
    (instant-move $Move $SIn $SOut)
    (print-choice $Move $SIn $SOut))
	
;
; INSTANT_MOVE(Move,SIn,SOut)

  (= (instant-move $Move $SIn $SOut)  
    (legal $Move $SIn $SOut)
    (set-det))


;
; ========================================
;
; RANDOM_CHOOSE(Player,Move,SIn,SOut)
;
; ========================================

  (= (random-choose $Player $Move $SIn $SOut)  
    (control $Player $SIn)
    (timing (random-move $Move $SIn $SOut))
    (set-det)
    (print-choice $Move $SIn $SOut))

;
; RANDOM_MOVE(M,SIn,SOut)

  (= (random-move $M $SIn $SOut)  
    (random-success (legal $M $SIn $SOut)))



;
; ========================================
;
; CAUTIOUS_CHOOSE(Player,Move,SIn,SOut)
;
; ========================================

  (= (cautious-choose $Player $Move $SIn $SOut)  
    (timing (cautious-move $Move $SIn $SOut))
    (set-det)
    (print-choice $Move $SIn $SOut))


;
; A CAUTIOUS_MOVE is one which does not lose immediately,
;
; and which does not allow a victor_move response.


  (= (cautious-move $Move $SIn $SOut)  
    (safe-move $Move $SIn $SOut)
    (not (victor-move $M2 $SOut $_)))


;
; A SAFE_MOVE is one which does not lose immediately.


  (= (safe-move $Move $SIn $SOut)  
    (control $Player $SIn)
    (opposite-role $Player $Opponent)
    (legal $Move $SIn $SOut)
    (not (game-outcome $Opponent $SOut)))

;
; Assumes legal already, just makes sure the player who
;
; moved doesn't lose as a result.

  (= (check-safe-move $Move $SIn $SOut)  
    (control $Player $SIn)
    (opposite-role $Player $Opponent)
    (not (game-outcome $Opponent $SOut)))




;
; ========================================
;
; RANDOM_AGGRESSIVE_CHOOSE(Player,Move,SIn,SOut)
;
; ========================================
;
; Wins immediately if possible, else plays a random move
;
; which doesn't lose immediately or allow opponent to
;
; win on the next move. If all moves can lose,
;
; plays any random move.



  (= (random-aggressive-choose $Player $Move $SIn $SOut)  
    (timing (random-aggressive-move $Move $SIn $SOut))
    (set-det)
    (print-choice $Move $SIn $SOut))



  (= (random-aggressive-move $Move $SIn $SOut)  
    (victor-move $Move $SIn $SOut)
    (set-det))
  (= (random-aggressive-move $Move $SIn $SOut)  
    (random-cautious-move $Move $SIn $SOut))


;
; ========================================
;
; RANDOM_CAUTIOUS_CHOOSE(Player,Move,SIn,SOut)
;
; ========================================
;
; Plays a random move which doesn't lose immediately or allow opponent to
;
; win on the next move.
;
; If none available, plays any random move (really, should play
;
; one which doesn't cause a lose in preference to one which does ...).


  (= (random-cautious-choose $Player $Move $SIn $SOut)  
    (timing (random-cautious-move $Move $SIn $SOut))
    (set-det)
    (print-choice $Move $SIn $SOut))


  (= (random-cautious-move $Move $SIn $SOut)  
    (legal-moves $Moves $SIn)
    (random-non-losing $Moves $Move $SIn $SOut)
    (set-det))
  (= (random-cautious-move $Move $SIn $SOut)  
    (print-resign-notice (random-move $Move $SIn $SOut)))


  (= print-resign-notice  
    (format "\nA cautious player would resign now ... \nbut perhaps my opponent won't see it!\n" Nil))


  (= print-forced-notice  
    (format "Forced choice: only 1 legal move~n" Nil))


  (= print-rushed-notice  
    (format "Rushed choice: no time to think!~n" Nil))


  (= print-forced-or-lost-notice  
    (format "Forced choice: only 1 legal move (or all others lose!)~n" Nil))


  (= (random-non-losing (:: $Move) $Move $SIn $SOut)  
    ( (set-det) print-forced-or-lost-notice 
      (legal $Move $SIn $SOut)))
  (= (random-non-losing $Moves $Move $SIn $SOut)  
    (random-select $Move1 $Moves $RestMoves)
    (nl-or-next $Move1 $RestMoves $Move $SIn $SOut))


  (= (nl-or-next $Move $_ $Move $SIn $SOut)  
    (cautious-move $Move $SIn $SOut)
    (set-det))
  (= (nl-or-next $_ $Moves $Move $SIn $SOut)  
    (random-non-losing $Moves $Move $SIn $SOut)
    (set-det))


  (= (legal-moves $Moves $SIn)  
    (setof $Move 
      (^ $S1 
        (legal $Move $SIn $S1)) $Moves))

;
; ============================================================

;
; A PASS_MOVE isn't really a legal move, it just transfers the
;
; player on move.


  (= (pass-move $SIn $SOut)  
    (control $P1 $SIn)
    (transfer-control $_ $SIn $SOut)
    (control $P2 $SOut)
    (format "Passing, control has now transferred from ~p to ~p~n" 
      (:: $P1 $P2)))



;
; A VICTOR_MOVE is one which wins the game immediately.

  (= (victor-move $Move $SIn $SOut)  
    (control $Player $SIn)
    (legal $Move $SIn $SOut)
    (game-outcome $Player $SOut))

;
; An ENDGAME_MOVE is one which ends the game immediately.

  (= (endgame-move $Move $SIn $SOut)  
    (legal $Move $SIn $SOut)
    (game-over $SOut))


;
; A MATE_MOVE is one which does not allow a cautious_move response.
;
; That is, either it ends the game immediately, or for all moves
;
; of the opponent, we have a victory move.

  (= (mate-move $Move $SIn $SOut)  
    (legal $Move $SIn $SOut)
    (not (cautious-move $M2 $SOut $_)))


;
; A THREATEN_MOVE is one which threatens victory, if the opponent doesn't respond
;
; to stop it. (This is like a CHECK in chess).


  (= (threaten-move $Move $SIn $SOut)  
    (legal $Move $SIn $SOut)
    (det-if-then 
      (transfer-control $_ $SOut $S1) 
      (victor-move $M2 $S1 $_)))


;
; An ENOUGH_ROPE_MOVE is one which allows the opponent to play a move
;
; which allows us to win next.


  (= (enough-rope-move $Move $SIn $SOut)  
    (legal $Move $SIn $SOut)
    (legal $M2 $SOut $S1)
    (victor-move $M3 $S1 $_))


