;                                           (convert-to-metta-file  history $_340778 metagame/play/history.pl metagame/play/history.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;; history.pl
;
; ;;
;
; ;; Interface routines:
;
; ;; -------------------
;
; ;; From top menu:
;
; ;; start <PosName>: Start game in pos checkpointed as <PosName>
;
; ;; save <F>: Save record of current game as <F>.rec
;
; ;; load <F>: Load record of game <F>.rec
;
; ;;
;
; ;; From move menu:
;
; ;; next <N>: Advance <N> positions in current game history.
;
; ;; prev <N>: Regress <N> positions in current game history.
;
; ;; next: next 1.
;
; ;; prev: prev 1.
;
; ;; clear history: Delete *all* history entries and saved positions.


  (= help-history  
    (help-history-overview help-history-top help-history-com))


  (= help-history-overview  
    (format "\nGame History Management\n-----------------------\nThese routines allow you to review the history of the current\ngame. To use them, you must set the white player to be human,\notherwise the program will begin playing from the chosen position\nand there will be no chance to review the past game.  \n\nIn addition, see the menu: EXAMINING AND MODIFYING STATE OF GAME,\nwhich allows you to save and restore important positions even when in\nthe middle of a game.  Note however that these positions will not\nnecessarily be hooked into the history mechanisms discussed here. \n" Nil))


  (= help-history-top  
    (format "\nFrom top menu:\nsave <F>: Save record of current game as <F>.rec\nload <F>: Load record of game <F>.rec as current history.  \nstart <PosName>:  Start game in pos checkpointed as <PosName>\n" Nil))


  (= help-history-com  
    (format "\nFrom move menu: \nnext <N>:  Advance <N> positions in current game history.\nprev <N>:  Regress <N> positions in current game history.\nnext: next 1.\nprev: prev 1.\nclear history: Delete *all* history entries and saved positions.\n\nNote the playing a new move erases whatever moves may have come after \nin the present game record. \n" Nil))


  (= (next-com $M $SIn $SOut)  
    (forward-hist 1 $SOut))

  (= (next-com $M $SIn $SOut $N)  
    (forward-hist $N $SOut))


  (= (prev-com $M $SIn $SOut)  
    (reverse-hist 1 $SOut))

  (= (prev-com $M $SIn $SOut $N)  
    (reverse-hist $N $SOut))


  (= (clear-com $_ $_ $_ history)  clear-history)

;
; Saving and loading games


  (= (save-top $Game)  
    (print-game-record-to-file $Game))


  (= (save-com $_ $_ $_ $Game)  
    (print-game-record-to-file $Game))


  (= (load-top $Game)  
    (read-game-record-from-file $Game))


  (= (load-com $_ $_ $_ $Game)  
    (read-game-record-from-file $Game))


;
; ================================================================================


  !(dynamic (, (/ follows 3) (/ current-pos-name 1)))



  (= (forward-hist $N $SOut)  
    (current-pos-name $NameIn)
    (forward-name $N $NameIn $NameOut)
    (checkpoint $NameOut $SOut)
    (set-current-pos-name $NameOut)
    (print-state $SOut)
    (last-move $LastMove)
    (print-notation $LastMove))


  (= (reverse-hist $N $SOut)  
    (current-pos-name $NameIn)
    (forward-name $N $NameOut $NameIn)
    (checkpoint $NameOut $SOut)
    (set-current-pos-name $NameOut)
    (print-state $SOut)
    (last-move $LastMove)
    (print-notation $LastMove))



  (= (forward-name  0 $NameIn $NameIn)  True)
  (= (forward-name $N $NameIn $NameOut)  
    (> $N 0)
    (is $N1 
      (- $N 1))
    (follows-history $NameIn $Name1)
    (forward-name $N1 $Name1 $NameOut))


  (= (follows-history $N1 $N2)  
    (follows $N1 $_ $N2))	
	
  (= (follows-history $N1 $Move $N2)  
    (follows $N1 $Move $N2))	



  (= (initialize-history $SIn)  
    (checkpoint-state init $SIn)
    (set-current-pos-name init))


;
; CURRENT_POSITION(-Pos)
;
; Pos is the current position in the history.

  (= (current-position $Pos)  
    (current-pos-name $Name)
    (checkpoint $Name $Pos))

;
; LAST_MOVE(-Move)
;
; Returns the internal representation (if any) of the move made prior
;
; to this one in the current position.

  (= (last-move $Move)  
    (current-pos-name $Name)
    (follows-history $_ $Move $Name))



  (= (set-current-pos-name $Name)  
    ( (remove-all-atoms  &self 
        (current-pos-name  $_)) (add-atom  &self (current-pos-name  $Name))))


  (= (change-current-pos-name $Name $Next)  
    ( (remove-atom  &self 
        (current-pos-name  $Name)) (add-atom  &self (current-pos-name  $Next))))



  (= (set-follows-history $Name $Move $Next)  
    ( (clear-history-after $Name) (add-atom  &self (follows  $Name $Move $Next))))


  (= (add-state-to-history $Move $State)  
    (checkpoint-state-gensym $State $Next)
    (update-history $Move $Next))


  (= (update-history $Move $Next)  
    (change-current-pos-name $Name $Next)
    (set-follows-history $Name $Move $Next))
	

  (= clear-history  
    ( (remove-all-atoms  &self 
        (checkpoint  $_ $_)) (remove-all-atoms  &self (follows  $_ $_ $_))))


  (= clear-history-visible  
    (or 
      (, 
        (clear-history-after $Name) fail) True))


  (= (clear-history-after $Name)  
    (det-if-then-else 
      (remove-atom  &self 
        (follows  $Name $_ $Next)) 
      (, 
        (remove-atom  &self 
          (checkpoint  $Next $_)) 
        (clear-history-after $Next)) True))
	
;
; Unifies State with each state occuring in the current game
;
; order.

  (= (history-state $State)  
    (checkpoint init $State))
  (= (history-state $State)  
    (follows-history $Name1 $Name2)
    (checkpoint $Name2 $State))


  (= (restore-state $N $State)  
    (det-if-then-else 
      (, 
        (current-predicate checkpoint $_) 
        (checkpoint $N $State)) 
      (, 
        (checkpoint $N $State) 
        (format "~nState named: <~w> now current state~n" 
          (:: $N))) 
      (format "~nError: No state: <~w> has been is checkpointed~n" 
        (:: $N))))
	
	

  (= (checkpoint-state $State)  
    (checkpoint-state-gensym $State $Name))


  (= (checkpoint-state-gensym $State $Name)  
    (gensym checkpoint $Name)
    (checkpoint-state $Name $State))
	

  (= (checkpoint-state $Name $State)  
    ( (remove-all-atoms  &self 
        (checkpoint  $Name $_)) 
      (add-atom  &self 
        (checkpoint  $Name $State)) 
      (format "~nState checkpointed under index: <~w>~n" 
        (:: $Name))))


;
; ================================================================================
;
; Saving and loading game records
;
; ================================================================================
;
; Game Record file format:
;
; A saved game record consists of a sequence of grammatical move descriptions,
;
; POSSIBLY ABBREVIATED (so long as they are unambiguous). This format is
;
; just what would have been read from a keyboard to enter the moves from the
;
; human interface.
;
; The file may also contain arbitrary comments, prefaced as usual by ';'.
;
; The ability to abbreviate game records makes this format extremely flexible.
;
; 
;
; One caution: The program replays the game in order to save or load the file,
;
; thus if move generation for a given game is slow this takes noticeable time.


;
; If MoveString is instantiated, will backtrack over moves
;
; in the game which are completions of MoveString.

  (= (move-string $Move $MoveString)  
    (follows-history $Name1 $Name2)
    (checkpoint $Name1 $State1)
    (checkpoint $Name2 $State2)
    (det-if-then 
      (completed-move $MoveString $Move $State1 $State2) True))


  (= (print-next-move $MoveString)  
    ( (move-string $Move $MoveString) 
      (print-tokens $MoveString) nl))



  (= (read-next-move $MoveString)  
    (read-keyboard-tokens $MoveString)
    (current-position $State1)
    (det-if-then 
      (completed-move $MoveString $Move $State1 $State2) True)
    (add-state-to-history $Move $State2))


  (= print-game-record  
    (or 
      (, 
        (print-next-move $_) fail) True)) 


  (= (get-initialize-history $In)  
    (get-initial-state $In)
    (initialize-history $In))



  (= read-game-record  
    ( (get-initialize-history $In) read-record-moves))


  (= read-record-moves  
    ( (read-next-move $_) 
      (set-det) read-record-moves))
  (= read_record_moves  True)



;
; PRINT_GAME_RECORD_TO_FILE(+File)
;
; Outputs a game to file File.rec.

  (= (print-game-record-to-file $File)  
    (sys-suffixed-filename $File record $GameFile)
    (format "~nWriting game record to file: ~w~n" 
      (:: $GameFile))
    (with-output-file $GameFile append print-game-record))


;
; PRINT_GAME_RECORD_TO_FILE(+File)
;
; Outputs a game to file File.rec.

  (= (read-game-record-from-file $File)  
    ( (sys-suffixed-filename $File record $GameFile) 
      (format "~nReading game record from file: ~w~n" 
        (:: $GameFile)) 
      (see $GameFile) read-game-record seen))
; ; write_old_seed,



