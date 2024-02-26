;                                           (convert_to_metta_file  setup $_109322 metagame/play/setup.pl metagame/play/setup.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; setup.pl
;
; An interface for setting up the board as we choose.
;
; We could modify a game file, but this would not allow
;
; other than symmetric games.


  (= (setup-position empty $Squares $SIn $SOut)  
    (set-det)
    (make-empty $Squares $SIn $SOut))
  (= (setup-position $Player $Assignments $SIn $SOut)  
    (place-pieces-on-squares $Assignments $Player $SIn $SOut))



  (= (setup-com $_ $SIn $SOut)  
    (setup $SIn $SOut))

;
; ======================================================================
;
; Setting up a position
;
; ======================================================================


  (= (setup $SIn $SOut)  
    (menu-command "~nEnter <setup> command ('help.'  gives  more information)~n" setup 
      (:: $Done $SIn $S1))
    (set-det)
    (continue-setup $Done $SIn $S1 $SOut))
  (= (setup $SIn $SOut)  
    (format "~nI did not understand your command. Please try again!~n" Nil)
    (setup $SIn $SOut))

;
; CONTINUE_SETUP(?Move,+SIn,+S1,-SOut).
;
; Ensure that we are done setting up, else back to the menu.
;
; If Move and S1 are bound, we accept S1 as the result.
;
; If only S1 is bound, we accept this as a new current state,
;
; and use it to get the final move.
;
; If neither, then some other command didn't do any work,
;
; and we start again from our original state to get the move.
;
; 

  (= (continue-setup $Move $SIn $S1 $S1)  
    (nonvar $Move)
    (= $Move done)
    (set-det))
  (= (continue-setup $Move $SIn $S1 $Nothing)  
    (nonvar $Move)
    (= $Move abort)
    (set-det))
  (= (continue-setup $Move $SIn $S1 $S1)  
    (nonvar $Move)
    (nonvar $S1)
    (set-det))
  (= (continue-setup $Move $SIn $S1 $SOut)  
    (nonvar $S1)
    (set-det)
    (setup $S1 $SOut))	
  (= (continue-setup $Move $SIn $S1 $SOut)  
    (setup $SIn $SOut))


;
; ----------------------------------------
;
; SETUP menu commands
;
; ----------------------------------------

;
; put e 2 black bishop

  (= (put-setup $_ $SIn $SOut $Row $Col $Color $Type)  
    (piece $Piece 
      (:: $Color $Type) Nil)
    (with-alpha-squares (gsquare $Square (:: ( $Row , $Col )) Nil))
    (place-piece $Piece $Square $SIn $SOut))


;
; initprom e 2 black bishop

  (= (initprom-setup $_ $SIn $SOut $Row $Col $Color $Type)  
    (piece $Piece 
      (:: $Color $Type) Nil)
    (with-alpha-squares (gsquare $Square (:: ( $Row , $Col )) Nil))
    (put-stage init-promote $SIn $S1)
    (add-in 
      (opponent-promotes $Piece $Square) $S1 $SOut))


;
; hand white white queen

  (= (hand-setup $_ $SIn $SOut $HandColor $Color $Type)  
    (piece $Piece 
      (:: $Color $Type) Nil)
    (player-color $Player $HandColor)
    (put-in-hand $Piece $Player $SIn $SOut))

;
; empty e 5

  (= (empty-setup $_ $SIn $SOut $Row $Col)  
    (with-alpha-squares (gsquare $Square (:: ( $Row , $Col )) Nil))
    (set-empty $Square $SIn $SOut))
	  
;
; move e 2 e 4

  (= (move-setup $_ $SIn $SOut $Row1 $Col1 $Row2 $Col2)  
    (with-alpha-squares (, (gsquare $Square1 (:: ( $Row1 , $Col1 )) Nil) (gsquare $Square2 (:: ( $Row2 , $Col2 )) Nil)))
    (move-piece $_ $Square1 $Square2 $SIn $SOut))
	

;
; stage assign

  (= (stage-setup $_ $SIn $SOut $Stage)  
    (put-stage $Stage $SIn $SOut))

;
; control black

  (= (control-setup $_ $SIn $SOut $Color)  
    (player-color $Player $Color)
    (put-control $Player $SIn $SOut))

;
; clear

  (= (clear-setup $_ $SIn $SOut)  
    (make-empty-board $SIn $SOut))
	

;
; add

  (= (add-setup $_ $SIn $SOut)  
    (format "Enter a property to be added: ~n" Nil)
    (read $Prop)
    (add-in $Prop $SIn $SOut))

;
; del

  (= (del-setup $_ $SIn $SOut)  
    (format "Enter a property to be deleted: ~n" Nil)
    (read $Prop)
    (del-in $Prop $SIn $SOut))


;
; RESTORE <name>
;
; Set current state to be a previously named checkpoint state.

  (= (restore-setup $_ $SIn $SOut $Name)  
    (restore-state $Name $SOut))
	
;
; CHECKPOINT

  (= (checkpoint-setup $_ $SIn $_)  
    (checkpoint-state $SIn))

;
; CHECKPOINT <NAME>
  (= (checkpoint-setup $_ $SIn $_ $Name)  
    (checkpoint-state $Name $SIn))




;
; done

  (= (done_setup  done $SIn $SIn)  True)

;
; abort: abandon changes and back to move menu.

  (= (abort_setup  abort $SIn $_)  True)

;
; display

  (= (display-setup $Move $SIn $_)  
    (format "~nCurrent State:  ~n" Nil)
    (print-state $SIn))



  (= (help-setup $_ $_ $_)  
    (help-setup))

  (= (help-setup)  
    (format "\nModifying Board Setup\n---------------------\nmove <x1> <y1> <x2> <y2>         => transfer piece to different square\nput <x> <y> <color> <piece>      => put a piece on square (<x>,<y>)\ninitprom <x> <y> <color> <piece> => opponent must promote piece on square\nempty <row> <col>\t\t => empty square (<row>,<col>)\nhand <Hcolor> <color> <piece>    => put piece in <Hcolor>'s hand\nclear\t\t\t\t => makes all the square empty\ncontrol <color>\t\t\t => puts player <color> in control\nstage <stage>\t\t\t => sets current stage to <stage>\nadd/del\t\t\t\t => add or delete a property from state\ndisplay\t\t\t\t => prints the board\ndone\t\t\t\t => exits setup stage with current setup\nabort\t\t\t\t => exits setup stage, abandon changes\ncheckpoint <n>.                  => record state under name <n> \nrestore <n>.                     => set state to that checkpointed as <n> \n\nExamples:\n\nmove e 2 e 4.\nput e 2 white king.\nhand white black queen.\ncontrol white.\nstage assign.\ninitprom e 2 black bishop. \ndone.\n" Nil))



