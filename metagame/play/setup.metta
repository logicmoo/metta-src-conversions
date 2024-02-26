;
; 

;
; 

;
; 

;
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
    (setup-position empty $Squares $SIn $SOut) 
    ( (set-det) (make-empty $Squares $SIn $SOut)))
; 

  (= 
    (setup-position $Player $Assignments $SIn $SOut) 
    (place-pieces-on-squares $Assignments $Player $SIn $SOut))
; 




  (= 
    (setup-com $_ $SIn $SOut) 
    (setup $SIn $SOut))
; 


;
; 

;
; 

;
; 



  (= 
    (setup $SIn $SOut) 
    ( (menu-command "~nEnter <setup> command ('help.'  gives  more information)~n" setup 
        (:: $Done $SIn $S1)) 
      (set-det) 
      (continue-setup $Done $SIn $S1 $SOut)))
; 

  (= 
    (setup $SIn $SOut) 
    ( (format "~nI did not understand your command. Please try again!~n" Nil) (setup $SIn $SOut)))
; 


;
; 

;
; 

;
; 

;
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
    (continue-setup $Move $SIn $S1 $S1) 
    ( (nonvar $Move) 
      (= $Move done) 
      (set-det)))
; 

  (= 
    (continue-setup $Move $SIn $S1 $Nothing) 
    ( (nonvar $Move) 
      (= $Move abort) 
      (set-det)))
; 

  (= 
    (continue-setup $Move $SIn $S1 $S1) 
    ( (nonvar $Move) 
      (nonvar $S1) 
      (set-det)))
; 

  (= 
    (continue-setup $Move $SIn $S1 $SOut) 
    ( (nonvar $S1) 
      (set-det) 
      (setup $S1 $SOut)))
; 
	
  (= 
    (continue-setup $Move $SIn $S1 $SOut) 
    (setup $SIn $SOut))
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
    (put-setup $_ $SIn $SOut $Row $Col $Color $Type) 
    ( (piece $Piece 
        (:: $Color $Type) Nil) 
      (with-alpha-squares (gsquare $Square (:: ( $Row , $Col )) Nil)) 
      (place-piece $Piece $Square $SIn $SOut)))
; 



;
; 


  (= 
    (initprom-setup $_ $SIn $SOut $Row $Col $Color $Type) 
    ( (piece $Piece 
        (:: $Color $Type) Nil) 
      (with-alpha-squares (gsquare $Square (:: ( $Row , $Col )) Nil)) 
      (put-stage init-promote $SIn $S1) 
      (add-in 
        (opponent-promotes $Piece $Square) $S1 $SOut)))
; 



;
; 


  (= 
    (hand-setup $_ $SIn $SOut $HandColor $Color $Type) 
    ( (piece $Piece 
        (:: $Color $Type) Nil) 
      (player-color $Player $HandColor) 
      (put-in-hand $Piece $Player $SIn $SOut)))
; 


;
; 


  (= 
    (empty-setup $_ $SIn $SOut $Row $Col) 
    ( (with-alpha-squares (gsquare $Square (:: ( $Row , $Col )) Nil)) (set-empty $Square $SIn $SOut)))
; 

	  
;
; 


  (= 
    (move-setup $_ $SIn $SOut $Row1 $Col1 $Row2 $Col2) 
    ( (with-alpha-squares (, (gsquare $Square1 (:: ( $Row1 , $Col1 )) Nil) (gsquare $Square2 (:: ( $Row2 , $Col2 )) Nil))) (move-piece $_ $Square1 $Square2 $SIn $SOut)))
; 

	

;
; 


  (= 
    (stage-setup $_ $SIn $SOut $Stage) 
    (put-stage $Stage $SIn $SOut))
; 


;
; 


  (= 
    (control-setup $_ $SIn $SOut $Color) 
    ( (player-color $Player $Color) (put-control $Player $SIn $SOut)))
; 


;
; 


  (= 
    (clear-setup $_ $SIn $SOut) 
    (make-empty-board $SIn $SOut))
; 

	

;
; 


  (= 
    (add-setup $_ $SIn $SOut) 
    ( (format "Enter a property to be added: ~n" Nil) 
      (read $Prop) 
      (add-in $Prop $SIn $SOut)))
; 


;
; 


  (= 
    (del-setup $_ $SIn $SOut) 
    ( (format "Enter a property to be deleted: ~n" Nil) 
      (read $Prop) 
      (del-in $Prop $SIn $SOut)))
; 



;
; 

;
; 


  (= 
    (restore-setup $_ $SIn $SOut $Name) 
    (restore-state $Name $SOut))
; 

	
;
; 


  (= 
    (checkpoint-setup $_ $SIn $_) 
    (checkpoint-state $SIn))
; 


;
; 

  (= 
    (checkpoint-setup $_ $SIn $_ $Name) 
    (checkpoint-state $Name $SIn))
; 





;
; 


  (= 
    (done_setup  done $SIn $SIn) True)
; 


;
; 


  (= 
    (abort_setup  abort $SIn $_) True)
; 


;
; 


  (= 
    (display-setup $Move $SIn $_) 
    ( (format "~nCurrent State:  ~n" Nil) (print-state $SIn)))
; 




  (= 
    (help-setup $_ $_ $_) 
    (help-setup))
; 


  (= 
    (help-setup) 
    (format "\nModifying Board Setup\n---------------------\nmove <x1> <y1> <x2> <y2>         => transfer piece to different square\nput <x> <y> <color> <piece>      => put a piece on square (<x>,<y>)\ninitprom <x> <y> <color> <piece> => opponent must promote piece on square\nempty <row> <col>\t\t => empty square (<row>,<col>)\nhand <Hcolor> <color> <piece>    => put piece in <Hcolor>'s hand\nclear\t\t\t\t => makes all the square empty\ncontrol <color>\t\t\t => puts player <color> in control\nstage <stage>\t\t\t => sets current stage to <stage>\nadd/del\t\t\t\t => add or delete a property from state\ndisplay\t\t\t\t => prints the board\ndone\t\t\t\t => exits setup stage with current setup\nabort\t\t\t\t => exits setup stage, abandon changes\ncheckpoint <n>.                  => record state under name <n> \nrestore <n>.                     => set state to that checkpointed as <n> \n\nExamples:\n\nmove e 2 e 4.\nput e 2 white king.\nhand white black queen.\ncontrol white.\nstage assign.\ninitprom e 2 black bishop. \ndone.\n" Nil))
; 




