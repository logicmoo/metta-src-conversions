;
; 

;
; 

;
; 

;
; 


;
; 


;
; 

;
; 

;
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
    (matches $Descr $Piece) 
    ( (piece-description $Descr $Player_Gen $Piece_Gen) 
      (piece-struct $Piece $Name $Player) 
      (matches-player $Player_Gen $Player) 
      (matches-name $Piece_Gen $Name)))
; 




  (= 
    (matches-player any-player $Player) 
    (player-role $Player))
; 

  (= 
    (matches_player  player player) True)
; 

  (= 
    (matches_player  opponent opponent) True)
; 


;
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
    (matches-name any-piece $Name) 
    (current-game-piece-name $Name))
; 

  (= 
    (matches-name 
      (Cons  $H $T) $Name) 
    (member $Name 
      (Cons  $H $T)))
; 
	


