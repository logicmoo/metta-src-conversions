;
; 

;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  !(my-ensure-loaded (library tokenizer))
; 

  !(my-ensure-loaded (library grammar))
; 


;
; 


  (= 
    (move-notation $ComplexMove $Notation) 
    ( (det-if-then-else 
        (var $ComplexMove) set-parsing-mode True) (move-notation $ComplexMove $Notation Nil)))
; 



;
; 


  (= 
    (-->  
      (move_notation  $M) 
      (,  
        (prelims  $M $Pre) 
        (,  
          (main  $Pre ()) period))) True)
; 



;
; 

;
; 

  (= 
    (-->  
      (prelims  
        (Cons  $P $Out) $Out) 
      (init_promote  $P)) True)
; 

  (= 
    (-->  
      (prelims  
        (Cons  end_assign $Rest) $Rest) ()) True)
; 

  (= 
    (-->  
      (prelims  $Rest $Rest) ()) True)
; 



;
; 

;
; 

;
; 

  (= 
    (-->  
      (init_promote  
        (opponent_promote  () () ())) ()) True)
; 

  (= 
    (-->  
      (init_promote  
        (opponent_promote  $Sq $OldPiece $NewPiece)) 
      (,  
        (promote) 
        (,  
          (gsquare  $Sq) 
          (,  
            (piece  $NewPiece) 
            (,  
              (;) line))))) True)
; 



;
; 

  (= 
    (-->  
      (consider_promote  
        (Cons  $T $Sel) $Rest) 
      (,  
        (attempt_promote  $T) 
        (select_promote  $Sel $Rest))) True)
; 


  (= 
    (-->  
      (select_promote  
        (Cons  
          (promote_select  $Square $OldPiece $OldPiece) $Rest) $Rest) ()) True)
; 

  (= 
    (-->  
      (select_promote  
        (Cons  
          (promote_select  $Square $OldPiece $NewPiece) $Rest) $Rest) 
      (,  
        (;) 
        (,  line 
          (,  
            (promote) 
            (,  
              (gsquare  $Square) 
              (piece  $NewPiece)))))) True)
; 

  (= 
    (-->  
      (select_promote  $Rest $Rest) ()) True)
; 




;
; 

;
; 

;
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
    (-->  
      (attempt_promote  
        (try_promote  $Square $OldPiece ())) ()) True)
; 

  (= 
    (-->  
      (attempt_promote  
        (try_promote  $Square $OldPiece $OldPiece)) ()) True)
; 

;
; 

  (= 
    (-->  
      (attempt_promote  
        (try_promote  $Square $OldPiece $NewPiece)) 
      (,  
        (;) 
        (,  
          (promote) 
          (,  
            (gsquare  $Square) 
            (piece  $NewPiece))))) True)
; 



;
; 

;
; 

  (= 
    (-->  
      (main  
        (Cons  $P $Out) $Out) 
      (placing  $P)) True)
; 

  (= 
    (-->  
      (main  $In $Out) 
      (,  
        (transfers  $In $T) 
        (consider_promote  $T $Out))) True)
; 



;
; 

  (= 
    (-->  
      (transfers  $In $Out) 
      (,  
        (transfer  $In $T) 
        (continued_transfers  $T $Out))) True)
; 


;
; 

;
; 

;
; 

  (= 
    (-->  
      (continued_transfers  $In $In) ()) True)
; 

  (= 
    (-->  
      (continued_transfers  
        (Cons  end_continues $Rest) $Rest) ()) True)
; 

  (= 
    (-->  
      (continued_transfers  $In $Out) 
      (,  
        (;) 
        (,  line 
          (transfers  $In $Out)))) True)
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
    (-->  
      (transfer  
        (Cons  $Move $Capture) $Rest) 
      (,  
        (moving  $Move) 
        (capture  $Capture $Rest))) True)
; 



  (= 
    (-->  
      (placing  $M) 
      (,  
        { (= $M 
            (place  $Piece $Player $Sq)) } 
        (,  
          (piece  $Piece) 
          (,  
            (paren_color_player  $Player) 
            (,  
              (->) 
              (gsquare  $Sq)))))) True)
; 

  (= 
    (-->  
      (placing  $M) 
      (,  
        { (= $M 
            (assign  $Piece $Player $Sq)) } 
        (,  
          (piece  $Piece) 
          (,  
            (paren_color_player  $Player) 
            (,  
              (->) 
              (gsquare  $Sq)))))) True)
; 


;
; 

;
; 

;
; 

  (= 
    (-->  
      (moving  
        (move  $Piece $Player $From $To)) 
      (,  
        (piece  $Piece) 
        (,  
          (gsquare  $From) 
          (,  
            (->) 
            (gsquare  $To))))) True)
; 


  (= 
    (-->  
      (capture  $In $Out) 
      (null_capture  $In $Out)) True)
; 

  (= 
    (-->  
      (capture  $In $Out) 
      (real_capture  $In $Out)) True)
; 


  (= 
    (-->  
      (null_capture  $X $X) ()) True)
; 


  (= 
    (-->  
      (real_capture  
        (Cons  $C $Cs) $Rest) 
      (,  
        (x) 
        (,  
          (simp_capture  $C) 
          (capture  $Cs $Rest)))) True)
; 


  (= 
    (-->  
      (simp_capture  $M) 
      (remove  $M)) True)
; 

  (= 
    (-->  
      (simp_capture  $M) 
      (possess  $M)) True)
; 


;
; 

  (= 
    (-->  
      (remove  
        (capture  remove $Caps)) 
      (capture_effects_list  $Caps)) True)
; 


;
; 

  (= 
    (-->  
      (possess  
        (capture  
          (possess  $Player) $Caps)) 
      (,  
        (capture_effects_list  $Caps) 
        (,  
          (/) 
          (paren_color_player  $Player)))) True)
; 


  (= 
    (-->  
      (capture_effects_list  ()) ()) True)
; 

  (= 
    (-->  
      (capture_effects_list  
        (Cons  $C $Caps)) 
      (,  
        (capture_effect  $C) 
        (capture_effects_list  $Caps))) True)
; 


;
; 

;
; 

  (= 
    (-->  
      (capture_effect  $C) 
      (,  
        { (captured  $C $Piece $Square) } 
        (,  
          (piece  $Piece) 
          (gsquare  $Square)))) True)
; 




;
; 

;
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
    (-->  
      (piece  $P) 
      (,  
        { (,  
            (piece_struct_name  $P $Name) 
            (piece_struct_owner  $P $O)) } 
        (,  
          (color_player  $O) 
          (piece_name  $Name)))) True)
; 



;
; 

;
; 

  (= 
    (-->  
      (paren_color_player  $P) 
      (,  
        (() 
        (,  
          (color_player  $P) 
          ())))) True)
; 


  (= 
    (-->  
      (color_player  player) 
      (white)) True)
; 

  (= 
    (-->  
      (color_player  opponent) 
      (black)) True)
; 


;
; 

;
; 



  (= 
    (color_player  white player) True)
; 

  (= 
    (color_player  black opponent) True)
; 



  (= 
    (player_color  player white) True)
; 

  (= 
    (player_color  opponent black) True)
; 



;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (print-read-move-from-file $File) 
    ( (format "~nReading move from file~n" Nil) 
      (read-move-from-file-to-list $File $Move) 
      (format "~nRead move from file~n" Nil) 
      (set-parsing-mode) 
      (format "~nParsing move in parse mode~n" Nil) 
      (move-notation $ComplexMove $Move Nil) 
      (set-printing-mode) 
      (format "~nParsing move in print mode~n" Nil) 
      (move-notation $ComplexMove $Move1 Nil) 
      (format "~nPrinting move~n" Nil) 
      (print-tokens $Move1)))
; 



  (= 
    (read-move-from-file-to-list $File $Move) 
    (read-tokens-from-file $File $Move))
; 


	
;
; 

;
; 

;
; 


  (= 
    (move-notation-string $Move $String) 
    ( (var $String) 
      (set-det) 
      (move-notation $Move $Tokens) 
      (print-tokens-to-string $Tokens $String)))
; 

  (= 
    (move-notation-string $Move $String) 
    ( (var $Move) 
      (read-tokens-from-string $String $Tokens) 
      (move-notation $Move $Tokens)))
; 


	

