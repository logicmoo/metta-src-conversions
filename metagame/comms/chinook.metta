;
; 

;
; 

;
; 

;
; 



  !(ensure-loaded (library pipes))
; 



  (= 
    (create-chinook) 
    ( (command-from-args 
        (:: nice -10 chinook) $Command) (interface-record-streams $Command $InStream $OutStream)))
; 



  (= 
    (tell-chinook $Statement) 
    (tell-outstream $Statement))
; 



  (= 
    (tellc $S) 
    (tell-chinook $S))
; 



  (= 
    (seem) 
    ( (global instream $I) (set-input $I)))
; 




;
; 



  (= 
    (chinook-quit) 
    ( (tell-chinook (:: q)) (interface-close-streams)))
; 



  (= 
    (chinook-choose $Player $Move $SIn $SOut) 
    ( (update-chinook $Player $SIn) 
      (current-input $OldStream) 
      (global instream $I) 
      (set-input $I) 
      (det-if-then-else 
        (chinook-move $Player $_ $MoveString) 
        (, 
          (set-input $OldStream) 
          (completed-move $MoveString $Move $SIn $SOut) 
          (print-choice $Move $SIn $SOut)) 
        (, 
          (set-input $OldStream) 
          (tell-chinook (:: O)) 
          (chinook-choose $Player $Move $SIn $SOut)))))
; 



  !(add-global handicap 0)
; 



  (= 
    (setup-chinook) 
    ( (global handicap $H) 
      (tell-chinook (:: i)) 
      (config $H)))
; 



;
; 

    


  (= 
    (config  0) True)
; 
 
  (= 
    (config 1) 
    (tell-chinook rb6))
; 

  (= 
    (config 2) 
    (tell-chinook rb6rd6))
; 

  (= 
    (config 3) 
    (tell-chinook rb6rd6rf6))
; 

  (= 
    (config 4) 
    (tell-chinook rb6rd6rf6rh6))
; 

  (= 
    (config 8) 
    ( (tell-chinook rb6rd6rf6rh6) (tell-chinook ra7rc7re7rg7)))
; 





  (= 
    (update-chinook $Role $SIn) 
    ( (move-count $N $SIn) 
      (det-if-then-else 
        (< $N 2) setup-chinook True) 
      (det-if-then-else 
        (last-move $Move) 
        (, 
          (meta-to-ch $Move $ChMove) 
          (format "To Chinook: I played ~w~n" 
            (:: $ChMove)) 
          (concat m $ChMove $TellMove) 
          (tell-chinook (:: $TellMove))) True) 
      (tell-chinook-move)))
; 



  (= 
    (tell-chinook-move) 
    (tell-chinook (:: g)))
; 


	

  (= 
    (meta-to-ch $Move $ChMove) 
    ( (ch-notation $Move $String) (concat-list $String $ChMove)))
; 




  (= 
    (chinook-move $Player $Number $Move) 
    ( (chinook-read $Number $FullMove) 
      (set-det) 
      (format $FullMove Nil) 
      (player-indent $Player $Indent) 
      (append $Indent $Move0 $FullMove) 
      (ch-to-meta $Move0 $Move)))
; 

;
; 

;
; 

	



  (= 
    (chinook-read $Number $Move) 
    ( (append-list 
        (:: "I move") $Pattern) 
      (format "Looking for pattern: ~w~n" 
        (:: $Pattern)) 
      (found $Pattern) 
      (read-keyboard-tokens (:: $Number $_)) 
      (read-line $Move)))
; 



  (= 
    (player_indent  player " ") True)
; 

  (= 
    (player_indent  opponent "  ... ") True)
; 



;
; 

;
; 




  (= 
    (ch_to_meta  () ()) True)
; 

  (= 
    (ch-to-meta 
      (Cons  $X 
        (Cons  $Y $Rest)) 
      (Cons  $XM 
        (Cons  $YM $RestM))) 
    ( (conv-square $X $Y $XM $YM) (ch-to-meta $Rest $RestM)))
; 



  (= 
    (conv-square $X $Y $XM $YM) 
    ( (conv-x $X $XM1) 
      (conv-y $Y $YM1) 
      (name $XM 
        (:: $XM1)) 
      (name $YM 
        (:: $YM1))))
; 




  (= 
    (conv-x $X $XM) 
    (is $XM 
      (+ 
        (- "h" $X) "a")))
; 


  (= 
    (conv-y $Y $YM) 
    (is $YM 
      (+ 
        (- "8" $Y) "1")))
; 





;
; 


;
; 


  (= 
    (ch-notation $ComplexMove $Notation) 
    ( (det-if-then-else 
        (var $ComplexMove) set-parsing-mode True) (ch-notation $ComplexMove $Notation Nil)))
; 




  (= 
    (-->  
      (ch_notation  $M) 
      (,  
        (prelims  $M $Pre) 
        (ch_main  $Pre ()))) True)
; 



;
; 

  (= 
    (-->  
      (ch_consider_promote  
        (Cons  $T $Sel) $Rest) 
      (,  
        (ch_attempt_promote  $T) 
        (ch_select_promote  $Sel $Rest))) True)
; 


  (= 
    (-->  
      (ch_select_promote  
        (Cons  
          (promote_select  $_ $_ $_) $Rest) $Rest) ()) True)
; 

  (= 
    (-->  
      (ch_select_promote  $Rest $Rest) ()) True)
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
      (ch_attempt_promote  
        (try_promote  $Square $OldPiece ())) ()) True)
; 

  (= 
    (-->  
      (ch_attempt_promote  
        (try_promote  $Square $OldPiece $NewPiece)) ()) True)
; 



;
; 

;
; 

  (= 
    (-->  
      (ch_main  $In $Out) 
      (,  
        (ch_first_transfer  $In $First) 
        (,  
          (ch_continued_transfers  $First $T) 
          (ch_consider_promote  $T $Out)))) True)
; 



;
; 

  (= 
    (-->  
      (ch_transfers  $In $Out) 
      (,  
        (ch_transfer  $In $T) 
        (ch_continued_transfers  $T $Out))) True)
; 


;
; 

;
; 

;
; 

  (= 
    (-->  
      (ch_continued_transfers  $In $In) ()) True)
; 

  (= 
    (-->  
      (ch_continued_transfers  
        (Cons  end_continues $Rest) $Rest) ()) True)
; 

  (= 
    (-->  
      (ch_continued_transfers  $In $Out) 
      (ch_transfers  $In $Out)) True)
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
      (ch_transfer  
        (Cons  $Move $Capture) $Rest) 
      (,  
        (ch_moving  $Move) 
        (ch_capture  $Capture $Rest))) True)
; 


  (= 
    (-->  
      (ch_first_transfer  
        (Cons  $Move $Capture) $Rest) 
      (,  
        (ch_first_moving  $Move) 
        (ch_capture  $Capture $Rest))) True)
; 



;
; 

;
; 

;
; 

  (= 
    (-->  
      (ch_first_moving  
        (move  $Piece $Player $From $To)) 
      (,  
        (ch_square  $From) 
        (ch_square  $To))) True)
; 


  (= 
    (-->  
      (ch_moving  
        (move  $Piece $Player $From $To)) 
      (ch_square  $To)) True)
; 




  (= 
    (-->  
      (ch_capture  $In $Out) 
      (ch_null_capture  $In $Out)) True)
; 

  (= 
    (-->  
      (ch_capture  $In $Out) 
      (ch_real_capture  $In $Out)) True)
; 


  (= 
    (-->  
      (ch_null_capture  $X $X) ()) True)
; 


  (= 
    (-->  
      (ch_real_capture  
        (Cons  $C $Cs) $Rest) 
      (,  
        (ch_simp_capture  $C) 
        (ch_capture  $Cs $Rest))) True)
; 


  (= 
    (-->  
      (ch_simp_capture  $M) 
      (ch_remove  $M)) True)
; 

  (= 
    (-->  
      (ch_simp_capture  $M) 
      (ch_possess  $M)) True)
; 


;
; 

  (= 
    (-->  
      (ch_remove  
        (capture  remove $Caps)) ()) True)
; 


;
; 

  (= 
    (-->  
      (ch_possess  
        (capture  
          (possess  $Player) $Caps)) ()) True)
; 



  (= 
    (-->  
      (ch_square  $Sq) 
      (,  
        { (,  
            (invert  $Sq $Sq1) 
            (square  $Sq1 $X $Y)) } 
        (,  
          ($Col) 
          (,  
            { (nth_letter  $X $Col) } 
            (number  $Y))))) True)
; 


	

