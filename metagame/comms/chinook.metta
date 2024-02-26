;                                           (convert-to-metta-file  chinook $_356174 metagame/comms/chinook.pl metagame/comms/chinook.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================


  !(ensure-loaded (library pipes))


  (= create-chinook  
    (command-from-args 
      (:: nice -10 chinook) $Command)
    (interface-record-streams $Command $InStream $OutStream))


  (= (tell-chinook $Statement)  
    (tell-outstream $Statement))


  (= (tellc $S)  
    (tell-chinook $S))


  (= seem  
    (global instream $I)
    (set-input $I))



;
; ; pattern.pl


  (= chinook-quit  
    ( (tell-chinook (:: q)) interface-close-streams))


  (= (chinook-choose $Player $Move $SIn $SOut)  
    (update-chinook $Player $SIn)
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
        (chinook-choose $Player $Move $SIn $SOut))))


  !(add-global handicap 0)


  (= setup-chinook  
    (global handicap $H)
    (tell-chinook (:: i))
    (config $H))


;
; handicap_position(N,Init,Pos,Command) :-
    


  (= (config  0)  True) 
  (= (config 1)  
    (tell-chinook rb6))
  (= (config 2)  
    (tell-chinook rb6rd6))
  (= (config 3)  
    (tell-chinook rb6rd6rf6))
  (= (config 4)  
    (tell-chinook rb6rd6rf6rh6))
  (= (config 8)  
    (tell-chinook rb6rd6rf6rh6)
    (tell-chinook ra7rc7re7rg7))




  (= (update-chinook $Role $SIn)  
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
          (tell-chinook (:: $TellMove))) True) tell-chinook-move))


  (= tell-chinook-move  
    (tell-chinook (:: g)))

	

  (= (meta-to-ch $Move $ChMove)  
    (ch-notation $Move $String)
    (concat-list $String $ChMove))



  (= (chinook-move $Player $Number $Move)  
    (chinook-read $Number $FullMove)
    (set-det)
    (format $FullMove Nil)
    (player-indent $Player $Indent)
    (append $Indent $Move0 $FullMove)
    (ch-to-meta $Move0 $Move))
;
; append(Move0,".",Move1),
;
; tokenize_chars(Move1,Move).
	



  (= (chinook-read $Number $Move)  
    (append-list 
      (:: "I move") $Pattern)
    (format "Looking for pattern: ~w~n" 
      (:: $Pattern))
    (found $Pattern)
    (read-keyboard-tokens (:: $Number $_))
    (read-line $Move))
; ; append_list(["I move ",[Num],". "],Pattern),


  (= (player-indent  player " ")  True)
  (= (player-indent  opponent "  ... ")  True)


;
; chmv([],[]) --> [].
;
; chmv([C|Cs],[M|Ms]) -->



  (= (ch-to-meta  () ())  True)
  (= (ch-to-meta (Cons  $X (Cons  $Y $Rest)) (Cons  $XM (Cons  $YM $RestM)))  
    (conv-square $X $Y $XM $YM)
    (ch-to-meta $Rest $RestM))


  (= (conv-square $X $Y $XM $YM)  
    (conv-x $X $XM1)
    (conv-y $Y $YM1)
    (name $XM 
      (:: $XM1))
    (name $YM 
      (:: $YM1)))



  (= (conv-x $X $XM)  
    (is $XM 
      (+ 
        (- "h" $X) "a")))

  (= (conv-y $Y $YM)  
    (is $YM 
      (+ 
        (- "8" $Y) "1")))




;
; ======================================================================

;
; Ensures parsing mode if really parsing, as otherwise get strange bugs.

  (= (ch-notation $ComplexMove $Notation)  
    (det-if-then-else 
      (var $ComplexMove) set-parsing-mode True)
    (ch-notation $ComplexMove $Notation Nil))



  (= (-->  (ch-notation  $M) (,  (prelims  $M $Pre) (ch-main  $Pre ())))  True)


;
; CONSIDER_PROMOTE
  (= (-->  (ch-consider-promote  ([|]  $T $Sel) $Rest) (,  (ch-attempt-promote  $T) (ch-select-promote  $Sel $Rest)))  True)

  (= (-->  (ch-select-promote  ([|]  (promote-select  $_ $_ $_) $Rest) $Rest) ())  True)
  (= (-->  (ch-select-promote  $Rest $Rest) ())  True)



;
; ATTEMPT_PROMOTE
;
; attempt_promote(try_promote(square(5,1),piece(piece2,player),piece(piece3,player)))
;
; --> [promote,'(',5,',',1,')',white,piece3,';']
;
; Unless it promotes, it isn't mentioned in the notation.
  (= (-->  (ch-attempt-promote  (try-promote  $Square $OldPiece ())) ())  True)
  (= (-->  (ch-attempt-promote  (try-promote  $Square $OldPiece $NewPiece)) ())  True)


;
; MAIN
;
; Repeated Transfers, followed by possible player promotion.
  (= (-->  (ch-main  $In $Out) (,  (ch-first-transfer  $In $First) (,  (ch-continued-transfers  $First $T) (ch-consider-promote  $T $Out))))  True)


;
; TRANSFERS
  (= (-->  (ch-transfers  $In $Out) (,  (ch-transfer  $In $T) (ch-continued-transfers  $T $Out)))  True)

;
; CONTINUED_TRANSFERS
;
; Either no more transfers, or ';' and more transfers.
;
; Could tighten this rule: can't continue unless did a movement.
  (= (-->  (ch-continued-transfers  $In $In) ())  True)
  (= (-->  (ch-continued-transfers  ([|]  end_continues $Rest) $Rest) ())  True)
  (= (-->  (ch-continued-transfers  $In $Out) (ch-transfers  $In $Out))  True)


;
; TRANSFER : [move(piece(piece6,player),player,square(5,1),square(4,1)),
;
; remove(piece(piece6,player),square(4,1))]
;
; --> [white,piece6,'(',5,',',1,')',->,'(',4,',',1,')',x,'(',4,',',1,')']
;
; 
  (= (-->  (ch-transfer  ([|]  $Move $Capture) $Rest) (,  (ch-moving  $Move) (ch-capture  $Capture $Rest)))  True)

  (= (-->  (ch-first-transfer  ([|]  $Move $Capture) $Rest) (,  (ch-first-moving  $Move) (ch-capture  $Capture $Rest)))  True)


;
; MOVING
;
; moving(move(piece(piece1,opponent),opponent,square(1,6),square(2,4)),S,[]).
;
; S = [black,piece1,'(',1,',',6,')',->,'(',2,',',4,')']
  (= (-->  (ch-first-moving  (move  $Piece $Player $From $To)) (,  (ch-square  $From) (ch-square  $To)))  True)

  (= (-->  (ch-moving  (move  $Piece $Player $From $To)) (ch-square  $To))  True)



  (= (-->  (ch-capture  $In $Out) (ch-null-capture  $In $Out))  True)
  (= (-->  (ch-capture  $In $Out) (ch-real-capture  $In $Out))  True)

  (= (-->  (ch-null-capture  $X $X) ())  True)

  (= (-->  (ch-real-capture  ([|]  $C $Cs) $Rest) (,  (ch-simp-capture  $C) (ch-capture  $Cs $Rest)))  True)

  (= (-->  (ch-simp-capture  $M) (ch-remove  $M))  True)
  (= (-->  (ch-simp-capture  $M) (ch-possess  $M))  True)

;
; REMOVE
  (= (-->  (ch-remove  (capture  remove $Caps)) ())  True)

;
; POSSESS
  (= (-->  (ch-possess  (capture  (possess  $Player) $Caps)) ())  True)


  (= (-->  (ch-square  $Sq) (,  {(,  (invert  $Sq $Sq1) (square  $Sq1 $X $Y)) } (,  ($Col) (,  {(nth-letter  $X $Col) } (number  $Y)))))  True)

	

