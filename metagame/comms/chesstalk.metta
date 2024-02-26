;                                           (convert-to-metta-file  chesstalk $_134802 metagame/comms/chesstalk.pl metagame/comms/chesstalk.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ; pattern.pl


  !(ensure-loaded (library pipes))


  (= create  
    (global chessprog $Prog)
    (create $Prog))


  (= (create $Player)  
    (player-command $Player $Command)
    (talk-interface $Command))
	

  (= (player-command gnu $Command)  
    (gnu-command $Command))
  (= (player-command morph $Command)  
    (morph-command $Command))


  (= (gnu-command $Command)  
    (command-from-args 
      (:: nice -10 gnuchess) $Command))


  (= (morph-command $Command)  
    (morph-setup-file $File)
    (command-from-args 
      (:: '( cd ~/Morph/ ; nice -5 morph -q -wh -f' $File  )) $Command))
; ; command_from_args(['( cd ~/Morph/ ; nice -5 morph -wh -f morphstart )'],Command).


  (= (tellm $S)  
    (tell-chess $S))

  (= seem  see-chess)


  (= (tell-chess $Statement)  
    (tell-outstream $Statement))


  (= see-chess  
    (global instream $I)
    (set-input $I))


  (= (morph-setup-file $File)  
    (global handicap $File)
    (set-det))
  (= (morph-setup-file  morphstart)  True)


;
; If there is a winning move, assume the opponent will play it,
;
; as you can't legally leave your king in check in real chess.

  (= (chess-choose $Player $Move $SIn $SOut)  
    (victor-move $Move $SIn $SOut)
    (set-det)
    (format "King is in check; Game over!~n" Nil))
  (= (chess-choose $Player $Move $SIn $SOut)  
    (chess-choose-real $Player $Move $SIn $SOut))


  (= (chess-choose-real $Player $Move $SIn $SOut)  
    (update-chess $Player $SIn)
    (current-input $OldStream)
    (global instream $I)
    (set-input $I)
    (det-if-then-else 
      (chess-move $Player $_ $MoveString) 
      (, 
        (set-input $OldStream) 
        (det-if-then-else 
          (completed-move $MoveString $Move $SIn $SOut) 
          (print-choice $Move $SIn $SOut) 
          (, 
            (format "Strange string: ~w~n" 
              (:: $MoveString)) fail))) 
      (, 
        (set-input $OldStream) fail)))



  (= (init-chess-if $Role $SIn)  
    (should-init $Role $SIn)
    (set-det)
    (init-chess $Role $SIn))
  (= (init-chess-if  $Role $SIn)  True)


;
; If SETUP global was set to yes, then setup the
;
; pos, and reset it to no so we don't set it up each time!

  (= (should-init $Role $SIn)  
    (move-count $N $SIn)
    (< $N 2))
  (= (should-init $Role $SIn)  
    (global setup yes)
    (setg setup no))



  (= (init-chess $Role $SIn)  
    (global chessprog $Prog)
    (init-prog $Prog $Role $SIn))



  (= (update-chess $Role $SIn)  
    ( (init-chess-if $Role $SIn) 
      (det-if-then-else 
        (last-move $Move) 
        (, 
          (meta-to-chess $Move $ChMove) 
          (format "To Chess: I played ~w~n" 
            (:: $ChMove)) 
          (= $ChMove $TellMove) 
          (tell-chess (:: $TellMove))) True) tell-chess-move))
; ; ( N < 2 -> init_chess(Role,SIn) ; true),


  (= tell_chess_move  True)


  (= flush-chess  
    (global chessprog $Prog)
    (flush-prog $Prog))


  (= (flush-prog gnu)  
    (tell-chess (:: bd)))
  (= (flush-prog  morph)  True)


;
; This doesn't work, as gnuchess just plays if you tell it to,
;
; regardless of whos move it is in the position!
;
; So Don't tell chess anything.

  (= (chess-prompt $Player)  flush-chess)
;
; player_color(Player,Color),
;
; tell_chess([Color]).

	

  (= (meta-to-chess $Move $ChMove)  
    (chess-notation $Move $String)
    (concat-list $String $ChMove))



  (= (chess-move $Player $Number $Move)  
    (chess-prompt $Player)
    (chess-read $Number $FullMove)
    (set-det)
    (format "~s" 
      (:: $FullMove))
    (player-indent $Player $Indent)
    (append $Indent $Move0 $FullMove)
    (chess-to-meta $Move0 $Move))


  (= (chess-header-string $Pattern)  
    (append-list 
      (:: "My move is: ") $Pattern))



  (= (chess-read $Number $Move)  
    (chess-header-string $String)
    (read-until-string $String)
    (read-template $Move))


  (= (read-template $Move)  
    (global chessprog $Prog)
    (read-template $Prog $Move))
; ; read_line(Move),

  (= (read-template gnu $Move)  
    (read-four $Move))
  (= (read-template morph $Move)  
    (read-five $Move))


  (= (read-four (:: $A $B $C $D))  
    (get0 $A)
    (get0 $B)
    (get0 $C)
    (get0 $D))

  (= (read-five (:: $A $B $C $D))  
    (get0 $A)
    (get0 $B)
    (get0 $_)
    (get0 $C)
    (get0 $D))


  (= (player-indent  $_ "")  True)


  (= (chess-to-meta $A $B)  
    (chess-to-meta $B $A Nil))


  (= (-->  (chess-to-meta  ()) ())  True)
  (= (-->  (chess-to-meta  ([|]  $XM ([|]  $YM $Sqs))) (,  ($X) (,  ($Y) (,  {(chess-conv-square  $X $Y $XM $YM) } (chess-rest  $Sqs)))))  True)

  (= (-->  (chess-rest  ()) ())  True)
  (= (-->  (chess-rest  $Sqs) (,  () (chess-to-meta  $Sqs)))  True)



  (= (chess-conv-square $X $Y $XM $YM)  
    (name $XM 
      (:: $X))
    (name $YM 
      (:: $Y)))



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

  (= (chess-notation $ComplexMove $Notation)  
    (det-if-then-else 
      (var $ComplexMove) set-parsing-mode True)
    (chess-notation $ComplexMove $Notation Nil))



  (= (-->  (chess-notation  $M) (,  (prelims  $M $Pre) (chess-main  $Pre ())))  True)


;
; CONSIDER_PROMOTE
  (= (-->  (chess-consider-promote  ([|]  $T $Sel) $Rest) (,  (chess-attempt-promote  $T) (chess-select-promote  $Sel $Rest)))  True)


  (= (-->  (chess-select-promote  ([|]  (promote-select  $Square $OldPiece $OldPiece) $Rest) $Rest) ())  True)
  (= (-->  (chess-select-promote  ([|]  (promote-select  $Square $OldPiece $NewPiece) $Rest) $Rest) (,  {(,  (\==  $OldPiece $NewPiece) (,  (piece-struct-name  $NewPiece $Name) (,  (name  $Name ([|]  $NewC $_)) (name  $NewL ($NewC))))) } ($NewL)))  True)
  (= (-->  (chess-select-promote  $Rest $Rest) ())  True)



;
; ATTEMPT_PROMOTE
;
; attempt_promote(try_promote(square(5,1),piece(piece2,player),piece(piece3,player)))
;
; --> [promote,'(',5,',',1,')',white,piece3,';']
;
; Unless it promotes, it isn't mentioned in the notation.
  (= (-->  (chess-attempt-promote  (try-promote  $Square $OldPiece ())) ())  True)
  (= (-->  (chess-attempt-promote  (try-promote  $Square $OldPiece $OldPiece)) ())  True)


;
; MAIN
;
; Repeated Transfers, followed by possible player promotion.
  (= (-->  (chess-main  $In $Out) (,  (chess-first-transfer  $In $First) (,  (chess-continued-transfers  $First $T) (chess-consider-promote  $T $Out))))  True)


;
; TRANSFERS
  (= (-->  (chess-transfers  $In $Out) (,  (chess-transfer  $In $T) (chess-continued-transfers  $T $Out)))  True)

;
; CONTINUED_TRANSFERS
;
; Either no more transfers, or ';' and more transfers.
;
; Could tighten this rule: can't continue unless did a movement.
  (= (-->  (chess-continued-transfers  $In $In) ())  True)
  (= (-->  (chess-continued-transfers  ([|]  end_continues $Rest) $Rest) ())  True)
  (= (-->  (chess-continued-transfers  $In $Out) (chess-transfers  $In $Out))  True)


;
; TRANSFER : [move(piece(piece6,player),player,square(5,1),square(4,1)),
;
; remove(piece(piece6,player),square(4,1))]
;
; --> [white,piece6,'(',5,',',1,')',->,'(',4,',',1,')',x,'(',4,',',1,')']
;
; 
  (= (-->  (chess-transfer  ([|]  $Move $Capture) $Rest) (,  (chess-moving  $Move) (chess-capture  $Capture $Rest)))  True)

  (= (-->  (chess-first-transfer  ([|]  $Move $Capture) $Rest) (,  (chess-first-moving  $Move) (chess-capture  $Capture $Rest)))  True)


;
; MOVING
;
; moving(move(piece(piece1,opponent),opponent,square(1,6),square(2,4)),S,[]).
;
; S = [black,piece1,'(',1,',',6,')',->,'(',2,',',4,')']
  (= (-->  (chess-first-moving  (move  $Piece $Player $From $To)) (,  (chess-square  $From) (chess-square  $To)))  True)

  (= (-->  (chess-moving  (move  $Piece $Player $From $To)) (chess-square  $To))  True)



  (= (-->  (chess-capture  $In $Out) (chess-null-capture  $In $Out))  True)
  (= (-->  (chess-capture  $In $Out) (chess-real-capture  $In $Out))  True)

  (= (-->  (chess-null-capture  $X $X) ())  True)

  (= (-->  (chess-real-capture  ([|]  $C $Cs) $Rest) (,  (chess-simp-capture  $C) (chess-capture  $Cs $Rest)))  True)

  (= (-->  (chess-simp-capture  $M) (chess-remove  $M))  True)
  (= (-->  (chess-simp-capture  $M) (chess-possess  $M))  True)

;
; REMOVE
  (= (-->  (chess-remove  (capture  remove $Caps)) ())  True)

;
; POSSESS
  (= (-->  (chess-possess  (capture  (possess  $Player) $Caps)) ())  True)


  (= (-->  (chess-square  $Sq) (,  {(square  $Sq $X $Y) } (,  ($Col) (,  {(nth-letter  $X $Col) } (number  $Y)))))  True)

	
;
; ================================================================================
;
; User Interface
;
; ================================================================================



  (= create-top  create)




  (= (square-chess-name $Player $Sq $Name)  
    (square-chess-name $Player $Sq $Name $S))

  (= (square-chess-name $Player $Sq $Name $S)  
    (player-role $Player)
    (det-if-then-else 
      (var $S) 
      (checkpoint init $S) True)
    (on $Piece $Sq $S)
    (piece-struct-name $Piece $PName)
    (piece-struct-owner $Piece $Player)
    (player-piece-print-name $Player $PName $ChessName)
    (chess-square $Sq $ChessSq Nil)
    (concat-list 
      (Cons  $ChessName $ChessSq) $Name))



  (= (setup-gnu $S)  
    (tell-chess (:: edit))
    (tell-chess (:: #))
    (whenever 
      (square-chess-name player $_ $Name $S) 
      (tell-chess (:: $Name)))
    (tell-chess (:: c))
    (whenever 
      (square-chess-name opponent $_ $Name $S) 
      (tell-chess (:: $Name)))
    (tell-chess (:: .)))


  (= (init-prog  morph $Role $SIn)  True)

;
; Careful when letting gnu play white in typical startup chess
;
; positions, as he thinks kings can castle!
  (= (init-prog gnu $Role $S)  
    ( (global handicap $H) 
      (> $H 0) 
      (set-det) 
      (tell-chess (:: 1)) 
      (tell-chess (:: new)) 
      (setup-gnu $S) set-gnu-depth 
      (init-gnu-role $Role)))



  (= set-gnu-depth  
    (det-if-then-else 
      (global depth $D) 
      (set-gnu-depth $D) True))

  (= (set-gnu-depth $DNum)  
    (number-chars $DNum $Chars)
    (symbol-chars $D $Chars)
    (tell-chess (:: depth))
    (tell-chess (:: $D)))





  (= (init-gnu-role $Role)  
    (player-color $Role $Color)
    (tell-chess (:: $Color))) 
;
; init_gnu_role(player) :- tell_chess([switch]).
;
; init_gnu_role(opponent).




  (= (tell-com $_ $_ $_ $X)  
    (tell-chess $X))
; /* init_prog(gnu,Role) :- tell_chess(['1']), tell_chess([new]), tell_chess([get]), tell_chess(['metastart.game']), tell_chess(['']), init_gnu_role(Role). init_gnu_role(player) :- tell_chess([switch]). init_gnu_role(opponent). */
; /* init_prog(gnu,Role,_S) :- tell_chess(['1']), tell_chess([new]), tell_chess([get]), tell_chess(['metastart.game']), tell_chess(['']), init_gnu_role(Role). */
; ;==========================================================================

  (= (tell-top $X)  
    (tell-chess $X))


  (= (depth-com $_ $_ $_ $X)  
    (set-gnu-depth $X))

  (= (depth-top $X)  
    (set-gnu-depth $X))


