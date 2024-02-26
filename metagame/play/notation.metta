;                                           (convert_to_metta_file  notation $_214764 metagame/play/notation.pl metagame/play/notation.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; Notation.pl
;
; 
;
; This defines the MOVE GRAMMAR for symmetric
;
; chess-like games.
;
; 
;
; Note that the notation for a move, like that for
;
; a game, ends in a period.


  !(my-ensure-loaded (library tokenizer))
  !(my-ensure-loaded (library grammar))

;
; Ensures parsing mode if really parsing, as otherwise get strange bugs.

  (= (move-notation $ComplexMove $Notation)  
    (det-if-then-else 
      (var $ComplexMove) set-parsing-mode True)
    (move-notation $ComplexMove $Notation Nil))


;
; MOVE_NOTATION

  (= (-->  (move_notation  $M) (,  (prelims  $M $Pre) (,  (main  $Pre ()) period)))  True)


;
; PRELIMS - covers ending assignments and init promotes.
;
; Can also be bypassed (thus nothing) if last move was a placement.
  (= (-->  (prelims  (Cons  $P $Out) $Out) (init_promote  $P))  True)
  (= (-->  (prelims  (Cons  end_assign $Rest) $Rest) ())  True)
  (= (-->  (prelims  $Rest $Rest) ())  True)


;
; INIT_PROMOTE
;
; init_promote(opponent_promote(square(5,1),piece(piece2,player),piece(piece3,player)),S,[]).
;
; S = [promote,'(',5,',',1,')',white,piece3] ?
  (= (-->  (init_promote  (opponent_promote  () () ())) ())  True)
  (= (-->  (init_promote  (opponent_promote  $Sq $OldPiece $NewPiece)) (,  (promote) (,  (gsquare  $Sq) (,  (piece  $NewPiece) (,  (;) line)))))  True)


;
; CONSIDER_PROMOTE
  (= (-->  (consider_promote  (Cons  $T $Sel) $Rest) (,  (attempt_promote  $T) (select_promote  $Sel $Rest)))  True)

  (= (-->  (select_promote  (Cons  (promote_select  $Square $OldPiece $OldPiece) $Rest) $Rest) ())  True)
  (= (-->  (select_promote  (Cons  (promote_select  $Square $OldPiece $NewPiece) $Rest) $Rest) (,  (;) (,  line (,  (promote) (,  (gsquare  $Square) (piece  $NewPiece))))))  True)
  (= (-->  (select_promote  $Rest $Rest) ())  True)



;
; ATTEMPT_PROMOTE
;
; attempt_promote(try_promote(square(5,1),piece(piece2,player),piece(piece3,player)))
;
; --> [promote,'(',5,',',1,')',white,piece3,';']
;
; If it couldn't promote at all, don't mention it.
;
; If it promotes to same piece, mention is optional.
;
; Thus if promotion isn't mentioned but it should promote, assume it chooses
;
; same piece.
  (= (-->  (attempt_promote  (try_promote  $Square $OldPiece ())) ())  True)
  (= (-->  (attempt_promote  (try_promote  $Square $OldPiece $OldPiece)) ())  True)
;
; attempt_promote(try_promote(_Square,_OldPiece,NewPiece)) --> {var(NewPiece)}, [].
  (= (-->  (attempt_promote  (try_promote  $Square $OldPiece $NewPiece)) (,  (;) (,  (promote) (,  (gsquare  $Square) (piece  $NewPiece)))))  True)


;
; MAIN
;
; Repeated Transfers, followed by possible player promotion.
  (= (-->  (main  (Cons  $P $Out) $Out) (placing  $P))  True)
  (= (-->  (main  $In $Out) (,  (transfers  $In $T) (consider_promote  $T $Out)))  True)


;
; TRANSFERS
  (= (-->  (transfers  $In $Out) (,  (transfer  $In $T) (continued_transfers  $T $Out)))  True)

;
; CONTINUED_TRANSFERS
;
; Either no more transfers, or ';' and more transfers.
;
; Could tighten this rule: can't continue unless did a movement.
  (= (-->  (continued_transfers  $In $In) ())  True)
  (= (-->  (continued_transfers  (Cons  end_continues $Rest) $Rest) ())  True)
  (= (-->  (continued_transfers  $In $Out) (,  (;) (,  line (transfers  $In $Out))))  True)


;
; TRANSFER : [move(piece(piece6,player),player,square(5,1),square(4,1)),
;
; remove(piece(piece6,player),square(4,1))]
;
; --> [white,piece6,'(',5,',',1,')',->,'(',4,',',1,')',x,'(',4,',',1,')']
;
; 
  (= (-->  (transfer  (Cons  $Move $Capture) $Rest) (,  (moving  $Move) (capture  $Capture $Rest)))  True)


  (= (-->  (placing  $M) (,  {(= $M  
    (place  $Piece $Player $Sq)) } (,  (piece  $Piece) (,  (paren_color_player  $Player) (,  (->) (gsquare  $Sq))))))  True)
  (= (-->  (placing  $M) (,  {(= $M  
    (assign  $Piece $Player $Sq)) } (,  (piece  $Piece) (,  (paren_color_player  $Player) (,  (->) (gsquare  $Sq))))))  True)

;
; MOVING
;
; moving(move(piece(piece1,opponent),opponent,square(1,6),square(2,4)),S,[]).
;
; S = [black,piece1,'(',1,',',6,')',->,'(',2,',',4,')']
  (= (-->  (moving  (move  $Piece $Player $From $To)) (,  (piece  $Piece) (,  (gsquare  $From) (,  (->) (gsquare  $To)))))  True)

  (= (-->  (capture  $In $Out) (null_capture  $In $Out))  True)
  (= (-->  (capture  $In $Out) (real_capture  $In $Out))  True)

  (= (-->  (null_capture  $X $X) ())  True)

  (= (-->  (real_capture  (Cons  $C $Cs) $Rest) (,  (x) (,  (simp_capture  $C) (capture  $Cs $Rest))))  True)

  (= (-->  (simp_capture  $M) (remove  $M))  True)
  (= (-->  (simp_capture  $M) (possess  $M))  True)

;
; REMOVE
  (= (-->  (remove  (capture  remove $Caps)) (capture_effects_list  $Caps))  True)

;
; POSSESS
  (= (-->  (possess  (capture  (possess  $Player) $Caps)) (,  (capture_effects_list  $Caps) (,  (/) (paren_color_player  $Player))))  True)

  (= (-->  (capture_effects_list  ()) ())  True)
  (= (-->  (capture_effects_list  (Cons  $C $Caps)) (,  (capture_effect  $C) (capture_effects_list  $Caps)))  True)

;
; Capture effects are of the form: piece square, like: white king (d,2)
;
; capture_effect(C) --> {captured(C,_Piece,Square)}, gsquare(Square).
  (= (-->  (capture_effect  $C) (,  {(captured  $C $Piece $Square) } (,  (piece  $Piece) (gsquare  $Square))))  True)



;
; A PIECE has both an owner and a name, like [white,bishop].
;
; A PIECE_NAME has just the name (as defined in grammar.pl
;
; 
;
; piece(piece(bishop,player)) --> [white,bishop]
;
; Interesting idea: Allow variables here, or omitted components,
;
; for pattern-matching move notation.
  (= (-->  (piece  $P) (,  {(,  (piece_struct_name  $P $Name) (piece_struct_owner  $P $O)) } (,  (color_player  $O) (piece_name  $Name))))  True)


;
; PAREN_COLOR_PLAYER
;
; Maps a player into the corresponding color, wrapped in parens.
  (= (-->  (paren_color_player  $P) (,  (() (,  (color_player  $P) ()))))  True)

  (= (-->  (color_player  player) (white))  True)
  (= (-->  (color_player  opponent) (black))  True)

;
; color_player(player,white).
;
; color_player(opponent,black).


  (= (color_player  white player)  True)
  (= (color_player  black opponent)  True)


  (= (player_color  player white)  True)
  (= (player_color  opponent black)  True)


;
; ================================================================================
;
; Reading moves from files and strings, printing back.
;
; ================================================================================

;
; ;; Read from pascal-like MOVE NOTATION, into list.
;
; ;; Reading is CASE-INSENSITIVE: all alpha characters
;
; ;; are converted to lower case when read in.
;
; ;; Also ignores extra blanks, tabs, and linefeeds.
;
; ;; Comments occur from some point in a line started by ;,
;
; ;; and will be ignored to end of line.
;
; ;; Can read games without spaces between operators and atoms,
;
; ;; so squares can be written (X,Y) instead of ( X , Y ).
;
; ;;
;
; Notated Move played: [white,piece6,(,5,,,1,),->,(,4,,,1,),x,(,4,,,1,),;,
;
; white,piece6,(,4,,,1,),->,(,3,,,1,),x,(,3,,,1,),;,
;
; white,piece6,(,3,,,1,),->,(,2,,,1,),x,(,2,,,1,),;,
;
; white,piece6,(,2,,,1,),->,(,1,,,1,),x,(,1,,,1,),'.']
;
; Stored in /generator/test_read.move


;
; print_read_move(File)
;
; Note print

  (= (print-read-move-from-file $File)  
    (format "~nReading move from file~n" Nil)
    (read-move-from-file-to-list $File $Move)
    (format "~nRead move from file~n" Nil)
    (set-parsing-mode)
    (format "~nParsing move in parse mode~n" Nil)
    (move-notation $ComplexMove $Move Nil)
    (set-printing-mode)
    (format "~nParsing move in print mode~n" Nil)
    (move-notation $ComplexMove $Move1 Nil)
    (format "~nPrinting move~n" Nil)
    (print-tokens $Move1))


  (= (read-move-from-file-to-list $File $Move)  
    (read-tokens-from-file $File $Move))

	
;
; MOVE_NOTATION_STRING(?Move,?String)
;
; Converts between an internal move notation and
;
; a string of (ascii) characters.

  (= (move-notation-string $Move $String)  
    (var $String)
    (set-det)
    (move-notation $Move $Tokens)
    (print-tokens-to-string $Tokens $String))
  (= (move-notation-string $Move $String)  
    (var $Move)
    (read-tokens-from-string $String $Tokens)
    (move-notation $Move $Tokens))

	

