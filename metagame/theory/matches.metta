;                                           (convert_to_metta_file  matches $_432756 metagame/theory/matches.pl metagame/theory/matches.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; matches.pl

;
; MATCHES(?Descr,?Piece)
;
; A Piece matches a Descr if both the player and type in
;
; the description are at least as general as those of the
;
; piece.
;
; 
;
; piece_struct(P,guppy,player),piece_description(T,any_player,[guppy,frog]),
;
; matches(T,P).

  (= (matches $Descr $Piece)  
    (piece-description $Descr $Player_Gen $Piece_Gen)
    (piece-struct $Piece $Name $Player)
    (matches-player $Player_Gen $Player)
    (matches-name $Piece_Gen $Name))



  (= (matches-player any-player $Player)  
    (player-role $Player))
  (= (matches_player  player player)  True)
  (= (matches_player  opponent opponent)  True)

;
; Note a type must be either any_piece or a LIST, not singletons.
;
; Really should peval to get all piece names, instead of ref. back
;
; to game each time.
;
; Note also second clause should really check piece is a current
;
; game piece (peval also won't work without this!).

  (= (matches-name any-piece $Name)  
    (current-game-piece-name $Name))
  (= (matches-name (Cons  $H $T) $Name)  
    (member $Name 
      (Cons  $H $T)))	


