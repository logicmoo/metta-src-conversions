;                                           (convert-to-metta-file  grammar $_156972 metagame/generator/grammar.pl metagame/generator/grammar.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;; grammar.pl
;
; ;; This file defines the grammar for symmetric chess-like games.
;
; ;; This grammar is used bidirectionally, to parse game definitions
;
; ;; into an internal representation, and to generate definitions
;
; ;; from an internal representation.
;
; ;;
;
; ;; The grammar is described in the paper:
;
; ;; Metagame in Symmetric Chess-Like Games
;
; ;;
;
; ;; The line and tab symbols in the rules are used for pretty-printing
;
; ;; purposes, and are ignored when parameter parsing_mode is parsing.


  (= (-->  (game  $Game) (,  {(game  $Game $Name $Board $Pieces $Goals $Constraints) } (,  (game) (,  (game-name  $Name) (,  line (,  (goal-defs  $Goals) (,  line (,  (board  $Board) (,  line (,  (opt-constraints  $Constraints) (,  line (,  (piece-defs  $Pieces) (,  (end game) period)))))))))))))  True)


  (= (-->  (game-name  $Game) (,  ($Game) {(atom  $Game) }))  True)

  (= (-->  (board  $B) (,  {(,  (board  $B) (,  (board-size  $B $XMax $YMax) (,  (board-type  $B $Type) (,  (board-promote-rows  $B $Promote) (,  (board-inversion  $B $Inversion) (board-assignments  $B $Assignments)))))) } (,  (board_size) (,  (number  $XMax) (,  (by) (,  (number  $YMax) (,  line (,  (board_type) (,  (board-type  $Type) (,  line (,  (opt-inversion  $Inversion) (,  (promote_rank) (,  (number  $Promote) (,  line (,  (setup) (,  line (assignment-list  $Assignments)))))))))))))))))  True)

  (= (-->  (board-type  planar) (planar))  True)
  (= (-->  (board-type  vertical_cylinder) (vertical_cylinder))  True)


  (= (inversion-type  forward)  True)
  (= (inversion-type  diagonal)  True)




  (= (-->  (assignment-list  $A) (|  (assignment-decision  $A) (assignments  $A)))  True)


  (= (-->  (assignments  ($A)) (assignment  $A))  True)
  (= (-->  (assignments  ([|]  $A $As)) (,  (assignment  $A) (,  line (assignments  $As))))  True)

  (= (-->  (assignment  (= $A  $S)) (,  (tab  5) (,  (piece-name  $A) (,  (at) (square-list  $S)))))  True)

  (= (-->  (assignment-decision  $D) (,  {(,  (decision  $D) (,  (decision-chooser  $D $C) (,  (decision-options  $D $O) (decision-constraints  $D $Con)))) } (,  (tab  3) (,  (decision) (,  (assigner  $C) (,  (assigns) (,  (piece-names  $O) (,  line (,  (tab  5) (,  (to) (,  (square-list  $Con) (,  line (,  (tab  3) (end decision))))))))))))))  True)

  (= (-->  (assigner  $C) (player  $C))  True)
  (= (-->  (assigner  random) (random))  True)


;
; 0 or more piece definitions.
  (= (-->  (piece-defs  ()) ())  True)
  (= (-->  (piece-defs  ([|]  $D $Defs)) (,  (piece-def  $D) (,  line (piece-defs  $Defs))))  True)


  (= (-->  (piece-def  $Def) (,  {(piece-definition  $Def $Name $Movement $Capture $Promote $Con) } (,  (define) (,  (piece-name  $Name) (,  line (,  (opt-moving  $Movement) (,  (opt-capturing  $Capture) (,  (opt-promoting  $Name $Promote) (,  (opt-constraints  $Con) (,  (end define) line))))))))))  True)


  (= (-->  (opt-moving  ()) ())  True)
  (= (-->  (opt-moving  $Movement) (,  (tab  3) (,  (moving) (,  line (,  (movement-def  $Movement) (,  line (,  (tab  3) (,  (end moving) line))))))))  True) 

  (= (-->  (opt-capturing  ()) ())  True)
  (= (-->  (opt-capturing  $Capture) (,  (tab  3) (,  (capturing) (,  line (,  (capture-def  $Capture) (,  line (,  (tab  3) (,  (end capturing) line))))))))  True) 

;
; If no promote name is given defaults to promoting to same piece.
  (= (-->  (opt-promoting  $Name (promote  $Name)) ())  True)
  (= (-->  (opt-promoting  $_ $Promote) (,  (tab  3) (,  (promoting) (,  (promote-def  $Promote) (,  line (,  (tab  3) (,  (end promoting) line)))))))  True) 



  (= (-->  (opt-constraints  $Constraint) (,  {(,  (constraint  $Constraint) (,  (constraint-must-capture  $Constraint no) (constraint-continue-captures  $Constraint no))) } ()))  True)	
  (= (-->  (opt-constraints  $Constraint) (,  (tab  3) (,  (constraints) (,  (constraint-def  $Constraint) line))))  True) 


;
; Defaults to diagonal inversion.
  (= (-->  (opt-inversion  diagonal) ())  True)	
  (= (-->  (opt-inversion  $Inversion) (,  (inversion) (,  (inversion-def  $Inversion) line)))  True) 


  (= (-->  (inversion-def  $Inversion) (,  {(inversion-type  $Inversion) } ($Inversion)))  True)



  (= (-->  (movement-def  ($M)) (simple-movement  $M))  True)
  (= (-->  (movement-def  ([|]  $M $Ms)) (,  (simple-movement  $M) (,  line (,  line (movement-def  $Ms)))))  True)

  (= (-->  (simple-movement  $M) (,  {(,  (movement  $M) (,  (movement-type  $M $T) (,  (movement-dir  $M $D) (movement-sym  $M $S)))) } (,  (tab  15) (,  (movement) (,  line (,  (movement-type  $T) (,  line (,  (gdirection  $D) (,  (syms  $S) (,  line (,  (tab  15) (end movement))))))))))))  True)


  (= (-->  (movement-type  $T) (leaper  $T))  True)
  (= (-->  (movement-type  $T) (rider  $T))  True)
  (= (-->  (movement-type  $T) (hopper  $T))  True)

  (= (-->  (leaper  $L) (,  {(leaper  $L) } (,  (tab  20) (leap))))  True)

  (= (-->  (rider  $R) (,  {(,  (rider  $R) (,  (rider-must  $R $Must) (,  (rider-min  $R $Min) (rider-max  $R $Max)))) } (,  (tab  20) (,  (ride) (,  (min-dist  $Min) (,  (max-dist  $Max) (longest  $Must)))))))  True)

  (= (-->  (min-dist  1) ())  True)
  (= (-->  (min-dist  $Min) (,  (min) (number  $Min)))  True)

  (= (-->  (max-dist  any) ())  True) 
  (= (-->  (max-dist  $N) (,  (max) (number  $N)))  True)

  (= (-->  (longest  no) ())  True)
  (= (-->  (longest  yes) (longest))  True)


  (= (-->  (hopper  $H) (,  {(,  (hopper  $H) (,  (hopper-before  $H $B) (,  (hopper-over  $H $O) (,  (hopper-after  $H $A) (hopper-type  $H $R))))) } (,  (tab  20) (,  (hop) (,  (before) (,  (compare-eq  $B) (,  (over) (,  (compare-eq  $O) (,  (after) (,  (compare-eq  $A) (,  line (,  (tab  20) (,  (hop_over) (description  $R))))))))))))))  True)


;
; --------------
;
; Symmetries
;
; --------------

  (= (-->  (syms  $Sym) (,  {(symmetry  $Sym) } (,  (symmetry) (symmetry-set  $Sym))))  True)

;
; Not produced all_sym.
  (= (-->  (symmetry-set  $Sym) (,  {(,  (sym-forward  $Sym yes) (,  (sym-side  $Sym yes) (sym-rotation  $Sym yes))) } (all_symmetry)))  True)
  (= (-->  (symmetry-set  $Sym) (,  openbrace (,  (sym-set  $Sym) closebrace)))  True)

  (= (-->  (sym-set  $Sym) (,  (forward  $Sym) (,  (side  $Sym) (rotation  $Sym))))  True)


  (= (-->  (forward  $Sym) (,  () {(sym-forward  $Sym no) }))  True)
  (= (-->  (forward  $Sym) (,  (forward) {(sym-forward  $Sym yes) }))  True)

  (= (-->  (side  $Sym) (,  () {(sym-side  $Sym no) }))  True)
  (= (-->  (side  $Sym) (,  (side) {(sym-side  $Sym yes) }))  True)

  (= (-->  (rotation  $Sym) (,  (rotation) {(sym-rotation  $Sym yes) }))  True)
  (= (-->  (rotation  $Sym) (,  {(sym-rotation  $Sym no) } ()))  True) 

;
; --------------
;
; Equations
;
; --------------

;
; Change from eq to = in generator.

  (= (-->  (compare-eq  $C) (,  {(,  (comparison  $C $Comp $Number) (,  (comparison-comp  $C $Comp) (comparison-num  $C $Number))) } (,  openb (,  (x) (,  (comparative  $Comp) (,  (delta  $Number) closeb))))))  True)

;
; comparative(Comp) --> [Comp], {comparative(Comp)}.

  (= (-->  (comparative  geq) (>=))  True)
  (= (-->  (comparative  eq) (=))  True)
  (= (-->  (comparative  leq) (<=))  True)


;
; -----------------------------
;
; Directions, Square_List, Deltas
;
; ----------------------------

  (= (-->  (gdirection  $Dir) (,  {(direction  $Dir $X $Y) } (,  (tab  20) (,  (<) (,  (delta  $X) (,  comma (,  (delta  $Y) (>))))))))  True)  

  (= (-->  (square-list  $Squares) (,  openbrace (,  (squares  $Squares) closebrace)))  True)

  (= (-->  (squares  ($H)) (gsquare  $H))  True)
  (= (-->  (squares  ([|]  $H $T)) (,  (gsquare  $H) (squares  $T)))  True)

  (= (-->  (gsquare  $Sq) (,  {(,  (square  $Sq $X $Y) (alpha-squares-mode  on)) } (,  (() (,  ($Col) (,  {(nth-letter  $X $Col) } (,  comma (,  (number  $Y) ()))))))))  True) 

  (= (-->  (gsquare  $Sq) (,  {(,  (square  $Sq $X $Y) (alpha-squares-mode  off)) } (,  (() (,  (number  $X) (,  comma (,  (number  $Y) ())))))))  True) 


  (= (-->  (delta  $Delta) (number  $Delta))  True)

  (= (-->  (number  $N) (,  ($N) {(number  $N) }))  True)


;
; --------------
;
; CAPTURES
;
; --------------

  (= (-->  (capture-def  ($C)) (simple-capture  $C))  True)
  (= (-->  (capture-def  ([|]  $C $Cs)) (,  (simple-capture  $C) (,  line (,  line (capture-def  $Cs)))))  True)

  (= (-->  (simple-capture  $C) (,  {(,  (capture  $C) (,  (capture-movement  $C $M) (,  (capture-methods  $C $T) (,  (capture-type  $C $R) (capture-effect  $C $E))))) } (,  (tab  5) (,  (capture by) (,  (capture-methods  $T) (,  line (,  (tab  14) (,  (type) (,  (description  $R) (,  line (,  (tab  14) (,  (effect) (,  (effect  $E) (,  line (,  (movement-def  $M) (,  line (,  (tab  5) (end capture))))))))))))))))))  True)

  (= (-->  (capture-methods  $M) (,  {(method  $M) } (,  openbrace (,  (retrieve  $M) (,  (clobber  $M) (,  (hop  $M) closebrace))))))  True)


  (= (-->  (retrieve  $Method) (,  () {(method-retrieve  $Method no) }))  True)
  (= (-->  (retrieve  $Method) (,  (retrieve) {(method-retrieve  $Method yes) }))  True)

  (= (-->  (clobber  $Method) (,  () {(method-clobber  $Method no) }))  True)
  (= (-->  (clobber  $Method) (,  (clobber) {(method-clobber  $Method yes) }))  True)

  (= (-->  (hop  $Method) (,  (hop) {(method-hop  $Method yes) }))  True)
  (= (-->  (hop  $Method) (,  {(method-hop  $Method no) } ()))  True) 


;
; Simplified as in paper grammar.
;
; Removed displacement
  (= (-->  (effect  remove) (remove))  True)
  (= (-->  (effect  (possess  $Player)) ($Player possesses))  True)
;
; effect(give) --> [opponent,possesses].
;
; effect(displace(Player)) --> [Player, displaces].
;
; effect(displace_enemy) --> [opponent, displaces].



;
; --------------
;
; GOALS
;
; --------------
;
; The generator always produces 1 stalemate goal,
;
; then adds the other 2 types of goals. But this
;
; grammar is actually more general than the games
;
; generated, and programs must be able to read anything
;
; in the class defined by this grammar.

  (= (-->  (goal-defs  $Goals) (,  (goals) (goals  $Goals)))  True)

  (= (-->  (goals  ()) ())  True)
  (= (-->  (goals  ([|]  $G $Goals)) (,  (simple-goal  $G) (,  line (,  (tab  5) (goals  $Goals)))))  True)

  (= (-->  (simple-goal  $Arrive) (,  {(arrive-goal  $Arrive $Desc $Squares) } (,  (arrive) (,  (description  $Desc) (,  (at) (square-list  $Squares))))))  True)

  (= (-->  (simple-goal  $Eradicate) (,  {(eradicate-goal  $Eradicate $Desc) } (,  (eradicate) (description  $Desc))))  True)
	               
  (= (-->  (simple-goal  $Stalemate) (,  {(stalemate-goal  $Stalemate $Player) } (,  (stalemate) (player  $Player))))  True)

;
; --------------------
;
; Descriptions
;
; --------------------

  (= (-->  (description  $Desc) (,  {(piece-description  $Desc $Player $Pieces) } (,  openb (,  (player-gen  $Player) (,  (piece-names  $Pieces) closeb)))))  True)

  (= (-->  (piece-names  any_piece) (any_piece))  True)
  (= (-->  (piece-names  $Pieces) (,  openbrace (,  (identifiers  $Pieces) closebrace)))  True)

  (= (-->  (identifiers  ($Piece)) (piece-name  $Piece))  True)
  (= (-->  (identifiers  ([|]  $P $Pieces)) (,  (piece-name  $P) (identifiers  $Pieces)))  True)


  (= (-->  (piece-name  $Piece) (,  ($Piece) {(atom  $Piece) }))  True)


  (= (-->  (player-gen  $Player) (,  openbrace (,  (player  $Player) closebrace)))  True)
  (= (-->  (player-gen  any_player) (any_player))  True)

  (= (-->  (player  player) (player))  True)
  (= (-->  (player  opponent) (opponent))  True)


  (= (player  player)  True)
  (= (player  opponent)  True)


;
; --------------
;
; Piece Movement Constraints
;
; --------------


  (= (-->  (constraint-defs  $Constraint) (,  {(,  (constraint  $Constraint) (,  (constraint-must-capture  $Constraint no) (constraint-continue-captures  $Constraint no))) } ()))  True)	
  (= (-->  (constraint-defs  $Constraint) (,  (constraints) (constraint-def  $Constraint)))  True)



  (= (-->  (constraint-def  $Constraint) (,  {(constraint  $Constraint) } (,  (must-capture  $Constraint) (continue-captures  $Constraint))))  True)

  (= (-->  (must-capture  $Constraint) (,  {(constraint-must-capture  $Constraint no) } ()))  True) 
  (= (-->  (must-capture  $Constraint) (,  (must_capture) {(constraint-must-capture  $Constraint yes) }))  True)

  (= (-->  (continue-captures  $Constraint) (,  () {(constraint-continue-captures  $Constraint no) }))  True)
  (= (-->  (continue-captures  $Constraint) (,  {(constraint-continue-captures  $Constraint yes) } (continue_captures)))  True)



;
; promote_def(Special) --> {promotion(Special,Prom)},
;
; promotion(Prom).

  (= (-->  (promote-def  $Prom) (,  {(decision  $Prom) } (promotion-decision  $Prom)))  True)
  (= (-->  (promote-def  (promote  $Prom)) (,  (promote_to) (piece-name  $Prom)))  True)


;
; Added chooser to promote within his own, other guys, or anyones
;
; piece_names. Must make sure this works, and generator uses it!

  (= (-->  (promotion-decision  $D) (,  {(,  (decision  $D) (,  (decision-chooser  $D $C) (decision-options  $D $O))) } (,  (decision) (,  (player  $C) (,  line (,  (tab  10) (,  (options) (description  $O))))))))  True)



  (= (-->  line (,  {(parsing-mode  printing) } (line)))  True)
  (= (-->  line (,  {(parsing-mode  parsing) } ()))  True)

  (= (-->  (tab  $T) (,  {(parsing-mode  printing) } ((tab  $T))))  True)
  (= (-->  (tab  $T) (,  {(parsing-mode  parsing) } ()))  True)

  (= (-->  (true  any) ())  True)


  (= (-->  semi (;))  True)

  (= (-->  colon (:))  True)

  (= (-->  comma (,))  True)

  (= (-->  openp (())  True)

  (= (-->  closep ()))  True)

  (= (-->  openb ([))  True)

  (= (-->  closeb (]))  True)

  (= (-->  openbrace ({))  True)

  (= (-->  closebrace (}))  True)

  (= (-->  period (.))  True)


  (= (-->  identifier (,  ($X) {(atom  $X) }))  True)


;
; ================================================================================
;
; Printing Assignments to strings
;
; ================================================================================
;
; Along with the move and game grammars, the standard form for assignments
;
; in the grammar above is used to transmit initial random assignments to the
;
; players at play-time.
;
; The following routines convert between the grammatical (token)
;
; representation, my system's internal representation, and a character string
;
; encoding of these tokens for communication purposes.
;
; Note the period is added as in all other string representations I use,
;
; to determine end of strings.


  (= (assignments-string $Assignment $String)  
    (var $String)
    (set-det)
    (assignments $Assignment $Tokens 
      (Cons  . $Rest))
    (print-tokens-to-string $Tokens $String))
  (= (assignments-string $Assignment $String)  
    (var $Assignment)
    (read-tokens-from-string $String $Tokens)
    (assignments $Assignment $Tokens 
      (Cons  . $Rest)))

;
; ================================================================================
;
; PORTRAY functions to print certain grammatical constructs
;
; for interface purposes.
;
; ================================================================================


  (= (portray-square (square $X $Y))  
    (with-alpha-squares (gsquare (square $X $Y) $S Nil))
    (print-tokens $S))


  (= (portray-player player)  
    (write white))
  (= (portray-player opponent)  
    (write black))


  (= (portray-piece (piece $A $B))  
    (piece 
      (piece $A $B) $S Nil)
    (print-tokens $S))


  (= (portray-moving (move $Piece $Player $From $To))  
    (moving 
      (move $Piece $Player $From $To) $S Nil)
    (print-tokens $S))
	

  (= (portray-game (game $Name $Board $Pieces $Goals $Constraints))  
    (format "<Game: ~w>" 
      (:: $Name)))

;
; ================================================================================
;
; Printing games
;
; ================================================================================


;
; GEN(L): generates an internal game representation,
;
; and then returns the list which is its grammatical representation
;
; using the grammar above.
;
; 

  (= (gen $L)  
    (generate-game $G)
    (game $G $L $_))


;
; RANDOM_GAME_TO_FILE(+File)
;
; -------------------
;
; The predicate used most commonly for generating new games.
;
; Outputs the game to File.
;
; 

  (= (random-game-to-file $File)  
    (print-gen-game-to-file $File))
	


  (= print-gen-game  
    (record-seed 
      (gen $G) 
      (print-tokens $G)))


  (= (print-gen-game-to-file $File)  
    (record-seed 
      (generate-game $G) set-printing-mode 
      (game $G $GameList $_) 
      (print-game-to-file $GameList $File) set-parsing-mode))


  (= (print-game-struct $G)  
    (set-printing-mode 
      (game $G $GameList $_) set-parsing-mode 
      (print-tokens $GameList)))
	


  (= (prettify-game $GameList $Pretty)  
    (set-parsing-mode 
      (game $G $GameList $_) set-printing-mode 
      (game $G $Pretty) set-parsing-mode))
	

  (= (pretty-print-game-to-file $GameList $File)  
    (prettify-game $GameList $Pretty)
    (print-game-to-file $Pretty $File))


;
; PRINT_GAME_TO_FILE(+Game,+File)
;
; Outputs a game to file File.game.

  (= (print-game-to-file $Game $File)  
    ( (sys-suffixed-filename $File game $GameFile) 
      (format "~nWriting game to file: ~w~n" 
        (:: $GameFile)) 
      (tell $GameFile) write-old-seed 
      (print-tokens $Game) told))

;
; If we've just generated a game, print its
;
; seed as a comment.

  (= write-old-seed  
    ( (old-seed $Seed) 
      (set-det) nl 
      (write '% seed: ') 
      (write $Seed) nl))
  (= write_old_seed  True)

	
;
; ================================================================================
;
; Reading games from files, printing back.
;
; ================================================================================

;
; ;; Read from pascal-like game output, into list.
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
; read_game_from_file_to_list('~/MeTTa/play/game.2.5.92',Game).
;
; print_read_game('~/MeTTa/play/game.2.5.92').


  (= (print-read-game $File)  
    ( (format "~nReading game from file~n" Nil) 
      (read-game-from-file-to-list $File $Game) 
      (format "~nRead game from file~n" Nil) set-parsing-mode 
      (format "~nParsing game in parse mode~n" Nil) 
      (game $G $Game Nil) set-printing-mode 
      (format "~nParsing game in print mode~n" Nil) 
      (game $G $Game1 Nil) 
      (format "~nPrinting game~n" Nil) 
      (print-tokens $Game1)))



  (= (read-game-from-file-to-list $File $Game)  
    (format "~nReading game from file: ~w~n" 
      (:: $File))
    (read-tokens-from-file $File $Game))


  (= (read-game-from-string-to-list $String $Game)  
    (format "~nReading game from string. ~n" Nil)
    (read-tokens-from-string $String $Game))



  (= (read-game-to-list $Game)  
    (read-tokens $Game))
	


