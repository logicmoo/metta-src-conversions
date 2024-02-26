;                                           (convert-to-metta-file  parse1 $_228740 metagame/theory/parse1.pl metagame/theory/parse1.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; parse1.pl

;
; Experimenting with different versions of generating and
;
; saving games.


  (= generate-make-test-game  
    (generate-game $G)
    (make-test-game $G))

;
; GENERATE_AND_LOAD(+File)
;
; Generates a new game into file 'File.game',
;
; then loads it.

  (= (generate-and-load $File)  
    (random-game-to-file $File)
    (load-game $File))


;
; LOAD_GAME(+GameName)
;
; Finds a file GameName.game in a library directory.
;
; Then loads this file as the current (test) game.

  (= (load-game $Name)  
    (find-game-file $Name $File)
    (file-make-test-game $File))

;
; Uses predicate in sysdev.pl

  (= (find-game-file $Name $File)  
    (find-suffixed-library-file $Name game $File))


;
; file_make_test_game('~/MeTTa/play/chess.game').

  (= (file-make-test-game $File)  
    (read-game-from-file-to-list $File $Game)
    (parse-make-test-game $Game))
	
;
; STRING_MAKE_TEST_GAME(+String)

  (= (string-make-test-game $String)  
    (read-game-from-string-to-list $String $Game)
    (parse-make-test-game $Game))



  (= (parse-make-test-game $Game)  
    (set-parsing-mode 
      (format "~nParsing game.~n" Nil) 
      (game $G $Game Nil) 
      (make-test-game $G)))
	

  !(dynamic (, (/ player-current-game 1) (/ opponent-current-game 1)))

;
; index_sym_dirs/0 defined in compile_syms.pl
;
; To compare speed without it, comment out that line.


  (= (make-test-game $G)  
    ( (game-name $G $Name) 
      (format "~nMaking ~w the current test game~n" 
        (:: $Name)) 
      (abolish (/ player-current-game 1)) 
      (abolish (/ opponent-current-game 1)) 
      (add-atom  &self 
        (player-current-game  $G)) 
      (invert $G opponent $G1) 
      (add-atom  &self 
        (opponent-current-game  $G1)) maybe-compile-syms))


  (= maybe-compile-syms  
    (det-if-then-else 
      (parameter compile-symmetries on) index-sym-dirs True))



