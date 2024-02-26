;                                           (convert_to_metta_file  gen_menu $_161002 metagame/play/gen_menu.pl metagame/play/gen_menu.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; gen_menu.pl

;
; This menu is accessible from the top-level menu.

  (= (gen-top)  
    (generate))

;
; ======================================================================
;
; Game Generation Menu
;
; ======================================================================


  (= (generate)  
    (menu-command "~nEnter <generation> command ('help.'  gives  more information)~n" gen Nil)
    (set-det)
    (continue-generate))
  (= (generate)  
    (format "~nI did not understand your command. Please try again!~n" Nil)
    (generate))

;
; CONTINUE_GENERATE.
;
; Here we always continue from this menu.
;
; The only way to get out is to use the 'done' command,
;
; which just calls metagame again.


  (= (continue-generate)  
    (generate))

;
; ----------------------------------------
;
; GENERATE menu commands
;
; ----------------------------------------


  (= (help-gen)  
    (help-gen1)
    (help-gen2))


  (= (help-gen1)  
    (format "\nGenerating a Game:\n------------------------------\ngames_library.       => show games in library\ngenerate <file>.     => generate (and load) random game, save as <file>.game\ngame <file>.         => loads <file>.game as the current game\ncompile {on/off}.    => set whether to compile symmetries when loading a game.\nquick.               => short form to turn of compiling symmetries. \ndone.                => return to top-level menu\n" Nil))


  (= (help-gen2)  
    (format "\nExamining Game and Changing System State\n-----------------------------------------\npieces.              => show the names of the pieces in the current game\ndefine <piece>.      => print the definition of <piece> in the current game\ngoals.               => print the goals of the current game\nboard.               => print board definition of the current game\nrules.               => print the full rules of the current game\nset <p> <v>.         => set generator parameter <p> to value <v>\nset <p>.             => set generator parameter <p> ('help set.')\nset.                 => show generator parameter settings\nrandomize <N>        => use random seed #<N> (N = 1..10)\n(un)trace {options}  => trace some system behavior ('help trace.') \ncd <dir>.            => change current directory to <dir> ('help cd.')\npwd.                 => show current directory name\nls.                  => show contents of current directory\nprolog. (abort)      => abort to prolog\nquit.                => exit session (back to shell)\n" Nil))



  (= (done-gen)  
    (metagame))


  (= (set-gen $P $V)  
    (set-gen-parameter $P $V))

  (= (set-gen $P)  
    (change-gen-param $P))

  (= (set-gen)  
    (show-gen-parameters))


  (= (randomize-gen $N)  
    (randomize $N))


  (= (generate-gen $File)  
    (generate-and-load $File))
	
  (= (generate-gen)  
    (generate-and-load random))


  (= (game-gen $File)  
    (load-game $File))


  (= (games-library-gen)  
    (games-library))


  (= (cd-gen $Dir)  
    (cd-print $Dir))


  (= (pwd-gen)  
    (pwd-print))


  (= (ls-gen)  
    (ls))


  (= (define-gen $PieceName)  
    (show-piece-definition $PieceName))


  (= (goals-gen)  
    (show-game-goals))


  (= (rules-gen)  
    (show-rules))


  (= (pieces-gen)  
    (show-piece-names))


  (= (board-gen)  
    (show-board))


  (= (quick-gen)  
    (set-parameter compile-symmetries off))


  (= (compile-gen $OnOff)  
    (set-parameter compile-symmetries $OnOff))



  (= (restart-gen)  
    (format "~nRestarting ...~n" Nil)
    (metagame))


  (= (quit-gen)  
    (print-quit))


  (= (prolog-gen)  
    (print-abort))


  (= (abort-gen)  
    (print-abort))


  (= (verbose-gen)  
    (set-verbose))


  (= (quiet-gen)  
    (set-quiet))


;
; -----------------------------------------------------------------
;
; Tracing
;
; -----------------------------------------------------------------


  (= (trace-gen $Module)  
    (set-tracing $Module on))
  (= (trace-gen $Module $Component)  
    (set-tracing $Module $Component on))


  (= (untrace-gen $Module)  
    (set-tracing $Module off))
  (= (untrace-gen $Module $Component)  
    (set-tracing $Module $Component off))


  (= (list-tracing-gen)  
    (list-tracing))

