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
    (metagame) 
    ( (recover-metagame-state) 
      (menu-command "~nEnter a command ('help.'  gives  more information)~n" top Nil) 
      (set-det) 
      (metagame)))
; 

  (= 
    (metagame) 
    ( (format "~nI did not understand your command. Please try again!~n" Nil) (metagame)))
; 



;
; 


  (= 
    (recover-metagame-state) 
    ( (restore-parameters) (recover-grammar)))
; 
 


  (= 
    (game-top $File) 
    (load-game $File))
; 



  (= 
    (player-top white $Player) 
    ( (format "~p moves will now be chosen by: ~p~n" 
        (:: white $Player)) (set-parameter player-method $Player)))
; 

  (= 
    (player-top black $Player) 
    ( (set-parameter opponent-method $Player) (format "~p moves will now be chosen by: ~p~n" (:: black $Player))))
; 



  (= 
    (players-top $Player $Opp) 
    ( (player-top white $Player) (player-top black $Opp)))
; 
	

  (= 
    (players-top) 
    ( (get-player-color player $White) 
      (get-player-color opponent $Black) 
      (format "Playing ~p as ~p vs. ~p as ~p.~n" 
        (:: $White player $Black opponent))))
; 


	



  (= 
    (show-players-top) 
    (get-players $_ $_))
; 




  (= 
    (games-library-top) 
    (games-library))
; 




  (= 
    (games-library) 
    ( (games-library-directory $D) (games-library $D)))
; 



  (= 
    (games-library $D) 
    ( (current-directory $Current) 
      (absolute-file-name $D $AbsD) 
      (cd $AbsD) 
      (= $Games *.game) 
      (format "~nThe following games are available in directory <~p>:~n~n" 
        (:: $AbsD)) 
      (shell (:: ls $Games)) 
      (cd $Current)))
; 



  (= 
    (games-in-directory $D) 
    (games-library $D))
; 




  (= 
    (cd-top $Dir) 
    (cd-print $Dir))
; 



  (= 
    (cd-print $Dir) 
    ( (format "~nChanging current directory to: ~p~n~n" 
        (:: $Dir)) (cd $Dir)))
; 



  (= 
    (pwd-top) 
    (pwd-print))
; 



  (= 
    (ls-top) 
    (ls))
; 



  (= 
    (pwd-print) 
    ( (current-directory $D) (format "~nCurrent directory is: ~p~n~n" (:: $D))))
; 




  (= 
    (define-top $PieceName) 
    (show-piece-definition $PieceName))
; 



  (= 
    (goals-top) 
    (show-game-goals))
; 




  (= 
    (show-game-goals) 
    ( (player-current-game $G) 
      (game-name $G $GameName) 
      (game-goal $G $Def) 
      (set-printing-mode) 
      (with-alpha-squares (goal-defs $Def $String Nil)) 
      (set-parsing-mode) 
      (format "~nGame <~p> has the following goals: ~n~n" 
        (:: $GameName)) 
      (print-tokens $String) 
      (nl) 
      (nl)))
; 



  (= 
    (rules-top) 
    (show-rules))
; 



  (= 
    (show-rules) 
    ( (player-current-game $G) 
      (game-name $G $GameName) 
      (format "~nGame <~p> is defined as follows: ~n~n" 
        (:: $GameName)) 
      (with-alpha-squares (print-game-struct $G)) 
      (nl) 
      (nl)))
; 




  (= 
    (pieces-top) 
    (show-piece-names))
; 



  (= 
    (board-top) 
    (show-board))
; 



  (= 
    (show-board) 
    ( (player-current-game $G) 
      (game-name $G $GameName) 
      (game-board $G $Def) 
      (set-printing-mode) 
      (with-alpha-squares (board $Def $String Nil)) 
      (set-parsing-mode) 
      (format "~nGame <~p> has the board information: ~n~n" 
        (:: $GameName)) 
      (print-tokens $String) 
      (nl) 
      (nl)))
; 





  (= 
    (set-top $P $V) 
    (set-parameter $P $V))
; 


  (= 
    (set-top) 
    (show-parameters))
; 


;
; 

;
; 


  (= 
    (setg-top $P $V) 
    (add-global $P $V))
; 



  (= 
    (showg-top) 
    (showg))
; 




  (= 
    (randomize-top $N) 
    (randomize $N))
; 



  (= 
    (quit-top) 
    (print-quit))
; 



  (= 
    (print-quit) 
    ( (format "~nBye!~n" Nil) (halt)))
; 





  (= 
    (abort-top) 
    (print-abort))
; 



  (= 
    (prolog-top) 
    (print-abort))
; 



  (= 
    (print-abort) 
    ( (format "~nTo return to METAGAME, type: 'metagame.'~n" Nil) (abort)))
; 



  (= 
    (play-top) 
    (start))
; 



  (= 
    (start-top) 
    (start))
; 




  (= 
    (generate-top $File) 
    (generate-and-load $File))
; 

	
  (= 
    (generate-top) 
    (generate-and-load random))
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
    (clock-top) 
    (print-clock))
; 


  (= 
    (clock-top unlimit) 
    (clock-unlimit))
; 


  (= 
    (clock-top print) 
    (print-clock))
; 


  (= 
    (clock-top reset) 
    (reset-clock))
; 


  (= 
    (clock-top adjust $Color $Time) 
    ( (player-color $Player $Color) (adjust-player-clock $Player $Time)))
; 


;
; 

;
; 

;
; 



  (= 
    (trace-top $Module) 
    (set-tracing $Module on))
; 

  (= 
    (trace-top $Module $Component) 
    (set-tracing $Module $Component on))
; 



  (= 
    (untrace-top $Module) 
    (set-tracing $Module off))
; 

  (= 
    (untrace-top $Module $Component) 
    (set-tracing $Module $Component off))
; 



  (= 
    (list-tracing-top) 
    ( (traced-modules $M) (format "The following modules are being traced: ~n~p~n" (:: $M))))
; 


