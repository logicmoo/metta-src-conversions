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
    (generate-make-test-game) 
    ( (generate-game $G) (make-test-game $G)))
; 


;
; 

;
; 

;
; 


  (= 
    (generate-and-load $File) 
    ( (random-game-to-file $File) (load-game $File)))
; 



;
; 

;
; 

;
; 


  (= 
    (load-game $Name) 
    ( (find-game-file $Name $File) (file-make-test-game $File)))
; 


;
; 


  (= 
    (find-game-file $Name $File) 
    (find-suffixed-library-file $Name game $File))
; 



;
; 


  (= 
    (file-make-test-game $File) 
    ( (read-game-from-file-to-list $File $Game) (parse-make-test-game $Game)))
; 

	
;
; 


  (= 
    (string-make-test-game $String) 
    ( (read-game-from-string-to-list $String $Game) (parse-make-test-game $Game)))
; 




  (= 
    (parse-make-test-game $Game) 
    ( (set-parsing-mode) 
      (format "~nParsing game.~n" Nil) 
      (game $G $Game Nil) 
      (make-test-game $G)))
; 

	

  !(dynamic (, (/ player-current-game 1) (/ opponent-current-game 1)))
; 


;
; 

;
; 



  (= 
    (make-test-game $G) 
    ( (game-name $G $Name) 
      (format "~nMaking ~w the current test game~n" 
        (:: $Name)) 
      (abolish (/ player-current-game 1)) 
      (abolish (/ opponent-current-game 1)) 
      (add-symbol  &self 
        (player_current_game  $G)) 
      (invert $G opponent $G1) 
      (add-symbol  &self 
        (opponent_current_game  $G1)) 
      (maybe-compile-syms)))
; 



  (= 
    (maybe-compile-syms) 
    (det-if-then-else 
      (parameter compile-symmetries on) index-sym-dirs True))
; 




