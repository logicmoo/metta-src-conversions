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

;
; 


;
; 

;
; 

;
; 


  (= 
    (sym-dir $Dir $Syms $DirOut) 
    (in-symmetric-closure $Dir $Syms $DirOut))
; 



;
; 

;
; 


  (= 
    (in-symmetric-closure $Dir $Syms $DirOut) 
    ( (closure 
        (:: $Dir) $Syms $Dirs) (member $DirOut $Dirs)))
; 



  (= 
    (closure $Set $Transforms $Closure) 
    (close1 $Set $Transforms $Set $Closure))
; 



  (= 
    (close1  () $_ $T $T) True)
; 

  (= 
    (close1 
      (Cons  $A $As) $Ts $Table $Close) 
    ( (close-item $Ts $A $As $Table $NewAs $NewTable) (close1 $NewAs $Ts $NewTable $Close)))
; 



  (= 
    (close_item  () $_ $As $T $As $T) True)
; 

  (= 
    (close-item 
      (Cons  $T $Ts) $A $As $TIn $NewAs $TOut) 
    ( (transform-item $A $T $ATrans) 
      (schedule-if $ATrans $As $TIn $NewAs1 $T1) 
      (close-item $Ts $A $NewAs1 $T1 $NewAs $TOut)))
; 



  (= 
    (schedule-if $Item $As $Table $As $Table) 
    ( (member $Item $Table) (set-det)))
; 

  (= 
    (schedule_if  $Item $As $Table 
      (Cons  $Item $As) 
      (Cons  $Item $Table)) True)
; 

	
	

  (= 
    (transform-item $Dir $Sym $NewDir) 
    (symmetry $Sym $Dir $NewDir))
; 



  (= 
    (symmetry forward 
      (dir $X $Y) 
      (dir $X $Y1)) 
    (negates $Y $Y1))
; 

  (= 
    (symmetry side 
      (dir $X $Y) 
      (dir $X1 $Y)) 
    (negates $X $X1))
; 

  (= 
    (symmetry  rotation 
      (dir  $X $Y) 
      (dir  $Y $X)) True)
; 




  (= 
    (negates $N $N1) 
    (| 
      (det-if-then 
        (var $N) 
        (is $N 
          (* $N1 -1))) 
      (det-if-then otherwise 
        (is $N1 
          (* $N -1)))))
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
    (game-movement $Game $Movement) 
    ( (game-piece-def $Game $_ $Def) (piece-defines-movement $Def $Movement)))
; 



  (= 
    (piece-defines-movement $Def $Movement) 
    ( (piece-movement $Def $CompMovement) (movement-component $CompMovement $Movement)))
; 

  (= 
    (piece-defines-movement $Def $M) 
    ( (piece-capture $Def $CompCapture) 
      (capture-component $CompCapture $Capture) 
      (capture-has-movement $Capture $M)))
; 




  (= 
    (game-syms-dir $Game $Syms $Dir) 
    ( (game-movement $Game $M) 
      (movement-dir $M $Dir) 
      (movement-syms $M $Syms)))
; 



  (= 
    (unique-sym-dirs $SymDirs) 
    (setof 
      (- 
        (- $Dir $Syms) $Sym) 
      (^ $Game 
        (, 
          (some-player-game $Game) 
          (game-sym-dir $Game $Dir $Syms $Sym))) $SymDirs))
; 



  (= 
    (game-sym-dir $Game $Dir $Syms $Sym) 
    ( (game-syms-dir $Game $Syms $Dir) (in-symmetric-closure $Dir $Syms $Sym)))
; 



  (= 
    (some-player-game $Game) 
    (or 
      (player-current-game $Game) 
      (opponent-current-game $Game)))
; 




  (= 
    (index-sym-dirs) 
    (with-temp-file syms $File 
      (, 
        (index-sym-dirs-to-file $File) 
        (compile $File))))
; 



  (= 
    (index-sym-dirs-to-file $File) 
    ( (format "~nIndexing Symmetries to file: ~w~n" 
        (:: $File)) 
      (assert-sym-indices) 
      (with-output-file $File write 
        (, 
          (print-sym-overwrite) 
          (listing sym-index) 
          (overwrite-game))) 
      (abolish (/ sym-index 3))))
; 




  (= 
    (assert-sym-indices) 
    ( (abolish (/ sym-index 3)) 
      (unique-sym-dirs $Dirs) 
      (whenever 
        (member 
          (- 
            (- $Dir $Syms) $Sym) $Dirs) 
        (assert-sym-index $Dir $Syms $Sym))))
; 



  (= 
    (assert-sym-index $Dir $Syms $Sym) 
    ( (dir-key $Dir $Key) (add-symbol  &self (sym_index  $Key $Syms $Sym))))
; 




  (= 
    (print-all-syms) 
    ( (unique-sym-dirs $L) 
      (member 
        (- 
          (- $Dir $_) $Sym) $L) 
      (format "~p --> ~p~n" 
        (:: $Dir $Sym)) 
      (fail)))
; 



  (= 
    (indexed-sym-dir $Dir $Syms $Sym) 
    ( (dir-key $Dir $Key) (sym-index $Key $Syms $Sym)))
; 




  (= 
    (print-sym-overwrite) 
    (format "\n% Compiled Symmetry File\n\n:- abolish(sym_dir/3).\n\nsym_dir(Dir,Syms,Sym) :- \n\tindexed_sym_dir(Dir,Syms,Sym).\n\n" Nil))
; 




  (= 
    (overwrite-game) 
    ( (print-game-overwrite) 
      (listing player-current-game) 
      (listing opponent-current-game)))
; 




  (= 
    (print-game-overwrite) 
    (format "\n% Compiled Game File\n\n:- abolish(player_current_game/1).\n:- abolish(opponent_current_game/1).\n\n" Nil))
; 




  (= 
    (dir-key 
      (dir $X $Y) $Key) 
    (is $Key 
      (+ 
        (* 1000 $X) $Y)))
; 



  (= 
    (square-key 
      (square $X $Y) $Key) 
    (is $Key 
      (+ 
        (* 1000 $X) $Y)))
; 



;
; 

;
; 




  (= 
    (time1) 
    ( (timing (fastsyms $FastS)) 
      (nl) 
      (print $FastS) 
      (timing (slowsyms $SlowS)) 
      (nl) 
      (print $SlowS)))
; 



  (= 
    (time-sym $N) 
    ( (timing (dotimes $N (fastsyms $_))) (timing (dotimes $N (slowsyms $_)))))
; 




  (= 
    (fastsyms $SS) 
    (setof $S 
      (indexed-sym-dir 
        (dir 2 1) 
        (:: forward side rotation) $S) $SS))
; 



  (= 
    (slowsyms $SS) 
    (setof $S 
      (in-symmetric-closure 
        (dir 2 1) 
        (:: forward side rotation) $S) $SS))
; 




; 

