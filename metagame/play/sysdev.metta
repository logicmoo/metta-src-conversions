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
    (my_absolute_file_name  $N $N) True)
; 



;
; 

;
; 


  (= 
    (my-use-module $M) 
    (det-if-then-else is-profiling 
      (profiling-load $M) 
      (use-module $M)))
; 



  (= 
    (my-ensure-loaded $M) 
    (det-if-then-else is-profiling 
      (profiling-load $M) 
      (ensure-loaded $M)))
; 



  (= 
    (profiling-load $M) 
    (compile $M))
; 


;
; 


  !(my-ensure-loaded ../misc/aux)
; 




  (= 
    (make-library-directory $Path) 
    ( (my-absolute-file-name $Path $D) (add-symbol  &self (library_directory  $D))))
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
    (metagame-subpath $Path $Dir) 
    ( (metagame-directory $Metagame) 
      (concat $Metagame / $Sub1) 
      (concat $Sub1 $Path $SubDir) 
      (my-absolute-file-name $SubDir $Dir)))
; 



  (= 
    (make-metagame-subpath $Path) 
    ( (metagame-subpath $Path $Dir) (make-library-directory $Dir)))
; 



  (= 
    (find-index-preds-file) 
    ( (metagame-subpath state/index-preds.pl $F) 
      (abolish (/ index-preds-file 1)) 
      (add-symbol  &self 
        (index_preds_file  $F))))
; 




  (= 
    (find-games-library) 
    ( (metagame-subpath games $D) 
      (abolish (/ games-library-directory 1)) 
      (add-symbol  &self 
        (games_library_directory  $D))))
; 




  (= 
    (find-theory-directory) 
    ( (metagame-subpath theory $D) 
      (abolish (/ theory-directory 1)) 
      (add-symbol  &self 
        (theory_directory  $D))))
; 



  (= 
    (find-dynamic-preds-file) 
    ( (metagame-subpath theory/dynamic-preds.pl $F) 
      (abolish (/ dynamic-preds-file 1)) 
      (add-symbol  &self 
        (dynamic_preds_file  $F))))
; 

 


  (= 
    (bind-environment-paths) 
    ( (make-metagame-subpath generator) 
      (make-metagame-subpath misc) 
      (make-metagame-subpath play) 
      (make-metagame-subpath games) 
      (make-metagame-subpath evals) 
      (make-metagame-subpath state) 
      (make-metagame-subpath theory) 
      (make-metagame-subpath comms) 
      (make-metagame-subpath coding) 
      (make-metagame-subpath learning) 
      (find-dynamic-preds-file) 
      (find-index-preds-file) 
      (find-games-library) 
      (find-theory-directory) 
      (add-symbol  &self 
        (library_directory  .))))
; 



;
; 



  (= 
    (sys_filename_suffix  prolog .pl) True)
; 

  (= 
    (sys_filename_suffix  state_compile _stat) True)
; 

  (= 
    (sys_filename_suffix  game .game) True)
; 

  (= 
    (sys_filename_suffix  eval .eval) True)
; 

  (= 
    (sys_filename_suffix  record .rec) True)
; 




  (= 
    (sys-suffixed-filename $File $Sys $Name) 
    ( (sys-filename-suffix $Sys $Suf) (suffixed-filename $File $Suf $Name)))
; 

	

  (= 
    (suffixed-filename $File $Suf $Name) 
    (bi-concat $File $Suf $Name))
; 




  (= 
    (find-suffixed-library-file $Name $Suffix $File) 
    ( (nofileerrors) 
      (sys-suffixed-filename $Name $Suffix $File1) 
      (exists-absolute-file-name 
        (library $File1) $File) 
      (set-det) 
      (fileerrors)))
; 

  (= 
    (find-suffixed-library-file $Name $Suffix $File) 
    ( (fileerrors) 
      (format "~nError: Couldn't find file ~w.~p~n" 
        (:: $Name $Suffix)) 
      (fail)))
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
    (load-main-system-files) 
    (compile (library player-files)))
; 



;
; 

;
; 

;
; 


  (= 
    (load-metagame) 
    ( (bind-environment-paths) 
      (load-main-system-files) 
      (load-system library) 
      (load-system generator) 
      (load-system play) 
      (load-system analysis) 
      (compile-and-load-player) 
      (add-system-portrayals)))
; 




  (= 
    (load-system $System) 
    ( (system-files $System $Files) (whenever (member $F $Files) (compile (library $F)))))
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
    (enter-metagame) 
    ( (metagame-version $V) 
      (format '~nMETAGAME Game-Playing Workbench ~p~n' 
        (:: $V)) 
      (format 'Copyright (c) 1992 Barney D. Pell~n~n' Nil) 
      (unix (argv $Argv)) 
      (process-commands $Argv) 
      (metagame)))
; 



  (= 
    (process-commands $Argv) 
    ( (det-if-then-else 
        (append $_ 
          (Cons  seed 
            (Cons  $N $_)) $Argv) 
        (randomize $N) True) 
      (whenever 
        (, 
          (append $_ 
            (Cons  $P 
              (Cons  $V $_)) $Argv) 
          (parameter $P $_)) 
        (set-parameter $P $V)) 
      (whenever 
        (append $_ 
          (Cons  file 
            (Cons  $D $_)) $Argv) 
        (compile $D))))
; 



;
; 

;
; 

;
; 



  (= 
    (set-parameter $P $V) 
    (det-if-then-else 
      (remove-symbol  &self 
        (parameter  $P $_)) 
      (add-symbol  &self 
        (parameter  $P $V)) 
      (det-if-then otherwise 
        (format 'Unknown parameter <~p>!~n' 
          (:: $P)))))
; 



  (= 
    (setp $P $V) 
    (set-parameter $P $V))
; 


;
; 


  (= 
    (add-parameter $P $V) 
    ( (remove-all-symbols  &self 
        (parameter  $P $_)) (add-symbol  &self (parameter  $P $V))))
; 
 


  (= 
    (change-parameter $P $Old $V) 
    (det-if-then-else 
      (remove-symbol  &self 
        (parameter  $P $Old)) 
      (add-symbol  &self 
        (parameter  $P $V)) 
      (det-if-then otherwise 
        (format 'Unknown parameter <~p>!~n' 
          (:: $P)))))
; 




  (= 
    (show) 
    (show-parameters))
; 



  (= 
    (show-parameters) 
    ( (listing parameter) 
      (getrand $R) 
      (format '~nrandom seed = ~p~n' 
        (:: $R))))
; 




  (= 
    (save-parameters) 
    ( (findall 
        (- $P $V) 
        (parameter $P $V) $Params) 
      (remove-all-symbols  &self 
        (saved_parameters  $_)) 
      (add-symbol  &self 
        (saved_parameters  $Params))))
; 



  (= 
    (restore-parameters) 
    (det-if-then-else 
      (remove-symbol  &self 
        (saved_parameters  $Params)) 
      (restore-parameters $Params) True))
; 


  (= 
    (restore_parameters  ()) True)
; 

  (= 
    (restore-parameters (Cons  (- $P $V) $Rest)) 
    ( (set-parameter $P $V) (restore-parameters $Rest)))
; 



;
; 

;
; 

;
; 



  (= 
    (set-global $P $V) 
    (det-if-then-else 
      (remove-symbol  &self 
        (global  $P $_)) 
      (add-symbol  &self 
        (global  $P $V)) 
      (det-if-then otherwise 
        (format 'Unknown global <~p>!~n' 
          (:: $P)))))
; 


;
; 


  (= 
    (add-global $P $V) 
    ( (remove-all-symbols  &self 
        (global  $P $_)) (add-symbol  &self (global  $P $V))))
; 
 


  (= 
    (change-global $P $Old $V) 
    (det-if-then-else 
      (remove-symbol  &self 
        (global  $P $Old)) 
      (add-symbol  &self 
        (global  $P $V)) 
      (det-if-then otherwise 
        (format 'Unknown global <~p>!~n' 
          (:: $P)))))
; 




  (= 
    (setg $P $V) 
    (set-global $P $V))
; 
 


  (= 
    (showg) 
    (show-globals))
; 



  (= 
    (show-globals) 
    ( (listing global) 
      (getrand $R) 
      (format '~nrandom seed = ~p~n' 
        (:: $R))))
; 




  (= 
    (save-globals) 
    ( (findall 
        (- $P $V) 
        (global $P $V) $Params) 
      (remove-all-symbols  &self 
        (saved_globals  $_)) 
      (add-symbol  &self 
        (saved_globals  $Params))))
; 



  (= 
    (restore-globals) 
    (det-if-then-else 
      (remove-symbol  &self 
        (saved_globals  $Params)) 
      (restore-globals $Params) True))
; 


  (= 
    (restore_globals  ()) True)
; 

  (= 
    (restore-globals (Cons  (- $P $V) $Rest)) 
    ( (set-global $P $V) (restore-globals $Rest)))
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
    (verbosely $Call) 
    (det-if-then-else verbose 
      (call $Call) True))
; 


;
; 


  (= 
    (verbosely-format $String $Args) 
    (verbosely (format $String $Args)))
; 



  (= 
    (verbose) 
    ( (parameter verbosity $X) (> $X 0)))
; 



  (= 
    (set-verbose) 
    (set-parameter verbosity 1))
; 


  (= 
    (set-quiet) 
    (set-parameter verbosity 0))
; 



;
; 

;
; 

;
; 



  (= 
    (portray-conj (, $X $Y)) 
    ( (print $X) 
      (write ,) 
      (print $Y)))
; 


  (= 
    (portray-disj (or $X $Y)) 
    ( (print $X) 
      (write  ; ) 
      (print $Y)))
; 


  (= 
    (portray-if (= $X $Y)) 
    ( (write () 
      (writeq $X) 
      (write :-) 
      (print $Y) 
      (write ))))
; 


  (= 
    (portray-var $N) 
    ( (put 124) (write $N)))
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
    (portray-state (state $S)) 
    ( (format "<State: ~n" Nil) 
      (print-state (state $S)) 
      (format ">~n" Nil)))
; 



  (= 
    (add-portray $Func) 
    ( (functor $Goal $Func 1) 
      (arg 1 $Goal $Term) 
      (add-symbol  &self 
        (:-  
          (portray  $Term) $Goal))))
; 



  (= 
    (system_portrayals  
      (portray_conj portray_disj portray_if portray_var portray_piece portray_player portray_square portray_moving portray_game portray_state)) True)
; 



  (= 
    (add-system-portrayals) 
    ( (system-portrayals $Ps) (add-portrayals $Ps)))
; 



  (= 
    (add-portrayals $Ps) 
    (whenever 
      (member $P $Ps) 
      (add-portray $P)))
; 





