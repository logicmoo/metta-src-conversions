;                                           (convert-to-metta-file  randomist $_13434 metagame/comms/randomist.pl metagame/comms/randomist.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; randomist.pl


  (= (move $Role $Move $SIn $SOut)  
    (my-name $Me)
    (format "~nPlayer ~p, as ~p, will select a RANDOM move.~n" 
      (:: $Me $Role))
    (random-choose $Role $Move $SIn $SOut)
    (verbosely-format "~nPlayer ~p, as ~p, has chosen RANDOM move: ~p.~n" 
      (:: $Me $Role $Move)))
; ; set_verbose, ; spy(random_choose), ; debug,

