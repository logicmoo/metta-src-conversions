;                                           (convert_to_metta_file  humanist $_141280 metagame/comms/humanist.pl metagame/comms/humanist.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; humanist.pl


  (= (move $Role $Move $SIn $SOut)  
    (my-name $Me)
    (format "~nPlayer ~p, as ~p, wants your help to select his move.~n" 
      (:: $Me $Role))
    (human-choose $Role $Move $SIn $SOut))

