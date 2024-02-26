;                                           (convert_to_metta_file  instantist $_203568 metagame/comms/instantist.pl metagame/comms/instantist.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; instantist.pl

  (= (move $Role $Move $SIn $SOut)  
    (my-name $Me)
    (format "~nPlayer ~p, as ~p, will select an INSTANT move.~n" 
      (:: $Me $Role))
    (instant-choose $Role $Move $SIn $SOut))

