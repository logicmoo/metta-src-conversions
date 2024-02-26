;                                           (convert-to-metta-file  dynamic_load $_102308 metagame/misc/dynamic_load.pl metagame/misc/dynamic_load.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================
;
; ;; dynamic_load.pl
;
; ;; Loads a theory file.


  (= (dl Nil)  
    (set-det))
  (= (dl (Cons  $F $Files))  
    (set-det)
    (dl $F)
    (dl $Files))
  (= (dl $Filename)  
    (dynamic-load $Filename))


  (= (dynamic-load $Filename)  
    (open $Filename read $Stream)
    (format "Loading theory file: ~w~n" 
      (:: $Filename))
    (dynamic-load-stream $Stream)
    (format "  Finished loading Theory file: ~w~n" 
      (:: $Filename))
    (close $Stream))


  (= (dynamic-load-stream $Stream)  
    (det-if-then-else 
      (, 
        (read $Stream $Term) 
        (\== $Term end-of-file)) 
      (, 
        (process-term $Term) 
        (dynamic-load-stream $Stream)) True))

;
; process_term(Term) :- assert(Term).

  (= (process-term $Term)  
    (theory-assert $Term))
	

