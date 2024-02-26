;                                           (convert-to-metta-file  comms $_503836 metagame/comms/comms.pl metagame/comms/comms.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;; comms.pl
;
; ;; Deals with communications protocols, and starting remote matches.



  !(ensure-loaded (library shells))


;
; ============================================================================
;
; Modifications to linda communications routines
;
; ============================================================================


  (= (linda-client-wait (with-self  $ServerAddr $PortNum))  
    (linda-client (with-self  $ServerAddr $PortNum))
    (set-det))
  (= (linda-client-wait (with-self  $ServerAddr $PortNum))  
    (linda-client-wait (with-self  $ServerAddr $PortNum)))



  (= (in-wait $Pattern)  
    (format "~nWaiting to receive pattern: ~w~n" 
      (:: $Pattern))
    (in-wait-loop $Pattern)
    (set-det)
    (format "~nReceived pattern: ~w~n" 
      (:: $Pattern)))
; ; in(Pattern), !,
	
;
; Keep waiting for TimeOut seconds, where linda:timeout(TimeOut),
;
; until get a pattern.
;
; Could have an ultimate time limit, after which we give up.
;
; Could also have a waiting period between.

  (= (in-wait-loop $Pattern)  
    (in-noblock $Pattern)
    (set-det))
  (= (in-wait-loop $Pattern)  
    (in-wait-loop $Pattern))



  (= (rd-wait $Pattern)  
    (format "~nWaiting to observe pattern: ~w~n" 
      (:: $Pattern))
    (rd-wait-loop $Pattern)
    (set-det)
    (format "~nObserved pattern: ~w~n" 
      (:: $Pattern)))
; ; rd(Pattern),
	
;
; Keep waiting for TimeOut seconds, where linda:timeout(TimeOut),
;
; until get a pattern.
;
; Could have an ultimate time limit, after which we give up.
;
; Could also have a waiting period between.

  (= (rd-wait-loop $Pattern)  
    (rd-noblock $Pattern)
    (set-det))
  (= (rd-wait-loop $Pattern)  
    (rd-wait-loop $Pattern))

;
; ============================================================================


  (= (remote-metagame $Addr $Args $Title)  
    (shell-rsh $Addr /homes/bdp/prolog/play/metagame $Args $Title))

;
; ============================================================================


;
; play_match(any,innes,'/homes/bdp/MeTTa/play/randomist',barney,'/homes/bdp/MeTTa/play/instantist').
;
; play_match(any,rando,'/homes/bdp/MeTTa/play/randomist',instanto,'/homes/bdp/MeTTa/play/instantist').
;
; play_match(shoveller,rando,'/homes/bdp/MeTTa/play/randomist',instanto,'/homes/bdp/MeTTa/play/instantist').


  (= (human-file  /homes/bdp/prolog/play/humanist)  True)

  (= (random-file  /homes/bdp/prolog/play/randomist)  True)

  (= (instant-file  /homes/bdp/prolog/play/instantist)  True)



  (= (play-human-match $Server $P1 $P2)  
    (human-file $F)
    (play-match $Server $P1 $F $P2 $F))


  (= (play-match $Server $Player1 $Info1 $Player2 $Info2)  
    (make-server $Server $Player1 $Info1 $Player2 $Info2))


  (= (make-server $Server $Player1 $Info1 $Player2 $Info2)  
    (command-from-args 
      (:: server $Server : $Player1 vs $Player2) - $Title)
    (remote-metagame $Server 
      (:: file /homes/bdp/prolog/play/serve-tourney player1-name $Player1 player1-info $Info1 player2-name $Player2 player2-info $Info2) $Title))


  (= (port-number $P $N)  
    (symbol-to-number $P $N))


  (= (symbol-to-number $A $N)  
    (name $A $N1)
    (number-chars $N $N1))






