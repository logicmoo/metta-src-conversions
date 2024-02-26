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




  !(ensure-loaded (library shells))
; 



;
; 

;
; 

;
; 



  (= 
    (linda-client-wait (with_self  $ServerAddr $PortNum)) 
    ( (linda-client (with_self  $ServerAddr $PortNum)) (set-det)))
; 

  (= 
    (linda-client-wait (with_self  $ServerAddr $PortNum)) 
    (linda-client-wait (with_self  $ServerAddr $PortNum)))
; 




  (= 
    (in-wait $Pattern) 
    ( (format "~nWaiting to receive pattern: ~w~n" 
        (:: $Pattern)) 
      (in-wait-loop $Pattern) 
      (set-det) 
      (format "~nReceived pattern: ~w~n" 
        (:: $Pattern))))
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
    (in-wait-loop $Pattern) 
    ( (in-noblock $Pattern) (set-det)))
; 

  (= 
    (in-wait-loop $Pattern) 
    (in-wait-loop $Pattern))
; 




  (= 
    (rd-wait $Pattern) 
    ( (format "~nWaiting to observe pattern: ~w~n" 
        (:: $Pattern)) 
      (rd-wait-loop $Pattern) 
      (set-det) 
      (format "~nObserved pattern: ~w~n" 
        (:: $Pattern))))
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
    (rd-wait-loop $Pattern) 
    ( (rd-noblock $Pattern) (set-det)))
; 

  (= 
    (rd-wait-loop $Pattern) 
    (rd-wait-loop $Pattern))
; 


;
; 



  (= 
    (remote-metagame $Addr $Args $Title) 
    (shell-rsh $Addr /homes/bdp/prolog/play/metagame $Args $Title))
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
    (human_file  /homes/bdp/prolog/play/humanist) True)
; 


  (= 
    (random_file  /homes/bdp/prolog/play/randomist) True)
; 


  (= 
    (instant_file  /homes/bdp/prolog/play/instantist) True)
; 




  (= 
    (play-human-match $Server $P1 $P2) 
    ( (human-file $F) (play-match $Server $P1 $F $P2 $F)))
; 



  (= 
    (play-match $Server $Player1 $Info1 $Player2 $Info2) 
    (make-server $Server $Player1 $Info1 $Player2 $Info2))
; 



  (= 
    (make-server $Server $Player1 $Info1 $Player2 $Info2) 
    ( (command-from-args 
        (:: server $Server : $Player1 vs $Player2) - $Title) (remote-metagame $Server (:: file /homes/bdp/prolog/play/serve-tourney player1-name $Player1 player1-info $Info1 player2-name $Player2 player2-info $Info2) $Title)))
; 



  (= 
    (port-number $P $N) 
    (atom-to-number $P $N))
; 



  (= 
    (atom-to-number $A $N) 
    ( (name $A $N1) (number-chars $N $N1)))
; 







