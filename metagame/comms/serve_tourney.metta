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



  !(prolog-flag redefine-warnings $_ off)
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
    (metagame) 
    (serve-tourney))
; 



  (= 
    (serve-tourney) 
    ( (find-players $Player1 $Info1 $Player2 $Info2) (serve-tourney $Player1 $Info1 $Player2 $Info2)))
; 



  (= 
    (find-players $Player1 $Info1 $Player2 $Info2) 
    ( (parameter player1-name $Player1) 
      (parameter player1-info $Info1) 
      (parameter player2-name $Player2) 
      (parameter player2-info $Info2)))
; 



  (= 
    (serve-tourney $Player1 $Info1 $Player2 $Info2) 
    ( (use-module (library linda/server)) 
      (greet-players $Player1 $Player2) 
      (nl) 
      (write ' I am your friendly Linda Server!') 
      (nl) 
      (linda (- $Addr (create-tourney $Addr $Player1 $Info1 $Player2 $Info2)))))
; 



  (= 
    (greet-players $P1 $P2) 
    (format "~nHello, player1: ~w, player2: ~w~n" 
      (:: $P1 $P2)))
; 




  (= 
    (create-tourney $Addr $Player1 $Info1 $Player2 $Info2) 
    ( (format "Server address is ~q~n" 
        (:: $Addr)) 
      (start-player $Addr $Player1 $Info1) 
      (wait-msecs 45000) 
      (start-ref $Addr $Player1 $Player2) 
      (wait-msecs 45000) 
      (start-player $Addr $Player2 $Info2)))
; 



;
; 


  (= 
    (good_addr  shoveller) True)
; 




  (= 
    (start-ref 
      (with_self  $ServerAddr $Port) $Player1 $Player2) 
    ( (good-addr $Slave) 
      (command-from-args 
        (:: ref $ServerAddr : $Player1 vs $Player2) - $Title) 
      (remote-metagame $Slave 
        (:: server-name $ServerAddr server-port $Port player1-name $Player1 player2-name $Player2 file ~/prolog/play/referee) $Title)))
; 



  (= 
    (start-player 
      (with_self  $ServerAddr $Port) $Player $MoveFile) 
    ( (good-addr $Slave) 
      (command-from-args 
        (:: $ServerAddr : $Player $MoveFile) - $Title) 
      (remote-metagame $Slave 
        (:: server-name $ServerAddr server-port $Port player1-name $Player file $MoveFile file ~/prolog/play/player) $Title)))
; 







