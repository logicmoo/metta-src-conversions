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
    (talk-interface $Command) 
    (interface-record-streams $Command $_ $_))
; 




  (= 
    (interface-record-streams $Command $InStream $OutStream) 
    ( (connect-pipes $Command $InStream $PipeIn $OutStream $PipeOut) 
      (add-global instream $InStream) 
      (add-global outstream $OutStream) 
      (add-global inpipe $PipeIn) 
      (add-global outpipe $PipeOut)))
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
    (connect-pipes $Command $InStream $PIn $OutStream $POut) 
    ( (open-new-pipe read $PIn $InStream) 
      (open-new-pipe write $POut $OutStream) 
      (shell (:: $Command > $PIn < $POut &))))
; 







  (= 
    (mode_arrow  read <) True)
; 

  (= 
    (mode_arrow  write >) True)
; 



  (= 
    (open-pipe $Pipe $Mode $Stream) 
    ( (mode-arrow $Mode $Arrow) 
      (command-from-args 
        (:: cat $Arrow $Pipe) $Command) 
      (unix (popen $Command $Mode $Stream))))
; 
 


  (= 
    (open-new-pipe $Mode $Pipe $Stream) 
    ( (new-pipe-file $Pipe) (open-pipe $Pipe $Mode $Stream)))
; 



  (= 
    (new-pipe-file $Pipe) 
    ( (gensym pipe $P) 
      (concat-list 
        (:: /tmp/ $P XXXXXX) $Template) 
      (mktemp $Template $Pipe) 
      (shell (:: mknod $Pipe p))))
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
    (reconnect-pipes) 
    ( (reconnect-inpipe) (reconnect-outpipe)))
; 



  (= 
    (reconnect-inpipe) 
    ( (global inpipe $PIn) 
      (open-pipe $PIn read $InStream) 
      (add-global instream $InStream)))
; 



  (= 
    (reconnect-outpipe) 
    ( (global outpipe $POut) 
      (open-pipe $POut write $OutStream) 
      (add-global outstream $OutStream)))
; 




  (= 
    (close-int) 
    (interface-close))
; 



  (= 
    (interface-close) 
    ( (interface-close-streams) (interface-close-pipes)))
; 



  (= 
    (close-streams) 
    (interface-close-streams))
; 



  (= 
    (interface-close-streams) 
    ( (global instream $InStream) 
      (close $InStream) 
      (global outstream $OutStream) 
      (close $OutStream)))
; 



  (= 
    (close-pipes) 
    (interface-close-pipes))
; 



  (= 
    (interface-close-pipes) 
    ( (rm-gpipe inpipe) (rm-gpipe outpipe)))
; 



  (= 
    (rm-gpipe $P) 
    ( (global $P $Pipe) (shell (:: rm $Pipe))))
; 




;
; 

;
; 


  (= 
    (tell-outstream $Statement) 
    ( (tracing-format tellmove "Telling <Opponent>: ~p~n" 
        (:: $Statement)) 
      (current-output $OldStream) 
      (global outstream $O) 
      (command-from-args $Statement $String) 
      (format $O $String Nil) 
      (format $O "~n" Nil) 
      (flush-output $O) 
      (set-output $OldStream)))
; 



;
; 

;
; 

;
; 



  !((abolish (/ found 1)) (abolish (/ found1 1)))
; 


;
; 

;
; 


  (= 
    (read-until-string $Pattern) 
    ( (tracing-format readmove "Looking for pattern: ~s~n" 
        (:: $Pattern)) 
      (found $Pattern) 
      (tracing-format readmove "~nPattern found: ~s~n" 
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
    (found $Symbol) 
    (found1 $Symbol))
; 


;
; 


  (= 
    (found1  
      (,  $_ ())) True)
; 

  (= 
    (found1 $SymbInfo) 
    ( (get0 $C) 
      (tracing-format readmove "~s" 
        (:: (:: $C))) 
      (new $C $SymbInfo $SymbInfoNew) 
      (found1 $SymbInfoNew)))
; 


;
; 

	
;
; 

;
; 

;
; 



