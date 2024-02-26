;                                           (convert_to_metta_file  pipes $_268156 metagame/comms/pipes.pl metagame/comms/pipes.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================


;
; ;; pipes.pl


  (= (talk-interface $Command)  
    (interface-record-streams $Command $_ $_))



  (= (interface-record-streams $Command $InStream $OutStream)  
    (connect-pipes $Command $InStream $PipeIn $OutStream $PipeOut)
    (add-global instream $InStream)
    (add-global outstream $OutStream)
    (add-global inpipe $PipeIn)
    (add-global outpipe $PipeOut))




;
; CONNECT_PIPES(+Command,-InStream,-PIn,-OutStream,-POut).
;
; Calls Command with input from new pipe PipeOut, and output
;
; to new pipe PipeIn, and opens inverted streams to those
;
; pipes so that InStream gets command's output, and
;
; OutStream sends command input.
;
; Thus we are fully hooked up to talk to the command.
;
; 


  (= (connect-pipes $Command $InStream $PIn $OutStream $POut)  
    (open-new-pipe read $PIn $InStream)
    (open-new-pipe write $POut $OutStream)
    (shell (:: $Command > $PIn < $POut &)))






  (= (mode_arrow  read <)  True)
  (= (mode_arrow  write >)  True)


  (= (open-pipe $Pipe $Mode $Stream)  
    (mode-arrow $Mode $Arrow)
    (command-from-args 
      (:: cat $Arrow $Pipe) $Command)
    (unix (popen $Command $Mode $Stream))) 


  (= (open-new-pipe $Mode $Pipe $Stream)  
    (new-pipe-file $Pipe)
    (open-pipe $Pipe $Mode $Stream))


  (= (new-pipe-file $Pipe)  
    (gensym pipe $P)
    (concat-list 
      (:: /tmp/ $P XXXXXX) $Template)
    (mktemp $Template $Pipe)
    (shell (:: mknod $Pipe p)))



;
; If connection has been broken for some reason,
;
; creates new streams corresponding to our input and
;
; output pipes.
;
; 

  (= (reconnect-pipes)  
    (reconnect-inpipe)
    (reconnect-outpipe))


  (= (reconnect-inpipe)  
    (global inpipe $PIn)
    (open-pipe $PIn read $InStream)
    (add-global instream $InStream))


  (= (reconnect-outpipe)  
    (global outpipe $POut)
    (open-pipe $POut write $OutStream)
    (add-global outstream $OutStream))



  (= (close-int)  
    (interface-close))


  (= (interface-close)  
    (interface-close-streams)
    (interface-close-pipes))


  (= (close-streams)  
    (interface-close-streams))


  (= (interface-close-streams)  
    (global instream $InStream)
    (close $InStream)
    (global outstream $OutStream)
    (close $OutStream))


  (= (close-pipes)  
    (interface-close-pipes))


  (= (interface-close-pipes)  
    (rm-gpipe inpipe)
    (rm-gpipe outpipe))


  (= (rm-gpipe $P)  
    (global $P $Pipe)
    (shell (:: rm $Pipe)))



;
; Use 'trace tellmove' to see what we are telling.
;
; 

  (= (tell-outstream $Statement)  
    (tracing-format tellmove "Telling <Opponent>: ~p~n" 
      (:: $Statement))
    (current-output $OldStream)
    (global outstream $O)
    (command-from-args $Statement $String)
    (format $O $String Nil)
    (format $O "~n" Nil)
    (flush-output $O)
    (set-output $OldStream))


;
; ---------------------------------------------------------------------------
;
; Pattern Matching in communications
;
; ---------------------------------------------------------------------------


  !((abolish (/ found 1)) (abolish (/ found1 1)))

;
; Reads current data stream until finds a string Pattern.
;
; 

  (= (read-until-string $Pattern)  
    (tracing-format readmove "Looking for pattern: ~s~n" 
      (:: $Pattern))
    (found $Pattern)
    (tracing-format readmove "~nPattern found: ~s~n" 
      (:: $Pattern)))


;
; FOUND(Symbol)
;
; 
;
; Read a sequence of chars until a pattern is found which matches each
;
; char. Uses Sahlin's routines above.
	

  (= (found $Symbol)  
    (found1 $Symbol))

;
; Use 'trace readmove' to see what we are reading.

  (= (found1  (,  $_ ()))  True)
  (= (found1 $SymbInfo)  
    (get0 $C)
    (tracing-format readmove "~s" 
      (:: (:: $C)))
    (new $C $SymbInfo $SymbInfoNew)
    (found1 $SymbInfoNew))

;
; ---------------------------------------------------------------------------
	
;
; ================================================================================
;
; Interface
;
; ================================================================================


