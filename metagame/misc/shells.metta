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



  !(ensure-loaded (library aux))
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
    (shell $Tree) 
    ( (command-from-args $Tree $Command) (unix (shell $Command))))
; 


;
; 

;
; 

;
; 

  (= 
    (shell $Command $Value) 
    (shell $Command /tmp/shelltmp $Value))
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
    (shell $Command $TmpFile $Value) 
    ( (shell (:: $Command >  $TmpFile ' ; echo ' . ' >> ' $TmpFile)) 
      (see $TmpFile) 
      (read $Value) 
      (seen) 
      (shell (:: rm $TmpFile))))
; 


;
; 

;
; 


  (= 
    (shell-out $Command $File) 
    (shell (:: $Command >  $File)))
; 


;
; 

;
; 

;
; 


  (= 
    (writep $Command $File) 
    (shell-out 
      (:: echo $Command) $File))
; 




  (= 
    (command-from-args $Tree $Command) 
    (command-from-args $Tree ' ' $Command))
; 


  (= 
    (command-from-args $Tree $Space $Command) 
    ( (flatten $Tree $List) 
      (interleave-list $List $Space $SpacedList) 
      (concat-list $SpacedList $Command)))
; 




  (= 
    (interleave-list Nil $_ Nil) 
    (set-det))
; 

  (= 
    (interleave-list 
      (:: $H) $_ 
      (:: $H)) 
    (set-det))
; 

  (= 
    (interleave-list 
      (Cons  $H $T) $Sym 
      (Cons  $H 
        (Cons  $Sym $TT))) 
    (interleave-list $T $Sym $TT))
; 




  (= 
    (spacify_list  () ()) True)
; 

  (= 
    (spacify-list 
      (Cons  $H $T) 
      (Cons  $H 
        (Cons  ' ' $TT))) 
    (spacify-list $T $TT))
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
    (unloaded-host $Addr) 
    (shell 
      (:: rsh ely /usr/etc/resman dbank) $Addr))
; 

	

;
; 

;
; 


  (= 
    (shell-rsh $Program $Args) 
    ( (current-host $Addr) (shell-rsh $Addr $Program $Args)))
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
    (shell-rsh any $Program $Args) 
    ( (set-det) 
      (unloaded-host $Addr) 
      (shell-rsh $Addr $Program $Args)))
; 

  (= 
    (shell-rsh $Addr $Program $Args) 
    (shell-rsh $Addr $Program $Args $Addr))
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
    (shell-rsh any $Program $Args $Title) 
    ( (set-det) 
      (unloaded-host $Addr) 
      (shell-rsh $Addr $Program $Args $Title)))
; 

  (= 
    (shell-rsh $Addr $Program $Args $Title) 
    (shell (:: %HOME/Bin/xrsh $Addr 'xterm ' -sb -name $Title -title $Title -e $Program $Args &)))
; 





;
; 

;
; 

;
; 



  (= 
    (start-sicstus-shell $Host $Args) 
    (shell-rsh $Host sicstus $Args))
; 


  (= 
    (start-sicstus-shell $Args) 
    (shell-rsh sicstus $Args))
; 





;
; 

;
; 

;
; 



  (= 
    (find-architecture $Arch) 
    ( (shell arch $Arch) (add-symbol  &self (found_current_architecture  $Arch))))
; 



  (= 
    (current-architecture $Arch) 
    ( (get-current-architecture $Arch1) (= $Arch $Arch1)))
; 



  (= 
    (get-current-architecture $Arch) 
    ( (environment-variable %ARCH $Arch) (set-det)))
; 

  (= 
    (get-current-architecture $Arch) 
    (det-if-then-else 
      (current-predicate found-current-architecture $_) 
      (found-current-architecture $Arch) 
      (find-architecture $Arch)))
; 


	

  (= 
    (bin-directory $D) 
    ( (current-architecture $Arch) (concat-list (:: ~/prolog/bin/ $Arch /) $D)))
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
    (arch-path-name $Arch $Path $Name) 
    ( (current-architecture $Arch) (concat-list $Path $Name)))
; 

	


  (= 
    (add-bin-library) 
    ( (bin-directory $D) (add-symbol  &self (library_directory  $D))))
; 



  (= 
    (current-directory $X) 
    (absolute-file-name . $X))
; 



  (= 
    (file-exists $X) 
    (unix (access $X 0)))
; 



;
; 

;
; 

;
; 


  (= 
    (environment-variable $Name $Value) 
    (shell 
      (:: echo $Name) $Value))
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
    (with-output-file $File $Mode $Goal) 
    ( (switch-output-to-file $File $Mode $Old $New) 
      (call $Goal) 
      (close $New) 
      (set-output $Old)))
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
    (with-input-file $File $Goal) 
    ( (switch-input-to-file $File $Old $New) 
      (call $Goal) 
      (close $New) 
      (set-input $Old)))
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
    (switch-output-to-file $File $Mode $Old $New) 
    ( (current-output $Old) 
      (open $File $Mode $New) 
      (set-output $New)))
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
    (switch-input-to-file $File $Old $New) 
    ( (current-input $Old) 
      (open $File read $New) 
      (set-input $New)))
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
    (with-temp-file $Template $File $Goal) 
    ( (gensym $Template $P) 
      (concat-list 
        (:: /tmp/ $P XXXXXX) $FullTemplate) 
      (mktemp $FullTemplate $File) 
      (call $Goal) 
      (det-if-then-else 
        (tracing savetables) True 
        (shell (:: rm $File)))))
; 


;
; 




;
; 


  (= 
    (cd) 
    (unix cd))
; 

  (= 
    (cd $X) 
    (unix (cd $X)))
; 


  (= 
    (ls) 
    (unix (shell ls)))
; 


  (= 
    (lsa) 
    (unix (shell 'ls -Al')))
; 


  (= 
    (pwd) 
    (unix (shell pwd)))
; 


