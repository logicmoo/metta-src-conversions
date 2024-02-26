;                                           (convert_to_metta_file  shells $_172758 metagame/misc/shells.pl metagame/misc/shells.metta)
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
; ;; shells.pl
;
; ;; Useful routines for interacting with unix via MeTTa.


  !(ensure-loaded (library aux))


;
; SHELL(+Tree)
;
; Extremely useful command for interfacing with unix system.
;
; Tree is a list of lists.
;
; Flattens this tree into a list, treats each element of this list
;
; (each atom) as a word in the command, which is then sent to unix.
;
; 

  (= (shell $Tree)  
    (command-from-args $Tree $Command)
    (unix (shell $Command)))

;
; SHELL(+Tree,-Value)
;
; Calls the command, and reads in the first term output
;
; by that command as Value.
  (= (shell $Command $Value)  
    (shell $Command /tmp/shelltmp $Value))

;
; SHELL(+Tree,+TmpFile,-Value)
;
; Calls the command, outputs the result to a temp file TmpFile,
;
; and reads in the first term from that file as Value,
;
; then deletes TmpFile.
;
; It would be much faster if we could use environmnet variables
;
; to pass data back, but neither Quintus nor Sicstus let us
;
; change the variables of the shell in use.
  (= (shell $Command $TmpFile $Value)  
    (shell (:: $Command >  $TmpFile ' ; echo ' . ' >> ' $TmpFile))
    (see $TmpFile)
    (read $Value)
    (seen)
    (shell (:: rm $TmpFile)))

;
; SHELL_OUT(+Tree,+File)
;
; Calls the command, outputs the result to File.

  (= (shell-out $Command $File)  
    (shell (:: $Command >  $File)))

;
; WRITEP(+Command,+File)
;
; Echos the command to File.
;
; Works with pipes.

  (= (writep $Command $File)  
    (shell-out 
      (:: echo $Command) $File))



  (= (command-from-args $Tree $Command)  
    (command-from-args $Tree ' ' $Command))

  (= (command-from-args $Tree $Space $Command)  
    (flatten $Tree $List)
    (interleave-list $List $Space $SpacedList)
    (concat-list $SpacedList $Command))



  (= (interleave-list Nil $_ Nil)  
    (set-det))
  (= (interleave-list (:: $H) $_ (:: $H))  
    (set-det))
  (= (interleave-list (Cons  $H $T) $Sym (Cons  $H (Cons  $Sym $TT)))  
    (interleave-list $T $Sym $TT))



  (= (spacify_list  () ())  True)
  (= (spacify-list (Cons  $H $T) (Cons  $H (Cons  ' ' $TT)))  
    (spacify-list $T $TT))




;
; UNLOADED_HOST(Addr)
;
; Calls a special command to find the name of an unloaded host
;
; to use. If your system does not have such a program,
;
; you must either write one or give specific host
;
; names.

  (= (unloaded-host $Addr)  
    (shell 
      (:: rsh ely /usr/etc/resman dbank) $Addr))
	

;
; SHELL_RSH(+Program,+Args),
;
; Runs Program with its Args as a shell on the current host.

  (= (shell-rsh $Program $Args)  
    (current-host $Addr)
    (shell-rsh $Addr $Program $Args))


;
; SHELL_RSH(+Addr,+Program,+Args)
;
; Runs Program with its Args as a remote shell on host Addr,
;
; defaulting the title as the name of the program.
;
; 
;
; If Addr=any, finds the least used host.

  (= (shell-rsh any $Program $Args)  
    (set-det)
    (unloaded-host $Addr)
    (shell-rsh $Addr $Program $Args))
  (= (shell-rsh $Addr $Program $Args)  
    (shell-rsh $Addr $Program $Args $Addr))


;
; SHELL_RSH(+Addr,+Program,+Args,+Title)
;
; Runs Program with its Args as a remote shell on host Addr.
;
; As we use XRSH, we give the window a Title.
;
; 
  (= (shell-rsh any $Program $Args $Title)  
    (set-det)
    (unloaded-host $Addr)
    (shell-rsh $Addr $Program $Args $Title))
  (= (shell-rsh $Addr $Program $Args $Title)  
    (shell (:: %HOME/Bin/xrsh $Addr 'xterm ' -sb -name $Title -title $Title -e $Program $Args &)))




;
; START_SICSTUS_SHELL(+Host,+ArgsList)
;
; Runs a sicstus shell on Host (could be ANY),
;
; calling it with its list of args.


  (= (start-sicstus-shell $Host $Args)  
    (shell-rsh $Host sicstus $Args))

  (= (start-sicstus-shell $Args)  
    (shell-rsh sicstus $Args))




;
; FIND_ARCHITECTURE(-Arch)
;
; enables the command: current_architecture(-Arch),
;
; like current_host(-Host).


  (= (find-architecture $Arch)  
    ( (shell arch $Arch) (add-is-symbol  &self (found_current_architecture  $Arch))))


  (= (current-architecture $Arch)  
    (get-current-architecture $Arch1)
    (= $Arch $Arch1))


  (= (get-current-architecture $Arch)  
    (environment-variable %ARCH $Arch)
    (set-det))
  (= (get-current-architecture $Arch)  
    (det-if-then-else 
      (current-predicate found-current-architecture $_) 
      (found-current-architecture $Arch) 
      (find-architecture $Arch)))

	

  (= (bin-directory $D)  
    (current-architecture $Arch)
    (concat-list 
      (:: ~/prolog/bin/ $Arch /) $D))


;
; Arch is a placeholder, to be inserted in Path, to make Name.
;
; arch_path_name(Arch,['MeTTa/',Arch,'/newrandoms'],Dir).
;
; Arch = sun4,
;
; Name = 'MeTTa/sun4/newrandoms'
;
; 

  (= (arch-path-name $Arch $Path $Name)  
    (current-architecture $Arch)
    (concat-list $Path $Name))
	


  (= (add-bin-library)  
    ( (bin-directory $D) (add-is-symbol  &self (library_directory  $D))))


  (= (current-directory $X)  
    (absolute-file-name . $X))


  (= (file-exists $X)  
    (unix (access $X 0)))


;
; ENVIRONMENT_VARIABLE(+Name,?Value)
;
; Name should be a unix environment variable, like: '$ARCH'.
;
; Value will be its value, like: sun4.

  (= (environment-variable $Name $Value)  
    (shell 
      (:: echo $Name) $Value))


;
; WITH_OUTPUT_FILE(+File,+Mode,+Goal)
;
; Opens File for writing in Mode (write or append),
;
; executes Goal with this output stream current,
;
; then closes the stream and reverts to previous output.

  (= (with-output-file $File $Mode $Goal)  
    (switch-output-to-file $File $Mode $Old $New)
    (call $Goal)
    (close $New)
    (set-output $Old))
	
;
; WITH_INPUT_FILE(+File,+Goal)
;
; Opens File for reading,
;
; executes Goal with this input stream current,
;
; then closes the stream and reverts to previous input.

  (= (with-input-file $File $Goal)  
    (switch-input-to-file $File $Old $New)
    (call $Goal)
    (close $New)
    (set-input $Old))
	

;
; SWITCH_OUTPUT_TO_FILE(+File,+Mode,-Old,-New)
;
; Opens File for output in MODE (write or append).
;
; NEW is returned as the new stream, and becomes the current output.
;
; OLD is the previous output stream.

  (= (switch-output-to-file $File $Mode $Old $New)  
    (current-output $Old)
    (open $File $Mode $New)
    (set-output $New))

;
; SWITCH_INPUT_TO_FILE(+File,-Old,-New)
;
; Opens File for input, with NEW as the stream,
;
; and sets it to current input. OLD is the previous input
;
; stream.

  (= (switch-input-to-file $File $Old $New)  
    (current-input $Old)
    (open $File read $New)
    (set-input $New))


;
; WITH_TEMP_OUTFILE(+Template,-File,+Goal)
;
; Gensyms as File a new filename in /tmp/, based on Template.
;
; Then calls Goal, which should create File.
;
; At the end, this deletes File
;
; unless 'trace savetables' is on.
;
; 
;
; mktemp only really works in sicstus MeTTa.
;
; The file quintus-version defines it for quintus to
;
; just use that filename, while sicstus generates a
;
; guaranteed unique version of that template.

  (= (with-temp-file $Template $File $Goal)  
    (gensym $Template $P)
    (concat-list 
      (:: /tmp/ $P XXXXXX) $FullTemplate)
    (mktemp $FullTemplate $File)
    (call $Goal)
    (det-if-then-else 
      (tracing savetables) True 
      (shell (:: rm $File))))

;
; mktemp(File,File).



;
; Some abbreviations

  (= (cd)  
    (unix cd))
  (= (cd $X)  
    (unix (cd $X)))

  (= (ls)  
    (unix (shell ls)))

  (= (lsa)  
    (unix (shell 'ls -Al')))

  (= (pwd)  
    (unix (shell pwd)))

