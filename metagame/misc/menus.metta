;                                           (convert-to-metta-file  menus $_316968 metagame/misc/menus.pl metagame/misc/menus.metta)
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
; ;; menus.pl


  !(my-ensure-loaded (library aux))


;
; ======================================================================
;
; Generic menu constructors
;
; ======================================================================

;
; PROCESS_COMMAND(+String,+Suffix,+Args)
;
; Check if first word in command, appropriately suffixed,
;
; is defined as a predicate. If so, call it with the args,
;
; if not, fail.
;
; Note: succeeds whether it finds a command or not.
;
; However, calling routines can check the instantiation of the args
;
; to determine whether it really changed anything.


  (= (process-command $String $Suffix $Args)  
    (det-if-then 
      (, 
        (append 
          (Cons  $Command1 $Options) 
          (:: .) $String) 
        (concat - $Suffix $Suff) 
        (concat $Command1 $Suff $Command) 
        (append 
          (Cons  $Command $Args) $Options $G) 
        (=.. $Goal $G) 
        (current-predicate $_ $Goal)) 
      (det-if-then-else 
        (call $Goal) True True)))



;
; MENU_COMMAND(FormatString,FormatArgsList,Suffix,MenuArgsList)
;
; 
;
; First, output the format string, with its format args, to the user.
;
; This should be a message indicating what choices are to be made,
;
; possibly pointing to a help menu, etc.
;
; 
;
; Then reads a sequence of words, which should be of the form:
;
; command arg1 ... argn
;
; The arg1..n are the args SPECIFIC to this command.
;
; The MenuArgs are the names of arguments which will be provided to
;
; EVERY command accessible via this menu, as the args before the
;
; command specific args.
;
; Suffix is an atom which will suffix the commands called, as they are
;
; specific to this menu.
;
; 
;
; Example: We might make a menu called: dentist.
;
; The opening message of the menu might be: "Choose a dental operation"
;
; Each operation in the menu might need to be sent following input/output variables:
;
; patient
;
; number_of_teeth
;
; suggestion
;
; 
;
; A specific operation selectable might be:
;
; pull_teeth (requires additional parameter: date)
;
; 
;
; Then the selector for this operation would be implemented as:
;
; 
;
; pull_teeth_dentist(Patient,Number,Suggestion,Date) :- ... definition
;
; 
;
; And the menu would be called as follows:
;
; 
;
; :- menu_command("Choose a dental operation",[],dentist,[P,N,S]).
;
; 
;
; 
;
; Simpler versions of this pred. omit the formatting information.


  (= (menu-command $FormatString $FormatArgs $Suffix $Args)  
    (format $FormatString $FormatArgs)
    (menu-command $Suffix $Args))

  (= (menu-command $FormatString $Suffix $Args)  
    (menu-command $FormatString Nil $Suffix $Args))

  (= (menu-command $Suffix $Args)  
    (read-keyboard-tokens $String)
    (process-command $String $Suffix $Args))



