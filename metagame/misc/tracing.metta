;                                           (convert_to_metta_file  tracing $_98416 metagame/misc/tracing.pl metagame/misc/tracing.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; tracing.pl
;
; Barney Pell
;
; 
;
; Supports verbose tracing of modules for software development.


  !(dynamic (/ %tracing 2))

;
; TRACING(+Module,+Call)
;
; If tracing is turned on for Module (which might be a pattern), calls Call,
;
; otherwise succeeds without it.


  (= (tracing $Module $Call)  
    (det-if-then-else 
      (tracing $Module) 
      (call $Call) True))

;
; TRACING_FORMAT(Module,String,Args)
;
; Like format/2, but only when we're tracing Module.
;
; Might cause trouble later when want to use streams also.

  (= (tracing-format $Module $String $Args)  
    (det-if-then-else 
      (tracing $Module) 
      (format $String $Args) True))

;
; SET_TRACING(Module,Status)
;
; Sets an individual module's status (usually on or off).

  (= (set-tracing $Module $Status)  
    ( (remove-all-atoms  &self 
        (%tracing  $Module $Status1)) (add-is-symbol  &self (%tracing  $Module $Status))))

;
; SET_TRACING(Module,Component,Status)
;
; Sets a component of an individual module to status (usually on or off).
;
; Example: set_tracing(ab,iterations,on).
  (= (set-tracing $Mod $Component $Status)  
    (functor $Module $Mod 1)
    (arg 1 $Module $Component)
    (set-tracing $Module $Status))


;
; tracing(+Pattern)
;
; Pattern is either an atomic module name, or conjunct of disjunct of patterns.
;
; 

  (= (tracing $M)  
    (var $M)
    (set-det)
    (format "~nError in tracing: Variable Module~n" Nil))
  (= (tracing (or $M1 $M2))  
    (or 
      (, 
        (set-det) 
        (tracing $M1)) 
      (tracing $M2)))
  (= (tracing (, $M1 $M2))  
    (set-det)
    (tracing $M1)
    (tracing $M2))
  (= (tracing $Module)  
    (tracing-module $Module))


  (= (tracing-module $Module)  
    ($tracing $Module on))


  (= (traced-modules $Modules)  
    (setof $M 
      (tracing-module $M) $Modules)
    (set-det))
  (= (traced_modules  ())  True)



;
; ================================================================================
;
; TIMING WHEN TRACING
;
; ================================================================================
;
; TRACE_TIMING(module,Call)
;
; just calls Call, if tracing is off for the module, otherwise calls Call and
;
; outputs the time used.
;
; Again Module can be a pattern.


  (= (trace-timing $Module $Call)  
    (tracing $Module)
    (set-det)
    (runtime $Call))
  (= (trace-timing $_ $Call)  
    (call $Call))


