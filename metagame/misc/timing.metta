;                                           (convert-to-metta-file  timing $_242776 metagame/misc/timing.pl metagame/misc/timing.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ; timing.pl
;
; ; ---------
;
; ; basic timing utilities
;
; ;
;
; ; runtime_once :- print the time to call a MeTTa goal the first time
;
; ; realtime_once :- same for real time.
;
; ; runtime :- print the time to solve a MeTTa goal (new times on backtracking)
;
; ; realtime :- same for real time.
;
; ; runtime_success :- returns the time to solve a MeTTa goal the first time.
;
; ; fails if doesn't solve it.
;
; ; realtime_success :- same for real time.
;
; ; cumulative_time(+Tasks) :- print the cumulative time used to do the tasks
;
; ; (in runtime).
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  !(ensure-loaded (library aux))
  !(ensure-loaded (library shells))

;
; -----------------------------
;
; TIMING(G,Method,Time,Success)
;
; -----------------------------

  (= (timing $G $Method $Time $Success)  
    (time-stat $Method $T0)
    (or 
      (, 
        (call $G) 
        (= $Success yes)) 
      (= $Success no))
    (time-stat $Method $T1)
    (is $Time 
      (- $T1 $T0))) 


;
; ------------------------
;
; TIMING(Goal,Method,Time)
;
; ------------------------
;
; Succeeds only when Success=yes.
;
; Formats result whether succeeds or not.
  (= (timing $Goal $Method $Time)  
    (timing $Goal $Method $Time $Success)
    (format "Success: ~w~n" 
      (:: $Success))
    (format 'Time is ~3d sec.~n' 
      (:: $Time))
    (= $Success yes))


  (= (realtime $G)  
    (timing $G realtime $_))


  (= (runtime $G)  
    (timing $G runtime $_))


  (= (runtimes $N $G)  
    (runtime (dotimes $N $G)))

;
; --------------------------------
;
; TIMING_SUCCESS(Goal,Method,Time)
;
; --------------------------------

  (= (timing-success $Goal $Method $Time)  
    (time-stat $Method $T0)
    (call $Goal)
    (set-det)
    (time-stat $Method $T1)
    (is $Time 
      (- $T1 $T0))) 


  (= (runtime-success $Goal $Time)  
    (timing-success $Goal runtime $Time))


  (= (realtime-success $Goal $Time)  
    (timing-success $Goal realtime $Time))


;
; TIMING_ONCE(Goal,Method,Time)
;
; -----------------------------
;
; Times first call to goal and prints result.
;
; Always succeeds.
;
; 

  (= (timing-once $Goal $Method $Time)  
    (timing $Goal $Method $Time $Success)
    (set-det)
    (format "Success: ~w~n" 
      (:: $Success))
    (format 'Time is ~3d sec.~n' 
      (:: $Time)))


  (= (runtime-once $G)  
    (timing-once $G runtime $_))

  (= (realtime-once $G)  
    (timing-once $G realtime $_))


;
; TIME_STAT(Type,Time)
;
; --------------------
;
; Type is either realtime or runtime.
;
; Result is in millisecs.

  (= (time-stat realtime $Time)  
    (realtime-msec $Time)) 
  (= (time-stat runtime $Time)  
    (statistics runtime 
      (Cons  $Time $_))) 

;
; ----------------------------------------
;
; Getting real time
;
; ----------------------------------------
;
; Returns current world Time in the format:
;
; Hour-Minute-Second
;
; 
;
; REAL_TIME(Time)
;
; ---------------
;
; Returns the current time in real time.

  (= (real-time $Time)  
    (shell 
      (:: date +%H-%M-%S) $Time))


  (= (time-seconds (- (- $H $M) $S) $Sec)  
    (is $Sec 
      (+ 
        (+ $S 
          (* 60 $M)) 
        (* 3600 $H))))


  (= (time-msec $T $MSec)  
    (time-seconds $T $Sec)
    (is $MSec 
      (* 1000 $Sec)))


  (= (realtime-seconds $Sec)  
    (real-time $T)
    (time-seconds $T $Sec))


  (= (realtime-msec $Sec)  
    (real-time $T)
    (time-msec $T $Sec))



;
; REALTIME_RANDOMIZE
;
; A hacky and expensive way to use the real time
;
; to initalize the random seed.

  (= realtime-randomize  
    (realtime-seconds $S)
    (random 10 $S $R)
    (is $X 
      (mod $R 100))
    (or 
      (dotimes $X 
        (, 
          (random $_) fail)) True))
	


;
; =============================================================================
;
; Cumulative timing
;
; =============================================================================


  (= (cumulative-time $Tasks)  
    (statistics runtime 
      (Cons  $T0 $_))
    (cumulative-time $Tasks $T0 1))

  (= (cumulative-time  () $_ $_)  True)
  (= (cumulative-time (Cons  $Task $Tasks) $T0 $N)  
    (det-if-then-else $Task 
      (= 
        (:: $Mark) " ") 
      (= 
        (:: $Mark) "*"))
    (statistics runtime 
      (Cons  $T $_))
    (is $CTime 
      (- $T $T0))
    (format ~p~8|~p~c~n 
      (:: $N $CTime $Mark))
    (is $N1 
      (+ $N 1))
    (set-det)
    (cumulative-time $Tasks $T0 $N1))


;
; =============================================================================
;
; Waiting
;
; =============================================================================


  (= (wait-msecs $MSec)  
    (statistics runtime 
      (Cons  $T0 $_))
    (is $T1 
      (+ $T0 $MSec))
    (wait-till-time $T1))


  (= (wait-till-time $T1)  
    (statistics runtime 
      (Cons  $TN $_))
    (det-if-then-else 
      (>= $TN $T1) True 
      (wait-till-time $T1)))

;
; ==============================================================================
;
; Turning on and off TIMING parameter.
;
; ==============================================================================
;
; Some routines call the predicate TIMING(Call) instead of call directly,
;
; which means call normally when timing mode off, else call runtime(Call).
;
; These are only useful for routines who measure REAL-TIME.


  (= (timing $Call)  
    (timing 
      (set-det) 
      (runtime $Call)))
  (= (timing $Call)  
    (call $Call))

  (= timing  
    (parameter timing on))


  (= set-timing  
    (set-parameter timing on))

  (= unset-timing  
    (set-parameter timing off))


