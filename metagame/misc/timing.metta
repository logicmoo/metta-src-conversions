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

  !(ensure-loaded (library shells))
; 


;
; 

;
; 

;
; 


  (= 
    (timing $G $Method $Time $Success) 
    ( (time-stat $Method $T0) 
      (or 
        (, 
          (call $G) 
          (= $Success yes)) 
        (= $Success no)) 
      (time-stat $Method $T1) 
      (is $Time 
        (- $T1 $T0))))
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
    (timing $Goal $Method $Time) 
    ( (timing $Goal $Method $Time $Success) 
      (format "Success: ~w~n" 
        (:: $Success)) 
      (format 'Time is ~3d sec.~n' 
        (:: $Time)) 
      (= $Success yes)))
; 



  (= 
    (realtime $G) 
    (timing $G realtime $_))
; 



  (= 
    (runtime $G) 
    (timing $G runtime $_))
; 



  (= 
    (runtimes $N $G) 
    (runtime (dotimes $N $G)))
; 


;
; 

;
; 

;
; 


  (= 
    (timing-success $Goal $Method $Time) 
    ( (time-stat $Method $T0) 
      (call $Goal) 
      (set-det) 
      (time-stat $Method $T1) 
      (is $Time 
        (- $T1 $T0))))
; 
 


  (= 
    (runtime-success $Goal $Time) 
    (timing-success $Goal runtime $Time))
; 



  (= 
    (realtime-success $Goal $Time) 
    (timing-success $Goal realtime $Time))
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
    (timing-once $Goal $Method $Time) 
    ( (timing $Goal $Method $Time $Success) 
      (set-det) 
      (format "Success: ~w~n" 
        (:: $Success)) 
      (format 'Time is ~3d sec.~n' 
        (:: $Time))))
; 



  (= 
    (runtime-once $G) 
    (timing-once $G runtime $_))
; 


  (= 
    (realtime-once $G) 
    (timing-once $G realtime $_))
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
    (time-stat realtime $Time) 
    (realtime-msec $Time))
; 
 
  (= 
    (time-stat runtime $Time) 
    (statistics runtime 
      (Cons  $Time $_)))
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
    (real-time $Time) 
    (shell 
      (:: date +%H-%M-%S) $Time))
; 



  (= 
    (time-seconds 
      (- 
        (- $H $M) $S) $Sec) 
    (is $Sec 
      (+ 
        (+ $S 
          (* 60 $M)) 
        (* 3600 $H))))
; 



  (= 
    (time-msec $T $MSec) 
    ( (time-seconds $T $Sec) (is $MSec (* 1000 $Sec))))
; 



  (= 
    (realtime-seconds $Sec) 
    ( (real-time $T) (time-seconds $T $Sec)))
; 



  (= 
    (realtime-msec $Sec) 
    ( (real-time $T) (time-msec $T $Sec)))
; 




;
; 

;
; 

;
; 


  (= 
    (realtime-randomize) 
    ( (realtime-seconds $S) 
      (random 10 $S $R) 
      (is $X 
        (mod $R 100)) 
      (or 
        (dotimes $X 
          (, 
            (random $_) 
            (fail))) True)))
; 

	


;
; 

;
; 

;
; 



  (= 
    (cumulative-time $Tasks) 
    ( (statistics runtime 
        (Cons  $T0 $_)) (cumulative-time $Tasks $T0 1)))
; 


  (= 
    (cumulative_time  () $_ $_) True)
; 

  (= 
    (cumulative-time 
      (Cons  $Task $Tasks) $T0 $N) 
    ( (det-if-then-else $Task 
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
      (cumulative-time $Tasks $T0 $N1)))
; 



;
; 

;
; 

;
; 



  (= 
    (wait-msecs $MSec) 
    ( (statistics runtime 
        (Cons  $T0 $_)) 
      (is $T1 
        (+ $T0 $MSec)) 
      (wait-till-time $T1)))
; 



  (= 
    (wait-till-time $T1) 
    ( (statistics runtime 
        (Cons  $TN $_)) (det-if-then-else (>= $TN $T1) True (wait-till-time $T1))))
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
    (timing $Call) 
    ( (timing) 
      (set-det) 
      (runtime $Call)))
; 

  (= 
    (timing $Call) 
    (call $Call))
; 


  (= 
    (timing) 
    (parameter timing on))
; 



  (= 
    (set-timing) 
    (set-parameter timing on))
; 


  (= 
    (unset-timing) 
    (set-parameter timing off))
; 



