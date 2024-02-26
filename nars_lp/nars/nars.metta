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


  !(module nars 
    (:: 
      (/ nars-main 1) 
      (/ nars-main 0)))
; 


  !(set-module (class library))
; 

  !(set-module (base system))
; 


  !(use-module (library (/ nars nal-reader)))
; 
 

;
; 


  (= 
    (narz-ground $G) 
    (ground $G))
; 



  (= 
    (nars-string $Name) 
    (or 
      (atom $Name) 
      (string $Name)))
; 











  (= 
    (do-nars-example-tests) 
    (run-nars-example-tests))
; 



  (= 
    (run-nars-example-tests) 
    ( (make) 
      (use-module 
        (library ../examples/prolog/nal/nal-examples) 
        (:: (/ nal-example-test 2))) 
      (add-history run-nars-example-tests) 
      (forall 
        (with_self  
          (nal-examples *) 
          (nal-example-test $Goal $Results)) 
        (take-nal-example-test $Goal $Results))))
; 




  (= 
    (with_self  
      (baseKB *) 
      (sanity-test)) 
    (do-nars-example-tests))
; 


  (= 
    (with_self  
      (baseKB *) 
      (feature-test)) 
    (run-nars-example-tests))
; 


  (= 
    (nal-into-prolog) 
    ( (make) (forall (, (nal-is-test $Type $Text) (== $Type into-prolog)) (nal-do-exec-test $Type $Text))))
; 




  (= 
    (nal-do-exec-test $Type $Text) 
    (nal-do-exec-test-text $Text))
; 



  (= 
    (nal-do-exec-test-text $Text) 
    ( (nal-read-clauses $Text $Clauses) (nars-exec-ex in $Clauses)))
; 



  (= 
    (nars-exec-ex $Clauses) 
    (nars-exec-ex in $Clauses))
; 


  (= 
    (nars-exec-ex $_ Nil) 
    (set-det))
; 

  (= 
    (nars-exec-ex $IO 
      (Cons  $C $Clauses)) 
    ( (set-det) 
      (nars-exec-ex $IO $C) 
      (nars-exec-ex $IO $Clauses)))
; 

  (= 
    (nars-exec-ex $IO $C) 
    ( (nal-to-prolog $IO $C $P) 
      (set-det) 
      (nars-exec-ex1 $IO $P)))
; 

  (= 
    (nars-exec-ex $IO $C) 
    (nars-exec-ex1 $IO $C))
; 




  (= 
    (nars-exec-ex1 $_ Nil) 
    (set-det))
; 

  (= 
    (nars-exec-ex1 $IO 
      (Cons  $C $Clauses)) 
    ( (set-det) 
      (nars-exec-ex1 $IO $C) 
      (nars-exec-ex1 $IO $Clauses)))
; 

  (= 
    (nars-exec-ex1 $IO 
      (nop $C)) 
    ( (set-det) (nars-exec-ex1 $IO $C)))
; 

  (= 
    (nars-exec-ex1 $_ 
      (nal-in $C $V3)) 
    ( (set-det) (nars-exec-ex1 in $C)))
; 

  (= 
    (nars-exec-ex1 $_ 
      (nal-out $C $V3)) 
    ( (set-det) (nars-exec-ex1 out $C)))
; 

  (= 
    (nars-exec-ex1 $_ 
      (outputMustContain $C)) 
    ( (set-det) (nars-exec-ex1 out $C)))
; 

  (= 
    (nars-exec-ex1 $_ 
      (expected $C)) 
    ( (set-det) (nars-exec-ex1 out $C)))
; 

  (= 
    (nars-exec-ex1 $_ 
      (write $C)) 
    ( (set-det) (nars-exec-ex1 cmt $C)))
; 

  (= 
    (nars-exec-ex1 $IO $C) 
    ( (dmsg (= $IO $C)) (fail)))
; 

  (= 
    (nars-exec-ex1 $_ 
      (do-steps $N)) 
    ( (set-det) 
      (forall 
        (between 1 $N $_) 
        (nop (inference-step $_))) 
      (set-det)))
; 

;
; 

  (= 
    (nars-exec-ex1 in $C) 
    ( (add-symbol  &self 
        (nars_db  $C)) 
      (nop (derive-event $C)) 
      (set-det)))
; 

  (= 
    (nars_exec_ex1  $_ $_) True)
; 




  (= 
    (nal-exec-tests) 
    ( (make) (forall (, (with_self  (nal-reader *) (nal-is-test $Type $Text)) (\== $Type read)) (nal-do-exec-test $Type $Text))))
; 




  (= 
    (nal-clauses-to-test $Clauses $Goal 
      (:: $ResultsExpected)) 
    (nal-clauses-to-test $Clauses True $Goal True $ResultsExpected))
; 


  (= 
    (nal-clauses-to-test Nil $Goal $Goal $InOut $InOut) 
    (set-det))
; 

  (= 
    (nal-clauses-to-test 
      (Cons  $C $Cs) $Goal $PGoal $In $Out) 
    ( (set-det) 
      (nal-clauses-to-test $C $Goal $MGoal $In $Mid) 
      (set-det) 
      (nal-clauses-to-test $Cs $MGoal $PGoal $Mid $Out)))
; 

  (= 
    (nal-clauses-to-test 
      (expected $C) $Goal $Goal $In $Out) 
    ( (set-det) 
      (nal-to-prolog out $C $P) 
      (conjoin-op or $In $P $Out)))
; 

  (= 
    (nal-clauses-to-test 
      (outputMustContain $C) $Goal $Goal $In $Out) 
    ( (set-det) 
      (nal-to-prolog out $C $P) 
      (conjoin-op or $In $P $Out)))
; 

  (= 
    (nal-clauses-to-test 
      (nal-out $C $W) $Goal $Goal $In $Out) 
    ( (set-det) 
      (nal-to-prolog out 
        (nal-out $C $W) $P) 
      (conjoin $In $P $Out)))
; 

  (= 
    (nal-clauses-to-test 
      (nal-in $C $W) $Goal $PGoal $InOut $InOut) 
    ( (set-det) 
      (nal-to-prolog in 
        (nal-in $C $W) $P) 
      (conjoin $Goal $P $PGoal)))
; 

  (= 
    (nal-clauses-to-test $CMT $Goal $Goal $In $Out) 
    ( (\== $In True) 
      (= $CMT 
        ($COMMENT $_ $_ $_)) 
      (set-det) 
      (nal-to-prolog out $CMT $P) 
      (conjoin $In $P $Out)))
; 

  (= 
    (nal-clauses-to-test $C $Goal $PGoal $InOut $InOut) 
    ( (nal-to-prolog in $C $P) 
      (conjoin $Goal $P $PGoal) 
      (set-det)))
; 


;
; 

;
; 


  (= 
    (nal-to-prolog $IO 
      ($COMMENT $C $_ $_) $M) 
    ( (set-det) (nal-to-prolog $IO (write $C) $M)))
; 

  (= 
    (nal-to-prolog $IO 
      (nal-in $C $_) $M) 
    ( (set-det) (nal-to-prolog $IO $C $M)))
; 

  (= 
    (nal-to-prolog $IO 
      (nal-out $C $_) $M) 
    ( (set-det) (nal-to-prolog $IO $C $M)))
; 

  (= 
    (nal-to-prolog $IO 
      (task $X $S $T $O $B) $M) 
    ( (not (ground $O)) 
      (= $O 
        (:: $C $F)) 
      (ignore (= $C 1.0)) 
      (ignore (= $F 1.0)) 
      (nal-to-prolog $IO 
        (task $X $S $T $O $B) $M)))
; 

  (= 
    (nal-to-prolog $IO 
      (task $X $S $T $O $B) $M) 
    ( (var $T) 
      (= $T present) 
      (nal-to-prolog $IO 
        (task $X $S $T $O $B) $M)))
; 

  (= 
    (nal-to-prolog $IO 
      (task judgement $S $_ $FC $_) $O) 
    ( (append-term $S $FC $M) (nal-to-prolog $IO $M $O)))
; 

  (= 
    (nal_to_prolog  in $X 
      (nop  $X)) True)
; 

  (= 
    (nal_to_prolog  out $X 
      (nop  
        (expected  $X))) True)
; 




  (= 
    (take-nal-example-test $Goal $ResultsExpected) 
    ( (= $Failed $_) 
      (wots $S 
        (ignore (take-nal-example-test-node $Goal $ResultsExpected $Failed))) 
      (det-if-then-else 
        (== $Failed failed) 
        (ansifmt 
          (:: red) $S) 
        (ansifmt 
          (:: green) $S))))
; 



  (= 
    (take-nal-example-test-node $Goal 
      (:: (or $Results $Expected)) $Failed) 
    ( (set-det) 
      (take-nal-example-test-node $Goal 
        (:: $Results) $Failed) 
      (take-nal-example-test-node $Goal 
        (:: $Expected) $Failed)))
; 


  (= 
    (take-nal-example-test-node $Goal $ResultsExpected $Failed) 
    ( (guess-pretty (take-nal-example-test $Goal $ResultsExpected)) 
      (format '~N~n```prolog~nTEST: ?- ~@~n```' 
        (:: (print-tree $Goal))) 
      (maplist 
        (>> 
          (:: $R) 
          (format '~NEXPECTED: `~@`' 
            (:: (print-tree $R)))) $ResultsExpected) 
      (take-nal-example-test-result $Goal $ResultsExpected $Failed)))
; 



  (= 
    (take-nal-example-test-result $Goal $ResultsExpected $Failed) 
    ( (= $Failed $_) 
      (det-if-then-else 
        (not (not (, (nars-call-ex $Goal) (narz-check-results $ResultsExpected)))) 
        (format ~N~n```diff~n+SUCCESS!~n``` Nil) 
        (, 
          (format ~N~n```diff~n-FAILED!~n``` Nil) 
          (= $Failed failed))) 
      (set-det) 
      (format '~n~n```prolog ' Nil) 
      (with_self  
        (mu *) 
        (print-tree-nl $ResultsExpected)) 
      (format ```~n~n Nil) 
      (set-det)))
; 


  




  (= 
    (nars-call-ex (, $X $Y)) 
    ( (set-det) 
      (nars-call-ex $X) 
      (nars-call-ex $Y)))
; 

  (= 
    (nars-call-ex (or $X $Y)) 
    (or 
      (, 
        (set-det) 
        (nars-call-ex $X)) 
      (nars-call-ex $Y)))
; 

;
; 

  (= 
    (nars-call-ex $L) 
    ( (is-list $L) 
      (set-det) 
      (maplist nars-call-ex $L)))
; 

  (= 
    (nars-call-ex $X) 
    (or 
      (if 
        (call $X) True) 
      (, 
        (nop (print-tree-nl (failed-nars-call-ex $X))) 
        (fail))))
; 




  (= 
    (narz-check-results $L) 
    ( (is-list $L) 
      (set-det) 
      (maplist narz-check-results $L)))
; 

  (= 
    (narz-check-results (or $R1 $R2)) 
    (or 
      (, 
        (set-det) 
        (narz-check-results $R1)) 
      (narz-check-results $R2)))
; 

  (= 
    (narz-check-results (, $R1 $RS)) 
    ( (set-det) 
      (narz-check-results $R1) 
      (narz-check-results $RS)))
; 

  (= 
    (narz-check-results (= $R $V)) 
    (or 
      (if 
        (, 
          (set-det) 
          (= $R $V)) True) 
      (nars-close-enough $R $V)))
; 

  (= 
    (narz-check-results $X) 
    (or 
      (if 
        (call $X) True) 
      (, 
        (fail) 
        (print-tree (test-failed $X)) 
        (fail))))
; 



  (= 
    (nars-close-enough $R $V) 
    ( (=@= $R $V) (set-det)))
; 

  (= 
    (nars-close-enough $R $V) 
    ( (number $R) 
      (number $V) 
      (set-det) 
      (is $RV 
        (abs (- $R $V))) 
      (< $RV 0.03)))
; 

  (= 
    (nars-close-enough $R $V) 
    ( (or 
        (not (compound $R)) 
        (not (compound $V))) 
      (set-det) 
      (== $R $V)))
; 

  (= 
    (nars-close-enough 
      (Cons  $R $RT) 
      (Cons  $V $VT)) 
    ( (set-det) 
      (nars-close-enough $R $V) 
      (nars-close-enough $RT $VT)))
; 

  (= 
    (nars-close-enough $R $V) 
    ( (compound-name-arguments $R $F $RA) 
      (compound-name-arguments $V $F $VA) 
      (set-det) 
      (maplist nars-close-enough $RA $VA)))
; 











;
; 

;
; 



  (= 
    (use-nars-config-info $List) 
    ( (is-list $List) 
      (set-det) 
      (maplist use-nars-config-info $List)))
; 

  (= 
    (use-nars-config-info (element $_ Nil $List)) 
    ( (set-det) (use-nars-config-info $List)))
; 

  (= 
    (use-nars-config-info (element $_ (:: (= name $Name) (= value $Value)) $_)) 
    ( (set-det) (use-nars-config-info (= $Name $Value))))
; 

  (= 
    (use-nars-config-info (= $Name $Value)) 
    ( (nars-string $Name) 
      (downcase-atom $Name $NameD) 
      (\= $NameD $Name) 
      (set-det) 
      (use-nars-config-info (= $NameD $Value))))
; 

  (= 
    (use-nars-config-info (= $Name $Value)) 
    ( (nars-string $Value) 
      (downcase-atom $Value $ValueD) 
      (\= $ValueD $Value) 
      (set-det) 
      (use-nars-config-info (= $Name $ValueD))))
; 

  (= 
    (use-nars-config-info (= $Name $Value)) 
    ( (atom $Value) 
      (atom-number $Value $Number) 
      (use-nars-config-info (= $Name $Number))))
; 

;
; 

;
; 

;
; 

  (= 
    (use-nars-config-info (= $Name $Value)) 
    (nb-setval $Name $Value))
; 

  (= 
    (use_nars_config_info  $_) True)
; 



;
; 


  !(use-nars-config-info (= volume 100))
; 

  !(use-nars-config-info (= novelty-horizon 100000))
; 

  !(use-nars-config-info (= decision-threshold 0.51))
; 

  !(use-nars-config-info (= concept-bag-size 80000))
; 

  !(use-nars-config-info (= concept-bag-levels 1000))
; 

  !(use-nars-config-info (= duration 5))
; 

  !(use-nars-config-info (= horizon 1))
; 

  !(use-nars-config-info (= truth-epsilon 0.01))
; 

  !(use-nars-config-info (= budget-epsilon 0.0001))
; 

  !(use-nars-config-info (= budget-threshold 0.01))
; 

  !(use-nars-config-info (= default-confirmation-expectation 0.6))
; 

  !(use-nars-config-info (= always-create-concept True))
; 

  !(use-nars-config-info (= default-creation-expectation 0.66))
; 

  !(use-nars-config-info (= default-creation-expectation-goal 0.6))
; 

  !(use-nars-config-info (= default-judgment-confidence 0.9))
; 

  !(use-nars-config-info (= default-judgment-priority 0.8))
; 

  !(use-nars-config-info (= default-judgment-durability 0.5))
; 

  !(use-nars-config-info (= default-question-priority 0.9))
; 

  !(use-nars-config-info (= default-question-durability 0.9))
; 

  !(use-nars-config-info (= default-goal-confidence 0.9))
; 

  !(use-nars-config-info (= default-goal-priority 0.9))
; 

  !(use-nars-config-info (= default-goal-durability 0.9))
; 

  !(use-nars-config-info (= default-quest-priority 0.9))
; 

  !(use-nars-config-info (= default-quest-durability 0.9))
; 

  !(use-nars-config-info (= bag-threshold 1.0))
; 

  !(use-nars-config-info (= forget-quality-relative 0.3))
; 

  !(use-nars-config-info (= revision-max-occurrence-distance 10))
; 

  !(use-nars-config-info (= task-link-bag-size 100))
; 

  !(use-nars-config-info (= task-link-bag-levels 10))
; 

  !(use-nars-config-info (= term-link-bag-size 100))
; 

  !(use-nars-config-info (= term-link-bag-levels 10))
; 

  !(use-nars-config-info (= term-link-max-matched 10))
; 

  !(use-nars-config-info (= novel-task-bag-size 1000))
; 

  !(use-nars-config-info (= novel-task-bag-levels 100))
; 

  !(use-nars-config-info (= novel-task-bag-selections 100))
; 

  !(use-nars-config-info (= sequence-bag-size 30))
; 

  !(use-nars-config-info (= sequence-bag-levels 10))
; 

  !(use-nars-config-info (= operation-bag-size 10))
; 

  !(use-nars-config-info (= operation-bag-levels 10))
; 

  !(use-nars-config-info (= operation-samples 6))
; 

  !(use-nars-config-info (= projection-decay 0.1))
; 

  !(use-nars-config-info (= maximum-evidental-base-length 20000))
; 

  !(use-nars-config-info (= termlink-max-reasoned 3))
; 

  !(use-nars-config-info (= term-link-record-length 10))
; 

  !(use-nars-config-info (= concept-beliefs-max 28))
; 

  !(use-nars-config-info (= concept-questions-max 5))
; 

  !(use-nars-config-info (= concept-goals-max 7))
; 

  !(use-nars-config-info (= reliance 0.9))
; 

  !(use-nars-config-info (= discount-rate 0.5))
; 

  !(use-nars-config-info (= immediate-eternalization True))
; 

  !(use-nars-config-info (= sequence-bag-attempts 10))
; 

  !(use-nars-config-info (= condition-bag-attempts 10))
; 

  !(use-nars-config-info (= derivation-priority-leak 0.4))
; 

  !(use-nars-config-info (= derivation-durability-leak 0.4))
; 

  !(use-nars-config-info (= curiosity-desire-confidence-mul 0.1))
; 

  !(use-nars-config-info (= curiosity-desire-priority-mul 0.1))
; 

  !(use-nars-config-info (= curiosity-desire-durability-mul 0.3))
; 

  !(use-nars-config-info (= curiosity-for-operator-only False))
; 

  !(use-nars-config-info (= break-nal-hol-boundary False))
; 

  !(use-nars-config-info (= question-generation-on-decision-making False))
; 

  !(use-nars-config-info (= how-question-generation-on-decision-making False))
; 

  !(use-nars-config-info (= anticipation-confidence 0.1))
; 

  !(use-nars-config-info (= anticipation-tolerance 100.0))
; 

  !(use-nars-config-info (= retrospective-anticipations False))
; 

  !(use-nars-config-info (= satisfaction-treshold 0.0))
; 

  !(use-nars-config-info (= complexity-unit 1.0))
; 

  !(use-nars-config-info (= interval-adapt-speed 4.0))
; 

  !(use-nars-config-info (= tasklink-per-content 4))
; 

  !(use-nars-config-info (= default-feedback-priority 0.9))
; 

  !(use-nars-config-info (= default-feedback-durability 0.5))
; 

  !(use-nars-config-info (= concept-forget-durations 2.0))
; 

  !(use-nars-config-info (= termlink-forget-durations 10.0))
; 

  !(use-nars-config-info (= tasklink-forget-durations 4.0))
; 

  !(use-nars-config-info (= event-forget-durations 4.0))
; 

  !(use-nars-config-info (= variable-introduction-combinations-max 8))
; 

  !(use-nars-config-info (= variable-introduction-confidence-mul 0.9))
; 

  !(use-nars-config-info (= anticipations-per-concept-max 8))
; 

  !(use-nars-config-info (= motor-babbling-confidence-threshold 0.8))
; 

  !(use-nars-config-info (= threads-amount 1))
; 

  !(use-nars-config-info (= milliseconds-per-step 0))
; 

  !(use-nars-config-info (= steps-clock True))
; 

  !(use-nars-config-info (= derivation-durability-leak 0.4))
; 

  !(use-nars-config-info (= derivation-priority-leak 0.4))
; 



  (= 
    (use-nars-config $File) 
    ( (or 
        (not (atom $File)) 
        (not (is-absolute-file-name $File))) 
      (absolute-file-name $File $Absolute) 
      (set-det) 
      (use-nars-config $Absolute)))
; 

  (= 
    (use-nars-config $Absolute) 
    ( (open $Absolute read $In) 
      (load-sgml $In $Dom 
        (:: 
          (dialect html5) 
          (attribute-value string) 
          (cdata string) 
          (nars-system-entities True) 
          (nars-space remove) 
          (nars-syntax-errors quiet) 
          (case-preserving-attributes False) 
          (case-sensitive-attributes False) 
          (max-errors -1))) 
      (set-det) 
      (close $In) 
      (use-nars-config-info $Dom) 
      (set-det)))
; 


 
  (= 
    (parse-config) 
    (use-nars-config (library ../config/mvpConfig.xml)))
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
    (nars_ctx  default) True)
; 


;
; 


  (= 
    (revision 
      (:: $S $T1) 
      (:: $S $T2) 
      (:: $S $T)) 
    (nars-revision 
      (:: $S $T1) 
      (:: $S $T2) 
      (:: $S $T)))
; 



  (= 
    (nars-revision 
      (:: $S $T1) 
      (:: $S $T2) 
      (:: $S $T)) 
    (narz-f-rev $T1 $T2 $T))
; 


;
; 


  (= 
    (choice $X $Y $Z) 
    (nars-choice $X $Y $Z))
; 



  (= 
    (nars-choice 
      (:: $S 
        (:: $F1 $C1)) 
      (:: $S 
        (:: $F2 $C2)) 
      (:: $S 
        (:: $F1 $C1))) 
    ( (>= $C1 $C2) (set-det)))
; 

  (= 
    (nars-choice 
      (:: $S 
        (:: $F1 $C1)) 
      (:: $S 
        (:: $F2 $C2)) 
      (:: $S 
        (:: $F2 $C2))) 
    ( (< $C1 $C2) (set-det)))
; 

  (= 
    (nars-choice 
      (:: $S1 $T1) 
      (:: $S2 $T2) 
      (:: $S1 $T1)) 
    ( (\= $S1 $S2) 
      (narz-f-exp $T1 $E1) 
      (narz-f-exp $T2 $E2) 
      (>= $E1 $E2) 
      (set-det)))
; 

  (= 
    (nars-choice 
      (:: $S1 $T1) 
      (:: $S2 $T2) 
      (:: $S2 $T2)) 
    ( (\= $S1 $S2) 
      (narz-f-exp $T1 $E1) 
      (narz-f-exp $T2 $E2) 
      (< $E1 $E2) 
      (set-det)))
; 



;
; 


  (= 
    (infer $T1 $T) 
    (nars-infer $T1 $T))
; 



  (= 
    (nars-infer $T1 $T) 
    ( (nars-ctx $Ctx) (nars-inference $Ctx (:: $T1 (:: 1 0.9)) $T)))
; 


  (= 
    (nars-infer 
      (inheritance $W1 
        (ext-image 
          (ext-image represent 
            (:: nil 
              (inheritance 
                (product (:: $X $T2)) $R))) 
          (:: nil $W2 $W3))) 
      (inheritance $W1 
        (ext-image represent 
          (:: nil $X))) 
      (:: 
        (inheritance 
          (ext-image represent 
            (:: nil $Y)) 
          (ext-image 
            (ext-image represent 
              (:: nil 
                (inheritance 
                  (product (:: $Y $T2)) $R))) 
            (:: nil $W2 $W3))) $V)) 
    ( (narz-f-ind 
        (:: 1 0.9) 
        (:: 1 0.9) $V) (set-det)))
; 


  (= 
    (nars-infer 
      (inheritance $W3 
        (ext-image 
          (ext-image represent 
            (:: nil 
              (inheritance 
                (product (:: $T1 $X)) $R))) 
          (:: $W1 $W2 nil))) 
      (inheritance $W3 
        (ext-image represent 
          (:: nil $X))) 
      (:: 
        (inheritance 
          (ext-image represent 
            (:: nil $Y)) 
          (ext-image 
            (ext-image represent 
              (:: nil 
                (inheritance 
                  (product (:: $T1 $Y)) $R))) 
            (:: $W1 $W2 nil))) $V)) 
    ( (narz-f-ind 
        (:: 1 0.9) 
        (:: 1 0.9) $V) (set-det)))
; 


  (= 
    (nars-infer $T1 $T2 $T) 
    ( (nars-ctx $Ctx) (nars-inference $Ctx (:: $T1 (:: 1 0.9)) (:: $T2 (:: 1 0.9)) $T)))
; 



;
; 


  (= 
    (inference $T1 $T) 
    ( (nars-ctx $Ctx) (nars-inference $Ctx $T1 $T)))
; 


;
; 



  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $S $P) $T1) 
      (:: 
        (inheritance $P $S) $T)) 
    (narz-f-cnv $T1 $T))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $S $P) $T1) 
      (:: 
        (implication $P $S) $T)) 
    (narz-f-cnv $T1 $T))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (implication 
          (negation $S) $P) $T1) 
      (:: 
        (implication 
          (negation $P) $S) $T)) 
    (narz-f-cnt $T1 $T))
; 


  (= 
    (nars-inference $Ctx 
      (:: 
        (negation $S) $T1) 
      (:: $S $T)) 
    (narz-f-neg $T1 $T))
; 

  (= 
    (nars-inference $Ctx 
      (:: $S 
        (:: $F1 $C1)) 
      (:: 
        (negation $S) $T)) 
    ( (< $F1 0.5) (narz-f-neg (:: $F1 $C1) $T)))
; 


;
; 


  (= 
    (nars-inference $Ctx 
      (:: $S1 $T) 
      (:: $S $T)) 
    ( (narz-reduce $S1 $S) 
      (\== $S1 $S) 
      (set-det)))
; 

  (= 
    (nars-inference $Ctx 
      (:: $S1 $T) 
      (:: $S $T)) 
    (or 
      (nars-equivalence $Ctx $S1 $S) 
      (nars-equivalence $Ctx $S $S1)))
; 


  (= 
    (nars-inference $Ctx $P $C) 
    ( (nars-inference $Ctx $P 
        (:: $S 
          (:: 1 1)) $C) (call $S)))
; 

  (= 
    (nars-inference $Ctx $P $C) 
    ( (nars-inference $Ctx 
        (:: $S 
          (:: 1 1)) $P $C) (call $S)))
; 



;
; 


  (= 
    (inference $X $Y $Z) 
    ( (nars-ctx $Ctx) (nars-inference $Ctx $X $Y $Z)))
; 



;
; 



  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $M $P) $T1) 
      (:: 
        (inheritance $S $M) $T2) 
      (:: 
        (inheritance $S $P) $T)) 
    ( (\= $S $P) (narz-f-ded $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $P $M) $T1) 
      (:: 
        (inheritance $S $M) $T2) 
      (:: 
        (inheritance $S $P) $T)) 
    ( (\= $S $P) (narz-f-abd $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $M $P) $T1) 
      (:: 
        (inheritance $M $S) $T2) 
      (:: 
        (inheritance $S $P) $T)) 
    ( (\= $S $P) (narz-f-ind $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $P $M) $T1) 
      (:: 
        (inheritance $M $S) $T2) 
      (:: 
        (inheritance $S $P) $T)) 
    ( (\= $S $P) (narz-f-exe $T1 $T2 $T)))
; 


;
; 


  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $S $P) $T1) 
      (:: 
        (inheritance $P $S) $T2) 
      (:: 
        (similarity $S $P) $T)) 
    (narz-f-int $T1 $T2 $T))
; 


;
; 


  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $P $M) $T1) 
      (:: 
        (inheritance $S $M) $T2) 
      (:: 
        (similarity $S $P) $T)) 
    ( (\= $S $P) (narz-f-com $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $M $P) $T1) 
      (:: 
        (inheritance $M $S) $T2) 
      (:: 
        (similarity $S $P) $T)) 
    ( (\= $S $P) (narz-f-com $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $M $P) $T1) 
      (:: 
        (similarity $S $M) $T2) 
      (:: 
        (inheritance $S $P) $T)) 
    ( (\= $S $P) (narz-f-ana $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $P $M) $T1) 
      (:: 
        (similarity $S $M) $T2) 
      (:: 
        (inheritance $P $S) $T)) 
    ( (\= $S $P) (narz-f-ana $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (similarity $M $P) $T1) 
      (:: 
        (similarity $S $M) $T2) 
      (:: 
        (similarity $S $P) $T)) 
    ( (\= $S $P) (narz-f-res $T1 $T2 $T)))
; 


;
; 


  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $P $M) $T1) 
      (:: 
        (inheritance $S $M) $T2) 
      (:: 
        (inheritance $N $M) $T)) 
    ( (\= $S $P) 
      (narz-reduce 
        (int-intersection (:: $P $S)) $N) 
      (narz-f-int $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $P $M) $T1) 
      (:: 
        (inheritance $S $M) $T2) 
      (:: 
        (inheritance $N $M) $T)) 
    ( (\= $S $P) 
      (narz-reduce 
        (ext-intersection (:: $P $S)) $N) 
      (narz-f-uni $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $P $M) $T1) 
      (:: 
        (inheritance $S $M) $T2) 
      (:: 
        (inheritance $N $M) $T)) 
    ( (\= $S $P) 
      (narz-reduce 
        (int-difference $P $S) $N) 
      (narz-f-dif $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $M $P) $T1) 
      (:: 
        (inheritance $M $S) $T2) 
      (:: 
        (inheritance $M $N) $T)) 
    ( (\= $S $P) 
      (narz-reduce 
        (ext-intersection (:: $P $S)) $N) 
      (narz-f-int $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $M $P) $T1) 
      (:: 
        (inheritance $M $S) $T2) 
      (:: 
        (inheritance $M $N) $T)) 
    ( (\= $S $P) 
      (narz-reduce 
        (int-intersection (:: $P $S)) $N) 
      (narz-f-uni $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $M $P) $T1) 
      (:: 
        (inheritance $M $S) $T2) 
      (:: 
        (inheritance $M $N) $T)) 
    ( (\= $S $P) 
      (narz-reduce 
        (ext-difference $P $S) $N) 
      (narz-f-dif $T1 $T2 $T)))
; 


;
; 


  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $S $M) $T1) 
      (:: 
        (inheritance 
          (int-intersection $L) $M) $T2) 
      (:: 
        (inheritance $P $M) $T)) 
    ( (narz-ground $S) 
      (narz-ground $L) 
      (member $S $L) 
      (delete $L $S $N) 
      (narz-reduce 
        (int-intersection $N) $P) 
      (narz-f-pnn $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $S $M) $T1) 
      (:: 
        (inheritance 
          (ext-intersection $L) $M) $T2) 
      (:: 
        (inheritance $P $M) $T)) 
    ( (narz-ground $S) 
      (narz-ground $L) 
      (member $S $L) 
      (delete $L $S $N) 
      (narz-reduce 
        (ext-intersection $N) $P) 
      (narz-f-npp $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $S $M) $T1) 
      (:: 
        (inheritance 
          (int-difference $S $P) $M) $T2) 
      (:: 
        (inheritance $P $M) $T)) 
    ( (atom $S) 
      (atom $P) 
      (narz-f-pnp $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $S $M) $T1) 
      (:: 
        (inheritance 
          (int-difference $P $S) $M) $T2) 
      (:: 
        (inheritance $P $M) $T)) 
    ( (atom $S) 
      (atom $P) 
      (narz-f-nnn $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $M $S) $T1) 
      (:: 
        (inheritance $M 
          (ext-intersection $L)) $T2) 
      (:: 
        (inheritance $M $P) $T)) 
    ( (narz-ground $S) 
      (narz-ground $L) 
      (member $S $L) 
      (delete $L $S $N) 
      (narz-reduce 
        (ext-intersection $N) $P) 
      (narz-f-pnn $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $M $S) $T1) 
      (:: 
        (inheritance $M 
          (int-intersection $L)) $T2) 
      (:: 
        (inheritance $M $P) $T)) 
    ( (narz-ground $S) 
      (narz-ground $L) 
      (member $S $L) 
      (delete $L $S $N) 
      (narz-reduce 
        (int-intersection $N) $P) 
      (narz-f-npp $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $M $S) $T1) 
      (:: 
        (inheritance $M 
          (ext-difference $S $P)) $T2) 
      (:: 
        (inheritance $M $P) $T)) 
    ( (atom $S) 
      (atom $P) 
      (narz-f-pnp $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $M $S) $T1) 
      (:: 
        (inheritance $M 
          (ext-difference $P $S)) $T2) 
      (:: 
        (inheritance $M $P) $T)) 
    ( (atom $S) 
      (atom $P) 
      (narz-f-nnn $T1 $T2 $T)))
; 


;
; 


  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $M $P) $T1) 
      (:: 
        (implication $S $M) $T2) 
      (:: 
        (implication $S $P) $T)) 
    ( (\= $S $P) (narz-f-ded $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $P $M) $T1) 
      (:: 
        (implication $S $M) $T2) 
      (:: 
        (implication $S $P) $T)) 
    ( (\= $S $P) (narz-f-abd $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $M $P) $T1) 
      (:: 
        (implication $M $S) $T2) 
      (:: 
        (implication $S $P) $T)) 
    ( (\= $S $P) (narz-f-ind $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $P $M) $T1) 
      (:: 
        (implication $M $S) $T2) 
      (:: 
        (implication $S $P) $T)) 
    ( (\= $S $P) (narz-f-exe $T1 $T2 $T)))
; 


;
; 


  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $S $P) $T1) 
      (:: 
        (implication $P $S) $T2) 
      (:: 
        (equivalence $S $P) $T)) 
    (narz-f-int $T1 $T2 $T))
; 


;
; 


  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $P $M) $T1) 
      (:: 
        (implication $S $M) $T2) 
      (:: 
        (equivalence $S $P) $T)) 
    ( (\= $S $P) (narz-f-com $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $M $P) $T1) 
      (:: 
        (implication $M $S) $T2) 
      (:: 
        (equivalence $S $P) $T)) 
    ( (\= $S $P) (narz-f-com $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $M $P) $T1) 
      (:: 
        (equivalence $S $M) $T2) 
      (:: 
        (implication $S $P) $T)) 
    ( (\= $S $P) (narz-f-ana $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $P $M) $T1) 
      (:: 
        (equivalence $S $M) $T2) 
      (:: 
        (implication $P $S) $T)) 
    ( (\= $S $P) (narz-f-ana $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (equivalence $M $P) $T1) 
      (:: 
        (equivalence $S $M) $T2) 
      (:: 
        (equivalence $S $P) $T)) 
    ( (\= $S $P) (narz-f-res $T1 $T2 $T)))
; 


;
; 


  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $P $M) $T1) 
      (:: 
        (implication $S $M) $T2) 
      (:: 
        (implication $N $M) $T)) 
    ( (\= $S $P) 
      (narz-reduce 
        (disjunction (:: $P $S)) $N) 
      (narz-f-int $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $P $M) $T1) 
      (:: 
        (implication $S $M) $T2) 
      (:: 
        (implication $N $M) $T)) 
    ( (\= $S $P) 
      (narz-reduce 
        (conjunction (:: $P $S)) $N) 
      (narz-f-uni $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $M $P) $T1) 
      (:: 
        (implication $M $S) $T2) 
      (:: 
        (implication $M $N) $T)) 
    ( (\= $S $P) 
      (narz-reduce 
        (conjunction (:: $P $S)) $N) 
      (narz-f-int $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $M $P) $T1) 
      (:: 
        (implication $M $S) $T2) 
      (:: 
        (implication $M $N) $T)) 
    ( (\= $S $P) 
      (narz-reduce 
        (disjunction (:: $P $S)) $N) 
      (narz-f-uni $T1 $T2 $T)))
; 


;
; 


  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $S $M) $T1) 
      (:: 
        (implication 
          (disjunction $L) $M) $T2) 
      (:: 
        (implication $P $M) $T)) 
    ( (narz-ground $S) 
      (narz-ground $L) 
      (member $S $L) 
      (delete $L $S $N) 
      (narz-reduce 
        (disjunction $N) $P) 
      (narz-f-pnn $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $S $M) $T1) 
      (:: 
        (implication 
          (conjunction $L) $M) $T2) 
      (:: 
        (implication $P $M) $T)) 
    ( (narz-ground $S) 
      (narz-ground $L) 
      (member $S $L) 
      (delete $L $S $N) 
      (narz-reduce 
        (conjunction $N) $P) 
      (narz-f-npp $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $M $S) $T1) 
      (:: 
        (implication $M 
          (conjunction $L)) $T2) 
      (:: 
        (implication $M $P) $T)) 
    ( (narz-ground $S) 
      (narz-ground $L) 
      (member $S $L) 
      (delete $L $S $N) 
      (narz-reduce 
        (conjunction $N) $P) 
      (narz-f-pnn $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $M $S) $T1) 
      (:: 
        (implication $M 
          (disjunction $L)) $T2) 
      (:: 
        (implication $M $P) $T)) 
    ( (narz-ground $S) 
      (narz-ground $L) 
      (member $S $L) 
      (delete $L $S $N) 
      (narz-reduce 
        (disjunction $N) $P) 
      (narz-f-npp $T1 $T2 $T)))
; 


;
; 


  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $M $P) $T1) 
      (:: $M $T2) 
      (:: $P $T)) 
    ( (narz-ground $P) (narz-f-ded $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $P $M) $T1) 
      (:: $M $T2) 
      (:: $P $T)) 
    ( (narz-ground $P) (narz-f-abd $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: $M $T1) 
      (:: 
        (equivalence $S $M) $T2) 
      (:: $S $T)) 
    ( (narz-ground $S) (narz-f-ana $T1 $T2 $T)))
; 


;
; 


  (= 
    (nars-inference $Ctx 
      (:: $P $T1) 
      (:: $S $T2) 
      (:: $C $T)) 
    ( (== $C 
        (implication $S $P)) (narz-f-ind $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: $P $T1) 
      (:: $S $T2) 
      (:: $C $T)) 
    ( (== $C 
        (equivalence $S $P)) (narz-f-com $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: $P $T1) 
      (:: $S $T2) 
      (:: $C $T)) 
    ( (narz-reduce 
        (conjunction (:: $P $S)) $N) 
      (== $N $C) 
      (narz-f-int $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: $P $T1) 
      (:: $S $T2) 
      (:: $C $T)) 
    ( (narz-reduce 
        (disjunction (:: $P $S)) $N) 
      (== $N $C) 
      (narz-f-uni $T1 $T2 $T)))
; 


;
; 


  (= 
    (nars-inference $Ctx 
      (:: $S $T1) 
      (:: 
        (conjunction $L) $T2) 
      (:: $P $T)) 
    ( (narz-ground $S) 
      (narz-ground $L) 
      (member $S $L) 
      (delete $L $S $N) 
      (narz-reduce 
        (conjunction $N) $P) 
      (narz-f-pnn $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: $S $T1) 
      (:: 
        (disjunction $L) $T2) 
      (:: $P $T)) 
    ( (narz-ground $S) 
      (narz-ground $L) 
      (member $S $L) 
      (delete $L $S $N) 
      (narz-reduce 
        (disjunction $N) $P) 
      (narz-f-npp $T1 $T2 $T)))
; 


;
; 


  (= 
    (nars-inference $Ctx 
      (:: 
        (implication 
          (conjunction $L) $C) $T1) 
      (:: $M $T2) 
      (:: 
        (implication $P $C) $T)) 
    ( (nonvar $L) 
      (member $M $L) 
      (nars-subtract $L 
        (:: $M) $A) 
      (\= $A Nil) 
      (narz-reduce 
        (conjunction $A) $P) 
      (narz-f-ded $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (implication 
          (conjunction $L) $C) $T1) 
      (:: 
        (implication $P $C) $T2) 
      (:: $M $T)) 
    ( (narz-ground $L) 
      (member $M $L) 
      (nars-subtract $L 
        (:: $M) $A) 
      (\= $A Nil) 
      (narz-reduce 
        (conjunction $A) $P) 
      (narz-f-abd $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (implication 
          (conjunction $L) $C) $T1) 
      (:: $M $T2) 
      (:: $S $T)) 
    ( (== $S 
        (implication 
          (conjunction (Cons  $M $L)) $C)) (narz-f-ind $T1 $T2 $T)))
; 


  (= 
    (nars-inference $Ctx 
      (:: 
        (implication 
          (conjunction $Lm) $C) $T1) 
      (:: 
        (implication $A $M) $T2) 
      (:: 
        (implication $P $C) $T)) 
    ( (nonvar $Lm) 
      (narz-replace $Lm $M $La $A) 
      (narz-reduce 
        (conjunction $La) $P) 
      (narz-f-ded $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (implication 
          (conjunction $Lm) $C) $T1) 
      (:: 
        (implication 
          (conjunction $La) $C) $T2) 
      (:: 
        (implication $A $M) $T)) 
    ( (nonvar $Lm) 
      (narz-replace $Lm $M $La $A) 
      (narz-f-abd $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (implication 
          (conjunction $La) $C) $T1) 
      (:: 
        (implication $A $M) $T2) 
      (:: 
        (implication $P $C) $T)) 
    ( (nonvar $La) 
      (narz-replace $Lm $M $La $A) 
      (narz-reduce 
        (conjunction $Lm) $P) 
      (narz-f-ind $T1 $T2 $T)))
; 


;
; 


  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $M $P) $T1) 
      (:: 
        (inheritance $M $S) $T2) 
      (:: 
        (implication 
          (inheritance $X $S) 
          (inheritance $X $P)) $T)) 
    ( (\= $S $P) (narz-f-ind $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $P $M) $T1) 
      (:: 
        (inheritance $S $M) $T2) 
      (:: 
        (implication 
          (inheritance $P $X) 
          (inheritance $S $X)) $T)) 
    ( (\= $S $P) (narz-f-abd $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $M $P) $T1) 
      (:: 
        (inheritance $M $S) $T2) 
      (:: 
        (equivalence 
          (inheritance $X $S) 
          (inheritance $X $P)) $T)) 
    ( (\= $S $P) (narz-f-com $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $P $M) $T1) 
      (:: 
        (inheritance $S $M) $T2) 
      (:: 
        (equivalence 
          (inheritance $P $X) 
          (inheritance $S $X)) $T)) 
    ( (\= $S $P) (narz-f-com $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $M $P) $T1) 
      (:: 
        (inheritance $M $S) $T2) 
      (:: 
        (conjunction (:: (inheritance (var $Y Nil) $S) (inheritance (var $Y Nil) $P))) $T)) 
    ( (\= $S $P) (narz-f-int $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (inheritance $P $M) $T1) 
      (:: 
        (inheritance $S $M) $T2) 
      (:: 
        (conjunction (:: (inheritance $S (var $Y Nil)) (inheritance $P (var $Y Nil)))) $T)) 
    ( (\= $S $P) (narz-f-int $T1 $T2 $T)))
; 


;
; 


  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $A 
          (inheritance $M1 $P)) $T1) 
      (:: 
        (inheritance $M2 $S) $T2) 
      (:: 
        (implication 
          (conjunction (:: $A (inheritance $X $S))) 
          (inheritance $X $P)) $T)) 
    ( (\= $S $P) 
      (== $M1 $M2) 
      (\= $A 
        (inheritance $M2 $S)) 
      (narz-f-ind $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $A 
          (inheritance $M1 $P)) $T1) 
      (:: 
        (inheritance $M2 $S) $T2) 
      (:: 
        (conjunction (:: (implication $A (inheritance (var $Y Nil) $P)) (inheritance (var $Y Nil) $S))) $T)) 
    ( (\= $S $P) 
      (== $M1 $M2) 
      (\= $A 
        (inheritance $M2 $S)) 
      (narz-f-int $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (conjunction $L1) $T1) 
      (:: 
        (inheritance $M $S) $T2) 
      (:: 
        (implication 
          (inheritance $Y $S) 
          (conjunction (Cons  (inheritance $Y $P2) $L3))) $T)) 
    ( (nars-subtract $L1 
        (:: (inheritance $M $P)) $L2) 
      (\= $L1 $L2) 
      (\= $S $P) 
      (narz-dependant $P $Y $P2) 
      (narz-dependant $L2 $Y $L3) 
      (narz-f-ind $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (conjunction $L1) $T1) 
      (:: 
        (inheritance $M $S) $T2) 
      (:: 
        (conjunction (Cons  (inheritance (var $Y Nil) $S) (Cons  (inheritance (var $Y Nil) $P) $L2))) $T)) 
    ( (nars-subtract $L1 
        (:: (inheritance $M $P)) $L2) 
      (\= $L1 $L2) 
      (\= $S $P) 
      (narz-f-int $T1 $T2 $T)))
; 


  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $A 
          (inheritance $P $M1)) $T1) 
      (:: 
        (inheritance $S $M2) $T2) 
      (:: 
        (implication 
          (conjunction (:: $A (inheritance $P $X))) 
          (inheritance $S $X)) $T)) 
    ( (\= $S $P) 
      (== $M1 $M2) 
      (\= $A 
        (inheritance $S $M2)) 
      (narz-f-abd $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (implication $A 
          (inheritance $P $M1)) $T1) 
      (:: 
        (inheritance $S $M2) $T2) 
      (:: 
        (conjunction (:: (implication $A (inheritance $P (var $Y Nil))) (inheritance $S (var $Y Nil)))) $T)) 
    ( (\= $S $P) 
      (== $M1 $M2) 
      (\= $A 
        (inheritance $S $M2)) 
      (narz-f-int $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (conjunction $L1) $T1) 
      (:: 
        (inheritance $S $M) $T2) 
      (:: 
        (implication 
          (inheritance $S $Y) 
          (conjunction (Cons  (inheritance $P2 $Y) $L3))) $T)) 
    ( (nars-subtract $L1 
        (:: (inheritance $P $M)) $L2) 
      (\= $L1 $L2) 
      (\= $S $P) 
      (narz-dependant $P $Y $P2) 
      (narz-dependant $L2 $Y $L3) 
      (narz-f-abd $T1 $T2 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (conjunction $L1) $T1) 
      (:: 
        (inheritance $S $M) $T2) 
      (:: 
        (conjunction (Cons  (inheritance $S (var $Y Nil)) (Cons  (inheritance $P (var $Y Nil)) $L2))) $T)) 
    ( (nars-subtract $L1 
        (:: (inheritance $P $M)) $L2) 
      (\= $L1 $L2) 
      (\= $S $P) 
      (narz-f-int $T1 $T2 $T)))
; 


;
; 



  (= 
    (nars-inference $Ctx 
      (:: 
        (conjunction $L1) $T1) 
      (:: 
        (inheritance $M $S) $T2) 
      (:: $C $T)) 
    ( (nars-subtract $L1 
        (:: (inheritance (var $N $D) $S)) $L2) 
      (\= $L1 $L2) 
      (replace-var $L2 
        (var $N $D) $L3 $M) 
      (narz-reduce 
        (conjunction $L3) $C) 
      (narz-f-cnv $T2 $T0) 
      (narz-f-ana $T1 $T0 $T)))
; 

  (= 
    (nars-inference $Ctx 
      (:: 
        (conjunction $L1) $T1) 
      (:: 
        (inheritance $S $M) $T2) 
      (:: $C $T)) 
    ( (nars-subtract $L1 
        (:: (inheritance $S (var $N $D))) $L2) 
      (\= $L1 $L2) 
      (replace-var $L2 
        (var $N $D) $L3 $M) 
      (narz-reduce 
        (conjunction $L3) $C) 
      (narz-f-cnv $T2 $T0) 
      (narz-f-ana $T1 $T0 $T)))
; 



  (= 
    (replace_var  () $_ () $_) True)
; 

  (= 
    (replace-var 
      (Cons  
        (inheritance $S1 $P) $T1) $S1 
      (Cons  
        (inheritance $S2 $P) $T2) $S2) 
    (replace-var $T1 $S1 $T2 $S2))
; 

  (= 
    (replace-var 
      (Cons  
        (inheritance $S $P1) $T1) $P1 
      (Cons  
        (inheritance $S $P2) $T2) $P2) 
    (replace-var $T1 $P1 $T2 $P2))
; 


  (= 
    (replace-all 
      (Cons  $H $T1) $H1 
      (Cons  $H $T2) $H2) 
    (replace-var $T1 $H1 $T2 $H2))
; 




;
; 


;
; 


  (= 
    (inheritance $X $Y) 
    ( (nars-ctx $Ctx) (nars-inheritance $Ctx $X $Y)))
; 



  (= 
    (nars-inheritance $Ctx 
      (ext-intersection $Ls) $P) 
    (narz-include 
      (:: $P) $Ls))
; 

  (= 
    (nars-inheritance $Ctx $S 
      (int-intersection $Lp)) 
    (narz-include 
      (:: $S) $Lp))
; 

  (= 
    (nars-inheritance $Ctx 
      (ext-intersection $S) 
      (ext-intersection $P)) 
    ( (narz-include $P $S) (\= $P (:: $_))))
; 

  (= 
    (nars-inheritance $Ctx 
      (int-intersection $S) 
      (int-intersection $P)) 
    ( (narz-include $S $P) (\= $S (:: $_))))
; 

  (= 
    (nars-inheritance $Ctx 
      (ext-set $S) 
      (ext-set $P)) 
    (narz-include $S $P))
; 

  (= 
    (nars-inheritance $Ctx 
      (int-set $S) 
      (int-set $P)) 
    (narz-include $P $S))
; 


  (= 
    (nars-inheritance $Ctx 
      (ext-difference $S $P) $S) 
    ( (narz-ground $S) (narz-ground $P)))
; 

  (= 
    (nars-inheritance $Ctx $S 
      (int-difference $S $P)) 
    ( (narz-ground $S) (narz-ground $P)))
; 


  (= 
    (nars-inheritance $Ctx 
      (product $L1) $R) 
    ( (narz-ground $L1) 
      (member 
        (ext-image $R $L2) $L1) 
      (narz-replace $L1 
        (ext-image $R $L2) $L2)))
; 

  (= 
    (nars-inheritance $Ctx $R 
      (product $L1)) 
    ( (narz-ground $L1) 
      (member 
        (int-image $R $L2) $L1) 
      (narz-replace $L1 
        (int-image $R $L2) $L2)))
; 


;
; 


  (= 
    (similarity $X $Y) 
    ( (nars-ctx $Ctx) (nars-similarity $Ctx $X $Y)))
; 



  (= 
    (nars-similarity $Ctx $X $Y) 
    ( (narz-ground $X) 
      (narz-reduce $X $Y) 
      (\== $X $Y) 
      (set-det)))
; 


  (= 
    (nars-similarity $Ctx 
      (ext-intersection $L1) 
      (ext-intersection $L2)) 
    (narz-same-set $L1 $L2))
; 

  (= 
    (nars-similarity $Ctx 
      (int-intersection $L1) 
      (int-intersection $L2)) 
    (narz-same-set $L1 $L2))
; 

  (= 
    (nars-similarity $Ctx 
      (ext-set $L1) 
      (ext-set $L2)) 
    (narz-same-set $L1 $L2))
; 

  (= 
    (nars-similarity $Ctx 
      (int-set $L1) 
      (int-set $L2)) 
    (narz-same-set $L1 $L2))
; 


;
; 


  (= 
    (implication $X $Y) 
    ( (nars-ctx $Ctx) (nars-implication $Ctx $X $Y)))
; 



  (= 
    (nars_implication  $Ctx 
      (similarity  $S $P) 
      (inheritance  $S $P)) True)
; 

  (= 
    (nars_implication  $Ctx 
      (equivalence  $S $P) 
      (implication  $S $P)) True)
; 


  (= 
    (nars-implication $Ctx 
      (conjunction $L) $M) 
    ( (narz-ground $L) (member $M $L)))
; 

  (= 
    (nars-implication $Ctx $M 
      (disjunction $L)) 
    ( (narz-ground $L) (member $M $L)))
; 


  (= 
    (nars-implication $Ctx 
      (conjunction $L1) 
      (conjunction $L2)) 
    ( (narz-ground $L1) 
      (narz-ground $L2) 
      (subset $L2 $L1)))
; 

  (= 
    (nars-implication $Ctx 
      (disjunction $L1) 
      (disjunction $L2)) 
    ( (narz-ground $L1) 
      (narz-ground $L2) 
      (subset $L1 $L2)))
; 


  (= 
    (nars-implication $Ctx 
      (inheritance $S $P) 
      (inheritance 
        (ext-intersection $Ls) 
        (ext-intersection $Lp))) 
    ( (narz-ground $Ls) 
      (narz-ground $Lp) 
      (narz-replace $Ls $S $L $P) 
      (narz-same $L $Lp)))
; 

  (= 
    (nars-implication $Ctx 
      (inheritance $S $P) 
      (inheritance 
        (int-intersection $Ls) 
        (int-intersection $Lp))) 
    ( (narz-ground $Ls) 
      (narz-ground $Lp) 
      (narz-replace $Ls $S $L $P) 
      (narz-same $L $Lp)))
; 

  (= 
    (nars-implication $Ctx 
      (similarity $S $P) 
      (similarity 
        (ext-intersection $Ls) 
        (ext-intersection $Lp))) 
    ( (narz-ground $Ls) 
      (narz-ground $Lp) 
      (narz-replace $Ls $S $L $P) 
      (narz-same $L $Lp)))
; 

  (= 
    (nars-implication $Ctx 
      (similarity $S $P) 
      (similarity 
        (int-intersection $Ls) 
        (int-intersection $Lp))) 
    ( (narz-ground $Ls) 
      (narz-ground $Lp) 
      (narz-replace $Ls $S $L $P) 
      (narz-same $L $Lp)))
; 


  (= 
    (nars-implication $Ctx 
      (inheritance $S $P) 
      (inheritance 
        (ext-difference $S $M) 
        (ext-difference $P $M))) 
    (narz-ground $M))
; 

  (= 
    (nars-implication $Ctx 
      (inheritance $S $P) 
      (inheritance 
        (int-difference $S $M) 
        (int-difference $P $M))) 
    (narz-ground $M))
; 

  (= 
    (nars-implication $Ctx 
      (similarity $S $P) 
      (similarity 
        (ext-difference $S $M) 
        (ext-difference $P $M))) 
    (narz-ground $M))
; 

  (= 
    (nars-implication $Ctx 
      (similarity $S $P) 
      (similarity 
        (int-difference $S $M) 
        (int-difference $P $M))) 
    (narz-ground $M))
; 

  (= 
    (nars-implication $Ctx 
      (inheritance $S $P) 
      (inheritance 
        (ext-difference $M $P) 
        (ext-difference $M $S))) 
    (narz-ground $M))
; 

  (= 
    (nars-implication $Ctx 
      (inheritance $S $P) 
      (inheritance 
        (int-difference $M $P) 
        (int-difference $M $S))) 
    (narz-ground $M))
; 

  (= 
    (nars-implication $Ctx 
      (similarity $S $P) 
      (similarity 
        (ext-difference $M $P) 
        (ext-difference $M $S))) 
    (narz-ground $M))
; 

  (= 
    (nars-implication $Ctx 
      (similarity $S $P) 
      (similarity 
        (int-difference $M $P) 
        (int-difference $M $S))) 
    (narz-ground $M))
; 


  (= 
    (nars-implication $Ctx 
      (inheritance $S $P) 
      (negation (inheritance $S (ext-difference $M $P)))) 
    (narz-ground $M))
; 

  (= 
    (nars-implication $Ctx 
      (inheritance $S 
        (ext-difference $M $P)) 
      (negation (inheritance $S $P))) 
    (narz-ground $M))
; 

  (= 
    (nars-implication $Ctx 
      (inheritance $S $P) 
      (negation (inheritance (int-difference $M $S) $P))) 
    (narz-ground $M))
; 

  (= 
    (nars-implication $Ctx 
      (inheritance 
        (int-difference $M $S) $P) 
      (negation (inheritance $S $P))) 
    (narz-ground $M))
; 


  (= 
    (nars-implication $Ctx 
      (inheritance $S $P) 
      (inheritance 
        (ext-image $S $M) 
        (ext-image $P $M))) 
    (narz-ground $M))
; 

  (= 
    (nars-implication $Ctx 
      (inheritance $S $P) 
      (inheritance 
        (int-image $S $M) 
        (int-image $P $M))) 
    (narz-ground $M))
; 

  (= 
    (nars-implication $Ctx 
      (inheritance $S $P) 
      (inheritance 
        (ext-image $M $Lp) 
        (ext-image $M $Ls))) 
    ( (narz-ground $Ls) 
      (narz-ground $Lp) 
      (append $L1 
        (Cons  $S $L2) $Ls) 
      (append $L1 
        (Cons  $P $L2) $Lp)))
; 

  (= 
    (nars-implication $Ctx 
      (inheritance $S $P) 
      (inheritance 
        (int-image $M $Lp) 
        (int-image $M $Ls))) 
    ( (narz-ground $Ls) 
      (narz-ground $Lp) 
      (append $L1 
        (Cons  $S $L2) $Ls) 
      (append $L1 
        (Cons  $P $L2) $Lp)))
; 


  (= 
    (nars-implication $Ctx 
      (negation $M) 
      (negation (conjunction $L))) 
    (narz-include 
      (:: $M) $L))
; 

  (= 
    (nars-implication $Ctx 
      (negation (disjunction $L)) 
      (negation $M)) 
    (narz-include 
      (:: $M) $L))
; 


  (= 
    (nars-implication $Ctx 
      (implication $S $P) 
      (implication 
        (conjunction $Ls) 
        (conjunction $Lp))) 
    ( (narz-ground $Ls) 
      (narz-ground $Lp) 
      (narz-replace $Ls $S $L $P) 
      (narz-same $L $Lp)))
; 

  (= 
    (nars-implication $Ctx 
      (implication $S $P) 
      (implication 
        (disjunction $Ls) 
        (disjunction $Lp))) 
    ( (narz-ground $Ls) 
      (narz-ground $Lp) 
      (narz-replace $Ls $S $L $P) 
      (narz-same $L $Lp)))
; 

  (= 
    (nars-implication $Ctx 
      (equivalence $S $P) 
      (equivalence 
        (conjunction $Ls) 
        (conjunction $Lp))) 
    ( (narz-ground $Ls) 
      (narz-ground $Lp) 
      (narz-replace $Ls $S $L $P) 
      (narz-same $L $Lp)))
; 

  (= 
    (nars-implication $Ctx 
      (equivalence $S $P) 
      (equivalence 
        (disjunction $Ls) 
        (disjunction $Lp))) 
    ( (narz-ground $Ls) 
      (narz-ground $Lp) 
      (narz-replace $Ls $S $L $P) 
      (narz-same $L $Lp)))
; 



;
; 


  (= 
    (equivalence $X $Y) 
    ( (nars-ctx $Ctx) (nars-equivalence $Ctx $X $Y)))
; 



  (= 
    (nars-equivalence $Ctx $X $Y) 
    ( (narz-ground $X) 
      (narz-reduce $X $Y) 
      (\== $X $Y) 
      (set-det)))
; 


  (= 
    (nars_equivalence  $Ctx 
      (similarity  $S $P) 
      (similarity  $P $S)) True)
; 


  (= 
    (nars_equivalence  $Ctx 
      (inheritance  $S 
        (ext_set  
          ($P))) 
      (similarity  $S 
        (ext_set  
          ($P)))) True)
; 

  (= 
    (nars_equivalence  $Ctx 
      (inheritance  
        (int_set  
          ($S)) $P) 
      (similarity  
        (int_set  
          ($S)) $P)) True)
; 


  (= 
    (nars-equivalence $Ctx 
      (inheritance $S 
        (ext-intersection $Lp)) 
      (conjunction $L)) 
    (findall 
      (nars-inheritance $Ctx $S $P) 
      (member $P $Lp) $L))
; 

  (= 
    (nars-equivalence $Ctx 
      (inheritance 
        (int-intersection $Ls) $P) 
      (conjunction $L)) 
    (findall 
      (nars-inheritance $Ctx $S $P) 
      (member $S $Ls) $L))
; 


  (= 
    (nars_equivalence  $Ctx 
      (inheritance  $S 
        (ext_difference  $P1 $P2)) 
      (conjunction  
        ( (inheritance  $S $P1) (negation  (inheritance  $S $P2))))) True)
; 

  (= 
    (nars_equivalence  $Ctx 
      (inheritance  
        (int_difference  $S1 $S2) $P) 
      (conjunction  
        ( (inheritance  $S1 $P) (negation  (inheritance  $S2 $P))))) True)
; 


  (= 
    (nars-equivalence $Ctx 
      (inheritance 
        (product $Ls) 
        (product $Lp)) 
      (conjunction $L)) 
    (equ-product $Ls $Lp $L))
; 


  (= 
    (nars-equivalence $Ctx 
      (inheritance 
        (product (Cons  $S $L)) 
        (product (Cons  $P $L))) 
      (inheritance $S $P)) 
    (narz-ground $L))
; 

  (= 
    (nars-equivalence $Ctx 
      (inheritance $S $P) 
      (inheritance 
        (product (Cons  $H $Ls)) 
        (product (Cons  $H $Lp)))) 
    ( (narz-ground $H) (nars-equivalence $Ctx (inheritance (product $Ls) (product $Lp)) (inheritance $S $P))))
; 


  (= 
    (nars-equivalence $Ctx 
      (inheritance 
        (product $L) $R) 
      (inheritance $T 
        (ext-image $R $L1))) 
    (narz-replace $L $T $L1))
; 

  (= 
    (nars-equivalence $Ctx 
      (inheritance $R 
        (product $L)) 
      (inheritance 
        (int-image $R $L1) $T)) 
    (narz-replace $L $T $L1))
; 


  (= 
    (nars_equivalence  $Ctx 
      (equivalence  $S $P) 
      (equivalence  $P $S)) True)
; 


  (= 
    (nars_equivalence  $Ctx 
      (equivalence  
        (negation  $S) $P) 
      (equivalence  
        (negation  $P) $S)) True)
; 


  (= 
    (nars-equivalence $Ctx 
      (conjunction $L1) 
      (conjunction $L2)) 
    (narz-same-set $L1 $L2))
; 

  (= 
    (nars-equivalence $Ctx 
      (disjunction $L1) 
      (disjunction $L2)) 
    (narz-same-set $L1 $L2))
; 


  (= 
    (nars-equivalence $Ctx 
      (implication $S 
        (conjunction $Lp)) 
      (conjunction $L)) 
    (findall 
      (nars-implication $Ctx $S $P) 
      (member $P $Lp) $L))
; 

  (= 
    (nars-equivalence $Ctx 
      (implication 
        (disjunction $Ls) $P) 
      (conjunction $L)) 
    (findall 
      (nars-implication $Ctx $S $P) 
      (member $S $Ls) $L))
; 


  (= 
    (nars-equivalence $Ctx $T1 $T2) 
    ( (not (atom $T1)) 
      (not (atom $T2)) 
      (narz-ground $T1) 
      (narz-ground $T2) 
      (=.. $T1 $L1) 
      (=.. $T2 $L2) 
      (nars-equivalence-list $Ctx $L1 $L2)))
; 



  (= 
    (nars_equivalence_list  $Ctx $L $L) True)
; 

  (= 
    (nars-equivalence-list $Ctx 
      (Cons  $H $L1) 
      (Cons  $H $L2)) 
    (nars-equivalence-list $Ctx $L1 $L2))
; 

  (= 
    (nars-equivalence-list $Ctx 
      (Cons  $H1 $L1) 
      (Cons  $H2 $L2)) 
    ( (nars-similarity $Ctx $H1 $H2) (nars-equivalence-list $Ctx $L1 $L2)))
; 

  (= 
    (nars-equivalence-list $Ctx 
      (Cons  $H1 $L1) 
      (Cons  $H2 $L2)) 
    ( (nars-equivalence $Ctx $H1 $H2) (nars-equivalence-list $Ctx $L1 $L2)))
; 


;
; 



  (= 
    (narz-reduce 
      (similarity 
        (ext-set (:: $S)) 
        (ext-set (:: $P))) 
      (similarity $S $P)) 
    (set-det))
; 

  (= 
    (narz-reduce 
      (similarity 
        (int-set (:: $S)) 
        (int-set (:: $P))) 
      (similarity $S $P)) 
    (set-det))
; 


  (= 
    (narz-reduce 
      (instance $S $P) 
      (inheritance 
        (ext-set (:: $S)) $P)) 
    (set-det))
; 

  (= 
    (narz-reduce 
      (property $S $P) 
      (inheritance $S 
        (int-set (:: $P)))) 
    (set-det))
; 

  (= 
    (narz-reduce 
      (inst-prop $S $P) 
      (inheritance 
        (ext-set (:: $S)) 
        (int-set (:: $P)))) 
    (set-det))
; 


  (= 
    (narz-reduce 
      (ext-intersection (:: $T)) $T) 
    (set-det))
; 

  (= 
    (narz-reduce 
      (int-intersection (:: $T)) $T) 
    (set-det))
; 


  (= 
    (narz-reduce 
      (ext-intersection (:: (ext-intersection $L1) (ext-intersection $L2))) 
      (ext-intersection $L)) 
    ( (nars-union $L1 $L2 $L) (set-det)))
; 

  (= 
    (narz-reduce 
      (ext-intersection (:: (ext-intersection $L1) $L2)) 
      (ext-intersection $L)) 
    ( (nars-union $L1 
        (:: $L2) $L) (set-det)))
; 

  (= 
    (narz-reduce 
      (ext-intersection (:: $L1 (ext-intersection $L2))) 
      (ext-intersection $L)) 
    ( (nars-union 
        (:: $L1) $L2 $L) (set-det)))
; 

  (= 
    (narz-reduce 
      (ext-intersection (:: (ext-set $L1) (ext-set $L2))) 
      (ext-set $L)) 
    ( (intersection $L1 $L2 $L) (set-det)))
; 

  (= 
    (narz-reduce 
      (ext-intersection (:: (int-set $L1) (int-set $L2))) 
      (int-set $L)) 
    ( (nars-union $L1 $L2 $L) (set-det)))
; 


  (= 
    (narz-reduce 
      (int-intersection (:: (int-intersection $L1) (int-intersection $L2))) 
      (int-intersection $L)) 
    ( (nars-union $L1 $L2 $L) (set-det)))
; 

  (= 
    (narz-reduce 
      (int-intersection (:: (int-intersection $L1) $L2)) 
      (int-intersection $L)) 
    ( (nars-union $L1 
        (:: $L2) $L) (set-det)))
; 

  (= 
    (narz-reduce 
      (int-intersection (:: $L1 (int-intersection $L2))) 
      (int-intersection $L)) 
    ( (nars-union 
        (:: $L1) $L2 $L) (set-det)))
; 

  (= 
    (narz-reduce 
      (int-intersection (:: (int-set $L1) (int-set $L2))) 
      (int-set $L)) 
    ( (intersection $L1 $L2 $L) (set-det)))
; 

  (= 
    (narz-reduce 
      (int-intersection (:: (ext-set $L1) (ext-set $L2))) 
      (ext-set $L)) 
    ( (nars-union $L1 $L2 $L) (set-det)))
; 


  (= 
    (narz-reduce 
      (ext-difference 
        (ext-set $L1) 
        (ext-set $L2)) 
      (ext-set $L)) 
    ( (nars-subtract $L1 $L2 $L) (set-det)))
; 

  (= 
    (narz-reduce 
      (int-difference 
        (int-set $L1) 
        (int-set $L2)) 
      (int-set $L)) 
    ( (nars-subtract $L1 $L2 $L) (set-det)))
; 


  (= 
    (narz-reduce 
      (product 
        (product $L) $T) 
      (product $L1)) 
    ( (append $L 
        (:: $T) $L1) (set-det)))
; 


  (= 
    (narz-reduce 
      (ext-image 
        (product $L1) $L2) $T1) 
    ( (member $T1 $L1) 
      (narz-replace $L1 $T1 $L2) 
      (set-det)))
; 

  (= 
    (narz-reduce 
      (int-image 
        (product $L1) $L2) $T1) 
    ( (member $T1 $L1) 
      (narz-replace $L1 $T1 $L2) 
      (set-det)))
; 


  (= 
    (narz-reduce 
      (negation (negation $S)) $S) 
    (set-det))
; 


  (= 
    (narz-reduce 
      (conjunction (:: $T)) $T) 
    (set-det))
; 

  (= 
    (narz-reduce 
      (disjunction (:: $T)) $T) 
    (set-det))
; 


  (= 
    (narz-reduce 
      (conjunction (:: (conjunction $L1) (conjunction $L2))) 
      (conjunction $L)) 
    ( (nars-union $L1 $L2 $L) (set-det)))
; 

  (= 
    (narz-reduce 
      (conjunction (:: (conjunction $L1) $L2)) 
      (conjunction $L)) 
    ( (nars-union $L1 
        (:: $L2) $L) (set-det)))
; 

  (= 
    (narz-reduce 
      (conjunction (:: $L1 (conjunction $L2))) 
      (conjunction $L)) 
    ( (nars-union 
        (:: $L1) $L2 $L) (set-det)))
; 


  (= 
    (narz-reduce 
      (disjunction 
        (disjunction $L1) 
        (disjunction $L2)) 
      (disjunction $L)) 
    ( (nars-union $L1 $L2 $L) (set-det)))
; 

  (= 
    (narz-reduce 
      (disjunction 
        (disjunction $L1) $L2) 
      (disjunction $L)) 
    ( (nars-union $L1 
        (:: $L2) $L) (set-det)))
; 

  (= 
    (narz-reduce 
      (disjunction $L1 
        (disjunction $L2)) 
      (disjunction $L)) 
    ( (nars-union 
        (:: $L1) $L2 $L) (set-det)))
; 


  (= 
    (narz_reduce  $X $X) True)
; 



;
; 


  (= 
    (nars-union $X $Y $Z) 
    (catch 
      (union $X $Y $Z) $_ fail))
; 

;
; 


  (= 
    (nars-subtract $X $Y $Z) 
    (catch 
      (subtract $X $Y $Z) $_ fail))
; 


;
; 



  (= 
    (equ_product  () () ()) True)
; 

  (= 
    (equ-product 
      (Cons  $T $Ls) 
      (Cons  $T $Lp) $L) 
    ( (equ-product $Ls $Lp $L) (set-det)))
; 

  (= 
    (equ-product 
      (Cons  $S $Ls) 
      (Cons  $P $Lp) 
      (Cons  
        (inheritance $S $P) $L)) 
    (equ-product $Ls $Lp $L))
; 



  (= 
    (narz-same-set $L1 $L2) 
    ( (\== $L1 Nil) 
      (\== $L1 
        (:: $_)) 
      (narz-same $L1 $L2) 
      (\== $L1 $L2)))
; 



  (= 
    (narz_same  () ()) True)
; 

  (= 
    (narz-same $L 
      (Cons  $H $T)) 
    ( (member $H $L) 
      (nars-subtract $L 
        (:: $H) $L1) 
      (narz-same $L1 $T)))
; 



  (= 
    (narz-include $L1 $L2) 
    ( (narz-ground $L2) 
      (include1 $L1 $L2) 
      (\== $L1 Nil) 
      (\== $L1 $L2)))
; 


 
  (= 
    (include1  () $_) True)
; 

  (= 
    (include1 
      (Cons  $H $T1) 
      (Cons  $H $T2)) 
    (include1 $T1 $T2))
; 

  (= 
    (include1 
      (Cons  $H1 $T1) 
      (Cons  $H2 $T2)) 
    ( (\== $H2 $H1) (include1 (Cons  $H1 $T1) $T2)))
; 



  (= 
    (narz_not_member  $_ ()) True)
; 

  (= 
    (narz-not-member $C 
      (Cons  $C $_)) 
    ( (set-det) (fail)))
; 

  (= 
    (narz-not-member 
      (:: $S $T) 
      (Cons  
        (:: $S1 $T) $_)) 
    ( (nars-equivalence $Ctx $S $S1) 
      (set-det) 
      (fail)))
; 

  (= 
    (narz-not-member $C 
      (Cons  $_ $L)) 
    (narz-not-member $C $L))
; 



  (= 
    (narz_replace  
      (Cons  $T $L) $T 
      (Cons  nil $L)) True)
; 

  (= 
    (narz-replace 
      (Cons  $H $L) $T 
      (Cons  $H $L1)) 
    (narz-replace $L $T $L1))
; 


  (= 
    (narz_replace  
      (Cons  $H1 $T) $H1 
      (Cons  $H2 $T) $H2) True)
; 

  (= 
    (narz-replace 
      (Cons  $H $T1) $H1 
      (Cons  $H $T2) $H2) 
    (narz-replace $T1 $H1 $T2 $H2))
; 



  (= 
    (narz-dependant 
      (var $V $L) $Y 
      (var $V 
        (Cons  $Y $L))) 
    (set-det))
; 

  (= 
    (narz-dependant 
      (Cons  $H $T) $Y 
      (Cons  $H1 $T1)) 
    ( (narz-dependant $H $Y $H1) 
      (narz-dependant $T $Y $T1) 
      (set-det)))
; 

  (= 
    (narz-dependant 
      (inheritance $S $P) $Y 
      (inheritance $S1 $P1)) 
    ( (narz-dependant $S $Y $S1) 
      (narz-dependant $P $Y $P1) 
      (set-det)))
; 

  (= 
    (narz-dependant 
      (ext-image $R $A) $Y 
      (ext-image $R $A1)) 
    ( (narz-dependant $A $Y $A1) (set-det)))
; 

  (= 
    (narz-dependant 
      (int-image $R $A) $Y 
      (int-image $R $A1)) 
    ( (narz-dependant $A $Y $A1) (set-det)))
; 

  (= 
    (narz_dependant  $X $_ $X) True)
; 



;
; 



  (= 
    (narz-f-rev 
      (:: $F1 $C1) 
      (:: $F2 $C2) 
      (:: $F $C)) 
    ( (< $C1 1) 
      (< $C2 1) 
      (is $M1 
        (/ $C1 
          (- 1 $C1))) 
      (is $M2 
        (/ $C2 
          (- 1 $C2))) 
      (is $F 
        (/ 
          (+ 
            (* $M1 $F1) 
            (* $M2 $F2)) 
          (+ $M1 $M2))) 
      (is $C 
        (/ 
          (+ $M1 $M2) 
          (+ 
            (+ $M1 $M2) 1)))))
; 



  (= 
    (narz-f-exp 
      (:: $F $C) $E) 
    (is $E 
      (+ 
        (* $C 
          (- $F 0.5)) 0.5)))
; 



  (= 
    (narz-f-neg 
      (:: $F1 $C1) 
      (:: $F $C1)) 
    (u-not $F1 $F))
; 



  (= 
    (narz-f-cnv 
      (:: $F1 $C1) 
      (:: 1 $C)) 
    ( (u-and 
        (:: $F1 $C1) $W) (u-w2c $W $C)))
; 



  (= 
    (narz-f-cnt 
      (:: $F1 $C1) 
      (:: 0 $C)) 
    ( (u-not $F1 $F0) 
      (u-and 
        (:: $F0 $C1) $W) 
      (u-w2c $W $C)))
; 



  (= 
    (narz-f-ded 
      (:: $F1 $C1) 
      (:: $F2 $C2) 
      (:: $F $C)) 
    ( (u-and 
        (:: $F1 $F2) $F) (u-and (:: $C1 $C2 $F) $C)))
; 



  (= 
    (narz-f-ana 
      (:: $F1 $C1) 
      (:: $F2 $C2) 
      (:: $F $C)) 
    ( (u-and 
        (:: $F1 $F2) $F) (u-and (:: $C1 $C2 $F2) $C)))
; 



  (= 
    (narz-f-res 
      (:: $F1 $C1) 
      (:: $F2 $C2) 
      (:: $F $C)) 
    ( (u-and 
        (:: $F1 $F2) $F) 
      (u-or 
        (:: $F1 $F2) $F0) 
      (u-and 
        (:: $C1 $C2 $F0) $C)))
; 



  (= 
    (narz-f-abd 
      (:: $F1 $C1) 
      (:: $F2 $C2) 
      (:: $F2 $C)) 
    ( (u-and 
        (:: $F1 $C1 $C2) $W) (u-w2c $W $C)))
; 



  (= 
    (narz-f-ind $T1 $T2 $T) 
    (narz-f-abd $T2 $T1 $T))
; 



  (= 
    (narz-f-exe 
      (:: $F1 $C1) 
      (:: $F2 $C2) 
      (:: 1 $C)) 
    ( (u-and 
        (:: $F1 $C1 $F2 $C2) $W) (u-w2c $W $C)))
; 



  (= 
    (narz_f_com  
      (0 $C1) 
      (0 $C2) 
      (0 0)) True)
; 

  (= 
    (narz-f-com 
      (:: $F1 $C1) 
      (:: $F2 $C2) 
      (:: $F $C)) 
    ( (u-or 
        (:: $F1 $F2) $F0) 
      (> $F0 0) 
      (is $F 
        (/ 
          (* $F1 $F2) $F0)) 
      (u-and 
        (:: $F0 $C1 $C2) $W) 
      (u-w2c $W $C)))
; 



  (= 
    (narz-f-int 
      (:: $F1 $C1) 
      (:: $F2 $C2) 
      (:: $F $C)) 
    ( (u-and 
        (:: $F1 $F2) $F) (u-and (:: $C1 $C2) $C)))
; 



  (= 
    (narz-f-uni 
      (:: $F1 $C1) 
      (:: $F2 $C2) 
      (:: $F $C)) 
    ( (u-or 
        (:: $F1 $F2) $F) (u-and (:: $C1 $C2) $C)))
; 



  (= 
    (narz-f-dif 
      (:: $F1 $C1) 
      (:: $F2 $C2) 
      (:: $F $C)) 
    ( (u-not $F2 $F0) 
      (u-and 
        (:: $F1 $F0) $F) 
      (u-and 
        (:: $C1 $C2) $C)))
; 



  (= 
    (narz-f-pnn 
      (:: $F1 $C1) 
      (:: $F2 $C2) 
      (:: $F $C)) 
    ( (u-not $F2 $F2n) 
      (u-and 
        (:: $F1 $F2n) $Fn) 
      (u-not $Fn $F) 
      (u-and 
        (:: $Fn $C1 $C2) $C)))
; 



  (= 
    (narz-f-npp 
      (:: $F1 $C1) 
      (:: $F2 $C2) 
      (:: $F $C)) 
    ( (u-not $F1 $F1n) 
      (u-and 
        (:: $F1n $F2) $F) 
      (u-and 
        (:: $F $C1 $C2) $C)))
; 



  (= 
    (narz-f-pnp 
      (:: $F1 $C1) 
      (:: $F2 $C2) 
      (:: $F $C)) 
    ( (u-not $F2 $F2n) 
      (u-and 
        (:: $F1 $F2n) $F) 
      (u-and 
        (:: $F $C1 $C2) $C)))
; 



  (= 
    (narz-f-nnn 
      (:: $F1 $C1) 
      (:: $F2 $C2) 
      (:: $F $C)) 
    ( (u-not $F1 $F1n) 
      (u-not $F2 $F2n) 
      (u-and 
        (:: $F1n $F2n) $Fn) 
      (u-not $Fn $F) 
      (u-and 
        (:: $Fn $C1 $C2) $C)))
; 


;
; 



  (= 
    (u-not $N0 $N) 
    ( (is $N 
        (- 1 $N0)) (set-det)))
; 



  (= 
    (u_and  
      ($N) $N) True)
; 

  (= 
    (u-and 
      (Cons  $N0 $Nt) $N) 
    ( (u-and $Nt $N1) 
      (is $N 
        (* $N0 $N1)) 
      (set-det)))
; 



  (= 
    (u_or  
      ($N) $N) True)
; 

  (= 
    (u-or 
      (Cons  $N0 $Nt) $N) 
    ( (u-or $Nt $N1) 
      (is $N 
        (- 
          (+ $N0 $N1) 
          (* $N0 $N1))) 
      (set-det)))
; 



  (= 
    (u-w2c $W $C) 
    ( (= $K 1) 
      (is $C 
        (/ $W 
          (+ $W $K))) 
      (set-det)))
; 






;
; 

;
; 


;
; 


  !(use-module (library heaps))
; 



  (= 
    (create-heap $E) 
    ( (empty-heap $H) (engine-create $_ (update-heap $H) $E)))
; 


  (= 
    (update-heap $H) 
    ( (engine-fetch $Command) 
      (det-if-then-else 
        (update-heap $Command $Reply $H $H1) True 
        (, 
          (= $H1 $H) 
          (= $Reply False))) 
      (engine-yield $Reply) 
      (update-heap $H1)))
; 


  (= 
    (update-heap 
      (add $Priority $Key) True $H0 $H) 
    (add-to-heap $H0 $Priority $Key $H))
; 

  (= 
    (update-heap 
      (get $Priority $Key) 
      (- $Priority $Key) $H0 $H) 
    (get-from-heap $H0 $Priority $Key $H))
; 



  (= 
    (heap-add $Priority $Key $E) 
    (engine-post $E 
      (add $Priority $Key) True))
; 



  (= 
    (heap-get $Priority $Key $E) 
    (engine-post $E 
      (get $Priority $Key) 
      (- $Priority $Key)))
; 


;
; 

;
; 



  (= 
    (priority 
      (:: $_ 
        (:: $F $C)) $P) 
    ( (narz-f-exp 
        (:: $F $C) $E) (is $P $E)))
; 



  (= 
    (input-event $Event) 
    (heap-add 1.0 $Event belief-events-queue))
; 



  (= 
    (derive-event $Event) 
    ( (priority $Event $P) (heap-add $P $Event belief-events-queue)))
; 



  (= 
    (inference-step $_) 
    (or 
      (, 
        (heap-get $Priority $Event belief-events-queue) 
        (heap-get $Priority2 $Event2 belief-events-queue) 
        (heap-add $Priority2 $Event2 belief-events-queue) 
        (inference $Event $Event2 $Conclusion) 
        (derive-event $Conclusion) 
        (write $Conclusion) 
        (nl)) True))
; 



  (= 
    (nars-main) 
    ( (create-heap belief-events-queue) (nars-main 1)))
; 

  (= 
    (nars-main $T) 
    ( (read-nal $X) (or (, (= $X 1) (write "performing 1 inference steps:") (nl) (inference-step $T) (write "done with 1 additional inference steps.") (nl) (nars-main (+ $T 1))) (, (\= $X 1) (write "Input: ") (write $X) (nl) (input-event $X) (nars-main (+ $T 1))))))
; 


;
; 


  (= 
    (read-nal $X) 
    (nal-read-clause current-input $X))
; 



  !(if (prolog-load-context reload False))
; 

  !(create-heap belief-events-queue)
; 

  !(endif *)
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



  !(fixup-exports *)
; 



