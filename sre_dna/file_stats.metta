;                                           (convert-to-metta-file  file_stats $_436666 sre_dna/file_stats.pl sre_dna/file_stats.metta)
;
; ------------------------------------------------
;
; Jan 1999
;
; Author: Brian Ross
;
; Dept. of Computer Science, Brock University
;
; 
;
; Statistics and I/O


  !(expects-dialect sicstus)
  !(use-module (library (/ (/ dialect sicstus) system)))


  (= (print-tourn-stats $Gen)  
    (collect-stats $Gen)
    (gp-stats $Gen $Time $Best $Worst $Avg $AvgD $Lamarck)
    (det-if-then-else 
      (var $Lamarck) 
      (= $Lamarck 
        (lamarck 0 0 0)) True)
    (print-stat $Gen $Time $Best $Worst $Avg $AvgD $Lamarck)
    (det-if-then-else 
      (popn-dump-P yes) 
      (dump-population $Gen) True)
    (set-det))
; ; should replace with param passing

;
; collect_stats computes some statistics. gp_stats might already be
;
; asserted for generation by Lamarckian evol routine, if used. Retract it,
;
; but retain it's stat.


  (= (collect-stats $Gen)  
    ( (bagof $V 
        (^ $E 
          (^ $ID 
            (individual $ID $V $E))) $L) 
      (size-of $L $N) 
      (sum-list $L $Sum) 
      (max-list $L $Max) 
      (bagof $ID 
        (^ $E 
          (individual $ID $Max $E)) $WL) 
      (length $WL $SizeW) 
      (min-list $L $Min) 
      (individual $_ $Min $Bexpr) 
      (bagof $ID 
        (^ $E 
          (individual $ID $Min $E)) $BL) 
      (length $BL $SizeB) 
      (is $Avg 
        (/ $Sum $N)) 
      (bagof $D 
        (^ $ID 
          (^ $VV 
            (^ $E 
              (, 
                (individual $ID $VV $E) 
                (tree-depth $E $D))))) $M) 
      (average $M $AvgDepth) 
      (since-last-datime total call $HourT $MinuteT $SecT) 
      (since-last-datime generation retract $Hour $Minute $Sec) 
      (or 
        (remove-atom  &self 
          (gp-stats  $Gen $_ $_ $_ $_ $_ $_ $Lamarck)) True) 
      (add-atom  &self 
        (gp-stats  $Gen 
          (:  $Hour 
            (:  $Minute 
              (:  
                (/  $Sec $HourT) 
                (:  $MinuteT $SecT)))) 
          (best  $Min $SizeB $Bexpr) 
          (worst  $Max $SizeW) 
          (avg  $Avg) $AvgDepth $Lamarck)) 
      (set-det)))
; ;time_stamp(';h:;02i:;02s;a',T),
; ; datime(datime(_,_,_,Hour,Minute,Sec)),


  (= (since-last-datime $For $SetReset $Hour $Minute $Sec)  
    (once (or (call $SetReset (got-time $For $Was)) (= $Was 0)))
    (get-time $Now)
    (is $DiffTime 
      (- $Now $Was))
    (det-if-then-else 
      (== $SetReset retract) 
      (add-atom  &self 
        (got-time  $For $Now)) True)
    (stamp-date-time $DiffTime 
      (date $Year $Month $Day $Hour $Minute $Sec $_ $_ $_) UTC)
    (set-det))


  !(dynamic (/ got-time 2))
  !((get-time $Now) 
    (add-atom  &self 
      (got-time  total $Now)) 
    (add-atom  &self 
      (got-time  total $Now)))

;
; print run statistics

  (= (dump-stats $Run)  
    ( (wd-P $Dir) 
      (working-directory $_ $Dir) 
      (set-file-name "stats" $Run $File) 
      (tell $File) 
      (since-last-datime total retract $Hour $Min $Sec) 
      (datime (datime $Year $Month $Day $DA_Hour $DA_Min $DA_Sec)) 
      (writel (:: nl nl '***** Summary statistics: Run ' $Run  ***** nl nl (with-self  (- (- (- $Year $Month) $Day) $Hour) (with-self  $Min $Sec)) nl nl)) 
      (gp-stats $Gen $Time $Best $Worst $Avg $AvgDepth $Lamarck) 
      (det-if-then-else 
        (var $Lamarck) 
        (= $Lamarck 
          (lamarck 0 0 0)) True) 
      (print-stat $Gen $Time $Best $Worst $Avg $AvgDepth $Lamarck) fail))
; ; Windows
; ; Windows
; ;once(time_stamp('Date: ;W, ;d ;M ;y Time: ;c:;02i;a', DateTime)),
; ; loops for all				   ;
; loop driver
  (= (dump-stats $Run)  
    (nl nl 
      (population-size-P $InitPopSize $PopSize) 
      (max-runs-P $MaxRun $RunSoln $MaxGen) 
      (prob-crossover-P $PC) 
      (prob-internal-crossover-P $PIC) 
      (prob-terminal-mutation-P $PTM) 
      (max-depth-P $DepthInit $DepthCross) 
      (error-tolerance-P $Err) 
      (fitness-func-P $FitFile) 
      (dctg-file-P $FileDCTG) 
      (evaluator-reset-P $EvalReset $N) 
      (gen-type-P $GenType) 
      (best-in-run $Bgen $Fitness $Expr) 
      (count-nodes $Expr all $ENodeCnt) 
      (tree-depth $Expr $Edepth) 
      (tournament-size-P $TS $TR) 
      (lamarckian-P $LP $LK $Lsel $LCross) 
      (unique-population-P $Unique) 
      (seed-P $RanMode $Y) 
      (min-grammar-prob-P $MinProb) 
      (gen-set-size-P $GenSet) 
      (sre-mintestcnt-P $Mintst) 
      (max-string-length-P $Maxstr) 
      (writel (:: nl 'Best Soln:' $Expr nl)) 
      (^^ $Expr 
        (construct $E)) 
      (write 'Expression: ') 
      (sre-pp $E) nl 
      (writel (:: 'found at generation ' $Bgen nl 'Soln # nodes:' $ENodeCnt nl 'Soln depth:' $Edepth nl 'Soln Fitness = ' $Fitness nl)) 
      (det-if-then-else 
        (gp-stats $MaxGen $_ 
          (best $MinLast $_ $BexprLast) $_ $_ $_ $_) 
        (, 
          (count-nodes $BexprLast all $ENodeCntLast) 
          (tree-depth $BexprLast $EdepthLast) 
          (writel (:: nl 'Best Last Gen' $MaxGen :  $BexprLast nl)) 
          (^^ $BexprLast 
            (construct $ELast)) 
          (write 'Expression: ') 
          (sre-pp $ELast) nl 
          (writel (:: 'Last # nodes:' $ENodeCntLast nl 'Last depth:' $EdepthLast nl 'Last Fitness = ' $MinLast nl))) True) 
      (writel (:: ------- nl 'Fitness func file:' $FitFile nl 'DCTG file:' $FileDCTG nl 'Evaluator reset:' $EvalReset ' N:' $N nl 'Generation type:' $GenType nl 'Init pop size = ' $InitPopSize nl 'Pop size = ' $PopSize nl 'Max runs = ' $MaxRun ,  $RunSoln nl 'Max gen = ' $MaxGen nl 'Prob crossover = ' $PC nl 'Prob int cross = ' $PIC nl 'Prob term mutation = ' $PTM nl 'Tournament size: sel = ' $TS ' repl = ' $TR nl 'Lamarckian: ' $LP 'of popn, iterate = ' $LK ', select = ' $Lsel ', Prob Cross=' $LCross nl 'Unique popn = ' $Unique nl 'Random seed:' $RanMode ', Y=' $Y nl 'Max depth init = ' $DepthInit nl 'Max depth crossover = ' $DepthCross nl 'Initial test set size =' $GenSet nl 'Max test set string length =' $Maxstr nl 'Minimum test set count =' $Mintst nl 'Min grammar probability =' $MinProb nl 'Error tolerance = ' $Err nl)) 
      (writel (:: '*** End of Run ' $Run  *** nl)) told 
      (tell user) 
      (write-soln "soln" $Run $Expr)))
; ; from gp_parameters file...
; ;crossover_P(PIC, PTC),
; ;rep_limit_P(Rep),
; ; some other stats...
; ;sre_mutation_P(SREmut),
; ;mutation_range_P(Mutrange),
; ; if last generation never reached (soln found before)
; ;'SRE numeric mutation rate =', SREmut, nl,
; ;'SRE mutation range = +/-',Mutrange,nl,
; ;write('Best...'), nl, dna_summary(Expr), ; for DNA only
; ;write('Best optimized...'), nl,
; ;mask_optimize(Expr, Fitness, ExprOpt),
; ;dna_summary(ExprOpt), ; for DNA only
; /* (gp_stats(MaxGen,_, best(MinLast, _, BexprLast), _, _, _, _) -> write('Last...'), nl, dna_summary(BexprLast), mask_optimize(BexprLast, MinLast, LastOpt), ; for DNA only write('Last optimized...'), nl, dna_summary(LastOpt) ; true), */  ;
; <-- new.
	;
; write_soln("solnopt", Run, ExprOpt). ; <-- new.
	
  (= (set-file-name $RootName $Run $File)  
    (append $RootName "-" $File0)
    (name $Run $File1)
    (append $File0 $File1 $File2)
    (datime (datime $Year $Month $Day $Hour $Min $Sec))
    (name $Year $N1)
    (name $Month $N2)
    (name $Day $N3)
    (name $Hour $N4)
    (name $Min $N5)
    (name $Sec $N6)
    (append $N3 $N2 $N1a)
    (append $N1a $N1 $N1b)
    (append $N1b "-" $N1c)
    (append $N1c $N4 $N1d)
    (append $N1d $N5 $N1e)
    (append $N1e $N6 $File3)
    (append $File2 $File3 $File4)
    (append $File4 ".txt" $File5)
    (name $File $File5)
    (set-det))
; /* (gp_stats(MaxGen,_, best(MinLast, _, BexprLast), _, _, _, _) -> write_soln("last", Run, BexprLast), ; <-- new write_soln("lastopt", Run, LastOpt) ; true). */
; ; dump_population(Run).
; ;once(time_stamp('.;d;02n;02y-;02c;02i', Name3)),
; ;name(Name3, File3),


  (= (print-stat $Gen $Time (best $Bfit $Bcount $Bexpr) (worst $Wfit $Wcount) (avg $Avg) $AvgD $_)  
    (lamarckian-P 0.0 $_ $_ $_)
    (set-det)
    (writel (:: nl nl '---> Generation ' $Gen ( $Time ) nl 'Average fitness:' $Avg nl 'Best count: ' $Bcount nl 'Best example:' $Bexpr nl 'Best fitness = ' $Bfit nl 'Worst count: ' $Wcount nl 'Worst fitness = ' $Wfit nl 'Average Depth:' $AvgD nl 'Lamarckian evolution: off ' nl nl))
    (set-det))
  (= (print-stat $Gen $Time (best $Bfit $Bcount $Bexpr) (worst $Wfit $Wcount) (avg $Avg) $AvgD (lamarck $FitImpr $MaxImpr $NumGain))  
    (lamarckian-P $Percent $_ $_ $_)
    (population-size-P $_ $PopSize)
    (is $N 
      (integer (* $Percent $PopSize)))
    (det-if-then-else 
      (> $NumGain 0) 
      (is $AvgLam 
        (/ $FitImpr $NumGain)) 
      (= $AvgLam 0))
    (writel (:: nl nl '---> Generation ' $Gen ( $Time ) nl 'Average fitness:' $Avg nl 'Best count: ' $Bcount nl 'Best example:' $Bexpr nl 'Best fitness = ' $Bfit nl 'Worst count: ' $Wcount nl 'Worst fitness = ' $Wfit nl 'Average Depth:' $AvgD nl 'Lamarckian evolution: ' nl      $NumGain ' gains out of ' $N ' tries' nl '    Total gain:' $FitImpr nl '    Max single gain:' $MaxImpr nl '    Avg gain:' $AvgLam nl nl))
    (set-det))

;
; print existing population


  (= (dump-population $Run)  
    ( (set-file-name "popn" $Run $File) 
      (tell $File) 
      (individual $ID $V $Expr) 
      (write-individual $ID $V $Expr) fail))
  (= (dump-population $_)  
    (told (tell user)))


  (= (write-individual $ID $V $Expr)  
    ( (writel (:: ----- nl 'Individual ' $ID ': fit=' $V nl $Expr nl)) 
      (^^ $Expr 
        (construct $E)) 
      (sre-pp $E) nl 
      (set-det)))

;
; ------------------------------------
;
; solution dump: writes soln expression to a file, for input later.
;
; Grammatical expression is written in multiple lines, since the full
;
; expression is often larger than MeTTa's builtin "write" can handle.


  (= (write-soln $Name $Run $E)  
    ( (set-file-name $Name $Run $File) 
      (tell $File) 
      (write soln() 
      (write-term $E) 
      (write ).) nl told 
      (tell user) 
      (set-det)))


  (= (write-term (node $X $List $Y))  
    (set-det)
    (write node()
    (write $X)
    (write ,[)
    (write-tlist $List)
    (write ],)
    (write $Y)
    (write )))
  (= (write-term $X)  
    (write $X))


  (= (write-tlist Nil)  
    (set-det))
  (= (write-tlist (Cons  $X (Cons  $Y $Z)))  
    ( (set-det) 
      (write-term $X) 
      (write ,) nl 
      (write-tlist (Cons  $Y $Z))))
  (= (write-tlist (:: $X))  
    (write-term $X))


