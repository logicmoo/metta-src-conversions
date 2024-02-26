;                                           (convert_to_metta_file  commands $_424050 index/commands.pl index/commands.metta)

  (= (keyword1  save '   	save in Prolog database')  True)
; /* commands */
  (= (keyword1  get '   	get from Prolog database')  True)
  (= (keyword1  show '   	show current')  True)
  (= (keyword1  count '   	count current')  True)
  (= (keyword1  del '   	delete')  True)
  (= (keyword1  add '   	add new')  True)
  (= (keyword1  init '   	initialise constraints')  True)
  (= (keyword1  find '   	find constraints')  True)
  (= (keyword1  check '   	check validity')  True)
  (= (keyword1  decomp '   	decompose relation')  True)
  (= (keyword1  comp '   	compose relations')  True)
  (= (keyword1  switch '   	switch on or off')  True)
  (= (keyword1  set '   	set level')  True)
  (= (keyword1  help '   	get help')  True)


  (= (keyword2  pos '   	positive tuples')  True)
  (= (keyword2  neg '   	negative tuples')  True)
  (= (keyword2  ics '   	integrity constraints')  True)
  (= (keyword2  calc '   	calculated relations')  True)


  (= (switch  cwa '    	Closed-World Assumption')  True)
  (= (switch  horn '    	display in Horn form')  True)
  (= (switch  eval '   	evaluate constraints')  True)
  (= (switch  debug '   	show debugging information')  True)


  (= (level  conf '    	confirmation level of constraint')  True)
  (= (level  acc '     	accuracy of constraint')  True)
  (= (level  split '     	splitting level of constraint')  True)


  !(forall 
    (^ $T 
      (keyword1 $W $T)) 
    (op 1200 fx $W))
  !(forall 
    (^ $T 
      (keyword2 $W $T)) 
    (op 1100 fx $W))
  !(forall 
    (^ $T 
      (level $W $T)) 
    (op 1100 fx $W))


  (= (commands $InICs $PosIn $NegIn)  
    (prompt-read '' $Command)
    (do-command $Command $InICs $PosIn $NegIn $OutICs $PosOut $NegOut)
    (set-det)
    (commands $OutICs $PosOut $NegOut))
  (= (commands $InICs $PosIn $NegIn)  
    (write no)
    (nl)
    (set-det)
    (commands $InICs $PosIn $NegIn))


  (= (do-command (save $X) $ICs $Pos $Neg $ICs $Pos $Neg)  
    (set-det)
    (save-command $X $ICs $Pos $Neg))
  (= (do-command (get $X) $InICs $PosIn $NegIn $OutICs $PosOut $NegOut)  
    (set-det)
    (get-command $X $InICs $PosIn $NegIn $OutICs $PosOut $NegOut))
  (= (do-command (show $X) $ICs $Pos $Neg $ICs $Pos $Neg)  
    (set-det)
    (show-command $X $ICs $Pos $Neg))
  (= (do-command (count $X) $ICs $Pos $Neg $ICs $Pos $Neg)  
    (set-det)
    (count-command $X $ICs $Pos $Neg))
  (= (do-command (del $X) $InICs $PosIn $NegIn $OutICs $PosOut $NegOut)  
    (set-det)
    (del-command $X $InICs $PosIn $NegIn $OutICs $PosOut $NegOut))
  (= (do-command (add $X) $InICs $PosIn $NegIn $OutICs $PosOut $NegOut)  
    (set-det)
    (add-command $X $InICs $PosIn $NegIn $OutICs $PosOut $NegOut))
  (= (do-command (init $X) $InICs $Pos $Neg $OutICs $Pos $Neg)  
    (set-det)
    (init-command $X $InICs $OutICs))
  (= (do-command (find $X) $InICs $PosIn $NegIn $OutICs $PosOut $NegOut)  
    (set-det)
    (find-command $X $InICs $PosIn $NegIn $OutICs $PosOut $NegOut))
  (= (do-command (check $X) $ICs $Pos $Neg $ICs $Pos $Neg)  
    (set-det)
    (check-command $X $ICs $Pos $Neg))
  (= (do-command (decomp $IC) $InICs $PosIn $Neg $OutICs $PosOut $Neg)  
    (set-det)
    (decompose $IC $PosIn $PosOut $InICs $OutICs))
  (= (do-command (comp $Rule) $ICs $PosIn $Neg $ICs $PosOut $Neg)  
    (set-det)
    (exec-rule $Rule $PosIn $PosNew)
    (append $PosIn $PosNew $PosOut))
  (= (do-command (switch $X) $ICs $Pos $Neg $ICs $Pos $Neg)  
    (set-det)
    (switch $X))
  (= (do-command (set $X) $ICs $Pos $Neg $ICs $Pos $Neg)  
    (set-det)
    (set $X))
  (= (do-command (help $Topic) $ICs $Pos $Neg $ICs $Pos $Neg)  
    (set-det)
    (help-command $Topic))
  (= (do-command $Command $ICs $Pos $Neg $ICs $Pos $Neg)  
    (call $Command))
  (= (do-command $Command $ICs $Pos $Neg $ICs $Pos $Neg)  
    (write ?))


  (= (save-command (pos $Rel) $ICs $Pos $Neg)  
    (save-pos $Rel $Pos))
  (= (save-command (neg $Rel) $ICs $Pos $Neg)  
    (save-neg $Rel $Neg))
  (= (save-command (ics $T) $ICs $Pos $Neg)  
    (save-ics $T $ICs))


  (= (get-command (pos $Rel) $ICs $PosIn $Neg $ICs $PosOut $Neg)  
    (get-pos $Rel $Pos)
    (append $PosIn $Pos $PosOut))
  (= (get-command (neg $Rel) $ICs $Pos $NegIn $ICs $Pos $NegOut)  
    (get-neg $Rel $Neg)
    (append $NegIn $Neg $NegOut))
  (= (get-command (ics $T) $InICs $Pos $Neg $OutICs $Pos $Neg)  
    (get-ics $T $ICs)
    (append $InICs $ICs $OutICs))


  (= (show-command all $ICs $Pos $Neg)  
    (set-det)
    (show-list 'positive tuples' $Pos)
    (show-list 'negative tuples' $Neg)
    (show-list 'integrity constraints' $ICs))
  (= (show-command rel $ICs $Pos $Neg)  
    (set-det)
    (bagof0 $R 
      (^ $A 
        (rel $R $A)) $Rels)
    (show-list relations $Rels))
  (= (show-command $Other $ICs $Pos $Neg)  
    (get-list $Other $ICs $Pos $Neg $List $Text)
    (set-det)
    (show-list $Text $List))
  (= (show-command $Wrong $ICs $Pos $Neg)  
    (show-list choices 
      (:: 'show all' 'show rel' 'show pos all' 'show pos Rel' 'show neg all' 'show neg Rel' 'show ics all' 'show ics Rel')))


  (= (count-command $Filter $ICs $Pos $Neg)  
    (get-list $Filter $ICs $Pos $Neg $List $Text)
    (length $List $N)
    (write-list (:: 'There are ' $N ' ' $Text .))
    (nl))


  (= (del_command  all $ICs $Pos $Neg () () ())  True)
  (= (del-command (ics $F) $InICs $Pos $Neg $OutICs $Pos $Neg)  
    (filter $InICs $F $Deleted $OutICs))
  (= (del-command (pos $Rel) $ICs $PosIn $Neg $ICs $PosOut $Neg)  
    (filter $PosIn $Rel $Deleted $PosOut))
  (= (del-command (neg $Rel) $ICs $Pos $NegIn $ICs $Pos $NegOut)  
    (filter $NegIn $Rel $Deleted $NegOut))
  (= (del-command (rel $R) $ICs $PosIn $NegIn $ICs $PosOut $NegOut)  
    ( (remove-is-symbol  &self 
        (rel  $R $AList)) 
      (filter $PosIn $Rel $_ $PosOut) 
      (filter $NegIn $Rel $_ $NegOut)))
  (= (del-command (calc $Rel) $InICs $Pos $Neg $OutICs $Pos $Neg)  
    ( (remove-all-atoms  &self 
        (proc  $Rel $Proc)) (remove (proc $Rel) $InICs $OutICs)))


  (= (add-command (ics $DisplayIC) $ICs $Pos $Neg (Cons  $IC $ICs) $Pos $Neg)  
    (display $IC $DisplayIC))
  (= (add_command  (pos  $Tuple) $ICs $Pos $Neg $ICs (Cons  $Tuple $Pos) $Neg)  True)
  (= (add_command  (neg  $Tuple) $ICs $Pos $Neg $ICs $Pos (Cons  $Tuple $Neg))  True)
  (= (add-command (rel $R $AList) $ICs $Pos $Neg $ICs $Pos $Neg)  
    (new-rel $R $AList))
  (= (add-command (calc $Rel) $InICs $PosIn $Neg (Cons  (proc $Rel) $OutICs) $PosOut $Neg)  
    (ask-proc $Rel)
    (filter $PosIn $Rel $RelTuples $_)
    (setof0 $T 
      (, 
        (member $T $RelTuples) 
        (satisfied $T Nil Nil Nil)) $SatTuples)
    (listdiff $PosIn $SatTuples $PosOut)
    (remove 
      (proc $Rel) $InICs $OutICs))
 


  (= (init-command (ics $Rel) $InICs $OutICs)  
    (init-ICs $Rel $ICs)
    (append $InICs $ICs $OutICs))


  (= (find-command ics $InICs $PosIn $NegIn $OutICs $PosOut $NegOut)  
    (char $InICs $PosIn $NegIn $OutICs $PosOut $NegOut))


  (= (check-command (ics $DisplayIC) $ICs $Pos $Neg)  
    (display $IC $DisplayIC)
    (check-ics $IC $Pos $Neg))
  (= (check-command (pos $Tuple) $ICs $Pos $Neg)  
    (set-det)
    (forall 
      (member $IC $ICs) 
      (check-ics $IC 
        (Cons  $Tuple $Pos) $Neg)))


  (= (help-command commands)  
    (set-det)
    (bagof0 
      (with_self  $W $T) 
      (keyword1 $W $T) $L)
    (show-list Commands $L))
  (= (help-command filters)  
    (set-det)
    (bagof0 
      (with_self  $W $T) 
      (keyword2 $W $T) $L)
    (show-list Filters $L))
  (= (help-command switches)  
    (set-det)
    (bagof0 
      (with_self  $W $T) 
      (switch $W $T) $L)
    (show-list Switches $L))
  (= (help-command levels)  
    (set-det)
    (bagof0 
      (with_self  $W $T) 
      (level $W $T) $L)
    (show-list Levels $L))
  (= (help-command $_)  
    (help-command commands)
    (help-command filters)
    (help-command switches)
    (help-command levels))


  (= (get-list (pos $Filter) $ICs $Pos $Neg $List 'positive tuples')  
    (filter $Pos $Filter $List))
  (= (get-list (neg $Filter) $ICs $Pos $Neg $List 'negative tuples')  
    (filter $Neg $Filter $List))
  (= (get-list (ics $Filter) $ICs $Pos $Neg $List 'integrity constraints')  
    (filter $ICs $Filter $Tmp)
    (compress $Tmp $List))


  (= (filter $In $F $Out)  
    (template $F $Template)
    (setof0 $Template 
      (member $Template $In) $Out))

  (= (filter $In $F $Out $Rest)  
    (filter $In $F $Out)
    (listdiff $In $Out $Rest))


  (= (new-rel $R $Attrs)  
    (rel $R $Attrs)
    (set-det))
  (= (new-rel $R $Attrs)  
    (rel $R $A)
    (set-det)
    (write 'Error: relation name already in use')
    (nl)
    (fail))
  (= (new-rel $R $Attrs)  
    (add-is-symbol  &self 
      (rel  $R $Attrs)))


  (= (ask-proc $Rel)  
    ( (prompt-read clause $Clause) 
      (add-is-symbol  &self 
        (proc  $Rel $Clause)) 
      (set-det) 
      (ask-proc $Rel)))
  (= (ask_proc  $Rel)  True)


  (= (get-pos $Rel $Pos)  
    (template $Rel $Tuple)
    (bagof0 $Tuple 
      (pos-tuple $Tuple) $Pos))


  (= (get-neg $Rel $Neg)  
    (template $Rel $Tuple)
    (bagof0 $Tuple 
      (neg-tuple $Tuple) $Neg))


  (= (get-ics $T $ICs)  
    (template $T $Templ)
    (bagof0 $Templ 
      (constraint $Templ) $ICs))


  (= (save-pos $Rel $Pos)  
    (template $Rel $T)
    (forall 
      (member $T $Pos) 
      (myassert (pos-tuple $T))))


  (= (save-neg $Rel $Neg)  
    (template $Rel $T)
    (forall 
      (member $T $Neg) 
      (myassert (neg-tuple $T))))


  (= (save-ics $T $ICs)  
    (template $T $Templ)
    (forall 
      (member $Templ $ICs) 
      (myassert (constraint $Templ))))


  (= (check-ics $IC $Pos $Neg)  
    (display $IC $DisplayIC)
    (evaluate $Pos $Neg $IC $Tuples $PosOut $NegOut $Answer)
    (write $DisplayIC)
    (det-if-then-else 
      (= $Answer refine) 
      (show-list ' is contradicted by' $Tuples) 
      (det-if-then-else 
        (= $Answer keep) 
        (, 
          (write ' is satisfied') 
          (nl)) 
        (det-if-then-else 
          (= $Answer 
            (keep $E)) 
          (, 
            (write-list (:: ' looks promising: ' $E)) 
            (nl)) 
          (det-if-then 
            (= $Answer 
              (ignore $E)) 
            (, 
              (write-list (:: ' has low confirmation: ' $E)) 
              (nl)))))))


  (= (switch $X)  
    ( (switch $X $T) 
      (remove-is-symbol  &self 
        (switched_on  $X)) 
      (set-det) 
      (write-list (:: $X ' is now off.')) 
      (nl)))
  (= (switch $X)  
    ( (switch $X $T) 
      (set-det) 
      (add-is-symbol  &self 
        (switched_on  $X)) 
      (write-list (:: $X ' is now on.')) 
      (nl)))
;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  index/commands.pl 251 28 7757))



  (= (set $X)  
    (=.. $X 
      (Cons  $Level $Rest))
    (level $Level $T)
    (set-det)
    (det-if-then-else 
      (= $Rest 
        (:: $Value)) 
      (, 
        (or 
          (, 
            (remove-is-symbol  &self 
              (level_set  $Level $V)) 
            (set-det)) True) 
        (add-is-symbol  &self 
          (level_set  $Level $Value))) 
      (det-if-then 
        (= $Rest Nil) 
        (, 
          (get-level $Level $L) 
          (write (= $Level $L)) 
          (nl)))))
  (= (set $_)  
    (setof0 
      (= $L $V) 
      (^ $T 
        (, 
          (level $L $T) 
          (get-level $L $V))) $L)
    (show-list levels $L))


  (= (get-level $Level $Value)  
    (level $Level $T)
    (or 
      (, 
        (level-set $Level $Value) 
        (set-det)) 
      (= $Value 0)))


  (= (compare $X)  
    (=.. $X 
      (:: $Level $Value))
    (get-level $Level $L)
    (det-if-then-else 
      (= $L 
        (+ $V)) 
      (>= $Value $V) 
      (det-if-then-else 
        (= $L 
          (- $V)) 
        (=< $Value $V) 
        (det-if-then-else 
          (= $L 
            (/ $V $A)) 
          (, 
            (is $Upper 
              (+ $V $A)) 
            (=< $Value $Upper) 
            (is $Lower 
              (- $V $A)) 
            (>= $Value $Lower)) 
          (det-if-then otherwise 
            (, 
              (write-list (:: 'Wrong level: ' (= $Level $L))) 
              (nl) 
              (break)))))))

