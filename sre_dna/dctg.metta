;                                           (convert_to_metta_file  dctg $_196846 sre_dna/dctg.pl sre_dna/dctg.metta)

  !(op 650 yfx ^^)
; /* logic compilation of Definite Clause Translation Grammar rules */
  !(op 601 xfy :)
  !(op 1150 xfx ::=)
  !(op 1175 xfx <:>)
  !(op 1150 xfx ::-)



  (= (translate-rule (<:> (::= $LP Nil) $Sem) $H)  
    (set-det)
    (t-lp $LP Nil $S $S $Sem $H))
; /* The form of a rule is: LP ::= RP <:> Args ::- Sem */

  (= (translate-rule (::= $LP Nil) $H)  
    (set-det)
    (t-lp $LP Nil $S $S Nil $H))

  (= (translate-rule (<:> (::= $LP $RP) $Sem) (= $H  $B))  
    (set-det)
    (t-rp $RP Nil $StL $S $SR $B1)
    (reverse $StL $RStL)
    (t-lp $LP $RStL $S $SR $Sem $H)
    (tidy $B1 $B))

  (= (translate-rule (::= $LP $RP) (= $H  $B))  
    (translate-rule 
      (<:> 
        (::= $LP $RP) Nil) 
      (= $H  $B)))


  (= (t-lp (, $LP $List) $StL $S $SR $Sem $H)  
    (append $List $SR $List2)
    (prod-number $Number)
    (assert-semantic-rule $Number $LP $StL $Sem)
    (add-extra-args 
      (:: 
        (node $LP $StL $Number) $S $List2) $LP $H))

  (= (t-lp $LP $StL $S $SR $Sem $H)  
    (prod-number $Number)
    (assert-semantic-rule $Number $LP $StL $Sem)
    (add-extra-args 
      (:: 
        (node $LP $StL $Number) $S $SR) $LP $H))


  (= (t-rp (set-det) $St $St $S $S (set-det))  
    (set-det))

  (= (t-rp Nil $St (Cons  Nil $St) $S $S1 (= $S $S1))  
    (set-det))

  (= (t-rp (:: $X) $St (Cons  (:: $NX) $St) $S $SR (c $S $X $SR))  
    (char $X $NX))

  (= (t-rp (:: $X) $St (Cons  (:: $X) $St) $S $SR (c $S $X $SR))  
    (set-det))

  (= (t-rp (Cons  $X $R) $St (Cons  (Cons  $NX $NR) $St) $S $SR (, (c $S $X $SR1) $RB))  
    (char $X $NX)
    (t-rp $R $St 
      (Cons  $NR $St) $SR1 $SR $RB))

  (= (t-rp (Cons  $X $R) $St (Cons  (Cons  $X $R) $St) $S $SR (, (c $S $X $SR1) $RB))  
    (set-det)
    (t-rp $R $St 
      (Cons  $R $St) $SR1 $SR $RB))

  (= (t-rp {$T } $St $St $S $S $T)  
    (set-det))

  (= (t-rp (, $T $R) $St $StR $S $SR (, $Tt $Rt))  
    (set-det)
    (t-rp $T $St $St1 $S $SR1 $Tt)
    (t-rp $R $St1 $StR $SR1 $SR $Rt))

  (= (t-rp (^^ $T $N) $St (Cons  $N $St) $S $SR $Tt)  
    (add-extra-args 
      (:: $N $S $SR) $T $Tt))

  (= (t-rp $T $St (Cons  $St1 $St) $S $SR $Tt)  
    (add-extra-args 
      (:: $St1 $S $SR) $T $Tt))


  (= (add-extra-args $L $T $T1)  
    (=.. $T $Tl)
    (append $Tl $L $Tl1)
    (=.. $T1 $Tl1))
 
;
; append([],L,L) :- !.
;
; append([X|R],L,[X|R1]) :- append(R,L,R1).

;
; reverse(X,RX) :- rev1(X,[],RX).
;
; 
;
; rev1([],R,R) :- !.
;
; 
;
; rev1([X|Y],Z,R) :- rev1(Y,[X|Z],R).


  (= (tidy (, $P1 $P2 $P3) $Q)  
    (tidy 
      (, $P1 $P2 $P3) $Q))

  (= (tidy (, $P1 $P2) (, $Q1 $Q2))  
    (set-det)
    (tidy $P1 $Q1)
    (tidy $P2 $Q2))

  (= (tidy $A $A)  
    (set-det))


  (= (char $X $NX)  
    (integer $X)
    (< $X 256)
    (set-det)
    (name $NX 
      (:: $X)))


  (= (c  (Cons  $X $S) $X $S)  True) 
  (= (grammar $File)  
    (seeing $Old)
    (see $File)
    (consume)
    (seen)
    (see $Old))
; /* defined as a system predicate */
; ; :- asserta(( term_expansion(T,E) :- translate_rule(T,E) , ! )). ; :- asserta(( term_expansion(T,E) :- process_rule(T,E) , ! )).


  (= (consume)  
    (repeat)
    (read $X)
    (check-it $X))


  (= (check-it $X)  
    (= $X end-of-file)
    (set-det)) 
  (= (check-it $X)  
    (process $X)
    (fail))


  (= (process $Grammar)  
    ( (or 
        (= $Grammar 
          (<:> $H $T)) 
        (= $Grammar 
          (::= $H $T))) 
      (set-det) 
      (translate-rule $Grammar $Clause) 
      (add-is-symbol  &self $Clause) 
      (set-det)))

  (= (process !$G)  
    ( (set-det) $G))
; ; Execute a command

  (= (process (= $P  $Q))  
    ( (set-det) (add-is-symbol  &self (:-  $P $Q))))
; ; Store a normal clause

  (= (process $P)  
    (add-is-symbol  &self $P))
; ; Store a unit clause


  (= (^^ (node $NonTerminal $Trees $Index) $Args)  
    (semantic-rule $Index $Args $NonTerminal $Trees))
; /* process_rule(T,E) :- translate_rule(T,E), !, assert(T). */ ;
; fast?


  (= (prod-number $X)  
    ( (remove-is-symbol  &self 
        (rule_number  $X)) 
      (is $X1 
        (+ $X 1)) 
      (add-is-symbol  &self 
        (rule_number  $X1))))
; /* get_sem(NonTerminal,Trees,Index,Head) :- semantic_rule(Index,NonTerminal,Trees,Head). */


  !(dynamic (/ rule-number 1))


  (= (rule_number  0)  True)


  (= (assert-semantic-rule $Number $LP $StL (, $Rule $Rules))  
    ( (set-det) 
      (or 
        (= $Rule 
          (::- $Head $Body)) 
        (, 
          (= $Head $Rule) 
          (= $Body True))) 
      (add-is-symbol  &self 
        (:-  
          (semantic_rule  $Number $Head $LP $StL) 
          (,  ! $Body))) 
      (assert-semantic-rule $Number $LP $StL $Rules)))

  (= (assert-semantic-rule $Number $LP $StL $Rule)  
    ( (or 
        (= $Rule 
          (::- $Head $Body)) 
        (, 
          (= $Head $Rule) 
          (= $Body True))) (add-is-symbol  &self (:-  (semantic_rule  $Number $Head $LP $StL) (,  ! $Body)))))



