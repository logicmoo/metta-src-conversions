;                                           (convert_to_metta_file  multagnt $_234414 multagnt/multagnt.pl multagnt/multagnt.metta)

  !(dynamic (/ parent 2))
; /******************************************************************/
; /* MULTAGNT.PRO */
; /* Brazdil's Simulation of a tutoring setting between two agents */
; /******************************************************************/
; /* impl. by : Yiu Cheung HO */
; /* Department of Computing */
; /* King's College London */
; /* 1989 */
; /* */
; /* Thomas Hoppe */
; /* Mommsenstr. 50 */
; /* 1000 Berlin 12 */
; /* F.R.G. */
; /* E-Mail: hoppet@db0tui11.pro */
; /* 1990 */
; /* */
; /* reference : Transfer of Knowledge between Systems, */
; /* Brazdil, P., Associacao Portuguesa para a Intel-*/
; /* igencia Artificial, Working Paper 87-1, Uni- */
; /* versity of Porto, 1987. */
; /* */
; /* Diagnosis of Misunderstanding, Yiu Cheung HO, */
; /* Project Report, Final Year One Unit Project */
; /* 88/89, Department of Computing, King's College */
; /* London, 1989. */
; /* */
; /* call : diagnosis */
; /* */
; /******************************************************************/

  !(dynamic (/ db-entry 3))
; /******************************************************************/
; /* YAP-, C- and M-MeTTa specific declaration of dynamical */
; /* clauses. */
; /******************************************************************/
  !(dynamic (/ def-theory 2))
  !(dynamic (/ digits-of-next-sym 1))
 
  !(op 999 xfx :)
  !(op 998 xfx <-)


  (= (diagnosis)  
    (init)
    (get-teacher teacher)
    (nl)
    (nl)
    (get-learner)
    (locate-error))
; /******************************************************************/
; /* User Interface */
; /******************************************************************/
 

  (= (init)  
    (abolish db-entry 3)
    (abolish def-theory 2)
    (multifile (/ db-entry 3))
    (multifile (/ def-theory 2))
    (dynamic (/ db-entry 3))
    (dynamic (/ def-theory 2)))


  !(init *)

  !(:: (teacher *))

  !(:: (learner1 *))


  (= (locate-error)  
    (repeat)
    (mode $Mode)
    (generate-error $Mode))
 

  (= (generate-error manual)  
    (repeat)
    (get-question $Question)
    (process-question $Question)
    (exit-manual)
    (set-det)
    (exit))
  (= (generate-error auto)  
    (select-question $Question)
    (process-question $Question)
    (exit-auto)
    (set-det)
    (exit))
  (= (generate-error $_)  
    (exit))
 

  (= (process-question $Question)  
    (what-cannot-do $Ls $Ts 
      (<- $Question $Answer) Nil $FaultyStep)
    (output-error $Ls $Ts 
      (<- $Question $Answer) $FaultyStep)
    (set-det))
  (= (process-question $Question)  
    (write ' *** The teacher cannot answer the question: ')
    (write $Question)
    (nl))
 

  (= (output-error $Tl $Tt (<- $Question $Answer) $FaultyStep)  
    (nl)
    (write ' Result of Diagnosis:')
    (nl)
    (write  --------------------)
    (nl)
    (nl)
    (write ' The query is:       ')
    (write $Question)
    (nl)
    (write ' Teachers answer is: ')
    (out-answer $Answer)
    (nl)
    (write ' Learners theory:    ')
    (write $Tl)
    (nl)
    (write ' Teachers theory:    ')
    (write $Tt)
    (nl)
    (write ' Faulty Steps:       ')
    (out-faulty $FaultyStep)
    (nl)
    (set-det))


  (= (out-faulty Nil)  
    (write 'no faulty step')
    (nl))
  (= (out-faulty $Steps)  
    (out-step $Steps)
    (nl))
 

  (= (out-step Nil)  
    (nl))
  (= (out-step (Cons  $Step $Steps))  
    (write                      )
    (write $Step)
    (nl)
    (write $Steps))
 

  (= (out-answer Nil)  
    (write True)
    (nl))
  (= (out-answer $Ans)  
    (out-ans $Ans))
 

  (= (out-ans Nil)  
    (nl))
  (= (out-ans (Cons  (val $Var $Val) $T))  
    (write (= $Var $Val))
    (nl)
    (write                      )
    (out-ans $T))
 

  (= (select-question $Question)  
    (generate-question $Question)
    (yes-no yes confirm $Reply)
    (= $Reply yes))
  (= (select-question $_)  
    (write ' no more questions')
    (nl)
    (set-det)
    (fail))
 

  (= (generate-question $Question)  
    (db-entry 
      (with_self  
        (teacher *) $_) $Question $_)
    (make-ground-term $Question)
    (nl)
    (write ' Question generated: ')
    (write $Question)
    (nl))
 

  (= (get-question $Question)  
    (write ' Input question: ')
    (read $Question))
 

  (= (mode auto)  
    (nl)
    (nl)
    (yes-no yes ' Do you want the system to generate questions ? ' $Reply)
    (nl)
    (= $Reply yes)
    (set-det))
  (= (mode  manual)  True)
 

  (= (exit-manual)  
    (yes-no no ' Exit manual mode ? ' $Reply)
    (= $Reply yes))
 

  (= (exit-auto)  
    (yes-no no ' Exit auto mode ? ' $Reply)
    (= $Reply yes))
 

  (= (exit)  
    (yes-no no ' Quit ? ' $Reply)
    (= $Reply yes))
 

  (= (get-teacher $Teacher)  
    (yes-no yes ' Do you want to load the provided teacher KB ? ' $Reply)
    (load-knowledge-base $Reply $Teacher)
    (knowledge-base-list $Reply Nil $Teacher $FileList)
    (yes-no no ' Do you want to load another teacher KB ? ' $Reply2)
    (more-knowledge $Reply2 $FileList))
 

  (= (load_knowledge_base  no $_)  True)
  (= (load-knowledge-base yes $File)  
    (nl)
    (consult $File)
    (nl)
    (set-det))
 

  (= (more_knowledge  no (Cons  $_ $_))  True)
  (= (more-knowledge no Nil)  
    (write ' *** You have not load any knowledge base yet !')
    (nl)
    (more-knowledge yes Nil))
  (= (more-knowledge yes $FileList)  
    (repeat)
    (ask-file ' Please input the filename of the KB: ' $File)
    (not-loaded $File $FileList $Load)
    (load-knowledge-base $Load $File)
    (knowledge-base-list $Load $FileList $File $NewList)
    (yes-no no ' Do you want to consult more KBs ? ' $Reply)
    (more-knowledge $Reply $NewList))
 

  (= (not-loaded $File $List no)  
    (member $File $List)
    (set-det))
  (= (not_loaded  $_ $_ yes)  True)

;
; yesno(Question):- yesno(Question,no).
;
; yesno(Question, Default):- format('~N~w? (~w): ',[Question,Default]),get_single_char(YN), (YN = 13 -> Default==yes; member(YN, `yY`)).


  (= (yes-no $Default $Message $Reply)  
    (repeat)
    (write ' ')
    (write $Message)
    (det-if-then-else 
      (== $Default yes) 
      (write ' (Yes/no) ') 
      (write ' (yes/No) '))
    (get-single-char $In)
    (det-if-then-else 
      (= 
        (:: $In) 
        (:: 101)) 
      (, 
        (set-det) 
        (halt 4)) 
      (det-if-then-else 
        (= 
          (:: $In) 
          (:: 97)) 
        (, 
          (set-det) 
          (abort)) 
        (det-if-then-else 
          (= $In 13) 
          (= $Reply $Default) 
          (reply $In $Reply))))
    (set-det))
 

  (= (reply $Reply yes)  
    (member $Reply 
      (:: yes y yes. y. 89 121)))
  (= (reply $Reply no)  
    (member $Reply 
      (:: no n no. n. 78 110)))
 

  (= (ask-file $Message $File)  
    (repeat)
    (write ' ')
    (write $Message)
    (read-in $File)
    (set-det))
 

  (= (knowledge_base_list  yes $List $File (Cons  $File $List))  True)
  (= (knowledge_base_list  no $List $_ $List)  True)
 

  (= (no-knowledge Nil)  
    (write ' *** You have not load any knowledge base yet !')
    (nl))
 

  (= (get-learner)  
    (ask-file ' Please input the filename for the learner KB: ' $File)
    (load-knowledge-base yes $File)
    (knowledge-base-list yes Nil $File $List)
    (yes-no no ' Do you want to load another KB for the learner ? ' $Reply)
    (more-knowledge $Reply $List))
 

  (= (can-do (with_self  (learner *) $Tl) (with_self  (teacher *) $Tt) $Question $TeachersAnswer)  
    (demo 
      (with_self  
        (learner *) $Tl) $Question $LearnersAnswer)
    (can-do-1 
      (with_self  
        (teacher *) $Tt) $Question $TeachersAnswer $LearnersAnswer))
; /******************************************************************/
; /* Brazdil's predicates for evaluating the behavior of "LEARNER" */
; /* and "TEACHER". */
; /******************************************************************/
 

  (= (can-do-1 $Teacher $Question $TeachersAnswer $LearnersAnswer)  
    (demo $Teacher $Question $TeachersAnswer)
    (demo $Teacher $LearnersAnswer $TeachersAnswer)
    (demo $Teacher $TeachersAnswer $LearnersAnswer))
 

  (= (cannot-do (with_self  (learner *) $Tl) (with_self  (teacher *) $Tt) $Question $TeachersAnswer)  
    (not (demo (with_self  (learner *) $Tl) $Question $LearnersAnswer))
    (demo 
      (with_self  
        (teacher *) $Tt) $Question $TeachersAnswer))
  (= (cannot-do $Learner $Teacher $Question $_)  
    (can-do $Learner $Teacher $Question $_)
    (set-det)
    (fail))
  (= (cannot-do (with_self  (learner *) $Tl) (with_self  (teacher *) $Tt) $Question $TeachersAnswer)  
    (demo 
      (with_self  
        (learner *) $Tl) $Question $_)
    (demo 
      (with_self  
        (teacher *) $Tt) $Question $TeachersAnswer))
; ; It seems that the condition LearnersAnswer <> TeachersAnswer is missing !
 

  (= (what-cannot-do $_ $_ (<- $Q $_) $_ $_)  
    (not (all-ground-term $Q))
    (nl)
    (write ' *** You asked a non ground question !')
    (nl)
    (set-det)
    (fail))
  (= (what-cannot-do $Ls $Ts (<- $Q $A) $FaultyStep $FaultyStep)  
    (can-do $Ls $Ts $Q $A))
  (= (what-cannot-do $Ls $Ts (<- $Q $A) $F1 (Cons  (<- $Q $A) $F1))  
    (is-faulty-step $Ls $Ts $Q $A))
  (= (what-cannot-do $Ls $Ts (<- $Q $A) $F1 $F2)  
    (cannot-do $Ls $Ts $Q $A)
    (demo-trace2 $Ls $Ts $Q $A $SubSteps)
    (what-cannot-do-list $Ls $Ts $SubSteps $F1 $F3)
    (faulty-step $Q $A $F1 $F3 $F2))
 

  (= (is-faulty-step $Ls $Ts $Q $A)  
    (cannot-do $Ls $Ts $Q $A)
    (set-det)
    (not (demo-trace2 $Ls $Ts $Q $A $_)))
 

  (= (faulty_step  $Q $A $F1 $F1 (Cons  (<-  $Q $A) $F1))  True)
  (= (faulty_step  $Q $A $F1 $F3 $F3)  True)
 

  (= (what_cannot_do_list  $_ $_ () $F $F)  True)
  (= (what-cannot-do-list $Ls $Ts (Cons  $Step1 $RestSteps) $F1 $F3)  
    (what-cannot-do $Ls $Ts $Step1 $F1 $F2)
    (what-cannot-do-list $Ls $Ts $RestSteps $F2 $F3))
 

  (= (demo $Theory $Goal $Conditions)  
    (var $Conditions)
    (set-det)
    (check-goal $Goal)
    (copy-vars $Goal $LVars $Goal2 $LVars2)
    (set-det)
    (show $Theory $Goal2)
    (link-vals $LVars $LVars2 $Conditions)
    (make-ground-term $Conditions))
; /******************************************************************/
; /* */
; /* call : demo(+Theory,+Goal,Conditions) */
; /* */
; /* arguments : Theory = ground term denoting a theory */
; /* Goal = ground term or list of ground terms*/
; /* Conditions = substitutions */
; /* */
; /* property : backtrackable */
; /* */
; /******************************************************************/
; /* 'demo' is used to prove the Goal in the background of a Theory */
; /* delivering a substitution in Conditions. */
; /* Bindings of variables and values are explicitly maintained by */
; /* this implementation, thus any subterm of the form "variable(S)"*/
; /* must actually be of the form "variabl(<atom>)" where <atom> is */
; /* the name of a variable in the Goal. */
; /* The substitutions in Condition may be either a variable or a */
; /* list of terms which all have the form val(variable(X),Y) where */
; /* X is the name of a variable (an atom) and Y is any term. */
; /* In the case Condition is uninstantiated, demo succeeds iff */
; /* Goal can be proven within the Theory. Condition is then instan-*/
; /* tiated with the corresponding substitutions in the form */
; /* described above. On backtracking it will deliver the next */
; /* possible proof with the corresponding substitution, if it */
; /* exists. */
; /* If Condition is instantiated in the form described above, demo */
; /* succeeds, if Goal can be proven with the given substitution. */
; /******************************************************************/
  (= (demo $Theory $Goal $Conditions)  
    (nonvar $Conditions)
    (check-conditions $Conditions)
    (check-goal $Goal)
    (set-vars $Goal $Conditions $Goal2)
    (copy-vars $Goal2 $_ $Goal3 $LVars3)
    (set-det)
    (show $Theory $Goal3)
    (no-new-values $LVars3)
    (not (identified-vars $LVars3))
    (set-det))
 

  (= (demo-trace2 $Ls $Ts $Goal $Conditions $Steps)  
    (set-vars $Goal $Conditions $Goal2)
    (copy-vars $Goal2 $_ $Goal3 $_)
    (copy-vars $Goal2 $_ $Goal4 $_)
    (set-det)
    (db-entry $Ls $Goal3 $_)
    (set-det)
    (db-entry $Ts $Goal4 $Body)
    (make-ground-term $Body)
    (set-vars $Body $Conditions $Body2)
    (copy-vars $Body2 $_ $Body3 $_)
    (show $Ts $Body3)
    (make-ground-term $Body3)
    (trace-list $Body3 $Steps))
; /******************************************************************/
; /* Brazdil's predicate for locating erroneous LEARNER's knowledge */
; /******************************************************************/
 

  (= (trace_list  () ())  True)
  (= (trace-list (Cons  $SubGoal $Rest) (Cons  (<- $SubGoal $_) $Steps))  
    (trace-list $Rest $Steps))
 

  (= (show $_ Nil)  
    (set-det))
; /******************************************************************/
; /* */
; /* call : show(+Theory,+Goal) */
; /* */
; /* arguments : Theory = ground term denoting a theory */
; /* Goal = ground term or list of ground terms*/
; /* */
; /* property : backtrackable */
; /* */
; /******************************************************************/
; /* 'show' is nothing else than an MeTTa meta-interpreter working */
; /* in the traditional way, except that substitutions are explicit-*/
; /* ly represented through terms in the form 'val(<var>,<term>)'. */
; /******************************************************************/
  (= (show $Th (not $G))  
    (set-det)
    (not (show $Th $G)))
  (= (show $Th (not $G))  
    (set-det)
    (not (show $Th $G)))
  (= (show $Th (val $X $Y))  
    (set-det)
    (is-value $X $Y))
  (= (show $Th (Cons  $G $Gs))  
    (set-det)
    (show $Th $G)
    (show $Th $Gs))
  (= (show $Th $G)  
    (db-entry $Th $G $B)
    (show $Th $B))
  (= (show $Th $G)  
    (def-theory $Th $ThList)
    (member $SubTh $ThList)
    (show $SubTh $G))
  (= (show $_ $G)  
    (predicate-property $G built-in)
    (set-det)
    (call $G))
  (= (show $_ $G)  
    (predicate-property $G unknown)
    (dynamic $G)
    (fail))
  (= (show $_ $G)  
    (not (==  (= $G $_) (get-atoms  &self)))
    (call $G)
    (set-det))
 

  (= (is-value $X $Y)  
    (var $X)
    (var $Y)
    (set-det))
  (= (is-value $X $_)  
    (var $X)
    (set-det)
    (fail))
  (= (is-value $_ $Y)  
    (var $Y)
    (set-det)
    (fail))
  (= (is-value $X $X)  
    (atomic $X)
    (set-det))
  (= (is-value (Cons  $Head1 $Tail1) (Cons  $Head2 $Tail2))  
    (set-det)
    (is-value $Head1 $Head2)
    (is-value $Tail1 $Tail2))
  (= (is-value $X $Y)  
    (not (atomic $X))
    (not (atomic $Y))
    (=.. $X 
      (Cons  $F $ArgsX))
    (=.. $Y 
      (Cons  $F $ArgsY))
    (set-det)
    (is-value $ArgsX $ArgsY))
 

  (= (copy_vars  (variable  $G) ($G) $G2 ($G2))  True)
; /******************************************************************/
; /* Variable handling procedures */
; /******************************************************************/
; /* */
; /* call : copy_vars(+G,+LVars,-G2,-LVars2) */
; /* */
; /* arguments : G = ground term */
; /* LVars = list of variables in G */
; /* G2 = variablelized term */
; /* LVars2 = list of variables in G2 */
; /* */
; /******************************************************************/
; /* 'copy_vars' sets G2 to a copy of G with all variables of the */
; /* form 'variable(<name>)' replaced with uninstantiated MeTTa */
; /* variables. */
; /******************************************************************/
  (= (copy-vars $G Nil $G Nil)  
    (atomic $G))
  (= (copy-vars $G $LVars $G2 $LVars2)  
    (=.. $G 
      (Cons  $F $Args))
    (copy-vars-list $Args Nil $LVars $Args2 Nil $LVars2)
    (=.. $G2 
      (Cons  $F $Args2)))
 

  (= (copy_vars_list  () $LVars $LVars () $LVars2 $LVars2)  True)
  (= (copy-vars-list (Cons  $A $As) $PV $LV (Cons  $A2 $A2s) $PV2 $LV2)  
    (copy-vars $A $AVL $A2 $AVL2)
    (join-vars $AVL $PV $PVplus $AVL2 $PV2 $PV2plus)
    (copy-vars-list $As $PVplus $LV $A2s $PV2plus $LV2))
 

  (= (join_vars  () $PV $PV () $PV2 $PV2)  True)
  (= (join-vars (Cons  $X $AVL) $PVin $PVout (Cons  $X2 $AVL2) $PV2in $PV2out)  
    (twin-member $X $PVin $X2 $PV2in)
    (join-vars $AVL $PVin $PVout $AVL2 $PV2in $PV2out))
  (= (join-vars (Cons  $X $AVL) $PVin $PVout (Cons  $X2 $AVL2) $PV2in $PV2out)  
    (join-vars $AVL 
      (Cons  $X $PVin) $PVout $AVL2 
      (Cons  $X2 $PV2in) $PV2out))
 

  (= (twin_member  $Var (Cons  $Var $_) $Val (Cons  $Val $_))  True)
  (= (twin-member $Var (Cons  $_ $Tail1) $Val (Cons  $_ $Tail2))  
    (twin-member $Var $Tail1 $Val $Tail2))
 

  (= (link-vals (Cons  $X $LV) (Cons  $X2 $LV2) (Cons  (val (variable $X) $X2) $Conditions))  
    (link-vals $LV $LV2 $Conditions))
; /******************************************************************/
; /* */
; /* call : link_vals(+LVars1,+LVars2,-Cond) */
; /* */
; /* arguments : LVars1 = list of atomic ground terms */
; /* LVars2 = list of terms (can be MeTTa variables)*/
; /* Cond = combined substitution */
; /* */
; /******************************************************************/
; /* 'link_vals' combines each corresponding varible name in LVars1 */
; /* with its value in LVars2, to form a list of substituitions of */
; /* the form val(<var>,<term>). */
; /******************************************************************/
  (= (link_vals  () () ())  True)
 

  (= (set_vars  $Goal () $Goal)  True)
; /******************************************************************/
; /* */
; /* call : set_vars(+Goal1,+Cond,-Goal2) */
; /* */
; /* arguments : Goal1 = a ground goal */
; /* Cond = a substitution */
; /* Goal2 = Goal1 with substituted variables */
; /* */
; /******************************************************************/
; /* 'set_vars' substitutes variables depicted by 'variable(<name>)'*/
; /* in Goal1 by its value in Goal2, according to the substitution */
; /* Cond. */
; /******************************************************************/
  (= (set-vars $Goal (Cons  (val (variable $Var) $Val) $Rest) $ResultGoal)  
    (atomic $Var)
    (substitute $Goal 
      (variable $Var) $Val $Goal2)
    (set-det)
    (set-vars $Goal2 $Rest $ResultGoal))
 

  (= (substitute  $Var $Var $Val $Val)  True)
  (= (substitute $Goal $_ $_ $Goal)  
    (atomic $Goal)
    (set-det))
  (= (substitute (Cons  $Arg $Tail) $Var $Val (Cons  $NewArg $NewTail))  
    (set-det)
    (substitute $Arg $Var $Val $NewArg)
    (substitute $Tail $Var $Val $NewTail))
  (= (substitute $Goal $Var $Val $FinalGoal)  
    (=.. $Goal 
      (Cons  $F $Args))
    (substitute $Args $Var $Val $NewArgs)
    (=.. $FinalGoal 
      (Cons  $F $NewArgs)))
 

  (= (all-ground-term $Variable)  
    (var $Variable)
    (set-det)
    (fail))
; /******************************************************************/
; /* */
; /* call : all_ground_term(+Term) */
; /* */
; /* arguments : Term = a MeTTa term */
; /* */
; /******************************************************************/
; /* 'all_ground_term' succeeds if Term is ground, i.e. all vari- */
; /* ables are instantiated. Modification note: In YAP-MeTTa */
; /* all_ground_term(Term) :- ground(Term). */
; /* and in any other DEC10-MeTTa dialects */
; /* all_ground_term(Term) :- numbervars(Term,0,0). */
; /* can be used to speed up the system */
; /******************************************************************/
  (= (all-ground-term $Atomic)  
    (atomic $Atomic)
    (set-det))
  (= (all-ground-term (Cons  $Head $Tail))  
    (set-det)
    (all-ground-term $Head)
    (all-ground-term $Tail))
  (= (all-ground-term $Structure)  
    (=.. $Structure 
      (Cons  $_ $Args))
    (all-ground-term $Args))
 

  (= (check-goal $Goal)  
    (not (all-ground-term $Goal))
    (write ' *** Only ground terms in goal allowed !')
    (set-det)
    (fail))
  (= (check-goal $Goal)  
    (not (proper-variable $Goal))
    (write ' *** <name> of any variable(<name>) should be symbolic ground !')
    (set-det)
    (fail))
  (= (check_goal  $_)  True)
 

  (= (proper-variable $Atom)  
    (atomic $Atom)
    (set-det))
  (= (proper-variable (variable $Name))  
    (not (atomic $Name))
    (write ' *** variable(')
    (write $Name)
    (write ') not symbolic')
    (nl)
    (set-det)
    (fail))
  (= (proper-variable (Cons  $Head $Tail))  
    (set-det)
    (proper-variable $Head)
    (proper-variable $Tail))
  (= (proper-variable $Structure)  
    (=.. $Structure 
      (Cons  $_ $Args))
    (proper-variable $Args))
 

  (= (check-conditions $Cond)  
    (not (all-ground-term $Cond))
    (write ' *** Only ground terms in conditions allowed !')
    (set-det)
    (fail))
  (= (check-conditions $Cond)  
    (not (proper-format $Cond))
    (write ' *** Conditions should be either an uninstanziated variable')
    (nl)
    (write '     or a list of structures, val(variable(<name>),<value>) !')
    (set-det)
    (fail))
  (= (check_conditions  $_)  True)
 

  (= (proper_format  ())  True)
  (= (proper-format (Cons  (val (variable $Atom) $_) $Tail))  
    (atomic $Atom)
    (proper-format $Tail))


  (= (make-ground-term $Body3)  
    (make-ground-term 10 $Body3))

  (= (make-ground-term $D $Variable)  
    (var $Variable)
    (new-symbol $X)
    (= $Variable 
      (variable $X))
    (set-det))
  (= (make-ground-term $D $Atom)  
    (atomic $Atom)
    (set-det))
  (= (make-ground-term $D $_)  
    (== $D 0)
    (format user-error ~N~q~n 
      (:: (make-ground-term $D $_)))
    (set-det)
    (fail))
  (= (make-ground-term $D (Cons  $Head $Tail))  
    (set-det)
    (is $D2 
      (- $D 1))
    (make-ground-term $D2 $Head)
    (set-det)
    (make-ground-term $D2 $Tail))
; ; \+ is_list(Head),
  (= (make-ground-term $D $Structure)  
    (compound $Structure)
    (set-det)
    (is $D2 
      (- $D 1))
    (=.. $Structure 
      (Cons  $_ $Args))
    (make-ground-term $D2 $Args))
 

  (= (no_new_values  ())  True)
; /******************************************************************/
; /* */
; /* call : no_new_values(+List) */
; /* */
; /* arguments : List = a MeTTa list */
; /* */
; /******************************************************************/
; /* 'no_new_values' succeeds if List is a list of uninstantiated */
; /* variables. */
; /******************************************************************/
  (= (no-new-values (Cons  $X $Xs))  
    (var $X)
    (no-new-values $Xs))
 

  (= (identified-vars (Cons  $X $Xs))  
    (member $Y $Xs)
    (same-var $X $Y))
; /******************************************************************/
; /* */
; /* call : identified_vars(+List) */
; /* */
; /* arguments : List = a list of variables */
; /* */
; /******************************************************************/
; /* 'identified_vars' succeeds if there exists at least one vari- */
; /* able in the List, which has been 'unified' with another vari- */
; /* able in the list. */
; /******************************************************************/
  (= (identified-vars (Cons  $_ $T))  
    (identified-vars $T))
 

  (= (same-var dummy $Y)  
    (var $Y)
    (set-det)
    (fail))
; /******************************************************************/
; /* */
; /* call : same_var(+Var1,+Var2) */
; /* */
; /* arguments : Var1 = a MeTTa variable */
; /* Var2 = a MeTTa variable */
; /* */
; /******************************************************************/
; /* 'same_var' succeeds if Var1 and Var2 are unified, but uninstan-*/
; /* tiated. */
; /******************************************************************/
  (= (same-var $X $Y)  
    (var $X)
    (var $Y))
 

  (= (digits_of_next_sym  "1")  True)
; /******************************************************************/
; /* Miscelenous predicates */
; /******************************************************************/
 

  (= (new-symbol $X)  
    ( (digits-of-next-sym $LN) 
      (revzap $LN Nil $RLN) 
      (append "sym" $RLN $LS) 
      (name $X $LS) 
      (inc-digits $LN $LN2) 
      (remove-is-symbol  &self 
        (digits_of_next_sym  $LN)) 
      (add-is-symbol  &self 
        (digits_of_next_sym  $LN2))))
 

  (= (inc-digits (Cons  $D1 $LDT) (Cons  $D2 $LDT))  
    (< $D1 57)
    (is $D2 
      (+ $D1 1)))
  (= (inc-digits (Cons  $_ $LDT) (Cons  48 $LDT2))  
    (inc-digits $LDT $LDT2))
  (= (inc_digits  () (49))  True)
 

  (= (revzap (Cons  $H $T) $V $R)  
    (revzap $T 
      (Cons  $H $V) $R))
  (= (revzap  () $R $R)  True)
 

  (= (read-in $W)  
    (ignore-space $C)
    (rcl $C $L)
    (extract-space $L $L1)
    (convert $W $L1))
 

  (= (ignore-space $C)  
    (repeat)
    (get0 $C)
    (non-space $C))
 

  (= (rcl  10 ())  True)
  (= (rcl $C1 (Cons  $C1 $P))  
    (proper-char $C1)
    (get0 $C2)
    (rcl $C2 $P))
  (= (rcl $C1 (Cons  $C1 $P))  
    (space $C1)
    (get0 $C2)
    (rcl $C2 $P))
  (= (rcl $C1 $L)  
    (put 7)
    (get0 $C2)
    (rcl $C2 $L))
 

  (= (convert  () ())  True)
  (= (convert $W $L)  
    (name $W $L))
 

  (= (non-space $C)  
    (space $C)
    (set-det)
    (fail))
  (= (non-space 10)  
    (set-det)
    (fail))
  (= (non-space $C)  
    (proper-char $C))
  (= (non-space $_)  
    (put 7)
    (set-det)
    (fail))
 

  (= (space  32)  True)
  (= (space  9)  True)
 

  (= (proper-char $C)  
    (> $C 32)
    (< $C 128))
 

  (= (extract-space $L $L2)  
    (reverse $L $R)
    (delete-space $R $R2)
    (reverse $R2 $L2))
 

  (= (delete-space (Cons  $S $T) $L)  
    (space $S)
    (delete-space $T $L))
  (= (delete_space  $L $L)  True)


  (= (reverse  () ())  True)
  (= (reverse (Cons  $X $Y) $Z)  
    (reverse $Y $Y1)
    (append $Y1 
      (:: $X) $Z))


