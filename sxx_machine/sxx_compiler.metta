;                                           (convert_to_metta_file  sxx_compiler $_425594 sxx_machine/sxx_compiler.pl sxx_machine/sxx_compiler.metta)

  (= (append  () $L $L)  True)
  (= (append (Cons  $X $L1) $L2 (Cons  $X $L3))  
    (append $L1 $L2 $L3))


  (= (legacy-functor $P . $A)  
    (functor $P Cons $A)
    (set-det))
  (= (legacy-functor $P $F $A)  
    (functor $P $F $A)
    (set-det))


  (= (comp $File)  
    (see $File)
    (file-name-extension $BName $_ $File)
    (= $InProg Nil)
    (readprogram $BName $InProg $Prog)
    (set-det)
    (seen)
    (makename 
      (:: tcomp- $BName) $Dir)
    (make-directory-path $Dir)
    (set-det)
    (gencode $Dir $Prog)
    (set-det))
; ; close(File) ,


  (= (gencode  $_ ())  True)
  (= (gencode $Package (Cons  $Pred $Preds))  
    (gencodeforpred $Package $Pred)
    (gencode $Package $Preds))


  (= (local-side-efs $_ !$Call)  
    (call $Call)
    (set-det))
  (= (local-side-efs $_ $H)  
    (\= $H 
      (= $_  $_))
    (set-det)
    (local-side-efs $_ 
      (= $H  True)))
  (= (local-side-efs $_ (= $H  $_))  
    (or 
      (predicate-property $H static) 
      (predicate-property $H built-in))
    (set-det))
  (= (local-side-efs $_ $Call)  
    (call (add-is-symbol  &self $Call))
    (set-det))
  (= (local_side_efs  $_ $Clause)  True)

  (= (readprogram $BName $In $Out)  
    (read $Clause)
    (local-side-efs $BName $Clause)
    (det-if-then-else 
      (= $Clause end-of-file) 
      (= $In $Out) 
      (, 
        (binarize $Clause $BinCl) 
        (addclausetoprogram $BinCl $In $NewIn) 
        (readprogram $BName $NewIn $Out))))

;
; binarize((Head ::- Body),Cl) :- ! , Cl = (Head :- Body) .

  (= (binarize (= $Head  $Body) (= $BinHead  $BinBody))  
    (set-det)
    (addcont $Head $Continuation $BinHead)
    (makebinbody $Body $Continuation $BinBody))
  (= (binarize $Head (= $BinHead  
    (call $Continuation)))  
    (addcont $Head $Continuation $BinHead))


  (= (makebinbody (, $G $B) $C $NewBody)  
    (set-det)
    (makebinbody $B $C $NewB)
    (addcont $G $NewB $NewBody))
  (= (makebinbody $SpecialGoal $C $NewBody)  
    (specialgoal $SpecialGoal $G)
    (set-det)
    (addcont $G $C $NewBody))
  (= (makebinbody (set-det) $C $NewBody)  
    (set-det)
    (= $NewBody 
      (cut 1 $C)))
  (= (makebinbody $G $C $NewBody)  
    (set-det)
    (addcont $G $C $NewBody))


  (= (addclausetoprogram $Cl Nil $ProgOut)  
    (set-det)
    (= $ProgOut 
      (:: (:: $Cl))))
  (= (addclausetoprogram $Cl (Cons  $Pred $Rest) $Out)  
    (samepred $Cl $Pred)
    (set-det)
    (= $Out 
      (Cons  
        (Cons  $Cl $Pred) $Rest)))
  (= (addclausetoprogram $Cl (Cons  $Pred $Rest) (Cons  $Pred $NewRest))  
    (addclausetoprogram $Cl $Rest $NewRest))



  (= (samepred (= $H1  $_) (Cons  (= $H2  $_) $_))  
    (legacy-functor $H1 $N $A)
    (legacy-functor $H2 $N $A))






  (= (addcont (set-det) $C $FB)  
    (set-det)
    (= $FB 
      (cut 1 $C)))
  (= (addcont $S $C $NG)  
    (specialgoal $S $NS)
    (set-det)
    (addcont $NS $C $NG))
  (= (addcont $F $C $FB)  
    (=.. $F 
      (Cons  $N $A))
    (append $A 
      (:: $C) $NA)
    (=.. $FB 
      (Cons  $N $NA)))



  (= (makename $EOL $Out)  
    (== $EOL Nil)
    (set-det)
    (= $Out ''))
  (= (makename $Atom $Out)  
    (atomic $Atom)
    (set-det)
    (= $Out $Atom))
  (= (makename (Cons  $AC $Rest) $OutC)  
    (makename $Rest $OC)
    (name $OC $OL)
    (name $AC $AL)
    (append $AL $OL $OutL)
    (name $OutC $OutL))


  (= (writel  ())  True)
  (= (writel (Cons  $X $R))  
    (wr $X)
    (writel $R))


  (= (wr (wr nl))  
    (set-det)
    (nl))
  (= (wr $Atom)  
    (atomic $Atom)
    (set-det)
    (write $Atom))
  (= (wr $VAR)  
    (legacy-functor $VAR %VAR 2)
    (set-det)
    (write $VAR))
  (= (wr $Goal)  
    (call $Goal))


  (= (wrargs $N $T)  
    (wrargs $N $T $_))

  (= (wrargs $N $_ $_)  
    (= $N 0)
    (set-det))
  (= (wrargs $N $T $Komma)  
    (det-if-then-else 
      (var $Komma) 
      (= $Komma yes) 
      (write ,))
    (writel (:: $T a $N))
    (is $M 
      (- $N 1))
    (wrargs $M $T $Komma))

;
; Pred is a list of clauses for a specific predicate
;
; it is binarized and also in reverse order


  (= (gencodeforpred $Package $Pred)  
    (= $Pred 
      (Cons  
        (= $H  $_) $_))
    (legacy-functor $H $N $A)
    (is $AA 
      (- $A 1))
    (makename 
      (:: pred- $N - $AA) $ClassName)
    (makename 
      (:: $Package / $ClassName .java) $FileName)
    (setof $F 
      (^ $Pred 
        (continuationof $Pred $F)) $Conts)
    (setof $F 
      (^ $Pred 
        (stringof $Pred $F)) $AllStrings)
    (mysetof $F 
      (^ $Pred 
        (intof $Pred $F)) $AllInts)
    (= $Strings 
      (Cons  cut $AllStrings))
    (tell $FileName)
    (prelude $Pred)
    (writel (:: 'class /* Pred */' $ClassName ' extends Code' (wr nl) {  (wr nl) (declforeachclause $Pred 1 $ClassName) (declforeachcontinuation $Conts $N $A) (declforeachstring $Strings 0) (declforeachint $AllInts) 'public static Code entry-code = new ' $ClassName (); (wr nl) 'public void Init(Prolog mach)' (wr nl) {  (initforeachcontinuation $Conts $N $A) } (wr nl) 'public int Arity() { return ' $AA  ; } (wr nl) 'public Code Exec(Prolog mach)' (wr nl) '{ Term aregs[] = {' (aregarray $AA) } ; (wr nl) 'mach.CreateChoicePoint(aregs) ;' (wr nl) 'return cl1.Exec(mach) ;' (wr nl) } (wr nl) } (wr nl) (wr nl)))
    (genpredcode $Pred last $ClassName $_ $Strings)
    (told))
; ;close(FileName) .


  (= (mysetof $X $Y $Z)  
    (setof $X $Y $Z)
    (set-det))
  (= (mysetof  $_ $_ ())  True)


  (= (genpredcode  () $_ $_ 0 $_)  True)
  (= (genpredcode (Cons  $Clause $Rest) $Last $ClassName $M $Strings)  
    (genpredcode $Rest notlast $ClassName $N $Strings)
    (is $M 
      (+ $N 1))
    (genclausecode $Clause $Last $ClassName $M $Strings))


  (= (genclausecode $Clause $Last $ClassName $N $Strings)  
    (= $Clause 
      (= $Head  $Body))
    (legacy-functor $Head $Name $Arity)
    (is $Amin2 
      (- $Arity 2))
    (is $Amin1 
      (- $Arity 1))
    (arg $Arity $Head 
      ($cont $Arity))
    (writel (:: '/* helper */class ' $ClassName -- $N ' extends ' $ClassName (wr nl) { (wr nl) 'public Code Exec(Prolog mach)' (wr nl) {  (det-if-then-else (= $Last last) (write 'mach.RemoveChoice() ;') (, (is $M (+ $N 1)) (writel (:: mach.FillAlternative(cl $M ) ;)))) (wr nl) 'Term local-aregs[] = mach.Areg ;' (wr nl) 'Term continuation = local-aregs[' $Amin1 ] ; (wr nl) (decl-deref-args $Amin2) (mynumbervars $Clause 1 $NumbVars) (det-if-then-else (> $NumbVars 1) (declvars $NumbVars) True) (gets $Head $Strings) (puts $Body $Strings) 'mach.CUTB = mach.CurrentChoice ;' (wr nl) (nullifyaregs $Clause) 'return ' (bodycont $Clause $Name $Arity)  ; (wr nl) } (wr nl) } (wr nl) (wr nl))))


  (= (nullifyaregs (= $H  $B))  
    (legacy-functor $H $_ $HA)
    (legacy-functor $B $_ $BA)
    (is $Diff 
      (- $HA $BA))
    (> $Diff 0)
    (nullify $Diff $HA))
  (= (nullifyaregs  $_)  True)


  (= (nullify 0 $_)  
    (set-det)
    (write 'null ;')
    (nl)
    (fail))
  (= (nullify $N $K)  
    (is $L 
      (- $K 1))
    (writel (:: local-aregs[ $L ] = ))
    (is $M 
      (- $N 1))
    (nullify $M $L))


  (= (mynumbervars $Var $I $O)  
    (var $Var)
    (set-det)
    (= $Var 
      ($VAR $I $_))
    (is $O 
      (+ $I 1)))
  (= (mynumbervars ($VAR $_ $_) $I $O)  
    (set-det)
    (= $I $O))
  (= (mynumbervars $A $I $O)  
    (atomic $A)
    (set-det)
    (= $I $O))
  (= (mynumbervars $Term $I $O)  
    (=.. $Term 
      (Cons  $_ $Args))
    (mynumbervarslist $Args $I $O))


  (= (mynumbervarslist  () $I $I)  True)
  (= (mynumbervarslist (Cons  $T $R) $I $O)  
    (mynumbervars $T $I $II)
    (mynumbervarslist $R $II $O))



  (= (gets $Head $Strings)  
    (=.. $Head 
      (Cons  $_ $Args))
    (gets $Args 0 $Strings))

  (= (gets (:: $_) $_ $_)  
    (set-det)) ;
; this is the continuation !
  (= (gets (Cons  $Arg $Args) $N $Strings)  
    (is $M 
      (+ $N 1))
    (getforarg $Arg $N $Strings)
    (gets $Args $M $Strings))


  (= (getforarg $Arg $N $Strings)  
    (writel (:: 'if (!( (areg' $N ).Unify( (constructterm $Arg $Strings) '))) return Prolog.Fail0 ;' (wr nl))))



  (= (puts (cut $_ $C) $Strings)  
    (set-det)
    (writel (:: 'mach.DoCut(mach.CUTB)  ;' (wr nl)))
    (putforarg $C 0 $Strings))
  (= (puts $Body $Strings)  
    (=.. $Body 
      (Cons  $_ $Args))
    (puts $Args 0 $Strings))

  (= (puts  () $_ $_)  True)
  (= (puts (Cons  $Arg $Args) $N $Strings)  
    (is $M 
      (+ $N 1))
    (putforarg $Arg $N $Strings)
    (puts $Args $M $Strings))


  (= (putforarg $Arg $N $Strings)  
    (writel (:: local-aregs[ $N ] =  (constructterm $Arg $Strings)  ; (wr nl))))


  (= (declvars 1)  
    (set-det))
  (= (declvars $N)  
    (is $M 
      (- $N 1))
    (writel (:: 'Term var' $M ' = Term.Var(mach) ;' (wr nl)))
    (declvars $M))



;
; variables are replaced with '$VAR'(integer,_)
;
; continuation is replaced by '$cont'(arity)


  (= (constructterm ($cont $_) $_)  
    (set-det)
    (writel (:: continuation)))
  (= (constructterm ($VAR $N $T) $_)  
    (set-det)
    (det-if-then-else 
      (var $T) 
      (, 
        (writel (:: var $N)) 
        (= $T 1)) 
      (writel (:: var $N .Deref()))))
  (= (constructterm (cut $_ $C) $Strings)  
    (set-det)
    (getnameindex $Strings cut 0 $I)
    (writel (:: Term.Compound(string $I ', new HeapChoice(mach.CUTB),' (constructterm $C $Strings) ))))

  (= (constructterm $IntegerTerm $_)  
    (integer $IntegerTerm)
    (set-det)
    (posneg $IntegerTerm))
  (= (constructterm $Atom $Strings)  
    (atom $Atom)
    (set-det)
    (getnameindex $Strings $Atom 0 $I)
    (writel (:: Const.Intern(string $I ))))
  (= (constructterm $Term $Strings)  
    (legacy-functor $Term $Name $_)
    (=.. $Term 
      (Cons  $_ $Args))
    (getnameindex $Strings $Name 0 $I)
    (writel (:: Term.Compound(string $I , (newargs $Args 1 $Strings) ))))


  (= (newargs  () $_ $_)  True)
  (= (newargs (Cons  $A $Args) $N $Strings)  
    (det-if-then-else 
      (> $N 1) 
      (write ,) True)
    (is $M 
      (+ $N 1))
    (constructterm $A $Strings)
    (newargs $Args $M $Strings))
			




  (= (bodycont (= $_  $B) $Name $Arity)  
    (legacy-functor $B $Name $Arity)
    (set-det)
    (writel (:: entry-code)))
  (= (bodycont (= $_  $B) $_ $_)  
    (legacy-functor $B call 1)
    (set-det)
    (writel (:: Prolog.Call1)))
  (= (bodycont (= $_  $B) $_ $_)  
    (legacy-functor $B call 2)
    (set-det)
    (writel (:: Prolog.Call2)))
  (= (bodycont (= $_  $B) $_ $_)  
    (legacy-functor $B cut 2)
    (set-det)
    (writel (:: Prolog.Call1)))
  (= (bodycont (= $_  $B) $_ $_)  
    (legacy-functor $B $Name $Arity)
    (writel (:: $Name $Arity cont)))


  (= (decl-deref-args -1)  
    (set-det))
  (= (decl-deref-args $N)  
    (writel (:: 'Term areg' $N ' = local-aregs[' $N '].Deref() ;' (wr nl)))
    (is $M 
      (- $N 1))
    (decl-deref-args $M))



  (= (initforeachcontinuation  () $_ $_)  True)
  (= (initforeachcontinuation (Cons  (/ $N $A) $R) $N $A)  
    (set-det)
    (initforeachcontinuation $R $N $A))
; ; this is entry code !
  (= (initforeachcontinuation (Cons  (/ call 1) $R) $N $A)  
    (set-det)
    (initforeachcontinuation $R $N $A))
; ; Call1
  (= (initforeachcontinuation (Cons  (/ call 2) $R) $N $A)  
    (set-det)
    (initforeachcontinuation $R $N $A))
; ; Call2
  (= (initforeachcontinuation (Cons  (/ cut 2) $R) $N $A)  
    (set-det)
    (initforeachcontinuation $R $N $A))
; ; Cut2
  (= (initforeachcontinuation (Cons  (/ $N $A) $R) $Name $Arity)  
    (is $B 
      (- $A 1))
    (writel (:: $N $A 'cont = pred-' $N - $B '.entry-code ;' (wr nl)))
    (initforeachcontinuation $R $Name $Arity))
  (= (initforeachcontinuation (Cons  (/ $N $A) $R) $Name $Arity)  
    (is $B 
      (- $A 1))
    (writel (:: $N $A 'cont = mach.LoadPred("' $N ", $B ) ; (wr nl)))
    (initforeachcontinuation $R $Name $Arity))


  (= (aregarray $N)  
    (aregarray 0 $N))

  (= (aregarray $N $Max)  
    (> $N $Max)
    (set-det))
  (= (aregarray $N $Max)  
    (det-if-then-else 
      (> $N 0) 
      (write ,) True)
    (is $M 
      (+ $N 1))
    (writel (:: mach.Areg[ $N ]))
    (aregarray $M $Max)) 


  (= (declforeachclause  () $_ $_)  True)
  (= (declforeachclause (Cons  $_ $R) $N $ClassName)  
    (writel (:: 'static Code cl' $N ' = new ' $ClassName -- $N () ; (wr nl)))
    (is $M 
      (+ $N 1))
    (declforeachclause $R $M $ClassName))



  (= (declforeachcontinuation  () $_ $_)  True)
  (= (declforeachcontinuation (Cons  (/ $N $A) $R) $N $A)  
    (set-det)
    (declforeachcontinuation $R $N $A))
  (= (declforeachcontinuation (Cons  (/ call 1) $R) $N $A)  
    (set-det)
    (declforeachcontinuation $R $N $A))
  (= (declforeachcontinuation (Cons  (/ call 2) $R) $N $A)  
    (set-det)
    (declforeachcontinuation $R $N $A))
  (= (declforeachcontinuation (Cons  (/ $N $A) $R) $Name $Arity)  
    (writel (:: 'static Code ' $N $A 'cont ;' (wr nl)))
    (declforeachcontinuation $R $Name $Arity))


  (= (declforeachstring  () $_)  True)
  (= (declforeachstring (Cons  $N $R) $M)  
    (is $MM 
      (+ $M 1))
    (writel (:: 'static Const string' $M ' = Const.Intern("' $N ") ; (wr nl)))
    (declforeachstring $R $MM))


  (= (declforeachint  ())  True)
  (= (declforeachint (Cons  $N $R))  
    (writel (:: 'static Int ' (posneg $N) ' = Term.Number(' $N ) ; (wr nl)))
    (declforeachint $R))


  (= (posneg $N)  
    (< $N 0)
    (set-det)
    (is $M 
      (- 0 $N))
    (writel (:: negint $M)))
  (= (posneg $N)  
    (writel (:: posint $N)))



  (= (getnameindex (Cons  $N $_) $N $In $In)  
    (set-det))
  (= (getnameindex (Cons  $_ $R) $N $In $Out)  
    (is $I 
      (+ $In 1))
    (getnameindex $R $N $I $Out)
    (set-det))
  (= (getnameindex  $_ $_ $In $In)  True)


  (= (continuationof (Cons  (= $_  $B) $_) (/ $N $A))  
    (legacy-functor $B $N $A))
  (= (continuationof (Cons  $_ $R) $F)  
    (continuationof $R $F))


  (= (stringof (Cons  $Cl $_) $F)  
    (strings1 $Cl $F))
  (= (stringof (Cons  $_ $R) $F)  
    (stringof $R $F))


  (= (strings1 $X $_)  
    (var $X)
    (set-det)
    (fail))
  (= (strings1 (, $A $_) $F)  
    (strings1 $A $F))
  (= (strings1 (, $_ $B) $F)  
    (set-det)
    (strings1 $B $F))
  (= (strings1 (= $A  $_) $F)  
    (strings1 $A $F))
  (= (strings1 (= $_  $B) $F)  
    (set-det)
    (strings1 $B $F))
  (= (strings1 $T $F)  
    (legacy-functor $T $N $_)
    (= $F $N))
  (= (strings1 $T $F)  
    (=.. $T 
      (Cons  $_ $Args))
    (stringsl $Args $F))


  (= (strings $X $_)  
    (var $X)
    (set-det)
    (fail))
  (= (strings $A $F)  
    (atom $A)
    (set-det)
    (= $F $A))
  (= (strings $A $_)  
    (atomic $A)
    (set-det)
    (fail))
  (= (strings $T $F)  
    (legacy-functor $T $N $_)
    (= $F $N))
  (= (strings $T $F)  
    (=.. $T 
      (Cons  $_ $Args))
    (stringsl $Args $F))


  (= (stringsl (Cons  $T $_) $F)  
    (strings $T $F))
  (= (stringsl (Cons  $_ $R) $F)  
    (stringsl $R $F))


  (= (intof (Cons  $Cl $_) $F)  
    (ints $Cl $F))
  (= (intof (Cons  $_ $R) $F)  
    (intof $R $F))


  (= (ints $X $_)  
    (var $X)
    (set-det)
    (fail))
  (= (ints $A $F)  
    (integer $A)
    (set-det)
    (= $F $A))
  (= (ints $A $_)  
    (atomic $A)
    (set-det)
    (fail))
  (= (ints $T $F)  
    (=.. $T 
      (Cons  $_ $Args))
    (intof $Args $F))





;
; the following are also in someMeTTa at the moment


  (= (specialgoal (< $X $Y) (smallerthan $X $Y))  
    (set-det))
  (= (specialgoal (=< $X $Y) (smallerorequal $X $Y))  
    (set-det))
  (= (specialgoal (> $X $Y) (smallerthan $Y $X))  
    (set-det))
  (= (specialgoal (>= $X $Y) (smallerorequal $Y $X))  
    (set-det))
  (= (specialgoal (=:= $X $Y) (arithequal $Y $X))  
    (set-det))
  (= (specialgoal (= $X $Y) (unify $Y $X))  
    (set-det))
  (= (specialgoal (or $X $Y) (or $X $Y))  
    (set-det))
  (= (specialgoal (@< $X $Y) (termsmallerthan $X $Y))  
    (set-det))
  (= (specialgoal (@> $X $Y) (termgreaterthan $X $Y))  
    (set-det))
  (= (specialgoal (@=< $X $Y) (termsmallerequal $X $Y))  
    (set-det))
  (= (specialgoal (@>= $X $Y) (termgreaterequal $X $Y))  
    (set-det))
  (= (specialgoal (== $X $Y) (termequal $X $Y))  
    (set-det))
  (= (specialgoal (not $X) (not $X))  
    (set-det))


  (= (w-cl (:: $Pred))  
    (set-det)
    (format ~N~p.~n 
      (:: $Pred)))
  (= (w-cl (Cons  $Prev $Pred))  
    (set-det)
    (w-cl $Pred)
    (w-cl $Prev))
  (= (w-cl $Pred)  
    (w-cl (:: $Pred)))


  (= (prelude $Pred)  
    (prelude)
    (write 
/*
)
    (w-cl $Pred)
    (write 
*/
))
  (= (prelude)  
    (write '// Generated code file - by dmiles')
    (nl)
    (fail))
  (= (prelude)  
    (write '// Copyright August 16, 1996,2018 LOGICMOO, KUL and CUM')
    (nl)
    (fail))
  (= (prelude)  
    (write '// Authors: Douglas R. Miles, Bart Demoen and Paul Tarau
import SxxMachine.*;
')
    (nl)
    (nl)
    (fail))
  (= prelude  True)

;
; main :- comp('board') , fail .
;
; main :- comp('test') , fail .
;
; main :- comp('read.pl') , fail .
;
; main :- comp('someMeTTa') , fail .
;
; main :- comp('chat') , fail .
;
; main :- comp('boyer') , fail .


  !(comp sxx-library.pl)
  !(comp tests/animal.pl)
