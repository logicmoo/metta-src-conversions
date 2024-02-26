;                                           (convert-to-metta-file  sxx_compiler_gen_static $_135806 sxx_machine/sxx_compiler_gen_static.pl sxx_machine/sxx_compiler_gen_static.metta)

  (= (append  () $L $L)  True)
  (= (append (Cons  $X $L1) $L2 (Cons  $X $L3))  
    (append $L1 $L2 $L3))


  (= (legacy-functor $P . $A)  
    (functor $P Cons $A)
    (set-det))
  (= (legacy-functor $P $F $A)  
    (functor $P $F $A)
    (set-det))


  !(dynamic (/ system-predicate 1))
  !(multifile (/ system-predicate 1))

;
; system_predicate('$builtin_member'(_,_)).

  (= (system-predicate  (%erase  $_))  True)
  (= (system-predicate  (%fast-write  $_))  True)
  (= (system-predicate  (%fast-write  $_ $_))  True)
  (= (system-predicate  (%call  $_ $_))  True)
  (= (system-predicate  (mutex-lock-bt  $_))  True)
  (= (system-predicate  (%set-exception  $_))  True)
  (= (system-predicate  (%get-exception  $_))  True)
  (= (system-predicate  (%get-current-B  $_))  True)
  (= (system-predicate  (%compare0  $_ $_ $_))  True)
  (= (system-predicate  (%compiled-predicate  $_ $_ $_))  True)
  (= (system-predicate  (%compiled-predicate-or-builtin  $_ $_ $_))  True)
  (= (system-predicate  (%hash-remove-first  $_ $_ $_))  True)
  (= (system-predicate  (%hash-adda  $_ $_ $_))  True)
  (= (system-predicate  (%hash-addz  $_ $_ $_))  True)
  (= (system-predicate  (%read-token0  $_ $_ $_))  True)
  (= (system-predicate  (%symbol-type0  $_ $_))  True)
  (= (system-predicate  (%begin-sync  $_ $_))  True)



  !(consult sxx-system)


  (= (comp $FileSpec)  
    (comp-to $FileSpec ../jsrc/bootlib))
    

  (= (comp-to $FileSpec $Where)  
    (not (exists-file $FileSpec))
    (set-det)
    (forall 
      (, 
        (absolute-file-name $FileSpec $File 
          (:: 
            (expand True) 
            (file-type prolog) 
            (solutions all))) 
        (exists-file $File)) 
      (comp-to $File $Where)))
  (= (comp-to $File $Dir0)  
    (must-det-l (, (makename (:: $Dir0 / SxxMachine) $Dir) (set-det) (exists-file $File) (file-base-name $File $Base) (file-name-extension $Stem $_ $Base) (makename (:: $Dir / $Stem .java) $FileName) (set-det) (wdmsg (comp-to $File $Dir $FileName)) (make-directory-path $Dir) (set-det) (nb-setval stem $Stem) (= $InProg Nil) (see $File) (readprogram $Stem $InProg $Prog) (set-det) seen (tell $FileName) (gencode-sss $Dir $Stem $Prog) (set-det) told))
    (set-det))
; ; close(File) ,
     



  (= (call-local-side-efs (public $_))  
    (set-det))
  (= (call-local-side-efs (package $V))  
    (nb-setval package $V))
  (= (call-local-side-efs (if $_))  
    (set-det))
  (= (call-local-side-efs (determinate $_))  
    (set-det))
  (= (call-local-side-efs (comp $_))  
    (set-det))
  (= (call-local-side-efs (comp-to $_ $_))  
    (set-det))
  (= (call-local-side-efs $Call)  
    (ignore (call $Call)))


  (= (local-side-efs $_ !(if $_))  
    (repeat 
      (read $Until) 
      (== $Until 
        !endif) 
      (set-det)))
  (= (local-side-efs $_ !$Call)  
    (call-local-side-efs $Call)
    (set-det))
  (= (local-side-efs $_ $H)  
    (\= $H 
      (= $_  $_))
    (set-det)
    (local-side-efs $_ 
      (= $H  True)))
;
; local_side_efs(_, (H:-_)):- ( predicate_property(H,static) ; predicate_property(H,built_in) ),!.
;
; local_side_efs(_, (Call)):- call(assertz(Call)),!.
  (= (local-side-efs  $_ $Clause)  True)



  (= (readprogram $Stem $In $Out)  
    (read $Clause)
    (local-side-efs $Stem $Clause)
    (det-if-then-else 
      (= $Clause end-of-file) 
      (= $In $Out) 
      (, 
        (binarize $Clause $BinCl) 
        (addclausetoprogram $BinCl $In $NewIn) 
        (readprogram $Stem $NewIn $Out))))

;
; binarize((Head ::- Body),Cl) :- ! , Cl = (Head :- Body) .


  (= (binarize !$C $Out)  
    (nb-current stem $Stem)
    (symbol-concat on-load- $Stem $Pred)
    (set-det)
    (binarize 
      (= $Pred  
        (write $C)) $Out))
  (= (binarize !$C $Out)  
    (nb-current stem $Stem)
    (symbol-concat on-load- $Stem $Pred)
    (set-det)
    (binarize 
      (= $Pred  $C) $Out))
  (= (binarize (= $Head  $Body) (= $BinHead  $BinBody))  
    (set-det)
    (addcont $Head $Continuation $BinHead)
    (makebinbody $Body $Continuation $BinBody))
  (= (binarize $Head (= $BinHead  
    (call $Continuation)))  
    (addcont $Head $Continuation $BinHead))


  (= (makebinbody $G $C $NewBody)  
    (var $G)
    (set-det)
    (makebinbody 
      (call $G) $C $NewBody))

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


  (= (wr (getval $Var))  
    (must (, (nb-current $Var $Value) (write $Value))))
  (= (wr (wr nl))  
    ( (set-det) nl))
  (= (wr $Atom)  
    (atomic $Atom)
    (set-det)
    (write $Atom))
  (= (wr $VAR)  
    (legacy-functor $VAR %VAR 2)
    (set-det)
    (write $VAR))
  (= (wr $Goal)  
    (must (call $Goal))
    (set-det))


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



  (= (gencode-sss $Dir $Stem $Preds)  
    (must-det-l (, (mysetof $F (^ $Preds (stringof $Preds $F)) $AllStrings) (mysetof $F (^ $Preds (intof $Preds $F)) $AllInts) (= $Strings (Cons  cut $AllStrings)) (writel (:: 'package SxxMachine; 
import SxxMachine.*;
import SxxMachine.Builtins.*;
import SxxMachine.Metaterm.*; 
import SxxMachine.sxx-meta.pred-or-2;
import SxxMachine.sxx-meta.*;
import SxxMachine.sxx-system.*;
import SxxMachine.sxx-read.*;
import SxxMachine.sxx-library.*;

public class ' $Stem ' /*extends CodeFile*/ {
')) (declforeachstring $Strings 0) (declforeachint $AllInts) (declforeachpred $Preds) (gencode-sss $Strings $Dir $Stem $Preds) (set-det) (writel (:: }


)))))
  
  (= (gencode-sss  $_ $_ $_ ())  True)
  (= (gencode-sss $Strings $Dir $Stem (Cons  $Pred $Preds))  
    (gencodeforpred $Strings $Dir $Stem $Pred)
    (gencode-sss $Strings $Dir $Stem $Preds))


  (= (gencodeforpred $Strings $Dir $Stem $Pred)  
    ( (= $Pred 
        (:: (= $H  $B))) 
      (== $H $B) 
      (add-atom  &self 
        (system-predicate  $H)) 
      (set-det) 
      (format '~N/* System pred ~q */~n' 
        (:: $H))))

  (= (gencodeforpred $Strings $Dir $Stem $Pred)  
    (= $Pred 
      (Cons  
        (= $H  $_) $_))
    (legacy-functor $H $PN $A)
    (is $AA 
      (- $A 1))
    (make-classname $PN $AA $N $ClassName)
    (nb-setval classname $ClassName)
    (gencodeforpred $Strings $Dir $Stem $Pred $H $N $A $AA $ClassName))


   ;
; (isSort(X),isSort(Y))-> (all Q hasSort(Q,X)



  (= (symbol-to-name $Atom $Out)  
    (symbol-to-name1 $Atom $Out)
    (set-det))
  (= (symbol-to-name  $Name $Name)  True)


  (= (symbol-to-name1 or or)  
    (set-det))
; /* symbol_to_name1((':-'),'system_neck'). symbol_to_name1(('=..'),'system_univ'). symbol_to_name1(('=@='),'system_variant'). symbol_to_name1(('\\+'),'system_not'). symbol_to_name1(('\\='),'system_notequals'). symbol_to_name1(('='),'system_equals'). symbol_to_name1(('=='),'strict_eq'). symbol_to_name1(('\\=='),'strict_noteq'). symbol_to_name1(('-->'),'dcg_arrow'). symbol_to_name1(('->'),'if_then_arrow'). symbol_to_name1(('*->'),'with_each_arrow'). symbol_to_name1((','),'system_comma'). symbol_to_name1(('^'),'system_carrot'). symbol_to_name1(('|'),'system_bar'). symbol_to_name1((':'),'system_colon'). */
  (= (symbol-to-name1 : module-colon)  
    (set-det))
  (= (symbol-to-name1 $S $O)  
    (functor $P $S 2)
    (specialgoal $P $PP)
    (functor $PP $O $_)
    (set-det))
  (= (symbol-to-name1 $S $O)  
    (functor $P $S 1)
    (specialgoal $P $PP)
    (functor $PP $O $_)
    (set-det))
  (= (symbol-to-name1 $A $B)  
    (symbol-to-name2 $A $B)
    (set-det))


  (= (symbol-to-name2 '' '')  
    (set-det))

  (= (symbol-to-name2 $Atom $Out)  
    (name $Atom 
      (Cons  36 
        (Cons  48 
          (Cons  48 
            (Cons  $A 
              (Cons  $B $Rest))))))
    (name $Mid1 
      (:: 36 48 48 $A $B))
    (set-det)
    (name $NAtom $Rest)
    (symbol-to-name2 $NAtom $Mid2)
    (symbol-concat $Mid1 $Mid2 $Out))

  (= (symbol-to-name2 $Atom $Out)  
    (name $Atom 
      (Cons  $C $Rest))
    (not (code-type $C alnum))
    (\== $C 95)
    (set-det)
    (format 
      (atom $Mid1) %~|~`0t~16R~4+ 
      (:: $C))
    (name $NAtom $Rest)
    (symbol-to-name2 $NAtom $Mid2)
    (symbol-concat $Mid1 $Mid2 $Out))

  (= (symbol-to-name2 $Atom $Out)  
    (name $Atom 
      (Cons  $C $Rest))
    (name $Mid1 
      (:: $C))
    (name $NAtom $Rest)
    (symbol-to-name2 $NAtom $Mid2)
    (symbol-concat $Mid1 $Mid2 $Out))
   


  (= (make-classname $Sym $AA $NN $ClassName)  
    (det-if-then 
      (symbol-to-name $Sym $N) 
      (, 
        (\== $Sym $N) 
        (set-det) 
        (make-classname $N $AA $NN $ClassName))))
  (= (make-classname $N $AA $N $ClassName)  
    (makename 
      (:: pred- $N - $AA) $ClassName)
    (set-det))


  (= (gencodeforpred $Strings $Dir $PINF $Pred $H $N $A $AA $ClassName)  
    (must-det-l (, (writel (:: 'public static class ' $ClassName ' extends Code {
')) (prelude $Pred) (writel (:: (wr nl) 'public Operation Exec(Prolog mach) {return exec-static(mach); }' (wr nl) 'public static Operation exec-static(Prolog mach){ ' (wr nl) '/* Term aregs[] = {' (aregarray $AA) } ;*/ (wr nl) 'Term aregs[] = mach.RegPull(' $AA ); (wr nl) 'mach.CreateChoicePoint(aregs) ;' (wr nl) 'return (Operation)exec-' $ClassName '--1(mach); }' (wr nl))) (set-det) (genpredcode $Strings $Pred last $ClassName $_ $Strings) (writel (:: }


)))))
; ;mysetof(F,Pred^continuationof(Pred,F),Conts) ,
; ; declforeachcontinuation(Conts,N,A),
; ;declforeachclause(Pred,1,ClassName),
; ; writel(['static {', initforeachcontinuation(Conts,N,A),'}',wr(nl)]),
			
                        


  (= (cont-ref $N)  
    (must-det (writel (:: (getval classname) ::exec- (getval classname) -- $N))))


  (= (mysetof $X $Y $Z)  
    (setof $X $Y $Z)
    (set-det))
  (= (mysetof  $_ $_ ())  True)


  (= (genpredcode  $PreDecl () $_ $_ 0 $_)  True)
  (= (genpredcode $Strings (Cons  $Clause $Rest) $Last $ClassName $M $Strings)  
    (genpredcode $Strings $Rest notlast $ClassName $N $Strings)
    (is $M 
      (+ $N 1))
    (genclausecode $Strings $Clause $Last $ClassName $M $Strings))


  (= (genclausecode $PreDecl $Clause $Last $ClassName $N $Strings)  
    (= $Clause 
      (= $Head  $Body))
    (set-det)
    (must-det-l (, (legacy-functor $Head $Name0 $Arity) (symbol-to-name $Name0 $Name) (is $Amin2 (- $Arity 2)) (is $Amin1 (- $Arity 1)) (arg $Arity $Head (%cont $Arity)) (writel (:: 'public static Operation exec-' $ClassName -- $N '(Prolog mach){ ' (det-if-then-else (= $Last last) (write 'mach.RemoveChoice() ;') (, (is $M (+ $N 1)) (writel (:: mach.FillAlternative( (cont-ref $M) ) ;)))) (wr nl) 'Term local-aregs[] = mach.Areg ;' (wr nl) 'Term continuation = local-aregs[' $Amin1 ] ; (wr nl) (decl-deref-args $Amin2) (mynumbervars $Clause 1 $NumbVars) (det-if-then-else (> $NumbVars 1) (declvars $NumbVars) True) (gets $Head $Strings) (puts $Body $Strings) 'mach.CUTB = mach.CurrentChoice ;' (wr nl) (nullifyaregs $Clause) 'return ' (bodycont $Strings $Clause $Name $Arity)  ;} (wr nl))))))



  (= (nullifyaregs (= $H  $B))  
    (legacy-functor $H $_ $HA)
    (legacy-functor $B $_ $BA)
    (is $Diff 
      (- $HA $BA))
    (> $Diff 0)
    (nullify $Diff $HA))
  (= (nullifyaregs  $_)  True)


  (= (nullify 0 $_)  
    ( (set-det) 
      (write 'null ;') nl fail))
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
      (%VAR $I $_))
    (is $O 
      (+ $I 1)))
  (= (mynumbervars (%VAR $_ $_) $I $O)  
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
    (writel (:: 'if (!( (areg' $N ).Unify( (constructterm $Arg $Strings) ',mach))) return Prolog.Fail0 ;' (wr nl))))



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
    (writel (:: 'Var var' $M ' = Data.V(mach) ;' (wr nl)))
    (declvars $M))



;
; variables are replaced with '$VAR'(integer,_)
;
; continuation is replaced by '$cont'(arity)


  (= (constructterm (%cont $_) $_)  
    (set-det)
    (writel (:: continuation)))
  (= (constructterm (%VAR $N $T) $_)  
    (set-det)
    (det-if-then-else 
      (var $T) 
      (, 
        (writel (:: var $N)) 
        (= $T 1)) 
      (writel (:: var $N .Deref()))))

;
; constructterm(cut(_,C),Strings) :- ! , writel(['Data.F("cut",new HeapChoice(mach.CUTB),',constructterm(C,Strings),')']) .
  (= (constructterm (cut $_ $C) $Strings)  
    (set-det)
    (writel (:: mach.HC( (constructterm $C $Strings) ))))

  (= (constructterm $Int $_)  
    (integer $Int)
    (set-det)
    (writel (:: (getval stem) .))
    (posneg $Int))
  (= (constructterm $Atom $Strings)  
    (string $Atom)
    (set-det)
    (writeConst $Atom $Strings))
  (= (constructterm $Int $_)  
    (float $Int)
    (set-det)
    (writel (:: Data.Float( $Int )))
    (set-det))

  (= (constructterm $Atom $Strings)  
    (== Nil $Atom)
    (set-det)
    (writeConst $Atom $Strings))
  (= (constructterm $Atom $Strings)  
    (atom $Atom)
    (set-det)
    (writeConst $Atom $Strings))
  (= (constructterm $Term $Strings)  
    (compound $Term)
    (set-det)
    (must-det-l (, (legacy-functor $Term $Name $_) (=.. $Term (Cons  $_ $Args)) (writel (:: Data.F( (writeConst $Name $Strings) , (newargs $Args 1 $Strings) ))))))


  (= (writeConst Nil $Strings)  
    (writel (:: Const.Nil))
    (set-det))
  (= (writeConst $Name $Strings)  
    (det-if-then 
      (, 
        (not (string $Name)) 
        (symbol-to-name $Name $Other)) 
      (, 
        (== $Name $Other) 
        (set-det) 
        (writel (:: (getval stem) .s- $Name)) 
        (set-det))))
  (= (writeConst $Name $Strings)  
    (getnameindex $Strings $Name 0 $I)
    (set-det)
    (writel (:: (getval stem) .s $I))
    (set-det))
  (= (writeConst $Name $Strings)  
    (must-det-l (, (symbol-string $Name $S) (writel (:: Const.Intern( (format ~q (:: $S)) )))))
    (set-det))



  (= (newargs  () $_ $_)  True)
  (= (newargs (Cons  $A $Args) $N $Strings)  
    (must-det-l (, (det-if-then-else (> $N 1) (write ,) True) (is $M (+ $N 1)) (constructterm $A $Strings) (newargs $Args $M $Strings))))
			




  (= (bodycont $String (= $_  $B) $Name $Arity)  
    (fail 
      (legacy-functor $B $Name $Arity) 
      (set-det) 
      (writel (:: entry-code))))
  (= (bodycont $String (= $_  $B) $_ $_)  
    (legacy-functor $B call 1)
    (set-det)
    (writel (:: Prolog.Call1)))
  (= (bodycont $String (= $_  $B) $_ $_)  
    (legacy-functor $B call 2)
    (set-det)
    (writel (:: Prolog.Call2)))
  (= (bodycont $String (= $_  $B) $_ $_)  
    (legacy-functor $B cut 2)
    (set-det)
    (writel (:: Prolog.Call1)))
;
; bodycont(_String,(_ :- B),_,_) :- legacy_functor(B,cut,2) , ! , writel(['MeTTa.Cut2']) .

  (= (bodycont $String (= $_  $B) $_ $_)  
    (fail 
      (legacy-functor $B $Name $Arity) 
      (getnameindex $String $Name 0 $I) 
      (set-det) 
      (is $MArity 
        (- $Arity 1)) 
      (writel (:: '(Operation) ' (getval stem) .s $I .FindProc( $MArity )))))


  (= (bodycont $String (= $_  $B) $_ $_)  
    (legacy-functor $B $Name $Arity)
    (is $MArity 
      (- $Arity 1))
    (functor $P $Name $MArity)
    (system-predicate $P)
    (set-det)
    (writel (:: '(Operation) ' (writeConst $Name $String) .FindProc( $MArity ))))

  (= (bodycont $String (= $_  $B) $_ $_)  
    (legacy-functor $B $Name0 $Arity)
    (is $MArity 
      (- $Arity 1))
    (symbol-to-name $Name0 $Name)
    (writel (:: (Operation)pred- $Name - $MArity ::exec-static)))


  (= (bodycont $String (= $_  $B) $_ $_)  
    (legacy-functor $B $Name0 $Arity)
    (symbol-to-name $Name0 $Name)
    (writel (:: $Name $Arity cont)))


  (= (decl-deref-args -1)  
    (set-det))
  (= (decl-deref-args $N)  
    (writel (:: 'Term areg' $N ' = local-aregs[' $N '].Deref() ;' (wr nl)))
    (is $M 
      (- $N 1))
    (decl-deref-args $M))


  (= (use-entry-code  $F $A)  
    (empty))

  (= (use-static-call  $F $A)  True)

  (= (use-load-pred  $F $A)  True)


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
    (symbol-to-name $N $SN)
    (writel (:: $N $A 'cont = pred-' $N - $B ::exec-pred- $SN -- $B or (wr nl)))
    (initforeachcontinuation $R $Name $Arity))
  (= (initforeachcontinuation (Cons  (/ $N $A) $R) $Name $Arity)  
    (use-load-pred $N $B)
    (set-det)
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
    (writel (:: 'static Operation cl' $N  =  $ClassName ::exec- $ClassName -- $N or (wr nl)))
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
    (writel (:: 'static Operation ' $N $A 'cont ;' (wr nl)))
    (declforeachcontinuation $R $Name $Arity))


  (= (varnamestr $_ $N)  
    (det-if-then 
      (, 
        (not (string $N)) 
        (symbol-to-name $N $O)) 
      (, 
        (== $N $O) 
        (set-det) 
        (writel (:: s- $N)) 
        (set-det))))
  (= (varnamestr $M $_)  
    (writel (:: s $M))
    (set-det))


  (= (declforeachstring  () $_)  True)
  (= (declforeachstring (Cons  $N $R) $M)  
    (is $MM 
      (+ $M 1))
    (declfor1string $N $M)
    (declforeachstring $R $MM))


  (= (declfor1string cut $M)  
    (> $M 0)
    (set-det))
  (= (declfor1string $N $M)  
    (symbol-string $N $S)
    (writel (:: 'final static Const ' (varnamestr $M $N) ' = Data.Intern(' (writeq $S) ) ; (wr nl))))
				


  (= (constructnum $Int)  
    (integer $Int)
    (is $AInt 
      (abs $Int))
    (is $X 
      (^ 2 30))
    (> $AInt $X)
    (set-det)
    (writel (:: 'Data.BigInt("' $Int "))))
  (= (constructnum $Int)  
    (integer $Int)
    (set-det)
    (writel (:: Data.Number( $Int L))))
  (= (constructnum $Int)  
    (float $Int)
    (writel (:: 'Data.BigDec("' $Int "))))


  (= (declforeachint  ())  True)
  (= (declforeachint (Cons  $N $R))  
    (writel (:: 'final static Int ' (posneg $N)  =  (constructnum $N)  ; (wr nl)))
    (declforeachint $R))


  (= (posneg $N)  
    (< $N 0)
    (set-det)
    (is $M 
      (- 0 $N))
    (writel (:: negint $M)))
  (= (posneg $N)  
    (writel (:: posint $N)))


  (= (declforeachpred  ())  True)
  (= (declforeachpred (Cons  $P $R))  
    (set-det)
    (declforeachpred1 $P)
    (declforeachpred $R))


  (= (declforeachpred1 (:: (= $P  $A)))  
    (== $P $A))
  (= (declforeachpred1 (Cons  (= $P  $_) $_))  
    (functor $P $F $A)
    (set-det)
    (symbol-to-name $F $Name)
    (is $AA 
      (- $A 1))
    (symbol-string $F $Str)
    (writel (:: 'final static Operation reg-' $Name - $AA ' = PredTable.Register(' (writeq $Str) , $AA ', new pred-' $Name - $AA ()); (wr nl)))
    (set-det))
	           
  (= (declforeachpred1  $_)  True)


  (= (getnameindex (Cons  $N $_) $N $In $In)  
    (set-det))
  (= (getnameindex (Cons  $_ $R) $N $In $Out)  
    (is $I 
      (+ $In 1))
    (getnameindex $R $N $I $Out)
    (set-det))
;
; getnameindex(_,_,In,In).



  (= (continuationof (Cons  (= $_  $B) $_) (/ $N $A))  
    (legacy-functor $B $N $A))
  (= (continuationof (Cons  $_ $R) $F)  
    (continuationof $R $F))


  (= (stringof (Cons  $Cl $_) $F)  
    (strings1 $Cl $F))
  (= (stringof (Cons  $_ $R) $F)  
    (stringof $R $F))


  (= (strings1 $X $_)  
    ( (var $X) 
      (set-det) fail))
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
    ( (var $X) 
      (set-det) fail))
  (= (strings $A $F)  
    (atom $A)
    (set-det)
    (= $F $A))
  (= (strings $A $_)  
    ( (atomic $A) 
      (set-det) fail))
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
    ( (var $X) 
      (set-det) fail))
  (= (ints $A $F)  
    (integer $A)
    (set-det)
    (= $F $A))
  (= (ints $A $_)  
    ( (atomic $A) 
      (set-det) fail))
  (= (ints $T $F)  
    (=.. $T 
      (Cons  $_ $Args))
    (intof $Args $F))



  (= (predof (Cons  $Cl $_) $F)  
    (preds $Cl $F))
  (= (predof (Cons  $_ $R) $F)  
    (predof $R $F))


  (= (preds $X $_)  
    ( (var $X) 
      (set-det) fail))
  (= (preds $P (/ $F $A))  
    (functor $P $F $A)
    (set-det))




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
    (write 
/*

)
    (w-cl $Pred)
    (write 

*/
))
  (= prelude  
    ( (write '// Generated code file - by dmiles') nl fail))
  (= prelude  
    ( (write '// Copyright August 16, 1996,2018 LOGICMOO, KUL and CUM') nl fail))
  (= prelude  
    ( (write '// Authors: Douglas R. Miles, Bart Demoen and Paul Tarau') nl nl fail))
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


  !(comp-to sxx-system ../jsrc/bootlib)
  !(comp-to sxx-read ../jsrc/bootlib)
  !(comp-to sxx-library ../jsrc/bootlib)
  !(comp-to sxx-meta ../jsrc/bootlib)
;
; :- comp_to(sxx_compiler_gen_static,'../jsrc/compiler').
  !(comp-to sxx-builtins-cafe ../jsrc/library)
  !(comp-to tests/* ../jsrc/testing)
  !(comp-to bench/* ../jsrc/benches)


