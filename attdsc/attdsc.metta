;                                           (convert-to-metta-file  attdsc $_363976 attdsc/attdsc.pl attdsc/attdsc.metta)

  !(op 300 xfx <==)
; /******************************************************************/
; /* ATTDSC.PRO Last Modification: Fri Jan 14 19:22:05 1994 */
; /* Bratko's simple algorithm for attributional descriptions. */
; /******************************************************************/
; ; ; Copyright (c) 1990 Ivan Bratko ;
; /******************************************************************/
; /* reimpl. by : Thomas Hoppe */
; /* Mommsenstr. 50 */
; /* D-10629 Berlin */
; /* F.R.G. */
; /* E-Mail: hoppet@cs.tu-berlin.de */
; /* 1991 */
; /* */
; /* reference : Chapter 18, */
; /* Ivan Bratko */
; /* MeTTa */
; /* 2nd extend edition */
; /* Addison-Wesley, 1990 */
; /* */
; /* call : learn(+ClassName) */
; /* */
; /* argument : ClassName = name of the class, whose descrip- */
; /* tion should be learned */
; /* */
; /******************************************************************/
; /* The representation used: */
; /* Attribute = attributes(AttributeName,ListOfPossibleValues) */
; /* Example = example(ClassName,ListOfAttributeValues) */
; /* AttributeValues = Attribute = Value */
; /* */
; /* learn induces class descriptions of the form: */
; /* */
; /* Class <== ListOfAttributeValues */
; /* */
; /* with the meaning: An object is an Class, if it fulfills the */
; /* class description given by ListOfAttributeValues. */
; /* */
; /******************************************************************/
; ; TH Sat May 29 23:45:01 1993 - Made some minor modifications

  !(dynamic (/ <== 2))
; /******************************************************************/
; /* Some M-, C- and YAP-MeTTa dependent declarations. */
; /******************************************************************/


  (= (learn $Class)  
    ( (bagof 
        (example $ClassX $Obj) 
        (example $ClassX $Obj) $Examples) 
      (learn $Examples $Class $Description) nl 
      (write $Class) 
      (write <==) nl 
      (writelist $Description) 
      (add-atom  &self 
        (<==  $Class $Description))))
; /******************************************************************/
; /* */
; /* call : learn(+Class) */
; /* */
; /* arguments : Class = class name to be learned */
; /* */
; /* side effects: asserts classification rules in the database */
; /* */
; /******************************************************************/
; /* 'learn' collects all examples into a list, constructs and out- */
; /* puts a description for Class, and asserts the corresponding */
; /* rule about Class. */
; /******************************************************************/

  (= (learn $Examples $Class Nil)  
    (not (member (example $Class $_) $Examples)))
; /******************************************************************/
; /* */
; /* call : learn(+Examples,+Class,-Description) */
; /* */
; /* arguments : Examples = list of all available examples */
; /* Class = class name to be learned */
; /* Description = induced concept description */
; /* */
; /******************************************************************/
; /* Description covers exactly the positive examples of Class in */
; /* list Examples. If no examples exist for Class, an empty De- */
; /* scription is returned. After a conjunction was learned the */
; /* examples matching the conjunction are removed from Examples */
; /* and for remaining RestExamples a further conjunct is learned. */
; /******************************************************************/
  (= (learn $Examples $Class (Cons  $Conj $Conjs))  
    (learn-conj $Examples $Class $Conj)
    (remove $Examples $Conj $RestExamples)
    (learn $RestExamples $Class $Conjs))
	

  (= (learn-conj $Examples $Class Nil)  
    (not (, (member (example $ClassX $_) $Examples) (not (== $ClassX $Class))))
    (set-det))
; /******************************************************************/
; /* */
; /* call : learn_conj(+Examples,+Class,-Conj) */
; /* */
; /* arguments : Examples = list of all available examples */
; /* Class = class name to be learned */
; /* Conj = list of attribute/value pairs */
; /* */
; /******************************************************************/
; /* Conj is a list of attribute/value pairs are satisfied by some */
; /* examples of class Class and no other classes. If there is no */
; /* other Example of a different class covered, the empty Conj is */
; /* returned. Otherwise, we choose the best attribute/value pair */
; /* according to the evaluation criterion used in 'score' and */
; /* filter out all examples, that cover this attribute/value pair. */
; /******************************************************************/
  (= (learn-conj $Examples $Class (Cons  $Cond $Conds))  
    (choose-cond $Examples $Class $Cond)
    (filter $Examples 
      (:: $Cond) $Examples1)
    (learn-conj $Examples1 $Class $Conds))
	

  (= (choose-cond $Examples $Class $AttVal)  
    (findall 
      (/ $AV $Score) 
      (score $Examples $Class $AV $Score) $AVs)
    (best $AVs $AttVal))
	

  (= (best  ((/  $AttVal $_)) $AttVal)  True)
  (= (best (Cons  (/ $AV0 $S0) (Cons  (/ $AV1 $S1) $AVSlist)) $AttVal)  
    (or 
      (, 
        (> $S1 $S0) 
        (set-det) 
        (best 
          (Cons  
            (/ $AV1 $S1) $AVSlist) $AttVal)) 
      (best 
        (Cons  
          (/ $AV0 $S0) $AVSlist) $AttVal)))
	

  (= (score $Examples $Class $AttVal $Score)  
    (candidate $Examples $Class $AttVal)
    (filter $Examples 
      (:: $AttVal) $Examples1)
    (length $Examples1 $N1)
    (count-pos $Examples1 $Class $NPos1)
    (> $NPos1 0)
    (is $Score 
      (- 
        (* 2 $NPos1) $N1)))
; /******************************************************************/
; /* */
; /* call : score(+Examples,+Class,-AttVal,-Score) */
; /* */
; /* arguments : Examples = list of all available examples */
; /* Class = class name to be learned */
; /* AttVal = chosen attribute/value pair */
; /* Score = value of AttVal */
; /* */
; /******************************************************************/
; /* 'score' determines an suitable attribute/value pair, determines*/
; /* how many examples are covered and computes a value for the */
; /* chosen attribute/value pair. */
; /* Remark: In the current implementation at least one example has */
; /* to be covered. Thus, in some cases it can happen, that rules */
; /* are generated, which cover exactly one example. Hence, we do */
; /* not benefite from learning. This can be changed by requiring */
; /* that at least two examples should be covered. */
; /******************************************************************/
	

  (= (candidate $Examples $Class (= $Att $Val))  
    ( (==  
        (= 
          (attribute  $Att $Values) true) 
        (get-atoms  &self)) 
      (member $Val $Values) 
      (suitable 
        (= $Att $Val) $Examples $Class)))
	

  (= (suitable $AttVal $Examples $Class)  
    (member 
      (example $ClassX $ObjX) $Examples)
    (not (== $ClassX $Class))
    (not (satisfy $ObjX (:: $AttVal)))
    (set-det))
; ; atleast one neg. example must not match AttVal
	

  (= (count-pos  () $_ 0)  True)
; /******************************************************************/
; /* */
; /* call : count_pos(+Examples,+Class,-N) */
; /* */
; /* arguments : Examples = list of all available examples */
; /* Class = class name to be learned */
; /* N = number of covered examples */
; /* */
; /******************************************************************/
; /* N is the number of positive examples of Class */
; /******************************************************************/
  (= (count-pos (Cons  (example $ClassX $_) $Examples) $Class $N)  
    (count-pos $Examples $Class $N1)
    (or 
      (, 
        (= $ClassX $Class) 
        (set-det) 
        (is $N 
          (+ $N1 1))) 
      (= $N $N1)))
	

  (= (filter $Examples $Cond $Examples1)  
    (findall 
      (example $Class $Obj) 
      (, 
        (member 
          (example $Class $Obj) $Examples) 
        (satisfy $Obj $Cond)) $Examples1))
; /******************************************************************/
; /* */
; /* call : filter(+Examples,+Cond,-Examples1) */
; /* */
; /* arguments : Examples = list of all available examples */
; /* Cond = attribute/value pair */
; /* Examples1 = list of examples with attr/val */
; /* */
; /******************************************************************/
; /* Examples1 contains elements of Examples that satisfy Condition */
; /******************************************************************/
		

  (= (remove  () $_ ())  True)
; /******************************************************************/
; /* */
; /* call : remove(+Examples,+Conj,-RestExamples) */
; /* */
; /* arguments : Examples = list of all available examples */
; /* Conj = list of attribute/value pair */
; /* RestExamples = list of examples not matching */
; /* Conj */
; /* */
; /******************************************************************/
; /* removing from Examples those examples that match Conj gives */
; /* RestExamples . */
; /******************************************************************/
  (= (remove (Cons  (example $Class $Obj) $Es) $Conj $Es1)  
    (satisfy $Obj $Conj)
    (set-det)
    (remove $Es $Conj $Es1))
  (= (remove (Cons  $E $Es) $Conj (Cons  $E $Es1))  
    (remove $Es $Conj $Es1))
	

  (= (satisfy $Object $Conj)  
    (not (, (member (= $Att $Val) $Conj) (member (= $Att $ValX) $Object) (not (== $ValX $Val)))))
	    

  (= (match $Object $Description)  
    (member $Conj $Description)
    (satisfy $Object $Conj))
	

  (= (writelist  ())  True)
  (= (writelist (Cons  $X $L))  
    ( (tab 2) 
      (write $X) nl 
      (writelist $L)))


  (= help  
    ( (write 'Load data set and start learning with command: [Filename].') nl))


  !(help *)

