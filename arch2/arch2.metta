;                                           (convert-to-metta-file  arch2 $_488052 arch2/arch2.pl arch2/arch2.metta)

  (= (learn $Concept)  
    (findbag $X 
      (example $X) $L)
    (learn $L $Concept))
; /******************************************************************/
; /* ARCH2.PRO Last Modification: Fri Jan 14 19:21:09 1994 */
; /* Winston's incremental learning procedure. */
; /******************************************************************/
; ; ; Copyright (c) 1990 Ivan Bratko ;
; /******************************************************************/
; /* reimpl. by : Thomas Hoppe */
; /* Mommsenstr. 50 */
; /* D-10629 Berlin */
; /* F.R.G. */
; /* E-Mail: hoppet@cs.tu-berlin.de */
; /* 1990 */
; /* */
; /* reference : Chapter 18, */
; /* Ivan Bratko */
; /* MeTTa */
; /* 2nd extend edition */
; /* Addison-Wesley, 1990 */
; /* */
; /* call : learn(-Concept) */
; /* */
; /* argument : Concept = learned concept description */
; /* */
; /******************************************************************/
; /* This is a strong restricted version of Winston's incremental */
; /* learning procedure for structural descriptions. */
; /* The following restrictions are known: */
; /* - The first example must be positive. */
; /* - The implementation can handle only up to six objects. */
; /* - The list containing missing or extra descriptors of a concept*/
; /* can contain only 3 descriptors. */
; /* */
; /* The representation used: */
; /* Object = object(ListOfParts,ListOfRelations) */
; /* Concept = concept(ListOfParts,MustRels,Rels,MustNotRels) */
; /* Positive example = + Object */
; /* Negative example = - Object */
; /* */
; /* Parts in an objects are denoted by MeTTa variables, in a */
; /* description they are denoted by constants (i.e. part1, ...) */
; /* */
; /* learn induces a structural concept description from a list of */
; /* examples. */
; /******************************************************************/
; ; TH Sat May 29 23:41:21 1993 - made some minor modifications

  (= (learn (Cons  $FirstExample $Examples) $ConceptDesc)  
    (initialize $FirstExample $InitialHypothesis)
    (process-examples $InitialHypothesis $Examples $ConceptDesc))
	

  (= (initialize (+ (object $Parts $Rels)) (concept $Parts Nil $Rels Nil))  
    (namevars $Parts 
      (:: part1 part2 part3 part4 part5 part6)))
; ; Turn variables in a description into constants. Atmost the concept ; can contain 6 objects.
	

  (= (namevars $List $NameList)  
    (append $List $_ $NameList))
; /******************************************************************/
; /* */
; /* call : namevars(+VarList,+NameList) */
; /* */
; /* argument : VarList = list of variables */
; /* NameList = list of constants */
; /* */
; /******************************************************************/
; /* namevars instantiates the variables in VarList with the */
; /* constants in NameList. */
; /******************************************************************/


  (= (process-examples  $ConceptDesc () $ConceptDesc)  True)
  (= (process-examples $CurDesc (Cons  $Example $Examples) $FinDesc)  
    (object-type $Example $Object $Type)
    (match $Object $CurDesc $Difference)
    (update $Type $Difference $CurDesc $NewDesc)
    (process-examples $NewDesc $Examples $FinDesc))
	

  (= (object-type  (+  $Object) $Object positive)  True)
  (= (object-type  (-  $Object) $Object negative)  True)


  (= (match (object $OParts $ORels) (concept $CParts $Musts $Rels $MustNots) (+ $Missing $Extras))  
    (list-diff $ORels $Musts 
      (+ Nil $RestRels))
    (short-lists (+ $Missing $Extras))
    (list-diff $OParts $CParts 
      (+ Nil Nil))
    (list-diff $RestRels $Rels 
      (+ $Missing $Extras))
    (list-diff $Extras $MustNots 
      (+ $MustNots $_)))
; /******************************************************************/
; /* */
; /* call : match(+ObjectDesc,+ConceptDesc,-Difference) */
; /* */
; /* argument : ObjectDesc = description of an example */
; /* ConceptDesc = current concept description */
; /* Difference = term of the form: Missing + Extra */
; /* */
; /******************************************************************/
; /* match matches the description of an example against the current*/
; /* concept description and determines two lists of Missing and */
; /* Extra descriptors. These are returned and form the basis for */
; /* updates of the current concept description. */
; /* Matching proceeds in the following order, first all: */
; /* - must matches are performed, then */
; /* - a difference template is generated */
; /* - parts of the object and concept descriptions are matched */
; /* - other relations are matched and */
; /* - it is checked whether all MustNots are missing. */
; /* On backtracking a different template is tried. */
; /******************************************************************/
	

  (= (list-diff  $List () (+  () $List))  True)
; /******************************************************************/
; /* */
; /* call : list_diff(+List1,+List2,-ListDiffs) */
; /* */
; /* argument : List1 = list of descriptors */
; /* List2 = list of descriptors */
; /* ListDiffs = List2\List1 + List1\List2 */
; /* */
; /******************************************************************/
  (= (list-diff $List1 (Cons  $X $List2) (+ $Miss $Extras))  
    (delete $List1 $List11 $X $Miss11 $Miss)
    (list-diff $List11 $List2 
      (+ $Miss11 $Extras)))
	

  (= (delete  () () $X $Dels ([|]  $X $Dels))  True)
; /******************************************************************/
; /* */
; /* call : delete(+List1,+List2,+Descriptor,-List3,-List4) */
; /* */
; /* argument : List1 = list of descriptors */
; /* List2 = list of descriptors possibly without */
; /* Descriptor */
; /* Descriptor= Descriptor which should be deleted */
; /* List3 = list of descriptors with Descriptor */
; /* deleted */
; /* List4 = list of descriptors possibly with */
; /* Descriptor */
; /* */
; /******************************************************************/
; /* If Descriptor is deleted from List1 then List4 = List1, */
; /* if not the List2 = List1 and List4 = [Descriptor|List3]. */
; /* (If Descriptor is not deleted then it is missing in List.) */
; /******************************************************************/  
  (= (delete (Cons  $Y $L) $L $X $Dels $Dels)  
    (== $X $Y)
    (set-det))
  (= (delete (Cons  $Y $L) $L $X $Dels $Dels)  
    (= $X $Y))
  (= (delete (Cons  $Y $L) (Cons  $Y $L1) $X $Dels $Dels1)  
    (delete $L $L1 $X $Dels $Dels1))
	

  (= (short-lists (+ $L1 $L2))  
    (append $L $_ 
      (:: $_ $_ $_))
    (append $L1 $L2 $L))
; /******************************************************************/
; /* */
; /* call : short_lists(List1 + List2) */
; /* */
; /* argument : List1 = list of descriptors */
; /* List2 = list of descriptors */
; /* */
; /******************************************************************/
; /* short_lists generates difference templates of the form: */
; /* List1 + List2; short lists are generated first to force finding*/
; /* good (in the sense of short) matches, before more complex */
; /* are tried. Templates are generated in the order: */
; /* [] + [], [] + [_], [_] + [], [] + [_,_], [_] + [_] .... */
; /* Each list can contain atmost 3 elements. */
; /******************************************************************/


  (= (update  negative (+  $_ ($ExtraRelation)) (concept  $Parts $Musts $Rels $MustNots) (concept  $Parts $Musts $Rels ([|]  $ExtraRelation $MustNots)))  True)
; /******************************************************************/
; /* */
; /* call : update(+TypeOfExample,+Difference,+CurrentDesc, */
; /* -NewDesc) */
; /* */
; /* argument : TypeOfExample = classification of the example */
; /* Difference = determined difference */
; /* CurrentDesc = current concept description */
; /* NewDesc = modified concept description */
; /* */
; /******************************************************************/
; /* updates modifies the current concept description in */
; /* correspondance to the determined difference. */
; /* The clauses make the following: */
; /* Clause 1: Forbid-relation rule: an extra relation in a negative*/
; /* example must be forbidden in */
; /* the concept description. */
; /* Clause 2: Require-relation rule: missing relations in a */
; /* negative example must be */
; /* required in the concept */
; /* description. */
; /* Clause 3: One missing and one extra relation in a negative */
; /* Can be handled separatly. */
; /* Clause 4: Climb-taxonomy rule: generalize an isa-relation by */
; /* climbing a-kind-of taxonomy. */
; /* The ako-taxonomy represents the */
; /* background knowledge of the */
; /* system. */
; /******************************************************************/
  (= (update negative (+ $Missing $_) (concept $Parts $Musts $Rels $MustNots) (concept $Parts $NewMusts $NewRels $MustNots))  
    (= $Missing 
      (Cons  $_ $_))
    (append $Missing $Musts $NewMusts)
    (list-diff $Rels $Missing 
      (+ $_ $NewRels)))
  (= (update negative (+ (:: $MissR) (:: $ExtraR)) $CurDesc $NewDesc)  
    (update negative 
      (+ Nil 
        (:: $ExtraR)) $CurDesc $InterDesc)
    (update negative 
      (+ 
        (:: $MissR) Nil) $InterDesc $FinDesc))
  (= (update positive (+ (:: (isa $Object $Class1)) (:: (isa $Object $Class2))) (concept $Parts $Musts $Rels $MustNots) (concept $Parts $Musts $NewRels $MustNots))  
    (climb $Class1 $Class)
    (climb $Class2 $Class)
    (set-det)
    (replace 
      (isa $Object $Class1) $Rels 
      (isa $Object $Class) $NewRels))
	

  (= (replace $Item $List $NewItem (Cons  $NewItem $List1))  
    (delete $List $List1 $Item $_ $_))
; /******************************************************************/
; /* */
; /* call : replace(+Item,+List,+NewItem,-NewList) */
; /* */
; /* argument : Item = descriptor */
; /* List = list of descriptors */
; /* NewItem = replacement descriptor */
; /* NewList = replaced list of descriptors */
; /* */
; /******************************************************************/
; /* replace removes Item from List and adds Newitem producing */
; /* NewList. */
; /******************************************************************/
	

  (= (climb  $Class $Class)  True)
; /******************************************************************/
; /* */
; /* call : climb(+Class1,-Class2) */
; /* */
; /* argument : Class1 = Subclass */
; /* Class2 = Superclass */
; /* */
; /******************************************************************/
; /* climb climbs in a-kind-of taxonomy from Class1 to superclass */
; /* Class2. */
; /******************************************************************/
  (= (climb $Class $SuperClass)  
    ( (==  
        (= 
          (ako  $Class1 $Class) true) 
        (get-atoms  &self)) (climb $Class1 $SuperClass)))


  !(dynamic (/ found 1))


  (= (findbag $X $G $_)  
    ( (add-atom  &self 
        (found  mark)) 
      (call $G) 
      (add-atom  &self 
        (found  $X)) fail))                            
  (= (findbag $_ $_ $L)  
    (collect-found Nil $L))                                     
                                                              

  (= (collect-found $L $L1)  
    (getnext $X)
    (collect-found 
      (Cons  $X $L) $L1))                      
  (= (collect-found  $L $L)  True)                                        
                                                              
;                                           (error
;                                             (syntax-error  operator_expected)
;                                             (file  arch2/arch2.pl 268 30 14766))



  (= help  
    ( (write 'Load data set with command: [Filename].') nl 
      (write 'Start arch2   with command: learn(X).') nl))


  !(help *)

