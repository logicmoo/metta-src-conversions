;                                           (convert_to_metta_file  aq1 $_111116 aq1/aq1.pl aq1/aq1.metta)

  !(op 700 xfx ..)
; /******************************************************************/
; /* aq1.pro Last modification: Fri Jan 14 19:30:51 1994 */
; /* Becker's implementation of AQ in MeTTa */
; /******************************************************************/
; ; ; Copyright (c) 1985 Jeffrey M. Becker ;
; /******************************************************************/
; /* reimpl. by : Thomas Hoppe */
; /* Mommsenstr. 50 */
; /* D-10629 Berlin */
; /* F.R.G. */
; /* E-Mail: hoppet@cs.tu-berlin.de */
; /* 1986 */
; /* */
; /* reference : AQ-PROLOG: A MeTTa Implementation of an */
; /* Attribute-Based Learning System, Becker, J.M., */
; /* Reports of the Intelligent Systems Group, */
; /* Department of Computer Science, University of */
; /* Illinois at Urbana-Champaign, Report Number */
; /* ISG 85-1, January 1985 */
; /* */
; /* Learning from Observation: Conceptual Clustering*/
; /* Michalski, R.S., Stepp, R.E., in: Machine */
; /* Learning, Michalski, R.S., Carbonell, J.G., */
; /* Mitchell, T.M. (eds.), Tioga Publishing */
; /* Company, Palo Alto, 1983. */
; /* */
; /* Inductive Learning, Michalski, R.S., in: Machine*/
; /* Learning, Michalski, R.S., Carbonell, J.G., */
; /* Mitchell, T.M. (eds.), Tioga Publishing */
; /* Company, Palo Alto, 1983. */
; /* */
; /* Update : The clause parent contained a typing error. */
; /* Now its correct. Thanks to Werner Emde. */
; /* */
; /* TH: Made some minor modification */
; /* */
; /******************************************************************/
; /* Because the program as a size about 22k, this version is */
; /* nearly undocumented, only the top-level procedures and some */
; /* very special changes against the report are documented. */
; /* For a detailed documentation consult the report. */
; /* */
; /* In general the following changes are implemented: */
; /* */
; /* - The top-level Routines are modified slightly. */
; /* - Ambique named predicates are renamed. */
; /* - Some special UNSW-PROLOG predicates are removed. */
; /* - Dependant on the 'bagof' predicate some cut's were necessary*/
; /* - Some ambiguity concerning atom comparision was removed. */
; /* - The unused predicat 'pos_cover_or_events' was removed. */
; /* */
; /* In the whole programm a destinction between < and @<, > and @> */
; /* etc. was introduced, which originates in DEC-10 PROLOG. */
; /* The <,>,=< and >= rever to arithmetric comparisions whereas */
; /* @<,@>,@=< and @>= rever to comparisions of atoms. Dependant on */
; /* you're local PROLOG system you have eventually to introduce */
; /* them. */
; /******************************************************************/


  (= (data $FILENAME)  
    (clear)
    (nl)
    (write '===> loading ')
    (write $FILENAME)
    (nl)
    (see $FILENAME)
    (repeat)
    (read $X)
    (or 
      (, 
        (= $X end-of-file) 
        (seen) 
        (add-is-symbol  &self 
          (dataset  $FILENAME)) 
        (set-det)) 
      (, 
        (process $X) 
        (fail))))
; /******************************************************************/
; /* */
; /* call : data (+FILENAME) */
; /* */
; /* arguments : FILENAME = dependent on you're local PROLOG */
; /* */
; /* side effects: Removes a previous loaded dataset */
; /* */
; /******************************************************************/
; /* Reads a dataset from the filesystem. */
; /******************************************************************/


  (= (process (domaintype $ATTR $D))  
    ( (add-is-symbol  &self 
        (domaintype  $ATTR $D)) (set-det)))
  (= (process (valueset $ATTR $VALSET))  
    ( (qsort $VALSET $VALS) 
      (add-is-symbol  &self 
        (valueset  $ATTR $VALS)) 
      (set-det)))
  (= (process (range $ATTR $LOW $HIGH))  
    ( (add-is-symbol  &self 
        (range  $ATTR $LOW $HIGH)) 
      (add-is-symbol  &self 
        (subtyp  $ATTR integer)) 
      (set-det)))
  (= (process (order $ATTR $ORD))  
    ( (length $ORD $HIGH) 
      (add-is-symbol  &self 
        (order  $ATTR $ORD)) 
      (add-is-symbol  &self 
        (range  $ATTR 1 $HIGH)) 
      (add-is-symbol  &self 
        (subtyp  $ATTR symbolic)) 
      (set-det)))
  (= (process (structure $ATTR $STRUC))  
    ( (explodestruc $ATTR $STRUC) 
      (add-is-symbol  &self 
        (structure  $ATTR $STRUC)) 
      (set-det)))
  (= (process (classes $CLIST))  
    (storeclasses $CLIST)
    (set-det))
  (= (process (events $CLASS $EVENTLIST))  
    (encodeevents $EVENTLIST $ENCODEDEVENTS)
    (storeevents $CLASS $ENCODEDEVENTS)
    (set-det))
  (= (process $X)  
    (write '===> Invalid Data: ')
    (nl)
    (write $X)
    (set-det))


  (= (storeclasses $CLIST)  
    ( (member $CLASSNAME $CLIST) 
      (add-is-symbol  &self 
        (class  $CLASSNAME)) 
      (fail)))
  (= (storeclasses  $_)  True)


  (= (storeevents $CLASS $EVENTLIST)  
    ( (member $EVENT $EVENTLIST) 
      (add-is-symbol  &self 
        (event  $CLASS $EVENT)) 
      (fail)))
  (= (storeevents  $_ $_)  True)


  (= (clear)  
    ( (==  
        (= 
          (dataset  $X) true) 
        (get-atoms  &self)) 
      (abolish domaintype 2) 
      (abolish valueset 2) 
      (abolish range 3) 
      (abolish order 2) 
      (abolish structure 2) 
      (abolish class 1) 
      (abolish event 2) 
      (abolish subtyp 2) 
      (abolish ancest 3) 
      (nl) 
      (write '===> Data ') 
      (write $X) 
      (write ' deleted.') 
      (nl) 
      (abolish dataset 1)))
; /******************************************************************/
; /* */
; /* call : clear */
; /* */
; /******************************************************************/
; /* Erases the actual dataset. */
; /******************************************************************/
  (= clear  True)


  (= (listdata)  
    ( (nl) 
      (==  
        (= 
          (dataset  $DATA_SET_NAME) true) 
        (get-atoms  &self)) 
      (write '===> Datenset ') 
      (write $DATA_SET_NAME) 
      (write :) 
      (nl) 
      (printdomaininfo) 
      (nl) 
      (printevents) 
      (set-det)))
; /******************************************************************/
; /* */
; /* call : listdata */
; /* */
; /******************************************************************/
; /* Display's the actual dataset */
; /******************************************************************/


  (= (printdomaininfo)  
    ( (==  
        (= 
          (domaintype  $VAR $DTYPE) true) 
        (get-atoms  &self)) 
      (write '===> Variable ') 
      (write $VAR) 
      (write ' of type ') 
      (write $DTYPE) 
      (write .) 
      (nl) 
      (fail)))
  (= printdomaininfo  True)


  (= (printevents)  
    ( (==  
        (= 
          (class  $CLASS) true) 
        (get-atoms  &self)) 
      (==  
        (= 
          (event  $CLASS $EVENT) true) 
        (get-atoms  &self)) 
      (printcomplex $EVENT) 
      (write  ::> ) 
      (write $CLASS) 
      (nl) 
      (fail)))
  (= printevents  True)


  (= (start)  
    (nl)
    (write '===> Maximal number of Stars in the next run ?')
    (nl)
    (read $MAX_STAR)
    (nl)
    (nl)
    (repeat)
    (write '===> Which mode in the next run ?')
    (nl)
    (nl)
    (write '      ic: Intersecting Covers')
    (nl)
    (write '      dc: Disjoint Covers')
    (nl)
    (write '      vl: VL mode (sequential)')
    (nl)
    (read $MODE)
    (nl)
    (abolish cover 2)
    (nl)
    (makecovers $MODE $MAX_STAR)
    (showcovers)
    (nl)
    (nl)
    (nl)
    (write '===> You wanna try a different mode (yes,no) ?')
    (nl)
    (read $RETRY)
    (nl)
    (== $RETRY no)
    (write '===> OK')
    (nl)
    (set-det))
; /******************************************************************/
; /* */
; /* call : start */
; /* */
; /******************************************************************/
; /* Start is the top-level loop of AQ-PROLOG. AQ can compute in */
; /* three different modes ic: Intersecting Covers, dc: Disjoint */
; /* Covers and vl: VL mode (a sequential one). */
; /******************************************************************/


  (= (makecovers ic $MAX_STAR)  
    ( (==  
        (= 
          (class  $CLASS) true) 
        (get-atoms  &self)) 
      (posevents $CLASS $EPOS) 
      (negevents $CLASS $ENEG) 
      (aq $EPOS $ENEG $EPOS $EPOS $MAX_STAR 
        (:: Nil) $COVER) 
      (storecover $CLASS $COVER) 
      (fail)))
  (= (makecovers ic $_)  
    (set-det))
  (= (makecovers dc $MAX_STAR)  
    ( (==  
        (= 
          (class  $CLASS) true) 
        (get-atoms  &self)) 
      (posevents $CLASS $EPOS) 
      (neg-cover-or-events $CLASS $ENEG) 
      (aq $EPOS $ENEG $EPOS $EPOS $MAX_STAR 
        (:: Nil) $COVER) 
      (storecover $CLASS $COVER) 
      (fail)))
  (= (makecovers dc $_)  
    (set-det))
  (= (makecovers vl $MAX_STAR)  
    ( (==  
        (= 
          (class  $CLASS) true) 
        (get-atoms  &self)) 
      (posevents $CLASS $EPOS) 
      (followingevents $CLASS $ENEG) 
      (aq $EPOS $ENEG $EPOS $EPOS $MAX_STAR 
        (:: Nil) $COVER) 
      (storecover $CLASS $COVER) 
      (fail)))
  (= (makecovers vl $_)  
    (set-det))
  (= (makecovers $X $_)  
    (nl)
    (write '===> ERROR - only the modes ic, dc or vl')
    (write ' are valid !')
    (nl)
    (fail))


  (= (storecover $CLASS $COVER)  
    ( (member $COMPLEX $COVER) 
      (add-is-symbol  &self 
        (cover  $CLASS $COMPLEX)) 
      (fail)))
  (= (storecover  $_ $_)  True)


  (= (posevents $CLASS $EPOS)  
    (findset $EVENT 
      (==  
        (= 
          (event  $CLASS $EVENT) true) 
        (get-atoms  &self)) $EPOS)
    (set-det))


  (= (negevents $CLASS $ENEG)  
    (findset $EVENT 
      (negevent $CLASS $EVENT) $ENEG)
    (set-det))


  (= (negevent $CLASS $EVENT)  
    ( (==  
        (= 
          (event  $NEG_CLASS $EVENT) true) 
        (get-atoms  &self)) (not (= $NEG_CLASS $CLASS))))


  (= (cover-or-event $CLASS $COMP)  
    (==  
      (= 
        (cover  $CLASS $COMP) true) 
      (get-atoms  &self)))
  (= (cover-or-event $CLASS $COMP)  
    (==  
      (= 
        (event  $CLASS $COMP) true) 
      (get-atoms  &self)))


  (= (neg-cover-or-events $CLASS $NEG_COMPS)  
    (findset $COMP 
      (negcomp $CLASS $COMP) $NEG_COMPS)
    (set-det))
; /******************************************************************/
; /* Against the AQ-PROLOG document the cut in neg_cover_or_events */
; /* is necessary to prevent backtracking, when the first clause of */
; /* makecovers fails. */
; /******************************************************************/


  (= (negcomp $CLASS $COMP)  
    (cover-or-event $NEG_CLASS $COMP)
    (not (= $NEG_CLASS $CLASS)))


  (= (followingevents $CLASS $SEVENTS)  
    (bagof $CLASS_NAME 
      (==  
        (= 
          (class  $CLASS_NAME) true) 
        (get-atoms  &self)) $CLASSES)
    (following $CLASS $CLASSES $FCLASSES)
    (findset $EVENT 
      (followev $FCLASSES $EVENT) $SEVENTS)
    (set-det))
; /******************************************************************/
; /* Watch out, this is the only occurence of 'bagof' for a correct */
; /* instantiation of CLASSES to an unmodified ordering see the */
; /* discussion of 'bagof' below. */
; /******************************************************************/


  (= (followev $FCLASSES $EVENT)  
    ( (member $CLASS $FCLASSES) (==  (= (event  $CLASS $EVENT) true) (get-atoms  &self))))


  (= (aq $_ $_ Nil $_ $_ $_ Nil)  
    (set-det))
  (= (aq $ELIST $FLIST $UN_COVERED Nil $MAX_STAR $BOUND $RESULT)  
    (write '===> Please wait a moment ...')
    (nl)
    (set-det)
    (aq $ELIST $FLIST $UN_COVERED $UN_COVERED $MAX_STAR $BOUND $RESULT))
  (= (aq $ELIST $FLIST $UN_COVERED $SEED_LIST $MAX_STAR $BOUND (Cons  $BEST $COVER))  
    (set-det)
    (first $SEED_LIST $SEED)
    (star $SEED $FLIST $MAX_STAR 
      (:: $ELIST $UN_COVERED) $BOUND $STAR)
    (lef $LEF)
    (selectbest $STAR 1 $LEF 
      (:: $ELIST $UN_COVERED) 
      (:: $BEST_COMP))
    (coveredbycomplex $BEST_COMP $UN_COVERED $COVERED_EVENTS)
    (trim $BEST_COMP $COVERED_EVENTS $BEST)
    (knockout1 $BEST $UN_COVERED $NEW_UN_COVERED)
    (knockout $STAR $SEED_LIST $NEW_SEED_LIST)
    (aq $ELIST $FLIST $NEW_UN_COVERED $NEW_SEED_LIST $MAX_STAR $BOUND $COVER)
    (set-det))


  (= (star $_ Nil $_ $_ $PSTAR $PSTAR)  
    (set-det))
; /******************************************************************/
; /* I'am not sure if the last cut in aq is necessary, but it works */
; /* correctly with it. */
; /******************************************************************/
  (= (star $E (Cons  $F $FTAIL) $MAX_STAR $LEF_ARGS $PSTAR $NEW_PSTAR)  
    (set-det)
    (extendagainst $E $F $ESTAR)
    (multiply $PSTAR $ESTAR $F $EP_STAR)
    (absorb $EP_STAR $MAX_STAR $AP_STAR)
    (lef $LEF)
    (selectbest $AP_STAR $MAX_STAR $LEF $LEF_ARGS $REDUCED_STAR)
    (star $E $FTAIL $MAX_STAR $LEF_ARGS $REDUCED_STAR $NEW_PSTAR))


  (= (multiply $COM_SET $PSTAR $NEG_EVENT $EP_STAR)  
    (findset $NEW_COMPS 
      (, 
        (member $COMP $COM_SET) 
        (dis-or-mult $COMP $PSTAR $NEG_EVENT $NEW_COMPS)) $EP_LIST)
    (appendx $EP_LIST $EP_STAR)
    (set-det))


  (= (dis-or-mult $COMP $_ $NEG $ERG)  
    (disjointcomps $COMP $NEG)
    (= $ERG 
      (:: $COMP))
    (set-det))
; /******************************************************************/
; /* DIS_OR_MULT was introduced, because in AQ-PROLOG the definition*/
; /* of multiply is heavily dependant on UNSW-PROLOG. The definition*/
; /* serves the purpose of a substitution for the '->' operator, */
; /* which seems to be the IF-THEN-ELSE definition in UNSW-PROLOG. */
; /******************************************************************/
  (= (dis-or-mult $COMP $PSTAR $_ $NEW)  
    (findset $A 
      (, 
        (member $P $PSTAR) 
        (product $COMP $P $A)) $NEW)
    (set-det))


  (= (absorb $STAR $MAX_STAR $ASTAR)  
    (length $STAR $N)
    (> $N $MAX_STAR)
    (set-det)
    (absourbr $STAR Nil $STAR1)
    (absourbr $STAR1 Nil $ASTAR))
  (= (absorb  $STAR $_ $STAR)  True)


  (= (absourbr Nil $S $S)  
    (set-det))
  (= (absourbr (Cons  $C $S) $B $AR_STAR)  
    (set-det)
    (knockout1 $C $S $RS)
    (absourbr $RS 
      (Cons  $C $B) $AR_STAR))


  (= (selectbest $PSTAR $MAX_SIZE $_ $_ $PSTAR)  
    (length $PSTAR $L)
    (=< $L $MAX_SIZE)
    (set-det))
  (= (selectbest $PSTAR $MAX_SIZE (Cons  $CT $CTX) $LEF_ARGS $REDUCED_STAR)  
    (set-det)
    (reduce $PSTAR $CT $LEF_ARGS $RSTAR)
    (selectbest $RSTAR $MAX_SIZE $CTX $LEF_ARGS $REDUCED_STAR))
  (= (selectbest $PSTAR $MAX_SIZE Nil $_ $RSTAR)  
    (firstn $PSTAR $MAX_SIZE $RSTAR)
    (set-det))


  (= (reduce $PSTAR (:: $CRIT_FN $N $D) (:: $EPLUS $UN_COV_EPLUS) $RSTAR)  
    (=.. $GET_CRIT_FN 
      (:: $CRIT_FN $COMP $EPLUS $UN_COV_EPLUS $V))
    (findset $VC 
      (, 
        (member $COMP $PSTAR) $GET_CRIT_FN 
        (= $VC 
          (:: $V $COMP))) $VLIST)
    (minmax $VLIST $MIN $MAX)
    (is $TOL 
      (+ $MIN 
        (/ 
          (* $N 
            (- $MAX $MIN)) $D)))
    (findset $C 
      (, 
        (member 
          (:: $V $C) $VLIST) 
        (=< $V $TOL)) $RSTAR)
    (set-det))


  (= (minmax (Cons  (:: $X $_) (Cons  (:: $Y $_) $R)) $MIN $MAX)  
    (=< $X $Y)
    (set-det)
    (lohi 
      (Cons  $X 
        (Cons  $Y $R)) $MIN $MAX))
  (= (minmax (Cons  (:: $X $_) (Cons  (:: $Y $_) $R)) $MIN $MAX)  
    (set-det)
    (lohi 
      (Cons  $Y 
        (Cons  $X $R)) $MIN $MAX))


  (= (lohi (Cons  $X (Cons  $Y (Cons  (:: $Z $_) $R))) $MIN $MAX)  
    (set-det)
    (min 
      (:: $X $Z) $A)
    (max 
      (:: $Y $Z) $B)
    (lohi 
      (Cons  $A 
        (Cons  $B $R)) $MIN $MAX))
  (= (lohi (:: $X $Y) $X $Y)  
    (set-det))


  (= (numbercovered $COMP $_ $EVENTS $N)  
    (coveredbycomplex $COMP $EVENTS $COVERED_E)
    (length $COVERED_E $P)
    (is $N 
      (- $P))
    (set-det))


  (= (numberofselectors $COMP $_ $_ $N)  
    (length $COMP $N)
    (set-det))


  (= (lef  ((numbercovered 0 1) (numberofselectors 0 1)))  True)


  (= (knockout $OUTER_COMPS $INNER_COMPS $UN_COV_COMPS)  
    (set-det)
    (findset $IN_COMP 
      (call (, (member $IN_COMP $INNER_COMPS) (nonecover $OUTER_COMPS $IN_COMP))) $UN_COV_COMPS))


  (= (nonecover Nil $IN_COMP)  
    (set-det))
  (= (nonecover (Cons  $COMP $CX) $IN_COMP)  
    (set-det)
    (not (covers $COMP $IN_COMP))
    (nonecover $CX $IN_COMP))


  (= (knockout1 $OUTER_C $INNER_COMPS $UN_COV_COMPS)  
    (set-det)
    (findset $IN_C 
      (call (, (member $IN_C $INNER_COMPS) (not (covers $OUTER_C $IN_C)))) $UN_COV_COMPS))


  (= (coveredbycomplex $COMPLEX $EVENTS $COVERED_E)  
    (findset $E 
      (, 
        (member $E $EVENTS) 
        (covers $COMPLEX $E)) $COVERED_E)
    (set-det))


  (= (newselector (Cons  (= $A1 $V1) $T1) (= $A1 $V2) (Cons  (= $A1 $V2) $T1))  
    (set-det))
  (= (newselector (Cons  (= $A1 $V1) $T1) (= $A2 $V2) (Cons  (= $A1 $V1) $T3))  
    (@< $A1 $A2)
    (set-det)
    (newselector $T1 
      (= $A2 $V2) $T3))
  (= (newselector (Cons  (= $A1 $V1) $T1) (= $A2 $V2) (Cons  (= $A2 $V2) (Cons  (= $A1 $V1) $T1)))  
    (@> $A1 $A2)
    (set-det))
  (= (newselector Nil $SEL (:: $SEL))  
    (set-det))


  (= (covers (Cons  (= $A $OUT_VAL) $OUT_C) (Cons  (= $A $IN_VAL) $IN_C))  
    (set-det)
    (includes 
      (= $A $OUT_VAL) 
      (= $A $IN_VAL))
    (covers $OUT_C $IN_C))
  (= (covers (Cons  (= $A1 $OUT_V) $OUT_C) (Cons  (= $A2 $IN_V) $IN_C))  
    (set-det)
    (@< $A2 $A1)
    (covers 
      (Cons  
        (= $A1 $OUT_V) $OUT_C) $IN_C))
  (= (covers Nil $_)  
    (set-det))


  (= (includes (= $ATTR $OUT_VALS) (= $ATTR $IN_VALS))  
    ( (==  
        (= 
          (domaintype  $ATTR nominal) true) 
        (get-atoms  &self)) 
      (set-det) 
      (subset $IN_VALS $OUT_VALS)))
  (= (includes (= $ATTR $OUT_VALS) (= $ATTR $IN_VALS))  
    ( (==  
        (= 
          (domaintype  $ATTR linear) true) 
        (get-atoms  &self)) 
      (set-det) 
      (includeslin $OUT_VALS $IN_VALS)))
  (= (includes (= $ATTR $OUT_VALS) (= $ATTR $IN_VALS))  
    ( (==  
        (= 
          (domaintype  $ATTR structured) true) 
        (get-atoms  &self)) 
      (set-det) 
      (supremum $ATTR $OUT_VALS $IN_VALS)))


  (= (disjointcomps (Cons  (= $A $V1) $T1) (Cons  (= $A $V2) $T2))  
    (set-det)
    (disjointsel 
      (= $A $V1) 
      (= $A $V2)))
  (= (disjointcomps (Cons  $_ $T1) (Cons  $_ $T2))  
    (set-det)
    (disjointcomps $T1 $T2))


  (= (disjointsel (= $ATTR $VALS1) (= $ATTR $VALS2))  
    ( (==  
        (= 
          (domaintype  $ATTR nominal) true) 
        (get-atoms  &self)) 
      (set-det) 
      (disjoint $VALS1 $VALS2)))
  (= (disjointsel (= $ATTR $VALS1) (= $ATTR $VALS2))  
    ( (==  
        (= 
          (domaintype  $ATTR linear) true) 
        (get-atoms  &self)) 
      (set-det) 
      (disjointlin $VALS1 $VALS2)))
  (= (disjointsel (= $ATTR $VALS1) (= $ATTR $VALS2))  
    ( (==  
        (= 
          (domaintype  $ATTR structured) true) 
        (get-atoms  &self)) 
      (set-det) 
      (not (supremum $ATTR $VALS1 $VALS2)) 
      (not (supremum $ATTR $VALS2 $VALS1))))


  (= (negate $COMPLEX $NEG_COMPS)  
    (findset $NEGC 
      (, 
        (member $SEL $COMPLEX) 
        (negatesel $SEL $NSEL) 
        (= $NEGC 
          (:: $NSEL))) $NEG_COMPS)
    (set-det))


  (= (negatesel (= $ATTR $VALS) (= $ATTR $NEG_VALS))  
    ( (==  
        (= 
          (domaintype  $ATTR nominal) true) 
        (get-atoms  &self)) 
      (set-det) 
      (==  
        (= 
          (valueset  $ATTR $ALL_VALS) true) 
        (get-atoms  &self)) 
      (difference $ALL_VALS $VALS $NEG_VALS) 
      (not (= $NEG_VALS Nil))))
  (= (negatesel (= $ATTR $VALS) (= $ATTR $NEG_VALS))  
    ( (==  
        (= 
          (domaintype  $ATTR linear) true) 
        (get-atoms  &self)) 
      (set-det) 
      (negatelin $ATTR $VALS $NEG_VALS) 
      (not (= $NEG_VALS Nil))))


  (= (extendagainst (Cons  (= $A $VP) $P) (Cons  (= $A $VN) $N) (Cons  (:: (= $A $VX)) $X))  
    (extendref 
      (= $A $VP) 
      (= $A $VN) 
      (= $A $VX))
    (set-det)
    (extendagainst $P $N $X))
  (= (extendagainst (Cons  (= $AP $VP) $P) (Cons  (= $AN $VN) $N) $X)  
    (@< $AP $AN)
    (set-det)
    (extendagainst $P 
      (Cons  
        (= $AN $VN) $N) $X))
  (= (extendagainst (Cons  (= $AP $VP) $P) (Cons  (= $AN $VN) $N) $X)  
    (set-det)
    (extendagainst 
      (Cons  
        (= $AP $VP) $P) $N $X))
  (= (extendagainst Nil $_ Nil)  
    (set-det))
  (= (extendagainst $_ Nil Nil)  
    (set-det))


  (= (extendref (= $ATTR $POS_VALS) (= $ATTR $NEG_VALS) (= $ATTR $EXT_VALS))  
    ( (==  
        (= 
          (domaintype  $ATTR nominal) true) 
        (get-atoms  &self)) 
      (set-det) 
      (disjoint $POS_VALS $NEG_VALS) 
      (negatesel 
        (= $ATTR $NEG_VALS) 
        (= $ATTR $EXT_VALS))))
  (= (extendref (= $ATTR $POS_VALS) (= $ATTR $NEG_VALS) (= $ATTR $EXT_VALS))  
    ( (==  
        (= 
          (domaintype  $ATTR linear) true) 
        (get-atoms  &self)) 
      (set-det) 
      (disjointlin $POS_VALS $NEG_VALS) 
      (negatelin $ATTR $NEG_VALS $NN_VALS) 
      (extendedlin $POS_VALS $NN_VALS $EXT_VALS)))
  (= (extendref (= $ATTR $POS_VAL) (= $ATTR $NEG_VAL) (= $ATTR $EXT_VAL))  
    ( (==  
        (= 
          (domaintype  $ATTR structured) true) 
        (get-atoms  &self)) 
      (set-det) 
      (supremum $ATTR $EXT_VAL $POS_VAL) 
      (not (supremum $ATTR $EXT_VAL $NEG_VAL)) 
      (parent $ATTR $EXT_VAL $EXT_PARENT) 
      (supremum $ATTR $EXT_PARENT $NEG_VAL)))


  (= (refunion (Cons  $C1 (Cons  $C2 $T)) $REFU)  
    (set-det)
    (refu $C1 $C2 $C3)
    (refunion 
      (Cons  $C3 $T) $REFU))
  (= (refunion (:: $COMP) $COMP)  
    (set-det))


  (= (refu (Cons  (= $A $V1) $C1) (Cons  (= $A $V2) $C2) (Cons  (= $A $VU) $CU))  
    (selunion 
      (= $A $V1) 
      (= $A $V2) 
      (= $A $VU))
    (set-det)
    (refu $C1 $C2 $CU))
  (= (refu (Cons  (= $A1 $V1) $C1) (Cons  (= $A2 $V2) $C2) $U)  
    (@< $A1 $A2)
    (set-det)
    (refu $C1 
      (Cons  
        (= $A2 $V2) $C2) $U))
  (= (refu (Cons  (= $A1 $V1) $C1) (Cons  (= $A2 $V2) $C2) $U)  
    (set-det)
    (refu 
      (Cons  
        (= $A1 $V1) $C1) $C2 $U))
  (= (refu Nil $_ Nil)  
    (set-det))
  (= (refu $_ Nil Nil)  
    (set-det))


  (= (selunion (= $ATTR $VALS1) (= $ATTR $VALS2) (= $ATTR $UVALS))  
    ( (==  
        (= 
          (domaintype  $ATTR nominal) true) 
        (get-atoms  &self)) 
      (set-det) 
      (union $VALS1 $VALS2 $UVALS) 
      (==  
        (= 
          (valueset  $ATTR $ALL_VALS) true) 
        (get-atoms  &self)) 
      (not (equals $UVALS $ALL_VALS))))
  (= (selunion (= $ATTR $VALS1) (= $ATTR $VALS2) (= $ATTR $UVALS))  
    ( (==  
        (= 
          (domaintype  $ATTR linear) true) 
        (get-atoms  &self)) 
      (set-det) 
      (low $VALS1 $L1) 
      (low $VALS2 $L2) 
      (min 
        (:: $L1 $L2) $LOW) 
      (highest $VALS1 $H1) 
      (highest $VALS2 $H2) 
      (max 
        (:: $H1 $H2) $HIGH) 
      (==  
        (= 
          (range  $ATTR $MIN $MAX) true) 
        (get-atoms  &self)) 
      (not (= $LOW $MIN)) 
      (not (== $HIGH $MAX)) 
      (== $UVALS 
        (:: (.. $LOW $HIGH))) 
      (set-det)))
  (= (selunion (= $ATTR $VAL1) (= $ATTR $VAL2) (= $ATTR $UV_AL))  
    ( (==  
        (= 
          (domaintype  $ATTR structured) true) 
        (get-atoms  &self)) 
      (supremum $ATTR $UV_AL $VAL1) 
      (supremum $ATTR $UV_AL $VAL2)))


  (= (product (Cons  (= $A $V1) $T1) (Cons  (= $A $V2) $T2) (Cons  (= $A $V3) $T3))  
    (set-det)
    (selproduct 
      (= $A $V1) 
      (= $A $V2) 
      (= $A $V3))
    (set-det)
    (product $T1 $T2 $T3))
  (= (product (Cons  (= $A1 $V1) $T1) (Cons  (= $A2 $V2) $T2) (Cons  (= $A1 $V1) $T3))  
    (@< $A1 $A2)
    (set-det)
    (product 
      (Cons  
        (= $A2 $V2) $T2) $T1 $T3))
  (= (product (Cons  (= $A1 $V1) $T1) (Cons  (= $A2 $V2) $T2) (Cons  (= $A2 $V2) $T3))  
    (set-det)
    (product 
      (Cons  
        (= $A1 $V1) $T1) $T2 $T3))
  (= (product $X Nil $X)  
    (set-det))
  (= (product Nil $X $X)  
    (set-det))


  (= (selproduct (= $ATTR $VALS1) (= $ATTR $VALS2) (= $ATTR $PROD_VALS))  
    ( (==  
        (= 
          (domaintype  $ATTR nominal) true) 
        (get-atoms  &self)) 
      (set-det) 
      (intersection $VALS1 $VALS2 $PROD_VALS) 
      (not (= $PROD_VALS Nil))))
  (= (selproduct (= $ATTR $VALS1) (= $ATTR $VALS2) (= $ATTR $PROD_VALS))  
    ( (==  
        (= 
          (domaintype  $ATTR linear) true) 
        (get-atoms  &self)) 
      (set-det) 
      (productlin $VALS1 $VALS2 $PROD_VALS) 
      (not (= $PROD_VALS Nil))))
  (= (selproduct (= $ATTR $VALS1) (= $ATTR $VALS2) (= $ATTR $VALS1))  
    ( (==  
        (= 
          (domaintype  $ATTR structured) true) 
        (get-atoms  &self)) 
      (supremum $ATTR $VALS2 $VALS1) 
      (set-det)))
  (= (selproduct (= $ATTR $VALS1) (= $ATTR $VALS2) (= $ATTR $VALS2))  
    ( (==  
        (= 
          (domaintype  $ATTR structured) true) 
        (get-atoms  &self)) 
      (supremum $ATTR $VALS1 $VALS2) 
      (set-det)))


  (= (trim $COMP $COVERED_EVENTS $TRIMMED_COMP)  
    (set-det)
    (refunion $COVERED_EVENTS $REFU)
    (trimcomp $COMP $REFU $TRIMMED_COMP))


  (= (trimcomp (Cons  (= $A $V1) $C1) (Cons  (= $A $VU) $CU) (Cons  (= $A $VT) $CT))  
    (set-det)
    (selproduct 
      (= $A $V1) 
      (= $A $VU) 
      (= $A $VT))
    (trimcomp $C1 $CU $CT))
  (= (trimcomp (Cons  (= $A1 $V1) $C1) (Cons  (= $A2 $VU) $CU) $CT)  
    (@< $A2 $A1)
    (set-det)
    (trimcomp 
      (Cons  
        (= $A1 $V1) $C1) $CU $CT))
  (= (trimcomp (Cons  (= $A1 $V1) $C1) (Cons  (= $A2 $VU) $CU) (Cons  (= $A1 $V1) $CT))  
    (set-det)
    (trimcomp $C1 
      (Cons  
        (= $A2 $VU) $CU) $CT))
  (= (trimcomp $X Nil $X)  
    (set-det))
  (= (trimcomp Nil $_ Nil)  
    (set-det))


  (= (encodeevents Nil Nil)  
    (set-det))
  (= (encodeevents (Cons  $E $REST) (Cons  $EE $ENCODE_REST))  
    (set-det)
    (encodeevent $E Nil $EE)
    (encodeevents $REST $ENCODE_REST))


  (= (encodeevent Nil $EVENT $EVENT)  
    (set-det))
  (= (encodeevent (Cons  $SEL $E) $PARTIAL_EV $NEW_PARTIAL_EV)  
    (set-det)
    (encodesel $SEL $ENCODED_SEL)
    (newselector $PARTIAL_EV $ENCODED_SEL $PPEV)
    (encodeevent $E $PPEV $NEW_PARTIAL_EV))


  (= (encodesel (:: (= $ATTR $VAL)) (= $ATTR (:: $VAL)))  
    ( (==  
        (= 
          (domaintype  $ATTR nominal) true) 
        (get-atoms  &self)) (set-det)))
  (= (encodesel (:: (= $ATTR $VAL)) (= $ATTR (:: (.. $VAL $VAL))))  
    ( (==  
        (= 
          (domaintype  $ATTR linear) true) 
        (get-atoms  &self)) 
      (==  
        (= 
          (subtyp  $ATTR integer) true) 
        (get-atoms  &self)) 
      (set-det)))
  (= (encodesel (:: (= $ATTR $SYM)) (= $ATTR (:: (.. $ORD $ORD))))  
    ( (==  
        (= 
          (domaintype  $ATTR linear) true) 
        (get-atoms  &self)) 
      (==  
        (= 
          (subtyp  $ATTR symbolic) true) 
        (get-atoms  &self)) 
      (set-det) 
      (ord $ATTR $SYM $ORD)))
  (= (encodesel (:: (= $ATTR $VAL)) (= $ATTR $VAL))  
    ( (==  
        (= 
          (domaintype  $ATTR structured) true) 
        (get-atoms  &self)) (set-det)))
  (= (encodesel $S $_)  
    (write '===> ERROR - unknown selector type: ')
    (write $S))


  (= (showcovers)  
    ( (==  
        (= 
          (class  $CLASS) true) 
        (get-atoms  &self)) 
      (showcover $CLASS) 
      (fail)))
  (= showcovers  True)


  (= (showcover $CLASS)  
    ( (nl) 
      (nl) 
      (write '===> Cover of class ') 
      (write $CLASS) 
      (write :) 
      (set-det) 
      (nl) 
      (==  
        (= 
          (cover  $CLASS $COVER) true) 
        (get-atoms  &self)) 
      (printcomplex $COVER) 
      (nl) 
      (fail)))
  (= (showcover  $_)  True)


  (= (printcomplex $COMPLEX)  
    (member $SELECTOR $COMPLEX)
    (printselector $SELECTOR)
    (fail))
  (= (printcomplex $_)  
    (set-det))


  (= (printselector (= $ATTR $VALS))  
    ( (==  
        (= 
          (domaintype  $ATTR nominal) true) 
        (get-atoms  &self)) 
      (set-det) 
      (write [) 
      (write $ATTR) 
      (write  = ) 
      (prinlist $VALS) 
      (write ])))
  (= (printselector (= $ATTR $VALS))  
    ( (==  
        (= 
          (domaintype  $ATTR linear) true) 
        (get-atoms  &self)) 
      (==  
        (= 
          (subtyp  $ATTR integer) true) 
        (get-atoms  &self)) 
      (set-det) 
      (write [) 
      (write $ATTR) 
      (write  = ) 
      (prinlin $VALS) 
      (write ])))
  (= (printselector (= $ATTR $VALS))  
    ( (==  
        (= 
          (domaintype  $ATTR linear) true) 
        (get-atoms  &self)) 
      (==  
        (= 
          (subtyp  $ATTR symbolic) true) 
        (get-atoms  &self)) 
      (set-det) 
      (write [) 
      (write $ATTR) 
      (write  = ) 
      (prinsym $ATTR $VALS) 
      (write ])))
  (= (printselector (= $ATTR $VAL))  
    ( (==  
        (= 
          (domaintype  $ATTR structured) true) 
        (get-atoms  &self)) 
      (set-det) 
      (write [) 
      (write $ATTR) 
      (write  = ) 
      (write $VAL) 
      (write ])))


  (= (intersection (Cons  $A $B) (Cons  $A $C) (Cons  $A $X))  
    (set-det)
    (intersection $B $C $X))
  (= (intersection (Cons  $A $B) (Cons  $C $D) $X)  
    (@< $A $C)
    (set-det)
    (intersection 
      (Cons  $C $D) $B $X))
  (= (intersection (Cons  $A $B) (Cons  $C $D) $X)  
    (set-det)
    (intersection 
      (Cons  $A $B) $D $X))
  (= (intersection $Y Nil Nil)  
    (set-det))
  (= (intersection Nil $Y Nil)  
    (set-det))


  (= (difference (Cons  $A $B) (Cons  $A $C) $X)  
    (set-det)
    (difference $B $C $X))
  (= (difference (Cons  $A $B) (Cons  $C $D) (Cons  $A $X))  
    (@< $A $C)
    (set-det)
    (difference $B 
      (Cons  $C $D) $X))
  (= (difference (Cons  $A $B) (Cons  $C $D) (Cons  $C $X))  
    (set-det)
    (difference 
      (Cons  $A $B) $D $X))
  (= (difference $Y Nil $Y)  
    (set-det))
  (= (difference Nil $Y Nil)  
    (set-det))


  (= (union (Cons  $A $B) (Cons  $A $C) (Cons  $A $X))  
    (set-det)
    (union $B $C $X))
  (= (union (Cons  $A $B) (Cons  $C $D) (Cons  $A $X))  
    (@< $A $C)
    (set-det)
    (union 
      (Cons  $C $D) $B $X))
  (= (union (Cons  $A $B) (Cons  $C $D) (Cons  $C $X))  
    (set-det)
    (union 
      (Cons  $A $B) $D $X))
  (= (union $Y Nil $Y)  
    (set-det))
  (= (union Nil $Y $Y)  
    (set-det))


  (= (disjoint (Cons  $A $B) (Cons  $C $D))  
    (@< $A $C)
    (set-det)
    (disjoint 
      (Cons  $C $D) $B))
  (= (disjoint (Cons  $A $B) (Cons  $C $D))  
    (@< $C $A)
    (set-det)
    (disjoint 
      (Cons  $A $B) $D))
  (= (disjoint $_ Nil)  
    (set-det))
  (= (disjoint Nil $_)  
    (set-det))


  (= (subset (Cons  $A $B) (Cons  $A $C))  
    (set-det)
    (subset $B $C))
  (= (subset (Cons  $A $B) (Cons  $C $D))  
    (@> $A $C)
    (set-det)
    (subset 
      (Cons  $A $B) $D))
  (= (subset Nil $_)  
    (set-det))


  (= (equals $X $Y)  
    (= $X $Y)
    (set-det))


  (= (cardinality $X $N)  
    (set-det)
    (length $X $N))


  (= (ord $ATTR $SYM $N)  
    ( (==  
        (= 
          (order  $ATTR $L) true) 
        (get-atoms  &self)) 
      (at $SYM $L $N 1) 
      (set-det)))


  (= (at  $SYM (Cons  $SYM $X) $N $N)  True)
  (= (at $SYM (Cons  $_ $X) $N $I)  
    (is $J 
      (+ $I 1))
    (at $SYM $X $N $J)
    (set-det))
  (= (at $_ $_ $_ $_)  
    (write '===> ERROR - symbol undeclared'))


  (= (low  (Cons  (..  $L $H) $_) $L)  True)


  (= (highest  ((..  $L $H)) $H)  True)
  (= (highest (Cons  (.. $L $H) $X) $HIGH)  
    (set-det)
    (highest $X $HIGH))


  (= (includeslin  $_ ())  True)
  (= (includeslin (Cons  (.. $LO $HO) $XO) (Cons  (.. $LI $HI) $XI))  
    (@< $HO $LI)
    (set-det)
    (includeslin $XO 
      (Cons  
        (.. $LI $HI) $XI)))
  (= (includeslin (Cons  (.. $LO $HO) $XO) (Cons  (.. $LI $HI) $XI))  
    (set-det)
    (@=< $LO $LI)
    (@>= $HO $HI)
    (includeslin 
      (Cons  
        (.. $LO $HO) $XO) $XI))


  (= (disjointlin Nil $_)  
    (set-det))
  (= (disjointlin $_ Nil)  
    (set-det))
  (= (disjointlin (Cons  (.. $L1 $H1) $X1) (Cons  (.. $L2 $H2) $X2))  
    (@< $H1 $L2)
    (set-det)
    (disjointlin $X1 
      (Cons  
        (.. $L2 $H2) $X2)))
  (= (disjointlin (Cons  (.. $L1 $H1) $X1) (Cons  (.. $L2 $H2) $X2))  
    (@< $H2 $L1)
    (set-det)
    (disjointlin $X2 
      (Cons  
        (.. $L1 $H1) $X1)))


  (= (negatelin $ATTR (Cons  (.. $LP $HP) $XP) $N)  
    (set-det)
    (neglinlow $ATTR $LP $LOW)
    (neglinmid 
      (Cons  
        (.. $LP $HP) $XP) $HI $MID)
    (neglinhi $ATTR $HI $HIGH)
    (appendx 
      (:: $LOW $MID $HIGH) $N))


  (= (neglinlow $ATTR $LP Nil)  
    ( (==  
        (= 
          (range  $ATTR $LP $_) true) 
        (get-atoms  &self)) (set-det)))
  (= (neglinlow $ATTR $LP (:: (.. $LOW $H)))  
    ( (set-det) 
      (==  
        (= 
          (range  $ATTR $LOW $_) true) 
        (get-atoms  &self)) 
      (is $H 
        (- $LP 1))))


  (= (neglinmid (:: (.. $L $H)) $H Nil)  
    (set-det))
  (= (neglinmid (Cons  (.. $L1 $H1) (Cons  (.. $L2 $H2) $X)) $HI (:: (.. $L $H) $N))  
    (> $L2 
      (+ $H1 1))
    (set-det)
    (is $L 
      (+ $H1 1))
    (is $H 
      (- $L2 1))
    (neglinmid 
      (Cons  
        (.. $L2 $H2) $X) $HI $N))
  (= (neglinmid (Cons  (.. $L1 $H1) (Cons  (.. $L2 $H2) $X)) $HI $N)  
    (set-det)
    (neglinmid $X $HI $N))


  (= (neglinhi $ATTR $HI Nil)  
    ( (==  
        (= 
          (range  $ATTR $_ $HI) true) 
        (get-atoms  &self)) (set-det)))
  (= (neglinhi $ATTR $HI (:: (.. $L $HIGH)))  
    ( (set-det) 
      (==  
        (= 
          (range  $ATTR $_ $HIGH) true) 
        (get-atoms  &self)) 
      (is $L 
        (+ $HI 1))))


  (= (extendedlin $_ Nil Nil)  
    (set-det))
  (= (extendedlin Nil $_ Nil)  
    (set-det))
  (= (extendedlin (Cons  (.. $LP $HP) $XP) (Cons  (.. $LN $HN) $XN) $XVALS)  
    (@< $HN $LP)
    (set-det)
    (extendedlin 
      (Cons  
        (.. $LP $HP) $XP) $XN $XVALS))
  (= (extendedlin (Cons  (.. $LP $HP) $XP) (Cons  (.. $LN $HN) $XN) $XVALS)  
    (@< $HP $LN)
    (set-det)
    (extendedlin $XP 
      (Cons  
        (.. $LN $HN) $XN) $XVALS))
  (= (extendedlin (Cons  (.. $LP $HP) $XP) (Cons  (.. $LN $HN) $XN) (Cons  (.. $LN $HN) $XVALS))  
    (set-det)
    (@=< $LN $LP)
    (@>= $HN $HP)
    (extendedlin $XP $XN $XVALS))


  (= (productlin Nil $_ Nil)  
    (set-det))
  (= (productlin $_ Nil Nil)  
    (set-det))
  (= (productlin (Cons  (.. $L1 $H1) $X1) (Cons  (.. $L2 $H2) $X2) $P)  
    (@< $H1 $L2)
    (set-det)
    (productlin $X1 
      (Cons  
        (.. $L2 $H2) $X2) $P))
  (= (productlin (Cons  (.. $L1 $H1) $X1) (Cons  (.. $L2 $H2) $X2) $P)  
    (@< $H2 $L1)
    (set-det)
    (productlin $X2 
      (Cons  
        (.. $L1 $H1) $X1) $P))
  (= (productlin (Cons  (.. $L1 $H1) $X1) (Cons  (.. $L2 $H2) $X2) (Cons  (.. $L $H) $P))  
    (set-det)
    (max 
      (:: $L1 $L2) $L)
    (min 
      (:: $H1 $H2) $H)
    (productlin $X1 $X2 $P))


  (= (prinlin (:: $A))  
    (set-det)
    (prinseg $A))
  (= (prinlin (:: $A $B))  
    (set-det)
    (prinseg $A)
    (write ' v ')
    (prinlin $B))
  (= (prinlin Nil)  
    (set-det)
    (write '===> ERROR - null RHS in linear selector'))


  (= (prinseg (.. $L $H))  
    (= $L $H)
    (write $L)
    (set-det))
  (= (prinseg $A)  
    (write $A)
    (set-det))


  (= (prinsym $ATTR (:: $A))  
    (set-det)
    (prinsymseg $ATTR $A))
  (= (prinsym $ATTR (Cons  $A $B))  
    (set-det)
    (prinsymseg $ATTR $A)
    (write ,)
    (prinsym $ATTR $B))
  (= (prinsym $_ Nil)  
    (set-det)
    (write '===> ERROR - null RHS in linear selector'))


  (= (prinsymseg $ATTR (.. $L $H))  
    (= $L $H)
    (ord $ATTR $SYM $L)
    (write $SYM)
    (set-det))
  (= (prinsymseg $ATTR (.. $L $H))  
    (ord $ATTR $SYML $L)
    (ord $ATTR $SYMH $H)
    (write (.. $SYML $SYMH))
    (set-det))


  (= (supremum $ATTR $HI_NODE $LO_NODE)  
    ( (==  
        (= 
          (ancest  $ATTR $LO_NODE $ALIST) true) 
        (get-atoms  &self)) (member $HI_NODE $ALIST)))
; /******************************************************************/
; /* In AQ-PROLOG the procedure 'predecessor' is called 'ancestor'. */
; /* Because in some (I think the most) PROLOG dialects a build-in */
; /* predicate 'ancestor' exists, I preferred to rename it. */
; /******************************************************************/
  (= (supremum  $ATTR $X $X)  True)


  (= (parent $ATTR $NODE $PARENT)  
    (==  
      (= 
        (ancest  $ATTR $NODE 
          (Cons  $PARENT $_)) true) 
      (get-atoms  &self)))


  (= (explodestruc $ATTR $STRUCTUR_SPEC)  
    ( (allnodes $STRUCTUR_SPEC $NODE_LIST) 
      (member $NODE $NODE_LIST) 
      (predecessorlist $NODE $STRUCTUR_SPEC $ALIST) 
      (add-is-symbol  &self 
        (ancest  $ATTR $NODE $ALIST)) 
      (fail)))
  (= (explodestruc  $_ $_)  True)


  (= (allnodes Nil Nil)  
    (set-det))
  (= (allnodes (Cons  (parent $SIBS $P) $X) $NODE_LIST)  
    (set-det)
    (qsort $SIBS $L1)
    (union $L1 
      (:: $P) $L2)
    (allnodes $X $L3)
    (union $L2 $L3 $NODE_LIST))


  (= (predecessorlist $NODE $STRUCTUR_SPEC (Cons  $P $X))  
    (father $NODE $STRUCTUR_SPEC $P)
    (set-det)
    (predecessorlist $P $STRUCTUR_SPEC $X))
; /******************************************************************/
; /* In AQ-PROLOG the procedure 'predecessorlist' is called */
; /* 'ancestorlist' Because I wanna be a little bit consistend in */
; /* the naming of predicates and 'ancestor' was already renamed */
; /* I preferred to rename this predicate too. */
; /******************************************************************/
  (= (predecessorlist $_ $_ Nil)  
    (set-det))


  (= (father $NODE (Cons  (parent $SIBS $P) $X) $P)  
    (member $NODE $SIBS)
    (set-det))
  (= (father $NODE (Cons  $_ $X) $P)  
    (father $NODE $X $P)
    (set-det))


  (= (first (Cons  $A $B) $A)  
    (set-det))


  (= (appendx $X $Y)  
    (findset $A 
      (, 
        (member $B $X) 
        (member $A $B)) $Y)
    (set-det))


  (= (firstn $_ 0 Nil)  
    (set-det))
  (= (firstn (Cons  $A $B) $N (Cons  $A $C))  
    (set-det)
    (is $M 
      (- $N 1))
    (firstn $B $M $C))
  (= (firstn Nil $_ Nil)  
    (set-det))


  (= (following $X (Cons  $X $AFTER_X) $AFTER_X)  
    (set-det))
  (= (following $X (Cons  $_ $LIST) $AFTER_X)  
    (set-det)
    (following $X $LIST $AFTER_X))
  (= (following $X Nil Nil)  
    (set-det))


  (= (qsort $L0 $L)  
    (qsort $L0 $L Nil)
    (set-det))

  (= (qsort (Cons  $X $L) $R $R0)  
    (partition $L $X $L0 $L1)
    (qsort $L1 $R1 $R0)
    (qsort $L0 $R 
      (Cons  $X $R1)))
  (= (qsort Nil $R $R)  
    (set-det))


  (= (partition (Cons  $X $L) $Y (Cons  $X $L0) $L1)  
    (@=< $X $Y)
    (set-det)
    (partition $L $Y $L0 $L1))
  (= (partition (Cons  $X $L) $Y $L0 (Cons  $X $L1))  
    (set-det)
    (partition $L $Y $L0 $L1))
  (= (partition  () $_ () ())  True)


  (= (remove $X Nil Nil)  
    (set-det))
  (= (remove $X (Cons  $X $B) $C)  
    (set-det)
    (remove $X $B $C))
  (= (remove $X (Cons  $A $B) (Cons  $A $C))  
    (set-det)
    (remove $X $B $C))


  (= (prinlist (:: $A))  
    (write $A)
    (set-det))
  (= (prinlist (Cons  $A $B))  
    (set-det)
    (write $A)
    (write ' v ')
    (prinlist $B))
  (= (prinlist Nil)  
    (write '===> Nothing to print')
    (set-det))


  (= (min (Cons  $X (Cons  $Y $T)) $Z)  
    (@=< $X $Y)
    (set-det)
    (min 
      (Cons  $X $T) $Z))
  (= (min (Cons  $X (Cons  $Y $T)) $Z)  
    (set-det)
    (min 
      (Cons  $Y $T) $Z))
  (= (min (:: $X) $X)  
    (set-det))


  (= (max (Cons  $X (Cons  $Y $T)) $Z)  
    (@>= $X $Y)
    (set-det)
    (max 
      (Cons  $X $T) $Z))
  (= (max (Cons  $X (Cons  $Y $T)) $Z)  
    (set-det)
    (max 
      (Cons  $Y $T) $Z))
  (= (max (:: $X) $X)  
    (set-det))


  (= (findset $X $G $L)  
    (findall $X $G $ZWERG)
    (sort $ZWERG $L))


  (= (help)  
    (nl)
    (nl)
    (write '===> AQ-PROLOG')
    (nl)
    (write '===> Load data with command: data(fn)')
    (nl)
    (write '===> Show data with command: listdata')
    (nl)
    (write '===> Start AQ  with command: start')
    (nl)
    (nl))
; /******************************************************************/
; /* This is adopted from the Clocksin/Mellish definition of */
; /* 'bagof', if you're local PROLOG system does not know 'findall' */
; /* you should use this definition. AQ is ensured to work correctly*/
; /* with this definition. In the case that your local MeTTa */
; /* dialect has 'findall' as built-in and AQ will give no result, */
; /* it is likely that the problem depends on the 'findall' */
; /* definition. You should rename the 'findalls' of AQ and try it */
; /* again. */
; /******************************************************************/
; /* findall(X,G,_) :- */
; /* asserta(yk_found(mark)), call(G), */
; /* asserta(yk_found(X)), fail . */
; /* findall(_,_,L) :- */
; /* yk_collect_found(L) . */
; /* */
; /* yk_collect_found([X|L]) :- */
; /* yk_getnext(X), yk_collect_found(L) . */
; /* yk_collect_found(nil) . */
; /* */
; /* yk_getnext(X) :- */
; /* retract(yk_found(X)), !, not (X == mark) . */
; /******************************************************************/


  !(help *)

