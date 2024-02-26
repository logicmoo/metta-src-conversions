;
; 


  !(module flatten 
    (:: 
      (/ flatten-term 7) 
      (/ flatten-term 2) 
      (/ flatten-literal 6) 
      (/ flatten-literal 2) 
      (/ flatten-clause 6) 
      (/ unflatten-clause 2) 
      (/ flatten-clause 2) 
      (/ unflatten-clause 3)))
; 


;
; 

  !(use-module 
    (home div-utils) 
    (:: 
      (/ clist-to-prolog 2) 
      (/ list-to-struct 2)))
; 

  !(use-module-if-exists 
    (library basics) 
    (:: (/ member 2)))
; 

  !(use-module-if-exists 
    (library strings) 
    (:: 
      (/ concat-symbol 3) 
      (/ midstring 6) 
      (/ substring 5)))
; 

  !(use-module-if-exists 
    (library occurs) 
    (:: 
      (/ sub-term 2) 
      (/ contains-var 2)))
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
    (flatten-term $Lin $Lout) 
    (flatten-term $Lin $_ Nil $_ Nil $_ $Lout))
; 


;
; 


;
; 

  (= 
    (flatten-term $Term $Var $S $S $Bg $Bg Nil) 
    ( (member 
        (/ $Var $Term1) $S) 
      (== $Term $Term1) 
      (set-det)))
; 


;
; 

;
; 

  (= 
    (flatten-term $X $X $S $S $Bg $Bg Nil) 
    ( (var $X) (set-det)))
; 


;
; 

  (= 
    (flatten-term Nil $V $S 
      (Cons  
        (/ $V Nil) $S) $Bg 
      (Cons  
        (nil-p Nil) $Bg) 
      (:: (nil-p $V))) 
    (set-det))
; 


;
; 

  (= 
    (flatten-term $A $V $S 
      (Cons  
        (/ $V $A) $S) $Bg 
      (Cons  $B $Bg) 
      (:: $L)) 
    ( (atom $A) 
      (set-det) 
      (concat-atom 
        (:: $A p) - $Functor) 
      (=.. $L 
        (:: $Functor $V)) 
      (=.. $B 
        (:: $Functor $A))))
; 


;
; 

  (= 
    (flatten-term $Int $V $S 
      (Cons  
        (/ $V $Int) $S) $Bg 
      (Cons  $B $Bg) 
      (:: $L)) 
    ( (integer $Int) 
      (set-det) 
      (map-function-to-pred $Int $PredName) 
      (=.. $L 
        (:: $PredName $V)) 
      (=.. $B 
        (:: $PredName $Int))))
; 

        

;
; 

  (= 
    (flatten-term 
      (Cons  $A $B) $V $S $Snew $Bg 
      (Cons  
        (cons-p $A $B 
          (Cons  $A $B)) $Bg2) $Literals) 
    ( (set-det) 
      (flatten-term $A $V1 $S $S1 $Bg $Bg1 $Literals1) 
      (flatten-term $B $V2 $S1 $Snew1 $Bg1 $Bg2 $Literals2) 
      (= $Snew 
        (Cons  
          (/ $V 
            (Cons  $A $B)) $Snew1)) 
      (append $Literals1 $Literals2 $Literals3) 
      (= $Literals 
        (Cons  
          (cons-p $V1 $V2 $V) $Literals3))))
; 



;
; 

  (= 
    (flatten-term $Function $V $S $Snew $Bg 
      (Cons  $BgPredicate $Bg1) $Literals) 
    ( (=.. $Function 
        (Cons  $Functor $Args)) 
      (flatten-args $Args $Vs $S $Snew1 $Bg $Bg1 $Literals1) 
      (= $Snew 
        (Cons  
          (/ $V $Function) $Snew1)) 
      (append $Vs 
        (:: $V) $NewArgs) 
      (concat-atom 
        (:: $Functor p) - $NewFunctor) 
      (=.. $Predicate 
        (Cons  $NewFunctor $NewArgs)) 
      (append $Args 
        (:: $Function) $BgArgs) 
      (=.. $BgPredicate 
        (Cons  $NewFunctor $BgArgs)) 
      (= $Literals 
        (Cons  $Predicate $Literals1))))
; 




  (= 
    (flatten_args  () () $S $S $Bg $Bg ()) True)
; 

  (= 
    (flatten-args 
      (Cons  $A $Args) 
      (Cons  $V $Vars) $S $Snew $Bg $Bg1 $Literals) 
    ( (flatten-term $A $V $S $Snew1 $Bg $Bg2 $L1) 
      (flatten-args $Args $Vars $Snew1 $Snew $Bg2 $Bg1 $L2) 
      (append $L1 $L2 $Literals)))
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
    (flatten-literal $In $Out) 
    (flatten-literal $In Nil $_ Nil $_ $Out))
; 



;
; 


  (= 
    (flatten-literal True $S $S $Bg $Bg Nil) 
    (set-det))
; 


  (= 
    (flatten-literal $Predicate $S $Snew $Bg $Bg1 $Literals) 
    ( (=.. $Predicate 
        (Cons  $Functor $Args)) 
      (flatten-args $Args $Vars $S $Snew $Bg $Bg1 $Literals1) 
      (=.. $NewPredicate 
        (Cons  $Functor $Vars)) 
      (= $Literals 
        (Cons  $NewPredicate $Literals1))))
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
    (flatten-literals 
      (, $A $B) $S $Snew $Bg $Bg1 $Literals) 
    ( (set-det) 
      (flatten-literal $A $S $Snew1 $Bg $Bg2 $Literals1) 
      (flatten-literals $B $Snew1 $Snew $Bg2 $Bg1 $Literals2) 
      (append $Literals1 $Literals2 $Literals)))
; 


  (= 
    (flatten-literals $A $S $Snew $Bg $Bg1 $Literals) 
    (flatten-literal $A $S $Snew $Bg $Bg1 $Literals))
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



  (= 
    (flatten-clause $In $Out) 
    ( (= $In 
        (Cons  
          (with_self  $_ 
            (p)) $_)) 
      (set-det) 
      (clist-to-prolog $In $F) 
      (flatten-clause $F $G) 
      (clist-to-prolog $Out $G) 
      (set-det)))
; 


  (= 
    (flatten-clause $In $Out) 
    ( (flatten-clause $In Nil $_ Nil $_ $Out) (set-det)))
; 


  (= 
    (flatten-clause $Clause $S $Snew $Bg $Bg1 $ClauseOut) 
    ( (=.. $Clause 
        (:: :- $Head $Body)) 
      (=.. $Head 
        (Cons  $Functor $Args)) 
      (flatten-args $Args $Vars $S $Snew1 $Bg $Bg2 $Literals1) 
      (=.. $NewHead 
        (Cons  $Functor $Vars)) 
      (flatten-literals $Body $Snew1 $Snew $Bg2 $Bg1 $Literals2) 
      (append $Literals1 $Literals2 $Literals) 
      (list-to-struct $Literals $StrucLits) 
      (=.. $ClauseOut 
        (:: :- $NewHead $StrucLits))))
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
    (substitute_in_literals  $Var $Term () ()) True)
; 

  (= 
    (substitute-in-literals $Var $Term 
      (Cons  $Lit1 $Lits) 
      (Cons  $Lit1new $Litsnew)) 
    ( (set-det) 
      (substitute-in-literal $Var $Term $Lit1 $Lit1new) 
      (substitute-in-literals $Var $Term $Lits $Litsnew)))
; 



  (= 
    (substitute-in-literal $Var $Term $LiteralIn $LiteralOut) 
    ( (=.. $LiteralIn 
        (Cons  $Functor $Vars)) 
      (substitute-args $Var $Term $Vars $Args) 
      (=.. $LiteralOut 
        (Cons  $Functor $Args))))
; 


;
; 


  (= 
    (substitute-args $Var $Term 
      (Cons  $V $Vs) 
      (Cons  $Term $Args)) 
    ( (== $Var $V) 
      (set-det) 
      (substitute-args $Var $Term $Vs $Args)))
; 


  (= 
    (substitute-args $Var $Term 
      (Cons  $V $Vs) 
      (Cons  $Arg $Args)) 
    ( (contains-var $Var $V) 
      (set-det) 
      (=.. $V 
        (Cons  $Functor $SubVars)) 
      (substitute-args $Var $Term $SubVars $SubArgs) 
      (=.. $Arg 
        (Cons  $Functor $SubArgs)) 
      (substitute-args $Var $Term $Vs $Args)))
; 


  (= 
    (substitute-args $Var $Term 
      (Cons  $V $Vs) 
      (Cons  $V $Args)) 
    (substitute-args $Var $Term $Vs $Args))
; 


  (= 
    (substitute_args  $Var $Term () ()) True)
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
; 

;
; 

;
; 

;
; 



  (= 
    (unflatten-clause 
      (= $Head $Body) 
      (= $Head1 $Body1)) 
    ( (list-to-struct $BodyListIn $Body) 
      (unflatten-clause1 $Head Nil $BodyListIn $Head1 $BodyListOut Nil Nil) 
      (list-to-struct $BodyListOut $Body1) 
      (set-det)))
; 



  (= 
    (unflatten-clause $In $Out) 
    ( (= $In 
        (Cons  
          (with_self  $_ 
            (p)) $_)) 
      (set-det) 
      (clist-to-prolog $In $F) 
      (unflatten-clause $F $G) 
      (clist-to-prolog $Out $G) 
      (set-det)))
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
    (unflatten-clause 
      (= $Head $Body) $Bg 
      (= $Head1 $Body1)) 
    ( (list-to-struct $BodyListIn $Body) 
      (unflatten-clause1 $Head Nil $BodyListIn $Head1 $BodyListOut Nil $Bg) 
      (list-to-struct $BodyListOut $Body1)))
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
    (unflatten-clause1 $HeadIn $BodyIn1 
      (Cons  $Literal $Rest) $HeadOut $BodyOut1 $BodyOut2 $Bg) 
    ( (=.. $Literal 
        (Cons  $PredFunctor $Args)) 
      (map-function-to-pred $Functor $PredFunctor) 
      (set-det) 
      (append $Fargs 
        (:: $Var) $Args) 
      (=.. $Function 
        (Cons  $Functor $Fargs)) 
      (substitute-in-literal $Var $Function $HeadIn $HeadInt) 
      (substitute-in-literals $Var $Function $BodyIn1 $BodyInt1) 
      (substitute-in-literals $Var $Function $Rest $BodyInt2) 
      (unflatten-clause1 $HeadInt $BodyInt1 $BodyInt2 $HeadOut $BodyOut1 $BodyOut2 $Bg)))
; 



  (= 
    (unflatten-clause1 $HeadIn $BodyIn1 
      (Cons  $Literal $Rest) $HeadOut $BodyOut1 $BodyOut2 $Bg) 
    ( (set-det) 
      (append $BodyIn1 
        (:: $Literal) $BodyInt1) 
      (unflatten-clause1 $HeadIn $BodyInt1 $Rest $HeadOut $BodyOut1 $BodyOut2 $Bg)))
; 




  (= 
    (unflatten_clause1  $Head $Body () $Head $Body () $Bg) True)
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
    (map-function-to-pred Nil nil-p) 
    (set-det))
; 
                 ;
; 

  (= 
    (map-function-to-pred . cons-p) 
    (set-det))
; 
               ;
; 

  (= 
    (map-function-to-pred $Integer $PredName) 
    ( (integer $Integer) 
      (var $PredName) 
      (number-chars $Integer $String) 
      (atom-chars $Atom $String) 
      (concat-atom 
        (:: integer $Atom p) - $PredName) 
      (set-det)))
; 

  (= 
    (map-function-to-pred $Integer $PredName) 
    ( (var $Integer) 
      (nonvar $PredName) 
      (midstring $PredName $S integer--p 8 $_ 2) 
      (name $S $List) 
      (number-chars $Integer $List) 
      (integer $Integer) 
      (set-det)))
; 

  (= 
    (map-function-to-pred $FunctionName $PredName) 
    ( (atom $FunctionName) 
      (var $PredName) 
      (concat-atom 
        (:: $FunctionName -p) $PredName) 
      (set-det)))
; 

  (= 
    (map-function-to-pred $FunctionName $PredName) 
    ( (atom $PredName) 
      (var $FunctionName) 
      (midstring $PredName -p $FunctionName $_ 2 0) 
      (set-det)))
; 


