
  !(op 900 xfx :)
; 

  !(op 800 xfx -->)
; 

  !(op 800 xfx ->->)
; 

  !(op 800 xfx ><)
; 

  !(op 800 xfx <)
; 



  (= 
    (init-ICs $Rel $ICs) 
    ( (rel $Rel $AList) 
      (choose-list 'Which dependencies' $AList $AttrList) 
      (bagof0 
        (mvd $Rel Nil 
          (:: $A)) 
        (member $A $AttrList) $IC1) 
      (bagof0 
        (fd $Rel Nil 
          (:: $A)) 
        (member $A $AttrList) $IC2) 
      (append $IC1 $IC2 $ICs)))
; 



  (= 
    (horn 
      (fd $Rel $FromList 
        (:: $To)) 
      (= 
        (= $A1 $A2) 
        ($Tuple1 $Tuple2))) 
    ( (values $Rel $FromList $ValueList $Tuple1) 
      (values $Rel $FromList $ValueList $Tuple2) 
      (value $Rel $To $A1 $Tuple1) 
      (value $Rel $To $A2 $Tuple2)))
; 

  (= 
    (horn 
      (mvd $Rel $FromList $ToList) 
      (= $Tuple1 
        ($Tuple2 $Tuple3))) 
    ( (rel $Rel $AttrList) 
      (listdiff $AttrList $FromList $List) 
      (listdiff $List $ToList $RestList) 
      (values $Rel $FromList $FromValues $Tuple1) 
      (values $Rel $FromList $FromValues $Tuple2) 
      (values $Rel $FromList $FromValues $Tuple3) 
      (values $Rel $ToList $ToValues $Tuple1) 
      (values $Rel $ToList $ToValues $Tuple2) 
      (values $Rel $RestList $RestValues $Tuple1) 
      (values $Rel $RestList $RestValues $Tuple3)))
; 

  (= 
    (horn 
      (join $Rel $R1 $R2) 
      (= $Tuple1 
        ($Tuple2 $Tuple3))) 
    ( (rel $R1 $R1Attrs) 
      (rel $R2 $R2Attrs) 
      (values $Rel $R1Attrs $R1Values $Tuple1) 
      (values $Rel $R2Attrs $R2Values $Tuple1) 
      (values $R1 $R1Attrs $R1Values $Tuple2) 
      (values $R2 $R2Attrs $R2Values $Tuple3)))
; 

  (= 
    (horn 
      (plus $Rel $List) 
      (= $Tuple $Body)) 
    ( (rel $Rel $RelAttrs) 
      (values $Rel $RelAttrs $Values $Tuple) 
      (make-body $List $Values $Body)))
; 

  (= 
    (horn 
      (proc $Rel) $Proc) 
    (proc $Rel $Proc))
; 
	;
; 



  (= 
    (make-body 
      (:: $R) $Values $L) 
    ( (set-det) 
      (rel $R $Attrs) 
      (values $R $Attrs $Values $L)))
; 

  (= 
    (make-body 
      (Cons  $R $Rs) $Values 
      (or $L $Ls)) 
    ( (rel $R $Attrs) 
      (values $R $Attrs $Values $L) 
      (make-body $Rs $Values $Ls)))
; 



  (= 
    (refinements 
      (fd $Rel $From 
        (:: $To)) 
      (:: 
        (+ $T1) 
        (+ $T2)) $ICs) 
    ( (set-det) 
      (diff $T1 $T2 $Diff) 
      (bagof0 
        (fd $Rel 
          (Cons  $Attr $From) 
          (:: $To)) 
        (fd-spec $To $Diff $Attr) $ICs)))
; 

  (= 
    (refinements 
      (mvd $Rel $From $To) 
      (:: 
        (+ $T1) 
        (+ $T2) 
        (- $T3)) $ICs) 
    ( (set-det) 
      (diff $T1 $T3 $D13) 
      (diff $T2 $T3 $D23) 
      (bagof0 
        (mvd $Rel 
          (Cons  $Attr $From) $ST) 
        (mvd-spec $Rel $From $To $D13 $D23 $Attr $ST) $ICs)))
; 

  (= 
    (refinements $IC $Tuples $ICs) 
    ( (display $IC $DisplayIC) 
      (write $DisplayIC) 
      (show-list ' is contradicted by' $Tuples) 
      (ic-spec $IC $ICs)))
; 



  (= 
    (fd-spec $To $Diff $Attr) 
    ( (member $Attr $Diff) (\= $Attr $To)))
; 



  (= 
    (mvd-spec $Rel $From $To $Diff $_ $Attr $ST) 
    ( (member $Attr $Diff) 
      (remove $Attr $To $ST) 
      (\= $ST Nil) 
      (compl 
        (mvd $Rel 
          (Cons  $Attr $From) $ST) 
        (mvd $Rel 
          (Cons  $Attr $From) $SCT)) 
      (\= $SCT Nil)))
; 

  (= 
    (mvd-spec $Rel $From $To $_ $Diff $Attr $ST) 
    ( (member $Attr $Diff) 
      (remove $Attr $To $ST) 
      (\= $ST Nil) 
      (compl 
        (mvd $Rel 
          (Cons  $Attr $From) $ST) 
        (mvd $Rel 
          (Cons  $Attr $From) $SCT)) 
      (\= $SCT Nil)))
; 



  (= 
    (ic-spec $IC 
      (Cons  $R $Rs)) 
    ( (prompt-read refinement $DR) 
      (set-det) 
      (display $R $DR) 
      (ic-spec $IC $Rs)))
; 

  (= 
    (ic_spec  $IC ()) True)
; 



  (= 
    (subsumed 
      (fd $Rel $From1 $To) 
      (fd $Rel $From2 $To)) 
    (subset $From2 $From1))
; 

  (= 
    (subsumed 
      (mvd $Rel $From1 $To) 
      (mvd $Rel $From2 $To)) 
    ( (set-det) (subset $From2 $From1)))
; 

  (= 
    (subsumed 
      (mvd $Rel $From1 $To1) 
      (mvd $Rel $From2 $To2)) 
    ( (subset $From2 $From1) (compl (mvd $Rel $From1 $To1) (mvd $Rel $From1 $To2))))
; 



  (= 
    (compl 
      (mvd $Rel $From $To) 
      (mvd $Rel $From $CTo)) 
    ( (rel $Rel $AttrList) 
      (listdiff $AttrList $From $AL1) 
      (listdiff $AL1 $To $CTo)))
; 



  (= 
    (compress $In $Out) 
    ( (select 
        (fd $Rel $From $To1) $In $In1) 
      (select 
        (fd $Rel $From $To2) $In1 $In2) 
      (set-det) 
      (append $To1 $To2 $To) 
      (compress 
        (Cons  
          (fd $Rel $From $To) $In2) $Out)))
; 

  (= 
    (compress  $ICs $ICs) True)
; 



  (= 
    (dep  
      (fd  $Rel $From $To) fd $Rel $From $To) True)
; 

  (= 
    (dep  
      (mvd  $Rel $From $To) mvd $Rel $From $To) True)
; 



  (= 
    (display 
      (fd $Rel $From $To) 
      (with_self  $Rel 
        (--> $From $To))) 
    (set-det))
; 

  (= 
    (display 
      (mvd $Rel $From $To) 
      (with_self  $Rel 
        (->-> $From $To))) 
    (set-det))
; 

  (= 
    (display 
      (join $Rel $R1 $R2) 
      (>< 
        (= $Rel $R1) $R2)) 
    (set-det))
; 

  (= 
    (display 
      (plus $Rel $List) 
      (= $Rel $List)) 
    (set-det))
; 

  (= 
    (display 
      (proc $Rel) 
      (with_self  
        (calculated *) $Rel)) 
    (set-det))
; 

  (= 
    (display  $X $X) True)
; 



  (= 
    (template all $X) 
    (set-det))
; 

  (= 
    (template fd 
      (fd $Rel $From $To)) 
    (set-det))
; 

  (= 
    (template mvd 
      (mvd $Rel $From $To)) 
    (set-det))
; 

  (= 
    (template join 
      (join $Rel $R1 $R2)) 
    (set-det))
; 

  (= 
    (template plus 
      (plus $Rel $List)) 
    (set-det))
; 

  (= 
    (template proc 
      (proc $Rel)) 
    (set-det))
; 

  (= 
    (template $R $Tuple) 
    ( (rel $R $A) 
      (set-det) 
      (values $R $A $V $Tuple)))
; 

  (= 
    (template  $X $X) True)
; 


