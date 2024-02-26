;                                           (convert-to-metta-file  object $_430040 index/object.pl index/object.metta)

  (= (diff $Tuple1 $Tuple2 $Diff)  
    (diff $Tuple1 $Tuple2 $Rel $Equal $Diff))
; /* object level <--> meta level */
; ; old diff

  (= (diff $Tuple1 $Tuple2 $Rel $Equal $Diff)  
    (=.. $Tuple1 
      (Cons  $Rel $Values1))
    (=.. $Tuple2 
      (Cons  $Rel $Values2))
    (rel $Rel $AttrList)
    (diff1 $Values1 $Values2 $AttrList $Equal $Diff))
; ; new diff (24-2-93)


  (= (diff1  () () () () ())  True)			;
; new diff1 (24-2-93)
  (= (diff1 (Cons  $V $Values1) (Cons  $V $Values2) (Cons  $A $AttrList) (Cons  $A $Equal) $Diff)  
    (diff1 $Values1 $Values2 $AttrList $Equal $Diff))
  (= (diff1 (Cons  $V1 $Values1) (Cons  $V2 $Values2) (Cons  $A $AttrList) $Equal (Cons  $A $Diff))  
    (\= $V1 $V2)
    (diff1 $Values1 $Values2 $AttrList $Equal $Diff))


  (= (values  $_ () () $_)  True)
  (= (values $Rel (Cons  $Attr $AttrList) (Cons  $Value $ValueList) $Tuple)  
    (value $Rel $Attr $Value $Tuple)
    (values $Rel $AttrList $ValueList $Tuple))


  (= (value $Rel $Attr $Value $Tuple)  
    (rel $Rel $AttrList)
    (al2vl $Attr $AttrList $Value $ValueList)
    (=.. $Tuple 
      (Cons  $Rel $ValueList)))


  (= (al2vl  $_ () $_ ())  True)
  (= (al2vl $Attr (Cons  $Attr $AttrList) $Value (Cons  $Value $ValueList))  
    (al2vl $Attr $AttrList $Value $ValueList))
  (= (al2vl $Attr (Cons  $A $AttrList) $Value (Cons  $V $ValueList))  
    (or 
      (\= $Attr $A) 
      (\= $Value $V))
    (al2vl $Attr $AttrList $Value $ValueList))

