;                                           (convert_to_metta_file  dctg_utils $_462940 sre_dna/dctg_utils.pl sre_dna/dctg_utils.metta)

;
; Misc DCTG utilities.
;
; B. Ross
;
; January 1999

;
; Pretty-printer...


  (= (prettyprint $Tree)  
    (pretty $Tree 0)
    (set-det))


  (= (pretty (node $Name $Kids $ID) $Tab)  
    (is $T 
      (* $Tab 4))
    (tab $T)
    (writel2 (:: $Name  (# $ID ',d ' $Tab ) nl))
    (is $Tab2 
      (+ $Tab 1))
    (prettykids $Kids $Tab2)
    (set-det))
  (= (pretty $Value $Tab)  
    (is $T 
      (* $Tab 4))
    (tab $T)
    (writel (:: $Value nl))
    (set-det))


  (= (prettykids Nil $_)  
    (set-det))
  (= (prettykids (Cons  $Node $Rest) $Tab)  
    (pretty $Node $Tab)
    (prettykids $Rest $Tab)
    (set-det))

;
; DCTG tree depth measurer...


  (= (tree-depth (node $_ $Kids $_) $D)  
    (tree-depth-kids $Kids $D2)
    (is $D 
      (+ $D2 1))
    (set-det))
  (= (tree-depth $_ 1)  
    (set-det))


  (= (tree-depth-kids Nil 0)  
    (set-det))
  (= (tree-depth-kids (Cons  $Node $Rest) $D)  
    (tree-depth $Node $D2)
    (tree-depth-kids $Rest $D3)
    (is $D 
      (max $D2 $D3))
    (set-det))

;
; listprint converts tree to list, using DCTG verification


  (= (listprint $Tree)  
    (user-args-P $UserArgs)
    (verification $Tree $UserArgs $List)
    (writel2 $List)
    (nl)
    (set-det))


