
  !(dynamic (/ node 3))
; 

  !(dynamic (/ decision-tree 1))
; 

  !(dynamic (/ example 3))
; 

  !(dynamic (/ attributes 1))
; 

  !(dynamic (/ classes 1))
; 

  !(dynamic (/ current-node 1))
; 

;                              (error
;                                (syntax_error  operator_expected)
;                                (file  idt/idt1.pl 69 17 3862))

  !(dynamic (/ found 1))
; 


;
; 


  (= 
    (log $X $Y) 
    (is $Y 
      (log $X)))
; 



  (= 
    (idt) 
    ( (repeat) 
      (nl) 
      (write 'Which file to use ? ') 
      (read $FileName) 
      (nl) 
      (initialize-kb) 
      (readfile $FileName) 
      (build-decision-tree) 
      (show-decision-tree) 
      (nl) 
      (write 'Quit (y/n) ? ') 
      (read y)))
; 



  (= 
    (initialize-kb) 
    ( (abolish node 3) 
      (abolish decision-tree 1) 
      (abolish example 3) 
      (abolish attributes 1) 
      (abolish classes 1) 
      (abolish current-node 1) 
      (set-det)))
; 



  (= 
    (readfile $FileName) 
    ( (concat $FileName .pl $File) 
      (see $File) 
      (repeat) 
      (read $Term) 
      (det-if-then-else 
        (= $Term end-of-file) 
        (, 
          (set-det) 
          (seen)) 
        (, 
          (add-symbol  &self $Term) 
          (fail)))))
; 



  (= 
    (build-decision-tree) 
    ( (generate-node-id $_) 
      (get-symbols  &self 
        (= 
          (attributes  $Attributes) true)) 
      (findbag $Ex 
        (get-symbols  &self 
          (= 
            (example  $Ex $_ $_) true)) $Exs) 
      (idt $Exs $Attributes $Node) 
      (add-symbol  &self 
        (decision_tree  $Node)) 
      (set-det)))
; 



  (= 
    (generate-node-id $Y) 
    ( (get-symbols  &self 
        (= 
          (current_node  $X) true)) 
      (set-det) 
      (remove-symbol  &self 
        (current_node  $X)) 
      (is $Y 
        (+ $X 1)) 
      (add-symbol  &self 
        (current_node  $Y))))
; 

  (= 
    (generate-node-id 0) 
    (add-symbol  &self 
      (current_node  0)))
; 



  (= 
    (idt  () $_ ()) True)
; 

  (= 
    (idt $Exs $_ 
      (:: (leaf $Class))) 
    (termination-criterion $Exs $Class))
; 

  (= 
    (idt $Exs $Attributes $ID) 
    ( (get-best-attribute $Attributes $Exs $BestAttribute) 
      (split-values $BestAttribute $Exs $DividedValues) 
      (delete $BestAttribute $Attributes $NewAttributes) 
      (generate-subtrees $DividedValues $NewAttributes $SubtreeIDs) 
      (generate-node-id $ID) 
      (add-symbol  &self 
        (node  $ID $BestAttribute $SubtreeIDs))))
; 



  (= 
    (termination-criterion 
      (Cons  $Ex $Exs) $Class) 
    ( (get-symbols  &self 
        (= 
          (example  $Ex $Class $_) true)) 
      (set-det) 
      (all-in-same-class $Exs $Class)))
; 



  (= 
    (all_in_same_class  () $_) True)
; 

  (= 
    (all-in-same-class 
      (Cons  $Ex $Exs) $C) 
    ( (get-symbols  &self 
        (= 
          (example  $Ex $C $_) true)) 
      (set-det) 
      (all-in-same-class $Exs $C)))
; 



  (= 
    (get-best-attribute $Attributes $Exs $BestAttribute) 
    ( (construct-contingency-table $Attributes $Exs) 
      (common-calculations $MC $N) 
      (calculate-parameter-classification $Attributes $MC $N $Values) 
      (get-best $Attributes $Values $BestAttribute)))
; 



  (= 
    (construct-contingency-table $Attributes $Exs) 
    ( (get-symbols  &self 
        (= 
          (classes  $Lc) true)) 
      (length $Lc $NroColTab) 
      (abolish table 3) 
      (create-list-of-zeros $NroColTab $List) 
      (initialize-contingency-tables $Attributes $List) 
      (construct-contingency-tables $Attributes $Exs)))
; 



  (= 
    (initialize_contingency_tables  () $_) True)
; 

  (= 
    (initialize-contingency-tables 
      (Cons  $A $As) $List) 
    ( (add-symbol  &self 
        (table  $A () $List)) (initialize-contingency-tables $As $List)))
; 



  (= 
    (create_list_of_zeros  0 ()) True)
; 

  (= 
    (create-list-of-zeros $N 
      (Cons  0 $R)) 
    ( (> $N 0) 
      (is $N1 
        (- $N 1)) 
      (create-list-of-zeros $N1 $R)))
; 



  (= 
    (construct_contingency_tables  () $_) True)
; 

  (= 
    (construct-contingency-tables 
      (Cons  $Attribute $Attributes) $ExampleList) 
    ( (contingency-table $Attribute $ExampleList) 
      (set-det) 
      (construct-contingency-tables $Attributes $ExampleList)))
; 



  (= 
    (contingency_table  $_ ()) True)
; 

  (= 
    (contingency-table $Attribute 
      (Cons  $Ex $Exs)) 
    ( (value $Attribute $Ex $V) 
      (position-of-class $Ex $Pc) 
      (update-table $Attribute $V $Pc) 
      (set-det) 
      (contingency-table $Attribute $Exs)))
; 



  (= 
    (value $A 
      (Cons  
        (= $A $V) $_) $V) 
    (set-det))
; 

  (= 
    (value $A 
      (Cons  $_ $Sels) $V) 
    (value $A $Sels $V))
; 

  (= 
    (value $A $No $V) 
    ( (get-symbols  &self 
        (= 
          (example  $No $_ $Ex) true)) (value $A $Ex $V)))
; 



  (= 
    (position-of-class $Ex $Pc) 
    ( (get-symbols  &self 
        (= 
          (example  $Ex $C $_) true)) 
      (get-symbols  &self 
        (= 
          (classes  $Classes) true)) 
      (position $C $Classes $Pc)))
; 



  (= 
    (position $X $L $P) 
    (position $X 1 $L $P))
; 


  (= 
    (position  $X $P 
      (Cons  $X $_) $P) True)
; 

  (= 
    (position $X $N 
      (Cons  $_ $R) $P) 
    ( (is $N1 
        (+ $N 1)) (position $X $N1 $R $P)))
; 



  (= 
    (update-table $Attribute $V $Pc) 
    ( (remove-symbol  &self 
        (table  $Attribute $TabLines $TotClass)) 
      (modify-table $TabLines $V $Pc $NewLines) 
      (increment-position-list 1 $Pc $TotClass $NewTotal) 
      (add-symbol  &self 
        (table  $Attribute $NewLines $NewTotal))))
; 



  (= 
    (modify-table Nil $V $Pc 
      (:: (, $V $Values 1))) 
    ( (get-symbols  &self 
        (= 
          (classes  $Classes) true)) 
      (length $Classes $NoOfColums) 
      (create-list-of-zeros $NoOfColums $L) 
      (increment-position-list 1 $Pc $L $Values)))
; 

  (= 
    (modify-table 
      (Cons  
        (, $V $Nums $Tot) $Rest) $V $Pc 
      (Cons  
        (, $V $NewNums $NewTot) $Rest)) 
    ( (is $NewTot 
        (+ $Tot 1)) (increment-position-list 1 $Pc $Nums $NewNums)))
; 

  (= 
    (modify-table 
      (Cons  $X $Rest1) $V $Pc 
      (Cons  $X $Rest2)) 
    (modify-table $Rest1 $V $Pc $Rest2))
; 



  (= 
    (increment-position-list $N $N 
      (Cons  $X $R) 
      (Cons  $Y $R)) 
    (is $Y 
      (+ $X 1)))
; 

  (= 
    (increment-position-list $N1 $N 
      (Cons  $X $R1) 
      (Cons  $X $R2)) 
    ( (is $N2 
        (+ $N1 1)) (increment-position-list $N2 $N $R1 $R2)))
; 



  (= 
    (common-calculations $MC $N) 
    ( (get-symbols  &self 
        (= 
          (table  $_ $_ $Xjs) true)) (common-calculations $Xjs 0 0 $MC $N)))
; 


  (= 
    (common-calculations Nil $TotalSum $N $MC $N) 
    ( (log $N $NLog) (is $MC (* (/ -1 $N) (- $TotalSum (* $N $NLog))))))
; 

  (= 
    (common-calculations 
      (:: 0) $S $N $S $N) 
    (set-det))
; 

  (= 
    (common-calculations 
      (Cons  $Xj $Xjs) $Ac1 $Ac2 $MC $N) 
    ( (log $Xj $XjLog) 
      (is $NAc1 
        (+ $Ac1 
          (* $Xj $XjLog))) 
      (is $NAc2 
        (+ $Ac2 $Xj)) 
      (common-calculations $Xjs $NAc1 $NAc2 $MC $N)))
; 



  (= 
    (calculate_parameter_classification  () $_ $_ ()) True)
; 

  (= 
    (calculate-parameter-classification 
      (Cons  $A $As) $MC $N 
      (Cons  $V $Vs)) 
    ( (gain-ratio $A $MC $N $V) (calculate-parameter-classification $As $MC $N $Vs)))
; 



  (= 
    (gain-ratio $A $MC $N $GR) 
    ( (get-symbols  &self 
        (= 
          (table  $A $Lines $_) true)) 
      (calculate-factors-B-and-IV $Lines $N 0 0 $B $IV) 
      (is $IM 
        (- $MC $B)) 
      (det-if-then-else 
        (> $IV 0) 
        (is $GR 
          (/ $IM $IV)) 
        (= $GR 1))))
; 



  (= 
    (calculate-factors-B-and-IV Nil $N $Sum1 $Sum2 $B $IV) 
    ( (log $N $NLog) 
      (is $B 
        (* 
          (/ -1 $N) 
          (- $Sum1 $Sum2))) 
      (is $IV 
        (* 
          (/ -1 $N) 
          (- $Sum2 
            (* $N $NLog))))))
; 

  (= 
    (calculate-factors-B-and-IV 
      (Cons  
        (, $_ $L $TotL) $Rest) $N $Ac1 $Ac2 $B $IV) 
    ( (sum-of-lines $L 0 $SL) 
      (log $TotL $TotLog) 
      (is $NAc1 
        (+ $Ac1 $SL)) 
      (is $NAc2 
        (+ $Ac2 
          (* $TotL $TotLog))) 
      (calculate-factors-B-and-IV $Rest $N $NAc1 $NAc2 $B $IV)))
; 



  (= 
    (sum_of_lines  () $X $X) True)
; 

  (= 
    (sum-of-lines 
      (Cons  0 $Ns) $Ac $Tot) 
    (sum-of-lines $Ns $Ac $Tot))
; 

  (= 
    (sum-of-lines 
      (Cons  $N $Ns) $Ac $Tot) 
    ( (log $N $NLog) 
      (is $Nac 
        (+ $Ac 
          (* $N $NLog))) 
      (sum-of-lines $Ns $Nac $Tot)))
; 



  (= 
    (get-best 
      (Cons  $A $As) 
      (Cons  $V $Vs) $Result) 
    (best-value $As $Vs 
      (, $A $V) $Result))
; 



  (= 
    (best_value  () () 
      (,  $A $_) $A) True)
; 

  (= 
    (best-value 
      (Cons  $A $As) 
      (Cons  $V $Vs) 
      (, $_ $TV) $Result) 
    ( (> $V $TV) (best-value $As $Vs (, $A $V) $Result)))
; 

  (= 
    (best-value 
      (Cons  $_ $As) 
      (Cons  $_ $Vs) 
      (, $TA $TV) $Result) 
    (best-value $As $Vs 
      (, $TA $TV) $Result))
; 



  (= 
    (split-values $Attribute $Exs $Result) 
    ( (get-values $Attribute $Exs $Values) (split-examples $Attribute $Values $Exs $Result)))
; 



  (= 
    (get-values $Attribute $Exs $Vals) 
    ( (findbag $V 
        (, 
          (member $Ex $Exs) 
          (value $Attribute $Ex $V)) $Vs) (remove-duplicates $Vs $Vals)))
; 



  (= 
    (split_examples  $_ 
      ($V) $Exs 
      ( (,  $V $Exs))) True)
; 

  (= 
    (split-examples $A 
      (Cons  $V $Vs) $Exs 
      (Cons  
        (, $V $VExs) $Rest)) 
    ( (findbag $Ex 
        (, 
          (member $Ex $Exs) 
          (value $A $Ex $V)) $VExs) 
      (difference $VExs $Exs $RestEx) 
      (split-examples $A $Vs $RestEx $Rest)))
; 



  (= 
    (generate_subtrees  () $_ ()) True)
; 

  (= 
    (generate-subtrees 
      (Cons  
        (, $Value $Exs) $Rest1) $Attributes 
      (Cons  
        (, $Value $Id) $Rest2)) 
    ( (idt $Exs $Attributes $Id) 
      (set-det) 
      (generate-subtrees $Rest1 $Attributes $Rest2)))
; 



  (= 
    (show-decision-tree) 
    ( (nl) 
      (get-symbols  &self 
        (= 
          (decision_tree  $Node) true)) 
      (show-subtree $Node 0) 
      (set-det)))
; 



  (= 
    (show-subtree $NodeNo $Indent) 
    ( (get-symbols  &self 
        (= 
          (node  $NodeNo $Attribute $SubtreeList) true)) (show-subtrees $SubtreeList $Attribute $Indent)))
; 

	

  (= 
    (show-subtrees Nil $_ $_) 
    (nl))
; 

  (= 
    (show-subtrees 
      (Cons  
        (, $Value 
          (leaf $X)) $Brothers) $Attribute $Indent) 
    ( (write (= $Attribute $Value)) 
      (write ' ') 
      (write  ==> ) 
      (write (= class $X)) 
      (nl) 
      (space $Indent) 
      (show-subtrees $Brothers $Attribute $Indent)))
; 
	
  (= 
    (show-subtrees 
      (Cons  
        (, $Value $NodeNo) $Brothers) $Attribute $Indent) 
    ( (name $Attribute $List1) 
      (length $List1 $N1) 
      (name $Value $List2) 
      (length $List2 $N2) 
      (write (= $Attribute $Value)) 
      (write ' and ') 
      (is $Offset 
        (+ 
          (+ 
            (+ 
              (+ $Indent $N1) 3) $N2) 5)) 
      (show-subtree $NodeNo $Offset) 
      (space $Indent) 
      (show-subtrees $Brothers $Attribute $Indent)))
; 



  (= 
    (space  0) True)
; 

  (= 
    (space $N) 
    ( (> $N 0) 
      (write ' ') 
      (is $N1 
        (- $N 1)) 
      (space $N1)))
; 



  (= 
    (remove_duplicates  () ()) True)
; 

  (= 
    (remove-duplicates 
      (Cons  $X $Xs) $Ys) 
    ( (member $X $Xs) (remove-duplicates $Xs $Ys)))
; 

  (= 
    (remove-duplicates 
      (Cons  $X $Xs) 
      (Cons  $X $Ys)) 
    (remove-duplicates $Xs $Ys))
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
    (delete  $X 
      (Cons  $X $Xs) $Xs) True)
; 

  (= 
    (delete $X 
      (Cons  $Y $Ys) 
      (Cons  $Y $Zs)) 
    (delete $X $Ys $Zs))
; 



  (= 
    (difference $L1 $L2 $L3) 
    (findbag $N 
      (, 
        (member $N $L2) 
        (not (member $N $L1))) $L3))
; 



  (= 
    (findbag $X $G $_) 
    ( (add-symbol  &self 
        (found  mark)) 
      (call $G) 
      (add-symbol  &self 
        (found  $X)) 
      (fail)))
; 
                            
  (= 
    (findbag $_ $_ $L) 
    (collect-found Nil $L))
; 
                                     
                                                              

  (= 
    (collect-found $L $L1) 
    ( (getnext $X) (collect-found (Cons  $X $L) $L1)))
; 
                      
  (= 
    (collect_found  $L $L) True)
; 
                                        
                                                              

  (= 
    (getnext $X) 
    ( (remove-symbol  &self 
        (found  $X)) 
      (set-det) 
      (not (== $X mark))))
; 
                


  (= 
    (help) 
    ( (write 'Start IDT with command: idt.') (nl)))
; 



  !(help *)
; 




