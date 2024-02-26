;                                           (convert-to-metta-file  idt1 $_41182 idt/idt1.pl idt/idt1.metta)

  !(dynamic (/ node 3))
; /******************************************************************/
; /* IDT.PRO Last modification: Wed Feb 9 14:20:27 1994 */
; /* Torgos ID3-like system based on the gain-ratio measure */
; /******************************************************************/
; ; ; Copyright (c) 1989 Luis Torgo ; ; This program is free software; you can redistribute it and/or ; modify it under the terms of the GNU General Public License ; Version 1 as published by the Free Software Foundation. ; ; This program is distributed in the hope that it will be useful, ; but WITHOUT ANY WARRANTY; without even the implied warranty of ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the ; GNU General Public License for more details. ; ; You should have received a copy of the GNU General Public ; License along with this program; if not, write to the Free ; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, ; USA. ;
; /******************************************************************/
; /* impl. by : Luis Torgo, Laboratorio Inteligencia Artificial */
; /* e Ciencas de Computacao, */
; /* Universidade do Porto, */
; /* Rua Campo Alegre 823, */
; /* 4100 Porto, */
; /* Portugal */
; /* 1989 */
; /* */
; /* Thomas Hoppe */
; /* Mommsenstr. 50 */
; /* D-10629 Berlin */
; /* F.R.G. */
; /* E-Mail: hoppet@cs.tu-berlin.de */
; /* 1990 */
; /* */
; /* reference : Learning Efficient Classification Procedures */
; /* and Their Application to Chess End Games, */
; /* Quinlan, J. R., in: Machine Learning, */
; /* Michalski, R.S., Carbonell, J.G., Mitchell, T.M.*/
; /* (eds.), Tioga Publishing Company, Palo Alto, */
; /* 1983. */
; /* */
; /* Induction of Decision Trees, J. Ross Quinlan */
; /* Machine Learning 1(1), 81-106, 1986 */
; /* */
; /* call : idt */
; /* */
; /******************************************************************/
; ; TH: Sat May 29 16:41:25 1993 - made some minor changes ; Mon Feb 7 22:12:43 1994 - for portability with Sicstus input ; files are no longer consulted. ; Wed Feb 9 13:42:44 1994 - log computations are modified for ; compatibility with Quintus-MeTTa ; - Zero-division bug in common ; calculations removed
; /******************************************************************/
; /* SWI-, YAP-, C- and M-MeTTa specific declaration of dynamical */
; /* clauses. */
; /******************************************************************/
  !(dynamic (/ decision-tree 1))
  !(dynamic (/ example 3))
  !(dynamic (/ attributes 1))
  !(dynamic (/ classes 1))
  !(dynamic (/ current-node 1))
;                                           (error
;                                             (syntax-error  operator_expected)
;                                             (file  idt/idt1.pl 69 17 3862))

  !(dynamic (/ found 1))

;
; Comment this out if you use Quintus MeTTa

  (= (log $X $Y)  
    (is $Y 
      (log $X)))


  (= idt  
    (repeat nl 
      (write 'Which file to use ? ') 
      (read $FileName) nl initialize-kb 
      (readfile $FileName) build-decision-tree show-decision-tree nl 
      (write 'Quit (y/n) ? ') 
      (read y)))
; /******************************************************************/
; /* Quintus-MeTTa specific declaration. */
; /******************************************************************/
; ; :- ensure_loaded(library(math)). ; :- ensure_loaded(library(basics)).
; /******************************************************************/
; /* */
; /* call : idt */
; /* */
; /* side effects: assertz and retracts clauses */
; /* */
; /******************************************************************/
; /* idt reads a filename from the terminal, initializes the know- */
; /* base, consults the correponding file builds a decision tree */
; /* and displays the tree. */
; /* The program assertz the following predicates, which must be */
; /* declared as dynamic in some MeTTa dialects: */
; /* node/3, decision_tree/1, example/3, attributes/1, classes/1, */
; /* current_node/1 and table/3. */
; /******************************************************************/


  (= initialize-kb  
    (abolish node 3)
    (abolish decision-tree 1)
    (abolish example 3)
    (abolish attributes 1)
    (abolish classes 1)
    (abolish current-node 1)
    (set-det))


  (= (readfile $FileName)  
    ( (concat $FileName .pl $File) 
      (see $File) repeat 
      (read $Term) 
      (det-if-then-else 
        (= $Term end-of-file) 
        (, 
          (set-det) seen) 
        (, 
          (add-atom  &self $Term) fail))))


  (= build-decision-tree  
    ( (generate-node-id $_) 
      (==  
        (= 
          (attributes  $Attributes) true) 
        (get-atoms  &self)) 
      (findbag $Ex 
        (==  
          (= 
            (example  $Ex $_ $_) true) 
          (get-atoms  &self)) $Exs) 
      (idt $Exs $Attributes $Node) 
      (add-atom  &self 
        (decision-tree  $Node)) 
      (set-det)))


  (= (generate-node-id $Y)  
    ( (==  
        (= 
          (current-node  $X) true) 
        (get-atoms  &self)) 
      (set-det) 
      (remove-atom  &self 
        (current-node  $X)) 
      (is $Y 
        (+ $X 1)) 
      (add-atom  &self 
        (current-node  $Y))))
  (= (generate-node-id 0)  
    (add-atom  &self 
      (current-node  0)))


  (= (idt  () $_ ())  True)
; /******************************************************************/
; /* */
; /* call : idt(+Examples,+Attributes,-Class) */
; /* */
; /* arguments : Examples = List of Examples */
; /* Attributes = List of Attributes */
; /* Class = Node ID of Class or leaf(Class) */
; /* */
; /******************************************************************/
; /* IDT determines an attribute-value pair which best splits the */
; /* examples according to the information-theoretical 'gain-ration'*/
; /* measure. The attribute-value pair is deleted from the set of */
; /* all attribute-value pairs and the process of generating a sub- */
; /* decision tree is called recursively with the according to the */
; /* attribute-value pair splitted examples. The recursion */
; /* terminates either if there is no more example to process or if */
; /* all examples belong to the same class. In the last case */
; /* leaf(Class) is returned insteed of the SubtreeIDs. */
; /* In the end for every generated subtree an ID is generated and */
; /* the tree structure is asserted in the database. */
; /******************************************************************/
  (= (idt $Exs $_ (:: (leaf $Class)))  
    (termination-criterion $Exs $Class))
  (= (idt $Exs $Attributes $ID)  
    ( (get-best-attribute $Attributes $Exs $BestAttribute) 
      (split-values $BestAttribute $Exs $DividedValues) 
      (delete $BestAttribute $Attributes $NewAttributes) 
      (generate-subtrees $DividedValues $NewAttributes $SubtreeIDs) 
      (generate-node-id $ID) 
      (add-atom  &self 
        (node  $ID $BestAttribute $SubtreeIDs))))


  (= (termination-criterion (Cons  $Ex $Exs) $Class)  
    ( (==  
        (= 
          (example  $Ex $Class $_) true) 
        (get-atoms  &self)) 
      (set-det) 
      (all-in-same-class $Exs $Class)))


  (= (all-in-same-class  () $_)  True)
  (= (all-in-same-class (Cons  $Ex $Exs) $C)  
    ( (==  
        (= 
          (example  $Ex $C $_) true) 
        (get-atoms  &self)) 
      (set-det) 
      (all-in-same-class $Exs $C)))


  (= (get-best-attribute $Attributes $Exs $BestAttribute)  
    (construct-contingency-table $Attributes $Exs)
    (common-calculations $MC $N)
    (calculate-parameter-classification $Attributes $MC $N $Values)
    (get-best $Attributes $Values $BestAttribute))


  (= (construct-contingency-table $Attributes $Exs)  
    ( (==  
        (= 
          (classes  $Lc) true) 
        (get-atoms  &self)) 
      (length $Lc $NroColTab) 
      (abolish table 3) 
      (create-list-of-zeros $NroColTab $List) 
      (initialize-contingency-tables $Attributes $List) 
      (construct-contingency-tables $Attributes $Exs)))


  (= (initialize-contingency-tables  () $_)  True)
  (= (initialize-contingency-tables (Cons  $A $As) $List)  
    ( (add-atom  &self 
        (table  $A () $List)) (initialize-contingency-tables $As $List)))


  (= (create-list-of-zeros  0 ())  True)
  (= (create-list-of-zeros $N (Cons  0 $R))  
    (> $N 0)
    (is $N1 
      (- $N 1))
    (create-list-of-zeros $N1 $R))


  (= (construct-contingency-tables  () $_)  True)
  (= (construct-contingency-tables (Cons  $Attribute $Attributes) $ExampleList)  
    (contingency-table $Attribute $ExampleList)
    (set-det)
    (construct-contingency-tables $Attributes $ExampleList))


  (= (contingency-table  $_ ())  True)
  (= (contingency-table $Attribute (Cons  $Ex $Exs))  
    (value $Attribute $Ex $V)
    (position-of-class $Ex $Pc)
    (update-table $Attribute $V $Pc)
    (set-det)
    (contingency-table $Attribute $Exs))


  (= (value $A (Cons  (= $A $V) $_) $V)  
    (set-det))
  (= (value $A (Cons  $_ $Sels) $V)  
    (value $A $Sels $V))
  (= (value $A $No $V)  
    ( (==  
        (= 
          (example  $No $_ $Ex) true) 
        (get-atoms  &self)) (value $A $Ex $V)))


  (= (position-of-class $Ex $Pc)  
    ( (==  
        (= 
          (example  $Ex $C $_) true) 
        (get-atoms  &self)) 
      (==  
        (= 
          (classes  $Classes) true) 
        (get-atoms  &self)) 
      (position $C $Classes $Pc)))


  (= (position $X $L $P)  
    (position $X 1 $L $P))

  (= (position  $X $P ([|]  $X $_) $P)  True)
  (= (position $X $N (Cons  $_ $R) $P)  
    (is $N1 
      (+ $N 1))
    (position $X $N1 $R $P))


  (= (update-table $Attribute $V $Pc)  
    ( (remove-atom  &self 
        (table  $Attribute $TabLines $TotClass)) 
      (modify-table $TabLines $V $Pc $NewLines) 
      (increment-position-list 1 $Pc $TotClass $NewTotal) 
      (add-atom  &self 
        (table  $Attribute $NewLines $NewTotal))))


  (= (modify-table Nil $V $Pc (:: (, $V $Values 1)))  
    ( (==  
        (= 
          (classes  $Classes) true) 
        (get-atoms  &self)) 
      (length $Classes $NoOfColums) 
      (create-list-of-zeros $NoOfColums $L) 
      (increment-position-list 1 $Pc $L $Values)))
  (= (modify-table (Cons  (, $V $Nums $Tot) $Rest) $V $Pc (Cons  (, $V $NewNums $NewTot) $Rest))  
    (is $NewTot 
      (+ $Tot 1))
    (increment-position-list 1 $Pc $Nums $NewNums))
  (= (modify-table (Cons  $X $Rest1) $V $Pc (Cons  $X $Rest2))  
    (modify-table $Rest1 $V $Pc $Rest2))


  (= (increment-position-list $N $N (Cons  $X $R) (Cons  $Y $R))  
    (is $Y 
      (+ $X 1)))
  (= (increment-position-list $N1 $N (Cons  $X $R1) (Cons  $X $R2))  
    (is $N2 
      (+ $N1 1))
    (increment-position-list $N2 $N $R1 $R2))


  (= (common-calculations $MC $N)  
    ( (==  
        (= 
          (table  $_ $_ $Xjs) true) 
        (get-atoms  &self)) (common-calculations $Xjs 0 0 $MC $N)))

  (= (common-calculations Nil $TotalSum $N $MC $N)  
    (log $N $NLog)
    (is $MC 
      (* 
        (/ -1 $N) 
        (- $TotalSum 
          (* $N $NLog)))))
  (= (common-calculations (:: 0) $S $N $S $N)  
    (set-det))
  (= (common-calculations (Cons  $Xj $Xjs) $Ac1 $Ac2 $MC $N)  
    (log $Xj $XjLog)
    (is $NAc1 
      (+ $Ac1 
        (* $Xj $XjLog)))
    (is $NAc2 
      (+ $Ac2 $Xj))
    (common-calculations $Xjs $NAc1 $NAc2 $MC $N))


  (= (calculate-parameter-classification  () $_ $_ ())  True)
  (= (calculate-parameter-classification (Cons  $A $As) $MC $N (Cons  $V $Vs))  
    (gain-ratio $A $MC $N $V)
    (calculate-parameter-classification $As $MC $N $Vs))


  (= (gain-ratio $A $MC $N $GR)  
    ( (==  
        (= 
          (table  $A $Lines $_) true) 
        (get-atoms  &self)) 
      (calculate-factors-B-and-IV $Lines $N 0 0 $B $IV) 
      (is $IM 
        (- $MC $B)) 
      (det-if-then-else 
        (> $IV 0) 
        (is $GR 
          (/ $IM $IV)) 
        (= $GR 1))))


  (= (calculate-factors-B-and-IV Nil $N $Sum1 $Sum2 $B $IV)  
    (log $N $NLog)
    (is $B 
      (* 
        (/ -1 $N) 
        (- $Sum1 $Sum2)))
    (is $IV 
      (* 
        (/ -1 $N) 
        (- $Sum2 
          (* $N $NLog)))))
  (= (calculate-factors-B-and-IV (Cons  (, $_ $L $TotL) $Rest) $N $Ac1 $Ac2 $B $IV)  
    (sum-of-lines $L 0 $SL)
    (log $TotL $TotLog)
    (is $NAc1 
      (+ $Ac1 $SL))
    (is $NAc2 
      (+ $Ac2 
        (* $TotL $TotLog)))
    (calculate-factors-B-and-IV $Rest $N $NAc1 $NAc2 $B $IV))


  (= (sum-of-lines  () $X $X)  True)
  (= (sum-of-lines (Cons  0 $Ns) $Ac $Tot)  
    (sum-of-lines $Ns $Ac $Tot))
  (= (sum-of-lines (Cons  $N $Ns) $Ac $Tot)  
    (log $N $NLog)
    (is $Nac 
      (+ $Ac 
        (* $N $NLog)))
    (sum-of-lines $Ns $Nac $Tot))


  (= (get-best (Cons  $A $As) (Cons  $V $Vs) $Result)  
    (best-value $As $Vs 
      (, $A $V) $Result))


  (= (best-value  () () (,  $A $_) $A)  True)
  (= (best-value (Cons  $A $As) (Cons  $V $Vs) (, $_ $TV) $Result)  
    (> $V $TV)
    (best-value $As $Vs 
      (, $A $V) $Result))
  (= (best-value (Cons  $_ $As) (Cons  $_ $Vs) (, $TA $TV) $Result)  
    (best-value $As $Vs 
      (, $TA $TV) $Result))


  (= (split-values $Attribute $Exs $Result)  
    (get-values $Attribute $Exs $Values)
    (split-examples $Attribute $Values $Exs $Result))


  (= (get-values $Attribute $Exs $Vals)  
    (findbag $V 
      (, 
        (member $Ex $Exs) 
        (value $Attribute $Ex $V)) $Vs)
    (remove-duplicates $Vs $Vals))


  (= (split-examples  $_ ($V) $Exs ((,  $V $Exs)))  True)
  (= (split-examples $A (Cons  $V $Vs) $Exs (Cons  (, $V $VExs) $Rest))  
    (findbag $Ex 
      (, 
        (member $Ex $Exs) 
        (value $A $Ex $V)) $VExs)
    (difference $VExs $Exs $RestEx)
    (split-examples $A $Vs $RestEx $Rest))


  (= (generate-subtrees  () $_ ())  True)
  (= (generate-subtrees (Cons  (, $Value $Exs) $Rest1) $Attributes (Cons  (, $Value $Id) $Rest2))  
    (idt $Exs $Attributes $Id)
    (set-det)
    (generate-subtrees $Rest1 $Attributes $Rest2))


  (= show-decision-tree  
    (nl 
      (==  
        (= 
          (decision-tree  $Node) true) 
        (get-atoms  &self)) 
      (show-subtree $Node 0) 
      (set-det)))
; /******************************************************************/
; /* */
; /* call : show_decision_tree */
; /* */
; /******************************************************************/
; /* A simple pretty-print procedure for displaying decision trees. */
; /* In steed of this procedure, we can also generate rules from the*/
; /* decision tree by traversing every path in the tree until a */
; /* leaf node was reached and collecting the attribute-value pairs */
; /* of that path. Then the leaf node forms the head of a Horn- */
; /* formula and the set of attribute-value pairs of the path forms */
; /* the body of the clause. */
; /******************************************************************/


  (= (show-subtree $NodeNo $Indent)  
    ( (==  
        (= 
          (node  $NodeNo $Attribute $SubtreeList) true) 
        (get-atoms  &self)) (show-subtrees $SubtreeList $Attribute $Indent)))
	

  (= (show-subtrees Nil $_ $_)  nl)
  (= (show-subtrees (Cons  (, $Value (leaf $X)) $Brothers) $Attribute $Indent)  
    ( (write (= $Attribute $Value)) 
      (write ' ') 
      (write  ==> ) 
      (write (= class $X)) nl 
      (space $Indent) 
      (show-subtrees $Brothers $Attribute $Indent)))	
  (= (show-subtrees (Cons  (, $Value $NodeNo) $Brothers) $Attribute $Indent)  
    (name $Attribute $List1)
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
    (show-subtrees $Brothers $Attribute $Indent))


  (= (space  0)  True)
; /******************************************************************/
; /* Utility predicates */
; /******************************************************************/
  (= (space $N)  
    (> $N 0)
    (write ' ')
    (is $N1 
      (- $N 1))
    (space $N1))


  (= (remove-duplicates  () ())  True)
  (= (remove-duplicates (Cons  $X $Xs) $Ys)  
    (member $X $Xs)
    (remove-duplicates $Xs $Ys))
  (= (remove-duplicates (Cons  $X $Xs) (Cons  $X $Ys))  
    (remove-duplicates $Xs $Ys))

;
; length([],0).
;
; length([L|Ls],N) :-
;
; length(Ls,N1),
;
; N is N1 + 1.


  (= (delete  $X ([|]  $X $Xs) $Xs)  True)
  (= (delete $X (Cons  $Y $Ys) (Cons  $Y $Zs))  
    (delete $X $Ys $Zs))


  (= (difference $L1 $L2 $L3)  
    (findbag $N 
      (, 
        (member $N $L2) 
        (not (member $N $L1))) $L3))


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
                                                              

  (= (getnext $X)  
    ( (remove-atom  &self 
        (found  $X)) 
      (set-det) 
      (not (== $X mark))))                


  (= help  
    ( (write 'Start IDT with command: idt.') nl))


  !(help *)



