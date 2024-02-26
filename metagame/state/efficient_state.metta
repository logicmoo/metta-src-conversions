;                                           (convert_to_metta_file  efficient_state $_110400 metagame/state/efficient_state.pl metagame/state/efficient_state.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 
;
; efficient_state.pl
;
; 
;
; The naive representation of state just maintains the state as a list
;
; of all the preds true in it.
;
; 
;
; The following routines create an indexed structure for representing game state,
;
; where predicates are indexed according to functor/arity. Thus, when finding which
;
; player is in control, we need not search past descriptors dealing with square occupancy,
;
; etc.
;
; 
;
; Within the entry for each predicate, the preds of that type currently true can
;
; also be impemented in a variety of ways, the most naive of which using a list
;
; representation.
;
; Here we are a bit more efficient, and index first on the functor of the predicate,
;
; and second on the particular arguments of it. The method of indexing on arguments
;
; varies from predicate to predicate. In particular, ON is represented as
;
; an array.
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



  !(my-ensure-loaded (library aux))

;
; ----------------------------------------
;
; The following are defined in sysdev.pl

;
; DYNAMIC_PREDS_FILE(File)

;
; INDEX_PREDS_FILE(File)
;
; ----------------------------------------

;
; ================================================================================
;
; Using state in predicates.
;
; ================================================================================

;
; Efficient representation indexes state as a list of predicate entries,
;
; each of which is individually optimized.

;
; STATE Abstract Data Type.
;
; Supports following operations:
;
; 
;
; new_state(-State) -- State is a new (empty) state, in which nothing is true.
;
; 
;
; is_state(+State) -- True when State is a state.
;
; 
;
; true_in(?Pred,+State) -- Pred is true in State.
;
; 
;
; add_in(+Pred,+StateIn,-StateOut) -- StateOut is like StateIn,
;
; with the addition of Pred (which must be ground).
;
; 
;
; del_in(+Pred,+StateIn,-StateOut) -- StateOut is like StateIn,
;
; except that Pred (which must have been true in StateIn) is
;
; not true in StateOut.


;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; db indexing ADT
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (= (new-state (state $S))  
    (initialize-state Nil $S))

;
; ;; INITIALIZE_STATE(+SIn,-SOut)

  (= (initialize-state $SIn $SOut)  
    (setof 
      (- $P $I) 
      (pred-index $P $I) $Indices)
    (initialize-state-indices $Indices $SIn $SOut))


  (= (initialize_state_indices  () $SIn $SIn)  True)
  (= (initialize-state-indices (Cons  (- $P $I) $Is) $SIn $SOut)  
    (initialize-state-index $P $I $SIn $S1)
    (initialize-state-indices $Is $S1 $SOut))
	

;
; ;; INITIALIZE_STATE_INDEX(Pred,Index,SIn,S1) :-
;
; ;; ensures that all the labels have initially empty lists,
;
; ;; so that we can always be sure there is a list there
;
; ;; when we find a label.

;
; initialize_state_index(_P,_I,SIn,[[]|SIn]).


  (= (initialize-state-index $P $I $SIn (Cons  $Bucket $SIn))  
    (init-bucket $P $Bucket))


  (= (is_state  (state  $_))  True)


  (= (true-in $Pred (state $State))  
    (db-true $Pred $State))


  (= (add-in $Pred (state $SIn) (state $SOut))  
    (db-add $Pred $SIn $SOut))


  (= (del-in $Pred (state $SIn) (state $SOut))  
    (db-del $Pred $SIn $SOut))


;
; Could optimize add/del key, by having
;
; find_bucket return a difference list and the
;
; rest of the list, so we can just replace here.
;
; This would save N traversals, where N is the
;
; pred_index of P (which bucket it is in).


  (= (db-true $P $SIn)  
    (db-key $P $Key)
    (in-key $Key $P $SIn))


  (= (db-add $P $SIn $SOut)  
    (db-key $P $Key)
    (add-key $Key $P $SIn $SOut))

;
; Ensure already inserted before deleting?

  (= (db-del $P $SIn $SOut)  
    (db-key $P $Key)
    (del-key $Key $P $SIn $SOut))


  (= (db-key $P $Key)  
    (pred-index $P $Key))


  (= (in-key $Key $P $SIn)  
    (find-bucket $Key $SIn $Bucket)
    (in-bucket $P $Bucket))


  (= (add-key $Key $P $SIn $SOut)  
    (find-bucket $Key $SIn $Bucket)
    (add-bucket $P $Bucket $Bucket1)
    (set-bucket $Key $SIn $Bucket1 $SOut))


  (= (del-key $Key $P $SIn $SOut)  
    (find-bucket $Key $SIn $Bucket)
    (del-bucket $P $Bucket $Bucket1)
    (set-bucket $Key $SIn $Bucket1 $SOut))


  (= (find-bucket $Index $State $Bucket)  
    (nth-element $Index $State $Bucket))


  (= (set-bucket $Index $SIn $Bucket $SOut)  
    (set-nth-element $Index $SIn $Bucket $SOut))


;
; in_bucket(P,Bucket) :-
;
; member_bag(P,Bucket).

;
; add_bucket(P,Bucket,Bucket1) :-
;
; add_bag(P,Bucket,Bucket1).

;
; del_bucket(P,Bucket,Bucket1) :-
;
; del_bag(P,Bucket,Bucket1).


;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Using nested-term-arrays to represent the board
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
; INIT_BUCKET


  (= (init-bucket (on $_ $_) $Bucket)  
    (set-det)
    (new-game-board $Bucket))
  (= (init_bucket  $_ ())  True)



  (= (empty-row $X $Term)  
    (functor $Term1 row $X)
    (empty-elements $X $Term1 $Term))



  (= (empty-elements 0 $Board $Board)  
    (set-det))
  (= (empty-elements $N $Board1 $Board)  
    (>= $N 1)
    (emptify $N $Board1 $Board2)
    (is $N1 
      (- $N 1))
    (empty-elements $N1 $Board2 $Board))


  (= (emptify $N $Board1 $Board2)  
    (empty-filler $E)
    (arg $N $Board1 $E)
    (= $Board1 $Board2))

;
; For a generic, unspecified state, all squares should
;
; be unbound. A program to set up the board should thus
;
; either add them as empty or not.
;
; 
;
; empty_filler(empty).

  (= (empty_filler  $_)  True)

;
; emptify(N,Board1,Board2) :-
;
; arg(N,Board1,empty),
;
; Board1 = Board2.



  (= (empty-board $X $Y $Term)  
    (functor $Term1 gameboard $Y)
    (empty-rows $Y $X $Term1 $Term))


  (= (empty-rows 0 $X $Board $Board)  
    (set-det))
  (= (empty-rows $N $X $Board1 $Board)  
    (>= $N 1)
    (emptify-row $N $X $Board1 $Board2)
    (is $N1 
      (- $N 1))
    (empty-rows $N1 $X $Board2 $Board))


  (= (emptify-row $N $X $Board1 $Board2)  
    (empty-row $X $Row)
    (arg $N $Board1 $Row)
    (= $Board1 $Board2))


  (= (new-game-board $Board)  
    (current-board-size $X $Y)
    (empty-board $X $Y $Board))


;
; ;; IN_BUCKET


  (= (in-bucket (on $Piece (square $X $Y)) $Board)  
    (set-det)
    (piece-on-square $X $Y $Board $Piece))
; ; path_arg([Y,X],Board,Piece).
  (= (in-bucket $P $Bucket)  
    (member-bag $P $Bucket))


;
; ;; ADD_BUCKET


  (= (add-bucket (on $Piece (square $X $Y)) $A $A1)  
    (set-det)
    (change-piece-on-square $X $Y $Piece $A $A1))
; ; change_path_arg([Y,X],A,A1,Piece).

  (= (add-bucket $P $A $A1)  
    (add-bag $P $A $A1))


  (= (del-bucket (on $Piece $Sq) $E $E1)  
    (set-det)
    (= $E $E1))
; ; Don't need to do anything here, since will always
; ; set it to something on next step!
  (= (del-bucket $P $E $E1)  
    (set-det)
    (del-bag $P $E $E1))


;
; ----------------------------------------------------------------------
;
; Low-level board accessing
;
; ----------------------------------------------------------------------
;
; :- assert(library_directory('/usr/groups/ailanguages/quintus3.1.1/generic/qplib3.1.1/library/')).

;
; :- my_use_module(library(changearg)).
;
; :- my_use_module(library(arg)).


  !(my-use-module (library args))


  (= (piece-on-square $X $Y $Board $Piece)  
    (path-arg 
      (:: $Y $X) $Board $Piece))


  (= (change-piece-on-square $X $Y $Piece $Board1 $Board2)  
    (same-functor $Board1 $Board2 $YMax)
    (change-item-in-column $YMax $Y $X $Board1 $Piece $Board2))


  (= (change-item-in-column $Curr $Curr $X $Board1 $Item $Board2)  
    (set-det)
    (corresponding-arg $Curr $Board1 $Row1 $Board2 $Row2)
    (same-functor $Row1 $Row2 $XMax)
    (change-item-in-row $XMax $X $Row1 $Item $Row2)
    (is $Curr1 
      (- $Curr 1))
    (copy-columns $Curr1 $Board1 $Board2))
  (= (change-item-in-column $Curr $Y $X $Board1 $Item $Board2)  
    (same-arg $Curr $Board1 $Board2)
    (is $Curr1 
      (- $Curr 1))
    (change-item-in-column $Curr1 $Y $X $Board1 $Item $Board2))
	

  (= (copy-columns 0 $Board1 $Board2)  
    (set-det))
  (= (copy-columns $Curr $Board1 $Board2)  
    (same-arg $Curr $Board1 $Board2)
    (is $Curr1 
      (- $Curr 1))
    (copy-columns $Curr1 $Board1 $Board2))
	


  (= (change-item-in-row $Curr $Curr $Row1 $Item $Row2)  
    (set-det)
    (corresponding-arg $Curr $Row1 $Old $Row2 $Item)
    (is $Curr1 
      (- $Curr 1))
    (copy-rows $Curr1 $Row1 $Row2))
  (= (change-item-in-row $Curr $X $Row1 $Item $Row2)  
    (same-arg $Curr $Row1 $Row2)
    (is $Curr1 
      (- $Curr 1))
    (change-item-in-row $Curr1 $X $Row1 $Item $Row2))
	

  (= (copy-rows 0 $Row1 $Row2)  
    (set-det))
  (= (copy-rows $Curr $Row1 $Row2)  
    (same-arg $Curr $Row1 $Row2)
    (is $Curr1 
      (- $Curr 1))
    (copy-rows $Curr1 $Row1 $Row2))
	
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; List implementation of arrays
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; nth_element(N,List,Elt): rewrite of nth/3
;
; set_nth_element(N,List,Val,NewList)


  (= (nth-element 1 (Cons  $H $T) $H)  
    (set-det))
  (= (nth-element $N (Cons  $H $T) $Nth)  
    (is $Next 
      (- $N 1))
    (nth-element $Next $T $Nth))



  (= (nth-element-between $G $G (Cons  $H $Rest) Nil $H $Rest)  
    (set-det))
  (= (nth-element-between $Goal $Current (Cons  $H $T) (Cons  $H $Before) $Nth $Rest)  
    (is $Next 
      (+ $Current 1))
    (nth-element-between $Goal $Next $T $Before $Nth $Rest))

;
; set_nth_element(N,LIn,Val,LOut)
;
; LOut is a list LIn, with the Nth element replaced
;
; by Val.


  (= (set-nth-element 1 (Cons  $H $Rest) $Val (Cons  $Val $Rest))  
    (set-det))
  (= (set-nth-element $N (Cons  $H $T) $Nth (Cons  $H $Rest))  
    (is $Next 
      (- $N 1))
    (set-nth-element $Next $T $Nth $Rest))


;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; BAG Abstract Data Type
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; member_bag(Elem, Bag)


  (= (member_bag  $Elem (Cons  $Elem $Bag))  True)
  (= (member-bag $Elem (Cons  $_ $Rest))  
    (member-bag $Elem $Rest))


;
; add_bag(Elem, Bag1, Bag2)
;
; is true when Bag1 and Bag2 are bags represented as unordered lists,
;
; and Bag2 = Bag1 U {Elem}. It may only be used to calculate Bag2
;
; given Elem and Bag1. However, if Bag1 is a list with a variable at
;
; the end, it may still be used, and will add new elements at the end.


  (= (add_bag  $Elem $Bag (Cons  $Elem $Bag))  True)


;
; del_bag(Elem, Bag1, Bag2)
;
; is true when Bag1 and Bag2 are bags represented as unordered lists,
;
; and Bag2 = Bag1 \ {Elem}. It may only be used to calculate Bag2
;
; given Elem and Bag1. If Bag1 does not contain Elem, this fails.
;
; If Set1 is not an unordered list, but contains more than one copy of Elem,
;
; only the first will be removed.

;
; del_bag(Elem, [Elem|Bag2], Bag2) :- !.
;
; del_bag(Elem, [X|Bag1], [X|Bag2]) :- !,
;
; del_bag(Elem, Bag1, Bag2).


  (= (del-bag $Elem (Cons  $H $T) $Rest)  
    (det-if-then-else 
      (= $Elem $H) 
      (= $Rest $T) 
      (, 
        (= $Rest 
          (Cons  $H $Rest1)) 
        (del-bag $Elem $T $Rest1))))
	    

;
; ================================================================================
;
; Indexing Dynamic Predicates.
;
; ================================================================================

;
; Here we precompute a unique integer index for each dynamic predicate.
;
; This relies on the domain theory not changing after we've done this
;
; optimization.



  (= (index-dynamic-preds)  
    (index-preds-file $File)
    (index-dynamic-preds-to-file $File)
    (compile $File))


  (= (index-dynamic-preds-to-file $File)  
    ( (format "~nIndexing Dynamic Predicates to file: ~w~n" 
        (:: $File)) 
      (assert-pred-indices) 
      (tell $File) 
      (listing pred-index) 
      (told) 
      (remove-all-atoms  &self 
        (pred_index  $_ $_))))


  (= (assert-pred-indices)  
    (or 
      (, 
        (abolish (/ pred-index 2)) 
        (pred-index-slow $Pred $Index) 
        (add-is-symbol  &self 
          (pred_index  $Pred $Index)) 
        (fail)) True))

;
; This is the predicate we partially execute to form an indexed table.

  (= (pred-index-slow $Pred $Index)  
    (dynamic-predicates $Preds)
    (nth $Index $Preds $Pred))


