;
; 

;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 




  !(my-ensure-loaded (library aux))
; 


;
; 

;
; 


;
; 


;
; 

;
; 


;
; 

;
; 

;
; 


;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (new-state (state $S)) 
    (initialize-state Nil $S))
; 


;
; 


  (= 
    (initialize-state $SIn $SOut) 
    ( (setof 
        (- $P $I) 
        (pred-index $P $I) $Indices) (initialize-state-indices $Indices $SIn $SOut)))
; 



  (= 
    (initialize_state_indices  () $SIn $SIn) True)
; 

  (= 
    (initialize-state-indices 
      (Cons  
        (- $P $I) $Is) $SIn $SOut) 
    ( (initialize-state-index $P $I $SIn $S1) (initialize-state-indices $Is $S1 $SOut)))
; 

	

;
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
    (initialize-state-index $P $I $SIn 
      (Cons  $Bucket $SIn)) 
    (init-bucket $P $Bucket))
; 



  (= 
    (is_state  
      (state  $_)) True)
; 



  (= 
    (true-in $Pred 
      (state $State)) 
    (db-true $Pred $State))
; 



  (= 
    (add-in $Pred 
      (state $SIn) 
      (state $SOut)) 
    (db-add $Pred $SIn $SOut))
; 



  (= 
    (del-in $Pred 
      (state $SIn) 
      (state $SOut)) 
    (db-del $Pred $SIn $SOut))
; 



;
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
    (db-true $P $SIn) 
    ( (db-key $P $Key) (in-key $Key $P $SIn)))
; 



  (= 
    (db-add $P $SIn $SOut) 
    ( (db-key $P $Key) (add-key $Key $P $SIn $SOut)))
; 


;
; 


  (= 
    (db-del $P $SIn $SOut) 
    ( (db-key $P $Key) (del-key $Key $P $SIn $SOut)))
; 



  (= 
    (db-key $P $Key) 
    (pred-index $P $Key))
; 



  (= 
    (in-key $Key $P $SIn) 
    ( (find-bucket $Key $SIn $Bucket) (in-bucket $P $Bucket)))
; 



  (= 
    (add-key $Key $P $SIn $SOut) 
    ( (find-bucket $Key $SIn $Bucket) 
      (add-bucket $P $Bucket $Bucket1) 
      (set-bucket $Key $SIn $Bucket1 $SOut)))
; 



  (= 
    (del-key $Key $P $SIn $SOut) 
    ( (find-bucket $Key $SIn $Bucket) 
      (del-bucket $P $Bucket $Bucket1) 
      (set-bucket $Key $SIn $Bucket1 $SOut)))
; 



  (= 
    (find-bucket $Index $State $Bucket) 
    (nth-element $Index $State $Bucket))
; 



  (= 
    (set-bucket $Index $SIn $Bucket $SOut) 
    (set-nth-element $Index $SIn $Bucket $SOut))
; 



;
; 

;
; 


;
; 

;
; 


;
; 

;
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
    (init-bucket 
      (on $_ $_) $Bucket) 
    ( (set-det) (new-game-board $Bucket)))
; 

  (= 
    (init_bucket  $_ ()) True)
; 




  (= 
    (empty-row $X $Term) 
    ( (functor $Term1 row $X) (empty-elements $X $Term1 $Term)))
; 




  (= 
    (empty-elements 0 $Board $Board) 
    (set-det))
; 

  (= 
    (empty-elements $N $Board1 $Board) 
    ( (>= $N 1) 
      (emptify $N $Board1 $Board2) 
      (is $N1 
        (- $N 1)) 
      (empty-elements $N1 $Board2 $Board)))
; 



  (= 
    (emptify $N $Board1 $Board2) 
    ( (empty-filler $E) 
      (arg $N $Board1 $E) 
      (= $Board1 $Board2)))
; 


;
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
    (empty_filler  $_) True)
; 


;
; 

;
; 

;
; 




  (= 
    (empty-board $X $Y $Term) 
    ( (functor $Term1 gameboard $Y) (empty-rows $Y $X $Term1 $Term)))
; 



  (= 
    (empty-rows 0 $X $Board $Board) 
    (set-det))
; 

  (= 
    (empty-rows $N $X $Board1 $Board) 
    ( (>= $N 1) 
      (emptify-row $N $X $Board1 $Board2) 
      (is $N1 
        (- $N 1)) 
      (empty-rows $N1 $X $Board2 $Board)))
; 



  (= 
    (emptify-row $N $X $Board1 $Board2) 
    ( (empty-row $X $Row) 
      (arg $N $Board1 $Row) 
      (= $Board1 $Board2)))
; 



  (= 
    (new-game-board $Board) 
    ( (current-board-size $X $Y) (empty-board $X $Y $Board)))
; 



;
; 



  (= 
    (in-bucket 
      (on $Piece 
        (square $X $Y)) $Board) 
    ( (set-det) (piece-on-square $X $Y $Board $Piece)))
; 

  (= 
    (in-bucket $P $Bucket) 
    (member-bag $P $Bucket))
; 



;
; 



  (= 
    (add-bucket 
      (on $Piece 
        (square $X $Y)) $A $A1) 
    ( (set-det) (change-piece-on-square $X $Y $Piece $A $A1)))
; 


  (= 
    (add-bucket $P $A $A1) 
    (add-bag $P $A $A1))
; 



  (= 
    (del-bucket 
      (on $Piece $Sq) $E $E1) 
    ( (set-det) (= $E $E1)))
; 

  (= 
    (del-bucket $P $E $E1) 
    ( (set-det) (del-bag $P $E $E1)))
; 



;
; 

;
; 

;
; 

;
; 


;
; 

;
; 



  !(my-use-module (library args))
; 



  (= 
    (piece-on-square $X $Y $Board $Piece) 
    (path-arg 
      (:: $Y $X) $Board $Piece))
; 



  (= 
    (change-piece-on-square $X $Y $Piece $Board1 $Board2) 
    ( (same-functor $Board1 $Board2 $YMax) (change-item-in-column $YMax $Y $X $Board1 $Piece $Board2)))
; 



  (= 
    (change-item-in-column $Curr $Curr $X $Board1 $Item $Board2) 
    ( (set-det) 
      (corresponding-arg $Curr $Board1 $Row1 $Board2 $Row2) 
      (same-functor $Row1 $Row2 $XMax) 
      (change-item-in-row $XMax $X $Row1 $Item $Row2) 
      (is $Curr1 
        (- $Curr 1)) 
      (copy-columns $Curr1 $Board1 $Board2)))
; 

  (= 
    (change-item-in-column $Curr $Y $X $Board1 $Item $Board2) 
    ( (same-arg $Curr $Board1 $Board2) 
      (is $Curr1 
        (- $Curr 1)) 
      (change-item-in-column $Curr1 $Y $X $Board1 $Item $Board2)))
; 

	

  (= 
    (copy-columns 0 $Board1 $Board2) 
    (set-det))
; 

  (= 
    (copy-columns $Curr $Board1 $Board2) 
    ( (same-arg $Curr $Board1 $Board2) 
      (is $Curr1 
        (- $Curr 1)) 
      (copy-columns $Curr1 $Board1 $Board2)))
; 

	


  (= 
    (change-item-in-row $Curr $Curr $Row1 $Item $Row2) 
    ( (set-det) 
      (corresponding-arg $Curr $Row1 $Old $Row2 $Item) 
      (is $Curr1 
        (- $Curr 1)) 
      (copy-rows $Curr1 $Row1 $Row2)))
; 

  (= 
    (change-item-in-row $Curr $X $Row1 $Item $Row2) 
    ( (same-arg $Curr $Row1 $Row2) 
      (is $Curr1 
        (- $Curr 1)) 
      (change-item-in-row $Curr1 $X $Row1 $Item $Row2)))
; 

	

  (= 
    (copy-rows 0 $Row1 $Row2) 
    (set-det))
; 

  (= 
    (copy-rows $Curr $Row1 $Row2) 
    ( (same-arg $Curr $Row1 $Row2) 
      (is $Curr1 
        (- $Curr 1)) 
      (copy-rows $Curr1 $Row1 $Row2)))
; 

	
;
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
    (nth-element 1 
      (Cons  $H $T) $H) 
    (set-det))
; 

  (= 
    (nth-element $N 
      (Cons  $H $T) $Nth) 
    ( (is $Next 
        (- $N 1)) (nth-element $Next $T $Nth)))
; 




  (= 
    (nth-element-between $G $G 
      (Cons  $H $Rest) Nil $H $Rest) 
    (set-det))
; 

  (= 
    (nth-element-between $Goal $Current 
      (Cons  $H $T) 
      (Cons  $H $Before) $Nth $Rest) 
    ( (is $Next 
        (+ $Current 1)) (nth-element-between $Goal $Next $T $Before $Nth $Rest)))
; 


;
; 

;
; 

;
; 



  (= 
    (set-nth-element 1 
      (Cons  $H $Rest) $Val 
      (Cons  $Val $Rest)) 
    (set-det))
; 

  (= 
    (set-nth-element $N 
      (Cons  $H $T) $Nth 
      (Cons  $H $Rest)) 
    ( (is $Next 
        (- $N 1)) (set-nth-element $Next $T $Nth $Rest)))
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
    (member_bag  $Elem 
      (Cons  $Elem $Bag)) True)
; 

  (= 
    (member-bag $Elem 
      (Cons  $_ $Rest)) 
    (member-bag $Elem $Rest))
; 



;
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
    (add_bag  $Elem $Bag 
      (Cons  $Elem $Bag)) True)
; 



;
; 

;
; 

;
; 

;
; 

;
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
    (del-bag $Elem 
      (Cons  $H $T) $Rest) 
    (det-if-then-else 
      (= $Elem $H) 
      (= $Rest $T) 
      (, 
        (= $Rest 
          (Cons  $H $Rest1)) 
        (del-bag $Elem $T $Rest1))))
; 

	    

;
; 

;
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
    (index-dynamic-preds) 
    ( (index-preds-file $File) 
      (index-dynamic-preds-to-file $File) 
      (compile $File)))
; 



  (= 
    (index-dynamic-preds-to-file $File) 
    ( (format "~nIndexing Dynamic Predicates to file: ~w~n" 
        (:: $File)) 
      (assert-pred-indices) 
      (tell $File) 
      (listing pred-index) 
      (told) 
      (remove-all-symbols  &self 
        (pred_index  $_ $_))))
; 



  (= 
    (assert-pred-indices) 
    (or 
      (, 
        (abolish (/ pred-index 2)) 
        (pred-index-slow $Pred $Index) 
        (add-symbol  &self 
          (pred_index  $Pred $Index)) 
        (fail)) True))
; 


;
; 


  (= 
    (pred-index-slow $Pred $Index) 
    ( (dynamic-predicates $Preds) (nth $Index $Preds $Pred)))
; 



