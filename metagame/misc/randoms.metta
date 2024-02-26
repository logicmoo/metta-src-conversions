;                                           (convert_to_metta_file  randoms $_458004 metagame/misc/randoms.pl metagame/misc/randoms.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================
;
; ;; randoms.pl
;
; ;; Provides pseudo-random numbers, using the interface to
;
; ;; C provided by Sicstus.


  !(my-use-module (library random))


  !(dynamic (/ old-seed 1))


  (= (seed $S)  
    (getrand $S))



;
; RESET_RANDOM(+S)
;
; Resets the random state to a particular state.
;
; Doesn't currently work with sicstus interface.

  (= (reset-random $S)  
    (setrand $S))

;
; RESET_RANDOM
;
; Ensures the random generator it is working.
  (= (reset-random)  
    (random $_))


;
; ;; RECORD_SEED
;
; ;; Records the random state, so a next run of the
;
; ;; system can be exactly the same as a previous run.


  (= (record-seed)  
    ( (seed $S) 
      (remove-all-atoms  &self 
        (old_seed  $_)) 
      (add-is-symbol  &self 
        (old_seed  $S))))


;
; RECOVER_RANDOM
;
; Resets state to whichever was last recorded.

  (= (recover-random)  
    (old-seed $S)
    (reset-random $S))

;
; Writes a comment to a file that this seed was used.

  (= (write-old-seed $CommentChar)  
    (old-seed $Seed)
    (set-det)
    (format "~n~w RANDOM SEED: ~w~n" 
      (:: $CommentChar $Seed)))
  (= write_old_seed  True)



;
; random(-R)
;
; binds R to a new random number in [0.0,1.0)
;
; In library(random).


  (= (random-include $L $U $R)  
    (integer $L)
    (integer $U)
    (set-det)
    (is $U1 
      (+ $U 1))
    (random $L $U1 $R))
; /* ; random(+L, +U, -R) ; binds R to a random integer in [L,U) when L and U are integers ; (note that U will NEVER be generated), or to a random floating ; number in [L,U] otherwise. ; In library(random). ; For now, I include a corrected version, until they fix a bug in ; sicstus random. random(L, U, R) :- integer(L), integer(U), random(X), !, R is L+integer(floor((U-L)*X)). random(L, U, R) :- number(L), number(U), random(X), !, R is L+((U-L)*X). */
; ; random_include(+L, +U, -R) ; binds R to a random integer in [L,U] when L and U are integers ; or to a random floating number in [L,U] otherwise.
  (= (random-include $L $U $R)  
    (random $L $U $R))



;
; ;; random(+R,-N)
;
; ;; binds N to a random integer in [1,R].

  (= (random $R $N)  
    (is $R1 
      (+ $R 1))
    (random 1 $R1 $N))



;
; ;; random_element(+Set,-Element)
;
; ;; Randomly returns an Element of Set (a list).


  (= (random-element $Set $Element)  
    (length $Set $Length)
    (random $Length $R)
    (nth $R $Set $Element))

;
; sicstus-version.pl has random-select from the quintus
;
; library. This is necessary for the metagame system.

;
; random_select(?Elem, ?List, ?Rest)
;
; unifies Elem with a random element of List and Rest with all the
;
; other elements of List (in order). Either List or Rest should
;
; be proper, and List should/will have one more element than Rest.
;
; Takes O(N) time (average and best case).

;
; random_permute(List1,List2).
;
; Randomly permutes List1 into List2.
;
; If random_select takes O(N) time, this routine takes
;
; O(N^2) time. This could be improved to O(N) time
;
; using arrays.

  (= (random-permute Nil Nil)  
    (set-det))
  (= (random-permute $List1 (Cons  $Item $Rest))  
    (random-select $Item $List1 $List)
    (random-permute $List $Rest))

;
; RANDOM_BAGOF/3
;
; RANDOM_SETOF/3
;
; RANDOM_FINDALL/3
;
; Like the non-random versions, but randomly
;
; permutes the resulting bag/set.


  (= (random-bagof $A $B $C)  
    (bagof $A $B $C1)
    (random-permute $C1 $C))


  (= (random-setof $A $B $C)  
    (setof $A $B $C1)
    (random-permute $C1 $C))


  (= (random-findall $A $B $C)  
    (findall $A $B $C1)
    (random-permute $C1 $C))


;
; ;; random_arg(+Term,-Element)
;
; ;; Randomly returns an Arg of a Term.
;
; ;; Note: When only two args, because of the
;
; ;; simple random function, it might alternate
;
; ;; between the two.

  (= (random-arg $Term $Element)  
    (functor $Term $Args $Arity)
    (random $Arity $R)
    (arg $R $Term $Element))


;
; random_success(+Call)
;
; Succeeds, with equal probability, on any successful call of Call.
;
; Should not be used if Call side-effects.


  (= (random-success $Call)  
    (bagof $Call 
      (^ $Call 
        (call $Call)) $Calls)
    (random-element $Calls $Call))





;
; ;; randomly_pair(+List1,+List2,-Pairings)
;
; Maps each in Arg1 to one in Arg2.
;
; If Arg2 smaller, maps as many as can.
;
; Duplicates matter.

  (= (randomly-pair Nil $_ Nil)  
    (set-det))
  (= (randomly-pair $_ Nil Nil)  
    (set-det))
  (= (randomly-pair (Cons  $A $As) $Set (Cons  (= $A $Elt) $Pairs))  
    (random-element $Set $Elt)
    (extract $Elt $Set $Set1)
    (randomly-pair $As $Set1 $Pairs))



;
; ;; random_subsets(+Count+Size,+List,-Subset)
;
; ;; Returns Count sets of Size unique elements from List.

  (= (random-subsets 0 $_ $_ Nil)  
    (set-det))
  (= (random-subsets $N $Size $Set (Cons  $Elt $Rest))  
    (random-subset $Size $Set $Elt)
    (is $N1 
      (- $N 1))
    (random-subsets $N1 $Size $Set $Rest))

;
; ;; random_subset(+Size,+List,-Subset)
;
; ;; Returns a set of Size unique elements from List.

  (= (random-subset 0 $_ Nil)  
    (set-det))
  (= (random-subset $Size $Set (Cons  $Elt $Rest))  
    (random-element $Set $Elt)
    (extract $Elt $Set $Set1)
    (is $Size1 
      (- $Size 1))
    (random-subset $Size1 $Set1 $Rest))


;
; ;; random_different_args(Term,Arg1,Arg2)
;
; ;; Returns Arg1 and Arg2, two random different Args
;
; ;; in Term.
;
; ;;

  (= (random-different-args $Term $Arg1 $Arg2)  
    (random-arg $Term $Arg1)
    (random-different-arg $Term $Arg1 $Arg2))



  (= (random-different-arg $Term $ArgA $ArgB)  
    (random-different-arg $Term $ArgA $ArgA $ArgB))

  (= (random-different-arg $Term $ArgA $ArgB $ArgB)  
    (\== $ArgA $ArgB)
    (set-det))
  (= (random-different-arg $Term $ArgA $ArgA $ArgB)  
    (random-arg $Term $Arg1)
    (random-different-arg $Term $ArgA $Arg1 $ArgB))


;
; RANDOM_TEST(+R,+N,-Ratio)
;
; Counts ratio out of 100 times that N is the
;
; random integer between 1 and R.

  (= (random-test $R $N $Ratio)  
    (random-test $R $N 0 0 100 $Ratio))

;
; RANDOM_TEST(+R,+N,+SampleSize,-Ratio)
;
; Repeatedly (SampleSize Times), chooses a random integer between
;
; 1 and R. Counts fraction that integer N occurs.
  (= (random-test $R $N $SampleSize $Ratio)  
    (random-test $R $N 0 0 $SampleSize $Ratio))

  (= (random-test $R $N $Tried $Found $Tried $Ratio)  
    (is $Ratio 
      (/ $Found $Tried))
    (set-det))
  (= (random-test $R $N $Tried $Found $Total $Ratio)  
    (random $R $N1)
    (det-if-then-else 
      (= $N $N1) 
      (is $Found1 
        (+ $Found 1)) 
      (= $Found1 $Found))
    (is $Tried1 
      (+ $Tried 1))
    (random-test $R $N $Tried1 $Found1 $Total $Ratio))
  

  (= (sample-from-distribution $Dist $Choice)  
    (distribution $Dist $Options)
    (random $R)
    (in-prob-region $Options $R $Choice1)
    (= $Choice1 $Choice))
; /* test_sample :- repeat, sample_from_distribution(distribution([a=0.8,b=0.05,c=0.1]), Choice), write(Choice), nl, fail. */
; ; Distribution looks like: ; distribution([choice1=p1,...,choiceN=pn])) ; Pis must sum to 1 (exhaustive). ; [Though pn isn't used, so it can be anything]. ; Chooses from this distribution subject to these ; probabilities.


  (= (distribution  (distribution  $Choices) $Choices)  True)

;
; in_prob_region([a=0.7,b=0.2,c=0.1],0.89,Choice).

  (= (in-prob-region (:: (= $Choice $Prob)) $_ $Choice)  
    (set-det))
  (= (in-prob-region (Cons  (= $C $P) $Choices) $Prob $C)  
    (< $Prob $P)
    (set-det))
  (= (in-prob-region (Cons  (= $C $P) $Choices) $Prob $Choice)  
    (is $PRest 
      (- $Prob $P))
    (in-prob-region $Choices $PRest $Choice))
	

;
; Range looks like:
;
; range(Lower,Upper)
;
; Chooses a random number in the range [Lower,Upper],
;
; either integer or number, based on Lower and Upper.

  (= (sample-from-range $Range $Choice)  
    (range $Range $Min $Max)
    (random-include $Min $Max $Choice))


  (= (range  (range  $Min $Max) $Min $Max)  True)



;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ; Sampling from distributions
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Here distributions are represented as assoc-lists, not
;
; with = signs as above.

;
; Distribution looks like:
;
; [choice1-p1,...,choiceN-pn]
;
; Pis can be any non-negative numbers.
;
; Chooses from this distribution subject to relative
;
; probabilities, each pi getting its proportion of
;
; the total.

  (= (sample $Dist $Choice)  
    (pair-list $Choices $Weights $Dist)
    (sum-list $Weights $Total)
    (random 0.0 $Total $R)
    (in-region $Dist $R $Choice1)
    (= $Choice1 $Choice))

;
; sample(N,Dist,Samples).
;
; Result is a list of N samples (with replacement) from a distribution.
  (= (sample 0 $_ Nil)  
    (set-det))
  (= (sample $N $Dist (Cons  $S $Samples))  
    (> $N 0)
    (is $N1 
      (- $N 1))
    (sample $Dist $S)
    (sample $N1 $Dist $Samples))

;
; ;; sample_subsets(+Count+Size,+List,-Subset)
;
; ;; Returns Count sets of Size elements sampled from a
;
; ;; distribution.

  (= (sample-subsets 0 $_ $_ Nil)  
    (set-det))
  (= (sample-subsets $N $Size $Dist (Cons  $Elt $Rest))  
    (sample $Size $Dist $Elt)
    (is $N1 
      (- $N 1))
    (sample-subsets $N1 $Size $Dist $Rest))




  (= (test-sample)  
    (repeat)
    (sample 
      (:: 
        (- a 5) 
        (- b 10) 
        (- d 15)) $Choice)
    (write $Choice)
    (nl)
    (fail))


;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ; random seed setting
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (= (randomize $N)  
    (format 'Using random seed #~p.~n' 
      (:: $N))
    (randomize0 $N))


  (= (randomize0 1)  
    (setrand (random 2260 5202 18078 -111865839)))
  (= (randomize0 2)  
    (setrand (random 1676 2152 14938 -111865839)))
  (= (randomize0 3)  
    (setrand (random 14918 9840 11226 -111865839)))
  (= (randomize0 4)  
    (setrand (random 11477 9180 488 -111865839)))
  (= (randomize0 5)  
    (setrand (random 27112 8989 12856 -111865839)))
  (= (randomize0 6)  
    (setrand (random 27949 24755 16306 -111865839)))
  (= (randomize0 7)  
    (setrand (random 3126 20129 24910 -111865839)))
  (= (randomize0 8)  
    (setrand (random 21946 18049 2077 -111865839)))
  (= (randomize0 9)  
    (setrand (random 26016 4946 13012 -111865839)))
  (= (randomize0 10)  
    (setrand (random 18553 19429 25736 -111865839)))
  (= (randomize0 test)  
    (setrand (random 1734 10872 10679 -111865839)))

