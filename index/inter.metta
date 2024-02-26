
  (= 
    (show-lists $Filter $Texts $Lists) 
    (show-lists $Filter $Texts 1 Nil $Lists))
; 


  (= 
    (show_lists  $Filter $Words $N $Text ()) True)
; 

  (= 
    (show-lists $Filter Nil $N $Text 
      (Cons  $H $T)) 
    (show-list $Filter $Text 
      (Cons  $H $T)))
; 

  (= 
    (show-lists $Filter 
      (Cons  $Word $Words) 1 $Text 
      (:: $H)) 
    ( (set-det) (show-lists $Filter $Words 1 $Text $H)))
; 

  (= 
    (show-lists $Filter 
      (Cons  $Word $Words) $N $Text 
      (Cons  $H $T)) 
    ( (append $Text 
        (:: $Word $N  --- ) $NewText) 
      (show-lists $Filter $Words 1 $NewText $H) 
      (is $N1 
        (+ $N 1)) 
      (show-lists $Filter 
        (Cons  $Word $Words) $N1 $Text $T)))
; 



  (= 
    (show-list $Filter $Text $List) 
    ( (filter $List $Filter $Filtered) (show-list $Text $Filtered)))
; 


  (= 
    (show-list $Text Nil) 
    ( (set-det) 
      (write-list (:: 'There are no ' $Text .)) 
      (nl)))
; 

  (= 
    (show-list $Text $List) 
    ( (write-list (:: $Text :)) 
      (display-list $List $List1) 
      (show-items $List1) 
      (nl)))
; 



  (= 
    (choose-list $Text $List $Sel) 
    ( (display-list $List $List1) 
      (choose-items 1 $List1) 
      (write-list (:: $Text ? )) 
      (read $Ns) 
      (det-if-then-else 
        (nths $List $Ns Nil $Sel) True 
        (det-if-then otherwise 
          (, 
            (write 'Wrong number! Try again.') 
            (nl) 
            (choose-list $Text $List $Sel))))))
; 



  (= 
    (nths $List all Nil $List) 
    (set-det))
; 

  (= 
    (nths $In 
      (- $A $A) $Tmp $Out) 
    ( (set-det) (nths $In $A $Tmp $Out)))
; 

  (= 
    (nths $In 
      (- $A $B) $Tmp $Out) 
    ( (set-det) 
      (< $A $B) 
      (is $A1 
        (+ $A 1)) 
      (nths $In $A $Tmp $Tmp1) 
      (nths $In 
        (- $A1 $B) $Tmp1 $Out)))
; 

  (= 
    (nths $In 
      (, $N $Ns) $Tmp $Out) 
    ( (set-det) 
      (nths $In $N $Tmp $Tmp1) 
      (nths $In $Ns $Tmp1 $Out)))
; 

;
; 

;
; 

  (= 
    (nths 
      (Cons  $X $R) 1 $Tmp 
      (Cons  $X $Tmp)) 
    (set-det))
; 

  (= 
    (nths 
      (Cons  $X $R) $N $Tmp $Out) 
    ( (is $N1 
        (- $N 1)) (nths $R $N1 $Tmp $Out)))
; 


;                                     (error
;                                       (syntax_error  operator_expected)
;                                       (file  index/inter.pl 56 6 1401))

;                                      (error
;                                        (syntax_error  operator_expected)
;                                        (file  index/inter.pl 60 6 1499))



  (= 
    (choose-items $N Nil) 
    (nl))
; 

  (= 
    (choose-items $N 
      (Cons  $H $T)) 
    ( (nl) 
      (write-list (::     $N .  $H)) 
      (is $N1 
        (+ $N 1)) 
      (choose-items $N1 $T)))
; 



  (= 
    (display_list  () ()) True)
; 

  (= 
    (display-list 
      (Cons  $H $T) 
      (Cons  $DH $DT)) 
    ( (det-if-then-else 
        (switched-on horn) 
        (displayhorn $H $DH) 
        (det-if-then otherwise 
          (display $H $DH))) (display-list $T $DT)))
; 



  (= 
    (displayhorn $X $HX) 
    ( (horn $X $HX) (set-det)))
; 

  (= 
    (displayhorn  $X $X) True)
; 



  (= 
    (prompt-read $Question $Answer) 
    ( (write-list (:: $Question ? )) 
      (read $Answer) 
      (det-if-then-else 
        (= $Answer stop) fail 
        (det-if-then otherwise True))))
; 



  (= 
    (yesno $Question) 
    ( (write-list $Question) 
      (read $Answer) 
      (det-if-then-else 
        (= $Answer yes) True 
        (det-if-then-else 
          (= $Answer no) fail 
          (det-if-then otherwise 
            (, 
              (call $Answer) 
              (yesno $Question)))))))
; 


;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  index/inter.pl 96 4 2236))


  (= 
    (write-debug $Message) 
    ( (switched-on debug) 
      (write 	| ) 
      (write-list $Message) 
      (nl)))
; 



  (= 
    (write-list $List) 
    ( (flatten $List $FList) (write-list1 $FList)))
; 



  (= 
    (write_list1  ()) True)
; 

  (= 
    (write-list1 (Cons  $H $T)) 
    ( (write $H) (write-list1 $T)))
; 



  (= 
    (quit) 
    (abort))
; 


