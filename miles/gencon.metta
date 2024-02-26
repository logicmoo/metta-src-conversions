;
; 


  !(module gencon 
    (:: 
      (/ gilppi 12) 
      (/ gilppi 14)))
; 




;
; 

  !(meta-predicate (, (gilppi : : : : : : : : : : : :) (gilppi + + : : : : : : : : : : : :)))
; 



;
; 

  !(use-module 
    (home kb) 
    (:: (/ store-clauses 2)))
; 

  !(use-module 
    (home show-utils) 
    (:: 
      (/ write-list 1) 
      (/ show-kb 0)))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (gilppi $Initialize $Stop_C $Quality_C $Update $Select $Add $Filter $One_of $Spec $Gen $L_newp $Output) 
    ( (c-call $Initialize 
        (:: $Partial_Sols)) (gilppi $Partial_Sols Nil $Initialize $Stop_C $Quality_C $Update $Select $Add $Filter $One_of $Spec $Gen $L_newp $Output)))
; 



  (= 
    (gilppi $_ $Complete_Sols $_ $Stop_C $_ $_ $_ $_ $_ $_ $_ $_ $_ $Output) 
    ( (c-call $Stop_C 
        (:: $Complete_Sols)) 
      (set-det) 
      (c-call $Output 
        (:: $Complete_Sols))))
; 


  (= 
    (gilppi $Partial_Sols $Complete_Sols $Initialize $Stop_C $Quality_C $Update $Select $Add $Filter $One_of $Spec $Gen $L_newp $Output) 
    ( (c-call $Select 
        (:: $Partial_Sols $PS $M $Partial_Sols1)) (det-if-then-else (c-call $Quality_C (:: $PS)) (, (c-call $Update (:: $Partial_Sols1 $Partial_Sols2)) (gilppi $Partial_Sols2 (Cons  $PS $Complete_Sols) $Initialize $Stop_C $Quality_C $Update $Select $Add $Filter $One_of $Spec $Gen $L_newp $Output)) (, (det-if-then-else (== $M active) (, (c-call $One_of (:: $PS $GS)) (det-if-then-else (== $GS spec) (, (write Specialising) (nl) (write-list $PS) (nl) (nl) (c-call $Spec (:: $PS $PSL))) (, (write Generalising) (nl) (write-list $PS) (nl) (nl) (c-call $Gen (:: $PS $PSL))))) (c-call $L_newp (:: $PS $PSL $Initialize $Stop_C $Quality_C $Update $Select $Add $Filter $One_of $Spec $Gen $L_newp $Output))) (c-call $Add (:: $Partial_Sols1 $PSL $Partial_Sols2)) (c-call $Filter (:: $Partial_Sols2 $Partial_Sols3)) (gilppi $Partial_Sols3 $Complete_Sols $Initialize $Stop_C $Quality_C $Update $Select $Add $Filter $One_of $Spec $Gen $L_newp $Output)))))
; 




  (= 
    (c-call $MPred $Arglist) 
    ( (c-mod $MPred $M $Pred) 
      (=.. $Call 
        (Cons  $Pred $Arglist)) 
      (call (with_self  $M $Call))))
; 



  (= 
    (c-mod 
      (with_self  $M $Pred) $M $Pred) 
    ( (simple $Pred) (set-det)))
; 

  (= 
    (c-mod 
      (with_self  $_ $P) $M1 $Pred) 
    ( (set-det) (c-mod $P $M1 $Pred)))
; 


