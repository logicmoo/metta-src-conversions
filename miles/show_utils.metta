;
; 


  !(module show-utils 
    (:: 
      (/ show-kb 0) 
      (/ show-ex 0) 
      (/ show-clause 1) 
      (/ show-kb-clause 4) 
      (/ show-clauses 1) 
      (/ show-names 0) 
      (/ show-kb-part 2) 
      (/ show-kb-types 0) 
      (/ show-type-restrictions 0) 
      (/ print-kb 1) 
      (/ show-heads 0) 
      (/ show-bodies 0) 
      (/ pp-clause 1) 
      (/ write-list 1)))
; 



;
; 

  !(use-module 
    (home bu-basics) 
    (:: 
      (/ head 3) 
      (/ body 3)))
; 

  !(use-module 
    (home div-utils) 
    (:: 
      (/ make-unique 2) 
      (/ mysetof 3)))
; 

  !(use-module 
    (home kb) 
    (:: 
      (/ get-clause 5) 
      (/ get-example 3)))
; 

  !(use-module 
    (home argument-types) 
    (:: (/ type-restriction 2)))
; 

  !(use-module-if-exists 
    (library basics) 
    (:: 
      (/ nonmember 2) 
      (/ member 2)))
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
    (show-kb) 
    ( (get-clause $I $H $B $_ $O) 
      (show-kb-clause $I $H $B $O) 
      (fail)))
; 

  (= 
    (show-kb) 
    (set-det))
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
    (print-kb $Filename) 
    ( (tell $Filename) 
      (show-kb) 
      (told)))
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
    (show-clause $I) 
    ( (get-clause $I $H $B $_ $O) 
      (write $I) 
      (write : ) 
      (write '(by ') 
      (write $O) 
      (write )) 
      (portray-clause (= $H $B)) 
      (set-det)))
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
    (show-clauses Nil) 
    (set-det))
; 

  (= 
    (show-clauses (Cons  $Id1 $Rest)) 
    ( (show-clause $Id1) 
      (nl) 
      (show-clauses $Rest)))
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
    (show-kb-clause $I $H $B $O) 
    ( (format '~N~n% Clause ~w (label ~w)~n' 
        (:: $I $O)) 
      (not (not (, (guess-varnames (= $H $B)) (implode-varnames (= $H $B)) (portray-clause (= $H $B))))) 
      (set-det)))
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
    (show-names) 
    (show-names Nil))
; 

  (= 
    (show-names $Accu) 
    ( (get-clause $_ $H $_ $_ $_) 
      (functor $H $Name $_) 
      (nonmember $Name $Accu) 
      (format "~10|~a~n" $Name) 
      (set-det) 
      (show-names (Cons  $Name $Accu))))
; 

  (= 
    (show-names $_) 
    (set-det))
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
    (show-kb-part $From $To) 
    ( (mysetof $I 
        (^ $H 
          (^ $B 
            (^ $S 
              (^ $O 
                (, 
                  (get-clause $I $H $B $S $O) 
                  (=< $From $I) 
                  (>= $To $I)))))) $IDL) (show-clauses $IDL)))
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
    (show-ex) 
    ( (get-example $I $F $C) 
      (write 'Example ') 
      (write $I) 
      (write : ) 
      (write $F) 
      (write  -> ) 
      (write $C) 
      (nl) 
      (fail)))
; 

  (= 
    (show-ex) 
    (set-det))
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
    (show-heads) 
    ( (head $L $Flag $C) 
      (write (head $L $Flag $C)) 
      (nl) 
      (fail)))
; 

  (= show_heads True)
; 



  (= 
    (show-bodies) 
    ( (body $L $Flag $C) 
      (write (body $L $Flag $C)) 
      (nl) 
      (fail)))
; 

  (= show_bodies True)
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
    (pp_clause  ()) True)
; 

  (= 
    (pp-clause (Cons  (with_self  $H $S) $Rest)) 
    ( (write (with_self  $H $S)) 
      (nl) 
      (pp-clause $Rest)))
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
    (write_list  ()) True)
; 

  (= 
    (write-list (Cons  $X0 $R)) 
    ( (copy-term $X0 $X) 
      (numbervars $X 0 $_) 
      (write $X) 
      (nl) 
      (write-list $R)))
; 

;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  miles/show_utils.pl 270 14 7417))



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
    (show-kb-types) 
    ( (findall 
        (with_self  $T $Def) 
        (, 
          (get-clause $_ $H $_ $_ type) 
          (=.. $H 
            (Cons  $T $_)) 
          (findall 
            (= $H1 $B1) 
            (, 
              (get-clause $_ $H1 $B1 $_ type) 
              (=.. $H1 
                (Cons  $T $_))) $Def)) $Tlist0) 
      (make-unique $Tlist0 $Tlist) 
      (nl) 
      (write 'The following types are defined in the knowledge base:') 
      (nl) 
      (show-kb-types (Cons  (with_self  (is-symbol *) Nil) (Cons  (with_self  (number *) Nil) (Cons  (with_self  (symbolic *) Nil) $Tlist))))))
; 


  (= 
    (show_kb_types  ()) True)
; 

  (= 
    (show-kb-types (Cons  (with_self  $T $Def) $R)) 
    ( (nl) 
      (write $T) 
      (write :) 
      (nl) 
      (show-kb-t $Def) 
      (show-kb-types $R)))
; 



  (= 
    (show_kb_t  ()) True)
; 

  (= 
    (show-kb-t (Cons  $C $R)) 
    ( (numbervars $C 0 $_) 
      (write $C) 
      (nl) 
      (show-kb-t $R)))
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
    (show-type-restrictions) 
    ( (type-restriction $M $A) 
      (numbervars 
        (, $M $A) 0 $_) 
      (nl) 
      (write 'type-restriction( ') 
      (write $M) 
      (write , ) 
      (write $A) 
      (write  )) 
      (fail)))
; 

  (= show_type_restrictions True)
; 

