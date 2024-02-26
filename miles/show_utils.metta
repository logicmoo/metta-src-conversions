;
;                        MODULE show_utils EXPORTS


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
;                         ; Show all clauses

;                         ; Show all examples

;                         ; Show one clause

;                         ; Show all names of predicates

;                          ; Show some clauses

;                          ; displays all available types

;                          ; Print all clauses to UNIX-file




;
;                         IMPORTS

  !(use-module 
    (home bu-basics) 
    (:: 
      (/ head 3) 
      (/ body 3)))
  !(use-module 
    (home div-utils) 
    (:: 
      (/ make-unique 2) 
      (/ mysetof 3)))
  !(use-module 
    (home kb) 
    (:: 
      (/ get-clause 5) 
      (/ get-example 3)))
  !(use-module 
    (home argument-types) 
    (:: (/ type-restriction 2)))
  !(use-module-if-exists 
    (library basics) 
    (:: 
      (/ nonmember 2) 
      (/ member 2)))
;
;                            METAPREDICATES

;
;                            none



;
;                           ***********************************************************************

;
;                            *

;
;                            * module: show_utils.pl

;
;                            *

;
;                            * author: B.Jung, M.Mueller, I.Stahl, B.Tausend              date:12/92

;
;                            *

;
;                             * changed:

;
;                             *

;
;                             * description:	various diplays predicates

;
;                             *

;
;                             * see also:

;
;                              *

;
;                              ***********************************************************************




;
;                               ***********************************************************************

;
;                               *

;
;                               * predicate:	show_kb/0

;
;                               *

;
;                               * syntax:	-

;
;                                *

;
;                                * args:	none

;
;                                *

;
;                                * description:	displays all clauses in kb asserted by known

;
;                                *

;
;                                 ***********************************************************************



  (= 
    (show-kb) 
    ( (get-clause $I $H $B $_ $O) 
      (show-kb-clause $I $H $B $O) 
      (fail)))
  (= 
    (show-kb) 
    (set-det))


;
;                                  ***********************************************************************

;
;                                  *

;
;                                   * predicate:	 print_kb/1

;
;                                   *

;
;                                   * syntax:	 print_kb(+ File)

;
;                                   *

;
;                                   * args: File: name of a file

;
;                                    *

;
;                                    * description:	prints kb to a file

;
;                                    *

;
;                                    ***********************************************************************



  (= 
    (print-kb $Filename) 
    ( (tell $Filename) 
      (show-kb) 
      (told)))



;
;                                      ***********************************************************************

;
;                                      *

;
;                                      * predicate:	 show_clause/1

;
;                                      *

;
;                                      * syntax:	show_clause(+ ID)

;
;                                       *

;
;                                       * args:	ID: the ID of a clause

;
;                                       *

;
;                                       * description:	displays the clause stored with ID

;
;                                       *

;
;                                        ***********************************************************************



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
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: 	show_clauses/1

;
;                                         *

;
;                                         * syntax:      show_clauses(+List_of_clauseIDs)

;
;                                         *

;
;                                         * args:	+List_of_clauseIDs: a list of clause IDs

;
;                                         *

;
;                                         * description:	displays each clause with ID in List_of_clauseIDs

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (show-clauses Nil) 
    (set-det))
  (= 
    (show-clauses (Cons  $Id1 $Rest)) 
    ( (show-clause $Id1) 
      (nl) 
      (show-clauses $Rest)))


;
;                                         ************************************************************************

;
;                                         *

;
;                                         * predicate:    show_kb_clause/4

;
;                                         *

;
;                                         * syntax:       show_kb_clause(+I,+H,+B,+O)

;
;                                         *

;
;                                         * args:         I: an ID in KB

;
;                                         *               H: the head of a clause

;
;                                         *               B: the body of a clause

;
;                                         *               O: the label of a clause

;
;                                         *

;
;                                         * description: displays a clause H:-B, used for xm

;
;                                         *

;
;                                         ************************************************************************



  (= 
    (show-kb-clause $I $H $B $O) 
    ( (format '~N~n% Clause ~w (label ~w)~n' 
        (:: $I $O)) 
      (not (not (, (guess-varnames (= $H $B)) (implode-varnames (= $H $B)) (portray-clause (= $H $B))))) 
      (set-det)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:show_names/0

;
;                                         *

;
;                                         * syntax:

;
;                                         *

;
;                                         * args:	none

;
;                                         *

;
;                                         * description:	lists all predicate names available in the kb

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (show-names) 
    (show-names Nil))
  (= 
    (show-names $Accu) 
    ( (get-clause $_ $H $_ $_ $_) 
      (functor $H $Name $_) 
      (nonmember $Name $Accu) 
      (format "~10|~a~n" $Name) 
      (set-det) 
      (show-names (Cons  $Name $Accu))))
  (= 
    (show-names $_) 
    (set-det))

;
;                                         ************************************************************************

;
;                                         *

;
;                                         * predicate: show_kb_part/2

;
;                                         *

;
;                                         * syntax:    show_kb_part(+From,+To)

;
;                                         *

;
;                                         * args:      From: the min ID of KB entries to be shown

;
;                                         *            To:   the max ID of KB entries to  be shown

;
;                                         *

;
;                                         * description: shows all clauses with From <= ID <= To

;
;                                         *

;
;                                         ************************************************************************



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
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	show_ex/0

;
;                                         *

;
;                                         * syntax:

;
;                                         *

;
;                                         * args:	none

;
;                                         *

;
;                                         * description: displays all examples in kb

;
;                                         *

;
;                                         ***********************************************************************



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
  (= 
    (show-ex) 
    (set-det))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:  show_heads/0, show_bodies/0

;
;                                         *

;
;                                         * syntax:

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description:	displays all intermediate heads/bodies stored by absorption,

;
;                                         *		saturation,... in the kb

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (show-heads) 
    ( (head $L $Flag $C) 
      (write (head $L $Flag $C)) 
      (nl) 
      (fail)))
  (= show_heads True)


  (= 
    (show-bodies) 
    ( (body $L $Flag $C) 
      (write (body $L $Flag $C)) 
      (nl) 
      (fail)))
  (= show_bodies True) 


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	pp_clause/1

;
;                                         *

;
;                                         * syntax: pp_clause(+CL)

;
;                                         *

;
;                                         * args: CL .. clause in list notation

;
;                                         *

;
;                                         * description: displays clause in list notation

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (pp_clause  ()) True)
  (= 
    (pp-clause (Cons  (with_self  $H $S) $Rest)) 
    ( (write (with_self  $H $S)) 
      (nl) 
      (pp-clause $Rest)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	write_list/1

;
;                                         *

;
;                                         * syntax:	write_list(+List)

;
;                                         *

;
;                                         * args:	List: a list

;
;                                         *

;
;                                         * description:	displays copy of a  list after instantiating all terms

;
;                                                        within the copy by $Var(N)

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (write_list  ()) True)
  (= 
    (write-list (Cons  $X0 $R)) 
    ( (copy-term $X0 $X) 
      (numbervars $X 0 $_) 
      (write $X) 
      (nl) 
      (write-list $R)))
  (= 
    (write-list (with_self  $PS (with_self  $_ $_))) 
    (write-list $PS))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:   show_kb_types/0

;
;                                         *

;
;                                         * syntax:

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description:	displays definitions of all types in the kb

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         ***********************************************************************



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
      (show-kb-types (Cons  (with_self  (is-symbol) Nil) (Cons  (with_self  (number) Nil) (Cons  (with_self  (symbolic) Nil) $Tlist))))))

  (= 
    (show_kb_types  ()) True)
  (= 
    (show-kb-types (Cons  (with_self  $T $Def) $R)) 
    ( (nl) 
      (write $T) 
      (write :) 
      (nl) 
      (show-kb-t $Def) 
      (show-kb-types $R)))


  (= 
    (show_kb_t  ()) True)
  (= 
    (show-kb-t (Cons  $C $R)) 
    ( (numbervars $C 0 $_) 
      (write $C) 
      (nl) 
      (show-kb-t $R)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:   show_type_restrictions/0

;
;                                         *

;
;                                         * syntax:

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description:	displays all type restrictions in the kb

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



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
  (= show_type_restrictions True)
