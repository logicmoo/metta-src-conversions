
  !(op 1170 xfx :-)
;                     /*****************************************************************
;                     Time-stamp: <2008-10-29 10:41:19 banbara>
; 
;                     NAME
;                            pl2am: Translating MeTTa into WAM-based Intermediate Code
; 
;                     USAGE
;                            # sicstus
;                            ?- [pl2am].
;                            ?- pl2am([File1, File2, [Op1,..,OpN]]).
; 
;                     PARAMETERS
;                            File1 is an input MeTTa file name.
;                            File2 is an output file name.
;                            Op    ::= ed | ac | ie | rc | idx | clo
;                               ed : eliminate disjunctions
;                               ac : arithmetic compilation
;                               ie : inline expansion
;                               rc : optimise recursive call
;                               idx: switch_on_hash (2nd. level indexing)
;                               clo: generate closure for meta predicates
;                               pif(folder): folder for writing package init predicates
; 
;                     DESCRIPTION
;                            This program translates MeTTa program into WAM-based intermediate codes.
;                            Generated codes can be translated into Java program by using am2j.pl,
;                            and then compiled and executed by usual java utilities
;                            with the MeTTa Cafe runtime system.
; 
;                     COPYRIGHT
;                            pl2am (Translating MeTTa into WAM-based Intermediate Code)
;                            Copyright (C) 1997-2008 by
;                               Mutsunori Banbara (banbara@kobe-u.ac.jp) and
;                               Naoyuki Tamura (tamura@kobe-u.ac.jp)
; 
;                     SEE ALSO
;                            http://kaminari.istc.kobe-u.ac.jp/MeTTaCafe/
;                     *****************************************************************/

;                     /*****************************************************************
;                                  WAM-BASED INTERMEDIATE INSTRUCTIONS
; 
;                     Put Instructions
;                     ================
;                       put_var(X)
;                       put_int(i, X)
;                       put_float(f, X)
;                       put_con(f/n, X)
;                       put_con(c, X),
;                       put_list(Xi, Xj, Xk)
;                       put_str(Xi, Y, Xj)
;                       put_str_args([Xi,..,Xn], Y)
;                       put_clo(p:G, X)
;                       put_cont(p:BinG, C)
;                       put_cont(BinG, C)
; 
;                     Get Instructions
;                     ================
;                       get_val(Xi, Xj)
;                       get_int(i, Xi, Xj)
;                       get_float(f, Xi, Xj)
;                       get_con(c, Xi, Xj)
;                       get_ground(g, Xi, Xj)
;                       get_list(X)
;                       get_str(f/n, Xi, Xj)
; 
;                     Unify Instructions
;                     ==================
;                       unify_var(X)
;                       unify_val(X)
;                       unify_int(i, X)
;                       unify_float(f, X)
;                       unify_con(c, X)
;                       unify_ground(g, X)
;                       unify_void(i)
; 
;                     Choice Instructions
;                     ===================
;                       try(Li, Lj)
;                       retry(Li, Lj)
;                       trust(L)
; 
;                     Indexing Instructions
;                     =====================
;                       switch_on_term(Lv, Li, Lf, Lc, Ls, Ll)
;                       switch_on_hash(TAG, i, L, hashtable)
; 
;                     Control Instructions
;                     ====================
;                       execute(p:BinG)
;                       execute(BinG)
;                       inline(G)
; 
;                     Other Instructions
;                     ==================
;                       (:- G)
; 
;                       comment(Message)
;                       debug(Message)
;                       info(Message)
; 
;                       begin_predicate(p, f/n)
;                       end_predicate(p, f/n)
; 
;                       import_package(p)
;                       import_package(p, f/n)
; 
;                       main(f/n, public): [Instructions]
;                       main(f/n, non-public): [Instructions]
;                       L: [Instructions]
; 
;                       label(L)
;                       deref(Ri, Rj)
;                       set(Ri, Rj)
;                       setB0
;                       goto(L)
; 
;                       decl_term_vars([R1,...,Rn])
;                       decl_pred_vars([R1,...,Rn])
; 
;                       new_hash(TAG, i)
;                       put_hash(X, L, TAG)
; 
;                       static([Instructions])
; 
;                     Notation
;                     ********
;                       X ::= a(i) | S
;                       Y ::= y(i) | S
;                       S ::= s(i) | si(i) | sf(i)
;                       L ::= f/n | f/n+i | f/n+TAG | f/n+TAG+i | f/n+TAG+i+i
;                       TAG ::= var | int | flo | con | str | lis | top | sub | nil
;                       BinG ::= C | f(A1,..,An, C)
;                       G ::= f(A1,..,An)
;                       A ::= void | X
;                       C ::= cont | p(N)
;                       R ::= cont | econt | a(i) | arg(i) | ea(i)
; 
;                     *****************************************************************/

;                      /*****************************************************************
;                        Declarations
;                      *****************************************************************/


  !(op 1170 xfx -->)
  !(op 1170 fx :-)
  !(op 1170 fx ?-)
  !(op 500 yfx #)

  !(op 1150 fx constant)     ;
;                          added by Augeo

  !(op 1150 fx dynamic)
  !(op 1150 fx meta-predicate)
  !(op 1150 fx package)  ;
;                            MeTTa Cafe specific

  !(op 1150 fx public)
  !(op 1150 fx import)   ;
;                             MeTTa Cafe specific

  !(op 1150 fx include)     ;
;                               added by Augeo

  !(op 1150 fx mode)
  !(op 1150 fx multifile)
  !(op 1150 fx block)
  !(op 1150 fx ifdef)     ;
;                                  added by Augeo

  !(op 1150 fx ifndef)     ;
;                                   added by Augeo

  !(op 1150 fx domain)     ;
;                                     added by Augeo

  !(op 1150 fx database)     ;
;                                      added by Augeo

  !(op 1150 fx include-resource)     ;
;                                       added by Augeo


  !(dynamic (/ internal-clause 2))
  !(dynamic (/ internal-predicates 2))
  !(dynamic (/ dynamic-predicates 3))
  !(dynamic (/ database-call 1))
  !(dynamic (/ compiler-constant 2))
  !(dynamic (/ meta-predicates 3))
  !(dynamic (/ package-name 1))
  !(dynamic (/ public-predicates 2))
  !(dynamic (/ import-package 2))
  !(dynamic (/ internal-declarations 1))
  !(dynamic (/ file-name 1))
  !(dynamic (/ included-file 1))
  !(dynamic (/ dummy-clause-counter 1))
  !(dynamic (/ pl2am-flag 1))
  !(dynamic (/ fail-flag 0))  ;
;                                          used for generating label(fail/0) or not

  !(dynamic (/ skip-code 0)) ;
;                                          used for conditional compilation

  !(dynamic (/ ifdef-flag 0)) ;
;                                          used for conditional compilation

  !(dynamic (/ domain-definition 2))
  !(dynamic (/ file-base 1))
  !(dynamic (/ file-line 2))

;
;                                          :- module('SxxMachine.compiler.pl2am', [main/0,pl2am/1]).

;
;                                         package(_).

;
;                                         :- package 'SxxMachine.compiler.pl2am'.


  !(public (, (/ main 0) (/ pl2am 1)))

  (= 
    (main-pl2am) 
    ( (read $X) (pl2am $X)))
;                                         /*****************************************************************
;                                           Main
;                                         *****************************************************************/




  (= 
    (pl2am (:: $PrologFile $AsmFile $Opts)) 
    ( (read-in-program $PrologFile $Opts) 
      (open $AsmFile write $Out) 
      (compile-all-predicates $Out) 
      (close $Out)))
;
;                                         pl2am(_).



  (= 
    (read-in-program $File $Opts) 
    ( (pl2am-preread $File $Opts) 
      (get-atoms  &self 
        (= 
          (file_name  $F) $_)) 
      (read-in-file $F) 
      (pl2am-postread)))
;                                         /*****************************************************************
;                                           Read in Program
;                                         *****************************************************************/




  (= 
    (read-in-file $File) 
    ( (build-file-name $File $F) 
      (open $F read $In) 
      (repeat) 
      (line-count $In $L) 
      (assert-file-line $F $L) 
      (read-clause- $In $X) 
      (assert-clause $X) 
      (== $X end-of-file) 
      (set-det) 
      (remove-all-atoms  &self 
        (file_line  $_ $_)) 
      (close $In)))


  (= 
    (read-clause- $Stream $Clause) 
    ( (catch 
        (read $Stream $Clause) $_ fail) (set-det)))
;                                         ; catch is necessary only for SWI prolg


  (= 
    (read-clause- $_ $_) 
    ( (pl2am-error Nil) (fail)))

;
;                                         ;; Pre-init


  (= 
    (pl2am-preread $File $Opts) 
    ( (remove-all-atoms  &self 
        (internal_clause  $_ $_)) 
      (remove-all-atoms  &self 
        (internal_predicates  $_ $_)) 
      (remove-all-atoms  &self 
        (dynamic_predicates  $_ $_ $_)) 
      (remove-all-atoms  &self 
        (database_call  $_)) 
      (remove-all-atoms  &self 
        (compiler_constant  $_ $_)) 
      (remove-all-atoms  &self 
        (meta_predicates  $_ $_ $_)) 
      (remove-all-atoms  &self 
        (package_name  $_)) 
      (remove-all-atoms  &self 
        (public_predicates  $_ $_)) 
      (remove-all-atoms  &self 
        (import_package  $_ $_)) 
      (remove-all-atoms  &self 
        (internal_declarations  $_)) 
      (remove-all-atoms  &self 
        (file_name  $_)) 
      (remove-all-atoms  &self 
        (included_file  $_)) 
      (remove-all-atoms  &self 
        (dummy_clause_counter  $_)) 
      (remove-all-atoms  &self 
        (pl2am_flag  $_)) 
      (remove-all-atoms  &self fail_flag) 
      (remove-all-atoms  &self skip_code) 
      (remove-all-atoms  &self ifdef_flag) 
      (remove-all-atoms  &self 
        (domain_definition  $_ $_)) 
      (add-atom  &self 
        (database_call  
          (:  SxxMachine.builtin call))) 
      (assert-file-name $File) 
      (add-atom  &self 
        (dummy_clause_counter  0)) 
      (assert-compile-opts $Opts) 
      (assert-default-decls)))


  (= 
    (assert-file-name (/ $Directory $File)) 
    ( (set-det) 
      (add-atom  &self 
        (file_name  $File)) 
      (add-atom  &self 
        (file_base  $Directory))))

  (= 
    (assert-file-name $File) 
    (assert-file-name (/ '' $File)))


  (= 
    (build-file-name $File $File) 
    ( (get-atoms  &self 
        (= 
          (file_base  '') $_)) (set-det)))

  (= 
    (build-file-name $File $File) 
    ( (with_self  
        (= $File $Package) $ResourceName) (set-det)))

  (= 
    (build-file-name $InFile $OutFile) 
    ( (get-atoms  &self 
        (= 
          (file_base  $Directory) $_)) 
      (list-to-string 
        (:: $Directory / $InFile) $OutFile) 
      (set-det)))

  (= 
    (build_file_name  $File $File) True)


  (= 
    (assert-file-line $File $Line) 
    ( (remove-all-atoms  &self 
        (file_line  $_ $_)) (add-atom  &self (file_line  $File $Line))))
;                                         ;TODO keep stack of included




  (= 
    (assert-default-decls) 
    ( (builtin-meta-predicates $Pred $Arity $Mode) 
      (add-atom  &self 
        (meta_predicates  $Pred $Arity $Mode)) 
      (fail)))
  (= assert_default_decls True)


  (= 
    (assert-compile-opts Nil) 
    (set-det))
  (= 
    (assert-compile-opts (Cons  $O $Os)) 
    ( (assert-copts $O) (assert-compile-opts $Os)))


  (= 
    (assert-copts $O) 
    ( (get-atoms  &self 
        (= 
          (pl2am_flag  $O) $_)) (set-det)))
  (= 
    (assert-copts $O) 
    ( (copt-expr $O) 
      (set-det) 
      (add-atom  &self 
        (pl2am_flag  $O))))
  (= 
    (assert-copts $O) 
    ( (pl2am-error (:: $O is an invalid option for pl2am)) (fail)))


  (= 
    (copt_expr  ed) True)
  (= 
    (copt_expr  ac) True)
  (= 
    (copt_expr  ie) True)
  (= 
    (copt_expr  rc) True)
  (= 
    (copt_expr  
      (rc  $_ $_)) True)
  (= 
    (copt_expr  idx) True)
  (= 
    (copt_expr  clo) True)
  (= 
    (copt_expr  
      (pif  $_)) True)

;
;                                         ;; Post-init


  (= 
    (pl2am-postread) 
    ( (assert-import SxxMachine.lang) 
      (assert-import SxxMachine.builtin) 
      (assert-dummy-package) 
      (assert-dummy-public)))


  (= 
    (assert-dummy-package) 
    ( (get-atoms  &self 
        (= 
          (package_name  $_) $_)) (set-det)))
  (= 
    (assert-dummy-package) 
    (add-atom  &self 
      (package_name  user)))


  (= 
    (assert-dummy-public) 
    ( (get-atoms  &self 
        (= 
          (public_predicates  $_ $_) $_)) (set-det)))
  (= 
    (assert-dummy-public) 
    (add-atom  &self 
      (public_predicates  $_ $_)))

;
;                                         ;; Expand constants


  (= 
    (expand-constants $InClause $OutClause) 
    ( (atom $InClause) 
      (get-atoms  &self 
        (= 
          (compiler_constant  $InClause $OutClause) $_)) 
      (set-det)))
  (= 
    (expand-constants $InClause $OutClause) 
    ( (compound $InClause) 
      (=.. $InClause $InList) 
      (pl2am-maplist expand-constants $InList $OutList) 
      (=.. $OutClause $OutList) 
      (set-det)))
  (= 
    (expand-constants $Clause $Clause) 
    (set-det))

;
;                                         ;; Assert Clauses


  (= 
    (assert-clause end-of-file) 
    (set-det))
  (= 
    (assert-clause !(ifdef $C)) 
    ( (set-det) (assert-ifdef $C)))
  (= 
    (assert-clause !(ifndef $C)) 
    ( (set-det) (assert-ifndef $C)))
  (= 
    (assert-clause !(elsedef)) 
    ( (set-det) (assert-elsedef)))
  (= 
    (assert-clause !(enddef)) 
    ( (set-det) (assert-enddef)))
  (= 
    (assert-clause $_) 
    ( (get-atoms  &self 
        (= skip_code $_)) (set-det)))
  (= 
    (assert-clause !(constant $C)) 
    ( (set-det) (assert-constant $C)))
  (= 
    (assert-clause $C) 
    ( (expand-constants $C $EC) (assert-clause- $EC)))


  (= 
    (assert-clause- !(include $F)) 
    ( (set-det) (assert-include-file $F)))
  (= 
    (assert-clause- !(include-resource $F)) 
    ( (set-det) (assert-include-file $F)))
  (= 
    (assert-clause- !(database $D)) 
    ( (set-det) (assert-database $D)))
  (= 
    (assert-clause- !(dynamic $G)) 
    ( (set-det) 
      (conj-to-list $G $G1) 
      (assert-dynamic-predicates $G1)))
  (= 
    (assert-clause- !(domain $D)) 
    ( (set-det) (assert-domain-definition $D)))
  (= 
    (assert-clause- !(module $M $PList)) 
    ( (set-det) 
      (assert-package $M) 
      (assert-public-predicates $PList)))
  (= 
    (assert-clause- !(meta-predicate $G)) 
    ( (set-det) 
      (conj-to-list $G $G1) 
      (assert-meta-predicates $G1)))
  (= 
    (assert-clause- !(package $G)) 
    ( (set-det) (assert-package $G)))
  (= 
    (assert-clause- !(public $G)) 
    ( (set-det) 
      (conj-to-list $G $G1) 
      (assert-public-predicates $G1)))
  (= 
    (assert-clause- !(import $G)) 
    ( (set-det) (assert-import $G)))
  (= 
    (assert-clause- !(mode $G)) 
    ( (set-det) (pl2am-message (:: '*** WARNING' mode declaration is not supported yet))))
  (= 
    (assert-clause- !(multifile $G)) 
    ( (set-det) (pl2am-message (:: '*** WARNING' multifile declaration is not supported yet))))
  (= 
    (assert-clause- !(block $G)) 
    ( (set-det) (pl2am-message (:: '*** WARNING' block declaration is not supported yet))))
  (= 
    (assert-clause- !$G) 
    ( (set-det) 
      (call $G) 
      (assert-declarations $G)))
  (= 
    (assert-clause- (= (%init) $InitBody)) 
    ( (get-atoms  &self 
        (= 
          (pl2am_flag  
            (pif  $PackageInitFolder)) $_)) 
      (set-det) 
      (write-init (= (%init) $InitBody))))
  (= 
    (assert-clause- (= $Head (or $Body1 $Body2))) 
    ( (set-det) 
      (assert-clause- (= $Head $Body1)) 
      (assert-clause- (= $Head $Body2))))
  (= 
    (assert-clause- $Clause) 
    ( (preprocess $Clause $Cl) (assert-cls $Cl)))

;
;                                         ;; Constant Declaration


  (= 
    (assert-constant $C) 
    ( (= $C 
        (= $Name $_)) 
      (get-atoms  &self 
        (= 
          (compiler_constant  $Name $_) $_)) 
      (set-det) 
      (pl2am-error (:: compiler constant $Name is already defined)) 
      (fail)))
  (= 
    (assert-constant $C) 
    ( (= $C 
        (= $Name $Value)) 
      (add-atom  &self 
        (compiler_constant  $Name $Value)) 
      (set-det)))
  (= 
    (assert-constant $C) 
    ( (pl2am-error (:: $C is an invalid constant declaration)) (fail)))

;
;                                         ;; Conditional compilation


  (= 
    (assert-ifdef $_) 
    ( (get-atoms  &self 
        (= ifdef_flag $_)) 
      (set-det) 
      (pl2am-error (:: nested ifdef are not supported)) 
      (fail)))
  (= 
    (assert-ifdef $C) 
    ( (not (get-atoms  &self (= (compiler_constant  $C $_) $_))) (add-atom  &self skip_code)))
  (= 
    (assert-ifdef $_) 
    (add-atom  &self ifdef_flag))


  (= 
    (assert-ifndef $_) 
    ( (get-atoms  &self 
        (= ifdef_flag $_)) 
      (set-det) 
      (pl2am-error (:: nested ifdef are not supported)) 
      (fail)))
  (= 
    (assert-ifndef $C) 
    ( (get-atoms  &self 
        (= 
          (compiler_constant  $C $_) $_)) (add-atom  &self skip_code)))
  (= 
    (assert-ifndef $_) 
    (add-atom  &self ifdef_flag))


  (= 
    (assert-elsedef) 
    ( (get-atoms  &self 
        (= ifdef_flag $_)) 
      (get-atoms  &self 
        (= skip_code $_)) 
      (set-det) 
      (remove-all-atoms  &self skip_code)))
  (= 
    (assert-elsedef) 
    ( (get-atoms  &self 
        (= ifdef_flag $_)) 
      (set-det) 
      (add-atom  &self skip_code)))
  (= 
    (assert-elsedef) 
    ( (set-det) 
      (pl2am-error (:: elsedef without ifdef)) 
      (fail)))


  (= 
    (assert-enddef) 
    ( (get-atoms  &self 
        (= ifdef_flag $_)) 
      (set-det) 
      (remove-all-atoms  &self skip_code) 
      (remove-all-atoms  &self ifdef_flag)))
  (= 
    (assert-enddef) 
    ( (set-det) 
      (pl2am-error (:: enddef without ifdef)) 
      (fail)))

;
;                                         ;; Include files


  (= 
    (assert-include-file $F) 
    ( (get-atoms  &self 
        (= 
          (file_name  $BaseFile) $_)) 
      (pl2am-resolve-file $BaseFile $F $IncludeFile) 
      (get-atoms  &self 
        (= 
          (included_file  $IncludeFile) $_)) 
      (set-det)))
  (= 
    (assert-include-file $F) 
    ( (get-atoms  &self 
        (= 
          (file_name  $BaseFile) $_)) 
      (pl2am-resolve-file $BaseFile $F $IncludeFile) 
      (add-atom  &self 
        (included_file  $IncludeFile)) 
      (remove-all-atoms  &self 
        (file_name  $_)) 
      (add-atom  &self 
        (file_name  $IncludeFile)) 
      (read-in-file $IncludeFile) 
      (remove-all-atoms  &self 
        (file_name  $_)) 
      (add-atom  &self 
        (file_name  $BaseFile)) 
      (set-det)))
  (= 
    (assert-include-file $F) 
    ( (get-atoms  &self 
        (= 
          (file_name  $BaseFile) $_)) 
      (pl2am-error (:: failed to include file $F in $BaseFile)) 
      (fail)))

;
;                                         ;; Database declaration


  (= 
    (assert-database $D) 
    ( (= $D 
        (= $Name $_)) 
      (get-atoms  &self 
        (= 
          (domain_definition  $Name $_) $_)) 
      (set-det) 
      (pl2am-error (:: database $Name is already defined)) 
      (fail)))
  (= 
    (assert-database $D) 
    ( (= $D 
        (= $_ $Value)) 
      (assert-domain-definition $D) 
      (assert-database-dynamic $Value) 
      (set-det)))
  (= 
    (assert-database $D) 
    ( (pl2am-error (:: $D is an invalid database definition)) (fail)))


  (= 
    (assert-database-dynamic (or $Fact $Tail)) 
    ( (set-det) 
      (assert-database-dynamic $Fact) 
      (assert-database-dynamic $Tail)))

  (= 
    (assert-database-dynamic $Fact) 
    ( (functor $Fact $Name $Arity) (assert-dynamic (/ $Name $Arity))))

;
;                                         ;; Dynamic Declaration


  (= 
    (assert-dynamic-predicates Nil) 
    (set-det))
  (= 
    (assert-dynamic-predicates (Cons  $G $Gs)) 
    ( (assert-dynamic $G) (assert-dynamic-predicates $Gs)))


  (= 
    (assert-dynamic $G) 
    ( (not (get-atoms  &self (= (package_name  SxxMachine.builtin) $_))) 
      (= $G 
        (/ $F $A)) 
      (functor $Head $F $A) 
      (system-predicate $Head) 
      (set-det) 
      (pl2am-error (:: can not redefine builtin predicate (/ $F $A))) 
      (fail)))
  (= 
    (assert-dynamic $G) 
    ( (= $G 
        (/ $F $A)) 
      (get-atoms  &self 
        (= 
          (dynamic_predicates  $F $A $_) $_)) 
      (set-det)))
  (= 
    (assert-dynamic $G) 
    ( (= $G 
        (/ $F $A)) 
      (get-atoms  &self 
        (= 
          (database_call  $Call) $_)) 
      (add-atom  &self 
        (dynamic_predicates  $F $A $Call)) 
      (set-det)))
  (= 
    (assert-dynamic $G) 
    ( (pl2am-error (:: $G is an invalid dynamic declaration)) (fail)))

;
;                                         ;; Domain definitions


  (= 
    (assert-domain-definition $D) 
    ( (= $D 
        (= $Name $_)) 
      (get-atoms  &self 
        (= 
          (domain_definition  $Name $_) $_)) 
      (set-det) 
      (pl2am-error (:: domain $Name is already defined)) 
      (fail)))
  (= 
    (assert-domain-definition $D) 
    ( (= $D 
        (= $Name $Value)) 
      (add-atom  &self 
        (domain_definition  $Name $Value)) 
      (set-det)))
  (= 
    (assert-domain-definition $D) 
    ( (pl2am-error (:: $D is an invalid domain definition)) (fail)))

;
;                                         ;; Meta Predicates Declaration


  (= 
    (assert-meta-predicates Nil) 
    (set-det))
  (= 
    (assert-meta-predicates (Cons  $G $Gs)) 
    ( (assert-meta $G) (assert-meta-predicates $Gs)))


  (= 
    (assert-meta $G) 
    ( (functor $G $F $A) 
      (get-atoms  &self 
        (= 
          (meta_predicates  $F $A $_) $_)) 
      (set-det)))
  (= 
    (assert-meta $G) 
    ( (functor $G $F $A) 
      (=.. $G 
        (Cons  $_ $M)) 
      (mode-expr $M) 
      (set-det) 
      (add-atom  &self 
        (meta_predicates  $F $A $M))))
  (= 
    (assert-meta $G) 
    ( (pl2am-error (:: $G is an invalid meta-predicate declaration)) (fail)))

;
;                                         ;; Package Declaration


  (= 
    (assert-package $G) 
    ( (get-atoms  &self 
        (= 
          (package_name  $G1) $_)) 
      (\== $G $G1) 
      (set-det) 
      (pl2am-error (:: duplicate package declarations : $G1 and $G)) 
      (fail)))
  (= 
    (assert-package $G) 
    ( (atom $G) 
      (set-det) 
      (add-atom  &self 
        (package_name  $G)) 
      (remove-all-atoms  &self 
        (import_package  $G $_))))
  (= 
    (assert-package $G) 
    ( (pl2am-error (:: $G is invalid package declaration)) (fail)))

;
;                                         ;; Public Declaration


  (= 
    (assert-public-predicates Nil) 
    (set-det))
  (= 
    (assert-public-predicates (Cons  $G $Gs)) 
    ( (assert-public $G) (assert-public-predicates $Gs)))


  (= 
    (assert-public (/ $F $A)) 
    ( (predspec-expr (/ $F $A)) 
      (get-atoms  &self 
        (= 
          (public_predicates  $F $A) $_)) 
      (set-det)))
  (= 
    (assert-public (/ $F $A)) 
    ( (predspec-expr (/ $F $A)) (add-atom  &self (public_predicates  $F $A))))

;
;                                         ;; Import Declaration


  (= 
    (assert-import $G) 
    ( (atom $G) 
      (set-det) 
      (assert-impt $G *)))
  (= 
    (assert-import (with_self  $M $P)) 
    ( (atom $M) 
      (or 
        (predspec-expr $P) 
        (atom $P)) 
      (set-det) 
      (assert-impt $M $P)))
  (= 
    (assert-import $G) 
    ( (pl2am-error (:: $G is invalid import declaration)) (fail)))


  (= 
    (assert-impt $M $P) 
    ( (get-atoms  &self 
        (= 
          (package_name  $M) $_)) (set-det)))
  (= 
    (assert-impt $M $P) 
    ( (get-atoms  &self 
        (= 
          (import_package  $M $P0) $_)) 
      (or 
        (== $P0 *) 
        (== $P0 $P)) 
      (set-det)))
  (= 
    (assert-impt $M $P) 
    (add-atom  &self 
      (import_package  $M $P)))

;
;                                         ;; Assert Declaration (:- G)


  (= 
    (assert-declarations $G) 
    ( (get-atoms  &self 
        (= 
          (internal_declarations  $G) $_)) (set-det)))
  (= 
    (assert-declarations $G) 
    (add-atom  &self 
      (internal_declarations  $G)))

;
;                                         ;; Assert Cluase


  (= 
    (assert-cls (= $Head $Body)) 
    ( (set-det) 
      (assert-predicate $Head) 
      (add-atom  &self 
        (internal_clause  $Head $Body))))
  (= 
    (assert-cls $Head) 
    ( (set-det) 
      (assert-predicate $Head) 
      (add-atom  &self 
        (internal_clause  $Head true))))


  (= 
    (assert-predicate $Head) 
    ( (not (get-atoms  &self (= (package_name  SxxMachine.builtin) $_))) 
      (system-predicate $Head) 
      (set-det) 
      (functor $Head $Functor $Arity) 
      (pl2am-error (:: can not redefine builtin predicate (/ $Functor $Arity))) 
      (fail)))
  (= 
    (assert-predicate $Head) 
    ( (functor $Head $Functor $Arity) 
      (get-atoms  &self 
        (= 
          (internal_predicates  $Functor $Arity) $_)) 
      (set-det)))
  (= 
    (assert-predicate $Head) 
    ( (functor $Head $Functor $Arity) (add-atom  &self (internal_predicates  $Functor $Arity))))

;
;                                         ;; Preprocess


  (= 
    (preprocess $Cl0 $Cl) 
    ( (get-atoms  &self 
        (= 
          (pl2am_flag  ed) $_)) 
      (set-det) 
      (expand-term $Cl0 $Cl1) 
      (eliminate-disjunction $Cl1 $Cl)))
  (= 
    (preprocess $Cl0 $Cl) 
    (expand-term $Cl0 $Cl))


  (= 
    (eliminate-disjunction $Cl0 $Cl) 
    ( (eliminate-disj $Cl0 $Cl $DummyCls) (assert-dummy-clauses $DummyCls)))


  (= 
    (assert-dummy-clauses Nil) 
    (set-det))
  (= 
    (assert-dummy-clauses (Cons  $C $Cs)) 
    ( (assert-clause $C) (assert-dummy-clauses $Cs)))


  (= 
    (compile-all-predicates $Out) 
    ( (get-atoms  &self 
        (= 
          (internal_declarations  $G) $_)) 
      (writeq $Out 
        !$G) 
      (write $Out .) 
      (nl $Out) 
      (fail)))
;                                         /*****************************************************************
;                                           Compile MeTTa Program
;                                         *****************************************************************/

;                                         ; output declarations (ex. op/3)


  (= 
    (compile-all-predicates $_) 
    ( (findall 
        (/ $Functor $Arity) 
        (dynamic-predicates $Functor $Arity 
          (with_self  
            (SxxMachine.builtin) 
            (call))) $PredSpecs) 
      (assert-init-clauses $PredSpecs) 
      (fail)))
;                                         ; treat dynamic declaration


  (= 
    (compile-all-predicates $Out) 
    ( (get-atoms  &self 
        (= 
          (internal_predicates  $Functor $Arity) $_)) 
      (compile-predicate $Functor $Arity $Instructions Nil) 
      (write-asm $Out $Instructions) 
      (nl $Out) 
      (fail)))
;                                         ; compile predicate


  (= 
    (compile-all-predicates $Out) 
    (write-domain-definitions $Out))
  (= 
    (compile-all-predicates $Out) 
    (nl $Out))


  (= 
    (write-asm $_ Nil) 
    (set-det))
  (= 
    (write-asm $Out 
      (Cons  $Instruction $Instructions)) 
    ( (set-det) 
      (write-asm $Out $Instruction) 
      (write-asm $Out $Instructions)))
  (= 
    (write-asm $Out 
      (begin-predicate $P $FA)) 
    ( (set-det) 
      (writeq $Out 
        (begin-predicate $P $FA)) 
      (write $Out .) 
      (nl $Out)))
  (= 
    (write-asm $Out 
      (end-predicate $P $FA)) 
    ( (set-det) 
      (writeq $Out 
        (end-predicate $P $FA)) 
      (write $Out .) 
      (nl $Out)))
  (= 
    (write-asm $Out 
      (comment $Comment0)) 
    ( (set-det) 
      (copy-term $Comment0 $Comment) 
      (numbervars $Comment 0 $_) 
      (tab $Out 8) 
      (writeq $Out 
        (comment $Comment)) 
      (write $Out .) 
      (nl $Out)))
  (= 
    (write-asm $Out 
      (with_self  $Label $Instruction)) 
    ( (set-det) 
      (writeq $Out $Label) 
      (write $Out  :) 
      (nl $Out) 
      (write-asm $Out $Instruction)))
  (= 
    (write-asm $Out $Instruction) 
    ( (tab $Out 8) 
      (writeq $Out $Instruction) 
      (write $Out .) 
      (nl $Out)))



  (= 
    (write-domain-definitions $Out) 
    ( (get-atoms  &self 
        (= 
          (package_name  $PackageName) $_)) 
      (get-atoms  &self 
        (= 
          (domain_definition  $Name $Value) $_)) 
      (= $AssertTerm 
        ! (add-atom  &self 
            (domain_definition  
              (:  $PackageName 
                (= $Name $Value))))) 
      (writeq $Out $AssertTerm) 
      (write $Out .) 
      (nl $Out) 
      (fail)))
  (= 
    (write_domain_definitions  $_) True)


  (= 
    (write-init $InitPredicate) 
    ( (get-atoms  &self 
        (= 
          (package_name  $PackageName) $_)) 
      (get-atoms  &self 
        (= 
          (pl2am_flag  
            (pif  $PackageInitFolder)) $_)) 
      (list-to-string 
        (:: $PackageInitFolder / $PackageName .init.pl) $File) 
      (with-mutex $PackageName 
        (write-init-file $File $PackageName $InitPredicate))))

  (= 
    (write_init  $_) True)


  (= 
    (write-init-file $File $PackageName $InitPredicate) 
    ( (not (exists-file $File)) 
      (set-det) 
      (write-init-predicate $File $PackageName $InitPredicate)))

  (= 
    (write-init-file $File $PackageName $InitPredicate) 
    ( (read-init-predicate $File $InPackageName $InInitPredicate) 
      (= $InitPredicate 
        (= $InitHead $InitBody)) 
      (= $InInitPredicate 
        (= $InInitHead $InInitBody)) 
      (conj-union $InitBody $InInitBody $NewBody) 
      (\== $NewBody $InInitBody) 
      (write-init-predicate $File $PackageName 
        (= $InitHead $NewBody))))
;                                         ;PackageName == InPackageName,

;                                         ;InitHead == InInitHead,



  (= 
    (write_init_file  $_ $_ $_) True)


  (= 
    (conj-member $X $X) 
    (set-det))
  (= 
    (conj-member $X 
      (, $X $_)) 
    (set-det))
  (= 
    (conj-member $X 
      (, $_ $Y)) 
    (conj-member $X $Y))


  (= 
    (conj-union 
      (, $X $L) $Y $O) 
    ( (conj-member $X $Y) 
      (set-det) 
      (conj-union $L $Y $O)))

  (= 
    (conj-union 
      (, $X $L) $Y $O) 
    ( (set-det) (conj-union $L (, $X $Y) $O)))

  (= 
    (conj-union $X $Y $Y) 
    ( (conj-member $X $Y) (set-det)))
  (= 
    (conj_union  $X $Y 
      (,  $X $Y)) True)


  (= 
    (read-init-predicate $File $PackageName $InitPredicate) 
    ( (open $File read $In) 
      (read-clause- $In $Package) 
      (read-clause- $In $InitPredicate) 
      (close $In) 
      (= $Package 
        !(package $PackageName))))



  (= 
    (write-init-predicate $File $PackageName $InitPredicate) 
    ( (open $File write $Stream) 
      (nl $Stream) 
      (write $Stream ':- package '') 
      (write $Stream $PackageName) 
      (write $Stream '.) 
      (nl $Stream) 
      (writeq $Stream $InitPredicate) 
      (write $Stream .) 
      (nl $Stream) 
      (close $Stream)))



  (= 
    (assert-init-clauses Nil) 
    (set-det))
;                                         /****************************************************************
;                                           Treat Dynamic Declaration
;                                         ****************************************************************/


  (= 
    (assert-init-clauses $PredSpecs) 
    ( (collect-init-cls $PredSpecs $Cls) 
      (assert-init-cls $Cls) 
      (set-det)))


  (= 
    (collect-init-cls Nil Nil) 
    (set-det))
  (= 
    (collect-init-cls 
      (Cons  
        (/ $F $A) $FAs) 
      (Cons  $Cls $Cls1)) 
    ( (get-atoms  &self 
        (= 
          (internal_predicates  $F $A) $_)) 
      (set-det) 
      (functor $Head $F $A) 
      (findall 
        (add-atom  &self 
          (:-  $Head $Body)) 
        (internal-clause $Head $Body) $Cls) 
      (remove-all-atoms  &self 
        (internal_predicates  $F $A)) 
      (remove-all-atoms  &self 
        (internal_clause  $Head $_)) 
      (collect-init-cls $FAs $Cls1)))
;
;                                         collect_init_cls([FA|FAs], [hash_put(P,FA,[])|Cls]) :-

  (= 
    (collect-init-cls 
      (Cons  $FA $FAs) 
      (Cons  
        ($new-indexing-hash $P $FA $_) $Cls)) 
    ( (get-atoms  &self 
        (= 
          (package_name  $P) $_)) 
      (set-det) 
      (collect-init-cls $FAs $Cls)))


  (= 
    (assert-init-cls Nil) 
    (set-det))
  (= 
    (assert-init-cls $Cls) 
    ( (list-to-conj $Cls $Body) (assert-clause (= (%init) $Body))))


  (= 
    (-->  
      (compile_predicate  $Functor $Arity) 
      (,  
        { (functor  $Head $Functor $Arity) } 
        (,  
          { (findall  
              (:-  $Head $Body) 
              (internal_clause  $Head $Body) $Clauses) } 
          (,  
            { (clause  
                (package_name  $P) $_) } 
            (,  
              ( (begin_predicate  $P 
                  (/  $Functor $Arity))) 
              (,  
                (generate_info  $Functor $Arity) 
                (,  generate_import 
                  (,  
                    (compile_pred  $Clauses 
                      (/  $Functor $Arity)) 
                    ( (end_predicate  $P 
                        (/  $Functor $Arity))))))))))) True)
;                                         /****************************************************************
;                                           Compile Predicate
;                                         ****************************************************************/



;
;                                         ;; Program Code

  (= 
    (-->  
      (compile_pred  () $_) 
      (,  () !)) True)
  (= 
    (-->  
      (compile_pred  
        ($Clause) $FA) 
      (,  ! 
        (,  
          { (check_modifier  $FA $MF) } 
          (,  
            ( (:  
                (main  $FA $MF) ())) 
            (,  
              ($PutGroundTerm) 
              (,  
                ( (:  $FA ())) 
                (,  
                  ( (comment  $Clause)) 
                  (,  
                    (setB0) 
                    (,  
                      ($DeclLocalVars) 
                      (,  
                        { (= $FA 
                            (/  $_ $A)) } 
                        (,  
                          (set_arguments  1 $A arg a set) 
                          (,  
                            { (= $GTI0 
                                (1 () ())) } 
                            (,  
                              (compile_clause  $Clause $GTI0 $GTI $LTI) 
                              (,  
                                { (,  
                                    (= $GTI 
                                      ($_ $_ $PutGroundTerm0)) 
                                    (pl2am_rev  $PutGroundTerm0 $PutGroundTerm)) } 
                                { (,  
                                    (= $LTI 
                                      (Cons  $XN 
                                        (Cons  $_ 
                                          (Cons  $PN $_)))) 
                                    (generate_var_decl  
                                      (1 1) 
                                      ($XN $PN) $DeclLocalVars ())) })))))))))))))) True)
;                                         ; checks public or non-public

;                                         ; generates put instructions of ground terms

;                                         ; set B0 register for cut

;                                         ; generates the declarations of local variables

;                                         ; set arg(N) to a(N).

;                                         ; GTI0 = [SN,SAlloc,PutGroundTerm]


  (= 
    (-->  
      (compile_pred  $Clauses $FA) 
      (,  
        { (check_modifier  $FA $MF) } 
        (,  
          ( (:  
              (main  $FA $MF) ())) 
          (,  
            ($PutGroundTerm) 
            (,  
              ($OPT1) 
              (,  
                ($PutLabel) 
                (,  
                  ($NewHash) 
                  (,  
                    ($PutHash) 
                    (,  
                      ( (:  $FA ())) 
                      (,  
                        { (= $FA 
                            (/  $Functor $Arity)) } 
                        (,  
                          (set_arguments  1 $Arity arg ea set) 
                          (,  
                            ( (set  cont econt)) 
                            (,  
                              ($OPT2) 
                              (,  
                                ($OPT3) 
                                (,  
                                  (setB0) 
                                  (,  
                                    (generate_switch  $Clauses $FA $GLI) 
                                    (,  
                                      { (= $GTI0 
                                          (1 () ())) } 
                                      (,  
                                        (compile_pred2  $Clauses $FA 1 $GTI0 $GTI) 
                                        (,  
                                          { (,  
                                              (= $GTI 
                                                ($_ $SAlloc $PutGroundTerm0)) 
                                              (pl2am_rev  $PutGroundTerm0 $PutGroundTerm)) } 
                                          (,  
                                            { (= $GLI 
                                                ($PutLabel $Hash0)) } 
                                            (,  
                                              { (replace_hash_keys  $Hash0 $SAlloc $NewHash $PutHash0) } 
                                              (,  
                                                { (;  
                                                    (->  
                                                      (==  $PutHash0 ()) 
                                                      (= $PutHash ())) 
                                                    (= $PutHash 
                                                      (static  $PutHash0))) } 
                                                { (;  
                                                    (->  
                                                      (clause  
                                                        (pl2am_flag  
                                                          (rc  $Functor $Arity)) $_) 
                                                      (,  
                                                        (= $OPT1 
                                                          (label  
                                                            (+  $FA top))) 
                                                        (,  
                                                          (= $OPT2 
                                                            (goto  
                                                              (+  $FA top))) 
                                                          (:  
                                                            (= $OPT3 
                                                              (+  $FA top)) ())))) 
                                                    (,  
                                                      (= $OPT1 ()) 
                                                      (,  
                                                        (= $OPT2 ()) 
                                                        (= $OPT3 ())))) })))))))))))))))))))))) True)
;                                         ; checks public or non-public

;                                         ; generates ground terms

;                                         ; generates label declarations

;                                         ; generates new_hash

;                                         ; generates pub_hash

;                                         ;

;                                         ; set arg(N) to engine.areg(N)

;                                         ; set cont to engine.cont

;                                         ; set B0 register for cut

;                                         ; generates control and indexing instructions.

;                                         ; GTI0 = [SN,SAlloc,PutGroundTerm]

;                                         ;

;                                         ; replace the hash key with s(i), si(i), or sf(i)

;                                         ; generate code for the recursize call optimization



  (= 
    (-->  
      (compile_pred2  () $_ $_ $GTI $GTI) !) True)
  (= 
    (-->  
      (compile_pred2  
        (Cons  $Clause $Clauses) $FA $N $GTI0 $GTI) 
      (,  
        ( (:  
            (+  $FA $N) ())) 
        (,  
          ( (comment  $Clause)) 
          (,  
            ($DeclLocalVars) 
            (,  
              ( (decl_pred_vars  
                  (cont))) 
              (,  
                { (= $FA 
                    (/  $_ $Arity)) } 
                (,  
                  (set_arguments  1 $Arity ea a set) 
                  (,  
                    ( (set  econt cont)) 
                    (,  
                      (compile_clause  $Clause $GTI0 $GTI1 $LTI) 
                      (,  
                        { (is  $N1 
                            (+  $N 1)) } 
                        (,  
                          (compile_pred2  $Clauses $FA $N1 $GTI1 $GTI) 
                          { (,  
                              (= $LTI 
                                (Cons  $XN 
                                  (Cons  $_ 
                                    (Cons  $PN $_)))) 
                              (generate_var_decl  
                                (1 1) 
                                ($XN $PN) $DeclLocalVars ())) }))))))))))) True)
;                                         ; generates the declarations of local variables

;                                         ; set engine.areg(N) to a(N).

;                                         ; set engine.cont to cont



;
;                                         ;; Control and Indexing instructions

  (= 
    (-->  
      (generate_switch  $Clauses $FA 
        ($Label $Hash)) 
      (,  
        { (generate_switch0  $Clauses $FA $Instrs ()) } 
        (,  
          (generate_bp_label  $Instrs 
            (+  $FA sub) 1 $Ls0 $SWTs) 
          (,  
            { (;  
                (->  
                  (retract  fail_flag) 
                  (= $Ls1 
                    (Cons  
                      (label  
                        (/  fail 0)) $Ls0))) 
                (= $Ls1 $Ls0)) } 
            (,  
              { (length  $Clauses $N) } 
              (,  
                { (generate_cl_label  $FA 1 $N $Ls2) } 
                (,  
                  { (pl2am_append  $Ls1 $Ls2 $Label) } 
                  { (gen_hash  $SWTs $Hash ()) }))))))) True)
;                                         ; generates try, retry, trust, switch_on_term, and switch_on_hash

;                                         ; generates sub-labels for BP

;                                         ; generates fail label (fail_flag may be asserted by generate_switch0/4)

;                                         ; generates labels for clauses

;                                         ; generates new_hash and put_hash instructions for switch_on_hash



  (= 
    (-->  
      (generate_switch0  $Clauses $FA) 
      (,  
        { (get_indices  $Clauses $FA 1 $Is) } 
        (generate_switch1  $Is $FA))) True)

;
;                                         ;; 1st. Indexing

  (= 
    (-->  
      (generate_switch1  $Is $FA) 
      (,  
        { (= $FA 
            (/  $_ 0)) } 
        (,  ! 
          (generate_tries  $Is)))) True)
  (= 
    (-->  
      (generate_switch1  $Is $_) 
      (,  
        { (all_variable_indices  $Is) } 
        (,  ! 
          (generate_tries  $Is)))) True)
  (= 
    (-->  
      (generate_switch1  $Is $FA) 
      (,  
        ( (switch_on_term  $LV $LI $LF $LC $LS $LL)) 
        (,  
          (generate_sw  $Is $FA var $LV () $PIs0) 
          (,  
            (generate_sw  $Is $FA int $LI $PIs0 $PIs1) 
            (,  
              (generate_sw  $Is $FA flo $LF $PIs1 $PIs2) 
              (,  
                (generate_sw  $Is $FA con $LC $PIs2 $PIs3) 
                (,  
                  (generate_sw  $Is $FA str $LS $PIs3 $PIs4) 
                  (generate_sw  $Is $FA lis $LL $PIs4 $_)))))))) True)

  (= 
    (-->  
      (generate_sw  $Is $FA $Tag $L $PIs0 $PIs) 
      (,  
        { (select_indices  $Is $Tag $Is1) } 
        (generate_sw1  $Is1 $FA $Tag $L $PIs0 $PIs))) True)

;
;                                         ;; 2nd. Indexing

  (= 
    (-->  
      (generate_sw1  () $_ $_ 
        (/  fail 0) $PIs $PIs) 
      (,  ! 
        {assert_fail })) True)
  (= 
    (-->  
      (generate_sw1  
        ($I) $_ $_ $L $PIs $PIs) 
      (,  ! 
        { (= $I 
            (Cons  $L $_)) })) True)
  (= 
    (-->  
      (generate_sw1  $Is $FA $Tag $L $PIs0 $PIs) 
      (,  
        { (no_switch_on_hash  $Is $Tag) } 
        (,  ! 
          (generate_sw2  $Is $FA $Tag $L $PIs0 $PIs)))) True)
  (= 
    (-->  
      (generate_sw1  $Is $FA $Tag 
        (+  $FA $Tag) $PIs0 $PIs) 
      (,  
        (generate_sw  $Is $FA nil $L $PIs0 $PIs) 
        (,  
          { (count_unique_hash  $Is $Size $Keys) } 
          (,  
            ( (:  
                (+  $FA $Tag) 
                (switch_on_hash  $Tag $Size $L $HT))) 
            (,  
              { (generate_hash_table  $Keys $Is $LIs) } 
              (generate_hash_tries  $LIs 
                (+  $FA $Tag) 0 $HT)))))) True)


  (= 
    (no-switch-on-hash $Is $Tag) 
    ( (get-atoms  &self 
        (= 
          (pl2am_flag  idx) $_)) 
      (set-det) 
      (or 
        (= $Tag var) 
        (or 
          (= $Tag lis) 
          (or 
            (= $Tag nil) 
            (, 
              (count-unique-hash $Is $C $_) 
              (< $C 2)))))))
  (= 
    (no_switch_on_hash  $_ $_) True)


  (= 
    (-->  
      (generate_sw2  $Is $_ $_ $L $PIs $PIs) 
      (,  
        { (pl2am_member  
            (,  $L $Is) $PIs) } !)) True)
  (= 
    (-->  
      (generate_sw2  $Is $FA $Tag 
        (+  $FA $Tag) $PIs0 
        (Cons  
          (,  
            (+  $FA $Tag) $Is) $PIs0)) 
      (,  
        ( (:  
            (+  $FA $Tag) ())) 
        (generate_tries  $Is))) True)

  (= 
    (-->  
      (generate_hash_tries  () $_ $_ ()) !) True)
  (= 
    (-->  
      (generate_hash_tries  
        (Cons  
          (:  $K ()) $LIs) $L0 $N 
        (Cons  
          (:  $K 
            (/  fail 0)) $Ls)) 
      (,  ! 
        (,  
          {assert_fail } 
          (generate_hash_tries  $LIs $L0 $N $Ls)))) True)
  (= 
    (-->  
      (generate_hash_tries  
        (Cons  
          (:  $K 
            ($I)) $LIs) $L0 $N 
        (Cons  
          (:  $K $L) $Ls)) 
      (,  ! 
        (,  
          { (= $I 
              (Cons  $L $_)) } 
          (generate_hash_tries  $LIs $L0 $N $Ls)))) True)
  (= 
    (-->  
      (generate_hash_tries  
        (Cons  
          (:  $K $Is) $LIs) $L0 $N 
        (Cons  
          (:  $K 
            (+  $L0 $N)) $Ls)) 
      (,  
        ( (:  
            (+  $L0 $N) ())) 
        (,  
          (generate_tries  $Is) 
          (,  
            { (is  $N1 
                (+  $N 1)) } 
            (generate_hash_tries  $LIs $L0 $N1 $Ls))))) True)


  (= 
    (generate-hash-table Nil $_ Nil) 
    (set-det))
  (= 
    (generate-hash-table 
      (Cons  $K $Ks) $Is0 
      (Cons  
        (with_self  $K $Is) $LIs)) 
    ( (select-hash $Is0 $K $Is) (generate-hash-table $Ks $Is0 $LIs)))


  (= 
    (select_hash  () $_ ()) True)
  (= 
    (select-hash 
      (Cons  $I $Is0) $K 
      (Cons  $I $Is)) 
    ( (= $I 
        (:: $_ $_ $Tag $Hash)) 
      (or 
        (= $Tag var) 
        (= $K $Hash)) 
      (set-det) 
      (select-hash $Is0 $K $Is)))
  (= 
    (select-hash 
      (Cons  $_ $Is0) $K $Is) 
    (select-hash $Is0 $K $Is))

;
;                                         ;; Choice Point (try, retry, trust)


  (= 
    (-->  
      (generate_tries  
        (Cons  $I $Is)) 
      (,  
        { (= $I 
            (Cons  $L $_)) } 
        (,  
          ( (try  $L)) 
          (generate_tries1  $Is)))) True)

  (= 
    (-->  
      (generate_tries1  
        ($I)) 
      (,  ! 
        (,  
          { (= $I 
              (Cons  $L $_)) } 
          ( (trust  $L))))) True)
  (= 
    (-->  
      (generate_tries1  
        (Cons  $I $Is)) 
      (,  
        { (= $I 
            (Cons  $L $_)) } 
        (,  
          ( (retry  $L)) 
          (generate_tries1  $Is)))) True)


  (= 
    (get_indices  () $_ $_ ()) True)
  (= 
    (get-indices 
      (Cons  $_ $Clauses) $FA $N 
      (Cons  
        (:: (+ $FA $N)) $Is)) 
    ( (= $FA 
        (/ $_ 0)) 
      (set-det) 
      (is $N1 
        (+ $N 1)) 
      (get-indices $Clauses $FA $N1 $Is)))
  (= 
    (get-indices 
      (Cons  $Clause $Clauses) $FA $N 
      (Cons  
        (:: 
          (+ $FA $N) $A1 $Tag $Hash) $Is)) 
    ( (= $Clause 
        (= $Head $_)) 
      (arg 1 $Head $A1) 
      (get-hash $A1 $Tag $Hash) 
      (is $N1 
        (+ $N 1)) 
      (get-indices $Clauses $FA $N1 $Is)))


  (= 
    (get-hash $X var 0) 
    ( (var $X) (set-det)))
  (= 
    (get-hash $X int $X) 
    ( (integer $X) (set-det)))
  (= 
    (get-hash $X flo $X) 
    ( (float $X) (set-det)))
  (= 
    (get-hash $X con $X) 
    ( (atom $X) (set-det)))
  (= 
    (get-hash $X lis 
      (/ . 2)) 
    ( (= $X 
        (Cons  $_ $_)) (set-det)))
  (= 
    (get-hash $X str 
      (/ $F $A)) 
    ( (functor $X $F $A) (set-det)))


  (= 
    (all_variable_indices  ()) True)
  (= 
    (all-variable-indices (Cons  (:: $_ $_ var $_) $Is)) 
    (all-variable-indices $Is))


  (= 
    (count_unique_hash  () 0 ()) True)
  (= 
    (count-unique-hash 
      (Cons  $I $Is) $C $K) 
    ( (count-unique-hash $Is $C0 $K0) 
      (= $I 
        (:: $_ $_ $Tag $Hash)) 
      (det-if-then-else 
        (or 
          (= $Tag var) 
          (pl2am-member 
            (:: $_ $_ $Tag $Hash) $Is)) 
        (, 
          (= $C $C0) 
          (= $K $K0)) 
        (, 
          (is $C 
            (+ $C0 1)) 
          (= $K 
            (Cons  $Hash $K0))))))


  (= 
    (select_indices  () $_ ()) True)
  (= 
    (select-indices 
      (Cons  $I $Is0) $Tag 
      (Cons  $I $Is)) 
    ( (= $I 
        (Cons  $_ 
          (Cons  $_ 
            (Cons  $T $_)))) 
      (or 
        (= $Tag var) 
        (or 
          (= $Tag $T) 
          (= $T var))) 
      (set-det) 
      (select-indices $Is0 $Tag $Is)))
  (= 
    (select-indices 
      (Cons  $_ $Is0) $Tag $Is) 
    (select-indices $Is0 $Tag $Is))

;
;                                         ;; Assert Fail Flag


  (= 
    (assert-fail) 
    ( (get-atoms  &self 
        (= fail_flag $_)) (set-det)))
  (= 
    (assert-fail) 
    (add-atom  &self fail_flag))

;
;                                         ;; Generate Labels for Backtrack Point


  (= 
    (-->  
      (generate_bp_label  () $_ $_ () ()) !) True)
  (= 
    (-->  
      (generate_bp_label  
        (Cons  $X $Xs) $CL $N $Ls 
        (Cons  $X $Hs)) 
      (,  
        { (= $X 
            (switch_on_hash  $_ $_ $_ $_)) } 
        (,  ! 
          (,  
            ($X) 
            (generate_bp_label  $Xs $CL $N $Ls $Hs))))) True)
  (= 
    (-->  
      (generate_bp_label  
        (Cons  
          (try  $L) $Xs) $CL $N 
        (Cons  
          (label  
            (+  $CL $N)) $Ls) $Hs) 
      (,  ! 
        (,  
          ( (try  $L 
              (+  $CL $N))) 
          (,  
            ( (:  
                (+  $CL $N) ())) 
            (,  
              { (is  $N1 
                  (+  $N 1)) } 
              (generate_bp_label  $Xs $CL $N1 $Ls $Hs)))))) True)
  (= 
    (-->  
      (generate_bp_label  
        (Cons  
          (retry  $L) $Xs) $CL $N 
        (Cons  
          (label  
            (+  $CL $N)) $Ls) $Hs) 
      (,  ! 
        (,  
          ( (retry  $L 
              (+  $CL $N))) 
          (,  
            ( (:  
                (+  $CL $N) ())) 
            (,  
              { (is  $N1 
                  (+  $N 1)) } 
              (generate_bp_label  $Xs $CL $N1 $Ls $Hs)))))) True)
  (= 
    (-->  
      (generate_bp_label  
        (Cons  
          (:  $L $X) $Xs) $_ $_ 
        (Cons  
          (label  $L) $Ls) $Hs) 
      (,  ! 
        (,  
          ( (:  $L ())) 
          (generate_bp_label  
            (Cons  $X $Xs) $L 1 $Ls $Hs)))) True)
  (= 
    (-->  
      (generate_bp_label  
        (Cons  $X $Xs) $CL $N $Ls $Hs) 
      (,  
        ($X) 
        (generate_bp_label  $Xs $CL $N $Ls $Hs))) True)


  (= 
    (generate-cl-label $_ $I $N Nil) 
    ( (> $I $N) (set-det)))
  (= 
    (generate-cl-label $FA $I $N 
      (Cons  
        (label (+ $FA $I)) $Ls)) 
    ( (is $I1 
        (+ $I 1)) (generate-cl-label $FA $I1 $N $Ls)))

;
;                                         ;; Generate Hash instructions for switch_on_hash


  (= 
    (-->  
      (gen_hash  ()) !) True)
  (= 
    (-->  
      (gen_hash  
        (Cons  
          (switch_on_hash  $T $S $_ $H) $Xs)) 
      (,  ! 
        (,  
          ( (new_hash  $T $S)) 
          (,  
            (gen_put_hash  $H $T) 
            (gen_hash  $Xs))))) True)

  (= 
    (-->  
      (gen_put_hash  () $_) !) True)
  (= 
    (-->  
      (gen_put_hash  
        (Cons  
          (:  $K $V) $Xs) $T) 
      (,  
        ( (put_hash  $K $V $T)) 
        (gen_put_hash  $Xs $T))) True)


  (= 
    (replace-hash-keys Nil $_ Nil Nil) 
    (set-det))
  (= 
    (replace-hash-keys 
      (Cons  
        (put-hash $K $L $H) $Xs) $SA $NHs 
      (Cons  
        (put-hash $X $L $H) $PHs)) 
    ( (set-det) 
      (replace-key $K $SA $X) 
      (replace-hash-keys $Xs $SA $NHs $PHs)))
  (= 
    (replace-hash-keys 
      (Cons  $X $Xs) $SA 
      (Cons  $X $NHs) $PHs) 
    (replace-hash-keys $Xs $SA $NHs $PHs))


  (= 
    (replace-key $K $Alloc $X) 
    ( (integer $K) 
      (allocated $Alloc 
        (with_self  $K 
          (int)) 
        (:: $X yes)) 
      (set-det)))
  (= 
    (replace-key $K $Alloc $X) 
    ( (float $K) 
      (allocated $Alloc 
        (with_self  $K 
          (flo)) 
        (:: $X yes)) 
      (set-det)))
  (= 
    (replace-key $K $Alloc $X) 
    ( (atom $K) 
      (allocated $Alloc 
        (with_self  $K 
          (con)) 
        (:: $X yes)) 
      (set-det)))
  (= 
    (replace-key $K $Alloc $X) 
    ( (nonvar $K) 
      (= $K 
        (/ $F $A)) 
      (atom $F) 
      (integer $A) 
      (allocated $Alloc 
        (with_self  $K 
          (con)) 
        (:: $X yes)) 
      (set-det)))
  (= 
    (replace-key $K $_ $_) 
    ( (pl2am-error (:: replacement of hash key $K failed)) (fail)))

;
;                                         ;; Import Declarations


  (= 
    (-->  generate_import 
      (,  
        { (findall  
            (,  $P $C) 
            (import_package  $P $C) $X) } 
        (gen_import  $X))) True)

  (= 
    (-->  
      (gen_import  ()) !) True)
  (= 
    (-->  
      (gen_import  
        (Cons  
          (,  $P *) $Xs)) 
      (,  ! 
        (,  
          ( (import_package  $P)) 
          (gen_import  $Xs)))) True)
  (= 
    (-->  
      (gen_import  
        (Cons  
          (,  $P $C) $Xs)) 
      (,  
        ( (import_package  $P $C)) 
        (gen_import  $Xs))) True)

;
;                                         ;; Information

  (= 
    (-->  
      (generate_info  $Functor $Arity) 
      (,  
        { (clause  
            (file_name  $File) $_) } 
        ( (info  
            ( (/  $Functor $Arity) $File))))) True)

;
;                                         ;; Check the Modifier of Predicate F/A.


  (= 
    (check-modifier 
      (/ %init 0) public) 
    (set-det))
  (= 
    (check-modifier 
      (/ $F $A) public) 
    ( (get-atoms  &self 
        (= 
          (public_predicates  $F $A) $_)) (set-det)))
  (= 
    (check_modifier  $_ 
      (-  non public)) True)

;
;                                         ;; generate a list of registers with given range.


  (= 
    (range-reg $I $N $_ Nil) 
    ( (> $I $N) (set-det)))
  (= 
    (range-reg $I $N $A 
      (Cons  $R $Rs)) 
    ( (=< $I $N) 
      (is $I1 
        (+ $I 1)) 
      (=.. $R 
        (:: $A $I)) 
      (range-reg $I1 $N $A $Rs)))

;
;                                         ;; generate set instructions


  (= 
    (-->  
      (gen_set  () ()) !) True)
  (= 
    (-->  
      (gen_set  
        (Cons  $X $Xs) 
        (Cons  $Y $Ys)) 
      (,  
        ( (set  $X $Y)) 
        (gen_set  $Xs $Ys))) True)

;
;                                         ;; generate deref instructions

  (= 
    (-->  
      (gen_deref  () ()) !) True)
  (= 
    (-->  
      (gen_deref  
        (Cons  $X $Xs) 
        (Cons  $Y $Ys)) 
      (,  
        ( (deref  $X $Y)) 
        (gen_deref  $Xs $Ys))) True)

;
;                                         ;; generate set and deref instructions

  (= 
    (-->  
      (set_arguments  $SN $EN $R1 $R2 $Flag) 
      (,  
        { (range_reg  $SN $EN $R1 $L1) } 
        (,  
          { (range_reg  $SN $EN $R2 $L2) } 
          (gen_set_arg  $Flag $L1 $L2)))) True)

  (= 
    (-->  
      (gen_set_arg  set $L1 $L2) 
      (gen_set  $L1 $L2)) True)
  (= 
    (-->  
      (gen_set_arg  deref $L1 $L2) 
      (gen_deref  $L1 $L2)) True)

;
;                                         ;; generate decl_var instructions

  (= 
    (-->  
      (generate_var_decl  
        ($X0 $P0) 
        ($XN $PN)) 
      (,  
        { (,  
            (is  $X1 
              (-  $XN 1)) 
            (is  $P1 
              (-  $PN 1))) } 
        (,  
          { (range_reg  $X0 $X1 a $XL) } 
          (,  
            { (range_reg  $P0 $P1 p $PL) } 
            (,  
              (gen_decl_term_vars  $XL) 
              (gen_decl_pred_vars  $PL)))))) True)

  (= 
    (-->  
      (gen_decl_term_vars  ()) !) True)
  (= 
    (-->  
      (gen_decl_term_vars  $XL) 
      ( (decl_term_vars  $XL))) True)

  (= 
    (-->  
      (gen_decl_pred_vars  ()) !) True)
  (= 
    (-->  
      (gen_decl_pred_vars  $PL) 
      ( (decl_pred_vars  $PL))) True)

  (= 
    (-->  
      (compile_clause  
        (:-  $Head $Body) $GTI0 $GTI $LTI) 
      (,  
        { (pretreat_body  $Body $Goals0) } 
        (,  
          { (localize_meta  $Goals0 $Goals) } 
          (,  
            { (precompile  $Head $Goals $Instrs) } 
            (,  
              ( (comment  
                  (:-  $Head $Goals))) 
              (,  
                (compile_chunks  $Instrs $GTI0 $GTI $LTI) !)))))) True)
;                                         /****************************************************************
;                                           Compile Clause
;                                         ****************************************************************/

;                                         ; cut, rename, compile aith exp.

;                                         ; add package name for meta predicates

;                                         ; generate get, put, put_clo, put_cont, inline

;                                         ; output precompiled clause


  (= 
    (-->  
      (compile_clause  $Clause $_ $_ $_) 
      (,  
        { (pl2am_error  
            (compilation of $Clause failed)) } 
        {fail })) True)

;
;                                         ;;;;;;;;; Pretreat Body and Compile Arithmetic Expressions


  (= 
    (pretreat-body $Body $Goals) 
    ( (pretreat-body0 $Body $Cut $Goals0 Nil) (pretreat-cut $Cut $Goals0 $Goals)))


  (= 
    (pretreat-cut $Cut $Gs $Gs) 
    ( (var $Cut) (set-det)))
  (= 
    (pretreat-cut 
      ($cut $Level) 
      (Cons  
        ($cut $Level) $Gs) 
      (Cons  %neck-cut $Gs)) 
    ( (not (pl2am-member ($cut $Level) $Gs)) (set-det)))
  (= 
    (pretreat-cut 
      ($cut $Level) 
      (Cons  
        ($cut $Level) $Gs) 
      (Cons  
        ($get-level $Level) 
        (Cons  %neck-cut $Gs))) 
    (set-det))
  (= 
    (pretreat_cut  
      ($cut  $Level) $Gs 
      (Cons  
        ($get_level  $Level) $Gs)) True)


  (= 
    (-->  
      (pretreat_body0  $G $_) 
      (,  
        { (var  $G) } 
        (,  ! 
          ( (call  $G))))) True)
  (= 
    (-->  
      (pretreat_body0  ! $Cut) 
      (,  ! 
        (,  
          { (= $Cut 
              ($cut  $Level)) } 
          ( ($cut  $Level))))) True)
  (= 
    (-->  
      (pretreat_body0  otherwise $_) !) True)
  (= 
    (-->  
      (pretreat_body0  true $_) !) True)
  (= 
    (-->  
      (pretreat_body0  fail $_) 
      (,  ! 
        (fail))) True)
  (= 
    (-->  
      (pretreat_body0  false $_) 
      (,  ! 
        (fail))) True)
  (= 
    (-->  
      (pretreat_body0  halt $_) 
      (,  ! 
        (halt))) True)
  (= 
    (-->  
      (pretreat_body0  abort $_) 
      (,  ! 
        (abort))) True)
  (= 
    (-->  
      (pretreat_body0  
        (,  $G1 $G2) $Cut) 
      (,  ! 
        (,  
          (pretreat_body0  $G1 $Cut) 
          (pretreat_body0  $G2 $Cut)))) True)
  (= 
    (-->  
      (pretreat_body0  $G $_) 
      (,  
        (pretreat_builtin  $G) !)) True)
  (= 
    (-->  
      (pretreat_body0  $G $_) 
      (,  
        { (,  
            (functor  $G $F $A) 
            (,  
              (clause  
                (dynamic_predicates  $F $A 
                  (:  $_ $Call)) $_) 
              (=..  $CG 
                ($Call $G)))) } 
        (,  ! 
          ($CG)))) True)
  (= 
    (-->  
      (pretreat_body0  
        (findall  $X $G $L) $Z) 
      (,  
        { (,  
            (nonvar  $G) 
            (,  
              (functor  $G $F $A) 
              (,  
                (clause  
                  (dynamic_predicates  $F $A $Call) $_) 
                (,  
                  (:  
                    (\==  $Call SxxMachine.builtin) call) 
                  (,  
                    (:  
                      (= $Call $P) $C) 
                    (,  
                      (clause  
                        (package_name  $P1) $_) 
                      (=..  $CG 
                        ($C (:  $P1 $G))))))))) } 
        (,  ! 
          (pretreat_body0  
            (findall  $X 
              (:  $P $CG) $L) $Z)))) True)

  (= 
    (-->  
      (pretreat_body0  $G $_) 
      ($G)) True)

;
;                                         ;; rename builtins

  (= 
    (-->  
      (pretreat_builtin  
        (= $X $Y)) 
      (,  ! 
        ( ($unify  $X $Y)))) True)
  (= 
    (-->  
      (pretreat_builtin  
        (\=  $X $Y)) 
      (,  ! 
        ( ($not_unifiable  $X $Y)))) True)
  (= 
    (-->  
      (pretreat_builtin  
        (==  $X $Y)) 
      (,  ! 
        ( ($equality_of_term  $X $Y)))) True)
  (= 
    (-->  
      (pretreat_builtin  
        (\==  $X $Y)) 
      (,  ! 
        ( ($inequality_of_term  $X $Y)))) True)
  (= 
    (-->  
      (pretreat_builtin  
        (?=  $X $Y)) 
      (,  ! 
        ( ($identical_or_cannot_unify  $X $Y)))) True)
  (= 
    (-->  
      (pretreat_builtin  
        (@<  $X $Y)) 
      (,  ! 
        ( ($before  $X $Y)))) True)
  (= 
    (-->  
      (pretreat_builtin  
        (@>  $X $Y)) 
      (,  ! 
        ( ($after  $X $Y)))) True)
  (= 
    (-->  
      (pretreat_builtin  
        (@=<  $X $Y)) 
      (,  ! 
        ( ($not_after  $X $Y)))) True)
  (= 
    (-->  
      (pretreat_builtin  
        (@>=  $X $Y)) 
      (,  ! 
        ( ($not_before  $X $Y)))) True)
  (= 
    (-->  
      (pretreat_builtin  
        (compare  $Op $X $Y)) 
      (,  
        { (==  $Op =) } 
        (,  ! 
          ( ($equality_of_term  $X $Y))))) True)
  (= 
    (-->  
      (pretreat_builtin  
        (compare  $Op $X $Y)) 
      (,  
        { (==  $Op <) } 
        (,  ! 
          ( ($before  $X $Y))))) True)
  (= 
    (-->  
      (pretreat_builtin  
        (compare  $Op $X $Y)) 
      (,  
        { (==  $Op >) } 
        (,  ! 
          ( ($after  $X $Y))))) True)
  (= 
    (-->  
      (pretreat_builtin  
        (=..  $X $Y)) 
      (,  ! 
        ( ($univ  $X $Y)))) True)
  (= 
    (-->  
      (pretreat_builtin  
        (=:=  $X $Y)) 
      (,  ! 
        (,  
          (pretreat_is  $U $X) 
          (,  
            (pretreat_is  $V $Y) 
            ( ($arith_equal  $U $V)))))) True)
  (= 
    (-->  
      (pretreat_builtin  
        (=\=  $X $Y)) 
      (,  ! 
        (,  
          (pretreat_is  $U $X) 
          (,  
            (pretreat_is  $V $Y) 
            ( ($arith_not_equal  $U $V)))))) True)
  (= 
    (-->  
      (pretreat_builtin  
        (>  $X $Y)) 
      (,  ! 
        (,  
          (pretreat_is  $U $X) 
          (,  
            (pretreat_is  $V $Y) 
            ( ($greater_than  $U $V)))))) True)
  (= 
    (-->  
      (pretreat_builtin  
        (>=  $X $Y)) 
      (,  ! 
        (,  
          (pretreat_is  $U $X) 
          (,  
            (pretreat_is  $V $Y) 
            ( ($greater_or_equal  $U $V)))))) True)
  (= 
    (-->  
      (pretreat_builtin  
        (<  $X $Y)) 
      (,  ! 
        (,  
          (pretreat_is  $U $X) 
          (,  
            (pretreat_is  $V $Y) 
            ( ($less_than  $U $V)))))) True)
  (= 
    (-->  
      (pretreat_builtin  
        (=<  $X $Y)) 
      (,  ! 
        (,  
          (pretreat_is  $U $X) 
          (,  
            (pretreat_is  $V $Y) 
            ( ($less_or_equal  $U $V)))))) True)
  (= 
    (-->  
      (pretreat_builtin  
        (is  $Z $X)) 
      (,  ! 
        (pretreat_is0  $Z $X))) True)

  (= 
    (-->  
      (pretreat_is  $Z $X) 
      (,  
        { (var  $X) } 
        (,  ! 
          { (= $X $Z) }))) True)
  (= 
    (-->  
      (pretreat_is  $Z $X) 
      (pretreat_is0  $Z $X)) True)

  (= 
    (-->  
      (pretreat_is0  $Z $X) 
      (,  
        { (clause  
            (pl2am_flag  ac) $_) } 
        (,  ! 
          (precompile_is  $X $Z)))) True)
  (= 
    (-->  
      (pretreat_is0  $Z $X) 
      ( (is  $Z $X))) True)

;
;                                         ;; compile aithmetic expressions

  (= 
    (-->  
      (precompile_is  $X $A) 
      (,  
        { (var  $X) } 
        (,  ! 
          ( (is  $A $X))))) True)
  (= 
    (-->  
      (precompile_is  $X $A) 
      (,  
        { (number  $X) } 
        (,  ! 
          { (= $X $A) }))) True)
  (= 
    (-->  
      (precompile_is  $X $A) 
      (,  
        { (builtin_arith_constant  $X) } 
        (,  ! 
          { (= $X $A) }))) True)
  (= 
    (-->  
      (precompile_is  
        (+  $X) $A) 
      (,  ! 
        (precomp_is  $X $A))) True)
  (= 
    (-->  
      (precompile_is  
        (-  $X) $A) 
      (,  ! 
        (precompile_is  
          (*  -1 $X) $A))) True)
  (= 
    (-->  
      (precompile_is  
        (+  $X $Y) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          (,  
            (precomp_is  $Y $V) 
            ( ($plus  $U $V $A)))))) True)
  (= 
    (-->  
      (precompile_is  
        (-  $X $Y) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          (,  
            (precomp_is  $Y $V) 
            ( ($minus  $U $V $A)))))) True)
  (= 
    (-->  
      (precompile_is  
        (*  $X $Y) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          (,  
            (precomp_is  $Y $V) 
            ( ($multi  $U $V $A)))))) True)
  (= 
    (-->  
      (precompile_is  
        (/  $X $Y) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          (,  
            (precomp_is  $Y $V) 
            ( ($float_quotient  $U $V $A)))))) True)
  (= 
    (-->  
      (precompile_is  
        (//  $X $Y) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          (,  
            (precomp_is  $Y $V) 
            ( ($int_quotient  $U $V $A)))))) True)
  (= 
    (-->  
      (precompile_is  
        (mod  $X $Y) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          (,  
            (precomp_is  $Y $V) 
            ( ($mod  $U $V $A)))))) True)
  (= 
    (-->  
      (precompile_is  
        (rem  $X $Y) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          (,  
            (precomp_is  $Y $V) 
            ( ($mod  $U $V $A)))))) True)
  (= 
    (-->  
      (precompile_is  
        (/\  $X $Y) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          (,  
            (precomp_is  $Y $V) 
            ( ($bitwise_conj  $U $V $A)))))) True)
  (= 
    (-->  
      (precompile_is  
        (\/  $X $Y) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          (,  
            (precomp_is  $Y $V) 
            ( ($bitwise_disj  $U $V $A)))))) True)
  (= 
    (-->  
      (precompile_is  
        (#  $X $Y) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          (,  
            (precomp_is  $Y $V) 
            ( ($bitwise_exclusive_or  $U $V $A)))))) True)
  (= 
    (-->  
      (precompile_is  
        (\  $X) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          ( ($bitwise_neg  $U $A))))) True)
  (= 
    (-->  
      (precompile_is  
        (<<  $X $Y) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          (,  
            (precomp_is  $Y $V) 
            ( ($shift_left  $U $V $A)))))) True)
  (= 
    (-->  
      (precompile_is  
        (>>  $X $Y) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          (,  
            (precomp_is  $Y $V) 
            ( ($shift_right  $U $V $A)))))) True)
  (= 
    (-->  
      (precompile_is  
        ($X) $A) 
      (,  ! 
        (precomp_is  $X $A))) True)
  (= 
    (-->  
      (precompile_is  
        (abs  $X) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          ( ($abs  $U $A))))) True)
  (= 
    (-->  
      (precompile_is  
        (min  $X $Y) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          (,  
            (precomp_is  $Y $V) 
            ( ($min  $U $V $A)))))) True)
  (= 
    (-->  
      (precompile_is  
        (max  $X $Y) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          (,  
            (precomp_is  $Y $V) 
            ( ($max  $U $V $A)))))) True)
  (= 
    (-->  
      (precompile_is  
        (round  $X) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          ( ($round  $U $A))))) True)
  (= 
    (-->  
      (precompile_is  
        (floor  $X) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          ( ($floor  $U $A))))) True)
  (= 
    (-->  
      (precompile_is  
        (ceiling  $X) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          ( ($ceil  $U $A))))) True)
  (= 
    (-->  
      (precompile_is  
        (sin  $X) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          ( ($sin  $U $A))))) True)
  (= 
    (-->  
      (precompile_is  
        (cos  $X) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          ( ($cos  $U $A))))) True)
  (= 
    (-->  
      (precompile_is  
        (tan  $X) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          ( ($tan  $U $A))))) True)
  (= 
    (-->  
      (precompile_is  
        (asin  $X) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          ( ($asin  $U $A))))) True)
  (= 
    (-->  
      (precompile_is  
        (acos  $X) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          ( ($acos  $U $A))))) True)
  (= 
    (-->  
      (precompile_is  
        (atan  $X) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          ( ($atan  $U $A))))) True)
  (= 
    (-->  
      (precompile_is  
        (sqrt  $X) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          ( ($sqrt  $U $A))))) True)
  (= 
    (-->  
      (precompile_is  
        (log  $X) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          ( ($log  $U $A))))) True)
  (= 
    (-->  
      (precompile_is  
        (exp  $X) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          ( ($exp  $U $A))))) True)
  (= 
    (-->  
      (precompile_is  
        (**  $X $Y) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          (,  
            (precomp_is  $Y $V) 
            ( ($pow  $U $V $A)))))) True)
  (= 
    (-->  
      (precompile_is  
        (degrees  $X) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          ( ($degrees  $U $A))))) True)
  (= 
    (-->  
      (precompile_is  
        (radians  $X) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          ( ($radians  $U $A))))) True)
  (= 
    (-->  
      (precompile_is  
        (rint  $X) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          ( ($rint  $U $A))))) True)
  (= 
    (-->  
      (precompile_is  
        (float  $X) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          ( ($float  $U $A))))) True)
  (= 
    (-->  
      (precompile_is  
        (float_integer_part  $X) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          ( ($float_integer_part  $U $A))))) True)
  (= 
    (-->  
      (precompile_is  
        (float_fractional_part  $X) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          ( ($float_fractional_part  $U $A))))) True)
  (= 
    (-->  
      (precompile_is  
        (truncate  $X) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          ( ($truncate  $U $A))))) True)
  (= 
    (-->  
      (precompile_is  
        (sign  $X) $A) 
      (,  ! 
        (,  
          (precomp_is  $X $U) 
          ( ($sign  $U $A))))) True)
  (= 
    (-->  
      (precompile_is  $X $_) 
      (,  
        { (pl2am_error  
            (unknown arithemetic expression $X)) } 
        {fail })) True)

  (= 
    (-->  
      (precomp_is  $X $A) 
      (,  
        { (var  $X) } 
        (,  
          { (var  $A) } 
          (,  ! 
            { (= $X $A) })))) True)
  (= 
    (-->  
      (precomp_is  $X $A) 
      (precompile_is  $X $A)) True)

;
;                                         ;;;;;;;;; Add Pacakge (module) name to meta predicates


  (= 
    (localize-meta $G0 $G) 
    ( (get-atoms  &self 
        (= 
          (package_name  $P) $_)) 
      (localize-meta $G0 $P $G) 
      (set-det)))

  (= 
    (localize-meta Nil $_ Nil) 
    (set-det))
  (= 
    (localize-meta 
      (Cons  $G $Gs) $P 
      (Cons  $G1 $Gs1)) 
    ( (localize-meta-goal $G $P $X) 
      (det-if-then-else 
        (with_self  
          (= $X $P) $Y) 
        (= $G1 $Y) 
        (= $G1 $X)) 
      (localize-meta $Gs $P $Gs1)))


  (= 
    (localize-meta-goal $G $P $G1) 
    ( (var $G) 
      (set-det) 
      (localize-meta-goal 
        (call $G) $P $G1)))
  (= 
    (localize-meta-goal 
      (with_self  $P $G) $_ $G1) 
    ( (set-det) (localize-meta-goal $G $P $G1)))
  (= 
    (localize-meta-goal 
      (, $X $Y) $P 
      (, $X1 $Y1)) 
    ( (set-det) 
      (localize-meta-goal $X $P $X1) 
      (localize-meta-goal $Y $P $Y1)))
  (= 
    (localize-meta-goal 
      (det-if-then $X $Y) $P 
      (det-if-then $X1 $Y1)) 
    ( (set-det) 
      (localize-meta-goal $X $P $X1) 
      (localize-meta-goal $Y $P $Y1)))
  (= 
    (localize-meta-goal 
      (or $X $Y) $P 
      (or $X1 $Y1)) 
    ( (set-det) 
      (localize-meta-goal $X $P $X1) 
      (localize-meta-goal $Y $P $Y1)))
  (= 
    (localize-meta-goal $G $P $G1) 
    ( (functor $G $F $A) 
      (or 
        (get-atoms  &self 
          (= 
            (meta_predicates  $F $A $M) $_)) 
        (builtin-local-predicates $F $A $M)) 
      (set-det) 
      (=.. $G 
        (Cons  $F $As)) 
      (localize-meta-args $M $As $P $As1) 
      (=.. $G1 
        (Cons  $F $As1))))
  (= 
    (localize-meta-goal $G $P 
      (call (with_self  $P $G))) 
    ( (var $P) (set-det)))
  (= 
    (localize-meta-goal $G $_ $G) 
    ( (system-predicate $G) (set-det)))
  (= 
    (localize_meta_goal  $G $P 
      (:  $P $G)) True)


  (= 
    (localize-meta-args Nil Nil $_ Nil) 
    (set-det))
  (= 
    (localize-meta-args 
      (Cons  : $Ms) 
      (Cons  $A $As) $P 
      (Cons  
        (with_self  $P $A) $As1)) 
    ( (or 
        (var $A) 
        (with_self  
          (\= $A $_) $_)) 
      (set-det) 
      (localize-meta-args $Ms $As $P $As1)))
  (= 
    (localize-meta-args 
      (Cons  or $Ms) 
      (Cons  $A $As) $P 
      (Cons  
        (with_self  $P $A) $As1)) 
    ( (or 
        (var $A) 
        (with_self  
          (\= $A $_) $_)) 
      (set-det) 
      (localize-meta-args $Ms $As $P $As1)))
  (= 
    (localize-meta-args 
      (Cons  $_ $Ms) 
      (Cons  $A $As) $P 
      (Cons  $A $As1)) 
    (localize-meta-args $Ms $As $P $As1))

;
;                                         ;;;;;;;;; Precompile Clause and Optimize Recursive Call


  (= 
    (precompile $Head $Goals $Instrs) 
    ( (precompile-head $Head $Instrs0 $Bs) 
      (precompile-body $Goals $Bs Nil) 
      (optimize-recursive-call $Head $Instrs0 $Instrs)))

;
;                                         ;; Precompile head (generates get instructions)


  (= 
    (-->  
      (precompile_head  $Head) 
      (,  
        { (=..  $Head 
            (Cons  $_ $Args)) } 
        (precomp_head  $Args 1))) True)

  (= 
    (-->  
      (precomp_head  () $_) !) True)
  (= 
    (-->  
      (precomp_head  
        (Cons  $A $As) $I) 
      (,  
        ( (get  $A 
            (a  $I))) 
        (,  
          { (is  $I1 
              (+  $I 1)) } 
          (precomp_head  $As $I1)))) True)

;
;                                         ;; Precompile body

;
;                                         ;; (generates put, put_clo, put_cont, and inline instructions)

  (= 
    (-->  
      (precompile_body  $Goals) 
      (,  
        { (clause  
            (pl2am_flag  ie) $_) } 
        (,  ! 
          (,  
            { (pickup_inline_goals  $Goals $IGs $Gs) } 
            (precomp_inline  $IGs $Gs))))) True)
  (= 
    (-->  
      (precompile_body  $Goals) 
      (precomp_body  $Goals)) True)

  (= 
    (-->  
      (precomp_body  ()) 
      (,  ! 
        ( (execute  cont)))) True)
  (= 
    (-->  
      (precomp_body  
        (Cons  
          (:  $M $G) $Cont)) 
      (,  ! 
        (,  
          (binarize_body  $G $Cont $G1) 
          ( (execute  
              (:  $M $G1)))))) True)
  (= 
    (-->  
      (precomp_body  
        (Cons  $G $Cont)) 
      (,  
        (binarize_body  $G $Cont $G1) 
        ( (execute  $G1)))) True)

  (= 
    (-->  
      (binarize_body  $G $Cont $G1) 
      (,  
        { (=..  $G 
            (Cons  $F $Args)) } 
        (,  
          { (functor  $G $F $A) } 
          (,  
            (precomp_call  $Args $Us $F $A) 
            (,  
              (precomp_cont  $Cont $V) 
              (,  
                { (pl2am_append  $Us 
                    ($V) $Ws) } 
                { (=..  $G1 
                    (Cons  $F $Ws)) })))))) True)
;                                         /*---------------------------------------------------------------
;                                          Binarization technique was developed by P.Tarau and M.Boyer,
;                                          please see:
;                                            * "Elementary Logic Programs"
;                                              P.Tarau and M.Boyer
;                                              Programming Language Implementation and Logic Programming,
;                                              pp.159--173, LNCS 456, Springer Verlag, 1990
;                                         ----------------------------------------------------------------*/

;                                         ;precomp_call(Args, Us), ; for no closure



  (= 
    (-->  
      (precomp_call  () ()) !) True)
  (= 
    (-->  
      (precomp_call  
        (Cons  $A $As) 
        (Cons  $U $Us)) 
      (,  
        ( (put  $A $U)) 
        (precomp_call  $As $Us))) True)

  (= 
    (-->  
      (precomp_cont  () cont) !) True)
  (= 
    (-->  
      (precomp_cont  
        (Cons  
          (:  $M $G) $Cont) $V) 
      (,  ! 
        (,  
          (binarize_body  $G $Cont $G1) 
          ( (put_cont  
              (:  $M $G1) $V))))) True)
  (= 
    (-->  
      (precomp_cont  
        (Cons  $G $Cont) $V) 
      (,  
        (binarize_body  $G $Cont $G1) 
        ( (put_cont  $G1 $V)))) True)

  (= 
    (-->  
      (precomp_inline  () $Gs1) 
      (,  ! 
        (precomp_body  $Gs1))) True)
  (= 
    (-->  
      (precomp_inline  
        (Cons  fail $_) $_) 
      (,  ! 
        ( (inline  fail)))) True)
  (= 
    (-->  
      (precomp_inline  
        (Cons  $G $Gs) $Gs1) 
      (,  
        { (=..  $G 
            (Cons  $F $Args)) } 
        (,  
          { (functor  $G $F $A) } 
          (,  
            (precomp_call  $Args $Us $F $A) 
            (,  
              { (=..  $G1 
                  (Cons  $F $Us)) } 
              (,  
                ( (inline  $G1)) 
                (precomp_inline  $Gs $Gs1))))))) True)
;                                         ;precomp_call(Args, Us),




  (= 
    (pickup-inline-goals Nil Nil Nil) 
    (set-det))
  (= 
    (pickup-inline-goals 
      (Cons  $G $Gs) 
      (Cons  $G $IGs) $BGs) 
    ( (builtin-inline-predicates $G) 
      (set-det) 
      (pickup-inline-goals $Gs $IGs $BGs)))
  (= 
    (pickup_inline_goals  $Gs () $Gs) True)

;
;                                         ;; Generate Closure


  (= 
    (-->  
      (precomp_call  $As $Us $Functor $Arity) 
      (,  
        { (clause  
            (pl2am_flag  clo) $_) } 
        (,  
          { (clause  
              (meta_predicates  $Functor $Arity $Mode) $_) } 
          (,  ! 
            (,  
              { (clause  
                  (package_name  $P) $_) } 
              (precomp_closure  $Mode $As $P $Us)))))) True)
  (= 
    (-->  
      (precomp_call  $As $Us $_ $_) 
      (precomp_call  $As $Us)) True)

  (= 
    (-->  
      (precomp_closure  () () $_ ()) !) True)
  (= 
    (-->  
      (precomp_closure  
        (Cons  : $Ms) 
        (Cons  $A $As) $P 
        (Cons  $U $Us)) 
      (,  
        { (get_closure  $A $P $C) } 
        (,  ! 
          (,  
            ( (put_clo  $C $U)) 
            (precomp_closure  $Ms $As $P $Us))))) True)
  (= 
    (-->  
      (precomp_closure  
        (Cons  $_ $Ms) 
        (Cons  $A $As) $P 
        (Cons  $U $Us)) 
      (,  
        ( (put  $A $U)) 
        (precomp_closure  $Ms $As $P $Us))) True)


  (= 
    (get-closure $G $_ $_) 
    ( (var $G) 
      (set-det) 
      (fail)))
  (= 
    (get-closure $_ $P $_) 
    ( (var $P) 
      (set-det) 
      (fail)))
  (= 
    (get-closure 
      (with_self  $P $G) $_ $Clo) 
    ( (set-det) (get-closure $G $P $Clo)))
  (= 
    (get-closure $G $P 
      (with_self  $P $G)) 
    ( (atom $P) 
      (callable $G) 
      (functor $G $F $A) 
      (not (get-atoms  &self (= (dynamic_predicates  $F $A $_) $_))) 
      (set-det)))
;                                         ; ???



;
;                                         ;; Optimize Recursive Call


  (= 
    (optimize-recursive-call $Head $Instrs0 $Instrs) 
    ( (get-atoms  &self 
        (= 
          (pl2am_flag  rc) $_)) 
      (set-det) 
      (optimize-rc $Instrs0 $Head $Instrs Nil)))
  (= 
    (optimize_recursive_call  $_ $Instrs $Instrs) True)


  (= 
    (-->  
      (optimize_rc  () $_) !) True)
  (= 
    (-->  
      (optimize_rc  
        (Cons  
          (execute  $Goal) $Xs) $Head) 
      (,  
        { (functor  $Head $F $A) } 
        (,  
          { (functor  $Goal $F $A1) } 
          (,  
            { (=:=  
                (+  $A 1) $A1) } 
            (,  ! 
              (,  
                { (assert_copts  
                    (rc  $F $A)) } 
                (,  
                  { (=..  $Goal 
                      (Cons  $F $Args)) } 
                  (,  
                    { (range_reg  1 $A ea $Rs0) } 
                    (,  
                      { (pl2am_append  $Rs0 
                          (econt) $Rs) } 
                      (,  
                        (gen_set  $Args $Rs) 
                        (,  
                          ( (goto  
                              (+  
                                (/  $F $A) top))) 
                          (optimize_rc  $Xs $Head)))))))))))) True)
  (= 
    (-->  
      (optimize_rc  
        (Cons  $X $Xs) $Head) 
      (,  
        ($X) 
        (optimize_rc  $Xs $Head))) True)

;
;                                         ;;;;;;;;; Compile Clause

  (= 
    (-->  
      (compile_chunks  $Chunk $GTI0 $GTI $LTI) 
      (,  
        { (alloc_voids  $Chunk () $Alloc) } 
        (compile_chunk  $Chunk $Alloc $GTI0 $GTI $LTI))) True)
;                                         ; check void variables



  (= 
    (-->  
      (compile_chunk  () $_ $GTI $GTI ()) !) True)
  (= 
    (-->  
      (compile_chunk  $Chunk $Alloc $GTI0 $GTI $LTI) 
      (,  
        { (,  
            (free_x_reg  $Chunk 1 $XN) 
            (,  
              (= $YN 1) 
              (= $PN 1))) } 
        (,  
          { (= $LTI0 
              ($XN $YN $PN $Alloc)) } 
          (comp_chunk  $Chunk $LTI0 $LTI $GTI0 $GTI)))) True)

  (= 
    (-->  
      (comp_chunk  () $LTI $LTI $GTI $GTI) !) True)
  (= 
    (-->  
      (comp_chunk  
        (Cons  
          (:  $L ()) $Cs) $LTI0 $LTI $GTI0 $GTI) 
      (,  ! 
        (,  
          ( (:  $L ())) 
          (comp_chunk  $Cs $LTI0 $LTI $GTI0 $GTI)))) True)
  (= 
    (-->  
      (comp_chunk  
        (Cons  
          (:  $L $C) $Cs) $LTI0 $LTI $GTI0 $GTI) 
      (,  ! 
        (,  
          ( (:  $L ())) 
          (comp_chunk  
            (Cons  $C $Cs) $LTI0 $LTI $GTI0 $GTI)))) True)
  (= 
    (-->  
      (comp_chunk  
        (Cons  $C $Cs) $LTI0 $LTI $GTI0 $GTI) 
      (,  ! 
        (,  
          (comp_instr  $C $LTI0 $LTI1 $GTI0 $GTI1) 
          (comp_chunk  $Cs $LTI1 $LTI $GTI1 $GTI)))) True)

;
;                                         ;; finds an available number A-register


  (= 
    (free_x_reg  () $XN $XN) True)
  (= 
    (free-x-reg 
      (Cons  
        (get $_ $V) $Cs) $XN0 $XN) 
    ( (nonvar $V) 
      (= $V 
        (a $N)) 
      (set-det) 
      (is $XN1 
        (max 
          (+ $N 1) $XN0)) 
      (free-x-reg $Cs $XN1 $XN)))
  (= 
    (free-x-reg 
      (Cons  
        (put $_ $V) $Cs) $XN0 $XN) 
    ( (nonvar $V) 
      (= $V 
        (a $N)) 
      (set-det) 
      (is $XN1 
        (max 
          (+ $N 1) $XN0)) 
      (free-x-reg $Cs $XN1 $XN)))
  (= 
    (free-x-reg 
      (Cons  $_ $Cs) $XN0 $XN) 
    (free-x-reg $Cs $XN0 $XN))

;
;                                         ;; finds void variables and allocates them in Alloc.


  (= 
    (alloc-voids $Chunks $Alloc0 $Alloc) 
    ( (variables $Chunks $Vars) (alloc-voids1 $Vars $Chunks $Alloc0 $Alloc)))


  (= 
    (alloc_voids1  () $_ $Alloc $Alloc) True)
  (= 
    (alloc-voids1 
      (Cons  $V $Vars) $Chunks $Alloc0 $Alloc) 
    ( (count-variable $V $Chunks 1) 
      (set-det) 
      (= $Alloc1 
        (Cons  
          (:: $V void $Seen) $Alloc0)) 
      (alloc-voids1 $Vars $Chunks $Alloc1 $Alloc)))
  (= 
    (alloc-voids1 
      (Cons  $_ $Vars) $Chunks $Alloc0 $Alloc) 
    (alloc-voids1 $Vars $Chunks $Alloc0 $Alloc))

;
;                                         ;;;;;;;;; Compile Precompiled Instructions: get, put, put_clo, and put_cont


  (= 
    (-->  
      (comp_instr  
        (get  $X $A) $LTI0 $LTI $GTI0 $GTI) 
      (,  ! 
        (gen_get  $X $A $LTI0 $LTI $GTI0 $GTI))) True)
;                                         /*
;                                           comp_instr(+Instr, +LTI0, ?LTI, +GTI0, ?GTI)
;                                           Instr    : Intermediate instruction
;                                           LTI      : [XN, YN, PN, Alloc]
;                                            XN      : The register a(XN) is available for "Term".
;                                            YN      : The register y(YN) is available for "Term[]".
;                                            PN      : The register p(PN) is available for "Predicate".
;                                            Alloc   : [[VarTerm, Register, Seen],...]
;                                           GTI      : [SN, SAlloc, SInstrs]
;                                            SN      : The registers s(SN), si(SN), or sf(SN) are available for static "Term".
;                                            SAlloc  : [[NonVarTerm:Type, Register, Seen],...]
;                                            SInstrs : list of instructions for static terms.
;                                           Seen     : Unbound variable | yes | void
;                                           Type     : int | flo | con | str | lis | arr
;                                         */


  (= 
    (-->  
      (comp_instr  
        (put  $X $V) $LTI0 $LTI $GTI0 $GTI) 
      (,  ! 
        (gen_put  $X $V $LTI0 $LTI $GTI0 $GTI))) True)
  (= 
    (-->  
      (comp_instr  
        (put_clo  $X $V) $LTI0 $LTI $GTI0 $GTI) 
      (,  ! 
        (gen_put_clo  $X $V $LTI0 $LTI $GTI0 $GTI))) True)
  (= 
    (-->  
      (comp_instr  
        (put_cont  $X $V) $LTI0 $LTI $GTI0 $GTI) 
      (,  ! 
        (gen_put_cont  $X $V $LTI0 $LTI $GTI0 $GTI))) True)
  (= 
    (-->  
      (comp_instr  $Instr $LTI $LTI $GTI $GTI) 
      ($Instr)) True)

;
;                                         ;;;;;;;;; put instructions

  (= 
    (-->  
      (gen_put  $_ $A $_ $_ $_ $_) 
      (,  
        { (nonvar  $A) } 
        (,  ! 
          (,  
            { (pl2am_error  
                ($A should be an unbound variable)) } 
            {fail })))) True)
  (= 
    (-->  
      (gen_put  $X $A $LTI0 $LTI $GTI $GTI) 
      (,  
        { (var  $X) } 
        (,  ! 
          (,  
            { (assign_reg  $X $R $Seen $LTI0 $LTI) } 
            (gen_put_var  $R $Seen $A))))) True)
  (= 
    (-->  
      (gen_put  $X $A $LTI $LTI $GTI0 $GTI) 
      (,  
        { (integer  $X) } 
        (,  ! 
          (,  
            { (assign_sreg  
                (:  $X int) $R $Seen $GTI0 $GTI1) } 
            (gen_put_int  $X $R $Seen $A $GTI1 $GTI))))) True)
  (= 
    (-->  
      (gen_put  $X $A $LTI $LTI $GTI0 $GTI) 
      (,  
        { (long  $X) } 
        (,  ! 
          (,  
            { (assign_sreg  
                (:  $X long) $R $Seen $GTI0 $GTI1) } 
            (gen_put_int  $X $R $Seen $A $GTI1 $GTI))))) True)
  (= 
    (-->  
      (gen_put  $X $A $LTI $LTI $GTI0 $GTI) 
      (,  
        { (float  $X) } 
        (,  ! 
          (,  
            { (assign_sreg  
                (:  $X flo) $R $Seen $GTI0 $GTI1) } 
            (gen_put_float  $X $R $Seen $A $GTI1 $GTI))))) True)
  (= 
    (-->  
      (gen_put  $X $A $LTI $LTI $GTI0 $GTI) 
      (,  
        { (atom  $X) } 
        (,  ! 
          (,  
            { (assign_sreg  
                (:  $X con) $R $Seen $GTI0 $GTI1) } 
            (gen_put_con  $X $R $Seen $A $GTI1 $GTI))))) True)
  (= 
    (-->  
      (gen_put  $X $A $LTI0 $LTI $GTI0 $GTI) 
      (,  
        { (,  
            (ground  $X) 
            (= $X 
              (Cons  $X1 $X2))) } 
        (,  ! 
          (,  
            (gen_put_args  
              ($X1 $X2) 
              ($R1 $R2) $LTI0 $LTI $GTI0 $GTI1) 
            (,  
              { (assign_sreg  
                  (:  $X lis) $R $Seen $GTI1 $GTI2) } 
              (gen_put_list  
                ($R1 $R2) $R $Seen $A $GTI2 $GTI)))))) True)
  (= 
    (-->  
      (gen_put  $X $A $LTI0 $LTI $GTI0 $GTI) 
      (,  
        { (,  
            (ground  $X) 
            (,  
              (=..  $X 
                (Cons  $_ $Args)) 
              (functor  $X $F $N))) } 
        (,  ! 
          (,  
            { (assign_sreg  
                (:  
                  (/  $F $N) con) $R0 $Seen0 $GTI0 $GTI1) } 
            (,  
              (gen_put_con  
                (/  $F $N) $R0 $Seen0 $_ $GTI1 $GTI2) 
              (,  
                (gen_put_args  $Args $Regs $LTI0 $LTI $GTI2 $GTI3) 
                (,  
                  { (assign_sreg  
                      (:  $Args arr) $R1 $Seen1 $GTI3 $GTI4) } 
                  (,  
                    (gen_put_str_args  $Regs $R1 $Seen1 $_ $GTI4 $GTI5) 
                    (,  
                      { (assign_sreg  
                          (:  $X str) $R $Seen $GTI5 $GTI6) } 
                      (gen_put_str  
                        ($R0 $R1) $R $Seen $A $GTI6 $GTI)))))))))) True)
  (= 
    (-->  
      (gen_put  $X $A $LTI0 $LTI $GTI0 $GTI) 
      (,  
        { (= $X 
            (Cons  $X1 $X2)) } 
        (,  ! 
          (,  
            (gen_put_args  
              ($X1 $X2) 
              ($R1 $R2) $LTI0 $LTI1 $GTI0 $GTI) 
            (,  
              { (assign_reg  $_ $R $Seen $LTI1 $LTI) } 
              (,  
                { (,  
                    (= $Seen yes) 
                    (= $R $A)) } 
                ( (put_list  $R1 $R2 $R)))))))) True)
  (= 
    (-->  
      (gen_put  $X $A $LTI0 $LTI $GTI0 $GTI) 
      (,  
        { (,  
            (=..  $X 
              (Cons  $_ $Args)) 
            (functor  $X $F $N)) } 
        (,  ! 
          (,  
            { (assign_sreg  
                (:  
                  (/  $F $N) con) $R0 $Seen0 $GTI0 $GTI1) } 
            (,  
              (gen_put_con  
                (/  $F $N) $R0 $Seen0 $_ $GTI1 $GTI2) 
              (,  
                (gen_put_args  $Args $Regs $LTI0 $LTI1 $GTI2 $GTI) 
                (,  
                  { (inc_YN  $R1 $LTI1 $LTI2) } 
                  (,  
                    { (assign_reg  $_ $R $Seen $LTI2 $LTI) } 
                    (,  
                      { (,  
                          (= $Seen yes) 
                          (= $R $A)) } 
                      (,  
                        ( (put_str_args  $Regs $R1)) 
                        ( (put_str  $R0 $R1 $R)))))))))))) True)

  (= 
    (-->  
      (gen_put_var  void $_ $A) 
      (,  ! 
        { (= $A void) })) True) ;
;                                          void is a special constant.

  (= 
    (-->  
      (gen_put_var  $R $Seen $A) 
      (,  
        { (var  $Seen) } 
        (,  ! 
          (,  
            { (,  
                (= $Seen yes) 
                (= $R $A)) } 
            ( (put_var  $R)))))) True)
  (= 
    (-->  
      (gen_put_var  $R $_ $A) 
      { (= $R $A) }) True)

  (= 
    (-->  
      (gen_put_int  $X $R $Seen $A $GTI0 $GTI) 
      (,  
        { (var  $Seen) } 
        (,  ! 
          (,  
            { (,  
                (= $Seen yes) 
                (= $R $A)) } 
            { (add_instr  
                (put_int  $X $R) $GTI0 $GTI) })))) True)
  (= 
    (-->  
      (gen_put_int  $_ $R $_ $A $GTI $GTI) 
      { (= $R $A) }) True)

  (= 
    (-->  
      (gen_put_float  $X $R $Seen $A $GTI0 $GTI) 
      (,  
        { (var  $Seen) } 
        (,  ! 
          (,  
            { (,  
                (= $Seen yes) 
                (= $R $A)) } 
            { (add_instr  
                (put_float  $X $R) $GTI0 $GTI) })))) True)
  (= 
    (-->  
      (gen_put_float  $_ $R $_ $A $GTI $GTI) 
      { (= $R $A) }) True)

  (= 
    (-->  
      (gen_put_con  $X $R $Seen $A $GTI0 $GTI) 
      (,  
        { (var  $Seen) } 
        (,  ! 
          (,  
            { (,  
                (= $Seen yes) 
                (= $R $A)) } 
            { (add_instr  
                (put_con  $X $R) $GTI0 $GTI) })))) True)
  (= 
    (-->  
      (gen_put_con  $_ $R $_ $A $GTI $GTI) 
      { (= $R $A) }) True)

  (= 
    (-->  
      (gen_put_list  
        ($R1 $R2) $R $Seen $A $GTI0 $GTI) 
      (,  
        { (var  $Seen) } 
        (,  ! 
          (,  
            { (,  
                (= $Seen yes) 
                (= $R $A)) } 
            { (add_instr  
                (put_list  $R1 $R2 $R) $GTI0 $GTI) })))) True)
  (= 
    (-->  
      (gen_put_list  $_ $R $_ $A $GTI $GTI) 
      { (= $R $A) }) True)

  (= 
    (-->  
      (gen_put_str_args  $Regs $R $Seen $A $GTI0 $GTI) 
      (,  
        { (var  $Seen) } 
        (,  ! 
          (,  
            { (,  
                (= $Seen yes) 
                (= $R $A)) } 
            { (add_instr  
                (put_str_args  $Regs $R) $GTI0 $GTI) })))) True)
  (= 
    (-->  
      (gen_put_str_args  $_ $R $_ $A $GTI $GTI) 
      { (= $R $A) }) True)

  (= 
    (-->  
      (gen_put_str  
        ($R0 $R1) $R $Seen $A $GTI0 $GTI) 
      (,  
        { (var  $Seen) } 
        (,  ! 
          (,  
            { (,  
                (= $Seen yes) 
                (= $R $A)) } 
            { (add_instr  
                (put_str  $R0 $R1 $R) $GTI0 $GTI) })))) True)
  (= 
    (-->  
      (gen_put_str  $_ $R $_ $A $GTI $GTI) 
      { (= $R $A) }) True)

  (= 
    (-->  
      (gen_put_args  () () $LTI $LTI $GTI $GTI) !) True)
  (= 
    (-->  
      (gen_put_args  
        (Cons  $X $Xs) 
        (Cons  $R $Rs) $LTI0 $LTI $GTI0 $GTI) 
      (,  
        (gen_put  $X $R $LTI0 $LTI1 $GTI0 $GTI1) 
        (gen_put_args  $Xs $Rs $LTI1 $LTI $GTI1 $GTI))) True)

  (= 
    (-->  
      (gen_put_clo  
        (:  $P $X) $A $LTI0 $LTI $GTI0 $GTI) 
      (,  
        { (=..  $X 
            (Cons  $F $Args)) } 
        (,  ! 
          (,  
            (gen_put_args  $Args $Regs $LTI0 $LTI1 $GTI0 $GTI) 
            (,  
              { (assign_reg  $_ $R $Seen $LTI1 $LTI) } 
              (,  
                { (,  
                    (= $Seen yes) 
                    (= $R $A)) } 
                (,  
                  { (=..  $X1 
                      (Cons  $F $Regs)) } 
                  (,  
                    { (;  
                        (->  
                          (clause  
                            (package_name  $P) $_) 
                          (= $CLO $X1)) 
                        (:  
                          (= $CLO $P) $X1)) } 
                    ( (put_clo  $CLO $R)))))))))) True)

;
;                                         ;;;;;;;;; get instructions

  (= 
    (-->  
      (gen_get  $X $A $LTI0 $LTI $GTI0 $GTI) 
      (gen_get  
        ( (= $A $X)) $LTI0 $LTI $GTI0 $GTI)) True)

  (= 
    (-->  
      (gen_get  () $LTI $LTI $GTI $GTI) !) True)
  (= 
    (-->  
      (gen_get  
        (Cons  
          (= $A $X) $_) $LTI $LTI $GTI $GTI) 
      (,  
        { (var  $A) } 
        (,  ! 
          (,  
            { (pl2am_error  
                ($A must not be a variable in 
                  (get  $X $A))) } 
            {fail })))) True)
  (= 
    (-->  
      (gen_get  
        (Cons  
          (= $A $X) $Instrs) $LTI0 $LTI $GTI0 $GTI) 
      (,  
        { (var  $X) } 
        (,  
          { (assign_reg  $X $R $Seen $LTI0 $LTI1) } 
          (,  
            { (nonvar  $Seen) } 
            (,  ! 
              (,  
                (gen_get_var  $R $Seen $A) 
                (gen_get  $Instrs $LTI1 $LTI $GTI0 $GTI))))))) True)
  (= 
    (-->  
      (gen_get  
        (Cons  
          (= $A $X) $Instrs) $LTI0 $LTI $GTI0 $GTI) 
      (,  
        { (var  $X) } 
        (,  ! 
          (,  
            { (add_alloc  
                ($X $A yes) $LTI0 $LTI1) } 
            (gen_get  $Instrs $LTI1 $LTI $GTI0 $GTI))))) True)
  (= 
    (-->  
      (gen_get  
        (Cons  
          (= $A $X) $Instrs) $LTI0 $LTI $GTI0 $GTI) 
      (,  
        { (integer  $X) } 
        (,  ! 
          (,  
            (gen_put  $X $R $LTI0 $LTI1 $GTI0 $GTI1) 
            (,  
              ( (get_int  $X $R $A)) 
              (gen_get  $Instrs $LTI1 $LTI $GTI1 $GTI)))))) True)
  (= 
    (-->  
      (gen_get  
        (Cons  
          (= $A $X) $Instrs) $LTI0 $LTI $GTI0 $GTI) 
      (,  
        { (float  $X) } 
        (,  ! 
          (,  
            (gen_put  $X $R $LTI0 $LTI1 $GTI0 $GTI1) 
            (,  
              ( (get_float  $X $R $A)) 
              (gen_get  $Instrs $LTI1 $LTI $GTI1 $GTI)))))) True)
  (= 
    (-->  
      (gen_get  
        (Cons  
          (= $A $X) $Instrs) $LTI0 $LTI $GTI0 $GTI) 
      (,  
        { (atom  $X) } 
        (,  ! 
          (,  
            (gen_put  $X $R $LTI0 $LTI1 $GTI0 $GTI1) 
            (,  
              ( (get_con  $X $R $A)) 
              (gen_get  $Instrs $LTI1 $LTI $GTI1 $GTI)))))) True)
  (= 
    (-->  
      (gen_get  
        (Cons  
          (= $A $X) $Instrs) $LTI0 $LTI $GTI0 $GTI) 
      (,  
        { (ground  $X) } 
        (,  ! 
          (,  
            (gen_put  $X $R $LTI0 $LTI1 $GTI0 $GTI1) 
            (,  
              ( (get_ground  $X $R $A)) 
              (gen_get  $Instrs $LTI1 $LTI $GTI1 $GTI)))))) True)
  (= 
    (-->  
      (gen_get  
        (Cons  
          (= $A $X) $Instrs) $LTI0 $LTI $GTI0 $GTI) 
      (,  
        { (= $X 
            (Cons  $X1 $X2)) } 
        (,  ! 
          (,  
            ( (get_list  $A)) 
            (,  
              (gen_unify  
                ($X1 $X2) $Instrs1 $LTI0 $LTI1 $GTI0 $GTI1) 
              (,  
                (gen_get  $Instrs1 $LTI1 $LTI2 $GTI1 $GTI2) 
                (gen_get  $Instrs $LTI2 $LTI $GTI2 $GTI))))))) True)
  (= 
    (-->  
      (gen_get  
        (Cons  
          (= $A $X) $Instrs) $LTI0 $LTI $GTI0 $GTI) 
      (,  
        { (,  
            (=..  $X 
              (Cons  $F $Args)) 
            (functor  $X $F $N)) } 
        (,  
          { (assign_sreg  
              (:  
                (/  $F $N) con) $R $Seen $GTI0 $GTI1) } 
          (,  
            (gen_put_con  
              (/  $F $N) $R $Seen $_ $GTI1 $GTI2) 
            (,  
              ( (get_str  
                  (/  $F $N) $R $A)) 
              (,  
                (gen_unify  $Args $Instrs1 $LTI0 $LTI1 $GTI2 $GTI3) 
                (,  
                  (gen_get  $Instrs1 $LTI1 $LTI2 $GTI3 $GTI4) 
                  (gen_get  $Instrs $LTI2 $LTI $GTI4 $GTI)))))))) True)

  (= 
    (-->  
      (gen_get_var  void $_ $_) !) True)
  (= 
    (-->  
      (gen_get_var  $R $_ $A) 
      ( (get_val  $R $A))) True)

;
;                                         ;;;;;;;;; unify instructions

  (= 
    (-->  
      (gen_unify  () () $LTI $LTI $GTI $GTI) !) True)
  (= 
    (-->  
      (gen_unify  
        (Cons  $X $Xs) $Instrs $LTI0 $LTI $GTI0 $GTI) 
      (,  
        { (var  $X) } 
        (,  ! 
          (,  
            { (assign_reg  $X $R $Seen $LTI0 $LTI1) } 
            (,  
              (gen_unify_var  $R $Seen) 
              (gen_unify  $Xs $Instrs $LTI1 $LTI $GTI0 $GTI)))))) True)
  (= 
    (-->  
      (gen_unify  
        (Cons  $X $Xs) $Instrs $LTI0 $LTI $GTI0 $GTI) 
      (,  
        { (integer  $X) } 
        (,  ! 
          (,  
            (gen_put  $X $R $LTI0 $LTI1 $GTI0 $GTI1) 
            (,  
              ( (unify_int  $X $R)) 
              (gen_unify  $Xs $Instrs $LTI1 $LTI $GTI1 $GTI)))))) True)
  (= 
    (-->  
      (gen_unify  
        (Cons  $X $Xs) $Instrs $LTI0 $LTI $GTI0 $GTI) 
      (,  
        { (float  $X) } 
        (,  ! 
          (,  
            (gen_put  $X $R $LTI0 $LTI1 $GTI0 $GTI1) 
            (,  
              ( (unify_float  $X $R)) 
              (gen_unify  $Xs $Instrs $LTI1 $LTI $GTI1 $GTI)))))) True)
  (= 
    (-->  
      (gen_unify  
        (Cons  $X $Xs) $Instrs $LTI0 $LTI $GTI0 $GTI) 
      (,  
        { (atom  $X) } 
        (,  ! 
          (,  
            (gen_put  $X $R $LTI0 $LTI1 $GTI0 $GTI1) 
            (,  
              ( (unify_con  $X $R)) 
              (gen_unify  $Xs $Instrs $LTI1 $LTI $GTI1 $GTI)))))) True)
  (= 
    (-->  
      (gen_unify  
        (Cons  $X $Xs) $Instrs $LTI0 $LTI $GTI0 $GTI) 
      (,  
        { (ground  $X) } 
        (,  ! 
          (,  
            (gen_put  $X $R $LTI0 $LTI1 $GTI0 $GTI1) 
            (,  
              ( (unify_ground  $X $R)) 
              (gen_unify  $Xs $Instrs $LTI1 $LTI $GTI1 $GTI)))))) True)
  (= 
    (-->  
      (gen_unify  
        (Cons  $X $Xs) 
        (Cons  
          (= $R $X) $Instrs) $LTI0 $LTI $GTI0 $GTI) 
      (,  
        { (assign_reg  $_ $R $Seen $LTI0 $LTI1) } 
        (,  
          (gen_unify_var  $R $Seen) 
          (gen_unify  $Xs $Instrs $LTI1 $LTI $GTI0 $GTI)))) True)

;
;                                         ;; unify_void, unify_variable, unify_value

  (= 
    (-->  
      (gen_unify_var  void $_) 
      (,  ! 
        ( (unify_void  1)))) True)
  (= 
    (-->  
      (gen_unify_var  $R $Seen) 
      (,  
        { (var  $Seen) } 
        (,  ! 
          (,  
            { (= $Seen yes) } 
            ( (unify_var  $R)))))) True)
  (= 
    (-->  
      (gen_unify_var  $R $_) 
      ( (unify_val  $R))) True)

;
;                                         ;;;;;;;;; generate continuation goal

  (= 
    (-->  
      (gen_put_cont  $X $R $LTI0 $LTI $GTI $GTI) 
      (,  
        { (inc_PN  $R $LTI0 $LTI) } 
        ( (put_cont  $X $R)))) True)

;
;                                         ;; A register


  (= 
    (assign-reg $X $Reg $Seen $LTI0 $LTI) 
    ( (nonvar $X) 
      (set-det) 
      (pl2am-error (:: $X must be unbound variable in (assign-reg $X $Reg $Seen $LTI0 $LTI))) 
      (fail)))
  (= 
    (assign-reg $X $Reg $Seen 
      (:: $XN $YN $PN $Alloc) 
      (:: $XN $YN $PN $Alloc)) 
    ( (allocated $Alloc $X 
        (:: $Reg $Seen)) (set-det)))
  (= 
    (assign-reg $X $Reg $Seen 
      (:: $XN $YN $PN $Alloc) 
      (:: $XN1 $YN $PN $Alloc1)) 
    ( (= $Reg 
        (a $XN)) 
      (is $XN1 
        (+ $XN 1)) 
      (= $Alloc1 
        (Cons  
          (:: $X $Reg $Seen) $Alloc))))


  (= 
    (allocated 
      (Cons  
        (Cons  $V $X) $_) $V0 $X) 
    ( (== $V $V0) (set-det)))
  (= 
    (allocated 
      (Cons  $_ $Alloc) $V0 $X) 
    (allocated $Alloc $V0 $X))

;
;                                         ;; S register


  (= 
    (assign-sreg $X $Reg $Seen $GTI0 $GTI) 
    ( (not (ground $X)) 
      (set-det) 
      (pl2am-error (:: $X must be ground term in (assign-sreg $X $Reg $Seen $GTI0 $GTI))) 
      (fail)))
  (= 
    (assign-sreg $X $Reg $Seen 
      (:: $SN $SAlloc $SInstrs) 
      (:: $SN $SAlloc $SInstrs)) 
    ( (allocated $SAlloc $X 
        (:: $Reg $Seen)) (set-det)))
  (= 
    (assign-sreg 
      (with_self  $X $T) $Reg $Seen 
      (:: $SN $SAlloc $SInstrs) 
      (:: $SN1 $SAlloc1 $SInstrs)) 
    ( (assign-sreg0 $T $SN $Reg) 
      (is $SN1 
        (+ $SN 1)) 
      (= $SAlloc1 
        (Cons  
          (:: 
            (with_self  $X $T) $Reg $Seen) $SAlloc))))


  (= 
    (assign-sreg0 int $SN 
      (si $SN)) 
    (set-det))
  (= 
    (assign-sreg0 flo $SN 
      (sf $SN)) 
    (set-det))
  (= 
    (assign-sreg0 $_ $SN 
      (s $SN)) 
    (set-det))
;
;                                         assign_sreg0(con, SN, sc(SN)) :- !.

;
;                                         assign_sreg0(str, SN, ss(SN)) :- !.

;
;                                         assign_sreg0(lis, SN, sl(SN)) :- !.

;
;                                         assign_sreg0(arr, SN, sa(SN)) :- !.


;
;                                         ;; incriment YN


  (= 
    (inc-YN 
      (y $YN) 
      (Cons  $XN 
        (Cons  $YN $Zs)) 
      (Cons  $XN 
        (Cons  $YN1 $Zs))) 
    (is $YN1 
      (+ $YN 1)))

;
;                                         ;; incriment PN


  (= 
    (inc-PN 
      (p $PN) 
      (Cons  $XN 
        (Cons  $YN 
          (Cons  $PN $Zs))) 
      (Cons  $XN 
        (Cons  $YN 
          (Cons  $PN1 $Zs)))) 
    (is $PN1 
      (+ $PN 1)))

;
;                                         ;; add an instruction to GTI


  (= 
    (add_instr  $Instr 
      ($SN $SAlloc $SInstrs0) 
      ($SN $SAlloc 
        (Cons  $Instr $SInstrs0))) True)

;
;                                         ;; add an allocation to LTI


  (= 
    (add_alloc  $E 
      ($XN $YN $PN $Alloc0) 
      ($XN $YN $PN 
        (Cons  $E $Alloc0))) True)


  (= 
    (builtin_meta_predicates  ^ 2 
      (? :)) True)
;                                         /*****************************************************************
;                                           Built-in Predicates and Constants
;                                         *****************************************************************/


  (= 
    (builtin_meta_predicates  call 1 
      (:)) True)
  (= 
    (builtin_meta_predicates  once 1 
      (:)) True)
  (= 
    (builtin_meta_predicates  \+ 1 
      (:)) True)
  (= 
    (builtin_meta_predicates  findall 3 
      (? : ?)) True)
  (= 
    (builtin_meta_predicates  bagof 3 
      (? : ?)) True)
  (= 
    (builtin_meta_predicates  setof 3 
      (? : ?)) True)
  (= 
    (builtin_meta_predicates  on_exception 3 
      (? : :)) True)
  (= 
    (builtin_meta_predicates  catch 3 
      (: ? :)) True)
  (= 
    (builtin_meta_predicates  synchronized 2 
      (? :)) True)
  (= 
    (builtin_meta_predicates  freeze 2 
      (? :)) True)


  (= 
    (builtin_local_predicates  assert 1 
      (:)) True)
  (= 
    (builtin_local_predicates  asserta 1 
      (:)) True)
  (= 
    (builtin_local_predicates  assertz 1 
      (:)) True)
  (= 
    (builtin_local_predicates  retract 1 
      (:)) True)
  (= 
    (builtin_local_predicates  retractall 1 
      (:)) True)
  (= 
    (builtin_local_predicates  assert 2 
      (: ?)) True)
  (= 
    (builtin_local_predicates  asserta 2 
      (: ?)) True)
  (= 
    (builtin_local_predicates  assertz 2 
      (: ?)) True)
  (= 
    (builtin_local_predicates  retract 2 
      (: ?)) True)
  (= 
    (builtin_local_predicates  retractall 2 
      (: ?)) True)
  (= 
    (builtin_local_predicates  save 2 
      (? :)) True)
  (= 
    (builtin_local_predicates  clause 2 
      (: ?)) True)
  (= 
    (builtin_local_predicates  abolish 1 
      (:)) True)
  (= 
    (builtin_local_predicates  log_level 1 
      (:)) True)
  (= 
    (builtin_local_predicates  loggable 1 
      (:)) True)
  (= 
    (builtin_local_predicates  log_error 2 
      (: ?)) True)
  (= 
    (builtin_local_predicates  log 2 
      (: ?)) True)
  (= 
    (builtin_local_predicates  log 3 
      (: ? ?)) True)
  (= 
    (builtin_local_predicates  log 4 
      (: ? ? ?)) True)
  (= 
    (builtin_local_predicates  log 5 
      (: ? ? ? ?)) True)
  (= 
    (builtin_local_predicates  log 6 
      (: ? ? ? ? ?)) True)
  (= 
    (builtin_local_predicates  log 7 
      (: ? ? ? ? ? ?)) True)

  (= 
    (builtin_meta_predicates  with_mutex 2 
      (? :)) True)

;
;                                          Control constructs


  (= 
    (builtin_inline_predicates  fail) True)
  (= 
    (builtin_inline_predicates  
      ($get_level  $_)) True)
  (= 
    (builtin_inline_predicates  $neck_cut) True)
  (= 
    (builtin_inline_predicates  
      ($cut  $_)) True)
;
;                                          Term unification

  (= 
    (builtin_inline_predicates  
      ($unify  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($not_unifiable  $_ $_)) True)
;
;                                          Type testing

  (= 
    (builtin_inline_predicates  
      (var  $_)) True)
  (= 
    (builtin_inline_predicates  
      (atom  $_)) True)
  (= 
    (builtin_inline_predicates  
      (integer  $_)) True)
  (= 
    (builtin_inline_predicates  
      (long  $_)) True)
  (= 
    (builtin_inline_predicates  
      (float  $_)) True)
  (= 
    (builtin_inline_predicates  
      (atomic  $_)) True)
  (= 
    (builtin_inline_predicates  
      (nonvar  $_)) True)
  (= 
    (builtin_inline_predicates  
      (number  $_)) True)
  (= 
    (builtin_inline_predicates  
      (java  $_)) True)
  (= 
    (builtin_inline_predicates  
      (java  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      (closure  $_)) True)
  (= 
    (builtin_inline_predicates  
      (ground  $_)) True)
;
;                                          Term comparison

  (= 
    (builtin_inline_predicates  
      ($equality_of_term  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($inequality_of_term  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($after  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($before  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($not_after  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($not_before  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($identical_or_cannot_unify  $_ $_)) True)
;
;                                          Term creation and decomposition

  (= 
    (builtin_inline_predicates  
      (copy_term  $_ $_)) True)
;
;                                          Arithmetic evaluation

  (= 
    (builtin_inline_predicates  
      (is  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($abs  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($asin  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($acos  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($atan  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($bitwise_conj  $_ $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($bitwise_disj  $_ $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($bitwise_exclusive_or  $_ $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($bitwise_neg  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($ceil  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($cos  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($degrees  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($exp  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($float_quotient  $_ $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($floor  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($int_quotient  $_ $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($log  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($max  $_ $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($min  $_ $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($minus  $_ $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($mod  $_ $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($multi  $_ $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($plus  $_ $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($pow  $_ $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($radians  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($rint  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($round  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($shift_left  $_ $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($shift_right  $_ $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($sin  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($sqrt  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($tan  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($float  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($float_integer_part  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($float_fractional_part  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($truncate  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($sign  $_ $_)) True)
;
;                                          Arithmetic comparison

  (= 
    (builtin_inline_predicates  
      ($arith_equal  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($arith_not_equal  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($greater_or_equal  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($greater_than  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($less_or_equal  $_ $_)) True)
  (= 
    (builtin_inline_predicates  
      ($less_than  $_ $_)) True)


  (= 
    (builtin_arith_constant  random) True)
  (= 
    (builtin_arith_constant  pi) True)
  (= 
    (builtin_arith_constant  e) True)


  (= 
    (eliminate-disj $Cl $NewCl $DummyCls) 
    ( (extract-disj $Cl $NewCl $Disjs Nil) (treat-disj $Disjs $DummyCls Nil)))
;                                         /*****************************************************************
;                                           Eliminate disjunctions
;                                         *****************************************************************/

;                                         ;  The clause a :- b;c is converted into a :- b. and a :- c.
;                                         ;  In addition,
;                                         ;    (C1 -> C2)        is converted into ((C1,!,C2) ; fail).
;                                         ;    ((C1 -> C2) ; C3) is converted into ((C1,!,C2) ; C3).
;                                         ;    not(C)            is converted into ((C,!,fail) ; true).
;                                         ;    \+(C)             is converted into ((C,!,fail) ; true).
;                                         ;  And then all of disjunctions are eliminated.
;                                         ;
;                                         ;  Note: this is based on flatten.pl in holmer's benchmark.




  (= 
    (-->  
      (extract_disj  $Cl $Cl) 
      (,  
        { (var  $Cl) } !)) True)
  (= 
    (-->  
      (extract_disj  $Cl 
        (:-  $H $NewB)) 
      (,  
        { (= $Cl 
            (:-  $H $B)) } 
        (,  ! 
          (extract_disj  $B $NewB $Cl)))) True)
  (= 
    (-->  
      (extract_disj  $Cl $Cl) !) True)

  (= 
    (-->  
      (extract_disj  $G $G $_) 
      (,  
        { (var  $G) } !)) True)
  (= 
    (-->  
      (extract_disj  
        (,  $G1 $G2) 
        (,  $NewG1 $NewG2) $Cl) 
      (,  ! 
        (,  
          (extract_disj  $G1 $NewG1 $Cl) 
          (extract_disj  $G2 $NewG2 $Cl)))) True)
  (= 
    (-->  
      (extract_disj  $G $NewG $Cl) 
      (,  
        { (is_disj  $G $DisjG) } 
        (,  ! 
          (,  
            { (retract  
                (dummy_clause_counter  $N)) } 
            (,  
              ( (disj  $DisjG $N $NewG $Cl)) 
              (,  
                { (is  $N1 
                    (+  $N 1)) } 
                { (assert  
                    (dummy_clause_counter  $N1)) })))))) True)
  (= 
    (-->  
      (extract_disj  $G $G $_) !) True)


  (= 
    (is-disj 
      (det-if-then $C1 $C2) 
      (or 
        (, $C1 
          (set-det) $C2) fail)) 
    (set-det))
  (= 
    (is-disj 
      (det-if-then-else $C1 $C2 $C3) 
      (or 
        (, $C1 
          (set-det) $C2) $C3)) 
    (set-det))
  (= 
    (is-disj 
      (or $C1 $C2) 
      (or $C1 $C2)) 
    (set-det))
  (= 
    (is-disj 
      (not $C) 
      (or 
        (, $C 
          (set-det) 
          (fail)) True)) 
    (set-det))
  (= 
    (is_disj  
      (\+  $C) 
      (;  
        (,  $C 
          (,  ! fail)) true)) True)


  (= 
    (-->  
      (treat_disj  ()) !) True)
  (= 
    (-->  
      (treat_disj  
        (Cons  
          (disj  
            (;  $A $B) $N $X $C) $Disjs)) 
      (,  
        { (variables  
            (;  $A $B) $Vars) } 
        (,  
          { (variables  $C $CVars) } 
          (,  
            { (intersect_vars  $Vars $CVars $Args) } 
            (,  
              { (clause  
                  (file_name  $File) $_) } 
              (,  
                { (list_to_string  
                    ($dummy_ $N _ $File) $Name) } 
                (,  
                  { (=..  $X 
                      (Cons  $Name $Args)) } 
                  (,  
                    { (copy_term  
                        (:-  $X $A) $DummyCla) } 
                    (,  
                      { (copy_term  
                          (:-  $X $B) $DummyClb) } 
                      (,  
                        ($DummyCla) 
                        (,  
                          ($DummyClb) 
                          (treat_disj  $Disjs)))))))))))) True)


  (= 
    (intersect-vars $V1 $V2 $Out) 
    ( (sort $V1 $Sorted1) 
      (sort $V2 $Sorted2) 
      (intersect-sorted-vars $Sorted1 $Sorted2 $Out)))


  (= 
    (intersect-sorted-vars Nil $_ Nil) 
    (set-det))
  (= 
    (intersect_sorted_vars  $_ () ()) True)
  (= 
    (intersect-sorted-vars 
      (Cons  $X $Xs) 
      (Cons  $Y $Ys) 
      (Cons  $X $Rs)) 
    ( (== $X $Y) 
      (set-det) 
      (intersect-sorted-vars $Xs $Ys $Rs)))
  (= 
    (intersect-sorted-vars 
      (Cons  $X $Xs) 
      (Cons  $Y $Ys) $Rs) 
    ( (@< $X $Y) 
      (set-det) 
      (intersect-sorted-vars $Xs 
        (Cons  $Y $Ys) $Rs)))
  (= 
    (intersect-sorted-vars 
      (Cons  $X $Xs) 
      (Cons  $Y $Ys) $Rs) 
    ( (@> $X $Y) 
      (set-det) 
      (intersect-sorted-vars 
        (Cons  $X $Xs) $Ys $Rs)))


  (= 
    (pl2am-error $M) 
    ( (get-atoms  &self 
        (= 
          (file_line  $File $Line) $_)) 
      (set-det) 
      (pl2am-message user-error 
        (Cons  *** 
          (Cons  PL2ASM 
            (Cons  ERROR 
              (Cons  in 
                (Cons  $File 
                  (Cons  at 
                    (Cons  $Line 
                      (Cons  : $M)))))))))))
;                                         /*****************************************************************
;                                           Utilities
;                                         *****************************************************************/

;                                         ;;; print



  (= 
    (pl2am-error $M) 
    (pl2am-message user-error 
      (Cons  *** 
        (Cons  PL2ASM 
          (Cons  ERROR $M)))))


  (= 
    (pl2am-message $M) 
    (pl2am-message user-output $M))

  (= 
    (pl2am-message $Stream Nil) 
    ( (nl $Stream) (flush-output $Stream)))
  (= 
    (pl2am-message $Stream 
      (Cons  $M $Ms)) 
    ( (write $Stream $M) 
      (write $Stream ' ') 
      (pl2am-message $Stream $Ms)))

;
;                                         ;; format


  (= 
    (mode_expr  ()) True)
  (= 
    (mode-expr (Cons  $M $Ms)) 
    ( (nonvar $M) 
      (pl2am-member $M 
        (:: : or + - ?)) 
      (set-det) 
      (mode-expr $Ms)))


  (= 
    (predspec-expr (/ $F $A)) 
    ( (atom $F) (integer $A)))

;
;                                         ;; list


  (= 
    (pl2am_append  () $Zs $Zs) True)
  (= 
    (pl2am-append 
      (Cons  $X $Xs) $Ys 
      (Cons  $X $Zs)) 
    (pl2am-append $Xs $Ys $Zs))


  (= 
    (pl2am-rev $L $R) 
    (pl2am-rev $L Nil $R))
  (= 
    (pl2am_rev  () $R $R) True)
  (= 
    (pl2am-rev 
      (Cons  $X $L) $Y $R) 
    (pl2am-rev $L 
      (Cons  $X $Y) $R))


  (= 
    (pl2am_member  $X 
      (Cons  $X $_)) True)
  (= 
    (pl2am-member $X 
      (Cons  $_ $Ys)) 
    (pl2am-member $X $Ys))


  (= 
    (pl2am-memq $X 
      (Cons  $Y $_)) 
    ( (== $X $Y) (set-det)))
  (= 
    (pl2am-memq $X 
      (Cons  $_ $Ys)) 
    (pl2am-memq $X $Ys))


  (= 
    (-->  
      (flatten_list  ()) !) True)
  (= 
    (-->  
      (flatten_list  
        (Cons  $L1 $L2)) 
      (,  ! 
        (,  
          (flatten_list  $L1) 
          (flatten_list  $L2)))) True)
  (= 
    (-->  
      (flatten_list  $L) 
      ($L)) True)

  (= 
    (-->  
      (flatten_code  ()) !) True)
  (= 
    (-->  
      (flatten_code  
        (Cons  
          (:  $L $C) $Code)) 
      (,  ! 
        (,  
          ( (:  $L ())) 
          (flatten_code  
            (Cons  $C $Code))))) True)
  (= 
    (-->  
      (flatten_code  
        (Cons  $Code1 $Code2)) 
      (,  ! 
        (,  
          (flatten_code  $Code1) 
          (flatten_code  $Code2)))) True)
  (= 
    (-->  
      (flatten_code  $Code) 
      ($Code)) True)


  (= 
    (pl2am_maplist  $_ () ()) True)
  (= 
    (pl2am-maplist $Goal 
      (Cons  $Elem1 $Tail1) 
      (Cons  $Elem2 $Tail2)) 
    ( (=.. $Term 
        (:: $Goal $Elem1 $Elem2)) 
      (call $Term) 
      (pl2am-maplist $Goal $Tail1 $Tail2)))


  (= 
    (pl2am-resolve-file $BaseFile $File $File) 
    ( (with_self  
        (= $File $Package) $ResourceName) (set-det)))

  (= 
    (pl2am-resolve-file $BaseFile $File $IncludeFile) 
    ( (pl2am-file-directory $BaseFile $Directory) (atom-concat $Directory $File $IncludeFile)))


  (= 
    (pl2am-file-directory $BaseFile $Directory) 
    ( (atom-chars $BaseFile $BaseFileChars) 
      (pl2am-rev $BaseFileChars $BaseFileCharsRev) 
      (pl2am-file-directory- $BaseFileCharsRev $DirectoryCharsRev) 
      (pl2am-add-directory-separator $DirectoryCharsRev $DirectoryCharsRev1) 
      (pl2am-rev $DirectoryCharsRev1 $DirectoryChars) 
      (atom-chars $Directory $DirectoryChars)))


  (= 
    (pl2am-file-directory- Nil Nil) 
    (set-det))
  (= 
    (pl2am-file-directory- 
      (:: \) 
      (:: \)) 
    (set-det))
  (= 
    (pl2am-file-directory- 
      (:: /) 
      (:: /)) 
    (set-det))
  (= 
    (pl2am-file-directory- 
      (Cons  \ $BaseFileCharsRev) $BaseFileCharsRev) 
    (set-det))
  (= 
    (pl2am-file-directory- 
      (Cons  / $BaseFileCharsRev) $BaseFileCharsRev) 
    (set-det))
  (= 
    (pl2am-file-directory- 
      (Cons  $_ $BaseFileCharsRev) $DirectoryCharsRev) 
    (pl2am-file-directory- $BaseFileCharsRev $DirectoryCharsRev))


  (= 
    (pl2am-add-directory-separator $D $D) 
    ( (= $D 
        (Cons  / $_)) (set-det)))
  (= 
    (pl2am-add-directory-separator $D $D) 
    ( (= $D 
        (Cons  \ $_)) (set-det)))
  (= 
    (pl2am_add_directory_separator  $D 
      (Cons  / $D)) True)

;
;                                         ;; transform


  (= 
    (conj-to-list $X $_) 
    ( (var $X) 
      (set-det) 
      (pl2am-error (:: variable $X can not be converted to [A|B] expression)) 
      (fail)))
  (= 
    (conj-to-list 
      (, $X1 $X2 $Xs) $Y) 
    ( (set-det) (conj-to-list (, $X1 $X2 $Xs) $Y)))
  (= 
    (conj-to-list 
      (, $X $Xs) 
      (Cons  $X $Zs)) 
    ( (set-det) (conj-to-list $Xs $Zs)))
  (= 
    (conj_to_list  $X 
      ($X)) True)


  (= 
    (list-to-string $List $String) 
    ( (list-to-chars $List $Chars0) 
      (flatten-list $Chars0 $Chars Nil) 
      (atom-codes $String $Chars)))


  (= 
    (list-to-chars Nil Nil) 
    (set-det))
  (= 
    (list-to-chars 
      (Cons  $L $Ls) 
      (Cons  $C $Cs)) 
    ( (atom $L) 
      (set-det) 
      (atom-codes $L $C) 
      (list-to-chars $Ls $Cs)))
  (= 
    (list-to-chars 
      (Cons  $L $Ls) 
      (Cons  $C $Cs)) 
    ( (number $L) 
      (set-det) 
      (number-codes $L $C) 
      (list-to-chars $Ls $Cs)))


  (= 
    (list-to-conj $X $Y) 
    ( (flatten-list $X $L Nil) (list-to-conj0 $L $Y)))


  (= 
    (list-to-conj0 $X $_) 
    ( (var $X) 
      (set-det) 
      (pl2am-error (:: variable $X can not be converted to '(A,B)' expression)) 
      (fail)))
  (= 
    (list_to_conj0  
      ($X) $X) True)
  (= 
    (list-to-conj0 
      (Cons  $X $Xs) 
      (, $X $Ys)) 
    ( (set-det) (list-to-conj0 $Xs $Ys)))

;
;                                         ;; misc


  (= 
    (variables $X $Vs) 
    (variables $X Nil $Vs))

  (= 
    (variables $X $Vs $Vs) 
    ( (var $X) 
      (pl2am-memq $X $Vs) 
      (set-det)))
  (= 
    (variables $X $Vs 
      (Cons  $X $Vs)) 
    ( (var $X) (set-det)))
  (= 
    (variables $X $Vs0 $Vs0) 
    ( (atomic $X) (set-det)))
  (= 
    (variables 
      (Cons  $X $Xs) $Vs0 $Vs) 
    ( (set-det) 
      (variables $X $Vs0 $Vs1) 
      (variables $Xs $Vs1 $Vs)))
  (= 
    (variables $X $Vs0 $Vs) 
    ( (=.. $X $Xs) (variables $Xs $Vs0 $Vs)))


  (= 
    (count-variable $V $X 1) 
    ( (== $V $X) (set-det)))
  (= 
    (count-variable $_ $X 0) 
    ( (var $X) (set-det)))
  (= 
    (count-variable $_ $X 0) 
    ( (atomic $X) (set-det)))
  (= 
    (count-variable $V 
      (Cons  $X $Y) $N) 
    ( (set-det) 
      (count-variable $V $X $N1) 
      (count-variable $V $Y $N2) 
      (is $N 
        (+ $N1 $N2))))
  (= 
    (count-variable $V $X $N) 
    ( (=.. $X $Xs) (count-variable $V $Xs $N)))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
;                                          END

;
;                                          written by SICStus MeTTa 3.12.8



  !(op 1170 xfx :-)
;                                         /*****************************************************************
;                                         Time-stamp: <2008-10-29 10:42:42 banbara>
; 
;                                         NAME
;                                                am2cpp: Translating WAM-based Intermediate Code into Java
; 
;                                         USAGE
;                                                # sicstus
;                                                ?- [am2cpp].
;                                                ?- am2cpp([File]).
; 
;                                                # sicstus
;                                                ?- [am2cpp].
;                                                ?- am2cpp([File, Dir]).
; 
;                                         PARAMETERS
;                                                File is an input WAM-based Intermediate file name.
; 
;                                         DESCRIPTION
;                                                This program translates WAM-based intermediate codes into Java.
;                                                For each predicate p/n, the file named "PRED_p_n.java" is generated.
;                                                Generated files can be compiled and executed by usual
;                                                java utilities (ex. javac) with the MeTTa Cafe runtime system.
; 
;                                         COPYRIGHT
;                                                am2cpp (Translating WAM-based Intermediate Code into Java)
;                                                Copyright (C) 1997-2008 by
;                                                   Mutsunori Banbara (banbara@kobe-u.ac.jp) and
;                                                   Naoyuki Tamura (tamura@kobe-u.ac.jp)
; 
;                                         SEE ALSO
;                                                http://kaminari.istc.kobe-u.ac.jp/MeTTaCafe/
;                                         *****************************************************************/

;                                         /*****************************************************************
;                                           Declarations
;                                         *****************************************************************/


  !(op 1170 xfx -->)
  !(op 1170 fx :-)
  !(op 1170 fx ?-)
  !(op 1150 fx public)
  !(op 1150 fx package)  ;
;                                          MeTTa Cafe specific


  !(op 1170 xfx :-)
  !(op 1170 xfx -->)
  !(op 1170 fx :-)
  !(op 1170 fx ?-)
  !(op 500 yfx #)
  !(op 1150 fx dynamic)
  !(op 1150 fx meta-predicate)
  !(op 1150 fx package)
  !(op 1150 fx public)
  !(op 1150 fx import)
  !(op 1150 fx mode)
  !(op 1150 fx multifile)
  !(op 1150 fx block)

  !(dynamic (/ dest-dir 1))
  !(dynamic (/ current-arity 1))
  !(dynamic (/ current-functor 1))
  !(dynamic (/ current-package 1))
  !(dynamic (/ domain-definition 1))
  !(dynamic (/ inlined 2))

;
;                                          :- module('TauMeTTaG.compiler.am2cpp', [main/0,am2cpp/1]).


  (= 
    (package $X) 
    (nb-setval package $X))

  !(package TauPrologG.compiler.am2cpp)
  !(public (, (/ main 0) (/ am2cpp 1)))

  (= 
    (main-am2cpp) 
    ( (read $X) (am2cpp $X)))
;                                         /*****************************************************************
;                                           Main
;                                         *****************************************************************/




  (= 
    (pl2cpp (:: $File)) 
    ( (set-det) (pl2cpp (:: $File .))))
  (= 
    (pl2cpp (:: $File $Dir)) 
    (am2cpp (:: $File $Dir)))


  (= 
    (am2cpp (:: $File)) 
    ( (set-det) (am2cpp (:: $File .))))
  (= 
    (am2cpp (:: $File $Dir)) 
    ( (remove-all-atoms  &self 
        (dest_dir  $_)) 
      (add-atom  &self 
        (dest_dir  $Dir)) 
      (open $File read $In) 
      (repeat) 
      (read $In $X) 
      (write-java $X $In) 
      (== $X end-of-file) 
      (set-det) 
      (close $In) 
      (write-domains)))


  (= 
    (write-domains) 
    ( (get-atoms  &self 
        (= 
          (dest_dir  $Dir) $_)) 
      (findall $D 
        (domain-definition $D) $LD) 
      (catch 
        (with_self  
          (TauPrologG.builtin) 
          (call (with_self  (TauPrologG.builtin) (write-domain-definitions $Dir $LD)))) $_ 
        (am2cpp-message (:: domain definitions are not supported and skipped)))))
;                                         ; on some platforms (like SWI MeTTa) predicate write_domain_definitions might be not available

;                                         ; so wrap it with catch and produce warning message




  (= 
    (write-java $X $_) 
    ( (var $X) 
      (set-det) 
      (am2cpp-error (:: unbound variable is found)) 
      (fail)))
  (= 
    (write-java end-of-file $_) 
    (set-det))
  (= 
    (write-java 
      !$G $_) 
    ( (set-det) (call $G)))
  (= 
    (write-java 
      (begin-predicate $P 
        (/ $F $A)) $In) 
    ( (get-atoms  &self 
        (= 
          (dest_dir  $Dir) $_)) 
      (remove-all-atoms  &self 
        (current_package  $_)) 
      (remove-all-atoms  &self 
        (current_arity  $_)) 
      (remove-all-atoms  &self 
        (current_functor  $_)) 
      (remove-all-atoms  &self 
        (inlined  $_ $_)) 
      (add-atom  &self 
        (current_package  $P)) 
      (add-atom  &self 
        (current_arity  $A)) 
      (add-atom  &self 
        (current_functor  $F)) 
      (predicate-encoding $F $F1) 
      (package-encoding $P $PDir) 
      (list-to-string 
        (:: $Dir / $PDir) $SrcDir) 
      (list-to-string 
        (:: $SrcDir / PRED- $F1 - $A .java) $SrcFile) 
      (mkdirs $SrcDir) 
      (open $SrcFile write $Out) 
      (write $Out 'package ') 
      (write-package $P $Out) 
      (write $Out or) 
      (nl $Out) 
      (repeat) 
      (read $In $X) 
      (write-java0 $X $In $Out) 
      (== $X 
        (end-predicate $P 
          (/ $F $A))) 
      (close $Out) 
      (set-det)))
  (= 
    (write-java $X $_) 
    ( (am2cpp-error (:: $X is an invalid argument in (/ write-java 2))) (fail)))


  (= 
    (write-java0 $X $_ $_) 
    ( (var $X) 
      (set-det) 
      (am2cpp-error (:: unbound variable is found)) 
      (fail)))
;                                         /*****************************************************************
;                                           Write Java
;                                         *****************************************************************/


  (= 
    (write-java0 Nil $_ $_) 
    (set-det))
  (= 
    (write-java0 
      (Cons  $X $Xs) $In $Out) 
    ( (set-det) 
      (write-java0 $X $In $Out) 
      (write-java0 $Xs $In $Out)))
  (= 
    (write-java0 
      (end-predicate $_ $_) $_ $Out) 
    ( (set-det) 
      (tab $Out 4) 
      (write $Out }) 
      (nl $Out) 
      (write $Out }) 
      (nl $Out)))
  (= 
    (write-java0 
      (comment $Comment) $_ $Out) 
    ( (set-det) 
      (numbervars $Comment 0 $_) 
      (tab $Out 4) 
      (write $Out // ) 
      (writeq $Out $Comment) 
      (nl $Out)))
  (= 
    (write-java0 
      (debug $Comment) $_ $Out) 
    ( (set-det) 
      (numbervars $Comment 0 $_) 
      (write $Out // ) 
      (writeq $Out $Comment) 
      (nl $Out)))
  (= 
    (write-java0 
      (info (Cons  $FA (Cons  $File $_))) $_ $Out) 
    ( (set-det) 
      (write $Out /*) 
      (nl $Out) 
      (write $Out ' ') 
      (writeq $Out $FA) 
      (write $Out ' defined in ') 
      (writeq $Out $File) 
      (nl $Out) 
      (write $Out ' This file is generated by Prolog Cafe.') 
      (nl $Out) 
      (write $Out ' PLEASE DO NOT EDIT!') 
      (nl $Out) 
      (write $Out */) 
      (nl $Out)))
  (= 
    (write-java0 
      (import-package $P) $_ $Out) 
    ( (set-det) 
      (write $Out 'import ') 
      (write-package $P $Out) 
      (write $Out .*;) 
      (nl $Out)))
  (= 
    (write-java0 
      (import-package $P $FA) $_ $Out) 
    ( (set-det) 
      (write $Out 'import ') 
      (write-package $P $Out) 
      (write $Out .) 
      (det-if-then-else 
        (= $FA 
          (/ $_ $_)) 
        (write-class-name $FA $Out) 
        (write-package $FA $Out)) 
      (write $Out or) 
      (nl $Out)))
  (= 
    (write-java0 
      (with_self  $Label $Instruction) $In $Out) 
    ( (set-det) 
      (write-label $Label $Out) 
      (write-java0 $Instruction $In $Out)))
  (= 
    (write-java0 
      (label (/ fail 0)) $_ $Out) 
    ( (set-det) 
      (tab $Out 4) 
      (write $Out 'private static final Operation ') 
      (write-index 
        (/ fail 0) $Out) 
      (write $Out ' = TauPrologG.Failure.FAIL-0') 
      (write $Out or) 
      (nl $Out)))
  (= 
    (write-java0 
      (label $L) $_ $Out) 
    (set-det))
;
;                                         	tab(Out, 4),

;
;                                         	write(Out, 'static final Operation '),

;
;                                         	write_index(L, Out),

;
;                                         	write(Out, ' = new '),

;
;                                         	write_class_name(L, Out),

;
;                                         	write(Out, '();'), nl(Out).

  (= 
    (write-java0 
      (goto $L) $_ $Out) 
    ( (set-det) 
      (tab $Out 8) 
      (write $Out 'return ') 
      (write-index $L $Out) 
      (write $Out (engine);) 
      (nl $Out)))
  (= 
    (write-java0 setB0 $_ $Out) 
    ( (set-det) 
      (tab $Out 8) 
      (write $Out engine.setB0();) 
      (nl $Out)))
  (= 
    (write-java0 
      (deref $_ void) $_ $_) 
    (set-det))
  (= 
    (write-java0 
      (deref $Ri $Rj) $_ $Out) 
    ( (set-det) 
      (tab $Out 8) 
      (write-reg $Rj $Out) 
      (write $Out  = ) 
      (write-reg $Ri $Out) 
      (write $Out .DeRef();) 
      (nl $Out)))
  (= 
    (write-java0 
      (set $_ void) $_ $_) 
    (set-det))
  (= 
    (write-java0 
      (set $Ri $Rj) $_ $Out) 
    ( (set-det) 
      (tab $Out 8) 
      (write-reg $Rj $Out) 
      (write $Out  = ) 
      (write-reg $Ri $Out) 
      (write $Out or) 
      (nl $Out)))
  (= 
    (write-java0 
      (decl-term-vars Nil) $_ $_) 
    (set-det))
  (= 
    (write-java0 
      (decl-term-vars $L) $_ $Out) 
    ( (set-det) 
      (tab $Out 8) 
      (write $Out 'Term ') 
      (write-reg-args $L $Out) 
      (write $Out or) 
      (nl $Out)))
  (= 
    (write-java0 
      (decl-pred-vars Nil) $_ $_) 
    (set-det))
  (= 
    (write-java0 
      (decl-pred-vars $L) $_ $Out) 
    ( (set-det) 
      (tab $Out 8) 
      (write $Out 'Operation ') 
      (write-reg-args $L $Out) 
      (write $Out or) 
      (nl $Out)))
  (= 
    (write-java0 
      (put-cont $BinG $C) $_ $Out) 
    ( (set-det) 
      (det-if-then-else 
        (with_self  
          (= $BinG $P) $G) True 
        (= $BinG $G)) 
      (functor $G $F $A0) 
      (is $A 
        (- $A0 1)) 
      (=.. $G 
        (Cons  $F $Args)) 
      (tab $Out 8) 
      (write-reg $C $Out) 
      (write $Out ' = new ') 
      (det-if-then-else 
        (nonvar $P) 
        (, 
          (write-package $P $Out) 
          (write $Out .)) True) 
      (write-class-name 
        (/ $F $A) $Out) 
      (write $Out () 
      (write-reg-args $Args $Out) 
      (write $Out );) 
      (nl $Out)))
  (= 
    (write-java0 
      (execute cont) $_ $Out) 
    ( (set-det) 
      (tab $Out 8) 
      (write $Out 'return cont;') 
      (nl $Out)))
  (= 
    (write-java0 
      (execute $BinG) $_ $Out) 
    ( (set-det) 
      (det-if-then-else 
        (with_self  
          (= $BinG $P) $G) True 
        (= $BinG $G)) 
      (functor $G $F $A0) 
      (is $A 
        (- $A0 1)) 
      (=.. $G 
        (Cons  $F $Args)) 
      (tab $Out 8) 
      (write $Out 'return new ') 
      (det-if-then-else 
        (nonvar $P) 
        (, 
          (write-package $P $Out) 
          (write $Out .)) True) 
      (write-class-name 
        (/ $F $A) $Out) 
      (write $Out () 
      (write-reg-args $Args $Out) 
      (write $Out );) 
      (nl $Out)))
  (= 
    (write-java0 
      (inline $G) $In $Out) 
    ( (write-inline $G $In $Out) (set-det)))
  (= 
    (write-java0 
      (new-hash $Tag $I) $_ $Out) 
    ( (set-det) 
      (tab $Out 4) 
      (write $Out 'private static final java.util.HashMap<Term, Operation> ') 
      (det-if-then-else 
        (== $Tag int) 
        (write $Out Int) 
        (write $Out $Tag)) 
      (write $Out ' = new java.util.HashMap<Term, Operation>(') 
      (write $Out $I) 
      (write $Out );) 
      (nl $Out)))
  (= 
    (write-java0 
      (put-hash $X $L $Tag) $_ $Out) 
    ( (set-det) 
      (tab $Out 8) 
      (det-if-then-else 
        (== $Tag int) 
        (write $Out Int) 
        (write $Out $Tag)) 
      (write $Out .put() 
      (det-if-then-else 
        (get-atoms  &self 
          (= 
            (inlined  $X 
              (/  $F $A)) $_)) 
        (, 
          (write $Out 'Const.intern("') 
          (write-constant $F $Out) 
          (write $Out ",) 
          (write $Out $A) 
          (write $Out ))) 
        (write-reg $X $Out)) 
      (write $Out , ) 
      (write-method-ref $L $Out) 
      (write $Out );) 
      (nl $Out)))
  (= 
    (write-java0 
      (static $Instrs) $In $Out) 
    ( (set-det) 
      (tab $Out 4) 
      (write $Out 'static {') 
      (nl $Out) 
      (write-java0 $Instrs $In $Out) 
      (tab $Out 4) 
      (write $Out }) 
      (nl $Out)))
;
;                                         ;; Put Instructions

  (= 
    (write-java0 
      (put-var $X) $_ $Out) 
    ( (set-det) 
      (tab $Out 8) 
      (write-reg $X $Out) 
      (write $Out ' = new Var(engine);') 
      (nl $Out)))
  (= 
    (write-java0 
      (put-int $I $X) $_ $Out) 
    ( (long $I) 
      (set-det) 
      (tab $Out 4) 
      (write $Out 'private static final LongTerm ') 
      (write-reg $X $Out) 
      (write $Out ' = new LongTerm(') 
      (write $Out $I) 
      (write $Out L);) 
      (nl $Out)))
  (= 
    (write-java0 
      (put-int $I $X) $_ $Out) 
    ( (set-det) 
      (tab $Out 4) 
      (write $Out 'private static final IntegerTerm ') 
      (write-reg $X $Out) 
      (write $Out ' = new IntegerTerm(') 
      (det-if-then-else 
        (java-integer $I) True 
        (write $Out 'new java.math.BigInteger("')) 
      (write $Out $I) 
      (det-if-then-else 
        (java-integer $I) True 
        (write $Out "))) 
      (write $Out );) 
      (nl $Out)))
  (= 
    (write-java0 
      (put-float $F $X) $_ $Out) 
    ( (set-det) 
      (tab $Out 4) 
      (write $Out 'private static final Float ') 
      (write-reg $X $Out) 
      (write $Out ' = new Float(') 
      (write $Out $F) 
      (write $Out );) 
      (nl $Out)))
  (= 
    (write-java0 
      (put-con 
        (/ $F $A) $X) $_ $Out) 
    ( (set-det) (add-atom  &self (inlined  $X (/  $F $A)))))
  (= 
    (write-java0 
      (put-con $C $X) $_ $Out) 
    ( (set-det) 
      (tab $Out 4) 
      (write $Out 'private static final Const ') 
      (write-reg $X $Out) 
      (write $Out ' = Const.intern("') 
      (det-if-then-else 
        (= $C 
          (/ $F $A)) 
        (, 
          (write-constant $F $Out) 
          (write $Out ", ) 
          (write $Out $A) 
          (write $Out );)) 
        (, 
          (write-constant $C $Out) 
          (write $Out ");))) 
      (nl $Out)))
  (= 
    (write-java0 
      (put-list $Xi $Xj $Xk) $_ $Out) 
    ( (set-det) 
      (det-if-then-else 
        (= $Xk 
          (s $_)) 
        (, 
          (tab $Out 4) 
          (write $Out 'private static final ListTerm ')) 
        (tab $Out 8)) 
      (write-reg $Xk $Out) 
      (write $Out ' = LIST(') 
      (write-reg $Xi $Out) 
      (write $Out , ) 
      (write-reg $Xj $Out) 
      (write $Out );) 
      (nl $Out)))
  (= 
    (write-java0 
      (put-str $Xi $Y $Xj) $_ $Out) 
    ( (set-det) 
      (det-if-then-else 
        (= $Xj 
          (s $_)) 
        (, 
          (tab $Out 4) 
          (write $Out 'private static final .Fun ')) 
        (tab $Out 8)) 
      (write-reg $Xj $Out) 
      (write $Out ' = F(') 
      (det-if-then-else 
        (get-atoms  &self 
          (= 
            (inlined  $Xi 
              (/  $F $A)) $_)) 
        (, 
          (write $Out ") 
          (write-constant $F $Out) 
          (write $Out ")) 
        (write-reg $Xi $Out)) 
      (write $Out , ) 
      (write-reg $Y $Out) 
      (write $Out );) 
      (nl $Out)))
  (= 
    (write-java0 
      (put-str-args $Xs 
        (s $Y)) $_ $Out) 
    ( (set-det) (add-atom  &self (inlined  (s  $Y) (str_args  $Xs)))))
  (= 
    (write-java0 
      (put-str-args $Xs $Y) $_ $Out) 
    ( (set-det) 
      (det-if-then-else 
        (= $Y 
          (s $_)) 
        (, 
          (tab $Out 4) 
          (write $Out 'private static final ')) 
        (tab $Out 8)) 
      (write $Out 'Term[] ') 
      (write-reg $Y $Out) 
      (write $Out  = {) 
      (write-reg-args $Xs $Out) 
      (write $Out };) 
      (nl $Out)))
  (= 
    (write-java0 
      (put-clo $G0 $X) $_ $Out) 
    ( (set-det) 
      (det-if-then-else 
        (with_self  
          (= $G0 $P) $G) True 
        (= $G0 $G)) 
      (functor $G $F $A) 
      (=.. $G 
        (Cons  $F $Args0)) 
      (am2cpp-append $Args0 
        (:: null) $Args) 
      (tab $Out 8) 
      (write-reg $X $Out) 
      (write $Out ' = new ClosureTerm(new ') 
      (det-if-then-else 
        (nonvar $P) 
        (, 
          (write-package $P $Out) 
          (write $Out .)) True) 
      (write-class-name 
        (/ $F $A) $Out) 
      (write $Out () 
      (write-reg-args $Args $Out) 
      (write $Out ));) 
      (nl $Out)))
;
;                                         ;; Get Instructions

  (= 
    (write-java0 
      (get-val $Xi $Xj) $_ $Out) 
    ( (set-det) 
      (tab $Out 8) 
      (write $Out 'if (! ') 
      (write-reg $Xi $Out) 
      (write $Out .unify() 
      (write-reg $Xj $Out) 
      (write $Out ', engine.trail))') 
      (nl $Out) 
      (tab $Out 12) 
      (write $Out 'return engine.fail();') 
      (nl $Out)))
  (= 
    (write-java0 
      (get-int $_ $Xi $Xj) $In $Out) 
    ( (set-det) (write-java0 (get-val $Xi $Xj) $In $Out)))
  (= 
    (write-java0 
      (get-float $_ $Xi $Xj) $In $Out) 
    ( (set-det) (write-java0 (get-val $Xi $Xj) $In $Out)))
;                                         /*
;                                         write_java0(get_int(N,Xi,Xj), In, Out) :- !,
;                                         	write_java0(deref(Xj,Xj), In, Out),
;                                         	; read mode
;                                         	tab(Out, 8),
;                                         	write(Out, 'if ('), write_reg(Xj, Out), write(Out, ' .IsInt() || '), write_reg(Xj, Out), write(Out, ' instanceof LongTerm){'), nl(Out),
;                                         	tab(Out, 12),
;                                         	write(Out, 'if (((NumberTerm) '), write_reg(Xj, Out), write(Out, ').intValue() != '),
;                                         	write(Out, N), write(Out, ')'), nl(Out),
;                                         	tab(Out, 16),
;                                          	write(Out, 'return engine.fail();'), nl(Out),
;                                         	; write mode
;                                         	tab(Out, 8),
;                                         	write(Out, '} else if ('), write_reg(Xj, Out), write(Out, ' instanceof Var){'), nl(Out),
;                                         	tab(Out, 12),
;                                         	write(Out, '((Var) '), write_reg(Xj, Out), write(Out, ').bind('),
;                                         	write_reg(Xi, Out), write(Out, ', engine.trail);'), nl(Out),
;                                         	tab(Out, 8),
;                                         	; otherwise fail
;                                          	write(Out, '} else {'), nl(Out),
;                                         	tab(Out, 12),
;                                          	write(Out, 'return engine.fail();'), nl(Out),
;                                         	tab(Out, 8),
;                                          	write(Out, '}'), nl(Out).
;                                         */


  (= 
    (write-java0 
      (get-con $_ $Xi $Xj) $In $Out) 
    ( (set-det) (write-java0 (get-val $Xi $Xj) $In $Out)))
;                                         /*
;                                         write_java0(get_float(N,Xi,Xj), In, Out) :- !,
;                                         	write_java0(deref(Xj,Xj), In, Out),
;                                         	; read mode
;                                         	tab(Out, 8),
;                                         	write(Out, 'if ('), write_reg(Xj, Out), write(Out, ' instanceof Float){'), nl(Out),
;                                         	tab(Out, 12),
;                                         	write(Out, 'if (((Float) '), write_reg(Xj, Out), write(Out, ').doubleValue() != '),
;                                         	write(Out, N), write(Out, ')'), nl(Out),
;                                         	tab(Out, 16),
;                                          	write(Out, 'return engine.fail();'), nl(Out),
;                                         	; write mode
;                                         	tab(Out, 8),
;                                         	write(Out, '} else if ('), write_reg(Xj, Out), write(Out, ' instanceof Var){'), nl(Out),
;                                         	tab(Out, 12),
;                                         	write(Out, '((Var) '), write_reg(Xj, Out), write(Out, ').bind('),
;                                         	write_reg(Xi, Out), write(Out, ', engine.trail);'), nl(Out),
;                                         	tab(Out, 8),
;                                         	; otherwise fail
;                                          	write(Out, '} else {'), nl(Out),
;                                         	tab(Out, 12),
;                                          	write(Out, 'return engine.fail();'), nl(Out),
;                                         	tab(Out, 8),
;                                          	write(Out, '}'), nl(Out).
;                                         */



  (= 
    (write-java0 
      (get-ground $_ $Xi $Xj) $In $Out) 
    ( (set-det) (write-java0 (get-val $Xi $Xj) $In $Out)))
;                                         /*
;                                         write_java0(get_con(_,Xi,Xj), In, Out) :- !,
;                                         	write_java0(deref(Xj,Xj), In, Out),
;                                         	; read mode
;                                         	tab(Out, 8),
;                                         	write(Out, 'if ('), write_reg(Xj, Out), write(Out, ' .IsConst()){'), nl(Out),
;                                         	tab(Out, 12),
;                                         	write(Out, 'if (! '),
;                                         	write_reg(Xj, Out), write(Out, '.equals('), write_reg(Xi, Out),
;                                         	write(Out, '))'), nl(Out),
;                                         	tab(Out, 16),
;                                          	write(Out, 'return engine.fail();'), nl(Out),
;                                         	; write mode
;                                         	tab(Out, 8),
;                                         	write(Out, '} else if ('), write_reg(Xj, Out), write(Out, ' instanceof Var){'), nl(Out),
;                                         	tab(Out, 12),
;                                         	write(Out, '((Var) '), write_reg(Xj, Out), write(Out, ').bind('),
;                                         	write_reg(Xi, Out), write(Out, ', engine.trail);'), nl(Out),
;                                         	tab(Out, 8),
;                                         	; otherwise fail
;                                          	write(Out, '} else {'), nl(Out),
;                                         	tab(Out, 12),
;                                          	write(Out, 'return engine.fail();'), nl(Out),
;                                         	tab(Out, 8),
;                                          	write(Out, '}'), nl(Out).
;                                          */


  (= 
    (write-java0 
      (get-list $X) $In $Out) 
    ( (set-det) 
      (write-java0 
        (deref $X $X) $In $Out) 
      (read-instructions 2 $In $Us) 
      (tab $Out 8) 
      (write $Out 'if (') 
      (write-reg $X $Out) 
      (write $Out ' IsList()){') 
      (nl $Out) 
      (tab $Out 12) 
      (write $Out 'Term[] args = {(') 
      (write-reg $X $Out) 
      (write $Out ').Arg(1),(') 
      (write-reg $X $Out) 
      (write $Out ).Arg(2)};) 
      (nl $Out) 
      (write-unify-read $Us 0 $Out) 
      (tab $Out 8) 
      (write $Out '} else if (') 
      (write-reg $X $Out) 
      (write $Out ' instanceof Var){') 
      (nl $Out) 
      (write-unify-write $Us $Rs $Out) 
      (tab $Out 12) 
      (write $Out '((Var) ') 
      (write-reg $X $Out) 
      (write $Out ).bind(LIST() 
      (write-reg-args $Rs $Out) 
      (write $Out '), engine.trail);') 
      (nl $Out) 
      (tab $Out 8) 
      (write $Out '} else {') 
      (nl $Out) 
      (tab $Out 12) 
      (write $Out 'return engine.fail();') 
      (nl $Out) 
      (tab $Out 8) 
      (write $Out }) 
      (nl $Out)))
;                                         ; read mode

;                                         ; write mode

;                                         ; otherwise fail


  (= 
    (write-java0 
      (get-str 
        (/ $F $A) $Xi $Xj) $In $Out) 
    ( (set-det) 
      (write-java0 
        (deref $Xj $Xj) $In $Out) 
      (read-instructions $A $In $Us) 
      (write-unify-write $Us $Rs $Out) 
      (tab $Out 12) 
      (write $Out 'if (!') 
      (write-reg $Xj $Out) 
      (write $Out .unify(F() 
      (write-reg $Xi $Out) 
      (write $Out , ) 
      (write-reg-args $Rs $Out) 
      (write $Out '), engine.trail)){') 
      (nl $Out) 
      (tab $Out 16) 
      (write $Out 'return engine.fail();') 
      (nl $Out) 
      (tab $Out 12) 
      (write $Out }) 
      (nl $Out)))
;                                         ; simple unify




  (= 
    (write-java0 
      (try $Li $Lj) $_ $Out) 
    ( (set-det) 
      (get-atoms  &self 
        (= 
          (current_arity  $A) $_)) 
      (tab $Out 8) 
      (write $Out engine.jtry) 
      (det-if-then-else 
        (=< $A 8) 
        (, 
          (write $Out $A) 
          (write $Out ()) 
        (, 
          (write $Out () 
          (write $Out $A) 
          (write $Out , ))) 
      (write $Out 'null, ') 
      (write-method-ref $Lj $Out) 
      (write $Out );) 
      (nl $Out) 
      (tab $Out 8) 
      (write $Out 'return ') 
      (write-index $Li $Out) 
      (write $Out (engine);) 
      (nl $Out)))
;                                         /*	; read mode
;                                         	tab(Out, 8),
;                                         	write(Out, 'if ('), write_reg(Xj, Out), write(Out, ' .IsStruct()){'), nl(Out), ;??? == F
;                                         	tab(Out, 12),
;                                         	write(Out, 'if (! '), write_reg(Xi, Out),
;                                         	write(Out, '.equals(((.Fun)'), write_reg(Xj, Out),
;                                         	write(Out, ').functor()))'), nl(Out),
;                                         	tab(Out, 16),
;                                         	write(Out, 'return engine.fail();'), nl(Out),
;                                         	tab(Out, 12),
;                                         	write(Out, 'Term[] args = ((.Fun)'),
;                                         	write_reg(Xj, Out), write(Out, ').args();'), nl(Out),
;                                         	write_unify_read(Us, 0, Out),
;                                         	; write mode
;                                         	tab(Out, 8),
;                                         	write(Out, '} else if ('), write_reg(Xj, Out), write(Out, ' instanceof Var){'), nl(Out),
;                                         	write_unify_write(Us, Rs, Out),
;                                         	tab(Out, 12),
;                                         	write(Out, 'Term[] args = {'), write_reg_args(Rs, Out), write(Out, '};'), nl(Out),
;                                         	tab(Out, 12),
;                                         	write(Out, '((Var) '), write_reg(Xj, Out), write(Out, ').bind(F('),
;                                         	write_reg(Xi, Out), write(Out, ', args), engine.trail);'), nl(Out),
;                                         	; otherwise fail
;                                         	tab(Out, 8),
;                                          	write(Out, '} else {'), nl(Out),
;                                         	tab(Out, 12),
;                                          	write(Out, 'return engine.fail();'), nl(Out),
;                                         	tab(Out, 8),
;                                          	write(Out, '}'), nl(Out).
;                                          */

;                                         ;;; Choice Instructions


  (= 
    (write-java0 
      (retry $Li $Lj) $_ $Out) 
    ( (set-det) 
      (tab $Out 8) 
      (write $Out 'engine.retry(null, ') 
      (write-method-ref $Lj $Out) 
      (write $Out );) 
      (nl $Out) 
      (tab $Out 8) 
      (write $Out 'return ') 
      (write-index $Li $Out) 
      (write $Out (engine);) 
      (nl $Out)))
  (= 
    (write-java0 
      (trust $L) $_ $Out) 
    ( (set-det) 
      (tab $Out 8) 
      (write $Out engine.trust(null);) 
      (nl $Out) 
      (tab $Out 8) 
      (write $Out 'return ') 
      (write-index $L $Out) 
      (write $Out (engine);) 
      (nl $Out)))
;
;                                         ;; Indexing Instructions

  (= 
    (write-java0 
      (switch-on-term $Lv $Li $Lf $Lc $Ls $Ll) $_ $Out) 
    ( (set-det) 
      (tab $Out 8) 
      (write $Out {) 
      (write-inline-start switch-on-term $Out) 
      (nl $Out) 
      (tab $Out 12) 
      (write $Out 'Term x = engine.Areg[0].DeRef();') 
      (nl $Out) 
      (write-if-method-call 'x IsVar() ' $Lv $Out) 
      (write-if-method-call 'x .IsList() ' $Ll $Out) 
      (write-if-method-call 'x .IsStruct() ' $Ls $Out) 
      (write-if-method-call 'x .IsConst() ' $Lc $Out) 
      (write-if-method-call 'x .IsInt() ' $Li $Out) 
      (write-if-method-call 'x IsFloat() ' $Lf $Out) 
      (tab $Out 12) 
      (write $Out 'return ') 
      (write-index $Lv $Out) 
      (write $Out (engine);) 
      (nl $Out) 
      (tab $Out 8) 
      (write $Out }) 
      (write-inline-end $Out) 
      (nl $Out)))
  (= 
    (write-java0 
      (switch-on-hash $Tag $_ $L $_) $_ $Out) 
    ( (set-det) 
      (tab $Out 8) 
      (write $Out 'return engine.switch-on-hash(') 
      (det-if-then-else 
        (== $Tag int) 
        (write $Out Int) 
        (write $Out $Tag)) 
      (write $Out , ) 
      (write-method-ref $L $Out) 
      (write $Out ).exec(engine);) 
      (nl $Out)))
  (= 
    (write-java0 $Instruction $_ $_) 
    ( (am2cpp-error (:: $Instruction is an invalid instruction)) (fail)))


  (= 
    (write-label 
      (main 
        (/ $F $A) $Modifier) $Out) 
    ( (set-det) 
      (get-atoms  &self 
        (= 
          (current_package  $P) $_)) 
      (nl $Out) 
      (nl $Out) 
      (det-if-then-else 
        (== $Modifier public) 
        (write $Out 'public ') True) 
      (write $Out 'final class ') 
      (write-class-name 
        (/ $F $A) $Out) 
      (write $Out ' extends ') 
      (write-predicate-base-class $A $Out) 
      (write $Out  {) 
      (nl $Out)))
;                                         /*****************************************************************
;                                           Write Label
;                                         *****************************************************************/

;                                         ; Import class constants within translation unit

;                                         ;	write(Out, 'import static '), write_package(P, Out), write(Out, '.'),
;                                         ;	write_class_name(F/A, Out), write(Out, '.*;'),
;                                         ;	nl(Out),

;                                         ; Class definition


  (= 
    (write-label 
      (/ $F $A) $Out) 
    ( (set-det) 
      (det-if-then-else 
        (> $A 4) 
        (, 
          (nl $Out) 
          (write-enum 'private final Term ' arg 5 $A ,  or 4 $Out) 
          (nl $Out)) True) 
      (nl $Out) 
      (write-constructor 
        (/ $F $A) $Out) 
      (nl $Out) 
      (nl $Out) 
      (write-to-string 
        (/ $F $A) $Out) 
      (nl $Out) 
      (nl $Out) 
      (tab $Out 4) 
      (write $Out @Override) 
      (nl $Out) 
      (tab $Out 4) 
      (write $Out 'public Operation exec(Prolog engine) {') 
      (nl $Out)))
;                                         ; instance variable declaration

;                                         ; constructor

;                                         ; toString method

;                                         ; exec method


  (= 
    (write-label $L $Out) 
    ( (tab $Out 4) 
      (write $Out }) 
      (nl $Out) 
      (nl $Out) 
      (tab $Out 4) 
      (write $Out 'private final static Operation ') 
      (write-index $L $Out) 
      (write $Out '(Prolog engine) {') 
      (nl $Out) 
      (set-det)))
;                                         ; method for control instructions and clauses


  (= 
    (write-label $Instruction $_ $_) 
    ( (am2cpp-error (:: $Instruction is an invalid instruction)) (fail)))


  (= 
    (write-constructor 
      (/ $F $A) $Out) 
    ( (tab $Out 4) 
      (write $Out 'public ') 
      (write-class-name 
        (/ $F $A) $Out) 
      (write $Out () 
      (det-if-then-else 
        (> $A 0) 
        (write-enum '' 'Term a' 1 $A ,  ,  0 $Out) True) 
      (write $Out 'Operation cont) {') 
      (nl $Out) 
      (> $A 0) 
      (for $I 1 $A) 
      (tab $Out 8) 
      (write $Out this.) 
      (write $Out arg) 
      (write $Out $I) 
      (write $Out  = ) 
      (write $Out a) 
      (write $Out $I) 
      (write $Out or) 
      (nl $Out) 
      (fail)))
;                                         /*****************************************************************
;                                           Write Constructor
;                                         *****************************************************************/


  (= 
    (write-constructor $_ $Out) 
    ( (tab $Out 8) 
      (write $Out 'this.cont = cont;') 
      (nl $Out) 
      (tab $Out 4) 
      (write $Out })))


  (= 
    (write-enum $Head $Sym $SN $EN $Delim $_ $Tab $Out) 
    ( (=< $SN $EN) 
      (tab $Out $Tab) 
      (write $Out $Head) 
      (for $I $SN $EN) 
      (write $Out $Sym) 
      (write $Out $I) 
      (det-if-then-else 
        (< $I $EN) 
        (write $Out $Delim) True) 
      (fail)))
  (= 
    (write-enum $_ $_ $SN $EN $_ $Tail $_ $Out) 
    ( (=< $SN $EN) (write $Out $Tail)))


  (= 
    (write-unify-read Nil $_ $_) 
    (set-det))
;                                         /*****************************************************************
;                                           Write Unify Instructions
;                                         *****************************************************************/

;                                         ;;; Read Mode


  (= 
    (write-unify-read 
      (Cons  
        (unify-void $I) $Xs) $N $Out) 
    ( (set-det) 
      (is $N1 
        (+ $N $I)) 
      (write-unify-read $Xs $N1 $Out)))
  (= 
    (write-unify-read 
      (Cons  $X $Xs) $N $Out) 
    ( (write-unify-r $X $N $Out) 
      (is $N1 
        (+ $N 1)) 
      (write-unify-read $Xs $N1 $Out)))


  (= 
    (write-unify-r $X $_ $_) 
    ( (var $X) 
      (set-det) 
      (am2cpp-error (:: unbound variable is found)) 
      (fail)))
  (= 
    (write-unify-r 
      (unify-var $X) $N $Out) 
    ( (set-det) 
      (tab $Out 12) 
      (write-reg $X $Out) 
      (write $Out  = ) 
      (write-reg 
        (args $N) $Out) 
      (write $Out or) 
      (nl $Out)))
  (= 
    (write-unify-r 
      (unify-val $X) $N $Out) 
    ( (set-det) 
      (tab $Out 12) 
      (write $Out 'if (! ') 
      (write-reg $X $Out) 
      (write $Out .unify() 
      (write-reg 
        (args $N) $Out) 
      (write $Out ', engine.trail))') 
      (nl $Out) 
      (tab $Out 16) 
      (write $Out 'return engine.fail();') 
      (nl $Out)))
  (= 
    (write-unify-r 
      (unify-int $_ $X) $N $Out) 
    ( (set-det) (write-unify-r (unify-val $X) $N $Out)))
;                                         ;???


  (= 
    (write-unify-r 
      (unify-float $_ $X) $N $Out) 
    ( (set-det) (write-unify-r (unify-val $X) $N $Out)))
;                                         ;???


  (= 
    (write-unify-r 
      (unify-con $_ $X) $N $Out) 
    ( (set-det) (write-unify-r (unify-val $X) $N $Out)))
;                                         ;???


  (= 
    (write-unify-r 
      (unify-ground $_ $X) $N $Out) 
    ( (set-det) (write-unify-r (unify-val $X) $N $Out)))
  (= 
    (write-unify-r $X $_ $_) 
    ( (am2cpp-error (:: $X is an invalid instruction)) (fail)))

;
;                                         ;; Write Mode


  (= 
    (write-unify-write Nil Nil $_) 
    (set-det))
  (= 
    (write-unify-write 
      (Cons  
        (unify-void 0) $Xs) $Rs $Out) 
    ( (set-det) (write-unify-write $Xs $Rs $Out)))
  (= 
    (write-unify-write 
      (Cons  
        (unify-void $I) $Xs) 
      (Cons  void $Rs) $Out) 
    ( (> $I 0) 
      (set-det) 
      (is $I1 
        (- $I 1)) 
      (write-unify-write 
        (Cons  
          (unify-void $I1) $Xs) $Rs $Out)))
  (= 
    (write-unify-write 
      (Cons  $X $Xs) 
      (Cons  $R $Rs) $Out) 
    ( (write-unify-w $X $R $Out) (write-unify-write $Xs $Rs $Out)))


  (= 
    (write-unify-w $X $_ $_) 
    ( (var $X) 
      (set-det) 
      (am2cpp-error (:: unbound variable is found)) 
      (fail)))
  (= 
    (write-unify-w 
      (unify-var $X) $X $Out) 
    ( (set-det) 
      (tab $Out 12) 
      (write-reg $X $Out) 
      (write $Out ' = new Var(engine);') 
      (nl $Out)))
  (= 
    (write-unify-w 
      (unify-val $X) $X $_) 
    (set-det))
  (= 
    (write-unify-w 
      (unify-int $_ $X) $X $_) 
    (set-det))
  (= 
    (write-unify-w 
      (unify-float $_ $X) $X $_) 
    (set-det))
  (= 
    (write-unify-w 
      (unify-con $_ $X) $X $_) 
    (set-det))
  (= 
    (write-unify-w 
      (unify-ground $_ $X) $X $_) 
    (set-det))
  (= 
    (write-unify-w $X $_ $_) 
    ( (am2cpp-error (:: $X is an invalid instruction)) (fail)))


  (= 
    (write-inline $X $In $Out) 
    ( (write-inline-start $X $Out) 
      (write-inline0 $X $In $Out) 
      (write-inline-end $Out)))
;                                         /*****************************************************************
;                                           Write Inline
;                                         *****************************************************************/




  (= 
    (write-inline-start $Goal $Out) 
    ( (tab $Out 8) 
      (write $Out '//START inline expansion of ') 
      (write $Out $Goal) 
      (nl $Out)))

  (= 
    (write-inline-end $Out) 
    ( (tab $Out 8) 
      (write $Out '//END inline expansion') 
      (nl $Out)))

;
;                                          Control constructs


  (= 
    (write-inline0 fail $_ $Out) 
    ( (set-det) 
      (tab $Out 8) 
      (write $Out 'return engine.fail();') 
      (nl $Out)))
  (= 
    (write-inline0 
      ($get-level $X) $_ $Out) 
    ( (set-det) (write-if-fail (op (set-det) (unify $X (# ('new IntegerTerm' engine.B0)))) Nil 8 $Out)))
  (= 
    (write-inline0 %neck-cut $_ $Out) 
    ( (set-det) 
      (tab $Out 8) 
      (write $Out engine.neckCut();) 
      (nl $Out)))
  (= 
    (write-inline0 
      ($cut $X) $_ $Out) 
    ( (set-det) 
      (write-deref-args 
        (:: $X) $Out) 
      (tab $Out 8) 
      (write $Out 'if (') 
      (write-reg $X $Out) 
      (write $Out ' .IsInt()) {') 
      (nl $Out) 
      (tab $Out 12) 
      (write $Out 'engine.cut(((IntegerTerm) ') 
      (write-reg $X $Out) 
      (write $Out ).intValue());) 
      (nl $Out) 
      (tab $Out 8) 
      (write $Out '} else {') 
      (nl $Out) 
      (tab $Out 12) 
      (write $Out 'throw new IllegalTypeException("integer", ') 
      (write-reg $X $Out) 
      (write $Out );) 
      (nl $Out) 
      (tab $Out 8) 
      (write $Out }) 
      (nl $Out)))
;
;                                          Term unification

  (= 
    (write-inline0 
      ($unify $X $Y) $_ $Out) 
    ( (set-det) (write-if-fail (op (set-det) (unify $X $Y)) Nil 8 $Out)))
  (= 
    (write-inline0 
      ($not-unifiable $X $Y) $_ $Out) 
    ( (set-det) (write-if-fail (unify $X $Y) Nil 8 $Out)))
;
;                                          Type testing

  (= 
    (write-inline0 
      (var $X) $_ $Out) 
    ( (set-det) (write-if-fail (op (set-det) (instanceof $X Var)) (:: $X) 8 $Out)))
  (= 
    (write-inline0 
      (atom $X) $_ $Out) 
    ( (set-det) (write-if-fail (op (set-det) (instanceof $X Const)) (:: $X) 8 $Out)))
  (= 
    (write-inline0 
      (integer $X) $_ $Out) 
    ( (set-det) (write-if-fail (op (set-det) (instanceof $X IntegerTerm)) (:: $X) 8 $Out)))
  (= 
    (write-inline0 
      (long $X) $_ $Out) 
    ( (set-det) (write-if-fail (op (set-det) (instanceof $X LongTerm)) (:: $X) 8 $Out)))
  (= 
    (write-inline0 
      (float $X) $_ $Out) 
    ( (set-det) (write-if-fail (op (set-det) (instanceof $X Float)) (:: $X) 8 $Out)))
  (= 
    (write-inline0 
      (nonvar $X) $_ $Out) 
    ( (set-det) (write-if-fail (instanceof $X Var) (:: $X) 8 $Out)))
  (= 
    (write-inline0 
      (number $X) $_ $Out) 
    ( (set-det) 
      (= $NI 
        (op 
          (set-det) 
          (instanceof $X IntegerTerm))) 
      (= $NL 
        (op 
          (set-det) 
          (instanceof $X LongTerm))) 
      (= $ND 
        (op 
          (set-det) 
          (instanceof $X Float))) 
      (write-if-fail 
        (op && 
          (op && $NI $ND) $NL) 
        (:: $X) 8 $Out)))
  (= 
    (write-inline0 
      (java $X) $_ $Out) 
    ( (set-det) (write-if-fail (op (set-det) (instanceof $X JavaObjectTerm)) (:: $X) 8 $Out)))
  (= 
    (write-inline0 
      (closure $X) $_ $Out) 
    ( (set-det) (write-if-fail (op (set-det) (instanceof $X ClosureTerm)) (:: $X) 8 $Out)))
  (= 
    (write-inline0 
      (atomic $X) $_ $Out) 
    ( (set-det) 
      (= $NS 
        (op 
          (set-det) 
          (instanceof $X Const))) 
      (= $NI 
        (op 
          (set-det) 
          (instanceof $X IntegerTerm))) 
      (= $NL 
        (op 
          (set-det) 
          (instanceof $X LongTerm))) 
      (= $ND 
        (op 
          (set-det) 
          (instanceof $X Float))) 
      (write-if-fail 
        (op && $NL 
          (op && $NS 
            (op && $NI $ND))) 
        (:: $X) 8 $Out)))
  (= 
    (write-inline0 
      (java $X $Y) $_ $Out) 
    ( (set-det) 
      (write-if-fail 
        (op 
          (set-det) 
          (instanceof $X JavaObjectTerm)) 
        (:: $X) 8 $Out) 
      (= $EXP 
        (# (Const.create (@ (getName (@ (getClass (@ (object (cast JavaObjectTerm $X)))))))))) 
      (write-if-fail 
        (op 
          (set-det) 
          (unify $Y $EXP)) Nil 8 $Out)))
  (= 
    (write-inline0 
      (ground $X) $_ $Out) 
    ( (set-det) (write-if-fail (op (set-det) (@ (isGround $X))) (:: $X) 8 $Out)))
;
;                                          Term comparison

  (= 
    (write-inline0 
      ($equality-of-term $X $Y) $_ $Out) 
    ( (set-det) (write-if-fail (op (set-det) (@ (equals $X $Y))) (:: $X $Y) 8 $Out)))
  (= 
    (write-inline0 
      ($inequality-of-term $X $Y) $_ $Out) 
    ( (set-det) (write-if-fail (@ (equals $X $Y)) (:: $X $Y) 8 $Out)))
  (= 
    (write-inline0 
      ($after $X $Y) $_ $Out) 
    ( (set-det) (write-if-fail (op <= (@ (compareTo $X $Y)) 0) (:: $X $Y) 8 $Out)))
  (= 
    (write-inline0 
      ($before $X $Y) $_ $Out) 
    ( (set-det) (write-if-fail (op >= (@ (compareTo $X $Y)) 0) (:: $X $Y) 8 $Out)))
  (= 
    (write-inline0 
      ($not-after $X $Y) $_ $Out) 
    ( (set-det) (write-if-fail (op > (@ (compareTo $X $Y)) 0) (:: $X $Y) 8 $Out)))
  (= 
    (write-inline0 
      ($not-before $X $Y) $_ $Out) 
    ( (set-det) (write-if-fail (op < (@ (compareTo $X $Y)) 0) (:: $X $Y) 8 $Out)))
  (= 
    (write-inline0 
      ($identical-or-cannot-unify $X $Y) $_ $Out) 
    ( (set-det) (write-if-fail (op && (op (set-det) (@ (equals $X $Y))) (unify $X $Y)) (:: $X $Y) 8 $Out)))
;
;                                          Term creation and decomposition

  (= 
    (write-inline0 
      (copy-term $X $Y) $_ $Out) 
    ( (nonvar $X) 
      (nonvar $Y) 
      (set-det) 
      (write-if-fail 
        (op 
          (set-det) 
          (unify $Y 
            (# (engine.copy $X)))) 
        (:: $X) 8 $Out)))
;
;                                          Arithmetic evaluation

  (= 
    (write-inline0 
      (is $X $Y) $_ $Out) 
    ( (set-det) (write-arith $_ $Y $X 8 $Out)))
  (= 
    (write-inline0 
      ($abs $X $Y) $_ $Out) 
    ( (set-det) (write-arith abs $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($asin $X $Y) $_ $Out) 
    ( (set-det) (write-arith asin $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($acos $X $Y) $_ $Out) 
    ( (set-det) (write-arith acos $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($atan $X $Y) $_ $Out) 
    ( (set-det) (write-arith atan $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($bitwise-conj $X $Y $Z) $_ $Out) 
    ( (set-det) (write-arith and $X $Y $Z 8 $Out)))
  (= 
    (write-inline0 
      ($bitwise-disj $X $Y $Z) $_ $Out) 
    ( (set-det) (write-arith or $X $Y $Z 8 $Out)))
  (= 
    (write-inline0 
      ($bitwise-exclusive-or $X $Y $Z) $_ $Out) 
    ( (set-det) (write-arith xor $X $Y $Z 8 $Out)))
  (= 
    (write-inline0 
      ($bitwise-neg $X $Y) $_ $Out) 
    ( (set-det) (write-arith not $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($ceil $X $Y) $_ $Out) 
    ( (set-det) (write-arith ceil $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($cos $X $Y) $_ $Out) 
    ( (set-det) (write-arith cos $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($degrees $X $Y) $_ $Out) 
    ( (set-det) (write-arith toDegrees $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($exp $X $Y) $_ $Out) 
    ( (set-det) (write-arith exp $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($float $X $Y) $_ $Out) 
    ( (set-det) (write-arith toFloat $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($float-integer-part $X $Y) $_ $Out) 
    ( (set-det) (write-arith floatIntPart $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($float-fractional-part $X $Y) $_ $Out) 
    ( (set-det) (write-arith floatFractPart $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($float-quotient $X $Y $Z) $_ $Out) 
    ( (set-det) (write-arith divide $X $Y $Z 8 $Out)))
  (= 
    (write-inline0 
      ($floor $X $Y) $_ $Out) 
    ( (set-det) (write-arith floor $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($int-quotient $X $Y $Z) $_ $Out) 
    ( (set-det) (write-arith intDivide $X $Y $Z 8 $Out)))
  (= 
    (write-inline0 
      ($log $X $Y) $_ $Out) 
    ( (set-det) (write-arith log $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($max $X $Y $Z) $_ $Out) 
    ( (set-det) (write-arith max $X $Y $Z 8 $Out)))
  (= 
    (write-inline0 
      ($min $X $Y $Z) $_ $Out) 
    ( (set-det) (write-arith min $X $Y $Z 8 $Out)))
  (= 
    (write-inline0 
      ($minus $X $Y $Z) $_ $Out) 
    ( (set-det) (write-arith subtract $X $Y $Z 8 $Out)))
  (= 
    (write-inline0 
      ($mod $X $Y $Z) $_ $Out) 
    ( (set-det) (write-arith mod $X $Y $Z 8 $Out)))
  (= 
    (write-inline0 
      ($multi $X $Y $Z) $_ $Out) 
    ( (set-det) (write-arith multiply $X $Y $Z 8 $Out)))
  (= 
    (write-inline0 
      ($plus $X $Y $Z) $_ $Out) 
    ( (set-det) (write-arith add $X $Y $Z 8 $Out)))
  (= 
    (write-inline0 
      ($pow $X $Y $Z) $_ $Out) 
    ( (set-det) (write-arith pow $X $Y $Z 8 $Out)))
  (= 
    (write-inline0 
      ($radians $X $Y) $_ $Out) 
    ( (set-det) (write-arith toRadians $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($rint $X $Y) $_ $Out) 
    ( (set-det) (write-arith rint $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($round $X $Y) $_ $Out) 
    ( (set-det) (write-arith round $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($shift-left $X $Y $Z) $_ $Out) 
    ( (set-det) (write-arith shiftLeft $X $Y $Z 8 $Out)))
  (= 
    (write-inline0 
      ($shift-right $X $Y $Z) $_ $Out) 
    ( (set-det) (write-arith shiftRight $X $Y $Z 8 $Out)))
  (= 
    (write-inline0 
      ($sign $X $Y) $_ $Out) 
    ( (set-det) (write-arith signum $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($sin $X $Y) $_ $Out) 
    ( (set-det) (write-arith sin $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($sqrt $X $Y) $_ $Out) 
    ( (set-det) (write-arith sqrt $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($tan $X $Y) $_ $Out) 
    ( (set-det) (write-arith tan $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($truncate $X $Y) $_ $Out) 
    ( (set-det) (write-arith truncate $X $Y 8 $Out)))
;
;                                          Arithmetic comparison

  (= 
    (write-inline0 
      ($arith-equal $X $Y) $_ $Out) 
    ( (set-det) (write-arith-compare != $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($arith-not-equal $X $Y) $_ $Out) 
    ( (set-det) (write-arith-compare == $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($greater-or-equal $X $Y) $_ $Out) 
    ( (set-det) (write-arith-compare < $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($greater-than $X $Y) $_ $Out) 
    ( (set-det) (write-arith-compare <= $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($less-or-equal $X $Y) $_ $Out) 
    ( (set-det) (write-arith-compare > $X $Y 8 $Out)))
  (= 
    (write-inline0 
      ($less-than $X $Y) $_ $Out) 
    ( (set-det) (write-arith-compare >= $X $Y 8 $Out)))


  (= 
    (write-deref-args Nil $_) 
    (set-det))
  (= 
    (write-deref-args 
      (Cons  
        (s $_) $Xs) $Out) 
    ( (set-det) (write-deref-args $Xs $Out)))
  (= 
    (write-deref-args 
      (Cons  
        (si $_) $Xs) $Out) 
    ( (set-det) (write-deref-args $Xs $Out)))
;                                         ; ???


  (= 
    (write-deref-args 
      (Cons  
        (sf $_) $Xs) $Out) 
    ( (set-det) (write-deref-args $Xs $Out)))
;                                         ; ???


  (= 
    (write-deref-args 
      (Cons  $X $Xs) $Out) 
    ( (write-java0 
        (deref $X $X) $_ $Out) (write-deref-args $Xs $Out)))


  (= 
    (write-if-fail $Cond $Args $Tab $Out) 
    ( (nonvar $Cond) 
      (ground $Args) 
      (set-det) 
      (= $EXP 
        (if-then $Cond 'return engine.fail()')) 
      (write-deref-args $Args $Out) 
      (write-inline-java $EXP $Tab $Out)))


  (= 
    (make-arith-arg $E $_) 
    ( (var $E) 
      (set-det) 
      (fail)))
  (= 
    (make-arith-arg $E $E) 
    ( (= $E 
        (si $_)) (set-det)))
  (= 
    (make-arith-arg $E $E) 
    ( (= $E 
        (sf $_)) (set-det)))
;
;                                         make_arith_arg(E, cast('NumberTerm',E)) :- E = a(_), !. ;???

  (= 
    (make_arith_arg  $E 
      (#  
        (Arithmetic.evaluate  $E))) True)


  (= 
    (write-arith $M $E $V $Tab $Out) 
    ( (make-arith-arg $E $A1) 
      (nonvar $V) 
      (det-if-then-else 
        (nonvar $M) 
        (, 
          (=.. $A0 
            (:: $M $A1)) 
          (= $A 
            (@ $A0))) 
        (= $A $A1)) 
      (= $EXP 
        (if-then 
          (op 
            (set-det) 
            (unify $V $A)) 'return engine.fail()')) 
      (write-inline-java $EXP $Tab $Out)))
;                                         ;EXP  = try_catch(SENT, 'BuiltinException', ['e.goal = this','throw e']),

;                                         ;write_deref_args([E], Out),



  (= 
    (write-arith $M $E1 $E2 $V $Tab $Out) 
    ( (nonvar $M) 
      (make-arith-arg $E1 $A1) 
      (make-arith-arg $E2 $A2) 
      (nonvar $V) 
      (=.. $A0 
        (:: $M $A1 $A2)) 
      (= $A 
        (@ $A0)) 
      (= $EXP 
        (if-then 
          (op 
            (set-det) 
            (unify $V $A)) 'return engine.fail()')) 
      (write-inline-java $EXP $Tab $Out)))
;                                         ;EXP = try_catch(SENT, 'BuiltinException', ['e.goal = this','throw e']),

;                                         ;write_deref_args([E1,E2], Out),




  (= 
    (write-arith-compare $M $E1 $E2 $Tab $Out) 
    ( (nonvar $M) 
      (make-arith-arg $E1 $A1) 
      (make-arith-arg $E2 $A2) 
      (=.. $A0 
        (:: arithCompareTo $A1 $A2)) 
      (= $A 
        (@ $A0)) 
      (= $EXP 
        (if-then 
          (op $M $A 0) 'return engine.fail()')) 
      (write-inline-java $EXP $Tab $Out)))
;                                         ;EXP = try_catch(SENT, 'BuiltinException', ['e.goal = this','throw e']),

;                                         ;write_deref_args([E1,E2], Out),




  (= 
    (write-inline-java $X $_ $_) 
    ( (var $X) 
      (set-det) 
      (fail)))
  (= 
    (write-inline-java Nil $_ $_) 
    (set-det))
  (= 
    (write-inline-java 
      (Cons  $X $Xs) $Tab $Out) 
    ( (set-det) 
      (write-inline-java $X $Tab $Out) 
      (write-inline-java $Xs $Tab $Out)))
  (= 
    (write-inline-java 
      (try-catch $TRY $EXCEPT $CATCH) $Tab $Out) 
    ( (set-det) 
      (tab $Out $Tab) 
      (write $Out 'try {') 
      (nl $Out) 
      (is $Tab1 
        (+ $Tab 4)) 
      (write-inline-java $TRY $Tab1 $Out) 
      (tab $Out $Tab) 
      (write $Out '} catch (') 
      (write $Out $EXCEPT) 
      (write $Out ' e) {') 
      (nl $Out) 
      (write-inline-java $CATCH $Tab1 $Out) 
      (tab $Out $Tab) 
      (write $Out }) 
      (nl $Out)))
  (= 
    (write-inline-java 
      (if-then $IF $THEN) $Tab $Out) 
    ( (set-det) 
      (tab $Out $Tab) 
      (write $Out 'if (') 
      (write-inline-exp $IF 0 $Out) 
      (write $Out ) {) 
      (nl $Out) 
      (is $Tab1 
        (+ $Tab 4)) 
      (write-inline-java $THEN $Tab1 $Out) 
      (tab $Out $Tab) 
      (write $Out }) 
      (nl $Out)))
  (= 
    (write-inline-java 
      (if-then-else $IF $THEN $ELSE) $Tab $Out) 
    ( (set-det) 
      (tab $Out $Tab) 
      (write $Out 'if (') 
      (write-inline-exp $IF 0 $Out) 
      (write $Out ) {) 
      (nl $Out) 
      (is $Tab1 
        (+ $Tab 4)) 
      (write-inline-java $THEN $Tab1 $Out) 
      (tab $Out $Tab) 
      (write $Out '} else {') 
      (nl $Out) 
      (write-inline-java $ELSE $Tab1 $Out) 
      (tab $Out $Tab) 
      (write $Out }) 
      (nl $Out)))
  (= 
    (write-inline-java $X $Tab $Out) 
    ( (tab $Out $Tab) 
      (write $Out $X) 
      (write $Out or) 
      (nl $Out)))


  (= 
    (write-inline-exp $X $_ $_) 
    ( (var $X) 
      (set-det) 
      (fail)))
  (= 
    (write-inline-exp Nil $_ $_) 
    (set-det))
  (= 
    (write-inline-exp 
      (:: $X) $Tab $Out) 
    ( (set-det) (write-inline-exp $X $Tab $Out)))
  (= 
    (write-inline-exp 
      (Cons  $X $Xs) $Tab $Out) 
    ( (set-det) 
      (write-inline-exp $X $Tab $Out) 
      (write $Out ,) 
      (write-inline-exp $Xs 0 $Out)))
  (= 
    (write-inline-exp 
      (bracket $Exp) $Tab $Out) 
    ( (set-det) 
      (tab $Out $Tab) 
      (write $Out () 
      (write-inline-exp $Exp 0 $Out) 
      (write $Out ))))
  (= 
    (write-inline-exp 
      (op $Op $Exp) $Tab $Out) 
    ( (set-det) 
      (tab $Out $Tab) 
      (write $Out $Op) 
      (write $Out ' ') 
      (write-inline-exp $Exp 0 $Out)))
  (= 
    (write-inline-exp 
      (op $Op $Exp1 $Exp2) $Tab $Out) 
    ( (set-det) 
      (tab $Out $Tab) 
      (write-inline-exp $Exp1 0 $Out) 
      (write $Out ' ') 
      (write $Out $Op) 
      (write $Out ' ') 
      (write-inline-exp $Exp2 0 $Out)))
  (= 
    (write-inline-exp 
      (instanceof $Exp $Class) $Tab $Out) 
    ( (set-det) 
      (tab $Out $Tab) 
      (write $Out () 
      (write-inline-exp $Exp 0 $Out) 
      (write $Out ' instanceof ') 
      (write $Out $Class) 
      (write $Out ))))
  (= 
    (write-inline-exp 
      (cast $Class $Exp) $Tab $Out) 
    ( (set-det) 
      (tab $Out $Tab) 
      (write $Out (() 
      (write $Out $Class) 
      (write $Out ) ) 
      (write-inline-exp $Exp 0 $Out) 
      (write $Out ))))
  (= 
    (write-inline-exp 
      (unify $X $Y) $Tab $Out) 
    ( (set-det) 
      (tab $Out $Tab) 
      (write-inline-exp $X 0 $Out) 
      (write $Out .unify() 
      (write-inline-exp $Y 0 $Out) 
      (write $Out ', engine.trail)')))
  (= 
    (write-inline-exp 
      (# $X) $Tab $Out) 
    ( (set-det) 
      (=.. $X 
        (Cons  $F $As)) 
      (tab $Out $Tab) 
      (write $Out $F) 
      (write $Out () 
      (write-inline-exp $As 0 $Out) 
      (write $Out ))))
  (= 
    (write-inline-exp 
      (@ $X) $Tab $Out) 
    ( (set-det) 
      (=.. $X 
        (Cons  $F $As)) 
      (write-inline-method $F $As $Tab $Out)))
  (= 
    (write-inline-exp $X $Tab $Out) 
    ( (= $X 
        (s $_)) 
      (set-det) 
      (tab $Out $Tab) 
      (write-reg $X $Out)))
  (= 
    (write-inline-exp $X $Tab $Out) 
    ( (= $X 
        (si $_)) 
      (set-det) 
      (tab $Out $Tab) 
      (write-reg $X $Out)))
;                                         ; ???


  (= 
    (write-inline-exp $X $Tab $Out) 
    ( (= $X 
        (sf $_)) 
      (set-det) 
      (tab $Out $Tab) 
      (write-reg $X $Out)))
;                                         ; ???


  (= 
    (write-inline-exp $X $Tab $Out) 
    ( (= $X 
        (a $_)) 
      (set-det) 
      (tab $Out $Tab) 
      (write-reg $X $Out)))
  (= 
    (write-inline-exp $X $Tab $Out) 
    ( (== $X void) 
      (set-det) 
      (tab $Out $Tab) 
      (write-reg $X $Out)))
;                                         ; ???


  (= 
    (write-inline-exp $X $Tab $Out) 
    ( (tab $Out $Tab) (write $Out $X)))


  (= 
    (write-inline-method $F $_ $_ $_) 
    ( (var $F) 
      (set-det) 
      (fail)))
  (= 
    (write-inline-method $_ $A $_ $_) 
    ( (var $A) 
      (set-det) 
      (fail)))
  (= 
    (write-inline-method $F 
      (:: $A) $Tab $Out) 
    ( (set-det) 
      (tab $Out $Tab) 
      (write-inline-exp $A 0 $Out) 
      (write $Out .) 
      (write $Out $F) 
      (write $Out ())))
  (= 
    (write-inline-method $F 
      (:: $A $B) $Tab $Out) 
    ( (tab $Out $Tab) 
      (write-inline-exp $A 0 $Out) 
      (write $Out .) 
      (write $Out $F) 
      (write $Out () 
      (write-inline-exp $B 0 $Out) 
      (write $Out ))))



  (= 
    (write-insert $X $_ $_) 
    ( (var $X) 
      (set-det) 
      (fail)))
;                                         /*****************************************************************
;                                           Write Insert
;                                         *****************************************************************/


  (= 
    (write-insert Nil $_ $_) 
    (set-det))
  (= 
    (write-insert 
      (Cons  $X $Xs) $_ $Out) 
    ( (atom $X) 
      (write $Out $X) 
      (nl $Out) 
      (write-insert $Xs $_ $Out)))


  (= 
    (write-to-string 
      (/ $F $A) $Out) 
    ( (tab $Out 4) 
      (write $Out @Override) 
      (nl $Out) 
      (tab $Out 4) 
      (write $Out 'public void toString(StringBuilder sb) {') 
      (nl $Out) 
      (predicate-encoding $F $F1) 
      (tab $Out 8) 
      (write $Out 'sb.append("') 
      (write $Out $F1) 
      (det-if-then-else 
        (> $A 0) 
        (, 
          (write $Out (");) 
          (nl $Out) 
          (write-enum '' arg 1 $A '.toString(sb); sb.append(", "); ' .toString(sb); 8 $Out) 
          (nl $Out) 
          (tab $Out 8) 
          (write $Out 'sb.append(")");') 
          (nl $Out)) 
        (, 
          (write $Out ");) 
          (nl $Out))) 
      (tab $Out 4) 
      (write $Out }) 
      (nl $Out)))
;                                         /*****************************************************************
;                                           Write toString(StringBuilder sb)
;                                         *****************************************************************/




  (= 
    (mkdirs $Dir) 
    ( (exists-directory $Dir) (set-det)))
;                                         /*****************************************************************
;                                           Auxiliaries
;                                         *****************************************************************/

;                                         ; Create a directory if missing


  (= 
    (mkdirs $Dir) 
    ( (file-directory-name $Dir $Parent) 
      (mkdirs $Parent) 
      (catch 
        (make-directory $Dir) $_ 
        (exists-directory $Dir)))) ;
;                                          it is ok if we failed to create a directory, because it is already exist


;
;                                          int


  (= 
    (java-integer $X) 
    ( (integer $X) 
      (=< -2147483648 $X) 
      (=< $X 2147483647)))

;
;                                          Read Instructions


  (= 
    (read-instructions 0 $_ Nil) 
    (set-det))
  (= 
    (read-instructions $N $In 
      (Cons  $X $Xs)) 
    ( (> $N 0) 
      (read $In $X) 
      (is $N1 
        (- $N 1)) 
      (read-instructions $N1 $In $Xs)))

;
;                                          Write package name


  (= 
    (write-package $P $Out) 
    ( (set-det) (write $Out $P)))

;
;                                          Write class name


  (= 
    (write-class-name $L $Out) 
    ( (write $Out PRED-) (write-index $L $Out)))

;
;                                          Write out base class name


  (= 
    (write-predicate-base-class 0 $Out) 
    ( (set-det) (write $Out Predicate)))
  (= 
    (write-predicate-base-class 1 $Out) 
    ( (set-det) (write $Out Predicate.P1)))
  (= 
    (write-predicate-base-class 2 $Out) 
    ( (set-det) (write $Out Predicate.P2)))
  (= 
    (write-predicate-base-class 3 $Out) 
    ( (set-det) (write $Out Predicate.P3)))
  (= 
    (write-predicate-base-class 4 $Out) 
    ( (set-det) (write $Out Predicate.P4)))
  (= 
    (write-predicate-base-class $_ $Out) 
    ( (set-det) (write $Out Predicate.P4)))

;
;                                          Write method reference


  (= 
    (write-method-ref 
      (/ fail 0) $Out) 
    ( (set-det) (write-index (/ fail 0) $Out)))
  (= 
    (write-method-ref $R $Out) 
    ( (get-atoms  &self 
        (= 
          (current_arity  $A) $_)) 
      (get-atoms  &self 
        (= 
          (current_functor  $F) $_)) 
      (write-class-name 
        (/ $F $A) $Out) 
      (write $Out ::) 
      (write-index $R $Out)))

;
;                                          Write if method call for switch_on_term


  (= 
    (write-if-method-call $Cond 
      (/ fail 0) $Out) 
    ( (set-det) 
      (tab $Out 12) 
      (write $Out 'if (') 
      (write $Out $Cond) 
      (write $Out ) {) 
      (nl $Out) 
      (tab $Out 16) 
      (write $Out 'return engine.fail();') 
      (nl $Out) 
      (tab $Out 12) 
      (write $Out }) 
      (nl $Out)))

  (= 
    (write-if-method-call $Cond $Method $Out) 
    ( (tab $Out 12) 
      (write $Out 'if (') 
      (write $Out $Cond) 
      (write $Out ) {) 
      (nl $Out) 
      (tab $Out 16) 
      (write $Out 'return ') 
      (write-index $Method $Out) 
      (write $Out (engine);) 
      (nl $Out) 
      (tab $Out 12) 
      (write $Out }) 
      (nl $Out)))

;
;                                          Write label


  (= 
    (write-index 
      (/ $F $A) $Out) 
    ( (set-det) (write-pred-spec (/ $F $A) $Out)))
  (= 
    (write-index 
      (+ $L $I) $Out) 
    ( (write-index $L $Out) 
      (write $Out -) 
      (write $Out $I)))

;
;                                          Write constant name


  (= 
    (write-constant $X $Out) 
    ( (constant-encoding $X $Y) (write $Out $Y)))

;
;                                          Write predicate specification


  (= 
    (write-pred-spec 
      (/ $F $A) $Out) 
    ( (predicate-encoding $F $F1) 
      (write $Out $F1) 
      (write $Out -) 
      (write $Out $A)))

;
;                                          Package name as directory


  (= 
    (package-encoding $P $Dir) 
    ( (atom-codes $P $Chs0) 
      (package-encoding $Chs0 $Chs Nil) 
      (atom-codes $Dir $Chs)))


  (= 
    (-->  
      (package_encoding  ()) !) True)
  (= 
    (-->  
      (package_encoding  
        (Cons  46 $Xs)) 
      (,  ! 
        (,  
          (47) 
          (package_encoding  $Xs)))) True)
  (= 
    (-->  
      (package_encoding  
        (Cons  $X $Xs)) 
      (,  ! 
        (,  
          ($X) 
          (package_encoding  $Xs)))) True)

;
;                                          Predicate Encoding


  (= 
    (predicate-encoding $X $Y) 
    ( (atom-codes $X $Chs0) 
      (pred-encoding $Chs0 $Chs Nil) 
      (atom-codes $Y $Chs)))


  (= 
    (-->  
      (pred_encoding  ()) !) True)
  (= 
    (-->  
      (pred_encoding  
        (Cons  $X $Xs)) 
      (,  
        (pred_encoding_char  $X) 
        (pred_encoding  $Xs))) True)

  (= 
    (-->  
      (pred_encoding_char  $X) 
      (,  
        { (,  
            (=<  97 $X) 
            (=<  $X 122)) } 
        (,  ! 
          ($X)))) True) ;
;                                          a..z

  (= 
    (-->  
      (pred_encoding_char  $X) 
      (,  
        { (,  
            (=<  65 $X) 
            (=<  $X 90)) } 
        (,  ! 
          ($X)))) True) ;
;                                          A..Z

  (= 
    (-->  
      (pred_encoding_char  $X) 
      (,  
        { (,  
            (=<  48 $X) 
            (=<  $X 57)) } 
        (,  ! 
          ($X)))) True) ;
;                                          0..9

  (= 
    (-->  
      (pred_encoding_char  95) 
      (,  ! 
        (95))) True)                    ;
;                                          '_'

  (= 
    (-->  
      (pred_encoding_char  36) 
      (,  ! 
        (36))) True)                    ;
;                                          '$' ???

  (= 
    (-->  
      (pred_encoding_char  $X) 
      (,  
        { (,  
            (=<  0 $X) 
            (=<  $X 65535)) } 
        (,  ! 
          (,  
            (36) 
            (pred_encoding_hex  $X))))) True)
;                                         ; '$'


  (= 
    (-->  
      (pred_encoding_char  $X) 
      { (,  
          (am2cpp_error  
            ($X is an invalid character code)) fail) }) True)

  (= 
    (-->  
      (pred_encoding_hex  $X) 
      (,  
        { (int_to_hex  $X () $H) } 
        (pred_encoding_hex_char  $H))) True)

  (= 
    (-->  
      (pred_encoding_hex_char  ()) 
      (,  ! 
        (48 48 48 48))) True) ;
;                                          0000

  (= 
    (-->  
      (pred_encoding_hex_char  
        ($X)) 
      (,  ! 
        (48 48 48 $X))) True) ;
;                                          000X

  (= 
    (-->  
      (pred_encoding_hex_char  
        ($X $Y)) 
      (,  ! 
        (48 48 $X $Y))) True) ;
;                                          00XY

  (= 
    (-->  
      (pred_encoding_hex_char  
        ($X $Y $Z)) 
      (,  ! 
        (48 $X $Y $Z))) True) ;
;                                          0XYZ

  (= 
    (-->  
      (pred_encoding_hex_char  
        ($X $Y $Z $W)) 
      (,  ! 
        ($X $Y $Z $W))) True) ;
;                                          XYZW



  (= 
    (int-to-hex 0 $H $H) 
    (set-det))
  (= 
    (int-to-hex $D $H0 $H) 
    ( (is $R 
        (mod $D 16)) 
      (is $D1 
        (// $D 16)) 
      (hex-map $R $R1) 
      (int-to-hex $D1 
        (Cons  $R1 $H0) $H)))


  (= 
    (hex-map 10 65) 
    (set-det)) ;
;                                          'A'

  (= 
    (hex-map 11 66) 
    (set-det)) ;
;                                          'B'

  (= 
    (hex-map 12 67) 
    (set-det)) ;
;                                          'C'

  (= 
    (hex-map 13 68) 
    (set-det)) ;
;                                          'D'

  (= 
    (hex-map 14 69) 
    (set-det)) ;
;                                          'E'

  (= 
    (hex-map 15 70) 
    (set-det)) ;
;                                          'F'

  (= 
    (hex-map $X $Y) 
    ( (=< 0 $X) 
      (=< $X 9) 
      (number-codes $X 
        (:: $Y))))

;
;                                          Constant Encoding (especially, escape sequence)


  (= 
    (constant-encoding $X $Y) 
    ( (atom-codes $X $Chs0) 
      (con-encoding $Chs0 $Chs) 
      (atom-codes $Y $Chs)))
;                                         ;???




  (= 
    (con-encoding Nil Nil) 
    (set-det))
  (= 
    (con-encoding 
      (Cons  7 $Xs) 
      (Cons  92 
        (Cons  97 $Ys))) 
    ( (set-det) (con-encoding $Xs $Ys))) ;
;                                          \a

  (= 
    (con-encoding 
      (Cons  8 $Xs) 
      (Cons  92 
        (Cons  98 $Ys))) 
    ( (set-det) (con-encoding $Xs $Ys))) ;
;                                          \b

  (= 
    (con-encoding 
      (Cons  9 $Xs) 
      (Cons  92 
        (Cons  116 $Ys))) 
    ( (set-det) (con-encoding $Xs $Ys))) ;
;                                          \t

  (= 
    (con-encoding 
      (Cons  10 $Xs) 
      (Cons  92 
        (Cons  110 $Ys))) 
    ( (set-det) (con-encoding $Xs $Ys))) ;
;                                          \n

  (= 
    (con-encoding 
      (Cons  11 $Xs) 
      (Cons  92 
        (Cons  118 $Ys))) 
    ( (set-det) (con-encoding $Xs $Ys))) ;
;                                          \v

  (= 
    (con-encoding 
      (Cons  12 $Xs) 
      (Cons  92 
        (Cons  102 $Ys))) 
    ( (set-det) (con-encoding $Xs $Ys))) ;
;                                          \f

  (= 
    (con-encoding 
      (Cons  13 $Xs) 
      (Cons  92 
        (Cons  114 $Ys))) 
    ( (set-det) (con-encoding $Xs $Ys))) ;
;                                          \r

  (= 
    (con-encoding 
      (Cons  34 $Xs) 
      (Cons  92 
        (Cons  34 $Ys))) 
    ( (set-det) (con-encoding $Xs $Ys))) ;
;                                          \"

  (= 
    (con-encoding 
      (Cons  39 $Xs) 
      (Cons  92 
        (Cons  39 $Ys))) 
    ( (set-det) (con-encoding $Xs $Ys))) ;
;                                          \'

  (= 
    (con-encoding 
      (Cons  92 $Xs) 
      (Cons  92 
        (Cons  92 $Ys))) 
    ( (set-det) (con-encoding $Xs $Ys))) ;
;                                          \\

  (= 
    (con-encoding 
      (Cons  $X $Xs) 
      (Cons  $X $Ys)) 
    (con-encoding $Xs $Ys))

;
;                                          Write Register name


  (= 
    (write-reg $X $_) 
    ( (var $X) 
      (set-det) 
      (am2cpp-error (:: register expression must not be unbound variable)) 
      (fail)))
  (= 
    (write-reg void $Out) 
    ( (set-det) (write $Out 'new Var(engine)')))
  (= 
    (write-reg 
      (ea $X) $Out) 
    ( (set-det) 
      (write $Out engine.Areg[) 
      (is $Y 
        (- $X 1)) 
      (write $Out $Y) 
      (write $Out ])))
;                                         /*
;                                         write_reg(ea(1),  Out) :- !, write(Out, 'engine.areg1').
;                                         write_reg(ea(2),  Out) :- !, write(Out, 'engine.areg2').
;                                         write_reg(ea(3),  Out) :- !, write(Out, 'engine.areg3').
;                                         write_reg(ea(4),  Out) :- !, write(Out, 'engine.areg4').
;                                         write_reg(ea(5),  Out) :- !, write(Out, 'engine.areg5').
;                                         write_reg(ea(6),  Out) :- !, write(Out, 'engine.areg6').
;                                         write_reg(ea(7),  Out) :- !, write(Out, 'engine.areg7').
;                                         write_reg(ea(8),  Out) :- !, write(Out, 'engine.areg8').
;                                         */


  (= 
    (write-reg econt $Out) 
    ( (set-det) (write $Out engine.cont)))
  (= 
    (write-reg 
      (arg $X) $Out) 
    ( (set-det) 
      (write $Out arg) 
      (write $Out $X)))
  (= 
    (write-reg 
      (a $X) $Out) 
    ( (set-det) 
      (write $Out a) 
      (write $Out $X)))
  (= 
    (write-reg 
      (s $X) $Out) 
    ( (get-atoms  &self 
        (= 
          (inlined  
            (s  $X) 
            (/  $F $A)) $_)) 
      (set-det) 
      (write $Out ") 
      (write-constant $F $Out) 
      (write $Out ")))
  (= 
    (write-reg 
      (s $X) $Out) 
    ( (get-atoms  &self 
        (= 
          (inlined  
            (s  $X) 
            (str_args  $Xs)) $_)) 
      (set-det) 
      (write-reg-args $Xs $Out)))
  (= 
    (write-reg 
      (s $X) $Out) 
    ( (set-det) 
      (write $Out s) 
      (write $Out $X)))
  (= 
    (write-reg 
      (si $X) $Out) 
    ( (set-det) 
      (write $Out si) 
      (write $Out $X))) ;
;                                          ???

  (= 
    (write-reg 
      (sf $X) $Out) 
    ( (set-det) 
      (write $Out sf) 
      (write $Out $X))) ;
;                                          ???

  (= 
    (write-reg 
      (y $X) $Out) 
    ( (set-det) 
      (write $Out y) 
      (write $Out $X)))
  (= 
    (write-reg 
      (p $X) $Out) 
    ( (set-det) 
      (write $Out p) 
      (write $Out $X)))
  (= 
    (write-reg cont $Out) 
    ( (set-det) (write $Out cont)))
  (= 
    (write-reg null $Out) 
    ( (set-det) (write $Out null)))
;
;                                          am2cpp only

  (= 
    (write-reg 
      (args $X) $Out) 
    ( (set-det) 
      (write $Out args[) 
      (write $Out $X) 
      (write $Out ])))
  (= 
    (write-reg $X $_) 
    ( (am2cpp-error (:: $X is an invalid register expression)) (fail)))


  (= 
    (write-reg-args Nil $_) 
    (set-det))
  (= 
    (write-reg-args 
      (:: $X) $Out) 
    ( (set-det) (write-reg $X $Out)))
  (= 
    (write-reg-args 
      (Cons  $X $Xs) $Out) 
    ( (write-reg $X $Out) 
      (write $Out , ) 
      (write-reg-args $Xs $Out)))


  (= 
    (for $M $M $N) 
    (=< $M $N))
;                                         /*****************************************************************
;                                                      WAM-BASED INTERMEDIATE INSTRUCTIONS
; 
;                                         Put Instructions
;                                         ================
;                                         +  put_var(X)
;                                         +  put_int(i, X)
;                                         +  put_float(f, X)
;                                         +  put_con(f/n, X)
;                                         +  put_con(c, X),
;                                         +  put_list(Xi, Xj, Xk)
;                                         +  put_str(Xi, Y, Xj)
;                                         +  put_str_args([Xi,..,Xn], Y)
;                                         +  put_clo(p:G, X)
; 
;                                         Get Instructions
;                                         ================
;                                         +  get_val(Xi, Xj)
;                                         +  get_int(i, Xi, Xj)
;                                         +  get_float(f, Xi, Xj)
;                                         +  get_con(c, Xi, Xj)
;                                         +  get_ground(g, Xi, Xj)
;                                         +  get_list(X)
;                                         +  get_str(f/n, Xi, Xj)
; 
;                                         Unify Instructions
;                                         ==================
;                                         +  unify_var(X)
;                                         +  unify_val(X)
;                                         +  unify_int(i, X)
;                                         +  unify_float(f, X)
;                                         +  unify_con(c, X)
;                                         +  unify_ground(g, X)
;                                         +  unify_void(i)
; 
;                                         Choice Instructions
;                                         ===================
;                                         +  try(Li, Lj)
;                                         +  retry(Li, Lj)
;                                         +  trust(L)
; 
;                                         Indexing Instructions
;                                         =====================
;                                         +  switch_on_term(Lv, Li, Lf, Lc, Ls, Ll)
;                                         +  switch_on_hash(TAG, i, L, hashtable)
; 
;                                         Other Instructions
;                                         ==================
;                                         +  comment(Message)
;                                         +  debug(Message)
; 
;                                         +  begin_predicate(p, f/n)
;                                         +  end_predicate(p, f/n)
; 
;                                         +  import_package(p)
;                                         +  import_package(p, f/n)
; 
;                                         +  main(f/n, public): [Instructions]
;                                         +  main(f/n, non-public): [Instructions]
;                                         +  L: [Instructions]
; 
;                                         +  label(L)
;                                         +  setB0
;                                         +  goto(L)
;                                         +  deref(Ri, Rj)
;                                         +  set(Ri, Rj)
; 
;                                         +  decl_term_vars([R1,...,Rn])
;                                         +  decl_pred_vars([R1,...,Rn])
; 
;                                         +  put_cont(p:BinG, C)
;                                         +  put_cont(BinG, C)
;                                         +  execute(p:BinG)
;                                         +  execute(BinG)
;                                         +  inline(G)
; 
;                                         +  new_hash(TAG, i)
;                                         +  put_hash(X, L, TAG)
; 
;                                         +  static([Instructions])
; 
;                                         Notation
;                                         ********
;                                           X ::= a(i) | S
;                                           Y ::= y(i) | S
;                                           S ::= s(i) | si(i) | sf(i)
;                                           L ::= f/n | f/n+i | f/n+TAG | f/n+TAG+i | f/n+TAG+i+i
;                                           TAG ::= var | int | flo | con | str | lis | top | sub | nil
;                                           BinG ::= C | f(A1,..,An, C)
;                                           G ::= f(A1,..,An)
;                                           A ::= void | X
;                                           C ::= cont | p(N)
;                                           R ::= cont | econt | a(i) | arg(i) | ea(i)
; 
;                                         *****************************************************************/

;                                         /*****************************************************************
;                                           Utilities
;                                         *****************************************************************/


  (= 
    (for $I $M $N) 
    ( (=< $M $N) 
      (is $M1 
        (+ $M 1)) 
      (for $I $M1 $N)))

;
;                                         ;; print


  (= 
    (am2cpp-error $M) 
    (am2cpp-message user-error 
      (Cons  *** 
        (Cons  AM2JAVA 
          (Cons  ERROR $M)))))


  (= 
    (am2cpp-message $M) 
    (am2cpp-message user-output $M))

  (= 
    (am2cpp-message $Stream Nil) 
    ( (nl $Stream) (flush-output $Stream)))
  (= 
    (am2cpp-message (Cons  $M $Ms)) 
    ( (write $Stream $M) 
      (write $Stream ' ') 
      (am2cpp-message $Stream $Ms)))

;
;                                         ;; list


  (= 
    (am2cpp_append  () $Zs $Zs) True)
  (= 
    (am2cpp-append 
      (Cons  $X $Xs) $Ys 
      (Cons  $X $Zs)) 
    (am2cpp-append $Xs $Ys $Zs))


  (= 
    (-->  
      (flatten_list  ()) !) True)
  (= 
    (-->  
      (flatten_list  
        (Cons  $L1 $L2)) 
      (,  ! 
        (,  
          (flatten_list  $L1) 
          (flatten_list  $L2)))) True)
  (= 
    (-->  
      (flatten_list  $L) 
      ($L)) True)


  (= 
    (list-to-string $List $String) 
    ( (list-to-chars $List $Chars0) 
      (flatten-list $Chars0 $Chars Nil) 
      (atom-codes $String $Chars)))


  (= 
    (list-to-chars Nil Nil) 
    (set-det))
  (= 
    (list-to-chars 
      (Cons  $L $Ls) 
      (Cons  $C $Cs)) 
    ( (atom $L) 
      (set-det) 
      (atom-codes $L $C) 
      (list-to-chars $Ls $Cs)))
  (= 
    (list-to-chars 
      (Cons  $L $Ls) 
      (Cons  $C $Cs)) 
    ( (number $L) 
      (set-det) 
      (number-codes $L $C) 
      (list-to-chars $Ls $Cs)))

;
;                                          END

;
;                                          written by SICStus MeTTa 3.12.8


