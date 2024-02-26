  !(op 1150 fx package)

  (= 
    (package $X) 
    (nb-setval package $X))

  !(package com.googlecode.prolog-cafe.builtin)
  !(public (/ system-predicate 1))
  !(multifile (/ system-predicate 1))
  !(dynamic (/ system-predicate 1))


  (= 
    (system_predicate  
      (system_predicate  $_)) True)
;
;                           Control constructs

  (= 
    (system_predicate  true) True)
  (= 
    (system_predicate  therwise) True)
  (= 
    (system_predicate  fail) True)
  (= 
    (system_predicate  false) True)
  (= 
    (system_predicate  !) True)
  (= 
    (system_predicate  
      ($get_level  $_)) True)
  (= 
    (system_predicate  $neck_cut) True)
  (= 
    (system_predicate  
      ($cut  $_)) True)
  (= 
    (system_predicate  
      (^  $_ $_)) True)
  (= 
    (system_predicate  
      (,  $_ $_)) True)
  (= 
    (system_predicate  
      (;  $_ $_)) True)
  (= 
    (system_predicate  
      (->  $_ $_)) True)
  (= 
    (system_predicate  
      (call  $_)) True)
  (= 
    (system_predicate  
      (catch  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (throw  $_)) True)
  (= 
    (system_predicate  
      (on_exception  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (raise_exception  $_)) True)
;
;                                     Term unification

  (= 
    (system_predicate  
      (= $_ $_)) True)
  (= 
    (system_predicate  
      ($unify  $_ $_)) True)
  (= 
    (system_predicate  
      (\=  $_ $_)) True)
  (= 
    (system_predicate  
      ($not_unifiable  $_ $_)) True)
;
;                                        Type testing

  (= 
    (system_predicate  
      (var  $_)) True)
  (= 
    (system_predicate  
      (atom  $_)) True)
  (= 
    (system_predicate  
      (integer  $_)) True)
  (= 
    (system_predicate  
      (long  $_)) True)
  (= 
    (system_predicate  
      (float  $_)) True)
  (= 
    (system_predicate  
      (atomic  $_)) True)
  (= 
    (system_predicate  
      (compound  $_)) True)
  (= 
    (system_predicate  
      (nonvar  $_)) True)
  (= 
    (system_predicate  
      (number  $_)) True)
  (= 
    (system_predicate  
      (java  $_)) True)
  (= 
    (system_predicate  
      (java  $_ $_)) True)
  (= 
    (system_predicate  
      (closure  $_)) True)
  (= 
    (system_predicate  
      (ground  $_)) True)
  (= 
    (system_predicate  
      (callable  $_)) True)
;
;                                          Term comparison

  (= 
    (system_predicate  
      (==  $_ $_)) True)
  (= 
    (system_predicate  
      ($equality_of_term  $_ $_)) True)
  (= 
    (system_predicate  
      (\==  $_ $_)) True)
  (= 
    (system_predicate  
      ($inequality_of_term  $_ $_)) True)
  (= 
    (system_predicate  
      (@<  $_ $_)) True)
  (= 
    (system_predicate  
      ($before  $_ $_)) True)
  (= 
    (system_predicate  
      (@>  $_ $_)) True)
  (= 
    (system_predicate  
      ($after  $_ $_)) True)
  (= 
    (system_predicate  
      (@=<  $_ $_)) True)
  (= 
    (system_predicate  
      ($not_after  $_ $_)) True)
  (= 
    (system_predicate  
      (@>=  $_ $_)) True)
  (= 
    (system_predicate  
      ($not_before  $_ $_)) True)
  (= 
    (system_predicate  
      (?=  $_ $_)) True)
  (= 
    (system_predicate  
      ($identical_or_cannot_unify  $_ $_)) True)
  (= 
    (system_predicate  
      (compare  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (sort  $_ $_)) True)
  (= 
    (system_predicate  
      (keysort  $_ $_)) True)
;
;                                         system_predicate(merge(_,_,_)).

;
;                                          Term creation and decomposition

  (= 
    (system_predicate  
      (arg  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (functor  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (=..  $_ $_)) True)
  (= 
    (system_predicate  
      ($univ  $_ $_)) True)
  (= 
    (system_predicate  
      (copy_term  $_ $_)) True)
;
;                                          Arithmetic evaluation

  (= 
    (system_predicate  
      (is  $_ $_)) True)
  (= 
    (system_predicate  
      ($abs  $_ $_)) True)
  (= 
    (system_predicate  
      ($asin  $_ $_)) True)
  (= 
    (system_predicate  
      ($acos  $_ $_)) True)
  (= 
    (system_predicate  
      ($atan  $_ $_)) True)
  (= 
    (system_predicate  
      ($bitwise_conj  $_ $_ $_)) True)
  (= 
    (system_predicate  
      ($bitwise_disj  $_ $_ $_)) True)
  (= 
    (system_predicate  
      ($bitwise_exclusive_or  $_ $_ $_)) True)
  (= 
    (system_predicate  
      ($bitwise_neg  $_ $_)) True)
  (= 
    (system_predicate  
      ($ceil  $_ $_)) True)
  (= 
    (system_predicate  
      ($cos  $_ $_)) True)
  (= 
    (system_predicate  
      ($degrees  $_ $_)) True)
  (= 
    (system_predicate  
      ($exp  $_ $_)) True)
  (= 
    (system_predicate  
      ($float  $_ $_)) True)
  (= 
    (system_predicate  
      ($float_integer_part  $_ $_)) True)
  (= 
    (system_predicate  
      ($float_fractional_part  $_ $_)) True)
  (= 
    (system_predicate  
      ($float_quotient  $_ $_ $_)) True)
  (= 
    (system_predicate  
      ($floor  $_ $_)) True)
  (= 
    (system_predicate  
      ($int_quotient  $_ $_ $_)) True)
  (= 
    (system_predicate  
      ($log  $_ $_)) True)
  (= 
    (system_predicate  
      ($max  $_ $_ $_)) True)
  (= 
    (system_predicate  
      ($min  $_ $_ $_)) True)
  (= 
    (system_predicate  
      ($minus  $_ $_ $_)) True)
  (= 
    (system_predicate  
      ($mod  $_ $_ $_)) True)
  (= 
    (system_predicate  
      ($multi  $_ $_ $_)) True)
  (= 
    (system_predicate  
      ($plus  $_ $_ $_)) True)
  (= 
    (system_predicate  
      ($pow  $_ $_ $_)) True)
  (= 
    (system_predicate  
      ($radians  $_ $_)) True)
  (= 
    (system_predicate  
      ($rint  $_ $_)) True)
  (= 
    (system_predicate  
      ($round  $_ $_)) True)
  (= 
    (system_predicate  
      ($shift_left  $_ $_ $_)) True)
  (= 
    (system_predicate  
      ($shift_right  $_ $_ $_)) True)
  (= 
    (system_predicate  
      ($sign  $_ $_)) True)
  (= 
    (system_predicate  
      ($sin  $_ $_)) True)
  (= 
    (system_predicate  
      ($sqrt  $_ $_)) True)
  (= 
    (system_predicate  
      ($tan  $_ $_)) True)
  (= 
    (system_predicate  
      ($truncate  $_ $_)) True)
;
;                                          Arithmetic comparison

  (= 
    (system_predicate  
      (=:=  $_ $_)) True)
  (= 
    (system_predicate  
      ($arith_equal  $_ $_)) True)
  (= 
    (system_predicate  
      (=\=  $_ $_)) True)
  (= 
    (system_predicate  
      ($arith_not_equal  $_ $_)) True)
  (= 
    (system_predicate  
      (<  $_ $_)) True)
  (= 
    (system_predicate  
      ($less_than  $_ $_)) True)
  (= 
    (system_predicate  
      (=<  $_ $_)) True)
  (= 
    (system_predicate  
      ($less_or_equal  $_ $_)) True)
  (= 
    (system_predicate  
      (>  $_ $_)) True)
  (= 
    (system_predicate  
      ($greater_than  $_ $_)) True)
  (= 
    (system_predicate  
      (>=  $_ $_)) True)
  (= 
    (system_predicate  
      ($greater_or_equal  $_ $_)) True)
;
;                                          Clause retrieval and information

  (= 
    (system_predicate  
      (clause  $_ $_)) True)
  (= 
    (system_predicate  
      (initialization  $_ $_)) True)
  (= 
    (system_predicate  
      ($new_indexing_hash  $_ $_ $_)) True)
;
;                                          Clause creation and destruction

  (= 
    (system_predicate  
      (assert  $_)) True)
  (= 
    (system_predicate  
      (assertz  $_)) True)
  (= 
    (system_predicate  
      (asserta  $_)) True)
  (= 
    (system_predicate  
      (retract  $_)) True)
  (= 
    (system_predicate  
      (abolish  $_)) True)
  (= 
    (system_predicate  
      (retractall  $_)) True)
;
;                                          All solutions

  (= 
    (system_predicate  
      (findall  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (bagof  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (setof  $_ $_ $_)) True)
;
;                                          Stream selection and control

  (= 
    (system_predicate  
      (current_input  $_)) True)
  (= 
    (system_predicate  
      (current_output  $_)) True)
  (= 
    (system_predicate  
      (set_input  $_)) True)
  (= 
    (system_predicate  
      (set_output  $_)) True)
  (= 
    (system_predicate  
      (open  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (open  $_ $_ $_ $_)) True)
  (= 
    (system_predicate  
      (close  $_)) True)
  (= 
    (system_predicate  
      (close  $_ $_)) True)
  (= 
    (system_predicate  
      (flush_output  $_)) True)
  (= 
    (system_predicate  flush_output) True)
  (= 
    (system_predicate  
      (stream_property  $_ $_)) True)
;
;                                          Character input/output

  (= 
    (system_predicate  
      (get_char  $_)) True)
  (= 
    (system_predicate  
      (get_char  $_ $_)) True)
  (= 
    (system_predicate  
      (get_code  $_)) True)
  (= 
    (system_predicate  
      (get_code  $_ $_)) True)
  (= 
    (system_predicate  
      (peek_char  $_)) True)
  (= 
    (system_predicate  
      (peek_char  $_ $_)) True)
  (= 
    (system_predicate  
      (peek_code  $_)) True)
  (= 
    (system_predicate  
      (peek_code  $_ $_)) True)
  (= 
    (system_predicate  
      (put_char  $_)) True)
  (= 
    (system_predicate  
      (put_char  $_ $_)) True)
  (= 
    (system_predicate  
      (put_code  $_)) True)
  (= 
    (system_predicate  
      (put_code  $_ $_)) True)
  (= 
    (system_predicate  nl) True)
  (= 
    (system_predicate  
      (nl  $_)) True)
  (= 
    (system_predicate  
      (get0  $_)) True)
  (= 
    (system_predicate  
      (get0  $_ $_)) True)
  (= 
    (system_predicate  
      (get  $_)) True)
  (= 
    (system_predicate  
      (get  $_ $_)) True)
  (= 
    (system_predicate  
      (put  $_)) True)
  (= 
    (system_predicate  
      (put  $_ $_)) True)
  (= 
    (system_predicate  
      (tab  $_)) True)
  (= 
    (system_predicate  
      (tab  $_ $_)) True)
  (= 
    (system_predicate  
      (skip  $_)) True)
  (= 
    (system_predicate  
      (skip  $_ $_)) True)
;
;                                          Byte input/output

  (= 
    (system_predicate  
      (get_byte  $_)) True)
  (= 
    (system_predicate  
      (get_byte  $_ $_)) True)
  (= 
    (system_predicate  
      (peek_byte  $_)) True)
  (= 
    (system_predicate  
      (peek_byte  $_ $_)) True)
  (= 
    (system_predicate  
      (put_byte  $_)) True)
  (= 
    (system_predicate  
      (put_byte  $_ $_)) True)
;
;                                          Term input/output

  (= 
    (system_predicate  
      (read  $_)) True)
  (= 
    (system_predicate  
      (read  $_ $_)) True)
  (= 
    (system_predicate  
      (read_with_variables  $_ $_)) True)
  (= 
    (system_predicate  
      (read_with_variables  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (read_line  $_)) True)
  (= 
    (system_predicate  
      (read_line  $_ $_)) True)
  (= 
    (system_predicate  
      (write  $_)) True)
  (= 
    (system_predicate  
      (write  $_ $_)) True)
  (= 
    (system_predicate  
      (writeq  $_)) True)
  (= 
    (system_predicate  
      (writeq  $_ $_)) True)
  (= 
    (system_predicate  
      (write_canonical  $_)) True)
  (= 
    (system_predicate  
      (write_canonical  $_ $_)) True)
  (= 
    (system_predicate  
      (write_term  $_ $_)) True)
  (= 
    (system_predicate  
      (write_term  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (op  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (current_op  $_ $_ $_)) True)
;
;                                          Logic and control

  (= 
    (system_predicate  
      (\+  $_)) True)
  (= 
    (system_predicate  
      (once  $_)) True)
  (= 
    (system_predicate  repeat) True)
;
;                                          Atomic term processing

  (= 
    (system_predicate  
      (atom_length  $_ $_)) True)
  (= 
    (system_predicate  
      (atom_concat  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (sub_atom  $_ $_ $_ $_ $_)) True)
  (= 
    (system_predicate  
      (atom_chars  $_ $_)) True)
  (= 
    (system_predicate  
      (atom_codes  $_ $_)) True)
  (= 
    (system_predicate  
      (char_code  $_ $_)) True)
  (= 
    (system_predicate  
      (number_chars  $_ $_)) True)
  (= 
    (system_predicate  
      (number_codes  $_ $_)) True)
  (= 
    (system_predicate  
      (name  $_ $_)) True)
;
;                                          Implementation defined hooks

  (= 
    (system_predicate  
      (set_prolog_flag  $_ $_)) True)
  (= 
    (system_predicate  
      (current_prolog_flag  $_ $_)) True)
  (= 
    (system_predicate  halt) True)
  (= 
    (system_predicate  
      (halt  $_)) True)
  (= 
    (system_predicate  abort) True)
;
;                                          DCG

  (= 
    (system_predicate  
      (C  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (expand_term  $_ $_)) True)
;
;                                          Hash creation and control

  (= 
    (system_predicate  
      (new_hash  $_)) True)
  (= 
    (system_predicate  
      (new_hash  $_ $_)) True)
  (= 
    (system_predicate  
      (hash_clear  $_)) True)
  (= 
    (system_predicate  
      (hash_contains_key  $_ $_)) True)
  (= 
    (system_predicate  
      (hash_get  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (hash_is_empty  $_)) True)
  (= 
    (system_predicate  
      (hash_keys  $_ $_)) True)
  (= 
    (system_predicate  
      (hash_map  $_ $_)) True)
  (= 
    (system_predicate  
      (hash_put  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (hash_remove  $_ $_)) True)
  (= 
    (system_predicate  
      (hash_size  $_ $_)) True)
  (= 
    (system_predicate  
      ($get_hash_manager  $_)) True)
;
;                                          Java interoperation

  (= 
    (system_predicate  
      (java_constructor0  $_ $_)) True)
  (= 
    (system_predicate  
      (java_constructor  $_ $_)) True)
  (= 
    (system_predicate  
      (java_declared_constructor0  $_ $_)) True)
  (= 
    (system_predicate  
      (java_declared_constructor  $_ $_)) True)
  (= 
    (system_predicate  
      (java_method0  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (java_method  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (java_declared_method0  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (java_declared_method  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (java_get_field0  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (java_get_field  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (java_get_declared_field0  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (java_get_declared_field  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (java_set_field0  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (java_set_field  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (java_set_declared_field0  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (java_set_declared_field  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (synchronized  $_ $_)) True)
  (= 
    (system_predicate  
      (java_conversion  $_ $_)) True)
;
;                                          MeTTa interpreter

  (= 
    (system_predicate  cafeteria) True)
  (= 
    (system_predicate  
      (consult  $_)) True)
  (= 
    (system_predicate  trace) True)
  (= 
    (system_predicate  notrace) True)
  (= 
    (system_predicate  debug) True)
  (= 
    (system_predicate  nodebug) True)
  (= 
    (system_predicate  
      (leash  $_)) True)
  (= 
    (system_predicate  
      (spy  $_)) True)
  (= 
    (system_predicate  
      (nospy  $_)) True)
  (= 
    (system_predicate  nospyall) True)
  (= 
    (system_predicate  listing) True)
  (= 
    (system_predicate  
      (listing  $_)) True)
;
;                                          Misc

  (= 
    (system_predicate  
      (length  $_ $_)) True)
  (= 
    (system_predicate  
      (numbervars  $_ $_ $_)) True)
  (= 
    (system_predicate  
      (statistics  $_ $_)) True)
;
;                                          END


