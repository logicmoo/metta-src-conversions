;                                           (convert-to-metta-file  nal_reader $_316108 nars_lp/nars/nal_reader.pl nars_lp/nars/nal_reader.metta)
;
; nal_reader.pl
;
; Read Non_Axiomatic Logic from MeTTa
  !(module nal-reader 
    (:: 
      (/ nal-tests 0) 
      (/ nars-tests 0) 
      (/ nal-test-files 0) 
      (/ nal-test 1) 
      (/ nal-read-clause 2) 
      (/ nal-call 3)))
; ; nal_test/2,
; ; nal_call/2,

  !(set-module (class library))
  !(set-module (base system))

  !(use-module (library logicmoo-common))

  !(use-module (library (/ logicmoo dcg-meta)))
  !(use-module (library (/ logicmoo dcg-must)))
  !(use-module (library narsese))




  (= (-->  (nal-task  $S) (,  chspace (,  ! (,  (nal-task  $S) !))))  True)
; /* task ::= [budget] sentence (* task to be processed *) sentence ::= statement"." [tense] [truth] (* judgement to be absorbed into beliefs *) | statement"?" [tense] [truth] (* question on thuth-value to be answered *) | statement"!" [desire] (* goal to be realized by operations *) | statement"@" [desire] (* question on desire-value to be answered *) statement ::= <"<">term copula term<">"> (* two terms related to each other *) | <"(">term copula term<")"> (* two terms related to each other, new notation *) | term (* a term can name a statement *) | "(^"word {","term} ")" (* an operation to be executed *) | word"("term {","term} ")" (* an operation to be executed, new notation *) copula ::= "-->" (* inheritance *) | "<->" (* similarity *) | "{--" (* instance *) | "--]" (* property *) | "{-]" (* instance-property *) | "==>" (* implication *) | "=/>" (* predictive implication *) | "=|>" (* concurrent implication *) | "=\\>" (* =\> retrospective implication *) | "<=>" (* equivalence *) | "</>" (* predictive equivalence *) | "<|>" (* concurrent equivalence *) term ::= word (* an atomic constant term *) | variable (* an atomic variable term *) | compound-term (* a term with internal structure *) | statement (* a statement can serve as a term *) compound-term ::= op-ext-set term {"," term} "}" (* extensional set *) | op-int-set term {"," term} "]" (* intensional set *) | "("op-multi"," term {"," term} ")" (* with prefix operator *) | "("op-single"," term "," term ")" (* with prefix operator *) | "(" term {op-multi term} ")" (* with infix operator *) | "(" term op-single term ")" (* with infix operator *) | "(" term {","term} ")" (* product, new notation *) | "(" op-ext-image "," term {"," term} ")"(* special case, extensional image *) | "(" op-int-image "," term {"," term} ")"(* special case, \ intensional image *) | "(" op-negation "," term ")" (* negation *) | op-negation term (* negation, new notation *) op-int-set::= "[" (* intensional set *) op-ext-set::= "{" (* extensional set *) op-negation::= "--" (* negation *) op-int-image::= "\\" (* \ intensional image *) op-ext-image::= "/" (* extensional image *) op-multi ::= "&&" (* conjunction *) | "*" (* product *) | "||" (* disjunction *) | "&|" (* parallel events *) | "&/" (* sequential events *) | "|" (* intensional intersection *) | "&" (* extensional intersection *) op-single ::= "-" (* extensional difference *) | "~" (* intensional difference *) variable ::= "$"word (* independent variable *) | "#"word (* dependent variable *) | "?"word (* query variable in question *) tense ::= ":/:" (* future event *) | ":|:" (* present event *) | ":\\:" (* :\: past event *) desire ::= truth (* same format, different interpretations *) truth ::= <";">frequency[<";">confidence]<";"> (* two numbers in [0,1]x(0,1) *) budget ::= <"$">priority[<";">durability][<";">quality]<"$"> (* three numbers in [0,1]x(0,1)x[0,1] *) word : #"[^\ ]+" (* unicode string *) priority : #"([0]?\.[0-9]+|1\.[0]*|1|0)" (* 0 <= x <= 1 *) durability : #"[0]?\.[0]*[1-9]{1}[0-9]*" (* 0 < x < 1 *) quality : #"([0]?\.[0-9]+|1\.[0]*|1|0)" (* 0 <= x <= 1 *) frequency : #"([0]?\.[0-9]+|1\.[0]*|1|0)" (* 0 <= x <= 1 *) confidence : #"[0]?\.[0]*[1-9]{1}[0-9]*" (* 0 < x < 1 *) occurrenceTime: */
  (= (-->  (nal-task  $OUT) (,  (nal-task-0  $TASK) (;  (->  (nal-three-vals  $V3) {(= $OUT  
    (nal-in  $TASK $V3)) }) {(= $OUT  $TASK) })))  True)

  (= (-->  (nal-task-0  (nal-task  $X $S $T $O $B)) (,  (optional  $B nal_budget) (,  ! (nal-sentence  $X $S $T $O))))  True)  ;
; task to be processed


  (= (-->  (nal-sentence  $X $S $T $O) (,  (nal-statement  $S) (nal-post-statement  $X $T $O)))  True)

  (= (-->  (nal-post-statement  $X $T $O) (;  (->  (nal-o  (46) $X judgement) (->  (optional  $T nal_tense) (,  (optional  $O nal_truth) !))) (;  (->  (nal-o  (63) $X question_truth) (->  (optional  $T nal_tense) (,  (optional  $O nal_truth) !))) (;  (->  (nal-o  (33) $X goal) (->  (optional  $T nal_tense) (optional  $O nal_desire))) (->  (nal-o  (64) $X question_desire) (->  (optional  $T nal_tense) (optional  $O nal_desire)))))))  True)
; /*nal_statement(S),*/
; ; judgement to be absorbed into beliefs
; /*nal_statement(S),*/
; ; question on truth_value to be answered
; /*nal_statement(S),*/
; ; goal to be realized by operations
; /*nal_statement(S),*/
; ; question on desire_value to be answered

;
; nal_statement(nal_word(S))--> nal_word(S).
  (= (-->  (nal-statement  $S) (,  (amw  (nal-statement-0  $S)) !))  True)
  (= (-->  (nal-statement-0  $S) (,  cwhite (nal-statement-0  $S)))  True)
  (= (-->  (nal-statement-0  $S) (;  (,  (amw  (60)) (,  ! (,  (nal-term  $A) (,  (nal-copula  $R) (,  (nal-term  $B) (,  (amw  (62)) {(=..  $S ($R $A $B)) })))))) (;  (,  nal_l_paren (,  (94) (,  (nal-term-list-comma  $L) (,  nal_paren_r {(= $S  
    !$L) })))) (;  (,  nal_l_paren (,  (nal-term  $A) (,  (nal-copula  $R) (,  (nal-term  $B) (,  nal_paren_r {(=..  $S ($R $A $B)) }))))) (;  (,  (nal-word  $A) (,  nal_l_paren (,  (nal-term-list-comma  $L) (,  nal_paren_r {(= $S  
    ! ([|]  $A $L)) })))) (,  (nal-term-1  $X) {(= $S  
    (nal-named-statement  $X)) }))))))  True)
; ; two terms related to each other
; ; an operation to be executed
; ; two terms related to each other, new notation
; ; an operation to be executed, new notation
; ; a term can name a statement(S)
;
; nal_statement_0(S)-->nal_rsymbol(S).

  (= (-->  (nal-copula  $X) (;  (nal-o  (45 45 62) $X inheritance) (;  (nal-o  (60 45 62) $X similarity) (;  (nal-o  (123 45 45) $X instance) (;  (nal-o  (45 45 93) $X property) (;  (nal-o  (123 45 93) $X inst_prop) (;  (nal-o  (61 61 62) $X implication) (;  (nal-o  (61 47 62) $X predictive_impl) (;  (nal-o  (61 124 62) $X concurrent_impl) (;  (nal-o  (61 92 62) $X retrospective_impl) (;  (nal-o  (60 61 62) $X equiv) (;  (nal-o  (60 47 62) $X predictive_equiv) (;  (nal-o  (60 124 62) $X concurrent_equiv) (;  (nal-o  (61 62) $X unknown_implication) (nal-o  (124 45) $X prolog_implication)))))))))))))))  True)
; ; dmiles added
; ; dmiles added

  (= (-->  (nal-term  $N) (,  (nal-term-old  $O) {(old-to-new  $O $N) }))  True)
  (= (-->  (nal-term-old  $S) (;  (nal-word  $S) (;  (nal-variable  $S) (;  (nal-compound-term  $S) (nal-statement  $S)))))  True)
; ; an atomic constant, term,
; ; an atomic variable, term,
; ; a term with internal structure
; ; a statement can serve as a term,


  (= (-->  (nal-term-0  $N) (,  (nal-term-0-old  $O) {(old-to-new  $O $N) }))  True)
  (= (-->  (nal-term-0-old  $S) (;  (nal-word-0  $S) (;  (nal-variable-0  $S) (;  (nal-compound-term-0  $S) (nal-statement-0  $S)))))  True)
; ; an atomic constant, term,
; ; an atomic variable, term,
; ; a, term, with internal structure
; ; a statement can serve as a, term,

  (= (-->  (nal-term-1  $N) (,  (nal-term-1-old  $O) (,  {(old-to-new  $O $N) } !)))  True)
  (= (-->  (nal-term-1-old  $S) (;  (nal-word  $S) (;  (nal-variable  $S) (nal-compound-term  $S))))  True)
; ; an atomic constant, term,
; ; an atomic variable, term,
; ; a, term, with internal structure


  (= (old-to-new (rel (Cons  $R (Cons  (var arg $L) $B))) (ext-image $New))  
    (length $Left $L)
    (append $Left $Right 
      (Cons  $R $B))
    (append $Left 
      (Cons  - $Right) $New)
    (set-det))
  (= (old-to-new (rel (Cons  $R (Cons  (var int $L) $B))) (int-image $New))  
    (length $Left $L)
    (append $Left $Right 
      (Cons  $R $B))
    (append $Left 
      (Cons  - $Right) $New)
    (set-det))
  (= (old-to-new  $X $X)  True)

  (= (-->  (nal-compound-term  $X) (,  (mw  (nal-compound-term-0  $X)) !))  True)

  (= (-->  (nal-compound-term-0  !($S)) (,  (94) (,  ! (,  (nal-term-1  $S) !))))  True)
  (= (-->  (nal-compound-term-0  $S) (,  (\+  (dcg-peek  (60))) (,  ! (,  (nal-compound-term-1  $S) !))))  True)
  (= (-->  (nal-compound-term-1  $S) (,  (;  (,  (nal-o  nal_op_ext_set $X ext_set) (,  (nal-term-list-comma  $L) (125))) (;  (,  (nal-o  nal_op_int_set $X int_set) (,  (nal-term-list-comma  $L) (93))) (;  (,  (nal-word-0  $A) (,  (91) (,  (nal-term-list-comma  $L) (,  (93) {(= $S  
    (v  $A $L)) })))) (;  (,  (nal-o  nal_op_negation $X negation) (,  (nal-term  $AB) {(= $L  
    ($AB)) })) (,  nal_l_paren (,  (nal-paren-compound-term  $X $L) nal_paren_r)))))) {(=..  $S ($X $L)) }))  True)
; ; extensional set
; ; intensional set
; ; @TODO notation
; ; negation, new notation

  (= (-->  (nal-paren-compound-term  $X $L) (;  (,  (nal-op-multi  $X) (,  nal_comma (nal-term-list-comma  $L))) (;  (,  (nal-op-single  $X) (,  nal_comma (,  (nal-term  $A) (,  nal_comma (,  (nal-term  $B) {(= $L  
    ($A $B)) }))))) (;  (,  (nal-o  nal_op_ext_image $X ext_image) (,  nal_comma (nal-term-list-comma  $L))) (;  (,  (nal-o  nal_op_int_image $X int_image) (,  nal_comma (nal-term-list-comma  $L))) (;  (,  (nal-o  nal_op_negation $X negation) (,  nal_comma (,  (nal-term  $AB) {(= $L  
    ($AB)) }))) (;  (,  (nal-term  $A) (,  (nal-op-multi  $X) (,  (nal-term  $B) {(= $L  
    ($A $B)) }))) (;  (,  (nal-term  $A) (,  (nal-op-single  $X) (,  (nal-term  $B) {(= $L  
    ($A $B)) }))) (;  (,  (nal-preserve-whitespace  (,  (nal-term-0  $A) (,  chspace (,  {(= $X  rel) } (nal-term-list-white  $SL (32)))))) {(= $L  
    ([|]  $A $SL)) }) (,  {(= $X  product) } (nal-term-list-comma  $L)))))))))))  True)
; ; with prefix operator
; ; with prefix operator
; ; special case, extensional image
; ; special case, \ intensional image
; ; negation
; ; with infix operator
; ; with infix operator
; ; product, new notation

  (= (-->  nal_op_int_set (91))  True)                          ;
; intensional set
  (= (-->  nal_op_ext_set (123))  True)                          ;
; extensional set
  (= (-->  nal_op_negation (45 45))  True)                        ;
; negation
  (= (-->  nal_op_int_image (92))  True)                       ;
; \ intensional image
  (= (-->  nal_op_ext_image (47))  True)                        ;
; / extensional image



  (= (nal-preserve-whitespace $DCG $S $E)  
    (locally 
      (b-setval whitespace preserve) 
      (phrase $DCG $S $E)))
;
; nal_no_preserve_whitespace(DCG,S,E) :- phrase(DCG,S,E).



  (= (-->  (nal-op-multi  $X) (;  (nal-o  (38 38) $X and) (;  (nal-o  (42) $X product) (;  (nal-o  (124 124) $X or) (;  (nal-o  (35) $X sequence_spatial) (;  (nal-o  (38 124) $X parallel_evnts) (;  (nal-o  (38 47) $X sequence_evnts) (;  (nal-o  (124) $X int_intersection) (nal-o  (38) $X ext_intersection)))))))))  True)
; ; conjunction
; ; product
; ; disjunction
; ; patham9 "sequence", wasn't really useful for NLP, it was called PART
; ; parallel events
; ; sequential events
; ; intensional intersection
; ; extensional intersection
  (= (-->  (nal-op-single  $X) (;  (nal-o  (45) $X ext_difference) (nal-o  (126) $X int_difference)))  True)
; ; extensional difference
; ; intensional difference

  (= (-->  (nal-variable  $VA) (,  (nal-variable-0  $V) (,  ! (maybe-plus-array2  $V $VA))))  True)

  (= (-->  (nal-variable-0  (var  $X $W)) (;  (,  (nal-o  (36) $X ind) (nal-word-0  $W)) (;  (,  (nal-o  (35) $X dep) (nal-word-0  $W)) (;  (,  (nal-o  (63) $X query) (nal-word-0  $W)) (;  (,  (nal-o  (47) $X arg) (nal-word-0  $W)) (,  (nal-o  (92) $X int) (nal-word-0  $W)))))))  True)
; ; independent variable
; ; dependent variable
; ; query variable in question
; ; query variable in params
; ; query variable in ....

  (= (-->  (nal-variable-0  _) (95))  True)
  (= (-->  (nal-variable-0  #) (35))  True)
  (= (-->  (nal-variable-0  $) (36))  True)


  (= (-->  (nal-tense  $X) (;  (nal-o  (58 47 58) $X future) (;  (nal-o  (58 124 58) $X present) (nal-o  (58 92 58) $X past))))  True)
; ; future event
; ; present event
; ; :\: past event

  (= (-->  (nal-tense  (t!  $X)) (,  (58 33) (,  (number  $X) (58))))  True)
  (= (-->  (nal-tense  (t  $X)) (,  (58) (,  (nal-term-1  $X) (58))))  True)

;
; Desire is same format of Truth, but different interpretations
  (= (-->  (nal-desire  $D) (nal-truth  $D))  True)									
;
; Truth is two numbers in [0,1]x(0,1)
  (= (-->  (nal-truth  ($F $C)) (,  (37) (,  ! (,  (nal-frequency  $F) (,  (optional  (,  (59) (nal-confidence  $C))) (,  (optional  (37)) !))))))  True)	                
  (= (-->  (nal-truth  ($F $C)) (,  (123) (,  (:  dcg_basics (number  $F)) (,  ! (,  {(is-float-0-1  $F) } (,  chspace (,  (:  dcg_basics (number  $C)) (,  {(is-float-0-1  $F) } (,  (125) !)))))))))  True)
  (= (-->  (nal-truth  ($F $C)) (,  (84 114 117 116 104 58) (read-nal-expected-truth  $F $C)))  True)
;
; Budget is three numbers in optional(O,0,1]x(0,1)x[0,1]
  (= (-->  (nal-budget  (nal-budget-pdq  $P $D $Q)) (,  (36) (,  ! (,  (nal-priority  $P) (,  (optional  (,  (59) (nal-durability  $D))) (,  (optional  (,  (59) (nal-quality  $Q))) (36)))))))  True)  


  (= (is-float-0-1 $F)  
    (=< 0.0 $F)
    (=< $F 1.0))

  (= (-->  (nal-word  $E) (amw  (nal-word-0  $E)))  True)

  (= (-->  (nal-word-0  (+  $E)) (,  (43) (,  (:  dcg_basics (integer  $E)) !)))  True)
  (= (-->  (nal-word-0  $E) (,  (:  dcg_basics (number  $E)) !))  True)
  (= (-->  (nal-word-0  $E) (,  (quoted-string  $E) !))  True)
  (= (-->  (nal-word-0  $E) (nal-word-str  $E))  True)

;
; nal_rsymbol(Chars,E) --> [C], {notrace(nal_sym_char(C))},!, nal_sym_continue(S), {append(Chars,[C|S],AChars),string_to_atom(AChars,E)},!.

  (= (-->  (nal-word-str  $MaybeArray) (,  (dcg-peek  ($C)) (,  {(char-type  $C alpha) } (,  ! (,  (nal-rsymbol  $E) (,  ! (maybe-plus-array  $E $MaybeArray)))))))  True)

  (= (-->  (maybe-plus-array2  $E (var-holds  $E ())) (,  (mw  (40)) (,  (mw  (41)) !)))  True)
  (= (-->  (maybe-plus-array2  $E (var-holds  $E $Ar)) (,  (40) (,  owhite (,  (nal-term-list-comma  $Ar) (,  owhite (41))))))  True)
  (= (-->  (maybe-plus-array2  $E $E) ())  True)

  (= (-->  (maybe-plus-array  $E $E) (,  (\+  (dcg-peek  (91))) !))  True)
  (= (-->  (maybe-plus-array  $E (idxOf  $E $Ar)) (,  (91) (,  owhite (,  (nal-term-list-comma  $Ar) (,  owhite (93))))))  True)


  (= (-->  (nal-priority  $F) (nal-float-inclusive  0 1 $F))  True)           ;
; 0 <= x <= 1
  (= (-->  (nal-durability  $F) (nal-float-exclusive  0 1 $F))  True)           ;
; 0 < x < 1
   
  (= (-->  (nal-quality  $F) (nal-float-inclusive  0 1 $F))  True)           ;
; 0 <= x <= 1
  (= (-->  (nal-frequency  $F) (nal-float-inclusive  0 1 $F))  True)           ;
; 0 <= x <= 1
  (= (-->  (nal-confidence  $F) (nal-float-exclusive  0 1 $F))  True)           ;
; 0 < x < 1

  (= (-->  (nal-o  $S $X $X) (,  owhite (,  $S owhite)))  True)
  (= (-->  (nal-o  $X $X) (nal-o  $X $X $X))  True)

  (= (-->  (nal-float-inclusive  $L $H $F) (amw  (->  (:  dcg_basics (number  $F)) {(nal-warn-if-strict  (,  (=<  $L $F) (=<  $F $H))) })))  True)
  (= (-->  (nal-float-exclusive  $L $H $F) (amw  (->  (:  dcg_basics (number  $F)) {(nal-warn-if-strict  (,  (<  $L $F) (<  $F $H))) })))  True)


  (= (nal-warn-if-strict $G)  
    (call $G)
    (set-det))
  (= (nal-warn-if-strict $G)  
    (nal-dmsg (nal-warn-if-strict $G))
    (set-det))


  !(set-dcg-meta-reader-options file-comment-reader nal-comment-expr-unused)


  (= (-->  (nal-comment-expr-unused  $_) {(,  ! fail) })  True)

  (= (-->  (nal-comment-expr  $X) (,  chspace (,  ! (nal-comment-expr  $X))))  True)
  (= (-->  (nal-comment-expr  (%COMMENT  $Expr $I $CP)) (,  (nal-comment-expr-3  $Expr $I $CP) !))  True)

  (= (-->  (nal-comment-expr-3  $T $N $CharPOS) (,  (47 42) (,  ! (,  (my-lazy-list-location  (file  $_ $_ $N $CharPOS)) (,  ! (,  (zalwayz  (read-string-until-no-esc  $S (42 47))) (,  ! (,  {(text-to-string-safe  $S $T) } !))))))))  True)
  (= (-->  (nal-comment-expr-3  $T $N $CharPOS) (,  {(nal-cmt-until-eoln  $Text) } (,  (dcg-peek  $Text) (,  ! (,  (my-lazy-list-location  (file  $_ $_ $N $CharPOS)) (,  ! (,  (zalwayz  (read-string-until-no-esc  $S eoln)) (,  ! (,  {(text-to-string-safe  $S $T) } !)))))))))  True)



  (= (nal-cmt-until-eoln  (,  (47 47) (\+  (dcg-peek  (101 120 112 101 99 116 101 100 58)))))  True)
  (= (nal-cmt-until-eoln  (,  (39) (,  (\+  (dcg-peek  (39 111 117 116 112 117 116 77 117 115 116 67 111 110 116 97 105 110))) (dcg-peek  (\+  (39 32 65 110 115 119 101 114 32))))))  True)
  (= (nal-cmt-until-eoln  (42 42))  True)



  (= (-->  nal_comma (amw  (44)))  True)
  (= (-->  nal_l_paren (amw  (40)))  True)
  (= (-->  nal_paren_r (amw  (41)))  True)

  (= (-->  (nal-term-list-white  ([|]  $H $T) $Sep) (,  (nal-term-0  $H) (;  (->  (,  $Sep owhite) (nal-term-list-white  $T $Sep)) (,  {(= $T  ) } owhite))))  True)
  (= (-->  (nal-term-list-comma  ([|]  $H $T)) (,  (nal-term  $H) (;  (->  nal_comma (nal-term-list-comma  $T)) {(= $T  ) })))  True)


  (= (builtin-symbol  _)  True)
  (= (builtin-symbol  --)  True)
  (= (builtin-symbol  ~)  True)
  (= (builtin-symbol  *)  True)
  (= (builtin-symbol  key_101)  True)



  (= (-->  (nal-rsymbol  $S) (,  {(,  (builtin-symbol  $S) (name  $S $Str)) } (,  $Str !)))  True)
  (= (-->  (nal-rsymbol  $E) (nal-rsymbol  () $E))  True)
  (= (-->  (nal-rsymbol  $Chars $E) (,  ($C) (,  {(notrace  (nal-sym-char  $C)) } (,  ! (,  (nal-sym-continue  $S) (,  {(,  (append  $Chars ([|]  $C $S) $AChars) (string-to-symbol  $AChars $E)) } !))))))  True)
  (= (-->  (nal-sym-continue  ()) (,  nal_peek_symbol_breaker !))  True)
  (= (-->  (nal-sym-continue  ([|]  $H $T)) (,  ($H) (,  {(nal-sym-char  $H) } (,  ! (nal-sym-continue  $T)))))  True)
  (= (-->  (nal-sym-continue  ()) ())  True)

  (= (-->  nal_peek_symbol_breaker (dcg-peek  (45 45)))  True)
  (= (-->  nal_peek_symbol_breaker (,  (dcg-peek  (45)) (,  ! {fail })))  True)
  (= (-->  nal_peek_symbol_breaker (dcg-peek  one_blank))  True)
  (= (-->  nal_peek_symbol_breaker (,  (dcg-peek  ($C)) (,  {(\+  (nal-sym-char  $C)) } !)))  True)


  (= (nal-sym-char $C)  
    (not (integer $C))
    (set-det)
    (char-code $C $D)
    (set-det)
    (nal-sym-char $D))
  (= (nal-sym-char $C)  
    (= 
      (:: $C) 
      (:: 95))
    (set-det))
  (= (nal-sym-char $C)  
    ( (bx (=< $C 32)) 
      (set-det) fail))
;
; nal_sym_char(44). ; allow comma in middle of symbol
;
; word is: #"[^\ ]+" ; unicode string
  (= (nal-sym-char $C)  
    ( (nal-never-symbol-char $NeverSymbolList) 
      (memberchk $C $NeverSymbolList) 
      (set-det) fail))  ;
; maybe 44 ? nal_comma
;
; nal_sym_char(C):- nb_current('$maybe_string',t),memberchk(C,`,.:;!;`),!,fail.
  (= (nal-sym-char $_)  
    (set-det))


  (= (nal-never-symbol-char  (34 59 40 41 126 39 91 93 33 60 62 96 123 125 44 61 46 92 94))  True)



  (= (-->  (nal-rsymbol-cont  $Prepend $E) (,  (nal-sym-continue  $S) (,  {(,  (append  $Prepend $S $AChars) (string-to-symbol  $AChars $E)) } !)))  True)



  (= (nal-is-test-file $X)  
    (filematch 
      (library ../nal-tests/**/*) $X)
    (not (nal-non-file $X))) 
  (= (nal-is-test-file $X)  
    (filematch 
      (library ../examples/**/*) $X)
    (symbol-contains $X nars)
    (not (nal-non-file $X))) 

  (= (nal-non-file $X)  
    (downcase-symbol $X $DC)
    (\== $X $DC)
    (set-det)
    (nal-non-file $DC))
  (= (nal-non-file $X)  
    (symbol-contains readme $X))
  (= (nal-non-file $X)  
    (exists-directory $X))
  (= (nal-non-file $X)  
    (symbol-concat $_ .pl $X))


  (= nal-test-files  
    (make (catch (forall (nal-is-test-file $X) (, (nal-dmsg (test-file-begin $X)) (ignore (nal-do-test-file $X)) (nal-dmsg (test-file-end $X)))) %aborted True)))


  (= (nal-do-test-file $File)  
    (or 
      (not (atom $File)) 
      (or 
        (not (is-absolute-file-name $File)) 
        (not (exists-file $File))))
    (filematch $File $Absolute)
    (set-det)
    (nal-do-test-file $Absolute))
  (= (nal-do-test-file $File)  
    (setup-call-cleanup 
      (open $File read $In) 
      (nal-do-test-stream $In) 
      (close $In)))

;
; Whole Group

  (= (nal-do-test-stream $In)  
    (nal-read-clauses $In $Expr)
    (set-det)
    (nars-exec-ex $Expr))
;
; One at a time
;
; nal_do_test_stream(In):- repeat, nal_read_clause(In,Expr), nars_exec_ex(Expr), Expr==end_of_file.


  (= (nal-dmsg $O)  
    (is-list $O)
    (set-det)
    (in-cmt (maplist print-tree-nl $O)))
; /* must_or_rtrace(nal_call(nal_do_test_file,Expr,OutL)),!, flatten([OutL],Out), maplist(nal_dmsg,Out),!. */
  (= (nal-dmsg $O)  
    (format ~N)
    (in-cmt (print-tree-nl $O)))


  (= (-->  (amw  $A) (,  cspace (,  ! (amw  $A))))  True)
  (= (-->  (amw  $A) (,  $A (more-cspace  chspace)))  True)

;
; chspace--> `,`,dcg_peek(chspace),!.
  (= (-->  chspace cspace)  True)
  (= (-->  aspaces (,  chspace (more-aspace  chspace)))  True)
; /* chspace--> `,`,!. chspace--> [C],{char_type(C,white)}. */
  (= (-->  (more-aspace  $P) (,  $P (,  ! (more-aspace  $P))))  True)
  (= (-->  (more-aspace  $_) ())  True)

  (= (-->  (more-cspace  $P) (,  $P (,  ! (more-cspace  $P))))  True)
  (= (-->  (more-cspace  $_) ())  True)

;
; NAL file reader
  (= (-->  (nal-file  $CMT) (,  {(retract  (:  t_l (%last-comment  $CMT))) } !))  True)
  (= (-->  (nal-file  end_of_file) (,  file_eof !))  True)
  (= (-->  (nal-file  $O) (,  eoln (,  ! (nal-file  $O))))  True)
  (= (-->  (nal-file  $O) (,  chspace (,  ! (nal-file  $O))))  True)
  (= (-->  (nal-file  ()) (,  (\+  (dcg-peek  ($_))) !))  True)
;
; nal_file(O) --> nal_file_element(O), !, owhite.
  (= (-->  (nal-file  $O) (,  (read-string-until-no-esc  $Str eoln) {(,  (phrase  (,  (nal-file-element  $O) owhite) $Str) !) }))  True)
;
; fallback to english in a file
  (= (-->  (nal-file  (unk-english  $Text)) (,  (read-string-until-no-esc  $Str eoln) {(,  (symbol-string  $Str $TextStr) (,  {(,  (format  '~N%~~ ') (,  (ansifmt  (red) $TextStr) nl)) } (split-string  $TextStr "" " \t\r\n" $Text))) }))  True)

;
; nal_file(planStepLPG(Name,Expr,Value)) --> owhite,sym_or_num(Name),`:`,owhite, nal(Expr),owhite, `[`,sym_or_num(Value),`]`,owhite. ; 0.0003: (PICK-UP ANDY IBM-R30 CS-LOUNGE) [0.1000]
;
; nal_file(Term,Left,Right):- eoln(EOL),append(LLeft,[46,EOL|Right],Left),read_term_from_codes(LLeft,Term,[double_quotes(string),syntax_errors(fail)]),!.
;
; nal_file(Term,Left,Right):- append(LLeft,[46|Right],Left), ( \+ member(46,Right)),read_term_from_codes(LLeft,Term,[double_quotes(string),syntax_errors(fail)]),!.

;
; non-standard
  (= (-->  (nal-file-element  $O) (,  chspace (,  ! (nal-file-element  $O))))  True)
  (= (-->  (nal-file-element  (expected  $O)) (,  (47 47 101 120 112 101 99 116 101 100 58) (,  (read-string-until  $Str eoln) (,  ! {(phrase  (read-nal-expected  $O) $Str) }))))  True) 
  (= (-->  (nal-file-element  (outputMustContain  $O)) (,  (39 39 111 117 116 112 117 116 77 117 115 116 67 111 110 116 97 105 110 40 39) (,  (amw  (nal-file-element  $O)) (,  (39 41) !))))  True)
;
; nal_file_element(outputMustContain(O)) --> `''outputMustContain('`, !, read_string_until(Str,`')`),`')`,{fmt(Str),phrase(nal_file_element(O),Str,[])}.
  (= (-->  (nal-file-element  (oneAnswer  $O)) (,  (39 32 65 110 115 119 101 114 32) (,  (read-string-until  $Str (,  (123) eoln)) {(phrase  (nal-task  $O) $Str ()) })))  True) 

  (= (-->  (nal-file-element  $Comment) (,  (nal-comment-expr  $Comment) !))  True)

  (= (-->  (nal-file-element  (= $N  $V)) (,  (42) (,  (nal-word  $N) (,  (amw  (61)) (nal-term  $V)))))  True)
  (= (-->  (nal-file-element  (nal-in  $H $V3)) (,  (73 78 58) (,  (nal-task-0  $H) (optional  (nal-three-vals  $V3)))))  True)
  (= (-->  (nal-file-element  (nal-out  $H $V3)) (,  (79 85 84 58) (,  (nal-task-0  $H) (optional  (nal-three-vals  $V3)))))  True)
;
; standard
  (= (-->  (nal-file-element  (do-steps  $N)) (,  (:  dcg_basics (number  $N)) !))  True)
  (= (-->  (nal-file-element  $H) (,  (nal-task  $H) !))  True)
  (= (-->  (nal-file-element  (nal-term  $H)) (nal-term  $H))  True)
;
; nal_read_clause("'the detective claims that tim lives in graz",A)

  (= (-->  ospace (,  chspace (,  ! ospace)))  True)
  (= (-->  ospace ())  True)

  (= (-->  (read-nal-expected  $O) (,  chspace (,  ! (read-nal-expected  $O))))  True)
  (= (-->  (read-nal-expected  ([|]  $H $T)) (,  (read-nal-expected-ele  $H) (read-nal-expected  $T)))  True)
  (= (-->  (read-nal-expected  ()) ())  True)
  (= (-->  (read-nal-expected-ele  $O) (,  chspace (,  ! (read-nal-expected-ele  $O))))  True)
;
; ^say executed with args ({SELF} * bedroom)
  (= (-->  (read-nal-expected-ele  (executed-with-args  $T $A)) (,  (amw  (nal-term  $T)) (,  (101 120 101 99 117 116 101 100 32 119 105 116 104 32 97 114 103 115) (amw  (nal-term  $A)))))  True)
  (= (-->  (read-nal-expected-ele  (answer  $O)) (,  (65 110 115 119 101 114 58) (nal-file-element  $O)))  True)
  (= (-->  (read-nal-expected-ele  (occurrenceTime  $T)) (,  (111 99 99 117 114 114 101 110 99 101 84 105 109 101 61) (,  ospace (:  dcg_basics (number  $T)))))  True)
  (= (-->  (read-nal-expected-ele  (truth  $F $C)) (,  (84 114 117 116 104 58) (read-nal-expected-truth  $F $C)))  True)

  (= (-->  (read-nal-expected-truth  $F $C) (,  chspace (,  ! (read-nal-expected-truth  $F $C))))  True)
  (= (-->  (read-nal-expected-truth  $_ $_) (,  (46) !))  True)
  (= (-->  (read-nal-expected-truth  $_ $_) (,  (\+  (dcg-peek  ($_))) !))  True)
  (= (-->  (read-nal-expected-truth  $F $C) (,  (read-nal-expected-truth-ele  $F $C) (,  ! (read-nal-expected-truth  $F $C))))  True)
  (= (-->  (read-nal-expected-truth-ele  $F $C) (,  (;  chspace (44)) (,  ! (read-nal-expected-truth-ele  $F $C))))  True)
  (= (-->  (read-nal-expected-truth-ele  $F $_) (,  (102 114 101 113 117 101 110 99 121 61) (,  ospace (nal-frequency  $F))))  True)
  (= (-->  (read-nal-expected-truth-ele  $_ $C) (,  (99 111 110 102 105 100 101 110 99 101 61) (,  ospace (nal-confidence  $C))))  True)

;
; {1 : 4;3}
  (= (-->  (nal-three-vals  $V3) (,  (123) (,  (read-string-until-no-esc  $Str (;  (125) eoln)) (,  {(read-term-from-codes  $Str $V3 ((double-quotes  string) (syntax-errors  fail))) } !))))  True)


;
; nal_file_with_comments(O,with_text(O,Txt),S,E):- copy_until_tail(S,Copy),text_to_string_safe(Copy,Txt),!.



  !(thread-local (with-self  (t-l *) (/ sreader-options 2)))


  (= nars-tests  
    (nal-tests nal-test-files))



  (= nal-tests  
    (make 
      (fmt '
NAL TEST') 
      (with-self  
        (nal-reader *) 
        (forall 
          (nal-is-test $_ $Test) 
          (nal-test $Test)))))



  !(use-module (library (/ dcg basics)))

;
; try_reader_test(Test):- is_stream(Test), !, \+ is_compound(Test), open_string(Test,Stream), try_reader_test(Stream).

  (= (nal-test $Test)  
    (in-cmt (, (fmt "\n-----------------------------\n") (fmt $Test) (fmt "-----------------------------\n")))
    (nal-call nal-dmsg $Test $Out)
    (nal-dmsg $Out))



  (= (nal-zave-varname $N $V)  
    (debug-var $N $V)
    (set-det))
;
; nal_zave_varname(N,V):- V = '$VAR'(N).


  (= (nal-read-clauses $Text $Out)  
    (findall $Cl 
      (nal-read-clause $Text $Cl) $OutL)
    (flatten 
      (:: $OutL) $Out))
; /* implode_varnames(Vs):- (var(Vs) ; Vs==[]),!. implode_varnames([NV|Vs]) :- implode_varnames(Vs), (var(NV) -> ignore((nal_variable_name(NV,Name),nal_zave_varname(Name,NV))); ignore((NV=(N=V),nal_zave_varname(N,V)))). */


  (= (nal-read-clause $NonStream $Out)  
    (not (is-stream $NonStream))
    (set-det)
    (must-or-rtrace (, (open-string $NonStream $Stream) (nal-read-clause $Stream $Out))))
; ; nal_dmsg(NonStream),

  (= (nal-read-clause $Stream $Out)  
    (%current-typein-module $M)
    (\== $M input)
    (set-det)
    (setup-call-cleanup 
      (%set-typein-module input) 
      (nal-read-clause $Stream $Out) 
      (%set-typein-module $M)))

  (= (nal-read-clause $Stream $Out)  
    (op 601 xfx 
      (with-self  
        (input *) /))
    (op 601 xfx 
      (with-self  
        (input *) \\))
    (det-if-then-else 
      (at-end-of-stream $Stream) 
      (= $Out Nil) 
      (, 
        (remove-all-atoms  &self 
          (:  t_l 
            (%last-comment  $_))) 
        (nal-read-term $Stream $Term) 
        (det-if-then-else 
          (== $Term end-of-file) 
          (= $Out Nil) 
          (, 
            (det-if-then-else 
              (= $Term 
                !$Exec) 
              (, 
                (with-self  
                  (input *) 
                  (call $Exec)) 
                (= $Out $More)) 
              (= $Out 
                (Cons  $Term $More))) 
            (nal-read-clause $Stream $More))))))


  (= (nal-read-term $In $Expr)  
    (notrace (, (is-stream $In) (remove-pending-buffer-codes $In $Codes) (read-codes-from-pending-input $In $Text) (\== $Text Nil)))
    (set-det)
    (call-cleanup 
      (parse-meta-ascii-nal nal-file $Text $Expr) 
      (append-buffer-codes $In $Codes)))
  (= (nal-read-term $Text $Expr)  
    (notrace (, (= ascii- $In) (remove-pending-buffer-codes $In $Codes)))
    (call-cleanup 
      (parse-meta-ascii-nal nal-file $Text $Expr) 
      (append-buffer-codes $In $Codes)))


  (= (parse-meta-ascii-nal nal-file $Text $Expr)  
    ( (parse-meta-ascii nal-file $Text $Expr) (remove-all-atoms  &self (:  t_l (%last-comment  $_)))))


  (= (with-self  (nal-reader *) {$X })  
    (call $X))

;
; Expand Stream or String

  (= (nal-call $Ctx $Stream $Out)  
    (not (compound $Stream))
    (must-or-rtrace (nal-read-clauses $Stream $List))
    (set-det)
    (nal-call $Ctx $List $Out))

  (= (nal-call $Ctx $List $Out)  
    (is-list $List)
    (set-det)
    (maplist 
      (nal-call $Ctx) $List $OutL)
    (flatten $OutL $Out))
  (= (nal-call $Ctx (= $InnerCtx (json $List)) $Out)  
    (set-det)
    (nal-call 
      (Cons  $InnerCtx $Ctx) $List $Out))

  (= (nal-call $Ctx $List $Out)  
    (sub-term $Sub $List)
    (nonvar $Sub)
    (nal-rule-rewrite $Ctx $Sub $NewSub)
    (nonvar $NewSub)
    (\== $Sub $NewSub)
    (subst $List $Sub $NewSub $NewList)
    (\== $List $NewList)
    (set-det)
    (nal-call $Ctx $NewList $Out))
; ; ignore((NewSub=='$',nal_dmsg(nal_rule_rewrite(_Ctx, Sub, NewSub)))),

  (= (nal-call $Ctx $List $Out)  
    (flatten 
      (:: $List) $Out)
    (set-det))





  (= (nal-rule-rewrite $Ctx (json $Replace) $Replace)  
    (nonvar $Replace)
    (set-det))



  (= (nal-join-symbolics $Sep $List $Joined)  
    (symbolics-to-string $List $Sep $Joined))


  !(fixup-exports *)
; /* nal_into_tokenized(Text,TokenizedText):- \+ string(Text),!, any_to_string(Text,String), nal_into_tokenized(String,TokenizedText). nal_into_tokenized(Text,TokenizedText):- split_string(Text, "", "\s\t\r\n", [L]), L\==Text,!, nal_into_tokenized(L,M), ;string_concat(M,"\n",TokenizedText). string_concat(M,"",TokenizedText). nal_into_tokenized(Text,TokenizedText):- L=[_S1,_S2|_SS], member(Split,["\n'","'\n","<META>'","<META>","\n"]), atomic_list_concat(L,Split,Text), maplist(nal_into_tokenized,L,LO), atomics_to_string(LO,Split, TokenizedText). nal_into_tokenized(Text,TokenizedText):- split_string(Text, "\n", "\s\t\n\r",StringList), maplist(into_text80_atoms,StringList,SentenceList), maplist(nal_join_atomics(' '),SentenceList,ListOfStrings), nal_join_atomics('\n',ListOfStrings,TokenizedText),!. */


  (= (nal-is-test  read "'the detective claims that tim lives in graz")  True)
  (= (nal-is-test  read "<{tim} --> (/,livingIn,_,{graz})>.")  True)
  (= (nal-is-test  read "<bird --> swimmer>. %0.87;0.91%")  True)
  (= (nal-is-test  read "''outputMustContain('<bird --> swimmer>. %0.87;0.91%')")  True)
  (= (nal-is-test  read "1")  True)

  (= (nal-is-test  read "$1")  True)
  (= (nal-is-test  read "#1")  True)
  (= (nal-is-test  read "?1")  True)
  (= (nal-is-test  read "/1")  True)
  (= (nal-is-test  read "\\1")  True)
;
; to distinguish
;
; "eaten by tiger" vs. "eating tiger"
;
; before: (/,eat,tiger,_) vs. (/,eat,_,tiger)
;
; now: (eat /2 tiger) vs. (eat /1 tiger)
  (= (nal-is-test  read "'eating tiger")  True)
  (= (nal-is-test  read "(eat /1 tiger)")  True)
  (= (nal-is-test  read "(/,eat,_,tiger)")  True)
  (= (nal-is-test  read "'eaten by tiger")  True)
  (= (nal-is-test  read "(eat /2 tiger)")  True)
  (= (nal-is-test  read "(/,eat,tiger,_)")  True)
  (= (nal-is-test  read "'intensional eating")  True)
  (= (nal-is-test  read "(eat \\1 tiger)")  True)
  (= (nal-is-test  read "(\\,eat,_,tiger)")  True)
  (= (nal-is-test  read "(eat \\2 tiger)")  True)
  (= (nal-is-test  read "(\\,eat,tiger,_)")  True)


;
; 
  (= (nal-is-test  exec "'Revision ------\n\n'Bird is a type of swimmer.\n<bird --> swimmer>.\n\n'Bird is probably not a type of swimmer.\n<bird --> swimmer>. %0.10;0.60%\n\n1\n\n'Bird is very likely to be a type of swimmer.\n''outputMustContain('<bird --> swimmer>. %0.87;0.91%')")  True)

  (= (nal-is-test  exec "\n********** revision\n  IN: <bird --> swimmer>. %1.00;0.90% {0 : 1} \n  IN: <bird --> swimmer>. %0.10;0.60% {0 : 2} \n1\n OUT: <bird --> swimmer>. %0.87;0.91% {1 : 1;2} \n")  True)


  (= (nal-is-test  exec "\n********** abduction\n  IN: <sport --> competition>. %1.00;0.90% {0 : 1} \n  IN: <chess --> competition>. %0.90;0.90% {0 : 2} \n1\n OUT: <sport --> chess>. %1.00;0.42% {1 : 2;1} \n OUT: <chess --> sport>. %0.90;0.45% {1 : 2;1} \n OUT: <chess <-> sport>. %0.90;0.45% {1 : 2;1} \n OUT: <(&,chess,sport) --> competition>. %1.00;0.81% {1 : 2;1} \n OUT: <(|,chess,sport) --> competition>. %0.90;0.81% {1 : 2;1} \n OUT: <<sport --> $1> ==> <chess --> $1>>. %0.90;0.45% {1 : 2;1} \n OUT: <<chess --> $1> ==> <sport --> $1>>. %1.00;0.42% {1 : 2;1} \n OUT: <<chess --> $1> <=> <sport --> $1>>. %0.90;0.45% {1 : 2;1} \n OUT: (&&,<chess --> #1>,<sport --> #1>). %0.90;0.81% {1 : 2;1} \n")  True)

  (= (nal-is-test  exec "\n********* induction\n  IN: <swan --> swimmer>. %0.90;0.90% {0 : 1} \n  IN: <swan --> bird>. %1.00;0.90% {0 : 2} \n1\n OUT: <bird --> swimmer>. %0.90;0.45% {1 : 2;1} \n OUT: <swimmer --> bird>. %1.00;0.42% {1 : 2;1} \n OUT: <bird <-> swimmer>. %0.90;0.45% {1 : 2;1} \n OUT: <swan --> (|,bird,swimmer)>. %1.00;0.81% {1 : 2;1} \n OUT: <swan --> (&,bird,swimmer)>. %0.90;0.81% {1 : 2;1} \n OUT: <<$1 --> swimmer> ==> <$1 --> bird>>. %1.00;0.42% {1 : 2;1} \n OUT: <<$1 --> bird> ==> <$1 --> swimmer>>. %0.90;0.45% {1 : 2;1} \n OUT: <<$1 --> bird> <=> <$1 --> swimmer>>. %0.90;0.45% {1 : 2;1} \n OUT: (&&,<#1 --> bird>,<#1 --> swimmer>). %0.90;0.81% {1 : 2;1} \n")  True)

  (= (nal-is-test  exec "\n********** exemplification\n  IN: <robin --> bird>. %1.00;0.90% {0 : 1} \n  IN: <bird --> animal>. %1.00;0.90% {0 : 2} \n1\n OUT: <robin --> animal>. %1.00;0.81% {1 : 2;1} \n OUT: <animal --> robin>. %1.00;0.45% {1 : 2;1} \n")  True)

  (= (nal-is-test  exec "\n********** conversion\n  IN: <bird --> swimmer>. %1.00;0.90% {0 : 1} \n  IN: <swimmer --> bird>?  {0 : 2} \n2\n OUT: <swimmer --> bird>. %1.00;0.47% {2 : 1} \n")  True)

  (= (nal-is-test  exec "\n********** y/n question\n  IN: <bird --> swimmer>. %1.00;0.90% {0 : 1} \n  IN: <bird --> swimmer>?  {0 : 2} \n1\n OUT: <bird --> swimmer>. %1.00;0.90% {0 : 1} \n")  True)

  (= (nal-is-test  exec "\n********** wh-question\n  IN: <bird --> swimmer>. %1.00;0.80% {0 : 1} \n  IN: <?1 --> swimmer>?  {0 : 2} \n1\n OUT: <bird --> swimmer>. %1.00;0.80% {0 : 1} \n")  True)

  (= (nal-is-test  exec "\n\n'the detective claims that tim lives in graz\n'<{tim} --> (/,livingIn,_,{graz})>.\n'and lawyer claims that this is not the case\n<{tim} --> (/,livingIn,_,{graz})>. %0%\n100\n'the first deponent, a psychologist,\n'claims that people with sunglasses are more aggressive\n<<(*,$1,sunglasses) --> own> ==> <$1 --> [aggressive]>>.\n'the third deponent claims, that he has seen tom with sunglasses on:\n<(*,{tom},sunglasses) --> own>.\n'the teacher claims, that people who are aggressive tend to be murders\n<<$1 --> [aggressive]> ==> <$1 --> murder>>.\n'the second deponent claims, that if the person lives in Graz, he is surely the murder\n<<$1 --> (/,livingIn,_,{graz})> ==> <$1 --> murder>>.\n'who is the murder?\n<{?who} --> murder>?\n''outputMustContain('<{tom} --> murder>. %1.00;0.73%')\n\n")  True)

  (= (nal-is-test  read "\n' Custom truth values These are added by appending {0.0 0.9} instead of %0.0;0.9% as we believe this increased the readability.\n\n' Example\n\n********** wh-question\n  IN: <bird --> swimmer>. %1.00;0.80% {0 : 1} \n  IN: <?1 --> swimmer>?  {0 : 2} \n1\n OUT: <bird --> swimmer>. %1.00;0.80% {0 : 1} \n\n' can now be\n\n\n********** wh-question\n  IN: <bird --> swimmer>. {1.0 0.80} {0 : 1} \n  IN: <?1 --> swimmer>?  {0 : 2} \n1\n OUT: <bird --> swimmer>. {1.0 0.80} {0 : 1} \n\n")  True)

  (= (nal-is-test  read "\n'Images\n\n(/,rel,_,b) \n\n' has to be written as\n\n(rel /1 b)\n\n' and image as\n\n(/,rel,a,_) \n\n' as\n\n(rel /2 a)\n\n' same for \\ with \\1 and \\2.\n\n")  True)

  (= (nal-is-test  read "\n'Intervals, to measure expected time distances between events, are always learned by ONA and stored as meta-data, they are not part of the Narsese I/O format anymore. Hence a sequence\n\n(&/,a,+5,b)\n\n' becomes\n\n(&/,a,b)\n\n' or\n\n(a &/ b)\n\n' and also the interval for implications is not used anymore.\n\n")  True)

  (= (nal-is-test  read "\n'Operators The syntactic sugar\n\n(^op,arg_1,arg_2,arg_3,arg_n)\n\n' is not supported anymore, instead the full notation has to be used which is supported by both systems:\n\n<(*,arg_1,arg_2,arg_3,arg_n) --> ^op>\n\n' though for operations without arguments, the following shortcut can be used:\n\n^op\n\n")  True)

  (= (nal-is-test  read "\n'Restrictions\n\n'1. Copulas in ONA are binary, since it's using an array-based heap for terms. \n' While there are ways to encode n-ary relations in a binary heap, the ONA philosophy, following KISS, \n' encourages the use of left-nesting, which is also used by the system itself to compose sequences of events:\n\n((a &/ b) &/ c).\n\n")  True)

  (= (nal-is-test  read "\n'2. The parallel temporal copula &| is not implemented, please use &/ for now, again due to KISS. \n' If the order does not matter in some background knowledge we want to give the system, in addition to\n\n<(a &/ b) =/> c>\n\n' also give it\n\n<(b &/ a) =/> c>\n\n' to achieve the same as with &| for now.\n")  True)

  (= (nal-is-test  read "\n'Optional syntactic improvements\n' The ONA parser does not require commas, and doesn't distinguish between < and (, also it supports infix format.\n\n<(|,a,b) --> M>. \n\n' can be written as\n\n<(a | b) --> M>. \n\n' or even as\n\n((a | b) --> M).\n\n' Note: Spaces cannot be omitted.\n")  True)

  (= (nal-is-test  read "\n'Tim is alive.\n\n<{Tim} --> [alive]>.\n\n'Tim is a human.\n\n<{Tim} --> human>.\n")  True)

  (= (nal-is-test  read "\n'Humans are a lifeform.\n\n<human --> lifeform>.\n\n'Lifeforms are like machines.\n\n<lifeform <-> machine>.\n")  True)

  (= (nal-is-test  read "\n'Tom eats chocolate.\n\n<(*,{Tom},chocolate) --> eat>.\n\n<{Tom} --> (/,eat,_,chocolate)>.\n\n<chocolate --> (/,eat,{Tom},_)>.\n")  True)

  (= (nal-is-test  read "\n*volume=0\n*motorbabbling=false\n<corridor --> in>. :|:\n<({SELF} * kitchen) --> ^go>. :|:\n<({cat} * kitchen) --> in>. :|:\n100\n<corridor --> in>. :|:\n<({SELF} * bedroom) --> ^go>. :|:\n<({cat} * bedroom) --> in>. :|:\n100\n<corridor --> in>. :|:\n<({SELF} * livingroom) --> ^go>. :|:\n//no cat this time, it doesn't like the livingroom :)\n100\n<corridor --> in>. :|:\n<({SELF} * bedroom) --> ^go>. :|:\n<({cat} * bedroom) --> in>. :|:\n100\n//Ok you are in corridor now\n<corridor --> in>. :|:\n")  True)

  (= (nal-is-test  read "\n*volume=0\nG! :|:\n")  True)

  (= (nal-is-test  read "//Jonas has asthma?\n<{jonas} --> [asthma]>?\n//expected: Answer: <{jonas} --> [asthma]>. Truth: frequency=1.000000, confidence=0.801900\n//Angelika has asthma?\n<{angelika} --> [asthma]>?\n//expected: Answer: <{angelika} --> [asthma]>. Truth: frequency=1.000000, confidence=0.810000")  True)


  (= (nal-is-test  exec "\n'********** [08 + 09 -> 10]:\n\n'The robot is holding key001. \n<(*,Self,key001) --> hold>. :|: %1.00;0.81% \n\n1\n\n'The robot is holding key001. \n<(*,Self,key001) --> hold>. :|:\n\n5\n\n'The robot is holding key001. \n''outputMustContain('<(*,Self,key001) --> hold>. :!1: %1.00;0.93%') \n")  True)


  (= (nal-is-test  exec "\n'********** compound composition, two premises\n\n'Sport is a type of competition. \n<sport --> competition>. %0.90% \n\n'Chess is a type of competition. \n<chess --> competition>. %0.80%  \n\n16\n\n'If something is either chess or sport, then it is a competition.\n''outputMustContain('<(|,chess,sport) --> competition>. %0.72;0.81%')\n\n'If something is both chess and sport, then it is a competition.\n''outputMustContain('<(&,chess,sport) --> competition>. %0.98;0.81%')\n")  True)


  (= (nal-is-test  exec "\n********** [07 + 09 -> 11]:\n  IN: <(*,key001) --> ^pick>. :|: %1.00;0.90% {0 : 0 : 1} \n\n1\n\n  IN: <(*,Self,key001) --> hold>. :|: %1.00;0.90% {1 : 1 : 2} \n\n1\n\n OUT: <<(*,Self,key001) --> hold> =\\> <(*,key001) --> ^pick>>. :\\: %1.00;0.45% {2 : 1 : 1;2} \n\n OUT: <<(*,key001) --> ^pick> =/> <(*,Self,key001) --> hold>>. :\\: %1.00;0.45% {2 : 1 : 1;2} \n\n OUT: <<(*,key001) --> ^pick> </> <(*,Self,key001) --> hold>>. :\\: %1.00;0.45% {2 : 1 : 1;2} \n\n OUT: <key001 --> (/,^pick,_)>. :\\: %1.00;0.90% {2 : 0 : 1} \n")  True)

  (= (nal-is-test  exec "********** deduction\n  IN: <bird --> animal>. %1.00;0.90% {0 : 1} \n  IN: <robin --> bird>. %1.00;0.90% {0 : 2} \n1\n OUT: <robin --> animal>. %1.00;0.81% {1 : 2;1} \n OUT: <animal --> robin>. %1.00;0.45% {1 : 2;1} ")  True)

  (= (nal-is-test  exec "********** deduction\n'the detective claims that tim lives in graz\n'<{tim} --> (/,livingIn,_,{graz})>.\n'and lawyer claims that this is not the case\n<{tim} --> (/,livingIn,_,{graz})>. %0%\n100\n'the first deponent, a psychologist,\n'claims that people with sunglasses are more aggressive\n<<(*,$1,sunglasses) --> own> ==> <$1 --> [aggressive]>>.\n'the third deponent claims, that he has seen tom with sunglasses on:\n<(*,{tom},sunglasses) --> own>.\n'the teacher claims, that people who are aggressive tend to be murders\n<<$1 --> [aggressive]> ==> <$1 --> murder>>.\n'the second deponent claims, that if the person lives in Graz, he is surely the murder\n<<$1 --> (/,livingIn,_,{graz})> ==> <$1 --> murder>>.\n'who is the murder?\n<{?who} --> murder>?\n''outputMustContain('<{tom} --> murder>. %1.00;0.73%')")  True)

  (= (nal-is-test  read "\n//First: Input diamond:\n// |    â\x96\\x88\â\x96\\x88\    |\n// |  â\x96\\x88\â\x96\\x88\  â\x96\\x88\â\x96\\x88\  |\n// |â\x96\\x88\â\x96\\x88\      â\x96\\x88\â\x96\\x88\|\n// |  â\x96\\x88\â\x96\\x88\  â\x96\\x88\â\x96\\x88\  |\n// |    â\x96\\x88\â\x96\\x88\    |:\n<{M1[-1.0,0.0]} --> [BRIGHT]>.\n<{M1[1.0,0.0]} --> [BRIGHT]>.\n<{M1[0.0,1.0]} --> [BRIGHT]>.\n<{M1[0.0,-1.0]} --> [BRIGHT]>.\n<{M1[0.5,0.5]} --> [BRIGHT]>.\n<{M1[-0.5,0.5]} --> [BRIGHT]>.\n<{M1[0.5,-0.5]} --> [BRIGHT]>.\n<{M1[-0.5,-0.5]} --> [BRIGHT]>.\n<{M1} --> (/,called,_,circle)>.\n\n//Re-observe imperfectly\n// |â\x96\\x92\â\x96\\x92\  â\x96\\x88\â\x96\\x88\    |\n// |  â\x96\\x88\â\x96\\x88\  â\x96\\x92\â\x96\\x92\  |\n// |â\x96\\x92\â\x96\\x92\      â\x96\\x88\â\x96\\x88\|\n// |  â\x96\\x88\â\x96\\x88\  â\x96\\x92\â\x96\\x92\â\x96\\x92\â\x96\\x92\|\n// |          |:\n<{M3[-1.0,1.0]} --> [BRIGHT]>. %0.5%\n<{M3[0.0,1.0]} --> [BRIGHT]>.\n<{M3[-0.5,0.5]} --> [BRIGHT]>.\n<{M3[0.5,0.5]} --> [BRIGHT]>. %0.5%\n<{M3[-1.0,0.0]} --> [BRIGHT]>. %0.5%\n<{M3[1.0,0.0]} --> [BRIGHT]>.\n<{M3[-0.5,-0.5]} --> [BRIGHT]>.\n<{M3[0.5,-0.5]} --> [BRIGHT]>. %0.5%\n<{M3[1.0,-0.5]} --> [BRIGHT]>. %0.5%\n\n50000\n//What was observed?\n<{M3} --> (/,called,_,?what)>?\n//A circle\n''outputMustContain('<{M3} --> (/,called,_,circle)>. %0.83;0.36%')")  True)

  (= (nal-is-test  exec "\n*volume=0\nburgers are tasty food\nthe stranger eats a burger in the pub\n100\nwho eats tasty food?\n//expected: Answer: <stranger --> (eat /1 ([tasty] & food))>. :|: occurrenceTime=2 Truth: frequency=1.000000, confidence=0.278811\nwho is in the pub?\n//expected: Answer: <stranger --> (in /1 pub)>. :|: occurrenceTime=3 Truth: frequency=1.000000, confidence=0.900000\n")  True)

  (= (nal-is-test  exec "\n//NARS, where is the cat?\n//Passive question <({cat} * ?where) --> in>? :|: wouldn't trigger a decision\n//Active question however does:\n<(<({cat} * #where) --> in> &/ <({SELF} * #where) --> ^say>) =/> G>.\nG! :|:\n100\n//expected: ^go executed with args ({SELF} * bedroom)\n")  True)

  (= (nal-is-test  read "\n//ok, feedback of NARS going to the bedroom, the cat is there!\n<({cat} * bedroom) --> in>. :|:\nG! :|:\n10\n//expected: ^say executed with args ({SELF} * bedroom)\n")  True)


  (= (nal-is-test  exec "\n'********** compound composition, two premises\n\n'Sport is a type of competition. \n<sport --> competition>. %0.90% \n\n'Chess is a type of competition. \n<chess --> competition>. %0.80%  \n\n16\n\n'If something is either chess or sport, then it is a competition.\n''outputMustContain('<(|,chess,sport) --> competition>. %0.72;0.81%')\n\n'If something is both chess and sport, then it is a competition.\n''outputMustContain('<(&,chess,sport) --> competition>. %0.98;0.81%')\n")  True)

  (= (nal-is-test  read "<dog --> (/,REPRESENT,_,DOG)>. %1.00;0.90% {0 : 2} ")  True)

  (= (nal-is-test  read "\n'********** induction on events \n\n'John is opening door_101\n<John --> (/,open,_,door_101)>. :|: \n\n6\n\n'John is entering room_101\n<John --> (/,enter,_,room_101)>. :|: \n\n20\n\n'If John enter room_101, he should open door_101 before\n''outputMustContain('<<John --> (/,enter,_,room_101)> =\\> (&/,<John --> (/,open,_,door_101)>,+6)>. :!6: %1.00;0.45%')\n\n'new: variable introduction also in time:\n\n'If someone enter room_101, he should open door_101 before\n''outputMustContain('<<$1 --> (/,enter,_,room_101)> =\\> (&/,<$1 --> (/,open,_,door_101)>,+6)>. :!6: %1.00;0.45%')\n\n'adjusted +2 to +3 in both conditions\n\n10\n")  True)

  (= (nal-is-test  read "\n''outputMustContain('<<John --> (/,enter,_,room_101)> =\\> (&/,<John --> (/,open,_,door_101)>,+6)>. :!6: %1.00;0.45%')")  True)
  (= (nal-is-test  read "\n''outputMustContain('<<$1 --> (/,enter,_,room_101)> =\\> (&/,<$1 --> (/,open,_,door_101)>,+6)>. :!6: %1.00;0.45%')")  True)


  (= (nal-is-test  read "\n<<John --> (/,enter,_,room_101)> =\\> (&/,<John --> (/,open,_,door_101)>,+6)>. :!6: %1.00;0.45%")  True)
  (= (nal-is-test  read "<<$1 --> (/,enter,_,room_101)> =\\> (&/,<$1 --> (/,open,_,door_101)>,+6)>. :!6: %1.00;0.45%")  True)
  (= (nal-is-test  read "****************I am a comment")  True)
  (= (nal-is-test  read "****************")  True)

  (= (nal-is-test  exec "\n*volume=0\n//When a person picks an object in a place, the picked object is also in that place\n<<#Person --> ((pick /1 $Object) & (in /1 $Place))> ==> <$Object --> (in /1 $Place)>>.\nJohn is in the playground.\nBob is in the office.\nJohn picked up the football.\nBob went to the kitchen.\n50\nThe football is in what?\n//expected: Answer: <football --> (in /1 playground)>. :|: occurrenceTime=4 Truth: frequency=1.000000, confidence=0.466560\n")  True)


  (= (nal-is-test  exec "\n********** conditional deduction\n  IN: <(&&,a,b) ==> c>. %1.00;0.90% {0 : 1} \n  IN: a. %1.00;0.90% {0 : 2} \n1\n OUT: <b ==> c>. %1.00;0.81% {1 : 1;2} \n")  True)

  (= (nal-is-test read $X)  
    (symbolic-list-concat $List 
 "IN: (&|,<#1() --> (/,at,Self,_)>,<(*,{t002},#1()) --> on>). :\\:\n IN: (&|,<#1() --> (/,at,Self,_)>,<(*,{t002},#1()) --> on>). :\\: %1.00;0.90% {0 : -1 : 1}\n IN: (&|,<#1() --> (/,on,{t002},_)>,<#1() --> (/,at,Self,_)>). :\\:\n IN: (&|,<#1() --> (/,on,{t002},_)>,<#1() --> (/,at,Self,_)>). :\\: %1.00;0.90% {0 : -1 : 1}\n IN: (&|,<(*,{t002},#1()) --> on>,<(*,Self,#1()) --> at>)!\n IN: (&|,<(*,{t002},#1()) --> on>,<(*,Self,#1()) --> at>)! %1.00;0.90% {0 : 2}\n IN: (&|,<(*,{t002},#1()) --> on>,<(*,Self,#1()) --> at>). :|:\n IN: (&|,<(*,{t002},#1()) --> on>,<(*,Self,#1()) --> at>). :|: %1.00;0.90% {0 : 0 : 1}\n IN: <(&|,<(*,#1,#2(#1)) --> on>,<(*,Self,#2(#1)) --> at>) =|> <(*,Self,#1) --> reachable>>.\n IN: <(&|,<(*,#1,#2(#1)) --> on>,<(*,Self,#2(#1)) --> at>) =|> <(*,Self,#1) --> reachable>>. %1.00;0.90% {0 : 1}\n IN: <(&|,<(*,#1,#2(#1)) --> on>,<(*,Self,#2(#1)) --> at>) =|> <(*,Self,#1) --> reachable>>. %1.00;0.90% {0 : 2}\n IN: <(&|,<(*,#1,#2(#1)) --> on>,<(*,Self,#2(#1)) --> at>) =|> <(*,Self,#1) --> reachable>>. \n IN: <(&|,<(*,#1,#2(#1)) --> on>,<(*,Self,#2(#1)) --> at>) =|> <(*,Self,#1) --> reachable>>.\n IN: <(&|,<(*,#1,#2(#1)) --> on>,<(*,Self,#2(#1)) --> at>)=|><(*,Self,#1) --> reachable>>.\n OUT: (&|,<#1() --> (/,at,Self,_)>,<(*,{t002},#1()) --> on>). :\\: %1.00;0.90%\n OUT: (&|,<#1() --> (/,at,Self,_)>,<(*,{t002},#1()) --> on>). :\\: %1.00;0.90% {8 : -1 : 1}\n OUT: (&|,<#1() --> (/,at,Self,_)>,<{t002} --> (/,on,_,#1())>). :\\: %1.00;0.90% {6 : -1 : 1}\n OUT: (&|,<#1() --> (/,on,{t002},_)>,<#1() --> (/,at,Self,_)>). :\\: %1.00;0.81%\n OUT: (&|,<#1() --> (/,on,{t002},_)>,<#1() --> (/,at,Self,_)>). :\\: %1.00;0.81% {2 : -1 : 2;1}\n OUT: (&|,<(*,Self,#1()) --> at>,<{t002} --> (/,on,_,#1())>)! %1.00;0.90% {6 : 2}\n OUT: (&|,<(*,Self,#1()) --> at>,<{t002} --> (/,on,_,#1())>)? :\\:  {23 : 1 : 3}\n OUT: (&|,<(*,Self,#1()) --> at>,<{t002} --> (/,on,_,#1())>)? :|:  {7 : 7 : 5}\n OUT: (&|,<(*,{t002},#1()) --> on>,<(*,Self,#1()) --> at>)! %1.00;0.81%\n OUT: (&|,<(*,{t002},#1()) --> on>,<(*,Self,#1()) --> at>)! %1.00;0.81% {16 : 2;1}\n OUT: (&|,<(*,{t002},#1()) --> on>,<(*,Self,#1()) --> at>). :\\: %1.00;0.90%\n OUT: (&|,<(*,{t002},#1()) --> on>,<(*,Self,#1()) --> at>). :\\: %1.00;0.90% {4 : -1 : 1}\n OUT: (&|,<(*,{t002},#1()) --> on>,<(*,Self,#1()) --> at>)? :|:  {1 : 1 : 3}\n OUT: (&|,<(*,{t002},#1()) --> on>,<Self --> (/,at,_,#1())>)! %1.00;0.90% {1 : 2}\n OUT: (&|,<(*,{t002},#1()) --> on>,<Self --> (/,at,_,#1())>). :\\: %1.00;0.90% {3 : -1 : 1}\n OUT: (&|,<(*,{t002},#1()) --> on>,<Self --> (/,at,_,#1())>)? :\\:  {15 : 1 : 3}\n OUT: (&|,<(*,{t002},#1()) --> on>,<Self --> (/,at,_,#1())>)? :|:  {2 : 2 : 4}\n OUT: <(&|,<(*,#1,#2(#1)) --> on>,<Self --> (/,at,_,#2(#1))>) ==> <(*,Self,#1) --> reachable>>. %1.00;0.90% {4 : 1}")
    (member $X $List))



  (= (nal-is-test read $X)  
    (symbolic-list-concat $List 
 "\n  ''outputMustContain('')\n  ''outputMustContain('(&/,(^go-to,{t003}),(^pick,{t002}),(^go-to,{t001}),(^open,{t001}))! %1.00;0.43%')\n  ''outputMustContain('(^go-to,{t001})! %1.00;0.81%')\n  ''outputMustContain('(^go-to,{t003})! %1.00;0.81%')\n  ''outputMustContain('...')\n  ''outputMustContain('<(&&,<$1 --> [chirping]>,<$1 --> [with-wings]>) ==> <$1 --> bird>>. %1.00;0.81%')\n  ''outputMustContain('<(&&,<$1 --> [with-wings]>,<(*,$1,worms) --> food>) ==> <$1 --> bird>>. %1.00;0.45%')\n  ''outputMustContain('<(&&,<$1 --> flyer>,<(*,$1,worms) --> food>) ==> <$1 --> [with-wings]>>. %1.00;0.45%')\n  ''outputMustContain('<(&&,<robin --> [chirping]>,<robin --> [with-beak]>) ==> <robin --> bird>>. %1.00;0.42%')\n  ''outputMustContain('<(&&,<robin --> [chirping]>,<robin --> [with-wings]>) ==> <robin --> bird>>. %1.00;0.81%')\n  ''outputMustContain('<(&/,(^go-to,{t003}),(^pick,{t002}),(^go-to,{t001}),(^open,{t001})) =/> <{t001} --> [opened]>>. %1.00;0.43%')\n  ''outputMustContain('<(&/,<(*,Self,{t002}) --> reachable>,(^pick,{t002}),(^go-to,{t001}),(^open,{t001})) =/> <{t001} --> [opened]>>. %1.00;0.81%')\n  ''outputMustContain('<<$1 --> [with-wings]> ==> (&&,<$1 --> flyer>,<(*,$1,worms) --> food>)>. %1.00;0.45%')\n  ''outputMustContain('<<robin --> [with-wings]> ==> <robin --> bird>>. %0.90;0.45%')\n  ''outputMustContain('<<robin --> [with-wings]> ==> <robin --> bird>>. %1.00;0.81%')\n  ''outputMustContain('<<robin --> bird> ==> <robin --> [with-wings]>>. %1.00;0.42%')\n  ''outputMustContain('<bird <-> swan>. %0.10')\n  (&&,<#x --> P >, <#x --> S>) ?\n  (&&,<S1 --> P1>, <S2 --> P2>) .\n  (&&,<S1 --> P1>, <S2 --> P2>)?\n  (--, <David {-- (/, taller_than, {Tom}, _)>).\n  (^go-to,{SELF},{t001}). :\\:\n  < (&&,<(*,#manswer,#m) --> replyTo>, <(*,#manswer,U) --> sender>) <=> <#m --> uResponse> >.\n  < <#x --> P > ==> <M --> (/,R, #x, _ ) > >.\n  <(&&, <#y --> S>, <#x --> P> ) ==> <#y --> (/, R, #x, _ )> > ?\n  <(&&,<$x --> [chirping]>,<$x --> [with-wings]>) ==> <$x --> bird>>.\n  <(&&,<$x --> flyer>,<$x --> [chirping]>, <(*, $x, worms) --> food>) ==> <$x --> bird>>.\n  <(&&,<$y --> [chirping]>,<$y --> [with-wings]>) ==> <$y --> bird>>.\n  <(&&,<robin --> [chirping]>,<robin --> [flying]>,<robin --> [with-wings]>) ==> <robin --> bird>>.\n  <(&&,<robin --> [flying]>,<robin --> [with-wings]>) ==> <robin --> [living]>>. %0.9%\n  <(&&,<robin --> [flying]>,<robin --> [with-wings]>) ==> <robin --> bird>>.\n  <(&&,<robin --> [flying]>,<robin --> [with-wings]>,<robin --> [chirping]>) ==> <robin --> bird>>.\n  <(&&,<robin --> [with-wings]>,<robin --> [chirping]>) ==> <robin --> bird>>.\n  <(&, [red], light) --> traffic_signal>?\n  <(&/, (||, S, P), +5) =/> M>. %0.9%\n  <(&/, a, +1) =/> b>.\n  <(&/, b, +1) =/> c>.\n  <(&/,(^go-to,{t003}),(^pick,{t002}),(^go-to,{t001}),(^open,{t001})) =/> <{t001} --> [opened]>>. :|:\n  <(&/,<(*,Self,{t002}) --> hold>,(^go-to,{t001}),(^open,{t001})) =/> <{t001} --> [opened]>>.\n  <(&/,<(*,Self,{t003}) --> at>,(^pick,{t002}),(^go-to,{t001}),(^open,{t001})) =/> <{t001} --> [opened]>>. :|:\n  <(*, John, key_101) --> hold>. :\\:\n  <(*,S1, S2 ) --> (*,P1, P2 )> ?\n  <(*,S1, S2 ) --> (*,P1, P2 )>.\n  <(*,{t003}) --> ^go-to>. :|:\n  <(^go-to,$1) =/> <(*,Self,$1) --> at>>.\n  <(^go-to,$1)=/><(*,SELF,$1) --> at>>.\n  <(^go-to,{SELF},$1)=/><(*,{SELF},$1) --> at>>.\n  <(|, boy, girl) --> youth>. %0.90%\n  <(~, boy, girl) --> [strong]>. %0.90%\n  <(~,swimmer, swan) --> bird>?\n  <<$y --> [with-wings]> ==> <$y --> flyer>>.\n  <<$y --> flyer> ==> <$y --> [with-wings]>>.\n  <<(*,$1) --> ^go-to> =/> <(*,SELF,$1) --> at>>.\n  <<(*,$1,$2) --> Friends> ==>     (||,    (&&,<$1 --> [Smokes]>,<$2 --> [Smokes]>),    (&&,(--,<$1 --> [Smokes]>),(--,<$2 --> [Smokes]>)))>. %0.6;0.9%\n  <<robin --> [flying]> ==> <robin --> [with-beak]>>. %0.90%\n  <John {-- (/, taller_than, {Tom}, _)>.\n  <Karl {-- (/, taller_than, {Tom}, _)>.\n  <light_1 {-- (&, [red], light)>.\n  <light_2 {-- (&, [red], light)>.\n  <Tom {-- (/, taller_than, _, boy)>?\n  <{Tweety} --> [with-wings]>.\n  [number]: # of cycles to process before continuing to next line\n  IN: (^go-to,{t001})!\n  IN: (^go-to,{t001}). :\\:\n  IN: (^go-to,{t003})! %1.00;0.90% {0 : 1}\n  IN: (^go-to,{t003}). :|:\n  IN: <(&&,<$1 --> [chirping]>,<$1 --> [with-wings]>) ==> <$1 --> bird>>. %1.00;0.90% {0 : 2}\n  IN: <(&&,<robin --> [chirping]>,<robin --> [flying]>,<robin --> [with-wings]>) ==> <robin --> bird>>. %1.00;0.90% {0 : 1}\n  IN: <(&&,<robin --> [chirping]>,<robin --> [flying]>,<robin --> [with-wings]>) ==> <robin --> bird>>. %1.00;0.90% {0 : 2}\n  IN: <(&&,<robin --> [chirping]>,<robin --> [with-wings]>) ==> <robin --> bird>>. %1.00;0.90% {0 : 1}\n  IN: <(&&,<robin --> [flying]>,<robin --> [with-wings]>) ==> <robin --> [living]>>. %0.90;0.90% {0 : 1}\n  IN: <(&&,<robin --> [flying]>,<robin --> [with-wings]>) ==> <robin --> bird>>. %1.00;0.90% {0 : 1}\n  IN: <(&/,(^go-to,{t003}),(^pick,{t002}),(^go-to,{t001}),(^open,{t001})) =/> <{t001} --> [opened]>>. :|:\n  IN: <(&/,<(*,Self,{t002}) --> hold>,(^go-to,{t001}),(^open,{t001})) =/> <{t001} --> [opened]>>.\n  IN: <(&/,<(*,Self,{t002}) --> hold>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) =/> <{t001} --> [opened]>>. %1.00;0.90% {0 : 1}\n  IN: <(&/,<(*,Self,{t002}) --> reachable>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) =/> <{t001} --> [opened]>>. %1.00;0.90% {0 : 1}\n  IN: <(&/,<(*,Self,{t003}) --> at>,(^pick,{t002}),(^go-to,{t001}),(^open,{t001})) =/> <{t001} --> [opened]>>. :|:\n  IN: <(&/,<(*,Self,{t003}) --> at>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) =/> <{t001} --> [opened]>>. :|: %1.00;0.90% {0 : 0 : 1}\n  IN: <(&/,<(*,{t003}) --> ^go-to>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) =/> <{t001} --> [opened]>>. :|: %1.00;0.90% {0 : 0 : 2}\n  IN: <(*, John, key_101) --> hold>. :/:\n  IN: <(*,{t001}) --> ^go-to>! %1.00;0.90% {0 : 1}\n  IN: <(*,{t001}) --> ^go-to>. :\\: %1.00;0.90% {0 : -1 : 1}\n  IN: <(*,{t003}) --> ^go-to>! %1.00;0.90% {0 : 1}\n  IN: <(*,{t003}) --> ^go-to>. :|: %1.00;0.90% {0 : 0 : 1}\n  IN: <(^go-to,#1) =/> <(*,Self,#1) --> at>>.\n  IN: <(^go-to,#1)=/><(*,Self,#1) --> at>>.\n  IN: <<$1 --> [with-wings]> ==> <$1 --> flyer>>. %1.00;0.90% {0 : 2}\n  IN: <<$1 --> flyer> ==> <$1 --> [with-wings]>>. %1.00;0.90% {0 : 2}\n  IN: <<(*,#1) --> ^go-to> =/> <(*,Self,#1) --> at>>. %1.00;0.90% {0 : 1}\n  IN: <<(*,#1) --> ^go-to> =/> <(*,Self,#1) --> at>>. %1.00;0.90% {0 : 2}\n  IN: <<robin --> [flying]> ==> <robin --> [with-beak]>>. %0.90;0.90% {0 : 2}\n  IN: <{Tweety} --> [with-wings]>. %1.00;0.90% {0 : 1}\n  out is a reference to the current output buffer, containing a list of strings; one for each output\n  OUT: (&/,(^go-to,{t003}),(^pick,{t002}),(^go-to,{t001}),(^open,{t001}))! %1.00;0.81%\n  OUT: (&/,<(*,{t003}) --> ^go-to>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>)! %1.00;0.81% {16 : 2;1}\n  OUT: (&/,<(*,{t003}) --> ^go-to>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>)? :\\:  {13 : 0 : 2;3}\n  OUT: (&/,<(*,{t003}) --> ^go-to>,<(*,{t002}) --> ^pick>,<{t001} --> (/,^go-to,_)>,<(*,{t001}) --> ^open>)? :\\:  {14 : 0 : 2;3}\n  OUT: (^go-to,{t001})! %1.00;0.81%\n  OUT: (^go-to,{t001}). :|: %1.00;0.90%\n  OUT: (^go-to,{t003})! %1.00;0.81%\n  OUT: (^go-to,{t003}). :|: %1.00;0.90%\n  OUT: <(&&,<$1 --> [chirping]>,<$1 --> [with-wings]>) ==> <$1 --> bird>>. %1.00;0.81%\n  OUT: <(&&,<$1 --> [chirping]>,<$1 --> [with-wings]>) ==> <$1 --> bird>>. %1.00;0.81% {1 : 2;1}\n  OUT: <(&&,<$1 --> [with-wings]>,<(*,$1,worms) --> food>) ==> <$1 --> bird>>. %1.00;0.45%\n  OUT: <(&&,<$1 --> [with-wings]>,<(*,$1,worms) --> food>) ==> <$1 --> bird>>. %1.00;0.45% {4 : 1;2}\n  OUT: <(&&,<$1 --> flyer>,<(*,$1,worms) --> food>) ==> <$1 --> [with-wings]>>. %1.00;0.45%\n  OUT: <(&&,<$1 --> flyer>,<(*,$1,worms) --> food>) ==> <$1 --> [with-wings]>>. %1.00;0.45% {4 : 1;2}\n  OUT: <(&&,<robin --> [chirping]>,<robin --> [with-beak]>) ==> <robin --> bird>>. %1.00;0.42%\n  OUT: <(&&,<robin --> [chirping]>,<robin --> [with-beak]>) ==> <robin --> bird>>. %1.00;0.42% {11 : 1;2}\n  OUT: <(&&,<robin --> [chirping]>,<robin --> [with-wings]>) ==> <robin --> bird>>. %1.00;0.81%\n  OUT: <(&&,<robin --> [chirping]>,<robin --> [with-wings]>) ==> <robin --> bird>>. %1.00;0.81% {5 : 1;2}\n  OUT: <(&/,(^go-to,{t003}),(^pick,{t002}),(^go-to,{t001}),(^open,{t001})) =/> <{t001} --> [opened]>>. :\\: %1.00;0.81%\n  OUT: <(&/,<(*,Self,{t002}) --> hold>,(^go-to,{t001}),(^open,{t001})) =/> <{t001} --> [opened]>>. %1.00;0.81%\n  OUT: <(&/,<(*,Self,{t002}) --> hold>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) =/> <{t001} --> [opened]>>. %1.00;0.81% {2 : 2;1}\n  OUT: <(&/,<(*,Self,{t002}) --> reachable>,(^pick,{t002}),(^go-to,{t001}),(^open,{t001})) =/> <{t001} --> [opened]>>. %1.00;0.81%\n  OUT: <(&/,<(*,Self,{t002}) --> reachable>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) =/> <{t001} --> [opened]>>. %1.00;0.81% {14 : 2;1}\n  OUT: <(&/,<(*,Self,{t003}) --> at>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) =/> <{t001} --> [opened]>>. %1.00;0.43% {5 : 2;1}\n  OUT: <(&/,<(*,Self,{t003}) --> at>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) =/> <{t001} --> [opened]>>. %1.00;0.43% {8 : 2;1}\n  OUT: <(&/,<(*,Self,{t003}) --> at>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) =/> <{t001} --> [opened]>>. :\\: %1.00;0.81% {17 : 0 : 1;2}\n  OUT: <(&/,<(*,{t003}) --> ^go-to>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) =/> <{t001} --> [opened]>>. %1.00;0.43% {11 : 1;2}\n  OUT: <(&/,<(*,{t003}) --> ^go-to>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) =/> <{t001} --> [opened]>>. :\\: %1.00;0.81% {16 : 0 : 2;1}\n  OUT: <(&/,<Self --> (/,at,_,{t003})>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) ==> <{t001} --> [opened]>>. :\\: %1.00;0.90% {15 : 0 : 1}\n  OUT: <(&/,<Self --> (/,hold,_,{t002})>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) ==> <{t001} --> [opened]>>. %1.00;0.90% {10 : 1}\n  OUT: <(&/,<Self --> (/,reachable,_,{t002})>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) ==> <{t001} --> [opened]>>. %1.00;0.90% {16 : 1}\n  OUT: <(&/,<{t002} --> (/,hold,Self,_)>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) ==> <{t001} --> [opened]>>. %1.00;0.90% {11 : 1}\n  OUT: <(&/,<{t002} --> (/,reachable,Self,_)>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) ==> <{t001} --> [opened]>>. %1.00;0.90% {9 : 1}\n  OUT: <(&/,<{t003} --> (/,^go-to,_)>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) ==> <{t001} --> [opened]>>. :\\: %1.00;0.90% {11 : 0 : 2}\n  OUT: <(&/,<{t003} --> (/,at,Self,_)>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) ==> <{t001} --> [opened]>>. :\\: %1.00;0.90% {7 : 0 : 1}\n  OUT: <(*,{t001}) --> ^go-to>! %1.00;0.81% {19 : 2;1}\n  OUT: <(*,{t001}) --> ^go-to>. :|: %1.00;0.90% {1 : 1 : 2}\n  OUT: <(*,{t003}) --> ^go-to>! %1.00;0.81% {19 : 2;1}\n  OUT: <(*,{t003}) --> ^go-to>. :|: %1.00;0.90% {1 : 1 : 2}\n  OUT: <(/,(*,{t001}),_) --> (/,^go-to,_)>. :\\: %1.00;0.90% {7 : -1 : 1}\n  OUT: <(/,(*,{t003}),_) --> (/,^go-to,_)>. :\\: %1.00;0.90% {7 : 0 : 1}\n  OUT: <(~,swimmer, swan) --> bird>. %0.10;0.73%\n  OUT: <<$1 --> [with-wings]> ==> (&&,<$1 --> flyer>,<(*,$1,worms) --> food>)>. %1.00;0.45%\n  OUT: <<$1 --> [with-wings]> ==> (&&,<$1 --> flyer>,<(*,$1,worms) --> food>)>. %1.00;0.45% {4 : 1;2}\n  OUT: <<(*,#1) --> ^go-to> =/> <Self --> (/,at,_,#1)>>. %1.00;0.90% {1 : 1}\n  OUT: <<(*,#1) --> ^go-to> =/> <Self --> (/,at,_,#1)>>. %1.00;0.90% {1 : 2}\n  OUT: <<(*,#1) --> ^go-to> =/> <Self --> (/,at,_,#1)>>. %1.00;0.90% {10 : 2}\n  OUT: <<(*,#1) --> ^go-to> =/> <Self --> (/,at,_,#1)>>. %1.00;0.90% {8 : 2}\n  OUT: <<robin --> [with-wings]> ==> <robin --> bird>>. %0.90;0.45%\n  OUT: <<robin --> [with-wings]> ==> <robin --> bird>>. %0.90;0.45% {8 : 2;1}\n  OUT: <<robin --> [with-wings]> ==> <robin --> bird>>. %1.00;0.81%\n  OUT: <<robin --> [with-wings]> ==> <robin --> bird>>. %1.00;0.81% {3 : 2;1}\n  OUT: <<robin --> bird> ==> <robin --> [with-wings]>>. %1.00;0.42%\n  OUT: <<robin --> bird> ==> <robin --> [with-wings]>>. %1.00;0.42% {8 : 2;1}\n  OUT: <{t001} --> (/,^go-to,_)>. :\\: %1.00;0.90% {2 : -1 : 1}\n  OUT: <{t003} --> (/,^go-to,_)>. :\\: %1.00;0.90% {2 : 0 : 1}\n  IN: <(*, John, key_101) --> hold>. :/:\n<John {-- (/, taller_than, {Tom}, _)>.\n<Tom {-- (/, taller_than, _, boy)>?\n(--, <David {-- (/, taller_than, {Tom}, _)>).\n<Karl {-- (/, taller_than, {Tom}, _)>.\n<(~,swimmer, swan) --> bird>?\nOUT: <(~,swimmer, swan) --> bird>. %0.10;0.73%\n<(|, boy, girl) --> youth>. %0.90%\n<(~, boy, girl) --> [strong]>. %0.90%\n<(&&,<$x --> flyer>,<$x --> [chirping]>, <(*, $x, worms) --> food>) ==> <$x --> bird>>.\nIN: <(*, John, key_101) --> hold>. :/:\n\n''outputMustContain('<bird <-> swan>. %0.10')\n\nnars hears a boom")
    (member $X $List)
    (\== $X "")
    (\== $X ''))


  (= (nal-is-test  exec "'********** conversions between inheritance and similarity\n\n'Swan is a type of bird. \n<swan --> bird>. \n\n'Bird is not a type of swan. \n<bird --> swan>. %0.10% \n\n1\n\n'Bird is different from swan.  \n''outputMustContain('<bird <-> swan>.')\n''outputMustContain('<bird <-> swan>. %0.10')\n''outputMustContain('<bird <-> swan>. %0.10;0.81%')\n\n")  True)

  (= (nal-is-test  read "\n<{a, b} |- (&/,a,b)>.")  True)

  (= (nal-is-test  read "\n<{a, b, after(a,b)} |- <a =/> b>>.")  True)

  (= (nal-is-test  read "\n<{ b! , <a =/> b>} |- a! >")  True)


;
; {Event a., Implication <a =/> b>.} |- Event b.

