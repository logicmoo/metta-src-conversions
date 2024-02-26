;                                           (convert-to-metta-file  asm $_167076 sxx_machine/bench/asm.pl sxx_machine/bench/asm.metta)
;
; File : asm.pl
;
; Author : Neng-Fa ZHOU
;
; Completed October 1993
;
; Updated: Febuary 1994
;
; Purpose: Assembler of NTOAM

  !(op 300 xfx <=)
; /* asm.pl this file translates instructions and symbols into byte code */
  !(op 1000 fx mode)
  !(op 950 xfx :)

  !(determinate (:: (/ cmp-error 1) (/ name 2) (/ length 2) (/ asm-hash-value 2)))


  (= (asm-bp $Infile $Outfile)  
    ( (with-self  True 
        (global-set %asm-bp 0 1)) 
      (see $Infile) 
      (asm-getaslist $Insts) seen 
      (tell $Outfile) 
      (asm0 $Insts $Prog $Index 0 $NIndex) 
      (functor $Psctable csym 255) 
      (functor $Labeltable lsym 255) 
      (global-create %sym-no 0 0) 
      (asm-pass1-bp $Prog $Index $Psctable $Labeltable $Ntext $Npsc) 
      (asm-magic 3) 
      (asm-putnum $Npsc 4) 
      (asm-putnum $Ntext 4) 
      (asm-putnum $NIndex 4) 
      (asm-pass2 $Prog $Index $Psctable $Labeltable) asm-mark-eot 
      (global-del %asm-bp 0) told))
; ; output_mess('==>asm_pass1'),
; ; output_mess('==>asm_pass2'),


  (= (asm $Infile $Outfile)  
    ( (with-self  True 
        (see $Infile)) 
      (asm-getaslist $Insts) seen 
      (tell $Outfile) 
      (asm0 $Insts $Prog $Index 0 $NIndex) 
      (functor $Psctable csym 255) 
      (functor $Labeltable lsym 255) 
      (global-create %sym-no 0 0) 
      (asm-pass1 $Prog $Index $Psctable $Labeltable $Ntext $Npsc) 
      (asm-magic 3) 
      (asm-putnum $Npsc 4) 
      (asm-putnum $Ntext 4) 
      (asm-putnum $NIndex 4) 
      (asm-pass2 $Prog $Index $Psctable $Labeltable) asm-mark-eot told))
; ; output_mess('==>asm_pass1'),
; ; output_mess('==>asm_pass2'),


  (= (asm-getaslist $Insts)  
    ( (with-self  True 
        (read $Inst)) (asm-getaslist1 $Inst $Insts)))


  (= (asm-getaslist1 end-of-file $Insts)  
    (with-self  True 
      (:= $Insts Nil)))
  (= (asm-getaslist1 $Inst $Insts)  
    ( (with-self  True 
        (:= $Insts 
          (Cons  $Inst $Insts1))) 
      (read $Inst1) 
      (asm-getaslist1 $Inst1 $Insts1)))


  (= (asm $Insts)  
    ( (with-self  True 
        (asm0 $Insts $Prog $Index 0 $NIndex)) 
      (functor $Psctable csym 255) 
      (functor $Labeltable lsym 255) 
      (global-create %sym-no 0 0) 
      (asm-pass1 $Prog $Index $Psctable $Labeltable $Ntext $Npsc) 
      (asm-magic 3) 
      (asm-putnum $Npsc 4) 
      (asm-putnum $Ntext 4) 
      (asm-putnum $NIndex 4) 
      (asm-pass2 $Prog $Index $Psctable $Labeltable) asm-mark-eot))
; ; output_mess('==>asm_pass1'),
; ; output_mess('==>asm_pass2'),


  (= (asm0 Nil $Prog $Index $NIndex0 $NIndex)  
    ( (with-self  True 
        (:= $Prog Nil)) 
      (:= $Index Nil) 
      (:= $NIndex $NIndex0)))
  (= (asm0 (Cons  $Inst $Insts) $Prog $Index $NIndex0 $NIndex)  
    ( (with-self  
        (asm-index-inst $Inst $Size) 
        (is $NIndex1 
          (+ $NIndex0 $Size))) 
      (= $Index 
        (Cons  $Inst $Index1)) 
      (asm0 $Insts $Prog $Index1 $NIndex1 $NIndex)))
  (= (asm0 (Cons  $Inst $Insts) $Prog $Index $NIndex0 $NIndex)  
    ( (with-self  True 
        (:= $Prog 
          (Cons  $Inst $Prog1))) (asm0 $Insts $Prog1 $Index $NIndex0 $NIndex)))


  (= (asm-pass1 $AsmInsts $Index $Csym $Lsym $Ntext $Npsc)  
    ( (with-self  True 
        (asm-pass11 $AsmInsts $Lsym $Csym 0 $Ntext)) 
      (asm-index-pass1 $Index $Csym) 
      (asmpass1-fillin $Lsym 255 $Csym) 
      (asmpass1-setundef $Csym 255 0 $Npsc)))


  (= (asm-pass1-bp $AsmInsts $Index $Csym $Lsym $Ntext $Npsc)  
    ( (with-self  True 
        (asm-pass11 $AsmInsts $Lsym $Csym 0 $Ntext)) 
      (asm-index-pass1 $Index $Csym) 
      (asmpass1-fillin-bp $Lsym 255 $Csym) 
      (asmpass1-setundef $Csym 255 0 $Npsc)))


  (= (asm-pass11 Nil $_ $_ $Lc $NLc)  
    (with-self  True 
      (:= $NLc $Lc)))
  (= (asm-pass11 (Cons  $Inst $Rest) $Lsym $Csym $Lc $NLc)  
    ( (<= 
        (label $X) $Inst) (with-self  (lab-member1 (lab $X $Lc) $Lsym) (asm-pass11 $Rest $Lsym $Csym $Lc $NLc))))
  (= (asm-pass11 (Cons  $Inst $Rest) $Lsym $Csym $Lc $NLc)  
    ( (with-self  
        (<= 
          (label $X) $Inst) 
        (error-double-define $X)) (asm-pass11 $Rest $Lsym $Csym $Lc $NLc)))
  (= (asm-pass11 (Cons  $Inst $Rest) $Lsym $Csym $Lc $NLc)  
    ( (with-self  True 
        (asm-pass12 $Inst $Csym $N)) 
      (is $Lc0 
        (+ $Lc $N)) 
      (asm-pass11 $Rest $Lsym $Csym $Lc0 $NLc)))


  (= (error-double-define (, $Pred $Arity $_))  
    (with-self  True True))
  (= (error-double-define (, $Pred $Arity))  
    (with-self  True 
      (cmp-error (:: 'The predicate ' (/ $Pred $Arity) ' is doubly defined'))))


  (= (asm-index-pass1 Nil $_)  
    (with-self  True True))
  (= (asm-index-pass1 (Cons  (pred $_ $_ $_ $_) $Rest) $Csym)  
    (with-self  True 
      (asm-index-pass1 $Rest $Csym)))
  (= (asm-index-pass1 (Cons  (arglabel $T $Val $Label) $Rest) $Csym)  
    ( (with-self  
        (== $T c) 
        (sym-member1 
          (sym $Val 0 $_ $_) $Csym)) (asm-index-pass1 $Rest $Csym)))
  (= (asm-index-pass1 (Cons  (arglabel $T (, $Str $Ar) $Label) $Rest) $Csym)  
    ( (with-self  
        (== $T s) 
        (sym-member1 
          (sym $Str $Ar $_ $_) $Csym)) (asm-index-pass1 $Rest $Csym)))
  (= (asm-index-pass1 (Cons  (arglabel $T $Val $Label) $Rest) $Csym)  
    (with-self  True 
      (asm-index-pass1 $Rest $Csym)))


  (= (asmpass1-fillin $Lsym $N $_)  
    (with-self  
      (< $N 1) True))
; /* Fill in the values of any predicates which are defined within this module.*/
  (= (asmpass1-fillin $Lsym $N $Csym)  
    ( (with-self  True 
        (arg $N $Lsym $L)) 
      (asmpass1-fillin $L $Csym) 
      (is $N1 
        (- $N 1)) 
      (asmpass1-fillin $Lsym $N1 $Csym)))
    
  (= (asmpass1-fillin $L $_)  
    (with-self  
      (var $L) True))
  (= (asmpass1-fillin (Cons  (lab (, $_ $_ $_) $LcValue) $Rest) $Table)  
    (with-self  True 
      (asmpass1-fillin $Rest $Table)))
  (= (asmpass1-fillin (Cons  (lab (, $Name $Arity) $LcValue) $Rest) $Table)  
    ( (with-self  True 
        (sym-member1 
          (sym $Name $Arity $LcValue $_) $Table)) (asmpass1-fillin $Rest $Table)))



  (= (asmpass1-fillin-bp $Lsym $N $_)  
    (with-self  
      (< $N 1) True))
  (= (asmpass1-fillin-bp $Lsym $N $Csym)  
    ( (with-self  True 
        (arg $N $Lsym $L)) 
      (asmpass1-fillin-bp $L $Csym) 
      (is $N1 
        (- $N 1)) 
      (asmpass1-fillin-bp $Lsym $N1 $Csym)))
    
  (= (asmpass1-fillin-bp $L $_)  
    (with-self  
      (var $L) True))
  (= (asmpass1-fillin-bp (Cons  (lab (, $_ $_ $_) $LcValue) $Rest) $Table)  
    (with-self  True 
      (asmpass1-fillin-bp $Rest $Table)))
  (= (asmpass1-fillin-bp (Cons  (lab (, $Name $Arity) $LcValue) $Rest) $Table)  
    ( (with-self  
        (predefined $Name $Arity) 
        (sym-member1 
          (sym $Name $Arity $LcValue $_) $Table)) (asmpass1-fillin-bp $Rest $Table)))
  (= (asmpass1-fillin-bp (Cons  (lab (, $Name $Arity) $LcValue) $Rest) $Table)  
    (with-self  True 
      (asmpass1-fillin-bp $Rest $Table)))


  (= (asmpass1-setundef $Csym $N $S0 $S)  
    (with-self  
      (< $N 1) 
      (= $S $S0)))
; /* asmpass1_fillin([lab((Name, Arity),LcValue)|Rest],Table) :- true : cmp_error(['the predicate ', Name/Arity, ' is doubly defined']), asmpass1_fillin(Rest, Table). */
; /* Fill in the values of any symbols which have not been defined with the value -2. */
  (= (asmpass1-setundef $Csym $N $S0 $S)  
    ( (with-self  True 
        (arg $N $Csym $L)) 
      (asmpass1-setundef $L $S0 $S1) 
      (is $N1 
        (- $N 1)) 
      (asmpass1-setundef $Csym $N1 $S1 $S)))

  (= (asmpass1-setundef $Tab $S0 $S)  
    (with-self  
      (var $Tab) 
      (= $S $S0)))
  (= (asmpass1-setundef (Cons  (sym $Pred $Arity $Val $_) $Rest) $S0 $S)  
    ( (with-self  
        (var $Val) 
        (:= $Val -2)) 
      (b-GET-LENGTH-cf $Pred $L) 
      (is $S1 
        (+ 
          (+ $S0 $L) 6)) 
      (asmpass1-setundef $Rest $S1 $S)))
  (= (asmpass1-setundef (Cons  (sym $Pred $Arity $Val $_) $Rest) $S0 $S)  
    ( (with-self  True 
        (b-GET-LENGTH-cf $Pred $L)) 
      (is $S1 
        (+ 
          (+ $S0 $L) 6)) 
      (asmpass1-setundef $Rest $S1 $S)))


  !(mode (asm-pass12 c d f))

  (= (asm-pass12  (label  $_) $_ 0)  True)

  (= (asm-pass12  (jmp  $_) $_ 2)  True)
; /* Conditional Jump */
  (= (asm-pass12 (jmpn-eq-struct-x $_ (, $S $N) $_ $_) $Csym 5)  
    (sym-member1 
      (sym $S $N $_ $_) $Csym))
  (= (asm-pass12 (jmpn-eq-struct-y $_ (, $S $N) $_ $_) $Csym 5)  
    (sym-member1 
      (sym $S $N $_ $_) $Csym))
  (= (asm-pass12 (jmpn-eq-symbol-x $_ $C $_ $_) $Csym 5)  
    (sym-member1 
      (sym $C 0 $_ $_) $Csym))
  (= (asm-pass12 (jmpn-eq-symbol-y $_ $C $_ $_) $Csym 5)  
    (sym-member1 
      (sym $C 0 $_ $_) $Csym))
  (= (asm-pass12  (jmpn-nil-x  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (jmpn-nil-y  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (switch-list-x  $_ $_ $_ $_) $_ 5)  True)
  (= (asm-pass12  (switch-list-y  $_ $_ $_ $_) $_ 5)  True)
  (= (asm-pass12  (switch-list-yxx  $_ $_ $_ $_) $_ 5)  True)
  (= (asm-pass12  (switch-list-yxy  $_ $_ $_ $_) $_ 5)  True)
  (= (asm-pass12  (switch-list-yyx  $_ $_ $_ $_) $_ 5)  True)
  (= (asm-pass12  (switch-list-yyy  $_ $_ $_ $_) $_ 5)  True)
  (= (asm-pass12  (jmpn-eq-int-x  $_ $_ $_ $_) $_ 5)  True)
  (= (asm-pass12  (jmpn-eq-int-y  $_ $_ $_ $_) $_ 5)  True)
  (= (asm-pass12  (jmpn-eql  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (jmp-eql  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (jmp-eql-yy  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (jmpn-gt  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (jmpn-gt-yy  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (jmpn-ge  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (jmpn-ge-yy  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (jmpn-id  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (jmp-id  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (jmpn-var-x  $_ $_) $_ 3)  True)
  (= (asm-pass12  (jmpn-var-y  $_ $_) $_ 3)  True)
  (= (asm-pass12  (jmp-var-x  $_ $_) $_ 3)  True)
  (= (asm-pass12  (jmp-var-y  $_ $_) $_ 3)  True)
  (= (asm-pass12  (jmpn-symbol-x  $_ $_) $_ 3)  True)
  (= (asm-pass12  (jmpn-symbol-y  $_ $_) $_ 3)  True)
  (= (asm-pass12  (jmpn-symbolic-x  $_ $_) $_ 3)  True)
  (= (asm-pass12  (jmpn-symbolic-y  $_ $_) $_ 3)  True)
  (= (asm-pass12  (jmpn-num-x  $_ $_) $_ 3)  True)
  (= (asm-pass12  (jmpn-num-y  $_ $_) $_ 3)  True)
  (= (asm-pass12  (jmpn-int-x  $_ $_) $_ 3)  True)
  (= (asm-pass12  (jmpn-int-y  $_ $_) $_ 3)  True)
  (= (asm-pass12  (jmpn-float-x  $_ $_) $_ 3)  True)
  (= (asm-pass12  (jmpn-float-y  $_ $_) $_ 3)  True)
  (= (asm-pass12  (hash-jmpn-nil  $_) $_ 2)  True)
  (= (asm-pass12  (hash-jmpn-list  $_) $_ 2)  True)
  (= (asm-pass12  (hash-jmpn-int  $_ $_) $_ 3)  True)
  (= (asm-pass12 (hash-jmpn-symbol $C $_) $Csym 3)  
    (sym-member1 
      (sym $C 0 $_ $_) $Csym))
  (= (asm-pass12 (hash-jmpn-struct (, $S $N) $_) $Csym 3)  
    (sym-member1 
      (sym $S $N $_ $_) $Csym))
  (= (asm-pass12 (hash-jmpn-struct-x (, $S $N) $_) $Csym 3)  
    (sym-member1 
      (sym $S $N $_ $_) $Csym))
  (= (asm-pass12 (hash-jmpn-struct-y (, $S $N) $_) $Csym 3)  
    (sym-member1 
      (sym $S $N $_ $_) $Csym))
  (= (asm-pass12 (hash-jmpn-struct-xx (, $S $N) $_) $Csym 3)  
    (sym-member1 
      (sym $S $N $_ $_) $Csym))
  (= (asm-pass12 (hash-jmpn-struct-xy (, $S $N) $_) $Csym 3)  
    (sym-member1 
      (sym $S $N $_ $_) $Csym))
  (= (asm-pass12 (hash-jmpn-struct-yx (, $S $N) $_) $Csym 3)  
    (sym-member1 
      (sym $S $N $_ $_) $Csym))
  (= (asm-pass12 (hash-jmpn-struct-yy (, $S $N) $_) $Csym 3)  
    (sym-member1 
      (sym $S $N $_ $_) $Csym))

  (= (asm-pass12 (unify-struct-x $_ (, $S $N)) $Csym 3)  
    (sym-member1 
      (sym $S $N $_ $_) $Csym))
; /* Unify */
  (= (asm-pass12 (unify-struct-y $_ (, $S $N)) $Csym 3)  
    (sym-member1 
      (sym $S $N $_ $_) $Csym))
  (= (asm-pass12  (unify-list-x  $_) $_ 2)  True)
  (= (asm-pass12  (unify-list-y  $_) $_ 2)  True)
  (= (asm-pass12  (unify-nil-x  $_) $_ 2)  True)
  (= (asm-pass12  (unify-nil-y  $_) $_ 2)  True)
  (= (asm-pass12 (unify-symbol-x $_ $C) $Csym 3)  
    (sym-member1 
      (sym $C 0 $_ $_) $Csym))
  (= (asm-pass12 (unify-symbol-y $_ $C) $Csym 3)  
    (sym-member1 
      (sym $C 0 $_ $_) $Csym))
  (= (asm-pass12  (unify-int-x  $_ $_) $_ 3)  True)
  (= (asm-pass12  (unify-int-y  $_ $_) $_ 3)  True)
  (= (asm-pass12  (unify-ux-ux  $_ $_) $_ 3)  True)
  (= (asm-pass12  (unify-ux-uy  $_ $_) $_ 3)  True)
  (= (asm-pass12  (unify-uy-uy  $_ $_) $_ 3)  True)
  (= (asm-pass12  (unify-cons-x  $_ $_) $_ 3)  True)
  (= (asm-pass12  (unify-cons-y  $_ $_) $_ 3)  True)

  (= (asm-pass12 (fork-unify-struct-y $_ (, $S $N) $_) $Csym 4)  
    (sym-member1 
      (sym $S $N $_ $_) $Csym))
  (= (asm-pass12  (fork-unify-list-y  $_ $_) $_ 3)  True)
  (= (asm-pass12  (fork-unify-nil-y  $_ $_) $_ 3)  True)
  (= (asm-pass12 (fork-unify-symbol-y $_ $C $_) $Csym 4)  
    (sym-member1 
      (sym $C 0 $_ $_) $Csym))
  (= (asm-pass12  (fork-unify-int-y  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (fork-unify-ux-uy  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (fork-unify-uy-uy  $_ $_ $_) $_ 4)  True)

  (= (asm-pass12 (fork-unicut-struct-y $_ (, $S $N) $_) $Csym 4)  
    (sym-member1 
      (sym $S $N $_ $_) $Csym))
  (= (asm-pass12  (fork-unicut-list-y  $_ $_) $_ 3)  True)
  (= (asm-pass12  (fork-unicut-nil-y  $_ $_) $_ 3)  True)
  (= (asm-pass12 (fork-unicut-symbol-y $_ $C $_) $Csym 4)  
    (sym-member1 
      (sym $C 0 $_ $_) $Csym))
  (= (asm-pass12  (fork-unicut-int-y  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (fork-unicut-ux-uy  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (fork-unicut-uy-uy  $_ $_ $_) $_ 4)  True)


  (= (asm-pass12 (unify0-struct-y $_ (, $S $N)) $Csym 3)  
    (sym-member1 
      (sym $S $N $_ $_) $Csym))
  (= (asm-pass12  (unify0-list-y  $_) $_ 2)  True)
  (= (asm-pass12  (unify0-nil-y  $_) $_ 2)  True)
  (= (asm-pass12 (unify0-symbol-y $_ $C) $Csym 3)  
    (sym-member1 
      (sym $C 0 $_ $_) $Csym))
  (= (asm-pass12  (unify0-int-y  $_ $_) $_ 3)  True)
  (= (asm-pass12  (unify0-ux-uy  $_ $_) $_ 3)  True)
  (= (asm-pass12  (unify0-uy-uy  $_ $_) $_ 3)  True)
  (= (asm-pass12 (unicut-struct-y $_ (, $S $N)) $Csym 3)  
    (sym-member1 
      (sym $S $N $_ $_) $Csym))
  (= (asm-pass12  (unicut-list-y  $_) $_ 2)  True)
  (= (asm-pass12  (unicut-nil-y  $_) $_ 2)  True)
  (= (asm-pass12 (unicut-symbol-y $_ $C) $Csym 3)  
    (sym-member1 
      (sym $C 0 $_ $_) $Csym))
  (= (asm-pass12  (unicut-int-y  $_ $_) $_ 3)  True)
  (= (asm-pass12  (unicut-uy-uy  $_ $_) $_ 3)  True)
  (= (asm-pass12  unicut $_ 1)  True)

  (= (asm-pass12  unify_arg_nil $_ 1)  True)
; /* Unify argument */
  (= (asm-pass12 (unify-arg-symbol $C) $Csym 2)  
    (sym-member1 
      (sym $C 0 $_ $_) $Csym))
  (= (asm-pass12  (unify-arg-int  $_) $_ 2)  True)
  (= (asm-pass12  (unify-arg-ux-ux  $_ $_) $_ 3)  True)
  (= (asm-pass12  (unify-arg-ux  $_) $_ 2)  True)
  (= (asm-pass12  (unify-arg-ux-vy  $_ $_) $_ 3)  True)
  (= (asm-pass12  (unify-arg-ux-vx  $_ $_) $_ 3)  True)
  (= (asm-pass12  (unify-arg-uy-uy  $_ $_) $_ 3)  True)
  (= (asm-pass12  (unify-arg-uy  $_) $_ 2)  True)
  (= (asm-pass12  (unify-arg-vx  $_) $_ 2)  True)
  (= (asm-pass12  (unify-arg-vy  $_) $_ 2)  True)
  (= (asm-pass12  unify_arg_list $_ 1)  True)
  (= (asm-pass12 (unify-arg-struct (, $S $N)) $Csym 2)  
    (sym-member1 
      (sym $S $N $_ $_) $Csym))
  (= (asm-pass12  unify_arg_void_one $_ 1)  True)
  (= (asm-pass12  (unify-arg-void  $_) $_ 2)  True)
  (= (asm-pass12  (unify-arg-wy  $_) $_ 2)  True)
  (= (asm-pass12  (unify-arg-vx-vx  $_ $_) $_ 3)  True)
  (= (asm-pass12  (unify-arg-vx-vy  $_ $_) $_ 3)  True)
  (= (asm-pass12  (unify-arg-vx-ux  $_ $_) $_ 3)  True)
  (= (asm-pass12  (unify-arg-vx-uy  $_ $_) $_ 3)  True)
  (= (asm-pass12  (unify-arg-vy-vx  $_ $_) $_ 3)  True)
  (= (asm-pass12  (unify-arg-vy-vy  $_ $_) $_ 3)  True)
  (= (asm-pass12  (unify-arg-vy-ux  $_ $_) $_ 3)  True)
  (= (asm-pass12  (unify-arg-vy-uy  $_ $_) $_ 3)  True)
  (= (asm-pass12  (unify-arg-iii  $_ $_ $_) $_ 4)  True)

  (= (asm-pass12 (move-struct-x $_ (, $S $N)) $Csym 3)  
    (sym-member1 
      (sym $S $N $_ $_) $Csym))
; /* Move */
  (= (asm-pass12 (move-struct-y $_ (, $S $N)) $Csym 3)  
    (sym-member1 
      (sym $S $N $_ $_) $Csym))
  (= (asm-pass12  (move-list-x  $_) $_ 2)  True)
  (= (asm-pass12  (move-list-y  $_) $_ 2)  True)
  (= (asm-pass12  (move-nil-x  $_) $_ 2)  True)
  (= (asm-pass12  (move-nil-y  $_) $_ 2)  True)
  (= (asm-pass12 (move-symbol-x $_ $C) $Csym 3)  
    (sym-member1 
      (sym $C 0 $_ $_) $Csym))
  (= (asm-pass12 (move-symbol-y $_ $C) $Csym 3)  
    (sym-member1 
      (sym $C 0 $_ $_) $Csym))
  (= (asm-pass12  (move-int-x  $_ $_) $_ 3)  True)
  (= (asm-pass12  (move-int-y  $_ $_) $_ 3)  True)
  (= (asm-pass12  (move-x-ux  $_ $_) $_ 3)  True)
  (= (asm-pass12  (move-x-uy  $_ $_) $_ 3)  True)
  (= (asm-pass12  (move-y-ux  $_ $_) $_ 3)  True)
  (= (asm-pass12  (move-y-uy  $_ $_) $_ 3)  True)
  (= (asm-pass12  (move-vx  $_) $_ 2)  True)
  (= (asm-pass12  (move-vy  $_) $_ 2)  True)
  (= (asm-pass12  (move-x-wy  $_ $_) $_ 3)  True)
  (= (asm-pass12  (move-y-wy  $_ $_) $_ 3)  True)
  (= (asm-pass12  (move-yy-yw  $_ $_ $_ $_) $_ 5)  True)
  (= (asm-pass12  (move-yw-yy  $_ $_ $_ $_) $_ 5)  True)
  (= (asm-pass12  (move-yy-yy  $_ $_ $_ $_) $_ 5)  True)
  (= (asm-pass12  (move-yy-yy-yy  $_ $_ $_ $_ $_ $_) $_ 7)  True)

  (= (asm-pass12  (and  $_ $_ $_) $_ 4)  True)
; /* Numeric */
  (= (asm-pass12  (or  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (lshiftl  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (lshiftr  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (complement  $_ $_) $_ 3)  True)
  (= (asm-pass12  (add  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (add1-y  $_) $_ 2)  True)
  (= (asm-pass12  (sub  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (sub1-y  $_) $_ 2)  True)
  (= (asm-pass12  (mul  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (div  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (idiv  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (mod  $_ $_ $_) $_ 4)  True)

  (= (asm-pass12 (para-struct (, $S $N)) $Csym 2)  
    (sym-member1 
      (sym $S $N $_ $_) $Csym))
; /*Parameter passing */
  (= (asm-pass12  para_list $_ 1)  True)
  (= (asm-pass12  para_nil $_ 1)  True)
  (= (asm-pass12 (para-symbol $C) $Csym 2)  
    (sym-member1 
      (sym $C 0 $_ $_) $Csym))
  (= (asm-pass12  (para-int  $_) $_ 2)  True)
  (= (asm-pass12  (para-ux  $_) $_ 2)  True)
  (= (asm-pass12  (para-uy  $_) $_ 2)  True)
  (= (asm-pass12  (para-vx  $_) $_ 2)  True)
  (= (asm-pass12  (para-vy  $_) $_ 2)  True)
  (= (asm-pass12  para_void_one $_ 1)  True)
  (= (asm-pass12  (para-void  $_) $_ 2)  True)
  (= (asm-pass12  (para-vy-vy  $_ $_) $_ 3)  True)
  (= (asm-pass12  (para-vy-ux  $_ $_) $_ 3)  True)
  (= (asm-pass12  (para-vy-uy  $_ $_) $_ 3)  True)
  (= (asm-pass12  (para-ux-vy  $_ $_) $_ 3)  True)
  (= (asm-pass12  (para-ux-ux  $_ $_) $_ 3)  True)
  (= (asm-pass12  (para-ux-uy  $_ $_) $_ 3)  True)
  (= (asm-pass12  (para-uy-vy  $_ $_) $_ 3)  True)
  (= (asm-pass12  (para-uy-ux  $_ $_) $_ 3)  True)
  (= (asm-pass12  (para-uy-uy  $_ $_) $_ 3)  True)
  (= (asm-pass12  (para-ux-ux-ux  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (para-ux-ux-uy  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (para-ux-uy-ux  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (para-ux-uy-uy  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (para-uy-ux-ux  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (para-uy-ux-uy  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (para-uy-uy-ux  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (para-uy-uy-uy  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (para-uy-uy-uy-uy  $_ $_ $_ $_) $_ 5)  True)

  (= (asm-pass12 (call (, $P $N)) $Csym 2)  
    (asm-pass12-call $P $N $Csym))
; /* Procedural */
  (= (asm-pass12  (callv  $_) $_ 2)  True)
  (= (asm-pass12 (execute (, $P $N)) $Csym 2)  
    (asm-pass12-call $P $N $Csym))
  (= (asm-pass12  (executev  $_) $_ 2)  True)
  (= (asm-pass12  return_a $_ 1)  True)
  (= (asm-pass12  return_b $_ 1)  True)
  (= (asm-pass12  (jmpn-det  $L) $_ 2)  True)
  (= (asm-pass12  (save-ht-jmp  $_ $_) $_ 3)  True)

  (= (asm-pass12  (allocate-flat  $N) $_ 2)  True)
; /* Allocate */
  (= (asm-pass12  (allocate-nonflat  $N) $_ 2)  True)
  (= (asm-pass12  (allocate-nondet  $N) $_ 2)  True)
  (= (asm-pass12  (flat-to-nondet  $N) $_ 2)  True)

  (= (asm-pass12  fail $_ 1)  True)
; /* Backtracking */
  (= (asm-pass12  fail0 $_ 1)  True)

  (= (asm-pass12  (fork  $_) $_ 2)  True)

  (= (asm-pass12  commit $_ 1)  True)
  (= (asm-pass12  cut $_ 1)  True)
  (= (asm-pass12  cut_fail $_ 1)  True)
  (= (asm-pass12  cut_return $_ 1)  True)
  (= (asm-pass12  save_b $_ 1)  True)
  (= (asm-pass12  (getbreg  $_) $_ 2)  True)
  (= (asm-pass12  (getpbreg  $_) $_ 2)  True)
  (= (asm-pass12  (putbreg  $_) $_ 2)  True)

  (= (asm-pass12  (hash-x  $_ $_) $_ 5)  True)
; /* Hashing */
  (= (asm-pass12  (hash-y  $_ $_) $_ 5)  True)
  (= (asm-pass12  (tabsize  $_) $_ 2)  True)

  (= (asm-pass12  (arg  $_ $_ $_) $_ 4)  True)
; /* builtin */
  (= (asm-pass12  (arg0  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (setarg  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (setarg0  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (functor  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (builtin0  $_ $_) $_ 3)  True)
  (= (asm-pass12  (builtin1  $_ $_ $_) $_ 4)  True)
  (= (asm-pass12  (builtin2  $_ $_ $_ $_) $_ 5)  True)
  (= (asm-pass12  (builtin3  $_ $_ $_ $_ $_) $_ 6)  True)
  (= (asm-pass12  (builtin4  $_ $_ $_ $_ $_ $_) $_ 7)  True)
  (= (asm-pass12  (func-arity  $_ $_) $_ 3)  True)

  (= (asm-pass12  halt $_ 1)  True)
; /* Miscellaneous */
  (= (asm-pass12  endfile $_ 1)  True)
  (= (asm-pass12  (gethtreg  $_ $_) $_ 3)  True)
  (= (asm-pass12  (puthtreg  $_ $_) $_ 3)  True)
  (= (asm-pass12  endfile $_ 1)  True)
  (= (asm-pass12  get_ar_cps $_ 1)  True)
  (= (asm-pass12  (put-ar-cps  $_) $_ 2)  True)
  (= (asm-pass12  (move-ar-cps  $_) $_ 2)  True)
  (= (asm-pass12  (jmpn-det-get-ar-cps  $L) $_ 2)  True)
  (= (asm-pass12  (allocate-susp  $N) $_ 2)  True)
  (= (asm-pass12  (susp-var-x  $_) $_ 2)  True)
  (= (asm-pass12  (susp-var-y  $_) $_ 2)  True)
  (= (asm-pass12 (delay (, $S $N) $_) $Csym 3)  
    (sym-member1 
      (sym $S $N $_ $_) $Csym))	
  (= (asm-pass12 (susp-var-delay $_ (, $S $N) $_ $_) $Csym 5)  
    (sym-member1 
      (sym $S $N $_ $_) $Csym))
  (= (asm-pass12  end_delay $_ 1)  True)

  (= (asm-pass12  (nondet  $_) $_ 2)  True)
  (= (asm-pass12  (jmp-susp  $_) $_ 2)  True)
  (= (asm-pass12  (jmpn-dvar-y  $_ $_) $_ 3)  True)
  (= (asm-pass12 (susp-var2-delay $_ (, $S $N) $_ $_) $Csym 5)  
    (sym-member1 
      (sym $S $N $_ $_) $Csym))
  (= (asm-pass12  (domain-set-false-yy  $_ $_) $_ 3)  True)
  (= (asm-pass12  (domain-set-false-yx  $_ $_) $_ 3)  True)

  (= (asm-pass12-call $P $N $Csym)  
    ( (isglobal %asm-bp 0) 
      (b-GET-SYM-TYPE-ccf $P $N $Type) 
      (with-self  
        (=\= $Type 3) True)))
; /* no a call to a c function */
  (= (asm-pass12-call $P $N $Csym)  
    (with-self  True 
      (sym-member1 
        (sym $P $N $_ $_) $Csym)))


  (= (asm-pass2 $Prog $Index $Csym $Lsym)  
    ( (with-self  True 
        (asm-rearange-csym $Csym 255 $_ $Csym1)) 
      (asm-symbol $Csym1) 
      (asm-pass2a $Prog $Csym $Lsym) 
      (asm-index $Index $Csym $Lsym)))
; /**/
; /* asm_pass12( Junk,_,N) :- true : N=0, cmp_error(['*** Error in assembly: unknown opcode: ',Junk]). */
; /******************* Pass 2 **************************************/


  (= (asm-index Nil $_ $_)  
    (with-self  True True))
  (= (asm-index (Cons  $Inst $Index) $Csym $Lsym)  
    ( (with-self  True 
        (asm-proc-index $Inst $Csym $Lsym)) (asm-index $Index $Csym $Lsym)))


  (= (asm-proc-index (pred $HashLab $Op $Num $Alt) $Csym $Lsym)  
    ( (with-self  True 
        (x-or-y $Op $XY)) 
      (asm-lookup-ltab $HashLab $Lsym $Val1) 
      (asm-lookup-ltab $Alt $Lsym $Val2) 
      (b-ASPN4-cccc $Val1 $XY $Num $Val2)))
  (= (asm-proc-index (arglabel $T $Val $Label) $Csym $Lsym)  
    ( (with-self  
        (== $T c) 
        (asm-lookup-ctab $Val 0 $Csym $Nval)) 
      (asm-lookup-ltab $Label $Lsym $L) 
      (writename $T) 
      (b-ASPN2-cc $Nval $L)))
  (= (asm-proc-index (arglabel $T $Val $Label) $Csym $Lsym)  
    ( (with-self  
        (== $T s) 
        (= $Val 
          (, $Str $Arity))) 
      (asm-lookup-ctab $Str $Arity $Csym $Nval) 
      (asm-lookup-ltab $Label $Lsym $L) 
      (writename $T) 
      (b-ASPN2-cc $Nval $L)))
  (= (asm-proc-index (arglabel $T $Val $Label) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $Label $Lsym $L)) 
      (writename $T) 
      (b-ASPN2-cc $Val $L)))


  (= (asm-pass2a Nil $_ $_)  
    (with-self  True True))
  (= (asm-pass2a (Cons  $Inst $Insts) $Csym $Lsym)  
    ( (with-self  True 
        (asm-pass2-inst $Inst $Csym $Lsym)) (asm-pass2a $Insts $Csym $Lsym)))



  (= (asm-pass2-inst (label $_) $_ $_)  
    (with-self  True True))
; /* asm_pass2_inst(X,_,_) :- true ? output_mess(X), fail. */
  (= (asm-pass2-inst (call $Lab) $Csym $Lsym)  
    ( (with-self  
        (asm-lookup-ltab $Lab $Lsym $EPaddr) 
        (opcode call-d $X)) (b-ASPN2-cc $X $EPaddr)))
  (= (asm-pass2-inst (call (, $P $N)) $Csym $Lsym)  
    (with-self  True 
      (det-if-then-else 
        (asm-lookup-ctab $P $N $Csym $Index) 
        (, 
          (opcode call $X) 
          (b-ASPN2-cc $X $Index)) 
        (warning (:: (/ $P $N) ' is called but not defined')))))
  (= (asm-pass2-inst (execute $Lab) $Csym $Lsym)  
    ( (with-self  
        (asm-lookup-ltab $Lab $Lsym $EPaddr) 
        (opcode djmp $X)) (b-ASPN2-cc $X $EPaddr)))
  (= (asm-pass2-inst (execute (, $P $N)) $Csym $Lsym)  
    (with-self  True 
      (det-if-then-else 
        (asm-lookup-ctab $P $N $Csym $Index) 
        (, 
          (opcode execute $X) 
          (b-ASPN2-cc $X $Index)) 
        (warning (:: (/ $P $N) ' is called but not defined')))))
  (= (asm-pass2-inst (jmp $L) $Csym $Lsym)  
    ( (with-self  
        (asm-lookup-ltab $L $Lsym $Val) 
        (opcode jmp $X)) (b-ASPN2-cc $X $Val)))
  (= (asm-pass2-inst (jmp-susp $L) $Csym $Lsym)  
    ( (with-self  
        (asm-lookup-ltab $L $Lsym $Val) 
        (opcode jmp-susp $X)) (b-ASPN2-cc $X $Val)))
  (= (asm-pass2-inst (jmpn-eq-struct-x $Op (, $S $N) $L1 $L2) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $S $N $Csym $I)) 
      (asm-lookup-ltab $L1 $Lsym $Val1) 
      (asm-lookup-ltab $L2 $Lsym $Val2) 
      (opcode jmpn-eq-struct-x $X) 
      (b-ASPN4-cccc $X $Op $I $Val1) 
      (b-ASPN-c $Val2)))
  (= (asm-pass2-inst (jmpn-eq-struct-y $Op (, $S $N) $L1 $L2) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $S $N $Csym $I)) 
      (asm-lookup-ltab $L1 $Lsym $Val1) 
      (asm-lookup-ltab $L2 $Lsym $Val2) 
      (opcode jmpn-eq-struct-y $X) 
      (b-ASPN4-cccc $X $Op $I $Val1) 
      (b-ASPN-c $Val2)))
  (= (asm-pass2-inst (jmpn-eq-symbol-x $Op $C $L1 $L2) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $C 0 $Csym $I)) 
      (asm-lookup-ltab $L1 $Lsym $Val1) 
      (asm-lookup-ltab $L2 $Lsym $Val2) 
      (opcode jmpn-eq-symbol-x $X) 
      (b-ASPN4-cccc $X $Op $I $Val1) 
      (b-ASPN-c $Val2)))
  (= (asm-pass2-inst (jmpn-eq-symbol-y $Op $C $L1 $L2) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $C 0 $Csym $I)) 
      (asm-lookup-ltab $L1 $Lsym $Val1) 
      (asm-lookup-ltab $L2 $Lsym $Val2) 
      (opcode jmpn-eq-symbol-y $X) 
      (b-ASPN4-cccc $X $Op $I $Val1) 
      (b-ASPN-c $Val2)))
  (= (asm-pass2-inst (jmpn-nil-x $Op $L1 $L2) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L1 $Lsym $Val1)) 
      (asm-lookup-ltab $L2 $Lsym $Val2) 
      (opcode jmpn-nil-x $X) 
      (b-ASPN4-cccc $X $Op $Val1 $Val2)))
  (= (asm-pass2-inst (jmpn-nil-y $Op $L1 $L2) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L1 $Lsym $Val1)) 
      (asm-lookup-ltab $L2 $Lsym $Val2) 
      (opcode jmpn-nil-y $X) 
      (b-ASPN4-cccc $X $Op $Val1 $Val2)))
  (= (asm-pass2-inst (switch-list-x $Op $L1 $L2 $L3) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L1 $Lsym $Val1)) 
      (asm-lookup-ltab $L2 $Lsym $Val2) 
      (asm-lookup-ltab $L3 $Lsym $Val3) 
      (opcode switch-list-x $X) 
      (b-ASPN4-cccc $X $Op $Val1 $Val2) 
      (b-ASPN-c $Val3)))
  (= (asm-pass2-inst (switch-list-y $Op $L1 $L2 $L3) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L1 $Lsym $Val1)) 
      (asm-lookup-ltab $L2 $Lsym $Val2) 
      (asm-lookup-ltab $L3 $Lsym $Val3) 
      (opcode switch-list-y $X) 
      (b-ASPN4-cccc $X $Op $Val1 $Val2) 
      (b-ASPN-c $Val3)))
  (= (asm-pass2-inst (switch-list-yxx $Op $L1 $L2 $L3) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L1 $Lsym $Val1)) 
      (asm-lookup-ltab $L2 $Lsym $Val2) 
      (asm-lookup-ltab $L3 $Lsym $Val3) 
      (opcode switch-list-yxx $X) 
      (b-ASPN4-cccc $X $Op $Val1 $Val2) 
      (b-ASPN-c $Val3)))
  (= (asm-pass2-inst (switch-list-yxy $Op $L1 $L2 $L3) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L1 $Lsym $Val1)) 
      (asm-lookup-ltab $L2 $Lsym $Val2) 
      (asm-lookup-ltab $L3 $Lsym $Val3) 
      (opcode switch-list-yxy $X) 
      (b-ASPN4-cccc $X $Op $Val1 $Val2) 
      (b-ASPN-c $Val3)))
  (= (asm-pass2-inst (switch-list-yyx $Op $L1 $L2 $L3) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L1 $Lsym $Val1)) 
      (asm-lookup-ltab $L2 $Lsym $Val2) 
      (asm-lookup-ltab $L3 $Lsym $Val3) 
      (opcode switch-list-yyx $X) 
      (b-ASPN4-cccc $X $Op $Val1 $Val2) 
      (b-ASPN-c $Val3)))
  (= (asm-pass2-inst (switch-list-yyy $Op $L1 $L2 $L3) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L1 $Lsym $Val1)) 
      (asm-lookup-ltab $L2 $Lsym $Val2) 
      (asm-lookup-ltab $L3 $Lsym $Val3) 
      (opcode switch-list-yyy $X) 
      (b-ASPN4-cccc $X $Op $Val1 $Val2) 
      (b-ASPN-c $Val3)))
  (= (asm-pass2-inst (jmpn-eq-int-x $Op $I $L1 $L2) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L1 $Lsym $Val1)) 
      (asm-lookup-ltab $L2 $Lsym $Val2) 
      (opcode jmpn-eq-int-x $X) 
      (b-ASPN4-cccc $X $Op $I $Val1) 
      (b-ASPN-c $Val2)))
  (= (asm-pass2-inst (jmpn-eq-int-y $Op $I $L1 $L2) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L1 $Lsym $Val1)) 
      (asm-lookup-ltab $L2 $Lsym $Val2) 
      (opcode jmpn-eq-int-y $X) 
      (b-ASPN4-cccc $X $Op $I $Val1) 
      (b-ASPN-c $Val2)))
  (= (asm-pass2-inst (jmpn-eql $Op1 $Op2 $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (x-or-y $Op1 $XY1) 
      (x-or-y $Op2 $XY2) 
      (opcode jmpn-eql $X) 
      (b-ASPN4-cccc $X $XY1 $XY2 $Val)))
  (= (asm-pass2-inst (jmp-eql $Op1 $Op2 $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (x-or-y $Op1 $XY1) 
      (x-or-y $Op2 $XY2) 
      (opcode jmp-eql $X) 
      (b-ASPN4-cccc $X $XY1 $XY2 $Val)))
  (= (asm-pass2-inst (jmp-eql-yy $Op1 $Op2 $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode jmp-eql-yy $X) 
      (b-ASPN4-cccc $X $Op1 $Op2 $Val)))
  (= (asm-pass2-inst (jmpn-gt $Op1 $Op2 $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (x-or-y $Op1 $XY1) 
      (x-or-y $Op2 $XY2) 
      (opcode jmpn-gt $X) 
      (b-ASPN4-cccc $X $XY1 $XY2 $Val)))
  (= (asm-pass2-inst (jmpn-gt-yy $Op1 $Op2 $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode jmpn-gt-yy $X) 
      (b-ASPN4-cccc $X $Op1 $Op2 $Val)))
  (= (asm-pass2-inst (jmpn-ge $Op1 $Op2 $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (x-or-y $Op1 $XY1) 
      (x-or-y $Op2 $XY2) 
      (opcode jmpn-ge $X) 
      (b-ASPN4-cccc $X $XY1 $XY2 $Val)))
  (= (asm-pass2-inst (jmpn-ge-yy $Op1 $Op2 $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode jmpn-ge-yy $X) 
      (b-ASPN4-cccc $X $Op1 $Op2 $Val)))
  (= (asm-pass2-inst (jmpn-id $Op1 $Op2 $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (x-or-y $Op1 $XY1) 
      (x-or-y $Op2 $XY2) 
      (opcode jmpn-id $X) 
      (b-ASPN4-cccc $X $XY1 $XY2 $Val)))
  (= (asm-pass2-inst (jmp-id $Op1 $Op2 $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (x-or-y $Op1 $XY1) 
      (x-or-y $Op2 $XY2) 
      (opcode jmp-id $X) 
      (b-ASPN4-cccc $X $XY1 $XY2 $Val)))
  (= (asm-pass2-inst (jmpn-var-x $Op $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode jmpn-var-x $X) 
      (b-ASPN3-ccc $X $Op $Val)))
  (= (asm-pass2-inst (jmpn-var-y $Op $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode jmpn-var-y $X) 
      (b-ASPN3-ccc $X $Op $Val)))
  (= (asm-pass2-inst (jmp-var-x $Op $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode jmp-var-x $X) 
      (b-ASPN3-ccc $X $Op $Val)))
  (= (asm-pass2-inst (jmp-var-y $Op $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode jmp-var-y $X) 
      (b-ASPN3-ccc $X $Op $Val)))
  (= (asm-pass2-inst (jmpn-symbol-x $Op $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode jmpn-symbol-x $X) 
      (b-ASPN3-ccc $X $Op $Val)))
  (= (asm-pass2-inst (jmpn-symbol-y $Op $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode jmpn-symbol-y $X) 
      (b-ASPN3-ccc $X $Op $Val)))
  (= (asm-pass2-inst (jmpn-symbolic-x $Op $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode jmpn-symbolic-x $X) 
      (b-ASPN3-ccc $X $Op $Val)))
  (= (asm-pass2-inst (jmpn-symbolic-y $Op $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode jmpn-symbolic-y $X) 
      (b-ASPN3-ccc $X $Op $Val)))
  (= (asm-pass2-inst (jmpn-num-x $Op $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode jmpn-num-x $X) 
      (b-ASPN3-ccc $X $Op $Val)))
  (= (asm-pass2-inst (jmpn-num-y $Op $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode jmpn-num-y $X) 
      (b-ASPN3-ccc $X $Op $Val)))
  (= (asm-pass2-inst (jmpn-float-x $Op $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode jmpn-float-x $X) 
      (b-ASPN3-ccc $X $Op $Val)))
  (= (asm-pass2-inst (jmpn-float-y $Op $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode jmpn-float-y $X) 
      (b-ASPN3-ccc $X $Op $Val)))
  (= (asm-pass2-inst (jmpn-int-x $Op $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode jmpn-int-x $X) 
      (b-ASPN3-ccc $X $Op $Val)))
  (= (asm-pass2-inst (jmpn-int-y $Op $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode jmpn-int-y $X) 
      (b-ASPN3-ccc $X $Op $Val)))
  (= (asm-pass2-inst (hash-jmpn-nil $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode hash-jmpn-nil $X) 
      (b-ASPN2-cc $X $Val)))
  (= (asm-pass2-inst (hash-jmpn-list $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode hash-jmpn-list $X) 
      (b-ASPN2-cc $X $Val)))
  (= (asm-pass2-inst (hash-jmpn-int $I $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode hash-jmpn-int $X) 
      (b-ASPN3-ccc $X $I $Val)))
  (= (asm-pass2-inst (hash-jmpn-symbol $C $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $C 0 $Csym $I)) 
      (asm-lookup-ltab $L $Lsym $Val) 
      (opcode hash-jmpn-symbol $X) 
      (b-ASPN3-ccc $X $I $Val)))
  (= (asm-pass2-inst (hash-jmpn-struct (, $S $N) $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $S $N $Csym $I)) 
      (asm-lookup-ltab $L $Lsym $Val) 
      (opcode hash-jmpn-struct $X) 
      (b-ASPN3-ccc $X $I $Val)))
  (= (asm-pass2-inst (hash-jmpn-struct-x (, $S $N) $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $S $N $Csym $I)) 
      (asm-lookup-ltab $L $Lsym $Val) 
      (opcode hash-jmpn-struct-x $X) 
      (b-ASPN3-ccc $X $I $Val)))
  (= (asm-pass2-inst (hash-jmpn-struct-y (, $S $N) $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $S $N $Csym $I)) 
      (asm-lookup-ltab $L $Lsym $Val) 
      (opcode hash-jmpn-struct-y $X) 
      (b-ASPN3-ccc $X $I $Val)))
  (= (asm-pass2-inst (hash-jmpn-struct-xx (, $S $N) $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $S $N $Csym $I)) 
      (asm-lookup-ltab $L $Lsym $Val) 
      (opcode hash-jmpn-struct-xx $X) 
      (b-ASPN3-ccc $X $I $Val)))
  (= (asm-pass2-inst (hash-jmpn-struct-xy (, $S $N) $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $S $N $Csym $I)) 
      (asm-lookup-ltab $L $Lsym $Val) 
      (opcode hash-jmpn-struct-xy $X) 
      (b-ASPN3-ccc $X $I $Val)))
  (= (asm-pass2-inst (hash-jmpn-struct-yx (, $S $N) $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $S $N $Csym $I)) 
      (asm-lookup-ltab $L $Lsym $Val) 
      (opcode hash-jmpn-struct-yx $X) 
      (b-ASPN3-ccc $X $I $Val)))
  (= (asm-pass2-inst (hash-jmpn-struct-yy (, $S $N) $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $S $N $Csym $I)) 
      (asm-lookup-ltab $L $Lsym $Val) 
      (opcode hash-jmpn-struct-yy $X) 
      (b-ASPN3-ccc $X $I $Val)))
  (= (asm-pass2-inst (jmpn-det $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode jmpn-det $X) 
      (b-ASPN2-cc $X $Val)))
  (= (asm-pass2-inst (jmpn-det-get-ar-cps $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode jmpn-det-get-ar-cps $X) 
      (b-ASPN2-cc $X $Val)))
  (= (asm-pass2-inst (save-ht-jmp $L1 $L2) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L1 $Lsym $Val1)) 
      (asm-lookup-ltab $L2 $Lsym $Val2) 
      (opcode save-ht-jmp $X) 
      (b-ASPN3-ccc $X $Val1 $Val2)))
  (= (asm-pass2-inst (unify-struct-x $Op (, $S $N)) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $S $N $Csym $I)) 
      (opcode unify-struct-x $X) 
      (b-ASPN3-ccc $X $Op $I)))
  (= (asm-pass2-inst (unify-struct-y $Op (, $S $N)) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $S $N $Csym $I)) 
      (opcode unify-struct-y $X) 
      (b-ASPN3-ccc $X $Op $I)))
  (= (asm-pass2-inst (unify0-struct-y $Op (, $S $N)) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $S $N $Csym $I)) 
      (opcode unify0-struct-y $X) 
      (b-ASPN3-ccc $X $Op $I)))
  (= (asm-pass2-inst (unicut-struct-y $Op (, $S $N)) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $S $N $Csym $I)) 
      (opcode unicut-struct-y $X) 
      (b-ASPN3-ccc $X $Op $I)))
  (= (asm-pass2-inst (unify-symbol-x $Op $C) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $C 0 $Csym $I)) 
      (opcode unify-symbol-x $X) 
      (b-ASPN3-ccc $X $Op $I)))
  (= (asm-pass2-inst (unify-symbol-y $Op $C) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $C 0 $Csym $I)) 
      (opcode unify-symbol-y $X) 
      (b-ASPN3-ccc $X $Op $I)))
  (= (asm-pass2-inst (unify0-symbol-y $Op $C) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $C 0 $Csym $I)) 
      (opcode unify0-symbol-y $X) 
      (b-ASPN3-ccc $X $Op $I)))
  (= (asm-pass2-inst (unicut-symbol-y $Op $C) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $C 0 $Csym $I)) 
      (opcode unicut-symbol-y $X) 
      (b-ASPN3-ccc $X $Op $I)))
  (= (asm-pass2-inst (unify-arg-symbol $C) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $C 0 $Csym $I)) 
      (opcode unify-arg-symbol $X) 
      (b-ASPN2-cc $X $I)))
  (= (asm-pass2-inst (unify-arg-struct (, $S $N)) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $S $N $Csym $I)) 
      (opcode unify-arg-struct $X) 
      (b-ASPN2-cc $X $I)))

  (= (asm-pass2-inst (move-struct-x $Op (, $S $N)) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $S $N $Csym $I)) 
      (opcode move-struct-x $X) 
      (b-ASPN3-ccc $X $Op $I)))
  (= (asm-pass2-inst (move-struct-y $Op (, $S $N)) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $S $N $Csym $I)) 
      (opcode move-struct-y $X) 
      (b-ASPN3-ccc $X $Op $I)))
  (= (asm-pass2-inst (move-symbol-x $Op $C) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $C 0 $Csym $I)) 
      (opcode move-symbol-x $X) 
      (b-ASPN3-ccc $X $Op $I)))
  (= (asm-pass2-inst (move-symbol-y $Op $C) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $C 0 $Csym $I)) 
      (opcode move-symbol-y $X) 
      (b-ASPN3-ccc $X $Op $I)))
  (= (asm-pass2-inst (and $Op1 $Op2 $Op3) $Csym $Lsym)  
    ( (with-self  True 
        (x-or-y $Op1 $XY1)) 
      (x-or-y $Op2 $XY2) 
      (x-or-y $Op3 $XY3) 
      (opcode and $X) 
      (b-ASPN4-cccc $X $XY1 $XY2 $XY3)))
  (= (asm-pass2-inst (or $Op1 $Op2 $Op3) $Csym $Lsym)  
    ( (with-self  True 
        (x-or-y $Op1 $XY1)) 
      (x-or-y $Op2 $XY2) 
      (x-or-y $Op3 $XY3) 
      (opcode or $X) 
      (b-ASPN4-cccc $X $XY1 $XY2 $XY3)))
  (= (asm-pass2-inst (lshiftl $Op1 $Op2 $Op3) $Csym $Lsym)  
    ( (with-self  True 
        (x-or-y $Op1 $XY1)) 
      (x-or-y $Op2 $XY2) 
      (x-or-y $Op3 $XY3) 
      (opcode lshiftl $X) 
      (b-ASPN4-cccc $X $XY1 $XY2 $XY3)))
  (= (asm-pass2-inst (lshiftr $Op1 $Op2 $Op3) $Csym $Lsym)  
    ( (with-self  True 
        (x-or-y $Op1 $XY1)) 
      (x-or-y $Op2 $XY2) 
      (x-or-y $Op3 $XY3) 
      (opcode lshiftr $X) 
      (b-ASPN4-cccc $X $XY1 $XY2 $XY3)))
  (= (asm-pass2-inst (complement $Op1 $Op2) $Csym $Lsym)  
    ( (with-self  True 
        (x-or-y $Op1 $XY1)) 
      (x-or-y $Op2 $XY2) 
      (opcode complement $X) 
      (b-ASPN3-ccc $X $XY1 $XY2)))
  (= (asm-pass2-inst (add $Op1 $Op2 $Op3) $Csym $Lsym)  
    ( (with-self  True 
        (x-or-y $Op1 $XY1)) 
      (x-or-y $Op2 $XY2) 
      (x-or-y $Op3 $XY3) 
      (opcode add $X) 
      (b-ASPN4-cccc $X $XY1 $XY2 $XY3)))
  (= (asm-pass2-inst (sub $Op1 $Op2 $Op3) $Csym $Lsym)  
    ( (with-self  True 
        (x-or-y $Op1 $XY1)) 
      (x-or-y $Op2 $XY2) 
      (x-or-y $Op3 $XY3) 
      (opcode sub $X) 
      (b-ASPN4-cccc $X $XY1 $XY2 $XY3)))
  (= (asm-pass2-inst (mul $Op1 $Op2 $Op3) $Csym $Lsym)  
    ( (with-self  True 
        (x-or-y $Op1 $XY1)) 
      (x-or-y $Op2 $XY2) 
      (x-or-y $Op3 $XY3) 
      (opcode mul $X) 
      (b-ASPN4-cccc $X $XY1 $XY2 $XY3)))
  (= (asm-pass2-inst (div $Op1 $Op2 $Op3) $Csym $Lsym)  
    ( (with-self  True 
        (x-or-y $Op1 $XY1)) 
      (x-or-y $Op2 $XY2) 
      (x-or-y $Op3 $XY3) 
      (opcode div $X) 
      (b-ASPN4-cccc $X $XY1 $XY2 $XY3)))
  (= (asm-pass2-inst (idiv $Op1 $Op2 $Op3) $Csym $Lsym)  
    ( (with-self  True 
        (x-or-y $Op1 $XY1)) 
      (x-or-y $Op2 $XY2) 
      (x-or-y $Op3 $XY3) 
      (opcode idiv $X) 
      (b-ASPN4-cccc $X $XY1 $XY2 $XY3)))
  (= (asm-pass2-inst (mod $Op1 $Op2 $Op3) $Csym $Lsym)  
    ( (with-self  True 
        (x-or-y $Op1 $XY1)) 
      (x-or-y $Op2 $XY2) 
      (x-or-y $Op3 $XY3) 
      (opcode mod $X) 
      (b-ASPN4-cccc $X $XY1 $XY2 $XY3)))
  (= (asm-pass2-inst (para-struct (, $S $N)) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $S $N $Csym $I)) 
      (opcode para-struct $X) 
      (b-ASPN2-cc $X $I)))
  (= (asm-pass2-inst (para-symbol $C) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $C 0 $Csym $I)) 
      (opcode para-symbol $X) 
      (b-ASPN2-cc $X $I)))
  (= (asm-pass2-inst (fork $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode fork $X) 
      (b-ASPN2-cc $X $Val)))
  (= (asm-pass2-inst (fork-unify-struct-y $Op (, $S $N) $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (asm-lookup-ctab $S $N $Csym $I) 
      (opcode fork-unify-struct-y $X) 
      (b-ASPN4-cccc $X $Op $I $Val)))
  (= (asm-pass2-inst (fork-unify-list-y $Op $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode fork-unify-list-y $X) 
      (b-ASPN3-ccc $X $Op $Val)))
  (= (asm-pass2-inst (fork-unify-nil-y $Op $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode fork-unify-nil-y $X) 
      (b-ASPN3-ccc $X $Op $Val)))
  (= (asm-pass2-inst (fork-unify-symbol-y $Op $C $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $C 0 $Csym $I)) 
      (asm-lookup-ltab $L $Lsym $Val) 
      (opcode fork-unify-symbol-y $X) 
      (b-ASPN4-cccc $X $Op $I $Val)))
  (= (asm-pass2-inst (fork-unify-int-y $Op1 $Op2 $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode fork-unify-int-y $X) 
      (b-ASPN4-cccc $X $Op1 $Op2 $Val)))
  (= (asm-pass2-inst (fork-unify-uy-uy $Op1 $Op2 $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode fork-unify-uy-uy $X) 
      (b-ASPN4-cccc $X $Op1 $Op2 $Val)))

  (= (asm-pass2-inst (fork-unicut-struct-y $Op (, $S $N) $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (asm-lookup-ctab $S $N $Csym $I) 
      (opcode fork-unicut-struct-y $X) 
      (b-ASPN4-cccc $X $Op $I $Val)))
  (= (asm-pass2-inst (fork-unicut-list-y $Op $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode fork-unicut-list-y $X) 
      (b-ASPN3-ccc $X $Op $Val)))
  (= (asm-pass2-inst (fork-unicut-nil-y $Op $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode fork-unicut-nil-y $X) 
      (b-ASPN3-ccc $X $Op $Val)))
  (= (asm-pass2-inst (fork-unicut-symbol-y $Op $C $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $C 0 $Csym $I)) 
      (asm-lookup-ltab $L $Lsym $Val) 
      (opcode fork-unicut-symbol-y $X) 
      (b-ASPN4-cccc $X $Op $I $Val)))
  (= (asm-pass2-inst (fork-unicut-int-y $Op1 $Op2 $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode fork-unicut-int-y $X) 
      (b-ASPN4-cccc $X $Op1 $Op2 $Val)))
  (= (asm-pass2-inst (fork-unicut-ux-uy $Op1 $Op2 $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode fork-unicut-ux-uy $X) 
      (b-ASPN4-cccc $X $Op1 $Op2 $Val)))
  (= (asm-pass2-inst (fork-unicut-uy-uy $Op1 $Op2 $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode fork-unicut-uy-uy $X) 
      (b-ASPN4-cccc $X $Op1 $Op2 $Val)))

  (= (asm-pass2-inst (hash-x $Op $L) $Csym $Lsym)  
    ( (with-self  True 
        (opcode hash-x $X)) 
      (asm-lookup-ltab $L $Lsym $Val) 
      (b-ASPN4-cccc $X $Op 0 0) 
      (b-ASPN-c $Val)))
  (= (asm-pass2-inst (hash-y $Op $L) $Csym $Lsym)  
    ( (with-self  True 
        (opcode hash-y $X)) 
      (asm-lookup-ltab $L $Lsym $Val) 
      (b-ASPN4-cccc $X $Op 0 0) 
      (b-ASPN-c $Val)))
  (= (asm-pass2-inst (callv $Op1) $Csym $Lsym)  
    ( (with-self  True 
        (opcode callv $X)) 
      (x-or-y $Op1 $XY1) 
      (b-ASPN2-cc $X $XY1)))
  (= (asm-pass2-inst (executev $Op1) $Csym $Lsym)  
    ( (with-self  True 
        (opcode executev $X)) 
      (x-or-y $Op1 $XY1) 
      (b-ASPN2-cc $X $XY1)))
  (= (asm-pass2-inst (functor $Op1 $Op2 $Op3) $Csym $Lsym)  
    ( (with-self  True 
        (opcode functor $X)) 
      (x-or-y $Op1 $XY1) 
      (x-or-y $Op2 $XY2) 
      (x-or-y $Op3 $XY3) 
      (b-ASPN4-cccc $X $XY1 $XY2 $XY3)))
  (= (asm-pass2-inst (func-arity $Op1 $Op2) $Csym $Lsym)  
    ( (with-self  True 
        (opcode func-arity $X)) 
      (x-or-y $Op1 $XY1) 
      (x-or-y $Op2 $XY2) 
      (b-ASPN3-ccc $X $XY1 $XY2)))
  (= (asm-pass2-inst (arg0 $Op1 $Op2 $Op3) $Csym $Lsym)  
    ( (with-self  
        (integer $Op1) 
        (opcode arg0 $X)) 
      (x-or-y $Op2 $XY2) 
      (x-or-y $Op3 $XY3) 
      (b-ASPN4-cccc $X $Op1 $XY2 $XY3)))
  (= (asm-pass2-inst (arg $Op1 $Op2 $Op3) $Csym $Lsym)  
    ( (with-self  True 
        (opcode arg $X)) 
      (x-or-y $Op1 $XY1) 
      (x-or-y $Op2 $XY2) 
      (x-or-y $Op3 $XY3) 
      (b-ASPN4-cccc $X $XY1 $XY2 $XY3)))
  (= (asm-pass2-inst (setarg0 $Op1 $Op2 $Op3) $Csym $Lsym)  
    ( (with-self  
        (integer $Op1) 
        (opcode setarg0 $X)) 
      (x-or-y $Op2 $XY2) 
      (x-or-y $Op3 $XY3) 
      (b-ASPN4-cccc $X $Op1 $XY2 $XY3)))
  (= (asm-pass2-inst (setarg $Op1 $Op2 $Op3) $Csym $Lsym)  
    ( (with-self  True 
        (opcode setarg $X)) 
      (x-or-y $Op1 $XY1) 
      (x-or-y $Op2 $XY2) 
      (x-or-y $Op3 $XY3) 
      (b-ASPN4-cccc $X $XY1 $XY2 $XY3)))
  (= (asm-pass2-inst (builtin0 $N $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode builtin0 $X) 
      (b-ASPN3-ccc $X $N $Val)))
  (= (asm-pass2-inst (builtin1 $N $L $Op1) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode builtin1 $X) 
      (x-or-y $Op1 $XY1) 
      (b-ASPN4-cccc $X $N $Val $XY1)))
  (= (asm-pass2-inst (builtin2 $N $L $Op1 $Op2) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode builtin2 $X) 
      (x-or-y $Op1 $XY1) 
      (x-or-y $Op2 $XY2) 
      (b-ASPN4-cccc $X $N $Val $XY1) 
      (b-ASPN-c $XY2)))
  (= (asm-pass2-inst (builtin3 $N $L $Op1 $Op2 $Op3) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode builtin3 $X) 
      (x-or-y $Op1 $XY1) 
      (x-or-y $Op2 $XY2) 
      (x-or-y $Op3 $XY3) 
      (b-ASPN4-cccc $X $N $Val $XY1) 
      (b-ASPN2-cc $XY2 $XY3)))
  (= (asm-pass2-inst (builtin4 $N $L $Op1 $Op2 $Op3 $Op4) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode builtin4 $X) 
      (x-or-y $Op1 $XY1) 
      (x-or-y $Op2 $XY2) 
      (x-or-y $Op3 $XY3) 
      (x-or-y $Op4 $XY4) 
      (b-ASPN4-cccc $X $N $Val $XY1) 
      (b-ASPN3-ccc $XY2 $XY3 $XY4)))
  (= (asm-pass2-inst (getbreg $Op1) $Csym $Lsym)  
    ( (with-self  True 
        (opcode getbreg $X)) 
      (x-or-y $Op1 $XY1) 
      (b-ASPN2-cc $X $XY1)))
  (= (asm-pass2-inst (getpbreg $Op1) $Csym $Lsym)  
    ( (with-self  True 
        (opcode getpbreg $X)) 
      (x-or-y $Op1 $XY1) 
      (b-ASPN2-cc $X $XY1)))
  (= (asm-pass2-inst (putbreg $Op1) $Csym $Lsym)  
    ( (with-self  True 
        (opcode putbreg $X)) 
      (x-or-y $Op1 $XY1) 
      (b-ASPN2-cc $X $XY1)))
  (= (asm-pass2-inst (gethtreg (y $X1) (y $X2)) $Csym $Lsym)  
    ( (with-self  True 
        (opcode gethtreg $X)) (b-ASPN3-ccc $X $X1 $X2)))
  (= (asm-pass2-inst (puthtreg (y $X1) (y $X2)) $Csym $Lsym)  
    ( (with-self  True 
        (opcode puthtreg $X)) (b-ASPN3-ccc $X $X1 $X2)))
  (= (asm-pass2-inst (delay (, $S $N) $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $S $N $Csym $I)) 
      (asm-lookup-ltab $L $Lsym $Val) 
      (opcode delay $X) 
      (b-ASPN3-ccc $X $I $Val)))
  (= (asm-pass2-inst (susp-var-delay $Op (, $S $N) $L1 $L2) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $S $N $Csym $I)) 
      (asm-lookup-ltab $L1 $Lsym $Val1) 
      (asm-lookup-ltab $L2 $Lsym $Val2) 
      (opcode susp-var-delay $X) 
      (b-ASPN4-cccc $X $Op $I $Val1) 
      (b-ASPN-c $Val2)))
  (= (asm-pass2-inst (jmpn-dvar-y $Op $L) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ltab $L $Lsym $Val)) 
      (opcode jmpn-dvar-y $X) 
      (b-ASPN3-ccc $X $Op $Val)))
  (= (asm-pass2-inst (susp-var2-delay $Op (, $S $N) $L1 $L2) $Csym $Lsym)  
    ( (with-self  True 
        (asm-lookup-ctab $S $N $Csym $I)) 
      (asm-lookup-ltab $L1 $Lsym $Val1) 
      (asm-lookup-ltab $L2 $Lsym $Val2) 
      (opcode susp-var2-delay $X) 
      (b-ASPN4-cccc $X $Op $I $Val1) 
      (b-ASPN-c $Val2)))
  (= (asm-pass2-inst $Inst $Csym $Lsym)  
    ( (functor $Inst $F $N) 
      (with-self  
        (opcode $F $X) 
        (b-ASPN-c $X)) 
      (asm-pass2-inst-op $Inst 0 $N)))
  (= (asm-pass2-inst $Inst $Csym $Lsym)  
    (with-self  True 
      (cmp-error (:: 'error in asm pass2 :' $Inst 'is not defined'))))


  (= (asm-pass2-inst-op $Inst $N0 $N1)  
    (with-self  
      (=:= $N0 $N1) True))
  (= (asm-pass2-inst-op $Inst $N0 $Nn)  
    ( (with-self  True 
        (is $N1 
          (+ $N0 1))) 
      (arg $N1 $Inst $Op) 
      (b-ASPN-c $Op) 
      (asm-pass2-inst-op $Inst $N1 $Nn)))


  (= (asm-magic $N)  
    ( (with-self  True 
        (asm-putnum 17 1)) 
      (asm-putnum 18 1) 
      (asm-putnum 19 1) 
      (asm-putnum $N 1)))


  (= (asm-index-inst (pred $_ $_ $_ $_) $Size)  
    (with-self  True 
      (= $Size 16)))
  (= (asm-index-inst (arglabel i $_ $_) $N)  
    (with-self  True 
      (:= $N 9)))
  (= (asm-index-inst (arglabel c $_ $_) $N)  
    (with-self  True 
      (:= $N 9)))
  (= (asm-index-inst (arglabel s $_ $_) $N)  
    (with-self  True 
      (:= $N 9)))


  (= (asm-symbol $Tab)  
    (with-self  
      (var $Tab) True))
  (= (asm-symbol (Cons  (sym $Pred $Arity $Val $_) $Symtab))  
    ( (with-self  True 
        (b-ASPN-c $Val)) 
      (asm-putnum $Arity 1) 
      (b-GET-LENGTH-cf $Pred $L) 
      (asm-putnum $L 1) 
      (writename $Pred) 
      (asm-symbol $Symtab)))


  (= (asm-putnum $Num $NBytes)  
    ( (with-self  
        (> $NBytes 1) 
        (is $Byte 
          (/\ $Num 255))) 
      (is $Rest 
        (>> $Num 8)) 
      (is $N 
        (- $NBytes 1)) 
      (asm-putnum $Rest $N) 
      (put $Byte)))
; /* putnum(Number, Length) will write Number as a binary number which will be Length bytes long */
; /* b_ASPN2_cc(X,Y):-b_ASPN_c(X),b_ASPN_c(Y). b_ASPN3_ccc(X,Y,U):-b_ASPN_c(X),b_ASPN_c(Y),b_ASPN_c(U). b_ASPN4_cccc(X,Y,U,V):-b_ASPN_c(X),b_ASPN_c(Y),b_ASPN_c(U),b_ASPN_c(V). */
; /* aspn(X):- true : write((X,4)),nl. asm_putnum(X,Bytes):- true : write((X,Bytes)),nl. */
; /* aspn(Num):- true : asm_putnum(Num,4). */
  (= (asm-putnum $Num $NBytes)  
    (with-self  True 
      (put $Num)))
; ; Num < 256 :


  (= asm-mark-eot  
    ( (with-self  True 
        (opcode endfile $X)) (b-ASPN2-cc $X 0)))


  (= (sym-member1 $Sym $Csym)  
    ( (with-self  
        (<= 
          (sym $F $N $Val $I) $Sym) 
        (asm-hash-value $F $HashVal)) 
      (is $Index 
        (+ 
          (mod 
            (+ $HashVal $N) 255) 1)) 
      (arg $Index $Csym $L) 
      (sym-member1 $F $N $Sym $L)))
; /* local utilities */

  (= (sym-member1 $F $N $Sym $List)  
    ( (with-self  
        (var $List) 
        (next-sym-no $I)) 
      (arg 4 $Sym $I) 
      (:= $List 
        (Cons  $Sym $_))))
  (= (sym-member1 $F $N $Sym (Cons  (sym $F $N $Val2 $I) $List))  
    (with-self  True 
      (= $Sym 
        (sym $F $N $Val2 $I))))
  (= (sym-member1 $F $N $Sym (Cons  $X1 $List))  
    (with-self  True 
      (sym-member1 $F $N $Sym $List)))

    
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (= (lab-member1 $Lab $Lsym)  
    ( (with-self  
        (<= 
          (lab $X $Val) $Lab) 
        (asm-hash-value $X $HashVal)) 
      (is $Index 
        (+ 
          (mod $HashVal 255) 1)) 
      (arg $Index $Lsym $L) 
      (lab-member1 $X $Lab $L)))
; ; write(user,Index),nl(user),

  (= (lab-member1 $X $Lab $List)  
    (with-self  
      (var $List) 
      (:= $List 
        (Cons  $Lab $_))))
  (= (lab-member1 $X $Lab (Cons  (lab $X $Val2) $List))  
    (with-self  True 
      (= $Lab 
        (lab $X $Val2))))
  (= (lab-member1 $X $Lab (Cons  $_ $List))  
    (with-self  True 
      (lab-member1 $X $Lab $List)))


;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (= (asm-lookup-ltab $Lab $Lsym $Val1)  
    ( (with-self  True 
        (asm-hash-value $Lab $HashVal)) 
      (is $Index 
        (+ 
          (mod $HashVal 255) 1)) 
      (arg $Index $Lsym $L) 
      (asm-lookup-ltab1 $Lab $L $Val1)))
    

  (= (asm-lookup-ltab1 $Lab $Var $Val1)  
    (with-self  
      (var $Var) fail))
  (= (asm-lookup-ltab1 $Lab (Cons  (lab $Lab $Val) $_) $Val1)  
    (with-self  True 
      (= $Val1 $Val)))
  (= (asm-lookup-ltab1 $Lab (Cons  $_ $Tab) $Val)  
    (with-self  True 
      (asm-lookup-ltab1 $Lab $Tab $Val)))


;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (= (asm-lookup-ctab $F $N $Csym $I)  
    ( (with-self  True 
        (asm-hash-value $F $HashVal)) 
      (is $Index 
        (+ 
          (mod 
            (+ $HashVal $N) 255) 1)) 
      (arg $Index $Csym $L) 
      (asm-lookup-ctab1 $F $N $L $I)))


  (= (asm-lookup-ctab1 $F $N (Cons  (sym $F $N $Val $I) $L) $I1)  
    (with-self  True 
      (= $I1 $I)))
  (= (asm-lookup-ctab1 $F $N (Cons  $_ $L) $I)  
    (with-self  True 
      (asm-lookup-ctab1 $F $N $L $I)))

;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (= (asm-rearange-csym $Csym $N $LCsym0 $LCsym)  
    (with-self  
      (< $N 1) 
      (= $LCsym $LCsym0)))
  (= (asm-rearange-csym $Csym $N $LCsym0 $LCsym)  
    ( (with-self  True 
        (arg $N $Csym $L)) 
      (asm-merge $L $LCsym0 $LCsym1) 
      (is $N1 
        (- $N 1)) 
      (asm-rearange-csym $Csym $N1 $LCsym1 $LCsym)))


  (= (asm-merge $L1 $L2 $L3)  
    (with-self  
      (var $L1) 
      (= $L3 $L2)))
  (= (asm-merge $L1 $L2 $L3)  
    (with-self  
      (var $L2) 
      (= $L3 $L1)))
  (= (asm-merge $L1 $L2 $L3)  
    ( (<= 
        (Cons  $Sym1 $T1) $L1) 
      (<= 
        (Cons  $Sym2 $T2) $L2) 
      (<= 
        (sym $F1 $N1 $Val1 $I1) $Sym1) 
      (<= 
        (sym $F2 $N2 $Val2 $I2) $Sym2) 
      (with-self  
        (< $I1 $I2) 
        (:= $L3 
          (Cons  $Sym1 $L4))) 
      (asm-merge $T1 $L2 $L4)))
  (= (asm-merge $L1 $L2 $L3)  
    ( (<= 
        (Cons  $Sym1 $T1) $L1) 
      (with-self  
        (<= 
          (Cons  $Sym2 $T2) $L2) 
        (:= $L3 
          (Cons  $Sym2 $L4))) 
      (asm-merge $L1 $T2 $L4)))

;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (= (next-sym-no $I)  
    ( (with-self  True 
        (global-get %sym-no 0 $I)) 
      (is $I1 
        (+ $I 1)) 
      (global-set %sym-no 0 $I1)))
; /* asm_hash_value(F,N,HashVal):- true : name(F,L), asm_sum_list(L,0,Sum), HashVal is Sum+N. asm_hash_value((F,N,No),HashVal):- true : name(F,L), asm_sum_list(L,0,Sum), HashVal is Sum+N+No. asm_hash_value((F,N),HashVal):- true : name(F,L), asm_sum_list(L,0,Sum), HashVal is Sum+N. asm_sum_list([],Sum0,Sum):- true : Sum=Sum0. asm_sum_list([X|Xs],Sum0,Sum):- true : Sum1 is Sum0+X, asm_sum_list(Xs,Sum1,Sum). */


  (= (x-or-y (vx $X) $Code)  
    (with-self  True 
      (is $Code 
        (<< $X 3))))
; ; xv
  (= (x-or-y (x $X) $Code)  
    (with-self  True 
      (is $Code 
        (+ 
          (<< $X 3) 1))))
; ; ux
  (= (x-or-y (vy $Y) $Code)  
    (with-self  True 
      (is $Code 
        (\/ 
          (<< $Y 3) 3))))
; ;vy
  (= (x-or-y (y $Y) $Code)  
    (with-self  True 
      (is $Code 
        (\/ 
          (<< $Y 3) 7))))
  (= (x-or-y $Op $Code)  
    (with-self  True 
      (is $Code 
        (\/ 
          (<< $Op 2) 2))))


  !(mode (opcode c f))

  (= (opcode  noop 0)  True)
  (= (opcode  jmp 1)  True)
  (= (opcode  djmp 2)  True)
  (= (opcode  jmpn_eq_struct_x 3)  True)
  (= (opcode  jmpn_eq_struct_y 4)  True)
  (= (opcode  jmpn_eq_atom_x 5)  True)
  (= (opcode  jmpn_eq_atom_y 6)  True)
  (= (opcode  jmpn_nil_x 7)  True)
  (= (opcode  jmpn_nil_y 8)  True)
  (= (opcode  switch_list_x 9)  True)
  (= (opcode  switch_list_y 10)  True)
  (= (opcode  switch_list_yxx 11)  True)
  (= (opcode  switch_list_yxy 12)  True)
  (= (opcode  switch_list_yyx 13)  True)
  (= (opcode  switch_list_yyy 14)  True)
  (= (opcode  jmpn_eq_int_x 15)  True)
  (= (opcode  jmpn_eq_int_y 16)  True)
  (= (opcode  jmpn_eql 17)  True)
  (= (opcode  jmp_eql 18)  True)
  (= (opcode  jmp_eql_yy 19)  True)
  (= (opcode  jmpn_gt 20)  True)
  (= (opcode  jmpn_gt_yy 21)  True)
  (= (opcode  jmpn_ge 22)  True)
  (= (opcode  jmpn_ge_yy 23)  True)
  (= (opcode  jmpn_id 24)  True)
  (= (opcode  jmp_id 25)  True)
  (= (opcode  jmpn_var_x 26)  True)
  (= (opcode  jmpn_var_y 27)  True)
  (= (opcode  jmp_var_x 28)  True)
  (= (opcode  jmp_var_y 29)  True)
  (= (opcode  jmpn_atom_x 30)  True)
  (= (opcode  jmpn_atom_y 31)  True)
  (= (opcode  jmpn_atomic_x 32)  True)
  (= (opcode  jmpn_atomic_y 33)  True)
  (= (opcode  jmpn_num_x 34)  True)
  (= (opcode  jmpn_num_y 35)  True)
  (= (opcode  jmpn_float_x 36)  True)
  (= (opcode  jmpn_float_y 37)  True)
  (= (opcode  jmpn_int_x 38)  True)
  (= (opcode  jmpn_int_y 39)  True)
  (= (opcode  hash_jmpn_nil 40)  True)
  (= (opcode  hash_jmpn_list 41)  True)
  (= (opcode  hash_jmpn_int 42)  True)
  (= (opcode  hash_jmpn_atom 43)  True)
  (= (opcode  hash_jmpn_struct 44)  True)
  (= (opcode  hash_jmpn_struct_x 45)  True)
  (= (opcode  hash_jmpn_struct_y 46)  True)
  (= (opcode  hash_jmpn_struct_xx 47)  True)
  (= (opcode  hash_jmpn_struct_xy 48)  True)
  (= (opcode  hash_jmpn_struct_yx 49)  True)
  (= (opcode  hash_jmpn_struct_yy 50)  True)
  (= (opcode  unify_struct_x 51)  True)
  (= (opcode  unify_struct_y 52)  True)
  (= (opcode  unify_list_x 53)  True)
  (= (opcode  unify_list_y 54)  True)
  (= (opcode  unify_nil_x 55)  True)
  (= (opcode  unify_nil_y 56)  True)
  (= (opcode  unify_atom_x 57)  True)
  (= (opcode  unify_int_x 58)  True)
  (= (opcode  unify_atom_y 59)  True)
  (= (opcode  unify_int_y 60)  True)
  (= (opcode  unify_ux_ux 61)  True)
  (= (opcode  unify_ux_uy 62)  True)
  (= (opcode  unify_uy_uy 63)  True)
  (= (opcode  unify_cons_x 64)  True)
  (= (opcode  unify_cons_y 65)  True)
  (= (opcode  fork_unify_struct_y 66)  True)
  (= (opcode  fork_unify_list_y 67)  True)
  (= (opcode  fork_unify_nil_y 68)  True)
  (= (opcode  fork_unify_atom_y 69)  True)
  (= (opcode  fork_unify_int_y 70)  True)
  (= (opcode  fork_unify_uy_uy 71)  True)
  (= (opcode  fork_unicut_struct_y 72)  True)
  (= (opcode  fork_unicut_list_y 73)  True)
  (= (opcode  fork_unicut_nil_y 74)  True)
  (= (opcode  fork_unicut_atom_y 75)  True)
  (= (opcode  fork_unicut_int_y 76)  True)
  (= (opcode  fork_unicut_uy_uy 77)  True)
  (= (opcode  unify0_struct_y 78)  True)
  (= (opcode  unify0_list_y 79)  True)
  (= (opcode  unify0_nil_y 80)  True)
  (= (opcode  unify0_atom_y 81)  True)
  (= (opcode  unify0_int_y 82)  True)
  (= (opcode  unify0_uy_uy 83)  True)
  (= (opcode  unicut_struct_y 84)  True)
  (= (opcode  unicut_list_y 85)  True)
  (= (opcode  unicut_nil_y 86)  True)
  (= (opcode  unicut_atom_y 87)  True)
  (= (opcode  unicut_int_y 88)  True)
  (= (opcode  unicut_uy_uy 89)  True)
  (= (opcode  unicut 90)  True)
  (= (opcode  unify_arg_nil 91)  True)
  (= (opcode  unify_arg_atom 92)  True)
  (= (opcode  unify_arg_int 93)  True)
  (= (opcode  unify_arg_ux_ux 94)  True)
  (= (opcode  unify_arg_ux 95)  True)
  (= (opcode  unify_arg_ux_vx 96)  True)
  (= (opcode  unify_arg_ux_vy 97)  True)
  (= (opcode  unify_arg_uy_uy 98)  True)
  (= (opcode  unify_arg_uy 99)  True)
  (= (opcode  unify_arg_wy 100)  True)
  (= (opcode  unify_arg_vx_vx 101)  True)
  (= (opcode  unify_arg_vx 102)  True)
  (= (opcode  unify_arg_vy_vy 103)  True)
  (= (opcode  unify_arg_vy 104)  True)
  (= (opcode  unify_arg_list 105)  True)
  (= (opcode  unify_arg_struct 106)  True)
  (= (opcode  unify_arg_void_one 107)  True)
  (= (opcode  unify_arg_void 108)  True)
  (= (opcode  unify_arg_vx_vy 109)  True)
  (= (opcode  unify_arg_vx_ux 110)  True)
  (= (opcode  unify_arg_vx_uy 111)  True)
  (= (opcode  unify_arg_vy_vx 112)  True)
  (= (opcode  unify_arg_vy_ux 113)  True)
  (= (opcode  unify_arg_vy_uy 114)  True)
  (= (opcode  unify_arg_iii 115)  True)
  (= (opcode  move_struct_x 116)  True)
  (= (opcode  move_struct_y 117)  True)
  (= (opcode  move_list_x 118)  True)
  (= (opcode  move_list_y 119)  True)
  (= (opcode  move_nil_x 120)  True)
  (= (opcode  move_nil_y 121)  True)
  (= (opcode  move_atom_x 122)  True)
  (= (opcode  move_int_x 123)  True)
  (= (opcode  move_atom_y 124)  True)
  (= (opcode  move_int_y 125)  True)
  (= (opcode  move_x_ux 126)  True)
  (= (opcode  move_x_uy 127)  True)
  (= (opcode  move_y_ux 128)  True)
  (= (opcode  move_yy_yy_yy 129)  True)
  (= (opcode  move_yy_yy 130)  True)
  (= (opcode  move_y_uy 131)  True)
  (= (opcode  move_vx 132)  True)
  (= (opcode  move_vy 133)  True)
  (= (opcode  move_x_wy 134)  True)
  (= (opcode  move_yy_yw 135)  True)
  (= (opcode  move_y_wy 136)  True)
  (= (opcode  move_yw_yy 137)  True)
  (= (opcode  and 138)  True)
  (= (opcode  or 139)  True)
  (= (opcode  lshiftl 140)  True)
  (= (opcode  lshiftr 141)  True)
  (= (opcode  complement 142)  True)
  (= (opcode  add 143)  True)
  (= (opcode  add1_y 144)  True)
  (= (opcode  sub 145)  True)
  (= (opcode  sub1_y 146)  True)
  (= (opcode  mul 147)  True)
  (= (opcode  div 148)  True)
  (= (opcode  idiv 149)  True)
  (= (opcode  mod 150)  True)
  (= (opcode  para_struct 151)  True)
  (= (opcode  para_list 152)  True)
  (= (opcode  para_nil 153)  True)
  (= (opcode  para_atom 154)  True)
  (= (opcode  para_int 155)  True)
  (= (opcode  para_ux_ux_ux 156)  True)
  (= (opcode  para_ux_ux 157)  True)
  (= (opcode  para_ux 158)  True)
  (= (opcode  para_uy_uy_uy_uy 159)  True)
  (= (opcode  para_uy_uy_uy 160)  True)
  (= (opcode  para_uy_uy 161)  True)
  (= (opcode  para_uy 162)  True)
  (= (opcode  para_vx 163)  True)
  (= (opcode  para_vy_vy 164)  True)
  (= (opcode  para_vy 165)  True)
  (= (opcode  para_void_one 166)  True)
  (= (opcode  para_void 167)  True)
  (= (opcode  para_vy_ux 168)  True)
  (= (opcode  para_vy_uy 169)  True)
  (= (opcode  para_ux_vy 170)  True)
  (= (opcode  para_ux_ux_uy 171)  True)
  (= (opcode  para_ux_uy 172)  True)
  (= (opcode  para_ux_uy_uy 173)  True)
  (= (opcode  para_uy_vy 174)  True)
  (= (opcode  para_ux_uy_ux 175)  True)
  (= (opcode  para_uy_ux 176)  True)
  (= (opcode  para_uy_ux_ux 177)  True)
  (= (opcode  para_uy_ux_uy 178)  True)
  (= (opcode  para_uy_uy_ux 179)  True)
  (= (opcode  call 180)  True)
  (= (opcode  call_d 181)  True)
  (= (opcode  callv 182)  True)
  (= (opcode  execute 183)  True)
  (= (opcode  executev 184)  True)
  (= (opcode  return_a 185)  True)
  (= (opcode  return_b 186)  True)
  (= (opcode  jmpn_det 187)  True)
  (= (opcode  save_ht_jmp 188)  True)
  (= (opcode  allocate_flat 189)  True)
  (= (opcode  allocate_nonflat 190)  True)
  (= (opcode  allocate_nondet 191)  True)
  (= (opcode  fail 192)  True)
  (= (opcode  fail0 193)  True)
  (= (opcode  fork 194)  True)
  (= (opcode  commit 195)  True)
  (= (opcode  cut 196)  True)
  (= (opcode  cut_fail 197)  True)
  (= (opcode  cut_return 198)  True)
  (= (opcode  save_b 199)  True)
  (= (opcode  getbreg 200)  True)
  (= (opcode  putbreg 201)  True)
  (= (opcode  getpbreg 202)  True)
  (= (opcode  hash_x 203)  True)
  (= (opcode  hash_y 204)  True)
  (= (opcode  arg 205)  True)
  (= (opcode  arg0 206)  True)
  (= (opcode  setarg 207)  True)
  (= (opcode  setarg0 208)  True)
  (= (opcode  functor 209)  True)
  (= (opcode  func_arity 210)  True)
  (= (opcode  get_ar_cps 211)  True)
  (= (opcode  put_ar_cps 212)  True)
  (= (opcode  move_ar_cps 213)  True)
  (= (opcode  jmpn_det_get_ar_cps 214)  True)
  (= (opcode  builtin0 215)  True)
  (= (opcode  builtin1 216)  True)
  (= (opcode  builtin2 217)  True)
  (= (opcode  builtin3 218)  True)
  (= (opcode  builtin4 219)  True)
  (= (opcode  allocate_susp 220)  True)
  (= (opcode  susp_var_x 221)  True)
  (= (opcode  susp_var_y 222)  True)
  (= (opcode  delay 223)  True)
  (= (opcode  susp_var_delay 224)  True)
  (= (opcode  end_delay 225)  True)
  (= (opcode  nondet 226)  True)
  (= (opcode  jmp_susp 227)  True)
  (= (opcode  jmpn_dvar_y 228)  True)
  (= (opcode  susp_var2_delay 229)  True)
  (= (opcode  domain_set_false_yy 230)  True)
  (= (opcode  domain_set_false_yx 231)  True)
  (= (opcode  halt 232)  True)
  (= (opcode  endfile 233)  True)
  (= (opcode  tabsize 234)  True)


  (= (output-mess $Mess)  
    ( (with-self  True 
        (telling $X)) 
      (tell user) 
      (write $Mess) nl 
      (tell $X)))
; /* opcode(X,N):- true : cmp_error(['no this instruction : ',X]). */


  (= (is-unify-arg-inst $Inst)  
    ( (with-self  True 
        (functor $Inst $F $N)) 
      (opcode $F $Code) 
      (>= $Code 91) 
      (=< $Code 115)))


