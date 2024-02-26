;                                           (convert-to-metta-file  vs $_263494 vs/vs.pl vs/vs.metta)

  (= learn  
    ( (writeln 'First positive example ?') 
      (read $POS_EX) nl 
      (initialize $POS_EX $G $S) 
      (versionspace $G $S)))
; /******************************************************************/
; /* VS.PRO Last Modification: Fri Jan 14 19:28:27 1994 */
; /* Mitchell's bi-directional search strategy in the version space */
; /******************************************************************/
; ; ; Copyright (c) 1988 Luc De Raedt ; ; This program is free software; you can redistribute it and/or ; modify it under the terms of the GNU General Public License ; Version 1 as published by the Free Software Foundation. ; ; This program is distributed in the hope that it will be useful, ; but WITHOUT ANY WARRANTY; without even the implied warranty of ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the ; GNU General Public License for more details. ; ; You should have received a copy of the GNU General Public ; Licensealong with this program; if not, write to the Free ; SoftwareFoundation, Inc., 675 Mass Ave, Cambridge, MA 02139, ; USA. ;
; /******************************************************************/
; /* impl. by : Luc De Raedt, Katholieke Universiteit Leuven, */
; /* Department of Computer Science, */
; /* Celestijnenlaan 200A, */
; /* B-3030 Heverlee, */
; /* Belgium */
; /* E-Mail: lucdr@kulcs.uucp or lucdr@kulcs.bitnet */
; /* 1988 */
; /* */
; /* reference : ES2ML Tutorial Exercise */
; /* Version Space Algorithm */
; /* Luc De Raedt */
; /* */
; /* Generalization as Search, */
; /* Tom M. Mitchell, */
; /* Artificial Intelligence 18, 1982. */
; /* */
; /* call : learn */
; /* */
; /******************************************************************/
; /* THIS VERSION LEARNS CONJUNCTIONS AND CONSTRUCTS EXAMPLES */
; /* FOR A SIMPLE ATTRIBUTE-VALUED GENERALISATION LANGUAGE */
; /* */
; /* LEARN will learn a concept, it needs only one example it will */
; /* ask for the classification of further examples, which it will */
; /* construct itself. The first example must be a list of */
; /* attributes. */
; /******************************************************************/
 

  (= (versionspace Nil $_)  
    (writeln 'There is no consistent concept description in this language !')
    (set-det))
; /******************************************************************/
; /* */
; /* call : versionspace (+G_SET,+S_SET) */
; /* */
; /* arguments : G_SET = Set of most general concepts */
; /* S_SET = Set of most special concepts */
; /* */
; /* side effects: terminal-I/O */
; /* */
; /******************************************************************/
; /* VERSIONSPACE succeeds if there is a consistent concept, which */
; /* is in the versionspace between g and s it will ask for the */
; /* classification of examples, which it generates. */
; /******************************************************************/
  (= (versionspace $_ Nil)  
    (writeln 'There is no consistent concept description in this language !')
    (set-det))
  (= (versionspace (:: $CONCEPT) (:: $CONCEPT))  
    (set-det)
    (writeln 'The consistent generalization is : ')
    (writeln $CONCEPT))
  (= (versionspace $G $S)  
    ( (writeln 'The G-set is : ') 
      (writeln $G) 
      (writeln 'The S-set is : ') 
      (writeln $S) nl 
      (writeln 'Next example :') 
      (generate-ex $G $S $NEXT_EX) 
      (set-det) 
      (writeln $NEXT_EX) 
      (writeln 'Classification of the example ? [p/n]') 
      (read $CLASS) nl 
      (adjust-versionspace $CLASS $NEXT_EX $G $S $NG $NS) 
      (versionspace $NG $NS)))
  (= (versionspace $G $S)  
    (writeln 'Impossible to generate relevant examples'))
  

  (= (adjust-versionspace p $EX $G $S $NG $NS)  
    (retain-g $G $NG $EX)
    (generalize-s $S $S1 $NG $EX)
    (prune-s $S1 $NS))
; /******************************************************************/
; /* */
; /* call : adjust_versionspace(+CLASSIFICATION, */
; /* +EXAMPLE, */
; /* +G_SET, */
; /* +S_SET, */
; /* -UPDATED_S_SET, */
; /* -UPDATED_G_SET) */
; /* */
; /* arguments : CLASSIFICATION = of the example p for positive */
; /* or n for negative */
; /* EXAMPLE = the example itself */
; /* G_SET = the actual G-set */
; /* S_SET = the actual S-set */
; /* UPDATED_G_SET = the updated G-set */
; /* UPDATED_S_SET = the updated S-set */
; /* */
; /******************************************************************/
; /* ADJUST_VERSIONSPACE succeeds if UPDATED_G_SET and UPDATED_S_SET*/
; /* specify the updated versionspace of G_SET and S_SET wrt EXAMPLE*/
; /* and CLASSIFICATION. */
; /******************************************************************/
  (= (adjust-versionspace n $EX $G $S $NG $NS)  
    (retain-s $S $NS $EX)
    (specialize-g $G $G1 $NS $EX)
    (prune-g $G1 $NG))
 

  (= (retain-g Nil Nil $_)  
    (set-det))
; /******************************************************************/
; /* */
; /* call : retain_g(+G_SET,-UPDATED_G_SET,+EXAMPLE) */
; /* */
; /* arguments : G_SET = the actual G-set */
; /* UPDATED_G_SET = the updated G-set */
; /* EXAMPLE = the example itself */
; /* */
; /******************************************************************/
; /* RETAIN_G succeeds if UPDATED_G_SET lists the elements of G_SET */
; /* which cover the EXAMPLE */
; /******************************************************************/
  (= (retain-g (Cons  $CONCEPT $G) (Cons  $CONCEPT $NG) $EX)  
    (covers $CONCEPT $EX)
    (set-det)
    (retain-g $G $NG $EX))
  (= (retain-g (Cons  $CONCEPT $G) $NG $EX)  
    (retain-g $G $NG $EX))
 

  (= (retain-s Nil Nil $_)  
    (set-det))
; /******************************************************************/
; /* */
; /* call : retain_s(+S_SET,-UPDATED_S_SET,+EXAMPLE) */
; /* */
; /* arguments : S_SET = the actual S-set */
; /* UPDATED_S_SET = the updated S-set */
; /* EXAMPLE = the example itself */
; /* */
; /******************************************************************/
; /* RETAIN_S succeeds if UPDATED_S_SET lists the elements of S_SET */
; /* which do not cover the EXAMPLE */
; /******************************************************************/
;                                           (error
;                                             (syntax-error  operator_expected)
;                                             (file  vs/vs.pl 164 11 8582))

  (= (retain-s (Cons  $CONCEPT $S) $NS $EX)  
    (retain-s $S $NS $EX))
 

  (= (generalize-s $S $NS $NG $EX)  
    (setofnil $NCON 
      (^ $CON 
        (, 
          (member $CON $S) 
          (valid-least-generalization $CON $EX $NCON $NG))) $NS))
; /******************************************************************/
; /* */
; /* call : generalize_s(+S_SET,-UPDATED_S_SET, */
; /* +G_SET,+EXAMPLE) */
; /* */
; /* arguments : S_SET = the actual S-set */
; /* UPDATED_S_SET = the updated S-set */
; /* G_SET = the actual G-set */
; /* EXAMPLE = the example itself */
; /* */
; /******************************************************************/
; /* GENERALIZE_S succeeds if UPDATED_S_SET lists the minimal */
; /* generalizations of the elements in S_SET wrt EXAMPLE such that */
; /* there is an element in G_SET which is more general. */
; /******************************************************************/
 

  (= (specialize-g $G $NG $NS $EX)  
    (setofnil $NCONCEPT 
      (^ $CONCEPT 
        (, 
          (member $CONCEPT $G) 
          (valid-greatest-specialization $CONCEPT $EX $NCONCEPT $NS))) $NG))
; /******************************************************************/
; /* */
; /* call : generalize_g(+G_SET,-UPDATED_G_SET, */
; /* +S_SET,+EXAMPLE) */
; /* */
; /* arguments : G_SET = the actual G-set */
; /* UPDATED_G_SET = the updated G-set */
; /* S_SET = the actual S-set */
; /* EXAMPLE = the example itself */
; /* */
; /******************************************************************/
; /* GENERALIZE_G succeeds if UPDATED_G_SET lists the greatest */
; /* specializations of the elements in G_SET wrt EXAMPLE such that */
; /* there is an element in S_SET which is more specific. */
; /******************************************************************/
 

  (= (valid-least-generalization $CONCEPT $EX $NCONCEPT $NG)  
    (least-generalization $CONCEPT $EX $NCONCEPT)
    (member $GENERAL $NG)
    (more-general $GENERAL $NCONCEPT))
; /******************************************************************/
; /* */
; /* call : valid_least_generalization(+CONCEPT,+EXAMPLE, */
; /* -GENERALIZATION, */
; /* +G_SET) */
; /* */
; /* arguments : CONCEPT = concept description */
; /* EXAMPLE = the example itself */
; /* GENERALIZATION = a new generalization */
; /* G_SET = the actual G-set */
; /* */
; /******************************************************************/
; /* VALID_LEAST_GENERALIZATION succeeds if GENERALIZATION is a */
; /* least generalization of EXAMPLE and CONCEPT such that there is */
; /* an element in G_SET which is more general than GENERALIZATION */
; /******************************************************************/
 

  (= (valid-greatest-specialization $CONCEPT $EX $NCONCEPT $NS)  
    (greatest-specialization $CONCEPT $EX $NCONCEPT)
    (member $SPECIFIC $NS)
    (more-general $NCONCEPT $SPECIFIC))
; /******************************************************************/
; /* */
; /* call : valid_greatest_specialization(+CONCEPT,+EXAMPLE,*/
; /* -SPECIALIZATION, */
; /* +S_SET) */
; /* */
; /* arguments : CONCEPT = concept description */
; /* EXAMPLE = the example itself */
; /* SPECIALIZATION = a new specialization */
; /* S_SET = the actual S-set */
; /* */
; /******************************************************************/
; /* VALID_GREATEST_SPECIALIZATION succeeds if SPECIALIZATION is a */
; /* greatest specialization of CONCEPT wrt EXAMPLE such that there */
; /* is an element in S_SET which is more specific than */
; /* SPECIALIZATION */
; /******************************************************************/


  (= (prune-s $S $NS)  
    (prune-s-acc $S $S $NS))
; /******************************************************************/
; /* */
; /* call : prune_s(+S_SET,-PRUNED_S_SET) */
; /* */
; /* arguments : S_SET = the actual S-set */
; /* PRUNED_S_SET = the pruned S-set */
; /* */
; /******************************************************************/
; /* PRUNE_S succeeds if PRUNED_S_SET is the set of non-redundant */
; /* elements in S_SET. An element is non-redundant if there is no */
; /* element in S_SET which is more specific. PRUNE_S using an */
; /* accumulating parameter to store intermediate results. */
; /******************************************************************/

  (= (prune-s-acc Nil $_ Nil)  
    (set-det))
  (= (prune-s-acc (Cons  $SPECIFIC $S) $ACC $NS)  
    (member $SPECIFIC1 $ACC)
    (not (== $SPECIFIC1 $SPECIFIC))
    (more-general $SPECIFIC $SPECIFIC1)
    (set-det)
    (prune-s-acc $S $ACC $NS))
  (= (prune-s-acc (Cons  $SPECIFIC $S) $ACC (Cons  $SPECIFIC $NS))  
    (prune-s-acc $S $ACC $NS))
 

  (= (prune-g $G $NG)  
    (prune-g-acc $G $G $NG))
; /******************************************************************/
; /* */
; /* call : prune_s(+G_SET,-PRUNED_G_SET) */
; /* */
; /* arguments : G_SET = the actual G-set */
; /* PRUNED_G_SET = the pruned G-set */
; /* */
; /******************************************************************/
; /* PRUNE_G succeeds if PRUNED_G_SET is the set of non-redundant */
; /* elements in G_SET an element is non-redundant if there is no */
; /* element in G_SET which is more general. PRUNE_G using an */
; /* accumulating parameter to store intermediate results. */
; /******************************************************************/

  (= (prune-g-acc Nil $_ Nil)  
    (set-det))
  (= (prune-g-acc (Cons  $GENERAL $G) $ACC $NG)  
    (member $GENERAL1 $ACC)
    (not (== $GENERAL $GENERAL1))
    (more-general $GENERAL1 $GENERAL)
    (set-det)
    (prune-g-acc $G $ACC $NG))
  (= (prune-g-acc (Cons  $GENERAL $G) $ACC (Cons  $GENERAL $NG))  
    (prune-g-acc $G $ACC $NG))
 

  (= (allcovers Nil $_)  
    (set-det))
; /******************************************************************/
; /* GENERATION OF EXAMPLES */
; /******************************************************************/
; /* */
; /* call : allcovers(+CONCEPT_LIST,+EXAMPLE) */
; /* */
; /* arguments : CONCEPT_LIST = list of concepts */
; /* EXAMPLE = the actual example */
; /* */
; /******************************************************************/
; /* ALLCOVERS succeeds if all elements of CONCEPT_LIST cover */
; /* EXAMPLE */
; /******************************************************************/
  (= (allcovers (Cons  $CON $REST) $EX)  
    (covers $CON $EX)
    (allcovers $REST $EX))
 
;                                           (error
;                                             (syntax-error  operator_expected)
;                                             (file  vs/vs.pl 334 11 18121))


  (= (generate-ex (Cons  $GENERAL $G) $S $EX)  
    (generate-ex $G $S $EX))
 

  (= (find-ex Nil Nil)  
    (set-det))
; /******************************************************************/
; /* */
; /* call : find_ex(+CONCEPT,+EXAMPLE) */
; /* */
; /* arguments : CONCEPT = general concept */
; /* EXAMPLE = the actual example */
; /* */
; /******************************************************************/
; /* FIND_EX succeeds if EXAMPLE is an example in the language of */
; /* the versionspace such that it is covered by the element in */
; /* EXAMPLE. */
; /******************************************************************/
  (= (find-ex (Cons  $GENERAL $G) (Cons  $LEAF $EX))  
    (isa $LEAF $GENERAL)
    (leaf $LEAF)
    (find-ex $G $EX))
 

  (= (initialize $POS_EX (:: $TOP) (:: $POS_EX))  
    (max $TOP $POS_EX))
; /******************************************************************/
; /* LANGUAGE DEPENDENT PREDICATES */
; /******************************************************************/
; /* */
; /* call : initialize(+EXAMPLE,-G_SET,-S_SET) */
; /* */
; /* arguments : EXAMPLE = the positive example */
; /* G_SET = initial G-set */
; /* S_SET = initial S-set */
; /* */
; /******************************************************************/
; /* INITIALIZE succeeds if G_SET is the g-set and S_SET is the */
; /* s-set derived from the positive example */
; /******************************************************************/
 

  (= (covers  () ())  True)
; /******************************************************************/
; /* */
; /* call : covers(+CONCEPT,+EXAMPLE) */
; /* */
; /* arguments : CONCEPT = concept description */
; /* EXAMPLE = example */
; /* */
; /******************************************************************/
; /* COVERS succeeds if CONCEPT covers EXAMPLE */
; /******************************************************************/
  (= (covers (Cons  $C $CONCEPT) (Cons  $E $EXAMPLE))  
    (isa $E $C)
    (covers $CONCEPT $EXAMPLE))
 

  (= (least-generalization  () () ())  True)
; /******************************************************************/
; /* */
; /* call : least_generalization(+CONCEPT1,+EXAMPLE, */
; /* -CONCEPT2) */
; /* */
; /* arguments : CONCEPT1 = concept description */
; /* EXAMPLE = example */
; /* CONCEPT2 = concept description */
; /* */
; /******************************************************************/
; /* LEAST_GENERALIZATION succeeds if CONCEPT2 is the least */
; /* generalization of CONCEPT1 and CONCEPT2 */
; /******************************************************************/
  (= (least-generalization (Cons  $CONCEPT $C) (Cons  $EX $E) (Cons  $NCONCEPT $N))  
    (lge $CONCEPT $EX $NCONCEPT)
    (least-generalization $C $E $N))
 

  (= (greatest-specialization (Cons  $CONCEPT $C) (Cons  $EX $E) (Cons  $NCONCEPT $C))  
    (gsp $CONCEPT $EX $NCONCEPT))
; /******************************************************************/
; /* */
; /* call : greatest_specialization(+CONCEPT1,+EXAMPLE, */
; /* -CONCEPT2) */
; /* */
; /* arguments : CONCEPT1 = concept description */
; /* EXAMPLE = example */
; /* CONCEPT2 = concept description */
; /* */
; /******************************************************************/
; /* GREATEST_SPECIALIZATION succeeds if CONCEPT2 is the greatest */
; /* specialization of CONCEPT1 and CONCEPT2 */
; /******************************************************************/
  (= (greatest-specialization (Cons  $CONCEPT $C) (Cons  $EX $E) (Cons  $CONCEPT $N))  
    (greatest-specialization $C $E $N))


  (= (more-general $CONCEPT1 $CONCEPT2)  
    (covers $CONCEPT1 $CONCEPT2))
; /******************************************************************/
; /* */
; /* call : more_general(+CONCEPT1,+CONCEPT2) */
; /* */
; /* arguments : CONCEPT1 = concept description */
; /* CONCEPT2 = concept description */
; /* */
; /******************************************************************/
; /* MORE_GENERAL succeeds if CONCEPT1 is more general than CONCEPT2*/
; /******************************************************************/
 

  (= (max Nil Nil)  
    (set-det))
; /******************************************************************/
; /* APPLICATION DEPENDENT PREDICATES */
; /******************************************************************/
; /* */
; /* call : max(+CONCEPT,+EXAMPLE) */
; /* */
; /* arguments : CONCEPT = concept description */
; /* EXAMPLE = example */
; /* */
; /******************************************************************/
; /* MAX succeeds if CONCEPT is a most general concept description */
; /* which covers EXAMPLE */
; /******************************************************************/
  (= (max (Cons  $TOP $T) (Cons  $EX $E))  
    (top $TOP $EX)
    (max $T $E))
 
;                                           (error
;                                             (syntax-error  operator_expected)
;                                             (file  vs/vs.pl 468 11 25683))

 
;                                           (error
;                                             (syntax-error  operator_expected)
;                                             (file  vs/vs.pl 480 12 26336))



  (= (isa  $X $X)  True)
; /******************************************************************/
; /* */
; /* call : isa(X,Y) */
; /* */
; /* arguments : X = knot of a taxonomy */
; /* Y = knot of a taxonomy */
; /* */
; /******************************************************************/
; /* Inheritance */
; /******************************************************************/
  (= (isa $X $Y)  
    (son $X $Z)
    (isa $Z $Y))
 

  (= (lge $X1 $X2 $X1)  
    (isa $X2 $X1)
    (set-det))
; /******************************************************************/
; /* */
; /* call : lge(X,Y,Z) */
; /* */
; /* arguments : X = knot of a taxonomy */
; /* Y = knot of a taxonomy */
; /* Z = knot of a taxonomy */
; /* */
; /******************************************************************/
; /* LGE succeeds if Z is least generalization of X and Y in a */
; /* taxonomy. */
; /******************************************************************/
 
  (= (lge $X1 $X2 $L)  
    (son $X1 $F)
    (lge $F $X2 $L))
 
;                                           (error
;                                             (syntax-error  operator_expected)
;                                             (file  vs/vs.pl 530 11 28892))

 

  (= (gsp $X1 $X2 $G)  
    (son $S $X1)
    (gsp $S $X2 $G))
 

  (= (writeln $X)  
    ( (display $X) nl))
; /******************************************************************/
; /* UTILITIES : comments are trivial */
; /******************************************************************/
 

  (= (member  $X ([|]  $X $Y))  True)
  (= (member $X (Cons  $Y $Z))  
    (member $X $Z))
 

  (= (append  () $X $X)  True)
  (= (append (Cons  $X $Y) $Z (Cons  $X $W))  
    (append $Y $Z $W))
 

  (= (setofnil $X $Y $Z)  
    (setof $X $Y $Z)
    (set-det))
; /******************************************************************/
; /* */
; /* call : setofnil(-X,+Y,-Z) */
; /* */
; /* arguments : X = variable */
; /* Y = relational expression */
; /* Z = list */
; /* */
; /******************************************************************/
; /* SETOFNIL succeeds if Z lists all possible instantiations of X */
; /* for which Y is true. A^B means existential quantification. */
; /******************************************************************/
  (= (setofnil  $X $Y ())  True)
 

  (= help  
    ( (write ' Start VS with command: learn.') nl))


  !(help *)

