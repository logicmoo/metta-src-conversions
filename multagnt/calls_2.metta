;                                           (convert-to-metta-file  calls_2 $_51376 multagnt/calls_2.pl multagnt/calls_2.metta)
  (= (?-  (teacher))  True)
  (= (?-  (learner2))  True)

  (= (?-  (cannot-do  $Tl $Tt (parent  warren catherine) $C))  True)
  (= (?-  (cannot-do  $Tl $Tt (parent  warren david) $C))  True)
  (= (?-  (cannot-do  $Tl $Tt (parent  warren (variable  child)) $C))  True)
  (= (?-  (can-do  $Tl $Tt (parent  (variable  parent) (variable  child)) $C))  True)

  (= (?-  (cannot-do  $Tl $Tt (entropy-increases  (variable  a) (variable  b)) $C))  True)

  (= (?-  (cannot-do  $Tl $Tt (t-member  f (g f d s a)) $C))  True)
  (= (?-  (cannot-do  $Tl $Tt (t-member  e (q w e r t z t r e w q)) $C))  True)
  (= (?-  (cannot-do  $Tl $Tt (t-member  (variable  a) (g f d s a)) $C))  True)
  (= (?-  (can-do  $Tl $Tt (t-member  (variable  a) (variable  list)) $C))  True)

  (= (?-  (cannot-do  $Tl $Tt (reverse  (c b a) (variable  c)) $C))  True)
  (= (?-  (cannot-do  $Tl $Tt (reverse  (a b c) (c b a)) $C))  True)

  (= (?-  (what-cannot-do  $Ls $Ts (<-  $Q $Ans) () $F))  True)
  (= (?-  (what-cannot-do  $Ls $Ts (<-  (qsort  (1 2) (variable  a)) $Ans) () $F))  True)
  (= (?-  (what-cannot-do  $Ls $Ts (<-  (qsort  (2 1) (variable  a)) $Ans) () $F))  True)
  (= (?-  (what-cannot-do  $Ls $Ts (<-  (qsort  (2 3 1) (variable  a)) $Ans) () $F))  True)
  (= (?-  (what-cannot-do  $Ls $Ts (<-  (qsort  (3 2 1) (variable  a)) $Ans) () $F))  True)

