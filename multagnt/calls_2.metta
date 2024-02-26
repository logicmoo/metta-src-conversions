;                                           (convert_to_metta_file  calls_2 $_478044 multagnt/calls_2.pl multagnt/calls_2.metta)
  (= (?-  (teacher))  True)
  (= (?-  (learner2))  True)

  (= (?-  (cannot_do  $Tl $Tt (parent  warren catherine) $C))  True)
  (= (?-  (cannot_do  $Tl $Tt (parent  warren david) $C))  True)
  (= (?-  (cannot_do  $Tl $Tt (parent  warren (variable  child)) $C))  True)
  (= (?-  (can_do  $Tl $Tt (parent  (variable  parent) (variable  child)) $C))  True)

  (= (?-  (cannot_do  $Tl $Tt (entropy_increases  (variable  a) (variable  b)) $C))  True)

  (= (?-  (cannot_do  $Tl $Tt (t_member  f (g f d s a)) $C))  True)
  (= (?-  (cannot_do  $Tl $Tt (t_member  e (q w e r t z t r e w q)) $C))  True)
  (= (?-  (cannot_do  $Tl $Tt (t_member  (variable  a) (g f d s a)) $C))  True)
  (= (?-  (can_do  $Tl $Tt (t_member  (variable  a) (variable  list)) $C))  True)

  (= (?-  (cannot_do  $Tl $Tt (reverse  (c b a) (variable  c)) $C))  True)
  (= (?-  (cannot_do  $Tl $Tt (reverse  (a b c) (c b a)) $C))  True)

  (= (?-  (what_cannot_do  $Ls $Ts (<-  $Q $Ans) () $F))  True)
  (= (?-  (what_cannot_do  $Ls $Ts (<-  (qsort  (1 2) (variable  a)) $Ans) () $F))  True)
  (= (?-  (what_cannot_do  $Ls $Ts (<-  (qsort  (2 1) (variable  a)) $Ans) () $F))  True)
  (= (?-  (what_cannot_do  $Ls $Ts (<-  (qsort  (2 3 1) (variable  a)) $Ans) () $F))  True)
  (= (?-  (what_cannot_do  $Ls $Ts (<-  (qsort  (3 2 1) (variable  a)) $Ans) () $F))  True)

