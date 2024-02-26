;                                           (convert-to-metta-file  calls_1 $_461756 multagnt/calls_1.pl multagnt/calls_1.metta)

  (= (?-  (teacher))  True)
  (= (?-  (learner1))  True)

  (= (?-  (demo  (:  teacher gilchrist_family) (parent  warren david) $L))  True)
  (= (?-  (demo  (:  teacher gilchrist_family) (parent  charles julia) $L))  True)
  (= (?-  (demo  (:  teacher gilchrist_family) (parent  warren (variable  a)) $L))  True)
  (= (?-  (demo  (:  teacher gilchrist_family) (parent  (variable  parent) (variable  child)) $L))  True)

  (= (?-  (demo  (:  teacher entropy) (entropy-increases  kitchen fridge) $L))  True)
  (= (?-  (demo  (:  teacher entropy) (entropy-increases  kitchen living_room) $L))  True)
  (= (?-  (demo  (:  teacher entropy) (entropy-increases  (variable  a) (variable  b)) $L))  True)

  (= (?-  (demo  (:  teacher t_member) (member  a (a b c)) $L))  True)
  (= (?-  (demo  (:  teacher t_member) (member  b (a b c)) $L))  True)
  (= (?-  (demo  (:  teacher t_member) (member  e (q w e r t z t r e w q)) $L))  True)
  (= (?-  (demo  (:  teacher t_member) (member  o (q w e r t z t r e w q)) $L))  True)
  (= (?-  (demo  (:  teacher t_member) (member  (variable  a) (a b c)) $L))  True)
  (= (?-  (demo  (:  teacher t_member) (member  (variable  a) (variable  list)) $L))  True)
  (= (?-  (demo  (:  teacher t_member) (member  a (variable  list)) $L))  True)

  (= (?-  (demo  (:  teacher t_reverse) (reverse  (a b c) (c b a)) $L))  True)
  (= (?-  (demo  (:  teacher t_reverse) (reverse  (b a c) (c b a)) $L))  True)
  (= (?-  (demo  (:  teacher t_reverse) (reverse  (variable  a) (c b a)) $L))  True)

  (= (?-  (can-do  $Tl $Tt (parent  warren david) $C))  True)
  (= (?-  (can-do  $Tl $Tt (parent  charles julia) $C))  True)
  (= (?-  (can-do  $Tl $Tt (parent  (variable  parent) (variable  child)) $C))  True)

  (= (?-  (demo  $Tl $Tt (entropy-increases  kitchen fridge) $C))  True)
  (= (?-  (can-do  $Tl $Tt (entropy-increases  kitchen fridge) $C))  True)
  (= (?-  (can-do  $Tl $Tt (entropy-increases  (variable  a) (variable  b)) $C))  True)

  (= (?-  (can-do  $Tl $Tt (member  a (g f d s a)) $C))  True)
  (= (?-  (can-do  $Tl $Tt (member  e (q w e r t z t r e w q)) $C))  True)
  (= (?-  (can-do  $Tl $Tt (member  (variable  a) (g f d s a)) $C))  True)
  (= (?-  (can-do  $Tl $Tt (member  (variable  a) (variable  list)) $C))  True)

  (= (?-  (can-do  $Tl $Tt (reverse  (a b c) (c b a)) $C))  True)
  (= (?-  (can-do  $Tl $Tt (reverse  (b a c) (c b a)) $C))  True)
  (= (?-  (can-do  $Tl $Tt (reverse  (variable  c) (c b a)) $C))  True)

