;                                           (convert-to-metta-file  arch2_1 $_305800 arch2/arch2_1.pl arch2/arch2_1.metta)

  (= (example  (+  (object  ($A $B $C) ((support  $A $C) (support  $B $C) (isa  $A rectangle) (isa  $B rectangle) (isa  $C rectangle)))))  True)
  (= (example  (-  (object  ($A $B $C) ((support  $A $C) (support  $B $C) (touch  $A $B) (isa  $A rectangle) (isa  $B rectangle) (isa  $C rectangle)))))  True)
  (= (example  (-  (object  ($A $B $C) ((isa  $A rectangle) (isa  $B rectangle) (isa  $C rectangle)))))  True)
  (= (example  (+  (object  ($A $B $C) ((support  $A $C) (support  $B $C) (isa  $A rectangle) (isa  $B rectangle) (isa  $C triangle)))))  True)



  (= (ako  figure polygone)  True)
  (= (ako  figure circle)  True)
  (= (ako  polygon convex_poly)  True)
  (= (ako  polygon concave_poly)  True)
  (= (ako  convex_poly stable_poly)  True)
  (= (ako  convex_poly unstable_poly)  True)
  (= (ako  stable_poly triangle)  True)
  (= (ako  stable_poly rectangle)  True)
  (= (ako  stable_poly trapezium)  True)
  (= (ako  unstable_poly unstable_triangle)  True)
  (= (ako  unstable_poly hexagon)  True)

