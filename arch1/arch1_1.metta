
  (= 
    (input  1 example arch 
      ( (part  part1) 
        (part  part2) 
        (part  part3) 
        (isa  part1 brick) 
        (isa  part2 brick) 
        (isa  part3 brick) 
        (left_of  part2 part1) 
        (supports  part1 part3) 
        (supports  part2 part3))) True)
; 

  (= 
    (input  2 near_miss arch 
      ( (part  part1) 
        (isa  part2 brick) 
        (part  part2) 
        (left_of  part2 part1) 
        (part  part3) 
        (isa  part1 brick) 
        (isa  part3 brick))) True)
; 

  (= 
    (input  3 near_miss arch 
      ( (part  part1) 
        (part  part2) 
        (part  part3) 
        (isa  part1 brick) 
        (isa  part2 brick) 
        (isa  part3 brick) 
        (left_of  part2 part1) 
        (touches  part1 part2) 
        (supports  part1 part3) 
        (supports  part2 part3))) True)
; 

  (= 
    (input  4 example arch 
      ( (part  part1) 
        (part  part2) 
        (part  part3) 
        (isa  part1 brick) 
        (isa  part2 brick) 
        (isa  part3 wedge) 
        (left_of  part2 part1) 
        (supports  part1 part3) 
        (supports  part2 part3))) True)
; 

  (= 
    (input  5 near_miss arch 
      ( (part  part1) 
        (part  part2) 
        (part  part3) 
        (isa  part1 brick) 
        (isa  part2 brick) 
        (isa  part3 cylinder) 
        (left_of  part2 part1) 
        (supports  part1 part3) 
        (supports  part2 part3))) True)
; 

	  

  (= 
    (infer  
      (touch  $O1 $O2) 
      (touch  $O2 $O1)) True)
; 


  (= 
    (infer 
      (isa $Object $Type1) 
      (isa $Object $Type2)) 
    ( (set-det) (ako $Type1 $Type2)))
; 



  (= 
    (ako  brick parallel_epiphed) True)
; 

  (= 
    (ako  wedge parallel_epiphed) True)
; 



