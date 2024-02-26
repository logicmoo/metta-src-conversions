;                                           (convert_to_metta_file  arch1_1 $_339820 arch1/arch1_1.pl arch1/arch1_1.metta)

  (= (input  1 example arch ((part  part1) (part  part2) (part  part3) (isa  part1 brick) (isa  part2 brick) (isa  part3 brick) (left_of  part2 part1) (supports  part1 part3) (supports  part2 part3)))  True)
; /********************************************************************/
; /* Each example is represented by a set of MeTTa facts. The arch */
; /* examples in Winston's book come out as follows: */
; /********************************************************************/
  (= (input  2 near_miss arch ((part  part1) (isa  part2 brick) (part  part2) (left_of  part2 part1) (part  part3) (isa  part1 brick) (isa  part3 brick)))  True)
  (= (input  3 near_miss arch ((part  part1) (part  part2) (part  part3) (isa  part1 brick) (isa  part2 brick) (isa  part3 brick) (left_of  part2 part1) (touches  part1 part2) (supports  part1 part3) (supports  part2 part3)))  True)
  (= (input  4 example arch ((part  part1) (part  part2) (part  part3) (isa  part1 brick) (isa  part2 brick) (isa  part3 wedge) (left_of  part2 part1) (supports  part1 part3) (supports  part2 part3)))  True)
  (= (input  5 near_miss arch ((part  part1) (part  part2) (part  part3) (isa  part1 brick) (isa  part2 brick) (isa  part3 cylinder) (left_of  part2 part1) (supports  part1 part3) (supports  part2 part3)))  True)
	  

  (= (infer  (touch  $O1 $O2) (touch  $O2 $O1))  True)
; /********************************************************************/
; /* background knowledge */
; /********************************************************************/
; /* infer - used during matching to try to infer facts that might */
; /* match. */
; /* Careful - match doesn't check for cycles in the inference chain. */
; /********************************************************************/

  (= (infer (isa $Object $Type1) (isa $Object $Type2))  
    (set-det)
    (ako $Type1 $Type2))


  (= (ako  brick parallel_epiphed)  True)
; /********************************************************************/
; /* ako - represent taxonomies */
; /********************************************************************/
  (= (ako  wedge parallel_epiphed)  True)


