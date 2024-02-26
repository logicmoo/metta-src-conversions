;                                           (convert_to_metta_file  ebg_2 $_501270 ebg/ebg_2.pl ebg/ebg_2.metta)

  (= (on  o1 o2)  True)

  (= (isa  o1 box)  True)
  (= (isa  o2 endtable)  True)

  (= (color  o1 red)  True)
  (= (color  o2 blue)  True)

  (= (volume  o1 1)  True)

  (= (density  o1 2)  True)

  (= (isa  o3 box)  True)

  (= (volume  o3 6)  True)

  (= (density  o3 2)  True)


  (= (save-to-stack $X $Y)  
    (lighter $X $Y))

  (= (weight $O $W)  
    (volume $O $V)
    (density $O $D)
    (is $W 
      (* $V $D)))

  (= (lighter $O1 $O2)  
    (weight $O1 $W1)
    (weight $O2 $W2)
    (< $W1 $W2))

  (= (weight $O 5)  
    (isa $O endtable))


  (= (?-  (;  (,  (ebg  (save_to_stack  o1 o2) $L) (,  (write  $L) fail)) true))  True)


