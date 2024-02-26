;                                           (convert_to_metta_file  ebg_1 $_448136 ebg/ebg_1.pl ebg/ebg_1.metta)

  (= (depressed  john)  True)

  (= (buy  john gun1)  True)

  (= (gun  gun1)  True)


  (= (kill $A $B)  
    (hate $A $B)
    (possess $A $C)
    (weapon $C))

  (= (hate $W $W)  
    (depressed $W))

  (= (possess $U $V)  
    (buy $U $V))

  (= (weapon $Z)  
    (gun $Z))


  (= (?-  (;  (,  (ebg  (kill  john john) $L) (,  (write  $L) fail)) true))  True)


