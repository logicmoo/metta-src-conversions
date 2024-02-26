;                                           (convert_to_metta_file  aq1_1 $_340696 aq1/aq1_1.pl aq1/aq1_1.metta)

  (= (domaintype  color nominal)  True)

  (= (valueset  color (red green blue))  True)

  (= (domaintype  temp linear)  True)

  (= (order  temp (cold warm hot))  True)

  (= (domaintype  shape nominal)  True)

  (= (valueset  shape (square hexagon octagon))  True)

  (= (classes  (past present future))  True)

  (= (events  past ((((= color  red)) ((= temp  warm)) ((= shape  square))) (((= color  green)) ((= temp  cold)) ((= shape  hexagon)))))  True)
  (= (events  present ((((= color  blue)) ((= temp  warm)) ((= shape  square))) (((= color  red)) ((= temp  hot)) ((= shape  hexagon)))))  True)
  (= (events  future ((((= color  green)) ((= temp  warm)) ((= shape  hexagon))) (((= color  blue)) ((= temp  hot)) ((= shape  octagon)))))  True)


