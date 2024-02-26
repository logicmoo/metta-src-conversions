;                                           (convert-to-metta-file  aq1_2 $_397698 aq1/aq1_2.pl aq1/aq1_2.metta)

  (= (domaintype  color nominal)  True)

  (= (valueset  color (red green blue))  True)

  (= (domaintype  temp linear)  True)

  (= (order  temp (cold warm hot))  True)

  (= (domaintype  shape nominal)  True)

  (= (valueset  shape (square hexagon octagon))  True)

  (= (classes  (past present future))  True)

  (= (events  past ((((= color  red)) ((= shape  square)) ((= temp  warm))) (((= color  red)) ((= shape  hexagon)) ((= temp  hot))) (((= color  green)) ((= shape  hexagon)) ((= temp  cold)))))  True)
  (= (events  present ((((= color  blue)) ((= shape  square)) ((= temp  warm))) (((= color  red)) ((= shape  hexagon)) ((= temp  hot))) (((= color  red)) ((= shape  hexagon)) ((= temp  hot)))))  True)
  (= (events  future ((((= color  green)) ((= shape  hexagon)) ((= temp  warm))) (((= color  red)) ((= shape  hexagon)) ((= temp  hot))) (((= color  blue)) ((= shape  octagon)) ((= temp  hot)))))  True)


