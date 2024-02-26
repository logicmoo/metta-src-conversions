;                                           (convert-to-metta-file  idt1_3 $_58408 idt/idt1_3.pl idt/idt1_3.metta)

  (= (classes  (accept reject))  True)

  (= (attributes  (account cash employed))  True)

  (= (example  1 accept ((= account  bank) (= employed  yes) (= cash  300)))  True)
  (= (example  2 accept ((= account  bank) (= employed  yes) (= cash  300)))  True)
  (= (example  3 reject ((= account  bank) (= employed  no) (= cash  300)))  True)
  (= (example  4 accept ((= account  bank) (= employed  no) (= cash  4000)))  True)
  (= (example  5 accept ((= account  none) (= employed  yes) (= cash  4000)))  True)
  (= (example  6 reject ((= account  none) (= employed  yes) (= cash  300)))  True)
  (= (example  7 reject ((= account  none) (= employed  no) (= cash  300)))  True)
  (= (example  8 reject ((= account  none) (= employed  no) (= cash  4000)))  True)
  (= (example  9 reject ((= account  none) (= employed  no) (= cash  4000)))  True)
  (= (example  10 reject ((= account  others) (= employed  no) (= cash  300)))  True)
  (= (example  11 accept ((= account  others) (= employed  no) (= cash  4000)))  True)
  (= (example  12 reject ((= account  others) (= employed  no) (= cash  300)))  True)

