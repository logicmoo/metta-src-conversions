;                                           (convert_to_metta_file  idt1_2 $_298570 idt/idt1_2.pl idt/idt1_2.metta)

  (= (classes  (accept reject))  True)

  (= (attributes  (account cash employed credits))  True)

  (= (example  1 accept ((= account  bank) (= cash  700) (= employed  yes) (= credits  200)))  True)
  (= (example  2 reject ((= account  bank) (= cash  300) (= employed  yes) (= credits  600)))  True)
  (= (example  3 reject ((= account  none) (= cash  0) (= employed  yes) (= credits  400)))  True)
  (= (example  4 accept ((= account  others) (= cash  1200) (= employed  yes) (= credits  600)))  True)
  (= (example  5 reject ((= account  others) (= cash  800) (= employed  yes) (= credits  600)))  True)
  (= (example  6 accept ((= account  others) (= cash  1600) (= employed  yes) (= credits  200)))  True)
  (= (example  7 accept ((= account  bank) (= cash  3000) (= employed  no) (= credits  300)))  True)
  (= (example  8 reject ((= account  none) (= cash  0) (= employed  no) (= credits  200)))  True)


