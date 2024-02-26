;                                           (convert-to-metta-file  animal $_50992 sxx_machine/tests/animal.pl sxx_machine/tests/animal.metta)

  (= (animal  goat)  True)
  (= (animal  elephant)  True)
  (= (animal $X)  
    (write 'please type one more animal name: ')
    (read $X))

