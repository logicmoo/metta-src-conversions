;                                           (convert_to_metta_file  attdsc_1 $_448868 attdsc/attdsc_1.pl attdsc/attdsc_1.metta)

  (= (attribute  size (small large))  True)
  (= (attribute  shape (long compact other))  True)
  (= (attribute  holes (none 1 2 3 many))  True)


  (= (example  nut ((= size  small) (= shape  compact) (= holes  1)))  True)
  (= (example  screw ((= size  small) (= shape  long) (= holes  none)))  True)
  (= (example  key ((= size  small) (= shape  long) (= holes  1)))  True)
  (= (example  nut ((= size  small) (= shape  compact) (= holes  1)))  True)
  (= (example  key ((= size  large) (= shape  long) (= holes  1)))  True)
  (= (example  screw ((= size  small) (= shape  compact) (= holes  none)))  True)
  (= (example  nut ((= size  small) (= shape  compact) (= holes  1)))  True)
  (= (example  pen ((= size  large) (= shape  long) (= holes  none)))  True)
  (= (example  scissors ((= size  large) (= shape  long) (= holes  2)))  True)
  (= (example  pen ((= size  large) (= shape  long) (= holes  none)))  True)
  (= (example  scissors ((= size  large) (= shape  other) (= holes  2)))  True)
  (= (example  key ((= size  small) (= shape  other) (= holes  2)))  True)


  (= (?-  (learn  nut))  True)
  (= (?-  (learn  key))  True)
  (= (?-  (learn  scissors))  True)

