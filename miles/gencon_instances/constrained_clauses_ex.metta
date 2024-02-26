
  (= 
    (type_restriction  
      (male  $A) 
      ( (atom  $A))) True)
;                          /*
;                          type_restriction((A < B),[number(A),number(B)]).
; 
; 
;                          ex(merge([1],[2],[1,2]),'+').
;                          ex(merge([6],[4],[4,6]),'+').
;                          ex(merge([2,3,4,5],[4,7],[2,3,4,4,5,7]),'+').
;                          ex(merge([44,55,66],[22,33,44,55],[22,33,44,44,55,55,66]),'+').
;                          ex(merge([],[54,66,77,88,97],[54,66,77,88,97]),'+').
;                          ex(merge([],[4],[4]),'+').
;                          ex(merge([],[],[]),'+').
;                          ex(merge([22,23,24,25],[],[22,23,24,25]),'+').
;                          ex(merge([24],[],[24]),'+').
;                          ex(merge([29,39,49,59],[37,79,99],[29,37,39,49,59,79,99]),'+').
;                          ex(merge([2],[4,7],[2,4,7]),'+').
;                          ex(merge([1],[2],[2,1]),'-').
;                          ex(merge([6],[4],[6,4]),'-').
;                          ex(merge([8],[7],[8]),'-').
;                          ex(merge([2,3,4,5],[4,7],[4,3,2,4,5,7]),'-').
;                          ex(merge([44,55,66],[22,33,44,55],[22,44,55,33,55,44,66]),'-').
;                          ex(merge([29,39,49,59],[37,79,99],[37,39,29,59,79,49,99]),'-').
;                          ex(merge([2],[4,7],[4,2]),'-').
;                          ex(merge([1,2],[3,4],[1,3,2,4]),'-').
; 
; 
;                          */


  (= 
    (type_restriction  
      (female  $A) 
      ( (atom  $A))) True)
  (= 
    (type_restriction  
      (parent  $A $B) 
      ( (atom  $A) (atom  $B))) True)



  (= 
    (ex  
      (father  ma b) +) True)
  (= 
    (ex  
      (father  mc d) +) True)
  (= 
    (ex  
      (father  me f) +) True)
  (= 
    (ex  
      (father  mg h) +) True)
  (= 
    (ex  
      (father  mi j) +) True)
  (= 
    (ex  
      (father  mk l) +) True)

  (= 
    (ex  
      (father  fa b) -) True)
  (= 
    (ex  
      (father  fc d) -) True)
  (= 
    (ex  
      (father  fe f) -) True)
  (= 
    (ex  
      (father  fg h) -) True)
  (= 
    (ex  
      (father  fi j) -) True)
  (= 
    (ex  
      (father  fk l) -) True)

  (= 
    (ex  
      (father  b ma) -) True)
  (= 
    (ex  
      (father  d mc) -) True)
  (= 
    (ex  
      (father  f me) -) True)
  (= 
    (ex  
      (father  h mg) -) True)
  (= 
    (ex  
      (father  j mi) -) True)
  (= 
    (ex  
      (father  l mk) -) True)
  (= 
    (ex  
      (father  b fa) -) True)
  (= 
    (ex  
      (father  d fc) -) True)
  (= 
    (ex  
      (father  f fe) -) True)
  (= 
    (ex  
      (father  h fg) -) True)
  (= 
    (ex  
      (father  j fi) -) True)
  (= 
    (ex  
      (father  l fk) -) True)


  (= 
    (ex  
      (human  ma) +) True)
  (= 
    (ex  
      (human  mc) +) True)
  (= 
    (ex  
      (human  me) +) True)
  (= 
    (ex  
      (human  mg) +) True)
  (= 
    (ex  
      (human  mi) +) True)
  (= 
    (ex  
      (human  mk) +) True)
  (= 
    (ex  
      (human  fa) +) True)
  (= 
    (ex  
      (human  fc) +) True)
  (= 
    (ex  
      (human  fe) +) True)
  (= 
    (ex  
      (human  fg) +) True)
  (= 
    (ex  
      (human  fi) +) True)
  (= 
    (ex  
      (human  fk) +) True)
  (= 
    (ex  
      (human  b) +) True)
  (= 
    (ex  
      (human  d) +) True)
  (= 
    (ex  
      (human  f) +) True)
  (= 
    (ex  
      (human  h) +) True)
  (= 
    (ex  
      (human  j) +) True)
  (= 
    (ex  
      (human  l) +) True)
  (= 
    (ex  
      (human  a) -) True)
  (= 
    (ex  
      (human  c) -) True)



  (= 
    (male  ma) True)
  (= 
    (male  mc) True)
  (= 
    (male  me) True)
  (= 
    (male  mg) True)
  (= 
    (male  mi) True)
  (= 
    (male  mk) True)
  (= 
    (male  b) True)
  (= 
    (male  d) True)
  (= 
    (male  f) True)
  (= 
    (male  h) True)
  (= 
    (male  j) True)
  (= 
    (male  l) True)


  (= 
    (female  fa) True)
  (= 
    (female  fc) True)
  (= 
    (female  fe) True)
  (= 
    (female  fg) True)
  (= 
    (female  fi) True)
  (= 
    (female  fk) True)



  (= 
    (parent  ma b) True)
  (= 
    (parent  mc d) True)
  (= 
    (parent  me f) True)
  (= 
    (parent  mg h) True)
  (= 
    (parent  mi j) True)
  (= 
    (parent  mk l) True)

  (= 
    (parent  fa b) True)
  (= 
    (parent  fc d) True)
  (= 
    (parent  fe f) True)
  (= 
    (parent  fg h) True)
  (= 
    (parent  fi j) True)
  (= 
    (parent  fk l) True)


;                                         /*
; 
;                                         ex(t(nil),+).
;                                         ex(t(tree(nil,0,nil)),+).
;                                         ex(t(tree(nil,0,tree(nil,0,nil))),+).
;                                         ex(t(tree(nil,0,tree(nil,s(0),nil))),+).
;                                         ex(t(tree(nil,0,tree(nil,s(s(0)),nil))),+).
;                                         ex(t(tree(nil,0,tree(nil,s(s(s(0))),nil))),+).
;                                         ex(t(tree(nil,s(0),nil)),+).
;                                         ex(t(tree(nil,s(0),tree(nil,0,nil))),+).
;                                         ex(t(tree(nil,s(0),tree(nil,s(0),nil))),+).
;                                         ex(t(tree(nil,s(0),tree(nil,s(s(0)),nil))),+).
;                                         ex(t(tree(nil,s(0),tree(nil,s(s(s(0))),nil))),+).
;                                         ex(t(tree(nil,s(s(0)),nil)),+).
;                                         ex(t(tree(nil,s(s(0)),tree(nil,0,nil))),+).
;                                         ex(t(tree(nil,s(s(0)),tree(nil,s(0),nil))),+).
;                                         ex(t(tree(nil,s(s(0)),tree(nil,s(s(0)),nil))),+).
;                                         ex(t(tree(nil,s(s(0)),tree(nil,s(s(s(0))),nil))),+).
;                                         ex(t(tree(nil,s(s(s(0))),nil)),+).
;                                         ex(t(tree(nil,s(s(s(0))),tree(nil,0,nil))),+).
;                                         ex(t(tree(nil,s(s(s(0))),tree(nil,s(0),nil))),+).
;                                         ex(t(tree(nil,s(s(s(0))),tree(nil,s(s(0)),nil))),+).
;                                         ex(t(tree(nil,s(s(s(0))),tree(nil,s(s(s(0))),nil))),+).
;                                         ex(t(tree(tree(nil,0,nil),0,nil)),+).
;                                         ex(t(tree(tree(nil,0,nil),0,tree(nil,0,nil))),+).
;                                         ex(t(tree(tree(nil,0,nil),0,tree(nil,s(0),nil))),+).
;                                         ex(t(tree(tree(nil,0,nil),0,tree(nil,s(s(0)),nil))),+).
;                                         ex(t(tree(tree(nil,0,nil),0,tree(nil,s(s(s(0))),nil))),+).
;                                         ex(t(tree(tree(nil,0,nil),s(0),nil)),+).
;                                         ex(t(tree(tree(nil,0,nil),s(0),tree(nil,0,nil))),+).
;                                         ex(t(tree(tree(nil,0,nil),s(0),tree(nil,s(0),nil))),+).
;                                         ex(t(tree(tree(nil,0,nil),s(0),tree(nil,s(s(0)),nil))),+).
;                                         ex(t(tree(tree(nil,0,nil),s(0),tree(nil,s(s(s(0))),nil))),+).
;                                         ex(t(tree(tree(nil,0,nil),s(s(0)),nil)),+).
;                                         ex(t(tree(tree(nil,0,nil),s(s(0)),tree(nil,0,nil))),+).
;                                         ex(t(tree(tree(nil,0,nil),s(s(0)),tree(nil,s(0),nil))),+).
;                                         ex(t(tree(tree(nil,0,nil),s(s(0)),tree(nil,s(s(0)),nil))),+).
;                                         ex(t(tree(tree(nil,0,nil),s(s(0)),tree(nil,s(s(s(0))),nil))),+).
;                                         ex(t(tree(tree(nil,0,nil),s(s(s(0))),nil)),+).
;                                         ex(t(tree(tree(nil,0,nil),s(s(s(0))),tree(nil,0,nil))),+).
;                                         ex(t(tree(tree(nil,0,nil),s(s(s(0))),tree(nil,s(0),nil))),+).
;                                         ex(t(tree(tree(nil,0,nil),s(s(s(0))),tree(nil,s(s(0)),nil))),+).
;                                         ex(t(tree(tree(nil,0,nil),s(s(s(0))),tree(nil,s(s(s(0))),nil))),+).
;                                         ex(t(tree(tree(nil,s(0),nil),0,nil)),+).
;                                         ex(t(tree(tree(nil,s(0),nil),0,tree(nil,0,nil))),+).
;                                         ex(t(tree(tree(nil,s(0),nil),0,tree(nil,s(0),nil))),+).
;                                         ex(t(tree(tree(nil,s(0),nil),0,tree(nil,s(s(0)),nil))),+).
;                                         ex(t(tree(tree(nil,s(0),nil),0,tree(nil,s(s(s(0))),nil))),+).
;                                         ex(t(tree(tree(nil,s(0),nil),s(0),nil)),+).
;                                         ex(t(tree(tree(nil,s(0),nil),s(0),tree(nil,0,nil))),+).
;                                         ex(t(tree(tree(nil,s(0),nil),s(0),tree(nil,s(0),nil))),+).
;                                         ex(t(tree(tree(nil,s(0),nil),s(0),tree(nil,s(s(0)),nil))),+).
;                                         ex(t(tree(tree(nil,s(0),nil),s(0),tree(nil,s(s(s(0))),nil))),+).
;                                         ex(t(tree(tree(nil,s(0),nil),s(s(0)),nil)),+).
;                                         ex(t(tree(tree(nil,s(0),nil),s(s(0)),tree(nil,0,nil))),+).
;                                         ex(t(tree(tree(nil,s(0),nil),s(s(0)),tree(nil,s(0),nil))),+).
;                                         ex(t(tree(tree(nil,s(0),nil),s(s(0)),tree(nil,s(s(0)),nil))),+).
;                                         ex(t(tree(tree(nil,s(0),nil),s(s(0)),tree(nil,s(s(s(0))),nil))),+).
;                                         ex(t(tree(tree(nil,s(0),nil),s(s(s(0))),nil)),+).
;                                         ex(t(tree(tree(nil,s(0),nil),s(s(s(0))),tree(nil,0,nil))),+).
;                                         ex(t(tree(tree(nil,s(0),nil),s(s(s(0))),tree(nil,s(0),nil))),+).
;                                         ex(t(tree(tree(nil,s(0),nil),s(s(s(0))),tree(nil,s(s(0)),nil))),+).
;                                         ex(t(tree(tree(nil,s(0),nil),s(s(s(0))),tree(nil,s(s(s(0))),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(0)),nil),0,nil)),+).
;                                         ex(t(tree(tree(nil,s(s(0)),nil),0,tree(nil,0,nil))),+).
;                                         ex(t(tree(tree(nil,s(s(0)),nil),0,tree(nil,s(0),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(0)),nil),0,tree(nil,s(s(0)),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(0)),nil),0,tree(nil,s(s(s(0))),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(0)),nil),s(0),nil)),+).
;                                         ex(t(tree(tree(nil,s(s(0)),nil),s(0),tree(nil,0,nil))),+).
;                                         ex(t(tree(tree(nil,s(s(0)),nil),s(0),tree(nil,s(0),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(0)),nil),s(0),tree(nil,s(s(0)),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(0)),nil),s(0),tree(nil,s(s(s(0))),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(0)),nil),s(s(0)),nil)),+).
;                                         ex(t(tree(tree(nil,s(s(0)),nil),s(s(0)),tree(nil,0,nil))),+).
;                                         ex(t(tree(tree(nil,s(s(0)),nil),s(s(0)),tree(nil,s(0),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(0)),nil),s(s(0)),tree(nil,s(s(0)),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(0)),nil),s(s(0)),tree(nil,s(s(s(0))),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(0)),nil),s(s(s(0))),nil)),+).
;                                         ex(t(tree(tree(nil,s(s(0)),nil),s(s(s(0))),tree(nil,0,nil))),+).
;                                         ex(t(tree(tree(nil,s(s(0)),nil),s(s(s(0))),tree(nil,s(0),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(0)),nil),s(s(s(0))),tree(nil,s(s(0)),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(0)),nil),s(s(s(0))),tree(nil,s(s(s(0))),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(s(0))),nil),0,nil)),+).
;                                         ex(t(tree(tree(nil,s(s(s(0))),nil),0,tree(nil,0,nil))),+).
;                                         ex(t(tree(tree(nil,s(s(s(0))),nil),0,tree(nil,s(0),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(s(0))),nil),0,tree(nil,s(s(0)),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(s(0))),nil),0,tree(nil,s(s(s(0))),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(s(0))),nil),s(0),nil)),+).
;                                         ex(t(tree(tree(nil,s(s(s(0))),nil),s(0),tree(nil,0,nil))),+).
;                                         ex(t(tree(tree(nil,s(s(s(0))),nil),s(0),tree(nil,s(0),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(s(0))),nil),s(0),tree(nil,s(s(0)),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(s(0))),nil),s(0),tree(nil,s(s(s(0))),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(s(0))),nil),s(s(0)),nil)),+).
;                                         ex(t(tree(tree(nil,s(s(s(0))),nil),s(s(0)),tree(nil,0,nil))),+).
;                                         ex(t(tree(tree(nil,s(s(s(0))),nil),s(s(0)),tree(nil,s(0),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(s(0))),nil),s(s(0)),tree(nil,s(s(0)),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(s(0))),nil),s(s(0)),tree(nil,s(s(s(0))),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(s(0))),nil),s(s(s(0))),nil)),+).
;                                         ex(t(tree(tree(nil,s(s(s(0))),nil),s(s(s(0))),tree(nil,0,nil))),+).
;                                         ex(t(tree(tree(nil,s(s(s(0))),nil),s(s(s(0))),tree(nil,s(0),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(s(0))),nil),s(s(s(0))),tree(nil,s(s(0)),nil))),+).
;                                         ex(t(tree(tree(nil,s(s(s(0))),nil),s(s(s(0))),tree(nil,s(s(s(0))),nil))),+).
; 
;                                         ex(t(0),-).
;                                         ex(t(s(0)),-).
;                                         ex(t(s(s(0))),-).
;                                         ex(t(s(s(s(0)))),-).
;                                         ex(t(tree(nil,nil,nil)),-).
;                                         ex(t(tree(nil,tree(nil,0,nil),nil)),-).
;                                         ex(t(tree(0,0,s(0))),-).
;                                         ex(t(tree(s(s(0)),s(0),s(s(s(0))))),-).
;                                         ex(t(tree(nil,0,s(0))),-).
;                                         ex(t(tree(nil,s(0),s(s(s(0))))),-).
;                                         ex(t(tree(0,0,nil)),-).
;                                         ex(t(tree(s(s(s(0))),s(0),nil)),-).
;                                         ex(t(tree(nil,s(nil),nil)),-).
;                                         ex(t(tree(nil,s(s(nil)),nil)),-).
;                                         ex(t(tree(nil,s(tree(nil,0,nil)),nil)),-).
;                                         ex(t(tree(nil,s(s(tree(nil,s(0),nil))),nil)),-).
;                                         ex(t(tree(nil,s(s(s(nil))),nil)),-).
; 
; 
;                                         ex(p(f([],[])),+).
;                                         ex(p(f([a],[b])),+).
;                                         ex(p(f([a,a],[b,b])),+).
;                                         ex(p(f([a,a,a],[b,b,b])),+).
;                                         ex(p(f([a,a,a,a],[b,b,b,b])),+).
; 
; 
;                                         ex(p([]),-).
;                                         ex(p([a]),-).
;                                         ex(p([a,a]),-).
;                                         ex(p([a,a,a]),-).
;                                         ex(p([a,a,a,a]),-).
;                                         ex(p([b]),-).
;                                         ex(p([b,b]),-).
;                                         ex(p([b,b,b]),-).
;                                         ex(p([b,b,b,b]),-).
;                                         ex(p(f([a],[])),-).
;                                         ex(p(f([],[b,b])),-).
;                                         ex(p(f([a,a,a],[b,b])),-).
;                                         ex(p(f([a],[b,b,b])),-).
;                                         ex(p(f([a,a,a,a],[b,b,b])),-).
;                                         */


