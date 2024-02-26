;                                           (convert_to_metta_file  sxx_read $_28460 sxx_machine/sxx_read.pl sxx_machine/sxx_read.metta)
;
; read/1: from screen - no error recovery - only simple syntax


  (= (vread $Term $Vars)  
    (nexttoken $Tok)
    (readall $Tok $Tokens)
    (maketerm $Tokens $Term)
    (set-det)
    (collectvars $Tokens Nil $Vars))


  (= (read $Term)  
    (nexttoken $Tok)
    (readall $Tok $Tokens)
    (maketerm $Tokens $Term)
    (set-det))


  (= (collectvars Nil $In $In)  
    (set-det))
  (= (collectvars (Cons  (var $Var $Name $Occ) $R) $In $Out)  
    (noteq $Name -)
    (notvmember $Name $In)
    (set-det)
    (collectvars $R 
      (Cons  
        (= $Var $Name) $In) $Out))
  (= (collectvars (Cons  $_ $R) $In $Out)  
    (collectvars $R $In $Out))


  (= (notvmember $_ Nil)  
    (set-det))
  (= (notvmember $Name (Cons  (= $_ $Name) $_))  
    (set-det)
    (fail))
  (= (notvmember $Name (Cons  $_ $R))  
    (notvmember $Name $R))


  (= (readall end-of-file $Tail)  
    (set-det)
    (halt))
  (= (readall end-of-clause $Tail)  
    (set-det)
    (= $Tail Nil))
  (= (readall $Token (Cons  $Token $Tail))  
    (nexttoken $NewToken)
    (readall $NewToken $Tail))



  (= (infix  1200 xfx :-)  True)
  (= (infix  1000 xfy ,)  True)
  (= (infix  1100 xfy ;)  True)
  (= (infix  1050 xfy ->)  True)
  (= (infix  700 xfx =)  True)
  (= (infix  700 xfx is)  True)
  (= (infix  700 xfx =..)  True)
  (= (infix  700 xfx ==)  True)
  (= (infix  700 xfx @<)  True)
  (= (infix  700 xfx @>)  True)
  (= (infix  700 xfx @=<)  True)
  (= (infix  700 xfx @>=)  True)
  (= (infix  700 xfx =:=)  True)
  (= (infix  700 xfx <)  True)
  (= (infix  700 xfx =<)  True)
  (= (infix  700 xfx >)  True)
  (= (infix  700 xfx >=)  True)
  (= (infix  550 xfy :)  True)
  (= (infix  500 yfx +)  True)
  (= (infix  500 yfx -)  True)
  (= (infix  500 yfx #)  True)
  (= (infix  400 yfx *)  True)
  (= (infix  400 yfx /)  True)
  (= (infix  400 yfx //)  True)
  (= (infix  400 yfx <<)  True)
  (= (infix  400 yfx >>)  True)
  (= (infix  300 xfx mod)  True)
  (= (infix  200 xfy ^)  True)


  (= (prefix  1200 fx :-)  True)
  (= (prefix  1200 fx ?-)  True)
  (= (prefix  500 fx +)  True)
  (= (prefix  500 fx -)  True)


;
; maketerm(In,Term)
;
; consists of tokens already


  (= (maketerm (:: (var $Var $Name $Occ)) $Out $_)  
    (= $Out $Var)
    (set-det))

  (= (maketerm (:: (int $Term)) $Out $_)  
    (set-det)
    (= $Out $Term))

  (= (maketerm (:: (real $Term)) $Out $_)  
    (set-det)
    (= $Out $Term))

  (= (maketerm (:: (const $Term)) $Out $_)  
    (set-det)
    (= $Out $Term))

  (= (maketerm (:: (string $Term)) $Out $_)  
    (set-det)
    (= $Out $Term))

  (= (maketerm (Cons  (const $Name) (Cons  (const () $Argswithcomma)) $Out $_)  
    (getargs $Argswithcomma $Args)
    (univ $Out $Name $Args))

  (= (maketerm $In $Term $Prec)  
    (ap $In1 
      (Cons  
        (const $Op) $In2) $In)
    (infix $P $Type $Op)
    (=< $P $Prec)
    (newprec $Type $P $P1 $P2)
    (maketerm $In1 $T1 $P1)
    (maketerm $In2 $T2 $P2)
    (univ $Term $Op 
      (:: $T1 $T2)))
  (= (maketerm (Cons  (const $Op) $In) $Term $Prec)  
    (prefix $P $Type $Op)
    (=< $P $Prec)
    (newprec $Type $P $P1)
    (maketerm $In $T1 $P1)
    (univ $Term $Op 
      (:: $T1)))
  (= (maketerm (Cons  (const [) $L) (Cons  $El $Tail) $_)  
    (ap $ElList 
      (Cons  
        (const ,) $Rest) $L)
    (maketerm $ElList $El 900)
    (maketerm 
      (Cons  
        (const [) $Rest) $Tail 900))
  (= (maketerm (Cons  (const [) $L) (Cons  $El $Tail) $_)  
    (ap $ElList 
      (Cons  
        (const |) $Rest) $L)
    (ap $TailL 
      (:: (const ])) $Rest)
    (maketerm $ElList $El 900)
    (maketerm $TailL $Tail 900))

  (= (maketerm (Cons  (const () $L) $Term $_)  
    (ap $L1 
      (:: (const ))) $L)
    (set-det)
    (maketerm $L1 $Term))
  (= (maketerm (Cons  (const [) $R) (:: $T2) $_)  
    (ap $L 
      (:: (const ])) $R)
    (maketerm $L $T2 900))
  (= (maketerm  ((const  [) (const  ])) () $_)  True)


  (= (newprec xfx $P $P1 $P2)  
    (set-det)
    (is $P1 
      (- $P 1))
    (is $P2 
      (- $P 1)))
  (= (newprec xfy $P $P1 $P2)  
    (set-det)
    (is $P1 
      (- $P 1))
    (= $P2 $P))
  (= (newprec yfx $P $P1 $P2)  
    (set-det)
    (is $P2 
      (- $P 1))
    (= $P1 $P))

  (= (newprec fx $P $P1)  
    (set-det)
    (is $P1 
      (- $P 1)))
  (= (newprec  fy $P $P)  True)


  (= (getargs $In $Out)  
    (ap $I 
      (:: (const ))) $In)
    (maketerm $I $T 900)
    (= $Out 
      (:: $T)))
  (= (getargs $In $Out)  
    (ap $I 
      (Cons  
        (const ,) $RI) $In)
    (maketerm $I $T 900)
    (= $Out 
      (Cons  $T $RT))
    (getargs $RI $RT))


  (= (maketerm $X $Y)  
    (maketerm $X $Y 1200))


  (= (ap  () $L $L)  True)
  (= (ap (Cons  $X $L1) $L2 (Cons  $X $L3))  
    (ap $L1 $L2 $L3))

