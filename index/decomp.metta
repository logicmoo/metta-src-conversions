;                                           (convert-to-metta-file  decomp $_506430 index/decomp.pl index/decomp.metta)

  (= (decompose $DisplayIC $PosIn $PosOut $InICs $OutICs)  
    (display $IC $DisplayIC)
    (contr $PosIn Nil $IC $Tuples $Answer)
    (det-if-then-else 
      (= $Answer no) 
      (split $IC $PosIn $PosOut $InICs $OutICs) 
      (det-if-then otherwise 
        (divide $PosIn $IC $PosOut $InICs $OutICs))))
; /* text of window: decomp */


  (= (split $Dep $PosIn $PosOut $ICs (Cons  $Join $ICs))  
    (dep $Dep $Type $Rel $From $To)
    (split1 $Rel $From $To $PosIn $PosOut $Join))


  (= (split1 $Rel $From $To $PosIn $PosOut (join $Rel $R1 $R2))  
    (append $From $To $In1)
    (new-name $In1 $Out1 $R1)
    (rel $Rel $AttrList)
    (listdiff $AttrList $To $In2)
    (new-name $In2 $Out2 $R2)
    (splits $Rel $R1 $R2 $Out1 $Out2 $PosIn $PosTmp)
    (remove-dups $PosTmp $PosOut))


  (= (splits  $Rel $R1 $R2 $AList1 $AList2 () ())  True)
  (= (splits $Rel $R1 $R2 $AList1 $AList2 (Cons  $T $Ts) (Cons  $T1 (Cons  $T2 $Rest)))  
    (values $Rel $AList1 $ValueList1 $T)
    (set-det)
    (=.. $T1 
      (Cons  $R1 $ValueList1))
    (values $Rel $AList2 $ValueList2 $T)
    (=.. $T2 
      (Cons  $R2 $ValueList2))
    (splits $Rel $R1 $R2 $AList1 $AList2 $Ts $Rest))
  (= (splits $Rel $R1 $R2 $AList1 $AList2 (Cons  $T $Ts) (Cons  $T $Rest))  
    (splits $Rel $R1 $R2 $AList1 $AList2 $Ts $Rest))
; ; T not in Rel


  (= (new-name $AListIn $AListOut $R)  
    (show-list attributes $AListIn)
    (prompt-read 'relation name' $Answer)
    (det-if-then-else 
      (= $Answer -) 
      (, 
        (concat-symbol $AListIn $R) 
        (= $AListOut $AListIn)) 
      (det-if-then-else 
        (= $Answer +) 
        (, 
          (prompt-read 'rel(Name,AttrList)' 
            (rel $R $AListOut)) 
          (permutation $AListOut $AListIn)) 
        (det-if-then otherwise 
          (, 
            (= $R $Answer) 
            (= $AListOut $AListIn)))))
    (new-rel $R $AListOut))


  (= (exec-rule $DisplayRule $PosIn $PosOut)  
    (display $Rule $DisplayRule)
    (horn $Rule 
      (= $Tuple  $Body))
    (setof0 $Tuple 
      (^ $Ts 
        (satisfied $Body $PosIn Nil $Ts)) $PosOut))


  (= (divide $PosIn $Dep $PosOut $InICs (Cons  (plus $Rel $Names) $OutICs))  
    (dep $Dep $Type $Rel $From $To)
    (filter $PosIn $Rel $PosFiltered $Rest)
    (splitsort $PosFiltered $Dep $SplitPos)
    (joinsort $SplitPos $Dep $NotContr $Contr)
    (find-division $Rel $NotContr $Contr $Numbers $NewPos1)
    (new-preds $Rel $NewPos1 Nil $PosTmp1 Nil $Names)
    (append $PosTmp1 $Rest $PosTmp2)
    (decomp-again $Names $Dep $PosTmp2 $PosOut $InICs $OutICs))
; ; divides(SplitPos,Dep,NewPos,Numbers),


  (= (decomp-again  () $Dep $Pos $Pos $ICs $ICs)  True)
  (= (decomp-again (Cons  $Name $Names) $Dep $PosIn $PosOut $InICs $OutICs)  
    (det-if-then-else 
      (yesno (:: 'Decompose ' $Name ? )) 
      (, 
        (dep $Dep $Type $R $From $To) 
        (dep $NewDep $Type $Name $From $To) 
        (display $NewDep $DDep) 
        (decompose $DDep $PosIn $PosTmp $InICs $TmpICs)) 
      (det-if-then otherwise 
        (, 
          (= $PosTmp $PosIn) 
          (= $TmpICs $InICs))))
    (decomp-again $Names $Dep $PosTmp $PosOut $TmpICs $OutICs))


  (= (divides  () $Dep () ())  True)
  (= (divides (Cons  $H $T) $Dep (Cons  $NewH $NewT) (Cons  $NsH $NsT))  
    (length $H $LH)
    (divide1 $H $LH $_ Nil 0 $_ $Dep $NewH $NsH)
    (set-det)
    (divides $T $Dep $NewT $NsT))


  (= (divide1 $Pos1 $Pos2 $IC $PosOut)  
    (divide1 $Pos1 0 $_ $Pos2 0 $_ $IC $PosOut $_))

  (= (divide1 Nil $K $K Nil $L $L $IC Nil Nil)  
    (set-det))
  (= (divide1 $Pos1 $K0 $K $Pos2 $L0 $L $IC $PosOut $M)  
    (contr $Pos1 Nil $IC $Tuples $Answer)
    (det-if-then-else 
      (or 
        (= $Answer yes) 
        (= $Answer possibly)) 
      (, 
        (= $Tuples 
          (Cons  
            (+ $T1) 
            (Cons  
              (+ $T2) $N))) 
        (remove $T2 $Pos1 $NewPos1) 
        (is $K1 
          (- $K0 1)) 
        (= $NewPos2 
          (Cons  $T2 $Pos2)) 
        (is $L1 
          (+ $L0 1)) 
        (= $PosOut $NewPosOut) 
        (= $M $NewM)) 
      (det-if-then 
        (= $Answer no) 
        (, 
          (= $NewPos1 $Pos2) 
          (= $K1 $L0) 
          (= $NewPos2 Nil) 
          (= $L1 0) 
          (= $PosOut 
            (Cons  $Pos1 $NewPosOut)) 
          (= $M 
            (Cons  $K0 $NewM)))))
    (set-det)
    (divide1 $NewPos1 $K1 $K $NewPos2 $L1 $L $IC $NewPosOut $NewM))


  (= (find-division $Rel $NotContr $Contr $Numbers $NewPos)  
    (det-if-then-else 
      (= $NotContr Nil) 
      (= $NewPos $Contr) 
      (det-if-then otherwise 
        (= $NewPos 
          (Cons  
            (:: $NotContr) $Contr))))
    (show-lists $Rel 
      (:: segment part) $NewPos)
    (yesno (:: 'Proceed? ')))


  (= (new-preds  $Rel () $New $New $Names $Names)  True)
  (= (new-preds $Rel (Cons  $H $T) $New0 $New $Names0 $Names)  
    (new-preds1 $Rel $H $New0 $New1 $Names0 $Names1)
    (new-preds $Rel $T $New1 $New $Names1 $Names))


  (= (new-preds1  $Rel () $New $New $Names $Names)  True)
  (= (new-preds1 $Rel (Cons  $R $Rs) $New0 $New $Names0 $Names)  
    (show-list $Rel 'partial relation' $R)
    (prompt-read 'relation name' $N)
    (rel $Rel $Attrs)
    (new-rel $N $Attrs)
    (new-relation $Rel $R $N $NewR)
    (append $New0 $NewR $New1)
    (add-if $N $Names0 $Names1)
    (new-preds1 $Rel $Rs $New1 $New $Names1 $Names))


  (= (new-relation $Rel $R $N $NewR)  
    (bagof0 $NewT 
      (^ $T 
        (, 
          (member $T $R) 
          (new-tuple $Rel $T $N $NewT))) $NewR))


  (= (new-tuple $Rel $T $N $NewT)  
    (=.. $T 
      (Cons  $Rel $Args))
    (set-det)
    (=.. $NewT 
      (Cons  $N $Args)))
  (= (new-tuple  $Rel $T $N $T)  True)	;
; tuple from other relation

