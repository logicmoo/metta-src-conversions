;
; 




  !(ensure-loaded dmiles)
; 


;
; 

  !(use-module-if-exists (library proxt))
; 

  !(use-module-if-exists (library ctypes))
; 

  !(use-module-if-exists (library strings))
; 

  !(use-module-if-exists (library basics))
; 

  !(consult miles)
; 

;
; 

  !(consult xmiles-functions)
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (runtime-entry start) 
    (xm))
; 



  !(dynamic (, (/ listItems 1) (/ my-exit-loop 1) (/ toplevel 1)))
; 



  (= 
    (my_exit_loop  no) True)
; 






;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 




  (= 
    (xm) 
    ( (det-if-then-else 
        (toplevel $Widget) 
        (, 
          (= $XMiles $Widget) 
          (open xmProtocol.tmp write $F) 
          (recordz messages 
            (file $F) $_) 
          (updateEvaluationLabel) 
          (refreshKnowledgeList $Widget rules $Calldata) 
          (refreshKnowledgeList $Widget examples $Calldata)) 
        (, 
          (xtToolkitInitialize) 
          (xtInitialize X-MILES xMILES $XMiles) 
          (add-symbol  &self 
            (toplevel  $XMiles)) 
          (clear-kb) 
          (xmCreateRowColumn $XMiles xMilesRow 
            (:: 
              (xmNorientation xmHORIZONTAL) 
              (xmNpacking xmPACK-TIGHT) 
              (xmNrowColumnType xmWORK-AREA)) $XMilesRow) 
          (xtManageChild $XMilesRow) 
          (xmCreateRowColumn $XMilesRow xMilesColumn1 
            (:: 
              (xmNorientation xmVERTICAL) 
              (xmNpacking xmPACK-TIGHT) 
              (xmNrowColumnType xmWORK-AREA)) $XMilesColumn1) 
          (xtManageChild $XMilesColumn1) 
          (createCommandArea $XMilesColumn1) 
          (createFunctionArea $XMilesColumn1) 
          (createArgumentArea $XMilesColumn1) 
          (createMessageArea $XMilesColumn1) 
          (xmCreateRowColumn $XMilesRow xMilesColumn2 
            (:: 
              (xmNorientation xmVERTICAL) 
              (xmNpacking xmPACK-TIGHT) 
              (xmNrowColumnType xmWORK-AREA)) $XMilesColumn2) 
          (xtManageChild $XMilesColumn2) 
          (createEditorArea $XMilesColumn2) 
          (createKnowledgeBaseArea $XMilesColumn2) 
          (xtRealizeWidget $XMiles))) (my-main-loop $XMiles)))
; 



  (= 
    (my-main-loop $Shell) 
    (det-if-then-else 
      (my-exit-loop yes) 
      (, 
        (remove-symbol  &self 
          (my_exit_loop  yes)) 
        (add-symbol  &self 
          (my_exit_loop  no))) 
      (, 
        (xtNextEvent $Event) 
        (xtDispatchEvent $Event) 
        (my-main-loop $Shell))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (createKnowledgeBaseArea $Parent) 
    ( (xmCreateFrame $Parent KnowledgeBaseFrame Nil $KnowledgeBaseFrame) 
      (xtManageChild $KnowledgeBaseFrame) 
      (xmCreateRowColumn $KnowledgeBaseFrame KnowledgeBaseColumn 
        (:: 
          (xmNadjustLast True) 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT) 
          (xmNrowColumnType xmWORK-AREA)) $KnowledgeBaseColumn) 
      (xtManageChild $KnowledgeBaseColumn) 
      (xmCreateRowColumn $KnowledgeBaseColumn titleRC 
        (:: 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN)) $TitleRowColumn) 
      (xtManageChild $TitleRowColumn) 
      (proxtGetDefaultCharset $DCharset) 
      (proxtStringToCharPtr 'Knowledge Base' $KbCP) 
      (xmStringCreate $KbCP $DCharset $StatusStr) 
      (xmCreateLabelGadget $TitleRowColumn titleLabel 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $StatusStr)) $KbLabel) 
      (xtManageChild $KbLabel) 
      (createEvaluationString $EString) 
      (proxtStringToCharPtr $EString $KbCP1) 
      (xmStringCreate $KbCP1 $DCharset $StatusStr1) 
      (xmCreateLabelGadget $TitleRowColumn titleEvaluation 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $StatusStr1)) $KbLabel1) 
      (xtManageChild $KbLabel1) 
      (recordz irene $KbLabel1 $_) 
      (xmCreateRowColumn $KnowledgeBaseColumn kbRowColumn 
        (:: 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN)) $KnowledgeBaseRow) 
      (xtManageChild $KnowledgeBaseRow) 
      (createRuleArea $KnowledgeBaseRow) 
      (createExampleArea $KnowledgeBaseRow) 
      (createKbButtons $KnowledgeBaseColumn)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (createRuleArea $Parent) 
    ( (xmCreateFrame $Parent RuleFrame Nil $RuleFrame) 
      (xtManageChild $RuleFrame) 
      (xmCreateRowColumn $RuleFrame RuleRowColumn 
        (:: 
          (xmNwidth 300) 
          (xmNadjustLast True) 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT) 
          (xmNrowColumnType xmWORK-AREA)) $RuleColumn) 
      (xtManageChild $RuleColumn) 
      (xmCreateRowColumn $RuleColumn ruleRow 
        (:: 
          (xmNorientation xmHORIZONTAL) 
          (xmNmarginHeight 0) 
          (xmNpacking xmPACK-TIGHT)) $RuleRow) 
      (xtManageChild $RuleRow) 
      (proxtGetDefaultCharset $DCharset) 
      (proxtStringToCharPtr 'Rules               ' $RuleCP) 
      (xmStringCreate $RuleCP $DCharset $RuleStr) 
      (xmCreateLabelGadget $RuleRow ruleLabel 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $RuleStr)) $RuleLabel) 
      (xtManageChild $RuleLabel) 
      (xmCreatePushButton $RuleRow 'Examine ...' Nil $Examine) 
      (xtManageChild $Examine) 
      (createExamineRulesPopup $Examine) 
      (createKnowledgeList $RuleColumn rules) 
      (createKbSubButtons $RuleColumn rules)))
; 





;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (createExampleArea $Parent) 
    ( (xmCreateFrame $Parent ExampleFrame Nil $ExampleFrame) 
      (xtManageChild $ExampleFrame) 
      (xmCreateRowColumn $ExampleFrame ExampleRowColumn 
        (:: 
          (xmNwidth 300) 
          (xmNadjustLast True) 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT) 
          (xmNrowColumnType xmWORK-AREA)) $ExampleRowColumn) 
      (xtManageChild $ExampleRowColumn) 
      (proxtGetDefaultCharset $DCharset) 
      (proxtStringToCharPtr Examples $ExampleCP) 
      (xmStringCreate $ExampleCP $DCharset $ExampleStr) 
      (xmCreateLabelGadget $ExampleRowColumn exampleLabel 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $ExampleStr)) $ExampleLabel) 
      (xtManageChild $ExampleLabel) 
      (createKnowledgeList $ExampleRowColumn examples) 
      (createKbSubButtons $ExampleRowColumn examples)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (createKnowledgeList $Parent $KindOfKnowledge) 
    ( (xmCreateScrolledWindow $Parent knowledgelistSW 
        (:: 
          (xmNheight 300) 
          (xmNwidth 300) 
          (xmNlistSizePolicy xmCONSTANT) 
          (xmNscrollBarDisplayPolicy xmSTATIC) 
          (xmNscrollingPolicy xmAUTOMATIC)) $KL) 
      (xtManageChild $KL) 
      (xmCreateRowColumn $KL knowledgelistRC 
        (:: 
          (xmNadjustLast False) 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT) 
          (xmNrowColumnType xmWORK-AREA)) $KnowledgeRC) 
      (recordz $KindOfKnowledge 
        (knowledgeList $KnowledgeRC) $Ref) 
      (xtGetValues $KnowledgeRC 
        (:: (xmNbackground $B))) 
      (xtGetValues $KL 
        (:: (xmNclipWindow $CW))) 
      (xtSetValues $CW 
        (:: (xmNbackground $B))) 
      (addKnowledgeListItems $KindOfKnowledge) 
      (xtManageChild $KnowledgeRC)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (addKnowledgeListItems rules) 
    ( (getNextId $Id) 
      (get-clause $Id $H $B $S $O) 
      (addRuleItem $Id $H $B $S $O) 
      (fail)))
; 

  (= 
    (addKnowledgeListItems examples) 
    ( (getNextId $Id) 
      (get-example $Id $F $C) 
      (addExampleItem $Id $F $C) 
      (fail)))
; 

  (= 
    (addKnowledgeListItems $_) 
    (set-det))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (getNextId $Id) 
    ( (id-count $MaxId) 
      (repeat) 
      (det-if-then-else 
        (recorded knowledgeList 
          (xm-id-count $I_old) $Ref) 
        (, 
          (erase $Ref) 
          (is $Id 
            (+ $I_old 1)) 
          (recordz knowledgeList 
            (xm-id-count $Id) $_)) 
        (, 
          (= $Id 1) 
          (recordz knowledgeList 
            (xm-id-count $Id) $_))) 
      (det-if-then-else 
        (< $MaxId $Id) 
        (, 
          (recorded knowledgeList 
            (xm-id-count $_) $Ref2) 
          (erase $Ref2) 
          (set-det) 
          (fail)) otherwise)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (deleteKnowledgeListItems rules) 
    ( (recorded current 
        (clause $_ $Label $_) $Ref) 
      (xtDestroyWidget $Label) 
      (erase $Ref) 
      (set-det) 
      (deleteKnowledgeListItems rules)))
; 

  (= 
    (deleteKnowledgeListItems examples) 
    ( (recorded current 
        (example $_ $Label $_) $Ref) 
      (xtDestroyWidget $Label) 
      (erase $Ref) 
      (set-det) 
      (deleteKnowledgeListItems examples)))
; 

  (= 
    (deleteKnowledgeListItems $_) 
    (set-det))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (addRuleItem $I $H $B $S $O) 
    ( (det-if-then 
        (recorded rules 
          (viewMin $MinText) $_) 
        (, 
          (xmTextGetString $MinText $MinCP) 
          (proxtCharPtrToString $MinCP $MinStr) 
          (atom-chars $MinStr $MinC) 
          (number-chars $Min $MinC))) 
      (det-if-then 
        (recorded rules 
          (viewMax $MaxText) $_) 
        (, 
          (xmTextGetString $MaxText $MaxCP) 
          (proxtCharPtrToString $MaxCP $MaxStr) 
          (atom-chars $MaxStr $MaxC) 
          (number-chars $Max $MaxC))) 
      (or 
        (recorded rules 
          (view all) $_) 
        (, 
          (recorded rules 
            (view labels $LabelList) $_) 
          (recorded rules 
            (view clause-heads $CHList) $_) 
          (>= $I $Min) 
          (=< $I $Max) 
          (functor $H $CH $_) 
          (or 
            (= $LabelList Nil) 
            (member $O $LabelList)) 
          (or 
            (= $CHList Nil) 
            (member $CH $CHList)))) 
      (proxtGetDefaultCharset $DCharset) 
      (recorded rules 
        (knowledgeList $KL) $_) 
      (xxmWriteToString 
        (show-kb-clause $I $H $B $O) $DCharset $XmS) 
      (det-if-then-else 
        (recorded current 
          (clause $I $W $_) $_) 
        (xtSetValues $W 
          (:: (xmNlabelString $XmS))) 
        (, 
          (xmCreateLabel $KL anyClause 
            (:: 
              (xmNalignment xmALIGNMENT-BEGINNING) 
              (xmNlabelString $XmS) 
              (xmNlabelType xmSTRING)) $Label) 
          (xtManageChild $Label) 
          (recordz current 
            (clause $I $Label notselected) $Ref) 
          (xtAddEventHandler $Label 
            (:: buttonReleaseMask) False selectClause rules) 
          (xtAddEventHandler $Label 
            (:: buttonPressMask) False firstClauseClick rules))) 
      (set-det)))
; 


  (= 
    (addRuleItem $_ $_ $_ $_ $_) 
    (set-det))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (addExampleItem $I $F $C) 
    ( (det-if-then 
        (recorded examples 
          (viewMin $MinText) $_) 
        (, 
          (xmTextGetString $MinText $MinCP) 
          (proxtCharPtrToString $MinCP $MinStr) 
          (atom-chars $MinStr $MinC) 
          (number-chars $Min $MinC))) 
      (det-if-then 
        (recorded examples 
          (viewMax $MaxText) $_) 
        (, 
          (xmTextGetString $MaxText $MaxCP) 
          (proxtCharPtrToString $MaxCP $MaxStr) 
          (atom-chars $MaxStr $MaxC) 
          (number-chars $Max $MaxC))) 
      (or 
        (recorded examples 
          (view all) $_) 
        (, 
          (recorded examples 
            (view clause-heads $CHList) $_) 
          (functor $F $CH $_) 
          (>= $I $Min) 
          (=< $I $Max) 
          (or 
            (= $CHList Nil) 
            (member $CH $CHList)))) 
      (proxtGetDefaultCharset $DCharset) 
      (recorded examples 
        (knowledgeList $KL) $_) 
      (number-chars $I $S1) 
      (append $S1 " (" $S2) 
      (atom-chars $C $S3) 
      (append $S2 $S3 $S4) 
      (append $S4 "): " $S5) 
      (atom-chars $A5 $S5) 
      (proxtStringToCharPtr $A5 $CP5) 
      (xmStringCreateLtoR $CP5 $DCharset $XmS1) 
      (xxmWriteToString 
        (write $F) $DCharset $XmS2) 
      (xmStringConcat $XmS1 $XmS2 $XmS3) 
      (xmStringSeparatorCreate $XmSep) 
      (xmStringConcat $XmS3 $XmSep $XmS) 
      (det-if-then-else 
        (recorded current 
          (example $I $W $_) $_) 
        (xtSetValues $W 
          (:: (xmNlabelString $XmS))) 
        (, 
          (xmCreateLabel $KL anyExample 
            (:: 
              (xmNalignment xmALIGNMENT-BEGINNING) 
              (xmNlabelString $XmS) 
              (xmNlabelType xmSTRING)) $Label) 
          (xtManageChild $Label) 
          (xtAddEventHandler $Label 
            (:: buttonReleaseMask) False selectClause examples) 
          (xtAddEventHandler $Label 
            (:: buttonPressMask) False firstClauseClick examples) 
          (recordz current 
            (example $I $Label notselected) $Ref))) 
      (set-det)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (refreshKnowledgeList $Widget $KindOfKnowledge $Calldata) 
    ( (deleteKnowledgeListItems $KindOfKnowledge) 
      (addKnowledgeListItems $KindOfKnowledge) 
      (updateEvaluationLabel)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (selectClause $Widget rules $CallData) 
    ( (recorded current 
        (clause $I $Widget notselected) $Ref) 
      (xtGetValues $Widget 
        (:: 
          (xmNbackground $B) 
          (xmNforeground $F))) 
      (xtSetValues $Widget 
        (:: 
          (xmNbackground $F) 
          (xmNforeground $B))) 
      (erase $Ref) 
      (recordz current 
        (clause $I $Widget selected) $_) 
      (recorded state 
        (getId $GetId) $_) 
      (xtGetValues $GetId 
        (:: (xmNset $GetState))) 
      (det-if-then-else 
        (= $GetState True) 
        (copyId $I 1) otherwise)))
; 


  (= 
    (selectClause $Widget rules $CallData) 
    ( (recorded current 
        (clause $I $Widget selected) $Ref) 
      (xtGetValues $Widget 
        (:: 
          (xmNbackground $B) 
          (xmNforeground $F))) 
      (xtSetValues $Widget 
        (:: 
          (xmNbackground $F) 
          (xmNforeground $B))) 
      (erase $Ref) 
      (recordz current 
        (clause $I $Widget notselected) $Ref) 
      (recorded state 
        (getId $GetId) $_) 
      (xtGetValues $GetId 
        (:: (xmNset $GetState))) 
      (det-if-then-else 
        (= $GetState True) 
        (copyId $I 1) otherwise)))
; 


  (= 
    (selectClause $Widget examples $CallData) 
    ( (recorded current 
        (example $I $Widget notselected) $Ref) 
      (xtGetValues $Widget 
        (:: 
          (xmNbackground $B) 
          (xmNforeground $F))) 
      (xtSetValues $Widget 
        (:: 
          (xmNbackground $F) 
          (xmNforeground $B))) 
      (erase $Ref) 
      (recordz current 
        (example $I $Widget selected) $Ref) 
      (recorded state 
        (getId $GetId) $_) 
      (xtGetValues $GetId 
        (:: (xmNset $GetState))) 
      (det-if-then-else 
        (= $GetState True) 
        (copyId $I 1) otherwise)))
; 


  (= 
    (selectClause $Widget examples $CallData) 
    ( (recorded current 
        (example $I $Widget selected) $Ref) 
      (xtGetValues $Widget 
        (:: 
          (xmNbackground $B) 
          (xmNforeground $F))) 
      (xtSetValues $Widget 
        (:: 
          (xmNbackground $F) 
          (xmNforeground $B))) 
      (erase $Ref) 
      (recordz current 
        (example $I $Widget notselected) $Ref) 
      (recorded state 
        (getId $GetId) $_) 
      (xtGetValues $GetId 
        (:: (xmNset $GetState))) 
      (det-if-then-else 
        (= $GetState True) 
        (copyId $I 1) otherwise)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (firstClauseClick $Widget $ClientData $CallData) 
    ( (xtAddEventHandler $Widget 
        (:: buttonPressMask) False copyClause $_) (xtAddTimeOut 1000 noDoubleClick $Widget $ID)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (copyClause $Widget $ClientData $CallData) 
    ( (recorded editor 
        (label $Label) $_) 
      (proxtGetDefaultCharset $DCharset) 
      (| 
        (det-if-then 
          (recorded editor 
            (editing $_ $_) $Ref) 
          (, 
            (erase $Ref) 
            (proxtStringToCharPtr Editor $LblCP1) 
            (xmStringCreate $LblCP1 $DCharset $LblS1) 
            (xtSetValues $Label 
              (:: (xmNlabelString $LblS1))))) otherwise) 
      (| 
        (det-if-then 
          (recorded current 
            (clause $Id $Widget $_) $_) 
          (, 
            (get-clause $Id $H $B $_ $_) 
            (xxmWriteToCharPtr 
              (portray-clause (= $H $B)) $CP) 
            (recorded editor 
              (textWidget $Editor) $_) 
            (xmTextSetString $Editor $CP) 
            (recordz editor 
              (editing rules $Id) $_) 
            (number-chars $Id $IdCS) 
            (append "Editor     editing rule " $IdCS $LblCS) 
            (atom-chars $LblAS $LblCS) 
            (proxtStringToCharPtr $LblAS $LblCP) 
            (xmStringCreate $LblCP $DCharset $LblS) 
            (xtSetValues $Label 
              (:: (xmNlabelString $LblS))))) 
        (det-if-then 
          (recorded current 
            (example $Id $Widget $_) $_) 
          (, 
            (get-example $Id $F $_) 
            (xxmWriteToCharPtr 
              (writeFullstop $F) $CP) 
            (recorded editor 
              (textWidget $Editor) $_) 
            (xmTextSetString $Editor $CP) 
            (recordz editor 
              (editing examples $Id) $_) 
            (number-chars $Id $IdCS) 
            (append "Editor     editing example " $IdCS $LblCS) 
            (atom-chars $LblAS $LblCS) 
            (proxtStringToCharPtr $LblAS $LblCP) 
            (xmStringCreate $LblCP $DCharset $LblS) 
            (xtSetValues $Label 
              (:: (xmNlabelString $LblS))))))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (noDoubleClick $Widget $IntervallId) 
    (xtRemoveEventHandler $Widget 
      (:: buttonPressMask) False copyClause $_))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (selectAll $Widget rules $CallData) 
    ( (recorded current 
        (clause $I $Widget notselected) $Ref) 
      (set-det) 
      (xtGetValues $Widget 
        (:: 
          (xmNbackground $B) 
          (xmNforeground $F))) 
      (xtSetValues $Widget 
        (:: 
          (xmNbackground $F) 
          (xmNforeground $B))) 
      (erase $Ref) 
      (recordz current 
        (clause $I $Widget selected) $Ref) 
      (selectAll $Widget rules $CallData)))
; 


  (= 
    (selectAll $Widget examples $CallData) 
    ( (recorded current 
        (example $I $Widget notselected) $Ref) 
      (set-det) 
      (xtGetValues $Widget 
        (:: 
          (xmNbackground $B) 
          (xmNforeground $F))) 
      (xtSetValues $Widget 
        (:: 
          (xmNbackground $F) 
          (xmNforeground $B))) 
      (erase $Ref) 
      (recordz current 
        (example $I $Widget selected) $Ref) 
      (selectAll $Widget examples $CallData)))
; 


  (= 
    (selectAll  $_ $_ $_) True)
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (unselectAll $Widget rules $CallData) 
    ( (recorded current 
        (clause $I $Widget selected) $Ref) 
      (set-det) 
      (xtGetValues $Widget 
        (:: 
          (xmNbackground $B) 
          (xmNforeground $F))) 
      (xtSetValues $Widget 
        (:: 
          (xmNbackground $F) 
          (xmNforeground $B))) 
      (erase $Ref) 
      (recordz current 
        (clause $I $Widget notselected) $Ref) 
      (unselectAll $Widget rules $CallData)))
; 


  (= 
    (unselectAll $Widget examples $CallData) 
    ( (recorded current 
        (example $I $Widget selected) $Ref) 
      (set-det) 
      (xtGetValues $Widget 
        (:: 
          (xmNbackground $B) 
          (xmNforeground $F))) 
      (xtSetValues $Widget 
        (:: 
          (xmNbackground $F) 
          (xmNforeground $B))) 
      (erase $Ref) 
      (recordz current 
        (example $I $Widget notselected) $Ref) 
      (unselectAll $Widget examples $CallData)))
; 


  (= 
    (unselectAll  $_ $_ $_) True)
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (deleteKnowledge $Widget rules $CallData) 
    ( (recorded current 
        (clause $I $Widget selected) $Ref) 
      (set-det) 
      (| 
        (det-if-then 
          (recorded editor 
            (editing rules $I) $_) 
          (clearEditor $_ $_ $_)) otherwise) 
      (xtGetValues $Widget 
        (:: 
          (xmNbackground $B) 
          (xmNforeground $F))) 
      (xtSetValues $Widget 
        (:: 
          (xmNbackground $F) 
          (xmNforeground $B))) 
      (erase $Ref) 
      (xtDestroyWidget $Widget) 
      (delete-clause $I) 
      (writeMessage ':- delete-clause(') 
      (writeMessage $I) 
      (writelnMessage ).) 
      (deleteKnowledge $Widget rules $CallData)))
; 

	
  (= 
    (deleteKnowledge $Widget examples $CallData) 
    ( (| 
        (det-if-then 
          (recorded editor 
            (editing examples $I) $_) 
          (clearEditor $_ $_ $_)) otherwise) 
      (recorded current 
        (example $I $Widget selected) $Ref) 
      (set-det) 
      (xtGetValues $Widget 
        (:: 
          (xmNbackground $B) 
          (xmNforeground $F))) 
      (xtSetValues $Widget 
        (:: 
          (xmNbackground $F) 
          (xmNforeground $B))) 
      (erase $Ref) 
      (xtDestroyWidget $Widget) 
      (delete-example $I) 
      (writeMessage ':- delete-example(') 
      (writeMessage $I) 
      (writelnMessage ).) 
      (deleteKnowledge $Widget examples $CallData)))
; 


  (= 
    (deleteKnowledge $_ $KindOfKnowledge $_) 
    ( (writeMessage '% selected ') 
      (writeMessage $KindOfKnowledge) 
      (writelnMessage ' deleted.') 
      (updateEvaluationLabel)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (deleteAllKnowledge $Widget rules $CallData) 
    ( (| 
        (det-if-then 
          (recorded editor 
            (editing rules $_) $_) 
          (clearEditor $_ $_ $_)) otherwise) 
      (recorded current 
        (clause $I $Widget $_) $Ref) 
      (set-det) 
      (xtGetValues $Widget 
        (:: 
          (xmNbackground $B) 
          (xmNforeground $F))) 
      (xtSetValues $Widget 
        (:: 
          (xmNbackground $F) 
          (xmNforeground $B))) 
      (erase $Ref) 
      (xtDestroyWidget $Widget) 
      (delete-clause $I) 
      (writeMessage ':- delete-clause(') 
      (writeMessage $I) 
      (writelnMessage ).) 
      (deleteAllKnowledge $Widget rules $CallData)))
; 

	
  (= 
    (deleteAllKnowledge $Widget examples $CallData) 
    ( (| 
        (det-if-then 
          (recorded editor 
            (editing examples $_) $_) 
          (clearEditor $_ $_ $_)) otherwise) 
      (recorded current 
        (example $I $Widget $_) $Ref) 
      (set-det) 
      (xtGetValues $Widget 
        (:: 
          (xmNbackground $B) 
          (xmNforeground $F))) 
      (xtSetValues $Widget 
        (:: 
          (xmNbackground $F) 
          (xmNforeground $B))) 
      (erase $Ref) 
      (xtDestroyWidget $Widget) 
      (delete-example $I) 
      (writeMessage ':- delete-example(') 
      (writeMessage $I) 
      (writelnMessage ).) 
      (deleteAllKnowledge $Widget examples $CallData)))
; 


  (= 
    (deleteAllKnowledge $_ $KindOfKnowledge $_) 
    ( (writeMessage '% all ') 
      (writeMessage $KindOfKnowledge) 
      (writelnMessage ' deleted.')))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (saveKnowledgeBegin $Widget $FileSelectionDialog $CallData) 
    ( (xtManageChild $FileSelectionDialog) 
      (xtAddCallback $FileSelectionDialog xmNokCallback saveKnowledgeEnd $FileSelectionDialog) 
      (xtAddCallback $FileSelectionDialog xmNcancelCallback cancelSaveFileSelect $FileSelectionDialog)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (saveKnowledgeEnd $Widget $FileSelectionDialog $CallData) 
    ( (xtUnmanageChild $FileSelectionDialog) 
      (xtRemoveCallback $FileSelectionDialog xmNokCallback saveKnowledgeEnd $FileSelectionDialog) 
      (xtRemoveCallback $FileSelectionDialog xmNcancelCallback cancelSaveFileSelect $FileSelectionDialog) 
      (sucheInListe 
        (value $FileNameString) $CallData) 
      (proxtGetDefaultCharset $DCharset) 
      (xmStringGetLtoR $FileNameString $DCharset $FileNameCP $_) 
      (proxtCharPtrToString $FileNameCP $FileName) 
      (| 
        (det-if-then 
          (midstring $FileName $_ .qof 0) 
          (, 
            (save-kb $FileName) 
            (writeMessage ':- save-kb(') 
            (writeMessage $FileName) 
            (writelnMessage ).))) 
        (det-if-then otherwise 
          (, 
            (print-kb $FileName) 
            (writeMessage ':- print-kb(') 
            (writeMessage $FileName) 
            (writelnMessage ).)))) 
      (writeMessage '% file "') 
      (writeMessage $FileName) 
      (writelnMessage " saved.)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (loadKnowledgeBegin $Widget $FileSelectionDialog $CallData) 
    ( (xtManageChild $FileSelectionDialog) 
      (xtAddCallback $FileSelectionDialog xmNokCallback loadKnowledgeEnd $FileSelectionDialog) 
      (xtAddCallback $FileSelectionDialog xmNcancelCallback cancelLoadFileSelect $FileSelectionDialog)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (loadKnowledgeEnd $Widget $FileSelectionDialog $CallData) 
    ( (xtUnmanageChild $FileSelectionDialog) 
      (xtRemoveCallback $FileSelectionDialog xmNokCallback loadKnowledgeEnd $FileSelectionDialog) 
      (xtRemoveCallback $FileSelectionDialog xmNcancelCallback cancelLoadFileSelect $FileSelectionDialog) 
      (sucheInListe 
        (value $FileNameString) $CallData) 
      (proxtGetDefaultCharset $DCharset) 
      (xmStringGetLtoR $FileNameString $DCharset $FileNameCP $_) 
      (proxtCharPtrToString $FileNameCP $FileName) 
      (| 
        (det-if-then 
          (midstring $FileName $_ .qof 0) 
          (, 
            (consult-kb $FileName) 
            (writeMessage ':- consult-kb(') 
            (writeMessage $FileName) 
            (writelnMessage ).))) 
        (det-if-then otherwise 
          (, 
            (init-kb $FileName) 
            (writeMessage ':- init-kb(') 
            (writeMessage $FileName) 
            (writelnMessage ).)))) 
      (writeMessage '% file "') 
      (writeMessage $FileName) 
      (writelnMessage " consulted.) 
      (refreshKnowledgeList $Widget rules $Calldata) 
      (refreshKnowledgeList $Widget examples $Calldata) 
      (updateEvaluationLabel)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (clearAllKnowledge $Widget $ClientData $CallData) 
    ( (deleteAllKnowledge $Widget rules $CallData) 
      (deleteAllKnowledge $Widget examples $CallData) 
      (clearEditor $_ $_ $_) 
      (writelnMessage ':- clear-kb.') 
      (writelnMessage '% knowledgebase cleared.') 
      (clear-kb) 
      (updateEvaluationLabel)))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (cancelLoadFileSelect $Widget $FileSelectionDialog $CallData) 
    ( (xtRemoveCallback $FileSelectionDialog xmNokCallback loadKnowledgeEnd $FileSelectionDialog) 
      (xtRemoveCallback $FileSelectionDialog xmNcancelCallback cancelLoadFileSelect $FileSelectionDialog) 
      (xtUnmanageChild $FileSelectionDialog)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (cancelSaveFileSelect $Widget $FileSelectionDialog $CallData) 
    ( (xtRemoveCallback $FileSelectionDialog xmNokCallback saveKnowledgeEnd $FileSelectionDialog) 
      (xtRemoveCallback $FileSelectionDialog xmNcancelCallback cancelSaveFileSelect $FileSelectionDialog) 
      (xtUnmanageChild $FileSelectionDialog)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (createKbButtons $Parent) 
    ( (xmCreateRowColumn $Parent ButtonRowColumn 
        (:: 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER) 
          (xmNnumColumns 1) 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN)) $ButtonRowColumn) 
      (xtManageChild $ButtonRowColumn) 
      (xmCreateFileSelectionDialog $ButtonRowColumn KBFileSelect Nil $KbFileSelectionDialog) 
      (xmCreatePushButton $ButtonRowColumn Load Nil $Load) 
      (xtManageChild $Load) 
      (xtAddCallback $Load xmNactivateCallback loadKnowledgeBegin $KbFileSelectionDialog) 
      (xmCreatePushButton $ButtonRowColumn Save Nil $Save) 
      (xtManageChild $Save) 
      (xtAddCallback $Save xmNactivateCallback saveKnowledgeBegin $KbFileSelectionDialog) 
      (xmCreatePushButton $ButtonRowColumn Clear Nil $Clear) 
      (xtManageChild $Clear) 
      (createYesNoPopup $Clear $YesNoPopup clearAllKnowledge $_)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (createKbSubButtons $Parent $KindOfKnowledge) 
    ( (xmCreateRowColumn $Parent ButtonRowColumn 
        (:: 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER) 
          (xmNnumColumns 2) 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN)) $ButtonRowColumn) 
      (xtManageChild $ButtonRowColumn) 
      (xmCreatePushButton $ButtonRowColumn 'View ...' Nil $View) 
      (xtManageChild $View) 
      (xmCreatePushButton $ButtonRowColumn 'Select All' Nil $Select) 
      (xtManageChild $Select) 
      (xtAddCallback $Select xmNactivateCallback selectAll $KindOfKnowledge) 
      (xmCreatePushButton $ButtonRowColumn 'Unselect All' Nil $Unselect) 
      (xtManageChild $Unselect) 
      (xtAddCallback $Unselect xmNactivateCallback unselectAll $KindOfKnowledge) 
      (xmCreatePushButton $ButtonRowColumn Refresh Nil $Refresh) 
      (xtManageChild $Refresh) 
      (xtAddCallback $Refresh xmNactivateCallback refreshKnowledgeList $KindOfKnowledge) 
      (xmCreatePushButton $ButtonRowColumn Delete Nil $Delete) 
      (xtManageChild $Delete) 
      (xtAddCallback $Delete xmNactivateCallback deleteKnowledge $KindOfKnowledge) 
      (| 
        (det-if-then 
          (= $KindOfKnowledge rules) 
          (, 
            (createViewRulesPopup $View) 
            (createLabelChangeButton $ButtonRowColumn))) 
        (det-if-then 
          (= $KindOfKnowledge examples) 
          (, 
            (createViewExamplesPopup $View) 
            (createClassChangeButtons $ButtonRowColumn))))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (createLabelChangeButton $Parent) 
    ( (xmCreatePushButton $Parent Label Nil $Label) 
      (xtManageChild $Label) 
      (xmCreateBulletinBoardDialog $Label 'Label Change' Nil $LabelChangeDialog) 
      (xtAddCallback $Label xmNactivateCallback popupDialog $LabelChangeDialog) 
      (xmCreateFrame $LabelChangeDialog LabelFrame Nil $LabelFrame) 
      (xtManageChild $LabelFrame) 
      (xmCreateRowColumn $LabelFrame labelChangeColumn 
        (:: 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-COLUMN)) $LabelChangeColumn) 
      (xtManageChild $LabelChangeColumn) 
      (proxtGetDefaultCharset $DCharset) 
      (proxtStringToCharPtr 'Change Label of all selected rules to:' $LCP) 
      (xmStringCreate $LCP $DCharset $LabelStr) 
      (xmCreateLabelGadget $LabelChangeColumn labelchange 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $LabelStr)) $KStatus) 
      (xtManageChild $KStatus) 
      (xmCreateText $LabelChangeColumn labelChangeText 
        (:: 
          (xmNeditable True) 
          (xmNeditMode xmSINGLE-LINE-EDIT) 
          (xmNwidth 150) 
          (xmNautoShowCursorPosition True)) $LabelChangeText) 
      (xtManageChild $LabelChangeText) 
      (xtAddActions (:: (action label-ok changeSelectedLabels $LabelChangeDialog))) 
      (proxtStringToCharPtr '<Key>Return: label-ok()' $TranslationString) 
      (xtParseTranslationTable $TranslationString $TranslationTable) 
      (xtOverrideTranslations $LabelChangeText $TranslationTable) 
      (xmCreateRowColumn $LabelChangeColumn buttonrow 
        (:: 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN) 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER)) $ButtonRow) 
      (xtManageChild $ButtonRow) 
      (xmCreatePushButton $ButtonRow OK! Nil $OK) 
      (xtManageChild $OK) 
      (xtAddCallback $OK xmNactivateCallback changeSelectedLabels 
        (:: $LabelChangeDialog $LabelChangeText)) 
      (xmCreatePushButton $ButtonRow Cancel Nil $Cancel) 
      (xtManageChild $Cancel) 
      (xtAddCallback $Cancel xmNactivateCallback cancelLabelChange $LabelChangeDialog)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (createSelectRulesPopup $Parent) 
    ( (xmCreateBulletinBoardDialog $Parent SelectRules Nil $SelectRulesDialog) 
      (xtAddCallback $Parent xmNactivateCallback popupDialog $SelectRulesDialog) 
      (xmCreateFrame $SelectRulesDialog SelectRulesFrame Nil $SelectRulesFrame) 
      (xtManageChild $SelectRulesFrame) 
      (xmCreateRowColumn $SelectRulesFrame selectRulesColumn 
        (:: 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-COLUMN)) $SelectRulesColumn) 
      (xtManageChild $SelectRulesColumn) 
      (proxtGetDefaultCharset $DCharset) 
      (proxtStringToCharPtr 'Select rules:' $TitleCP) 
      (xmStringCreate $TitleCP $DCharset $TitleStr) 
      (xmCreateLabelGadget $SelectRulesColumn title 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $TitleStr)) $TitleLabel) 
      (xtManageChild $TitleLabel) 
      (xmCreateText $SelectRulesColumn selectRulesText 
        (:: 
          (xmNeditable True) 
          (xmNeditMode xmSINGLE-LINE-EDIT) 
          (xmNwidth 150) 
          (xmNautoShowCursorPosition True)) $SelectRulesText) 
      (xtManageChild $SelectRulesText) 
      (xtAddActions (:: (action select-ok selectRules $SelectRulesDialog))) 
      (proxtStringToCharPtr '<Key>Return: select-ok()' $TranslationString) 
      (xtParseTranslationTable $TranslationString $TranslationTable) 
      (xtOverrideTranslations $SelectRulesText $TranslationTable) 
      (xmCreateRowColumn $SelectRulesColumn buttonrow 
        (:: 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN) 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER)) $ButtonRow) 
      (xtManageChild $ButtonRow) 
      (xmCreatePushButton $ButtonRow Select Nil $Select) 
      (xtManageChild $Select) 
      (xtAddCallback $Select xmNactivateCallback selectRules $SelectRulesDialog) 
      (xmCreatePushButton $ButtonRow 'Select All' Nil $SelectAll) 
      (xtManageChild $SelectAll) 
      (xtAddCallback $SelectAll xmNactivateCallback selectRules 
        (:: rules $SelectRulesDialog)) 
      (xmCreatePushButton $ButtonRow Cancel Nil $Cancel) 
      (xtManageChild $Cancel) 
      (xtAddCallback $Cancel xmNactivateCallback yesNoPopdown $SelectRulesDialog)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (createExamineRulesPopup $Parent) 
    ( (xmCreateBulletinBoardDialog $Parent ExamineRules Nil $ExamineRulesDialog) 
      (xtAddCallback $Parent xmNactivateCallback popupExamineRules $_) 
      (recordz rules 
        (examine dialog $ExamineRulesDialog) $_) 
      (xmCreateFrame $ExamineRulesDialog ExamineRulesFrame Nil $ExamineRulesFrame) 
      (xtManageChild $ExamineRulesFrame) 
      (xmCreateRowColumn $ExamineRulesFrame examineRulesColumn 
        (:: 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT)) $ExamineRulesColumn) 
      (xtManageChild $ExamineRulesColumn) 
      (xmCreateRowColumn $ExamineRulesColumn titlerow 
        (:: 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN) 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER)) $TitleRow) 
      (xtManageChild $TitleRow) 
      (proxtGetDefaultCharset $DCharset) 
      (proxtStringToCharPtr 'Examine rule:' $TitleCP) 
      (xmStringCreate $TitleCP $DCharset $TitleStr) 
      (xmCreateLabelGadget $TitleRow title 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $TitleStr)) $TitleLabel) 
      (xtManageChild $TitleLabel) 
      (xmCreateScrolledWindow $ExamineRulesColumn ruleSW 
        (:: 
          (xmNheight 200) 
          (xmNwidth 250) 
          (xmNlistSizePolicy xmCONSTANT) 
          (xmNscrollBarDisplayPolicy xmSTATIC) 
          (xmNscrollingPolicy xmAUTOMATIC)) $RuleSW) 
      (xtManageChild $RuleSW) 
      (xmCreateRowColumn $RuleSW ruleRC 
        (:: 
          (xmNadjustLast False) 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT) 
          (xmNrowColumnType xmWORK-AREA)) $RuleRC) 
      (recordz rules 
        (examineRC $RuleRC) $Ref) 
      (xtGetValues $RuleRC 
        (:: (xmNbackground $B))) 
      (xtGetValues $RuleSW 
        (:: (xmNclipWindow $CW))) 
      (xtSetValues $CW 
        (:: (xmNbackground $B))) 
      (xtManageChild $RuleRC) 
      (xmCreateRowColumn $ExamineRulesColumn buttonrow 
        (:: 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN) 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER)) $ButtonRow) 
      (xtManageChild $ButtonRow) 
      (xmCreatePushButton $ButtonRow Examine Nil $Examine) 
      (xtManageChild $Examine) 
      (xtAddCallback $Examine xmNactivateCallback showExaminedRule $_) 
      (showExaminedRule $_ $_ $_) 
      (xmCreatePushButton $ButtonRow Cancel Nil $Cancel) 
      (xtManageChild $Cancel) 
      (xtAddCallback $Cancel xmNactivateCallback cancelExamineRules $_)))
; 
	




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (showExaminedRule $Widget $RuleNoText $CallData) 
    ( (proxtGetDefaultCharset $DCharset) 
      (| 
        (det-if-then 
          (recorded rules 
            (examineText $OldText) $Ref) 
          (, 
            (erase $Ref) 
            (xtDestroyWidget $OldText))) otherwise) 
      (recorded rules 
        (examineRC $ExamineRC) $_) 
      (det-if-then-else 
        (, 
          (recorded current 
            (clause $RuleNo $_ selected) $_) 
          (get-clause $RuleNo $H $B $S $O)) 
        (, 
          (get-evaluation $RuleNo $Evaluation) 
          (= $Evaluation 
            (evaluation $E1 $E2 $E3 $E4 $E5 $E6 $E7 $E8 $E9)) 
          (or 
            (type-restriction $H $R) 
            (= $R 'No type restriction found!')) 
          (numbervars 
            (, $H $B) 0 $_) 
          (xxmWriteToString 
            (, 
              (show-kb-clause $RuleNo $H $B $O) 
              (nl) 
              (write type-restriction:) 
              (nl) 
              (write $R) 
              (nl)) $DCharset $XmS3) 
          (xxmWriteToString 
            (, 
              (nl) 
              (write evaluation:) 
              (nl) 
              (write $E1) 
              (nl) 
              (write $E2) 
              (nl) 
              (write $E3) 
              (nl) 
              (write $E4) 
              (nl) 
              (write $E5) 
              (nl) 
              (write $E6) 
              (nl) 
              (write $E7) 
              (nl) 
              (write $E8) 
              (nl) 
              (write $E9) 
              (nl)) $DCharset $XmS4) 
          (xmStringConcat $XmS3 $XmS4 $XmS) 
          (xmCreateLabel $ExamineRC anyClause 
            (:: 
              (xmNalignment xmALIGNMENT-BEGINNING) 
              (xmNlabelString $XmS) 
              (xmNlabelType xmSTRING)) $Label)) 
        (, 
          (atom-chars $ASno "No rule selected!") 
          (proxtStringToCharPtr $ASno $CPno) 
          (xmStringCreateLtoR $CPno $DCharset $XmSno) 
          (xmCreateLabel $ExamineRC anyClause 
            (:: 
              (xmNalignment xmALIGNMENT-BEGINNING) 
              (xmNlabelString $XmSno) 
              (xmNlabelType xmSTRING)) $Label))) 
      (xtManageChild $Label) 
      (recordz rules 
        (examineText $Label) $_) 
      (set-det)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (createViewRulesPopup $Parent) 
    ( (xmCreateBulletinBoardDialog $Parent ViewRules Nil $ViewRulesDialog) 
      (xtAddCallback $Parent xmNactivateCallback popupViewRules $_) 
      (xmCreateFrame $ViewRulesDialog ViewRulesFrame Nil $ViewRulesFrame) 
      (xtManageChild $ViewRulesFrame) 
      (xmCreateRowColumn $ViewRulesFrame viewRulesColumn 
        (:: 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT)) $ViewRulesColumn) 
      (xtManageChild $ViewRulesColumn) 
      (proxtGetDefaultCharset $DCharset) 
      (proxtStringToCharPtr 'View rules:' $TitleCP) 
      (xmStringCreate $TitleCP $DCharset $TitleStr) 
      (xmCreateLabelGadget $ViewRulesColumn title 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $TitleStr)) $TitleLabel) 
      (xtManageChild $TitleLabel) 
      (xmCreateFrame $ViewRulesColumn ViewLabelsFrame Nil $ViewLabelFrame) 
      (xtManageChild $ViewLabelFrame) 
      (xmCreateRowColumn $ViewLabelFrame labelrow 
        (:: 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN) 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER)) $LabelRow) 
      (xtManageChild $LabelRow) 
      (xmCreateRowColumn $LabelRow exLabColumn 
        (:: 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT)) $ExLabColumn) 
      (xtManageChild $ExLabColumn) 
      (xmCreateRowColumn $LabelRow viewLabColumn 
        (:: 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT)) $ViewLabColumn) 
      (xtManageChild $ViewLabColumn) 
      (proxtStringToCharPtr 'existing labels' $ExLabCP) 
      (xmStringCreate $ExLabCP $DCharset $ExLabStr) 
      (xmCreateLabelGadget $ExLabColumn subtitle 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $ExLabStr)) $ExLabLabel) 
      (xtManageChild $ExLabLabel) 
      (proxtStringToCharPtr 'viewed labels' $ViewLabCP) 
      (xmStringCreate $ViewLabCP $DCharset $ViewLabStr) 
      (xmCreateLabelGadget $ViewLabColumn subtitle 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $ViewLabStr)) $ViewLabLabel) 
      (xtManageChild $ViewLabLabel) 
      (xmCreateScrolledWindow $ExLabColumn exLabSW 
        (:: 
          (xmNheight 150) 
          (xmNwidth 140) 
          (xmNlistSizePolicy xmCONSTANT) 
          (xmNscrollBarDisplayPolicy xmSTATIC) 
          (xmNscrollingPolicy xmAUTOMATIC)) $ExLabSW) 
      (xtManageChild $ExLabSW) 
      (xmCreateRowColumn $ExLabSW exLabRC 
        (:: 
          (xmNadjustLast False) 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT) 
          (xmNrowColumnType xmWORK-AREA)) $ExLabRC) 
      (xtGetValues $ExLabRC 
        (:: (xmNbackground $ExLabB))) 
      (xtGetValues $ExLabSW 
        (:: (xmNclipWindow $ExLabCW))) 
      (xtSetValues $ExLabCW 
        (:: (xmNbackground $ExLabB))) 
      (xtManageChild $ExLabRC) 
      (xmCreateScrolledWindow $ViewLabColumn viewedLabSW 
        (:: 
          (xmNheight 150) 
          (xmNwidth 140) 
          (xmNlistSizePolicy xmCONSTANT) 
          (xmNscrollBarDisplayPolicy xmSTATIC) 
          (xmNscrollingPolicy xmAUTOMATIC)) $ViewLabSW) 
      (xtManageChild $ViewLabSW) 
      (xmCreateRowColumn $ViewLabSW viewLabRC 
        (:: 
          (xmNadjustLast False) 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT) 
          (xmNrowColumnType xmWORK-AREA)) $ViewLabRC) 
      (xtGetValues $ViewLabRC 
        (:: (xmNbackground $ViewLabB))) 
      (xtGetValues $ViewLabSW 
        (:: (xmNclipWindow $ViewLabCW))) 
      (xtSetValues $ViewLabCW 
        (:: (xmNbackground $ViewLabB))) 
      (xtManageChild $ViewLabRC) 
      (xmCreateFrame $ViewRulesColumn ViewClauseHeadFrame Nil $ViewClauseHeadFrame) 
      (xtManageChild $ViewClauseHeadFrame) 
      (xmCreateRowColumn $ViewClauseHeadFrame clauseHeadrow 
        (:: 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN) 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER)) $ClauseHeadRow) 
      (xtManageChild $ClauseHeadRow) 
      (xmCreateRowColumn $ClauseHeadRow exCHColumn 
        (:: 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT)) $ExCHColumn) 
      (xtManageChild $ExCHColumn) 
      (xmCreateRowColumn $ClauseHeadRow viewCHColumn 
        (:: 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT)) $ViewCHColumn) 
      (xtManageChild $ViewCHColumn) 
      (proxtStringToCharPtr 'existing clause heads' $ExCHCP) 
      (xmStringCreate $ExCHCP $DCharset $ExCHStr) 
      (xmCreateLabelGadget $ExCHColumn subtitle 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $ExCHStr)) $ExCHLabel) 
      (xtManageChild $ExCHLabel) 
      (proxtStringToCharPtr 'viewed clause heads' $ViewCHCP) 
      (xmStringCreate $ViewCHCP $DCharset $ViewCHStr) 
      (xmCreateLabelGadget $ViewCHColumn subtitle 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $ViewCHStr)) $ViewCHLabel) 
      (xtManageChild $ViewCHLabel) 
      (xmCreateScrolledWindow $ExCHColumn existingCHSW 
        (:: 
          (xmNheight 150) 
          (xmNwidth 140) 
          (xmNlistSizePolicy xmCONSTANT) 
          (xmNscrollBarDisplayPolicy xmSTATIC) 
          (xmNscrollingPolicy xmAUTOMATIC)) $ExCHSW) 
      (xtManageChild $ExCHSW) 
      (xmCreateRowColumn $ExCHSW exCHRC 
        (:: 
          (xmNadjustLast False) 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT) 
          (xmNrowColumnType xmWORK-AREA)) $ExCHRC) 
      (xtGetValues $ExCHRC 
        (:: (xmNbackground $ExCHB))) 
      (xtGetValues $ExCHSW 
        (:: (xmNclipWindow $ExCHCW))) 
      (xtSetValues $ExCHCW 
        (:: (xmNbackground $ExCHB))) 
      (xtManageChild $ExCHRC) 
      (xmCreateScrolledWindow $ViewCHColumn viewedCHSW 
        (:: 
          (xmNheight 150) 
          (xmNwidth 140) 
          (xmNlistSizePolicy xmCONSTANT) 
          (xmNscrollBarDisplayPolicy xmSTATIC) 
          (xmNscrollingPolicy xmAUTOMATIC)) $ViewCHSW) 
      (xtManageChild $ViewCHSW) 
      (xmCreateRowColumn $ViewCHSW viewCHRC 
        (:: 
          (xmNadjustLast False) 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT) 
          (xmNrowColumnType xmWORK-AREA)) $ViewCHRC) 
      (xtGetValues $ViewCHRC 
        (:: (xmNbackground $ViewCHB))) 
      (xtGetValues $ViewCHSW 
        (:: (xmNclipWindow $ViewCHCW))) 
      (xtSetValues $ViewCHCW 
        (:: (xmNbackground $ViewCHB))) 
      (xtManageChild $ViewCHRC) 
      (xmCreateRowColumn $ViewRulesColumn minMaxRow 
        (:: 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN) 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER)) $MinMaxRow) 
      (xtManageChild $MinMaxRow) 
      (proxtStringToCharPtr 'Min: ' $MinLabelChrPtr) 
      (xmStringCreate $MinLabelChrPtr $DCharset $MinLabelStr) 
      (xmCreateLabelGadget $MinMaxRow minLabel 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $MinLabelStr)) $MinLabel) 
      (xtManageChild $MinLabel) 
      (xmCreateRowColumn $MinMaxRow Min 
        (:: 
          (xmNadjustLast True) 
          (xmNmarginHeight 0) 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-TIGHT)) $MinRow) 
      (xtManageChild $MinRow) 
      (xmCreatePushButton $MinRow < Nil $Lower) 
      (xtManageChild $Lower) 
      (xmCreateText $MinRow argumentText 
        (:: 
          (xmNeditable True) 
          (xmNeditMode xmSINGLE-LINE-EDIT) 
          (xmNwidth 30) 
          (xmNautoShowCursorPosition True)) $MinText) 
      (proxtStringToCharPtr 1 $MinChrPtr) 
      (xmTextSetString $MinText $MinChrPtr) 
      (xtManageChild $MinText) 
      (recordz rules 
        (viewMin $MinText) $_) 
      (xtAddCallback $Lower xmNactivateCallback lowerMin $MinText) 
      (xmCreatePushButton $MinRow > Nil $Raise) 
      (xtManageChild $Raise) 
      (xtAddCallback $Raise xmNactivateCallback raiseMin $MinText) 
      (proxtStringToCharPtr 'Max: ' $MaxLabelChrPtr) 
      (xmStringCreate $MaxLabelChrPtr $DCharset $MaxLabelStr) 
      (xmCreateLabelGadget $MinMaxRow maxLabel 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $MaxLabelStr)) $MaxLabel) 
      (xtManageChild $MaxLabel) 
      (xmCreateRowColumn $MinMaxRow Max 
        (:: 
          (xmNadjustLast True) 
          (xmNmarginHeight 0) 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-TIGHT)) $MaxRow) 
      (xtManageChild $MaxRow) 
      (xmCreatePushButton $MaxRow < Nil $LowerMax) 
      (xtManageChild $LowerMax) 
      (xmCreateText $MaxRow argumentText 
        (:: 
          (xmNeditable True) 
          (xmNeditMode xmSINGLE-LINE-EDIT) 
          (xmNwidth 30) 
          (xmNautoShowCursorPosition True)) $MaxText) 
      (proxtStringToCharPtr 100 $MaxChrPtr) 
      (xmTextSetString $MaxText $MaxChrPtr) 
      (xtManageChild $MaxText) 
      (recordz rules 
        (viewMax $MaxText) $_) 
      (xtAddCallback $LowerMax xmNactivateCallback lowerMax $MaxText) 
      (xmCreatePushButton $MaxRow > Nil $RaiseMax) 
      (xtManageChild $RaiseMax) 
      (xtAddCallback $RaiseMax xmNactivateCallback raiseMax $MaxText) 
      (recordz rules 
        (view all) $_) 
      (recordz rules 
        (view labels Nil) $_) 
      (recordz rules 
        (view clause-heads Nil) $_) 
      (recordz rules 
        (view dialog $ViewRulesDialog) $_) 
      (recordz rules 
        (view exLabRC $ExLabRC) $_) 
      (recordz rules 
        (view viewLabRC $ViewLabRC) $_) 
      (recordz rules 
        (view exCHRC $ExCHRC) $_) 
      (recordz rules 
        (view viewCHRC $ViewCHRC) $_) 
      (xmCreateRowColumn $ViewRulesColumn buttonrow 
        (:: 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN) 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER)) $ButtonRow) 
      (xtManageChild $ButtonRow) 
      (xmCreatePushButton $ButtonRow View Nil $View) 
      (xtManageChild $View) 
      (xtAddCallback $View xmNactivateCallback viewRules $_) 
      (xmCreatePushButton $ButtonRow 'View All' Nil $ViewAll) 
      (xtManageChild $ViewAll) 
      (xtAddCallback $ViewAll xmNactivateCallback viewAllRules $_) 
      (xmCreatePushButton $ButtonRow Cancel Nil $Cancel) 
      (xtManageChild $Cancel) 
      (xtAddCallback $Cancel xmNactivateCallback cancelViewRules $_)))
; 





;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (createViewExamplesPopup $Parent) 
    ( (xmCreateBulletinBoardDialog $Parent ViewExamples Nil $ViewExamplesDialog) 
      (xtAddCallback $Parent xmNactivateCallback popupViewExamples $_) 
      (xmCreateFrame $ViewExamplesDialog ViewExamplesFrame Nil $ViewExamplesFrame) 
      (xtManageChild $ViewExamplesFrame) 
      (xmCreateRowColumn $ViewExamplesFrame viewExamplesColumn 
        (:: 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT)) $ViewExamplesColumn) 
      (xtManageChild $ViewExamplesColumn) 
      (proxtGetDefaultCharset $DCharset) 
      (proxtStringToCharPtr 'View examples:' $TitleCP) 
      (xmStringCreate $TitleCP $DCharset $TitleStr) 
      (xmCreateLabelGadget $ViewExamplesColumn title 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $TitleStr)) $TitleLabel) 
      (xtManageChild $TitleLabel) 
      (xmCreateFrame $ViewExamplesColumn ViewClauseHeadFrame Nil $ViewClauseHeadFrame) 
      (xtManageChild $ViewClauseHeadFrame) 
      (xmCreateRowColumn $ViewClauseHeadFrame clauseHeadrow 
        (:: 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN) 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER)) $ClauseHeadRow) 
      (xtManageChild $ClauseHeadRow) 
      (xmCreateRowColumn $ClauseHeadRow exCHColumn 
        (:: 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT)) $ExCHColumn) 
      (xtManageChild $ExCHColumn) 
      (xmCreateRowColumn $ClauseHeadRow viewCHColumn 
        (:: 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT)) $ViewCHColumn) 
      (xtManageChild $ViewCHColumn) 
      (proxtStringToCharPtr 'existing clause heads' $ExCHCP) 
      (xmStringCreate $ExCHCP $DCharset $ExCHStr) 
      (xmCreateLabelGadget $ExCHColumn subtitle 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $ExCHStr)) $ExCHLabel) 
      (xtManageChild $ExCHLabel) 
      (proxtStringToCharPtr 'viewed clause heads' $ViewCHCP) 
      (xmStringCreate $ViewCHCP $DCharset $ViewCHStr) 
      (xmCreateLabelGadget $ViewCHColumn subtitle 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $ViewCHStr)) $ViewCHLabel) 
      (xtManageChild $ViewCHLabel) 
      (xmCreateScrolledWindow $ExCHColumn existingCHSW 
        (:: 
          (xmNheight 150) 
          (xmNwidth 140) 
          (xmNlistSizePolicy xmCONSTANT) 
          (xmNscrollBarDisplayPolicy xmSTATIC) 
          (xmNscrollingPolicy xmAUTOMATIC)) $ExCHSW) 
      (xtManageChild $ExCHSW) 
      (xmCreateRowColumn $ExCHSW exCHRC 
        (:: 
          (xmNadjustLast False) 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT) 
          (xmNrowColumnType xmWORK-AREA)) $ExCHRC) 
      (xtGetValues $ExCHRC 
        (:: (xmNbackground $ExCHB))) 
      (xtGetValues $ExCHSW 
        (:: (xmNclipWindow $ExCHCW))) 
      (xtSetValues $ExCHCW 
        (:: (xmNbackground $ExCHB))) 
      (xtManageChild $ExCHRC) 
      (xmCreateScrolledWindow $ViewCHColumn viewedCHSW 
        (:: 
          (xmNheight 150) 
          (xmNwidth 140) 
          (xmNlistSizePolicy xmCONSTANT) 
          (xmNscrollBarDisplayPolicy xmSTATIC) 
          (xmNscrollingPolicy xmAUTOMATIC)) $ViewCHSW) 
      (xtManageChild $ViewCHSW) 
      (xmCreateRowColumn $ViewCHSW viewCHRC 
        (:: 
          (xmNadjustLast False) 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT) 
          (xmNrowColumnType xmWORK-AREA)) $ViewCHRC) 
      (xtGetValues $ViewCHRC 
        (:: (xmNbackground $ViewCHB))) 
      (xtGetValues $ViewCHSW 
        (:: (xmNclipWindow $ViewCHCW))) 
      (xtSetValues $ViewCHCW 
        (:: (xmNbackground $ViewCHB))) 
      (xtManageChild $ViewCHRC) 
      (xmCreateRowColumn $ViewExamplesColumn minMaxRow 
        (:: 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN) 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER)) $MinMaxRow) 
      (xtManageChild $MinMaxRow) 
      (proxtStringToCharPtr 'Min: ' $MinLabelChrPtr) 
      (xmStringCreate $MinLabelChrPtr $DCharset $MinLabelStr) 
      (xmCreateLabelGadget $MinMaxRow minLabel 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $MinLabelStr)) $MinLabel) 
      (xtManageChild $MinLabel) 
      (xmCreateRowColumn $MinMaxRow Min 
        (:: 
          (xmNadjustLast True) 
          (xmNmarginHeight 0) 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-TIGHT)) $MinRow) 
      (xtManageChild $MinRow) 
      (xmCreatePushButton $MinRow < Nil $Lower) 
      (xtManageChild $Lower) 
      (xmCreateText $MinRow argumentText 
        (:: 
          (xmNeditable True) 
          (xmNeditMode xmSINGLE-LINE-EDIT) 
          (xmNwidth 30) 
          (xmNautoShowCursorPosition True)) $MinText) 
      (proxtStringToCharPtr 1 $MinChrPtr) 
      (xmTextSetString $MinText $MinChrPtr) 
      (xtManageChild $MinText) 
      (recordz examples 
        (viewMin $MinText) $_) 
      (xtAddCallback $Lower xmNactivateCallback lowerMin $MinText) 
      (xmCreatePushButton $MinRow > Nil $Raise) 
      (xtManageChild $Raise) 
      (xtAddCallback $Raise xmNactivateCallback raiseMin $MinText) 
      (proxtStringToCharPtr 'Max: ' $MaxLabelChrPtr) 
      (xmStringCreate $MaxLabelChrPtr $DCharset $MaxLabelStr) 
      (xmCreateLabelGadget $MinMaxRow maxLabel 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $MaxLabelStr)) $MaxLabel) 
      (xtManageChild $MaxLabel) 
      (xmCreateRowColumn $MinMaxRow Max 
        (:: 
          (xmNadjustLast True) 
          (xmNmarginHeight 0) 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-TIGHT)) $MaxRow) 
      (xtManageChild $MaxRow) 
      (xmCreatePushButton $MaxRow < Nil $LowerMax) 
      (xtManageChild $LowerMax) 
      (xmCreateText $MaxRow argumentText 
        (:: 
          (xmNeditable True) 
          (xmNeditMode xmSINGLE-LINE-EDIT) 
          (xmNwidth 30) 
          (xmNautoShowCursorPosition True)) $MaxText) 
      (proxtStringToCharPtr 100 $MaxChrPtr) 
      (xmTextSetString $MaxText $MaxChrPtr) 
      (xtManageChild $MaxText) 
      (recordz examples 
        (viewMax $MaxText) $_) 
      (xtAddCallback $LowerMax xmNactivateCallback lowerMax $MaxText) 
      (xmCreatePushButton $MaxRow > Nil $RaiseMax) 
      (xtManageChild $RaiseMax) 
      (xtAddCallback $RaiseMax xmNactivateCallback raiseMax $MaxText) 
      (recordz examples 
        (view all) $_) 
      (recordz examples 
        (view clause-heads Nil) $_) 
      (recordz examples 
        (view dialog $ViewExamplesDialog) $_) 
      (recordz examples 
        (view exCHRC $ExCHRC) $_) 
      (recordz examples 
        (view viewCHRC $ViewCHRC) $_) 
      (xmCreateRowColumn $ViewExamplesColumn buttonrow 
        (:: 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN) 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER)) $ButtonRow) 
      (xtManageChild $ButtonRow) 
      (xmCreatePushButton $ButtonRow View Nil $View) 
      (xtManageChild $View) 
      (xtAddCallback $View xmNactivateCallback viewExamples $_) 
      (xmCreatePushButton $ButtonRow 'View All' Nil $ViewAll) 
      (xtManageChild $ViewAll) 
      (xtAddCallback $ViewAll xmNactivateCallback viewAllExamples $_) 
      (xmCreatePushButton $ButtonRow Cancel Nil $Cancel) 
      (xtManageChild $Cancel) 
      (xtAddCallback $Cancel xmNactivateCallback cancelViewExamples $_)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (changeSelectedLabels $Widget 
      (:: $LabelChangeDialog $LabelText) $CallData) 
    ( (xtUnmanageChild $LabelChangeDialog) 
      (xmTextGetString $LabelText $LabelCP) 
      (proxtCharPtrToString $LabelCP $LabelAS) 
      (setSelectedLabels $LabelAS)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (setSelectedLabels $Label) 
    ( (recorded current 
        (clause $Id $Widget selected) $Ref) 
      (erase $Ref) 
      (get-clause $Id $H $B $CL $_) 
      (delete-clause $Id) 
      (store-clause $_ $CL $Label $Id) 
      (setSelectedLabels $Label) 
      (recorda current 
        (clause $Id $Widget selected) $_) 
      (addRuleItem $Id $H $B $CL $Label) 
      (updateEvaluationLabel)))
; 

  (= 
    (setSelectedLabels $_) 
    (set-det))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (cancelLabelChange $Widget $LabelChangeDialog $CallData) 
    (xtUnmanageChild $LabelChangeDialog))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (createClassChangeButtons $Parent) 
    ( (xmCreateRowColumn $Parent 'Class Change' 
        (:: 
          (xmNadjustLast True) 
          (xmNmarginHeight 0) 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN) 
          (xmNrowColumnType xmWORK-AREA)) $ClassChangeRow) 
      (xtManageChild $ClassChangeRow) 
      (xmCreatePushButton $ClassChangeRow + Nil $Positive) 
      (xtManageChild $Positive) 
      (xtAddCallback $Positive xmNactivateCallback classChangeSelected +) 
      (xmCreatePushButton $ClassChangeRow - Nil $Negative) 
      (xtManageChild $Negative) 
      (xtAddCallback $Negative xmNactivateCallback classChangeSelected -) 
      (xmCreatePushButton $ClassChangeRow ? Nil $Quest) 
      (xtManageChild $Quest) 
      (xtAddCallback $Quest xmNactivateCallback classChangeSelected ?)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (classChangeSelected $Widget $Classification $CallData) 
    ( (recorded current 
        (example $Id $Label selected) $Ref) 
      (erase $Ref) 
      (get-example $Id $Fact $_) 
      (delete-example $Id) 
      (store-ex $Fact $Classification $Id) 
      (classChangeSelected $Widget $Classification $CallData) 
      (recorda current 
        (example $Id $Label selected) $_) 
      (addExampleItem $Id $Fact $Classification) 
      (updateEvaluationLabel)))
; 

  (= 
    (classChangeSelected $_ $_ $_) 
    (set-det))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (createArgumentArea $Parent) 
    ( (xmCreateFrame $Parent Arguments Nil $ArgumentFrame) 
      (xtManageChild $ArgumentFrame) 
      (xmCreateRowColumn $ArgumentFrame Arguments 
        (:: 
          (xmNwidth 300) 
          (xmNadjustLast True) 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT) 
          (xmNrowColumnType xmWORK-AREA)) $ArgumentColumn) 
      (xtManageChild $ArgumentColumn) 
      (proxtStringToCharPtr Arguments $ArgumentChrPtr) 
      (proxtGetDefaultCharset $DCharset) 
      (xmStringCreate $ArgumentChrPtr $DCharset $ArgumentStr) 
      (xmCreateLabelGadget $ArgumentColumn argumentLabel 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $ArgumentStr)) $ArgumentLabel) 
      (xtManageChild $ArgumentLabel) 
      (xmCreateRowColumn $ArgumentColumn kommandoRC 
        (:: 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN) 
          (xmNnumColumns 2)) $ArgumentRow) 
      (xtManageChild $ArgumentRow) 
      (createArguments $ArgumentRow 12) 
      (xmCreateRowColumn $ArgumentColumn buttonrow 
        (:: 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-TIGHT) 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER)) $ButtonRow) 
      (xtManageChild $ButtonRow) 
      (xmCreateToggleButton $ButtonRow 'Get Id       ' Nil $GetId) 
      (xtManageChild $GetId) 
      (recordz state 
        (getId $GetId) $_) 
      (xmCreatePushButton $ButtonRow Clear Nil $Clear) 
      (xtManageChild $Clear) 
      (xtAddCallback $Clear xmNactivateCallback clearArguments 12) 
      (proxtStringToCharPtr '         Depth: ' $DepthChrPtr) 
      (xmStringCreate $DepthChrPtr $DCharset $DepthStr) 
      (xmCreateLabelGadget $ButtonRow depthLabel 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $DepthStr)) $DepthLabel) 
      (xtManageChild $DepthLabel) 
      (xmCreateRowColumn $ButtonRow Depth 
        (:: 
          (xmNadjustLast True) 
          (xmNmarginHeight 0) 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-TIGHT)) $DepthRow) 
      (xtManageChild $DepthRow) 
      (xmCreatePushButton $DepthRow < Nil $Lower) 
      (xtManageChild $Lower) 
      (xmCreateText $DepthRow argumentText 
        (:: 
          (xmNeditable True) 
          (xmNeditMode xmSINGLE-LINE-EDIT) 
          (xmNwidth 30) 
          (xmNautoShowCursorPosition True)) $DText) 
      (proxtStringToCharPtr 5 $DChrPtr) 
      (xmTextSetString $DText $DChrPtr) 
      (xtManageChild $DText) 
      (recordz state 
        (argumentWidget depth $DText) $_) 
      (xtAddCallback $Lower xmNactivateCallback lowerDepth $DText) 
      (xmCreatePushButton $DepthRow > Nil $Raise) 
      (xtManageChild $Raise) 
      (xtAddCallback $Raise xmNactivateCallback raiseDepth $DText)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (createArguments $_ 0) 
    (set-det))
; 

  (= 
    (createArguments $Parent $Count) 
    ( (is $C1 
        (- $Count 1)) 
      (createArguments $Parent $C1) 
      (xmCreateText $Parent argumentText 
        (:: 
          (xmNeditable True) 
          (xmNeditMode xmSINGLE-LINE-EDIT) 
          (xmNwidth 50) 
          (xmNautoShowCursorPosition True)) $ArgumentText) 
      (xtManageChild $ArgumentText) 
      (recordz state 
        (argumentWidget $Count $ArgumentText) $_)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (clearArguments  $Widget 0 $CallData) True)
; 

  (= 
    (clearArguments $Widget $Count $CallData) 
    ( (is $C1 
        (- $Count 1)) 
      (clearArguments $Widget $C1 $CallData) 
      (recorded state 
        (argumentWidget $Count $Text) $_) 
      (proxtStringToCharPtr '' $EmptyChrPtr) 
      (xmTextSetString $Text $EmptyChrPtr)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (lowerDepth $Widget $DepthText $CallData) 
    ( (xmTextGetString $DepthText $OldDepthCP) 
      (proxtCharPtrToString $OldDepthCP $OldDepthStr) 
      (atom-chars $OldDepthStr $OldDepthC) 
      (number-chars $OldDepth $OldDepthC) 
      (| 
        (det-if-then 
          (= $OldDepth 0) 
          (is $NewDepth 100)) 
        (det-if-then otherwise 
          (is $NewDepth 
            (- $OldDepth 1)))) 
      (number-chars $NewDepth $NewDepthC) 
      (atom-chars $NewDepthStr $NewDepthC) 
      (proxtStringToCharPtr $NewDepthStr $NewDepthCP) 
      (xmTextSetString $DepthText $NewDepthCP)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (lowerID $Widget $IDText $CallData) 
    ( (xmTextGetString $IDText $OldIDCP) 
      (proxtCharPtrToString $OldIDCP $OldIDStr) 
      (atom-chars $OldIDStr $OldIDC) 
      (number-chars $OldID $OldIDC) 
      (| 
        (det-if-then 
          (= $OldID 0) 
          (is $NewID 9999)) 
        (det-if-then otherwise 
          (is $NewID 
            (- $OldID 1)))) 
      (number-chars $NewID $NewIDC) 
      (atom-chars $NewIDStr $NewIDC) 
      (proxtStringToCharPtr $NewIDStr $NewIDCP) 
      (xmTextSetString $IDText $NewIDCP)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (lowerMin $Widget $MinText $CallData) 
    ( (| 
        (det-if-then 
          (recorded rules 
            (viewMin $MinText) $_) 
          (= $KindOfKnowledge rules)) 
        (det-if-then otherwise 
          (= $KindOfKnowledge examples))) 
      (xmTextGetString $MinText $OldMinCP) 
      (proxtCharPtrToString $OldMinCP $OldMinStr) 
      (atom-chars $OldMinStr $OldMinC) 
      (number-chars $OldMin $OldMinC) 
      (recorded $KindOfKnowledge 
        (viewMax $MaxText) $_) 
      (xmTextGetString $MaxText $MaxCP) 
      (proxtCharPtrToString $MaxCP $MaxStr) 
      (atom-chars $MaxStr $MaxC) 
      (number-chars $Max $MaxC) 
      (| 
        (det-if-then 
          (=< $OldMin 1) 
          (is $NewMin $Max)) 
        (det-if-then otherwise 
          (is $NewMin 
            (- $OldMin 1)))) 
      (number-chars $NewMin $NewMinC) 
      (atom-chars $NewMinStr $NewMinC) 
      (proxtStringToCharPtr $NewMinStr $NewMinCP) 
      (xmTextSetString $MinText $NewMinCP)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (lowerMax $Widget $MaxText $CallData) 
    ( (| 
        (det-if-then 
          (recorded rules 
            (viewMin $MinText) $_) 
          (= $KindOfKnowledge rules)) 
        (det-if-then otherwise 
          (= $KindOfKnowledge examples))) 
      (xmTextGetString $MaxText $OldMaxCP) 
      (proxtCharPtrToString $OldMaxCP $OldMaxStr) 
      (atom-chars $OldMaxStr $OldMaxC) 
      (number-chars $OldMax $OldMaxC) 
      (recorded $KindOfKnowledge 
        (viewMin $MinText) $_) 
      (xmTextGetString $MinText $MinCP) 
      (proxtCharPtrToString $MinCP $MinStr) 
      (atom-chars $MinStr $MinC) 
      (number-chars $Min $MinC) 
      (| 
        (det-if-then 
          (=< $OldMax $Min) 
          (is $NewMax 9999)) 
        (det-if-then otherwise 
          (is $NewMax 
            (- $OldMax 1)))) 
      (number-chars $NewMax $NewMaxC) 
      (atom-chars $NewMaxStr $NewMaxC) 
      (proxtStringToCharPtr $NewMaxStr $NewMaxCP) 
      (xmTextSetString $MaxText $NewMaxCP)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (raiseDepth $Widget $DepthText $CallData) 
    ( (xmTextGetString $DepthText $OldDepthCP) 
      (proxtCharPtrToString $OldDepthCP $OldDepthStr) 
      (atom-chars $OldDepthStr $OldDepthC) 
      (number-chars $OldDepth $OldDepthC) 
      (| 
        (det-if-then 
          (= $OldDepth 100) 
          (is $NewDepth 0)) 
        (det-if-then otherwise 
          (is $NewDepth 
            (+ $OldDepth 1)))) 
      (number-chars $NewDepth $NewDepthC) 
      (atom-chars $NewDepthStr $NewDepthC) 
      (proxtStringToCharPtr $NewDepthStr $NewDepthCP) 
      (xmTextSetString $DepthText $NewDepthCP)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (raiseID $Widget $IDText $CallData) 
    ( (xmTextGetString $IDText $OldIDCP) 
      (proxtCharPtrToString $OldIDCP $OldIDStr) 
      (atom-chars $OldIDStr $OldIDC) 
      (number-chars $OldID $OldIDC) 
      (| 
        (det-if-then 
          (= $OldID 9999) 
          (is $NewID 0)) 
        (det-if-then otherwise 
          (is $NewID 
            (+ $OldID 1)))) 
      (number-chars $NewID $NewIDC) 
      (atom-chars $NewIDStr $NewIDC) 
      (proxtStringToCharPtr $NewIDStr $NewIDCP) 
      (xmTextSetString $IDText $NewIDCP)))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (raiseMin $Widget $MinText $CallData) 
    ( (| 
        (det-if-then 
          (recorded rules 
            (viewMin $MinText) $_) 
          (= $KindOfKnowledge rules)) 
        (det-if-then otherwise 
          (= $KindOfKnowledge examples))) 
      (xmTextGetString $MinText $OldMinCP) 
      (proxtCharPtrToString $OldMinCP $OldMinStr) 
      (atom-chars $OldMinStr $OldMinC) 
      (number-chars $OldMin $OldMinC) 
      (recorded $KindOfKnowledge 
        (viewMax $MaxText) $_) 
      (xmTextGetString $MaxText $MaxCP) 
      (proxtCharPtrToString $MaxCP $MaxStr) 
      (atom-chars $MaxStr $MaxC) 
      (number-chars $Max $MaxC) 
      (| 
        (det-if-then 
          (>= $OldMin $Max) 
          (is $NewMin 1)) 
        (det-if-then otherwise 
          (is $NewMin 
            (+ $OldMin 1)))) 
      (number-chars $NewMin $NewMinC) 
      (atom-chars $NewMinStr $NewMinC) 
      (proxtStringToCharPtr $NewMinStr $NewMinCP) 
      (xmTextSetString $MinText $NewMinCP)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (raiseMax $Widget $MaxText $CallData) 
    ( (| 
        (det-if-then 
          (recorded rules 
            (viewMin $MinText) $_) 
          (= $KindOfKnowledge rules)) 
        (det-if-then otherwise 
          (= $KindOfKnowledge examples))) 
      (xmTextGetString $MaxText $OldMaxCP) 
      (proxtCharPtrToString $OldMaxCP $OldMaxStr) 
      (atom-chars $OldMaxStr $OldMaxC) 
      (number-chars $OldMax $OldMaxC) 
      (recorded $KindOfKnowledge 
        (viewMin $MinText) $_) 
      (xmTextGetString $MinText $MinCP) 
      (proxtCharPtrToString $MinCP $MinStr) 
      (atom-chars $MinStr $MinC) 
      (number-chars $Min $MinC) 
      (| 
        (det-if-then 
          (>= $OldMax 9999) 
          (is $NewMax $Min)) 
        (det-if-then otherwise 
          (is $NewMax 
            (+ $OldMax 1)))) 
      (number-chars $NewMax $NewMaxC) 
      (atom-chars $NewMaxStr $NewMaxC) 
      (proxtStringToCharPtr $NewMaxStr $NewMaxCP) 
      (xmTextSetString $MaxText $NewMaxCP)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (copyId $_ 13) 
    (set-det))
; 

  (= 
    (copyId $Id $Count) 
    ( (recorded state 
        (argumentWidget $Count $Text) $_) 
      (xmTextGetLastPosition $Text $Length) 
      (= $Length 0) 
      (set-det) 
      (number-chars $Id $IdChars) 
      (atom-chars $IdStr $IdChars) 
      (proxtStringToCharPtr $IdStr $IdCharPtr) 
      (xmTextSetString $Text $IdCharPtr)))
; 

  (= 
    (copyId $Id $Count) 
    ( (is $C1 
        (+ $Count 1)) (copyId $Id $C1)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (createCommandArea $Parent) 
    ( (xmCreateFrame $Parent Controll Nil $ControllFrame) 
      (xtManageChild $ControllFrame) 
      (xmCreateRowColumn $ControllFrame controll 
        (:: 
          (xmNwidth 300) 
          (xmNadjustLast True) 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT) 
          (xmNrowColumnType xmWORK-AREA)) $ControllRowColumn) 
      (xtManageChild $ControllRowColumn) 
      (xmCreateRowColumn $ControllRowColumn kommandoRC 
        (:: 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN)) $KommandoRowColumn) 
      (xtManageChild $KommandoRowColumn) 
      (proxtStringToCharPtr Comand: $KommandoChrPtr) 
      (proxtGetDefaultCharset $DCharset) 
      (xmStringCreate $KommandoChrPtr $DCharset $KommandoStr) 
      (xmCreateLabelGadget $KommandoRowColumn kommandoLabel 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $KommandoStr)) $Klabel) 
      (xtManageChild $Klabel) 
      (proxtStringToCharPtr '          Input' $StatusCP) 
      (xmStringCreate $StatusCP $DCharset $StatusStr) 
      (xmCreateLabelGadget $KommandoRowColumn kommandoStatus 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $StatusStr)) $KStatus) 
      (xtManageChild $KStatus) 
      (xmCreateText $ControllRowColumn kommandotext 
        (:: 
          (xmNeditable True) 
          (xmNeditMode xmSINGLE-LINE-EDIT) 
          (xmNwidth 290) 
          (xmNautoShowCursorPosition True)) $Ktext) 
      (xtManageChild $Ktext) 
      (xtAddActions (:: (action ok doKommando (:: $Ktext $KStatus)))) 
      (proxtStringToCharPtr '<Key>Return: ok()' $TranslationString) 
      (xtParseTranslationTable $TranslationString $TranslationTable) 
      (xtOverrideTranslations $Ktext $TranslationTable) 
      (xmCreateRowColumn $ControllRowColumn buttonrow 
        (:: 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN) 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER)) $ButtonRow) 
      (xtManageChild $ButtonRow) 
      (xmCreatePushButton $ButtonRow OK! Nil $OK) 
      (xtManageChild $OK) 
      (xmCreatePushButton $ButtonRow Clear Nil $Clear) 
      (xtManageChild $Clear) 
      (xmCreatePushButton $ButtonRow 'Quit X-Miles' Nil $Beenden) 
      (createYesNoPopup $Beenden $YesNoPopup beenden $_) 
      (xtManageChild $Beenden) 
      (xtSetValues $ControllRowColumn 
        (:: (xmNdefaultButton $OK))) 
      (xtAddCallback $OK xmNactivateCallback doKommando 
        (:: $Ktext $KStatus)) 
      (xtAddCallback $Clear xmNactivateCallback doEmptyKommando $Ktext)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (createFunctionArea $Parent) 
    ( (xmCreateFrame $Parent Functions Nil $FunctionFrame) 
      (xtManageChild $FunctionFrame) 
      (xmCreateRowColumn $FunctionFrame Function 
        (:: 
          (xmNwidth 300) 
          (xmNadjustLast True) 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT) 
          (xmNrowColumnType xmWORK-AREA)) $FunctionColumn) 
      (xtManageChild $FunctionColumn) 
      (proxtStringToCharPtr 'Learning Operators' $FunctionChrPtr) 
      (proxtGetDefaultCharset $DCharset) 
      (xmStringCreate $FunctionChrPtr $DCharset $FunctionStr) 
      (xmCreateLabelGadget $FunctionColumn functionLabel 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $FunctionStr)) $FunctionLabel) 
      (xtManageChild $FunctionLabel) 
      (xmCreateRowColumn $FunctionColumn functionRC 
        (:: 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN) 
          (xmNnumColumns 2) 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER)) $FunctionRowColumn) 
      (xtManageChild $FunctionRowColumn) 
      (groups $ListOfGroups) 
      (createFunctionGroups $FunctionRowColumn $ListOfGroups)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (createFunctionGroups  $_ ()) True)
; 

  (= 
    (createFunctionGroups $Parent 
      (Cons  $Groupname $ListOfGroups)) 
    ( (xmCreatePushButton $Parent $Groupname Nil $ButtonWidget) 
      (xtManageChild $ButtonWidget) 
      (xmCreatePopupMenu $ButtonWidget 'Learning functions' 
        (:: 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-COLUMN) 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER)) $LearningFunctions) 
      (xtAddEventHandler $LearningFunctions 
        (:: buttonReleaseMask) False functionsPopdown $_) 
      (xtAddCallback $ButtonWidget xmNactivateCallback popupFunctions $LearningFunctions) 
      (groupdef $Groupname $ListOfButtons) 
      (createFunctionButtons $LearningFunctions $ListOfButtons) 
      (createFunctionGroups $Parent $ListOfGroups)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (createFunctionButtons  $_ ()) True)
; 

  (= 
    (createFunctionButtons $Parent 
      (Cons  $Button $ListOfButtons)) 
    ( (xmCreatePushButton $Parent $Button Nil $ButtonWidget) 
      (xtManageChild $ButtonWidget) 
      (xtAddCallback $ButtonWidget xmNactivateCallback callFunction $Button) 
      (createFunctionButtons $Parent $ListOfButtons)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (callFunction $Widget $Buttonname $CallData) 
    ( (recorded learnfuncs 
        (popedup $PopupShell) $Ref) 
      (erase $Ref) 
      (xtUnmanageChild $PopupShell) 
      (operatordef $Buttonname $Funcname $InOutPattern $InChecks $XOuts $Refresh) 
      (writeMessage :- ) 
      (writeMessage $Funcname) 
      (| 
        (det-if-then 
          (= $InOutPattern Nil) 
          (, 
            (writelnMessage .) 
            (= $Fails Nil) 
            (= $Args Nil))) 
        (det-if-then otherwise 
          (, 
            (writeMessage () 
            (replaceXmVars $InOutPattern $InChecks $Args $Fails)))) 
      (flushErrorBuffer) 
      (| 
        (det-if-then 
          (= $Fails Nil) 
          (, 
            (=.. $F 
              (Cons  $Funcname $Args)) 
            (| 
              (det-if-then 
                (call $F) 
                (, 
                  (writelnMessage '% yes') 
                  (outVars $Args $InOutPattern $XOuts) 
                  (refresh $Refresh))) 
              (det-if-then otherwise 
                (writelnMessage '% no'))))) 
        (det-if-then otherwise 
          (writelnMessage '% Error occured, function not executed!'))) 
      (updateEvaluationLabel)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (replaceXmVars Nil $_ Nil Nil) 
    ( (set-det) (writelnMessage ).)))
; 

  (= 
    (replaceXmVars 
      (Cons  $Pat $InOutPattern) $InChecks $Args $Fails) 
    ( (| 
        (det-if-then 
          (atom $Pat) 
          (atom-chars $Pat $PatC)) 
        (det-if-then otherwise 
          (= $PatC "constant"))) 
      (append "xmarg" 
        (:: $M1) $Argstr) 
      (append "xmarg" 
        (:: $M3 $M4) $Argstr2) 
      (append "xmopt" 
        (:: $M2) $Optstr) 
      (append "xmopt" 
        (:: $M5 $M6) $Optstr2) 
      (append "xmout" 
        (:: $_) $Outstr) 
      (| 
        (det-if-then 
          (= $PatC "xmdepth") 
          (, 
            (recorded state 
              (argumentWidget depth $Text) $_) 
            (xmTextGetString $Text $ArgCP) 
            (proxtCharPtrToString $ArgCP $ArgStr) 
            (atom-chars $ArgStr $ArgChars) 
            (| 
              (number-chars $Value $ArgChars) 
              (= $Value $ArgStr)) 
            (= $InChecks 
              (Cons  $IC $ICs)) 
            (=.. $C 
              (:: $IC $Value)) 
            (| 
              (det-if-then 
                (call $C) 
                (= $Fail Nil)) 
              (= $Fail 
                (:: 0))) 
            (writeMessage $ArgStr) 
            (| 
              (= $InOutPattern Nil) 
              (writeMessage ,)))) 
        (| 
          (det-if-then 
            (= $PatC $Argstr) 
            (, 
              (number-chars $N 
                (:: $M1)) 
              (recorded state 
                (argumentWidget $N $Text) $_) 
              (xmTextGetString $Text $ArgCP) 
              (proxtCharPtrToString $ArgCP $ArgStr) 
              (atom-chars $ArgStr $ArgChars) 
              (| 
                (number-chars $Value $ArgChars) 
                (= $Value $ArgStr)) 
              (= $InChecks 
                (Cons  $IC $ICs)) 
              (=.. $C 
                (:: $IC $Value)) 
              (| 
                (det-if-then 
                  (call $C) 
                  (= $Fail Nil)) 
                (= $Fail 
                  (:: $N))) 
              (writeMessage $ArgStr) 
              (| 
                (= $InOutPattern Nil) 
                (writeMessage ,)))) 
          (| 
            (det-if-then 
              (= $PatC $Argstr2) 
              (, 
                (number-chars $N 
                  (:: $M3 $M4)) 
                (recorded state 
                  (argumentWidget $N $Text) $_) 
                (xmTextGetString $Text $ArgCP) 
                (proxtCharPtrToString $ArgCP $ArgStr) 
                (atom-chars $ArgStr $ArgChars) 
                (| 
                  (number-chars $Value $ArgChars) 
                  (= $Value $ArgStr)) 
                (= $InChecks 
                  (Cons  $IC $ICs)) 
                (=.. $C 
                  (:: $IC $Value)) 
                (| 
                  (det-if-then 
                    (call $C) 
                    (= $Fail Nil)) 
                  (= $Fail 
                    (:: $N))) 
                (writeMessage $ArgStr) 
                (| 
                  (= $InOutPattern Nil) 
                  (writeMessage ,)))) 
            (| 
              (det-if-then 
                (= $PatC "xmoptdepth") 
                (, 
                  (recorded state 
                    (argumentWidget depth $Text) $_) 
                  (xmTextGetString $Text $ArgCP) 
                  (proxtCharPtrToString $ArgCP $ArgStr) 
                  (atom-chars $ArgStr $ArgChars) 
                  (= $InChecks 
                    (Cons  $IC $ICs)) 
                  (| 
                    (det-if-then 
                      (= $ArgChars Nil) 
                      (= $Value novalue)) 
                    (det-if-then otherwise 
                      (, 
                        (| 
                          (number-chars $Value $ArgChars) 
                          (= $Value $ArgStr)) 
                        (=.. $C 
                          (:: $IC $Value)) 
                        (| 
                          (det-if-then 
                            (call $C) 
                            (= $Fail Nil)) 
                          (= $Fail 
                            (:: 0))) 
                        (writeMessage $ArgStr) 
                        (| 
                          (= $InOutPattern Nil) 
                          (writeMessage ,))))))) 
              (| 
                (det-if-then 
                  (= $PatC $Optstr) 
                  (, 
                    (number-chars $N 
                      (:: $M2)) 
                    (recorded state 
                      (argumentWidget $N $Text) $_) 
                    (xmTextGetString $Text $ArgCP) 
                    (proxtCharPtrToString $ArgCP $ArgStr) 
                    (atom-chars $ArgStr $ArgChars) 
                    (= $InChecks 
                      (Cons  $IC $ICs)) 
                    (| 
                      (det-if-then 
                        (= $ArgChars Nil) 
                        (= $Value novalue)) 
                      (det-if-then otherwise 
                        (, 
                          (| 
                            (number-chars $Value $ArgChars) 
                            (= $Value $ArgStr)) 
                          (=.. $C 
                            (:: $IC $Value)) 
                          (| 
                            (det-if-then 
                              (call $C) 
                              (= $Fail Nil)) 
                            (= $Fail 
                              (:: $N))) 
                          (writeMessage $ArgStr) 
                          (| 
                            (= $InOutPattern Nil) 
                            (writeMessage ,))))))) 
                (| 
                  (det-if-then 
                    (= $PatC $Optstr2) 
                    (, 
                      (number-chars $N 
                        (:: $M5 $M6)) 
                      (recorded state 
                        (argumentWidget $N $Text) $_) 
                      (xmTextGetString $Text $ArgCP) 
                      (proxtCharPtrToString $ArgCP $ArgStr) 
                      (atom-chars $ArgStr $ArgChars) 
                      (= $InChecks 
                        (Cons  $IC $ICs)) 
                      (| 
                        (det-if-then 
                          (= $ArgChars Nil) 
                          (= $Value novalue)) 
                        (det-if-then otherwise 
                          (, 
                            (| 
                              (number-chars $Value $ArgChars) 
                              (= $Value $ArgStr)) 
                            (=.. $C 
                              (:: $IC $Value)) 
                            (| 
                              (det-if-then 
                                (call $C) 
                                (= $Fail Nil)) 
                              (= $Fail 
                                (:: $N))) 
                            (writeMessage $ArgStr) 
                            (| 
                              (= $InOutPattern Nil) 
                              (writeMessage ,))))))) 
                  (| 
                    (det-if-then 
                      (= $PatC $Outstr) 
                      (, 
                        (= $InChecks $ICs) 
                        (= $PatC 
                          (Cons  $_ $P1)) 
                        (append "X" $P1 $P2) 
                        (atom-chars $P3 $P2) 
                        (writeMessage $P3) 
                        (= $Fail Nil) 
                        (= $Value variable) 
                        (| 
                          (= $InOutPattern Nil) 
                          (writeMessage ,)))) 
                    (det-if-then otherwise 
                      (, 
                        (= $Value $Pat) 
                        (= $InChecks $ICs) 
                        (= $Fail Nil) 
                        (writeMessage $Pat) 
                        (| 
                          (= $InOutPattern Nil) 
                          (writeMessage ,))))))))))) 
      (set-det) 
      (replaceXmVars $InOutPattern $ICs $Values $F) 
      (append $Fail $F $Fails) 
      (| 
        (det-if-then 
          (= $Value novalue) 
          (= $Args $Values)) 
        (| 
          (det-if-then 
            (= $Value variable) 
            (= $Args 
              (Cons  $Var $Values))) 
          (det-if-then otherwise 
            (= $Args 
              (Cons  $Value $Values)))))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (outVars  $_ $_ ()) True)
; 

  (= 
    (outVars 
      (Cons  $Arg1 $Args) 
      (Cons  $Pat $InOutPattern) $XOuts) 
    ( (atom-chars $Pat $PatC) 
      (append "xmout" 
        (:: $_) $Outstr) 
      (| 
        (det-if-then 
          (= $PatC $Outstr) 
          (, 
            (= $XOuts 
              (Cons  $XO $XOs)) 
            (=.. $C 
              (:: $XO $Arg1)) 
            (call $C))) 
        (det-if-then otherwise 
          (= $XOuts $XOs))) 
      (set-det) 
      (outVars $Args $InOutPattern $XOs)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (refresh  ()) True)
; 

  (= 
    (refresh (Cons  $KindOfKnowledge $R)) 
    ( (refreshKnowledgeList $_ $KindOfKnowledge $_) (refresh $R)))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (createYesNoPopup $Parent $YesNoPopup $YesPred $YesArg) 
    ( (xmCreatePopupMenu $Parent YesNo 
        (:: 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-COLUMN) 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER)) $YesNoPopup) 
      (xmCreatePushButton $YesNoPopup Yes Nil $Yes) 
      (xmCreatePushButton $YesNoPopup No Nil $No) 
      (xtAddCallback $Parent xmNactivateCallback popupDialog $YesNoPopup) 
      (xtAddCallback $Yes xmNactivateCallback $YesPred $YesArg) 
      (xtAddCallback $No xmNactivateCallback yesNoPopdown $YesNoPopup) 
      (xtAddEventHandler $YesNoPopup 
        (:: buttonReleaseMask) False yesNoPopdown $YesNoPopup) 
      (xtManageChildren (:: $Yes $No))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (popupDialog $Widget $PopupShell $CallData) 
    ( (xtGetValues $Widget 
        (:: (xmNwidth $Xs))) 
      (xtTranslateCoords $Widget $Xs 0 $X $Y) 
      (xtSetValues $PopupShell 
        (:: 
          (xmNx $X) 
          (xmNy $Y))) 
      (xtManageChild $PopupShell)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (popupViewRules $Widget $ClientData $CallData) 
    ( (recorded rules 
        (view dialog $VRDialog) $_) 
      (xtGetValues $Widget 
        (:: (xmNwidth $Xs))) 
      (xtTranslateCoords $Widget $Xs 0 $X $Y) 
      (xtSetValues $VRDialog 
        (:: 
          (xmNx $X) 
          (xmNy $Y))) 
      (xtManageChild $VRDialog) 
      (fillExistingLabels) 
      (fillViewedLabels) 
      (fillExistingClauseHeads) 
      (fillViewedClauseHeads)))
; 





;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (popupExamineRules $Widget $ClientData $CallData) 
    ( (recorded rules 
        (examine dialog $ERDialog) $_) 
      (xtGetValues $Widget 
        (:: (xmNwidth $Xs))) 
      (xtTranslateCoords $Widget $Xs 0 $X $Y) 
      (xtSetValues $ERDialog 
        (:: 
          (xmNx $X) 
          (xmNy $Y))) 
      (showExaminedRule $_ $_ $_) 
      (xtManageChild $ERDialog)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (popupViewExamples $Widget $ClientData $CallData) 
    ( (recorded examples 
        (view dialog $VRDialog) $_) 
      (xtGetValues $Widget 
        (:: (xmNwidth $Xs))) 
      (xtTranslateCoords $Widget $Xs 0 $X $Y) 
      (xtSetValues $VRDialog 
        (:: 
          (xmNx $X) 
          (xmNy $Y))) 
      (xtManageChild $VRDialog) 
      (fillExistingExampleCHs) 
      (fillViewedExampleCHs)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (fillExistingLabels) 
    ( (recorded rules 
        (view exLabRC $ExLabRC) $_) 
      (listLabels $LabelList) 
      (createLabelWidgets $ExLabRC $LabelList)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (fillViewedLabels) 
    ( (recorded rules 
        (view viewLabRC $ViewLabRC) $_) 
      (recorded rules 
        (view labels $LabelList) $_) 
      (createLabelWidgets $ViewLabRC $LabelList)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (fillExistingClauseHeads) 
    ( (recorded rules 
        (view exCHRC $ExCHRC) $_) 
      (listClauseHeads $LabelList) 
      (createLabelWidgets $ExCHRC $LabelList)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (fillViewedClauseHeads) 
    ( (recorded rules 
        (view viewCHRC $ViewCHRC) $_) 
      (recorded rules 
        (view clause-heads $CHList) $_) 
      (createLabelWidgets $ViewCHRC $CHList)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (fillExistingExampleCHs) 
    ( (recorded examples 
        (view exCHRC $ExCHRC) $_) 
      (listExampleCHs $LabelList) 
      (createLabelWidgets $ExCHRC $LabelList)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (fillViewedExampleCHs) 
    ( (recorded examples 
        (view viewCHRC $ViewCHRC) $_) 
      (recorded examples 
        (view clause-heads $CHList) $_) 
      (createLabelWidgets $ViewCHRC $CHList)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (createLabelWidgets $Parent 
      (Cons  $Name $NameList)) 
    ( (proxtStringToCharPtr $Name $NameChrPtr) 
      (proxtGetDefaultCharset $DCharset) 
      (xmStringCreate $NameChrPtr $DCharset $NameStr) 
      (xmCreateLabel $Parent nameLabel 
        (:: 
          (xmNalignment xmALIGNMENT-BEGINNING) 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $NameStr)) $NameLabel) 
      (xtManageChild $NameLabel) 
      (recordz labels 
        (parent-child $Parent $NameLabel $NameStr) $_) 
      (xtAddEventHandler $NameLabel 
        (:: buttonReleaseMask) False selectLabel $_) 
      (set-det) 
      (createLabelWidgets $Parent $NameList)))
; 

  (= 
    (createLabelWidgets $_ Nil) 
    (set-det))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (destroyLabelWidgets $Parent) 
    ( (var $Parent) 
      (recorded labels 
        (parent-child $Parent $L $_) $Ref) 
      (set-det) 
      (erase $Ref) 
      (xtDestroyWidget $L) 
      (destroyLabelWidgets $_)))
; 

  (= 
    (destroyLabelWidgets $Parent) 
    ( (recorded labels 
        (parent-child $Parent $L $_) $Ref) 
      (set-det) 
      (erase $Ref) 
      (xtDestroyWidget $L) 
      (destroyLabelWidgets $Parent)))
; 

  (= 
    (destroyLabelWidgets $_) 
    (set-det))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (listLabels $LabelList) 
    ( (recorded rules 
        (view labels $ViewedLabels) $_) 
      (listLabels $ViewedLabels $LL1) 
      (append $LabelList $ViewedLabels $LL1) 
      (set-det)))
; 

  (= 
    (listLabels $LLin $LLout) 
    ( (get-clause $_ $_ $_ $_ $Label) 
      (nonmember $Label $LLin) 
      (set-det) 
      (= $LLin2 
        (Cons  $Label $LLin)) 
      (listLabels $LLin2 $LLout)))
; 

  (= 
    (listLabels $A $A) 
    (set-det))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (listClauseHeads $LabelList) 
    ( (recorded rules 
        (view clause-heads $ViewedCHs) $_) 
      (listClauseHeads $ViewedCHs $LL1) 
      (append $LabelList $ViewedCHs $LL1) 
      (set-det)))
; 

  (= 
    (listClauseHeads $LLin $LLout) 
    ( (get-clause $_ $C $_ $_ $_) 
      (functor $C $CH $_) 
      (nonmember $CH $LLin) 
      (set-det) 
      (= $LLin2 
        (Cons  $CH $LLin)) 
      (listClauseHeads $LLin2 $LLout)))
; 

  (= 
    (listClauseHeads $A $A) 
    (set-det))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (listExampleCHs $LabelList) 
    ( (recorded examples 
        (view clause-heads $ViewedCHs) $_) 
      (listExampleCHs $ViewedCHs $LL1) 
      (append $LabelList $ViewedCHs $LL1) 
      (set-det)))
; 

  (= 
    (listExampleCHs $LLin $LLout) 
    ( (get-example $_ $C $_) 
      (functor $C $CH $_) 
      (nonmember $CH $LLin) 
      (set-det) 
      (= $LLin2 
        (Cons  $CH $LLin)) 
      (listExampleCHs $LLin2 $LLout)))
; 

  (= 
    (listExampleCHs $A $A) 
    (set-det))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (selectLabel $Widget $ClientData $CallData) 
    ( (recorded labels 
        (parent-child $P $Widget $N) $Ref) 
      (set-det) 
      (erase $Ref) 
      (xtDestroyWidget $Widget) 
      (| 
        (det-if-then 
          (recorded rules 
            (view exLabRC $P) $_) 
          (recorded rules 
            (view viewLabRC $P2) $_)) 
        (| 
          (det-if-then 
            (recorded rules 
              (view viewLabRC $P) $_) 
            (recorded rules 
              (view exLabRC $P2) $_)) 
          (| 
            (det-if-then 
              (recorded rules 
                (view exCHRC $P) $_) 
              (recorded rules 
                (view viewCHRC $P2) $_)) 
            (| 
              (det-if-then 
                (recorded rules 
                  (view viewCHRC $P) $_) 
                (recorded rules 
                  (view exCHRC $P2) $_)) 
              (| 
                (det-if-then 
                  (recorded examples 
                    (view exCHRC $P) $_) 
                  (recorded examples 
                    (view viewCHRC $P2) $_)) 
                (| 
                  (det-if-then 
                    (recorded examples 
                      (view viewCHRC $P) $_) 
                    (recorded examples 
                      (view exCHRC $P2) $_)) 
                  (| 
                    (det-if-then 
                      (recorded examples 
                        (view exCHRC $P) $_) 
                      (recorded examples 
                        (view viewCHRC $P2) $_)) 
                    (det-if-then 
                      (recorded examples 
                        (view viewCHRC $P) $_) 
                      (recorded examples 
                        (view exCHRC $P2) $_))))))))) 
      (xmCreateLabel $P2 nameLabel 
        (:: 
          (xmNalignment xmALIGNMENT-BEGINNING) 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $N)) $NameLabel) 
      (xtManageChild $NameLabel) 
      (recordz labels 
        (parent-child $P2 $NameLabel $N) $_) 
      (xtAddEventHandler $NameLabel 
        (:: buttonReleaseMask) False selectLabel $_) 
      (set-det)))
; 




  (= 
    (viewRules $Widget $ClientData $CallData) 
    ( (| 
        (det-if-then 
          (recorded rules 
            (view all) $R1) 
          (erase $R1)) otherwise) 
      (recorded rules 
        (view labels $_) $R2) 
      (erase $R2) 
      (recorded rules 
        (view clause-heads $_) $R3) 
      (erase $R3) 
      (recorded rules 
        (view viewLabRC $ViewLabRC) $_) 
      (recorded rules 
        (view viewCHRC $ViewCHRC) $_) 
      (viewRules $ViewLabRC $LabelList) 
      (recordz rules 
        (view labels $LabelList) $_) 
      (viewRules $ViewCHRC $CHList) 
      (recordz rules 
        (view clause-heads $CHList) $_) 
      (destroyLabelWidgets $_) 
      (refresh (:: rules)) 
      (recorded rules 
        (view dialog $X) $_) 
      (xtUnmanageChild $X)))
; 

  (= 
    (viewRules $P $List) 
    ( (recorded labels 
        (parent-child $P $C $N) $Ref) 
      (xtDestroyWidget $C) 
      (erase $Ref) 
      (proxtGetDefaultCharset $DCharset) 
      (xmStringGetLtoR $N $DCharset $NCP $_) 
      (proxtCharPtrToString $NCP $Name) 
      (viewRules $P $L2) 
      (= $List 
        (Cons  $Name $L2))))
; 

  (= 
    (viewRules $_ Nil) 
    (set-det))
; 




  (= 
    (viewAllRules $Widget $ClientData $CallData) 
    ( (| 
        (recorded rules 
          (view all) $_) 
        (recordz rules 
          (view all) $_)) 
      (refresh (:: rules)) 
      (destroyLabelWidgets $_) 
      (recorded rules 
        (view dialog $X) $_) 
      (xtUnmanageChild $X)))
; 




  (= 
    (cancelViewRules $Widget $ClientData $CallData) 
    ( (destroyLabelWidgets $_) 
      (recorded rules 
        (view dialog $X) $_) 
      (xtUnmanageChild $X)))
; 




  (= 
    (cancelExamineRules $Widget $ClientData $CallData) 
    ( (destroyLabelWidgets $_) 
      (recorded rules 
        (examine dialog $X) $_) 
      (xtUnmanageChild $X)))
; 




  (= 
    (viewExamples $Widget $ClientData $CallData) 
    ( (| 
        (det-if-then 
          (recorded examples 
            (view all) $R1) 
          (erase $R1)) otherwise) 
      (recorded examples 
        (view clause-heads $_) $R3) 
      (erase $R3) 
      (recorded examples 
        (view viewCHRC $ViewCHRC) $_) 
      (viewExamples $ViewCHRC $CHList) 
      (recordz examples 
        (view clause-heads $CHList) $_) 
      (destroyLabelWidgets $_) 
      (refresh (:: examples)) 
      (recorded examples 
        (view dialog $X) $_) 
      (xtUnmanageChild $X)))
; 

  (= 
    (viewExamples $P $List) 
    ( (recorded labels 
        (parent-child $P $C $N) $Ref) 
      (xtDestroyWidget $C) 
      (erase $Ref) 
      (proxtGetDefaultCharset $DCharset) 
      (xmStringGetLtoR $N $DCharset $NCP $_) 
      (proxtCharPtrToString $NCP $Name) 
      (viewExamples $P $L2) 
      (= $List 
        (Cons  $Name $L2))))
; 

  (= 
    (viewExamples $_ Nil) 
    (set-det))
; 




  (= 
    (viewAllExamples $Widget $ClientData $CallData) 
    ( (| 
        (recorded examples 
          (view all) $_) 
        (recordz examples 
          (view all) $_)) 
      (refresh (:: examples)) 
      (destroyLabelWidgets $_) 
      (recorded examples 
        (view dialog $X) $_) 
      (xtUnmanageChild $X)))
; 




  (= 
    (cancelViewExamples $Widget $ClientData $CallData) 
    ( (destroyLabelWidgets $_) 
      (recorded examples 
        (view dialog $X) $_) 
      (xtUnmanageChild $X)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (popupFunctions $Widget $PopupShell $CallData) 
    ( (xtGetValues $Widget 
        (:: (xmNheight $Ys))) 
      (xtTranslateCoords $Widget 0 $Ys $X $Y) 
      (xtSetValues $PopupShell 
        (:: 
          (xmNx $X) 
          (xmNy $Y))) 
      (xtManageChild $PopupShell) 
      (recordz learnfuncs 
        (popedup $PopupShell) $_)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (yesNoPopdown $Widget $JaNeinPopup $CallData) 
    (xtUnmanageChild $JaNeinPopup))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (functionsPopdown $Widget $ClientData $CallData) 
    ( (recorded learnfuncs 
        (popedup $PopupShell) $Ref) 
      (erase $Ref) 
      (xtUnmanageChild $PopupShell)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (doKommando $Widget 
      (:: $KommandoText $KStatus) $CallData) 
    ( (proxtGetDefaultCharset $DCharset) 
      (proxtStringToCharPtr '-EXIT-    Input' $ACP) 
      (xmStringCreate $ACP $DCharset $AXmS) 
      (xtSetValues $KStatus 
        (:: (xmNlabelString $AXmS))) 
      (xmTextGetString $KommandoText $KcharPtr) 
      (proxtCharPtrToString $KcharPtr $KS) 
      (xxmStringToTerm $KcharPtr $Charset $Term) 
      (writeMessage :- ) 
      (writelnMessage $KS) 
      (| 
        (det-if-then 
          (call $Term) 
          (, 
            (proxtStringToCharPtr '- yes -   Input' $YesCP) 
            (xmStringCreate $YesCP $DCharset $YesXmS) 
            (xtSetValues $KStatus 
              (:: (xmNlabelString $YesXmS))) 
            (writelnMessage '% yes'))) 
        (det-if-then otherwise 
          (, 
            (proxtStringToCharPtr '- no -    Input' $NoCP) 
            (xmStringCreate $NoCP $DCharset $NoXmS) 
            (xtSetValues $KStatus 
              (:: (xmNlabelString $NoXmS))) 
            (writelnMessage '% no')))) 
      (doEmptyKommando $_ $KommandoText $_)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (doEmptyKommando $Widget $KommandoText $CallData) 
    ( (proxtStringToCharPtr '' $KommandoChrPtr) 
      (xmTextSetString $KommandoText $KommandoChrPtr) 
      (refreshKnowledgeList $Widget rules $Calldata) 
      (refreshKnowledgeList $Widget examples $Calldata) 
      (updateEvaluationLabel)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (beenden $Widget $ClientData $CallData) 
    ( (recorded messages 
        (file $F) $Ref) 
      (erase $Ref) 
      (close $F) 
      (write 'X-MILES korrekt beendet!') 
      (nl) 
      (remove-symbol  &self 
        (my_exit_loop  no)) 
      (add-symbol  &self 
        (my_exit_loop  yes))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (createMessageArea $Parent) 
    ( (xmCreateFrame $Parent Messages Nil $MessageFrame) 
      (xtManageChild $MessageFrame) 
      (xmCreateRowColumn $MessageFrame messages 
        (:: 
          (xmNadjustLast True) 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT) 
          (xmNrowColumnType xmWORK-AREA)) $MessageColumn) 
      (xtManageChild $MessageColumn) 
      (proxtStringToCharPtr Messages $MessageTitleChrPtr) 
      (proxtGetDefaultCharset $DCharset) 
      (xmStringCreate $MessageTitleChrPtr $DCharset $MessageTitleStr) 
      (xmCreateLabelGadget $MessageColumn messageLabel 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $MessageTitleStr)) $MessageLabel) 
      (xtManageChild $MessageLabel) 
      (xmCreateScrolledText $MessageColumn messageText 
        (:: 
          (xmNeditable False) 
          (xmNeditMode xmMULTI-LINE-EDIT) 
          (xmNrows 8) 
          (xmNlistSizePolicy xmCONSTANT) 
          (xmNscrollBarDisplayPolicy xmSTATIC) 
          (xmNscrollingPolicy xmAUTOMATIC) 
          (xmNautoShowCursorPosition True)) $MessageText) 
      (xtManageChild $MessageText) 
      (recordz messages 
        (textWidget $MessageText) $_) 
      (xmCreateRowColumn $MessageColumn ButtonRow 
        (:: 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER) 
          (xmNnumColumns 1) 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN)) $ButtonRow) 
      (xtManageChild $ButtonRow) 
      (xmCreatePushButton $ButtonRow Save Nil $Save) 
      (xtManageChild $Save) 
      (xtAddCallback $Save xmNactivateCallback saveMessages $_) 
      (xmCreatePushButton $ButtonRow Clear Nil $Clear) 
      (xtManageChild $Clear) 
      (xtAddCallback $Clear xmNactivateCallback clearMessages $_) 
      (open xmProtocol.tmp write $F) 
      (recordz messages 
        (file $F) $_)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (saveMessages $Widget $ClientData $CallData) 
    ( (recorded messages 
        (textWidget $MessageText) $_) 
      (proxtStringToCharPtr '' $EmptyCharPtr) 
      (xmTextSetString $MessageText $EmptyCharPtr) 
      (recorded messages 
        (file $F) $Ref) 
      (erase $Ref) 
      (close $F) 
      (unix (shell 'mv xmProtocol.tmp xmProtocol.sav')) 
      (open xmProtocol.tmp write $Fnew) 
      (recordz messages 
        (file $Fnew) $_) 
      (writelnMessage '% wrote "xmProtocol.sav"')))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (clearMessages $Widget $ClientData $CallData) 
    ( (recorded messages 
        (textWidget $MessageText) $_) 
      (proxtStringToCharPtr '' $EmptyCharPtr) 
      (xmTextSetString $MessageText $EmptyCharPtr) 
      (recorded messages 
        (file $F) $Ref) 
      (erase $Ref) 
      (close $F) 
      (open xmProtocol.tmp write $Fnew) 
      (recordz messages 
        (file $Fnew) $_)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (writeError $Message) 
    ( (| 
        (det-if-then 
          (number $Message) 
          (, 
            (number-chars $Message $MsgChars) 
            (atom-chars $MsgStr $MsgChars))) 
        (det-if-then otherwise 
          (= $MsgStr $Message))) 
      (| 
        (det-if-then 
          (recorded error 
            (errorText $EText) $Ref) 
          (, 
            (erase $Ref) 
            (concat $EText $MsgStr $NewEText))) 
        (det-if-then otherwise 
          (= $NewEText $MsgStr))) 
      (recordz error 
        (errorText $NewEText) $_)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (writelnError $Message) 
    ( (| 
        (det-if-then 
          (number $Message) 
          (, 
            (number-chars $Message $MsgChars) 
            (append $MsgChars 
              (:: 10) $MC) 
            (atom-chars $MsgStr $MC))) 
        (det-if-then otherwise 
          (, 
            (atom-chars $Message $MsgChars) 
            (append $MsgChars 
              (:: 10) $MC) 
            (atom-chars $MsgStr $MC)))) 
      (| 
        (det-if-then 
          (recorded error 
            (errorText $EText) $Ref) 
          (, 
            (erase $Ref) 
            (concat $EText $MsgStr $NewEText))) 
        (det-if-then otherwise 
          (= $NewEText $MsgStr))) 
      (recordz error 
        (errorText $NewEText) $_)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (flushErrorBuffer) 
    (| 
      (det-if-then 
        (recorded error 
          (errorText $EText) $Ref) 
        (, 
          (erase $Ref) 
          (writeMessage $EText))) otherwise))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (writeMessage $Message) 
    ( (recorded messages 
        (textWidget $MessageText) $_) 
      (| 
        (det-if-then 
          (number $Message) 
          (, 
            (number-chars $Message $MsgChars) 
            (atom-chars $MsgStr $MsgChars) 
            (proxtStringToCharPtr $MsgStr $MessageCharPtr))) 
        (| 
          (det-if-then 
            (atom $Message) 
            (proxtStringToCharPtr $Message $MessageCharPtr)) 
          (det-if-then otherwise 
            (xxmWriteToCharPtr 
              (write $Message) $MessageCharPtr)))) 
      (xmTextGetLastPosition $MessageText $LastPos) 
      (xmTextReplace $MessageText $LastPos $LastPos $MessageCharPtr) 
      (recorded messages 
        (file $F) $_) 
      (write $F $Message)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (writelnMessage $Message) 
    ( (recorded messages 
        (textWidget $MessageText) $_) 
      (| 
        (det-if-then 
          (number $Message) 
          (, 
            (number-chars $Message $MsgChars) 
            (append $MsgChars 
              (:: 10) $MC2) 
            (atom-chars $MsgStr $MC2) 
            (proxtStringToCharPtr $MsgStr $MessageCharPtr))) 
        (| 
          (det-if-then 
            (atom $Message) 
            (, 
              (atom-chars $Message $MsgChars) 
              (append $MsgChars 
                (:: 10) $MC2) 
              (atom-chars $MsgStr $MC2) 
              (proxtStringToCharPtr $MsgStr $MessageCharPtr))) 
          (det-if-then otherwise 
            (xxmWriteToCharPtr 
              (, 
                (write $Message) 
                (nl)) $MessageCharPtr)))) 
      (xmTextGetLastPosition $MessageText $LastPos) 
      (xmTextReplace $MessageText $LastPos $LastPos $MessageCharPtr) 
      (xmTextScroll $MessageText 1) 
      (recorded messages 
        (file $F) $_) 
      (write $F $Message) 
      (nl $F)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (createEditorArea $Parent) 
    ( (xmCreateFrame $Parent Editor Nil $EditorFrame) 
      (xtManageChild $EditorFrame) 
      (xmCreateRowColumn $EditorFrame editor 
        (:: 
          (xmNadjustLast True) 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT) 
          (xmNrowColumnType xmWORK-AREA)) $EditorRowColumn) 
      (xtManageChild $EditorRowColumn) 
      (proxtStringToCharPtr Editor $EditorTitleChrPtr) 
      (proxtGetDefaultCharset $DCharset) 
      (xmStringCreate $EditorTitleChrPtr $DCharset $EditorTitleStr) 
      (xmCreateLabelGadget $EditorRowColumn editorLabel 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $EditorTitleStr)) $EditorLabel) 
      (xtManageChild $EditorLabel) 
      (recordz editor 
        (label $EditorLabel) $_) 
      (xmCreateScrolledText $EditorRowColumn kommandotext 
        (:: 
          (xmNeditable True) 
          (xmNeditMode xmMULTI-LINE-EDIT) 
          (xmNrows 5) 
          (xmNlistSizePolicy xmCONSTANT) 
          (xmNscrollBarDisplayPolicy xmSTATIC) 
          (xmNscrollingPolicy xmAUTOMATIC) 
          (xmNautoShowCursorPosition True)) $EditorText) 
      (xtManageChild $EditorText) 
      (recordz editor 
        (textWidget $EditorText) $Ref) 
      (xmCreateRowColumn $EditorRowColumn ButtonRowColumn 
        (:: 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER) 
          (xmNnumColumns 1) 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN)) $ButtonRowColumn) 
      (xtManageChild $ButtonRowColumn) 
      (xmCreatePushButton $ButtonRowColumn 'Add rule' Nil $AddRule) 
      (xtManageChild $AddRule) 
      (xmCreatePushButton $ButtonRowColumn 'Change rule' Nil $ChangeRule) 
      (createYesNoPopup $ChangeRule $YesNoPopup changeRule $_) 
      (xtManageChild $ChangeRule) 
      (xmCreatePushButton $ButtonRowColumn 'Add example' Nil $AddExample) 
      (xtManageChild $AddExample) 
      (xmCreatePushButton $ButtonRowColumn 'Change example' Nil $ChangeExample) 
      (createYesNoPopup $ChangeExample $YesNoPopup2 changeExample $_) 
      (xtManageChild $ChangeExample) 
      (xmCreatePushButton $ButtonRowColumn Clear Nil $Clear) 
      (xtManageChild $Clear) 
      (xtAddCallback $Clear xmNactivateCallback clearEditor $_) 
      (xtAddCallback $AddRule xmNactivateCallback addRule $_) 
      (xtAddCallback $AddExample xmNactivateCallback addExample $_)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (clearEditor $Widget $ClientData $CallData) 
    ( (recorded editor 
        (textWidget $EditorText) $_) 
      (recorded editor 
        (label $Label) $_) 
      (proxtGetDefaultCharset $DCharset) 
      (| 
        (det-if-then 
          (recorded editor 
            (editing $_ $_) $Ref) 
          (, 
            (erase $Ref) 
            (proxtStringToCharPtr Editor $LblCP) 
            (xmStringCreate $LblCP $DCharset $LblS) 
            (xtSetValues $Label 
              (:: (xmNlabelString $LblS))))) otherwise) 
      (proxtStringToCharPtr '' $EmptyChrPtr) 
      (xmTextSetString $EditorText $EmptyChrPtr)))
; 
	


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (addRule $Widget $ClientData $CallData) 
    ( (recorded editor 
        (textWidget $EditorText) $Ref) 
      (xmTextGetString $EditorText $RuleCP) 
      (xxmStringToTerm $RuleCP $_ $RuleTerm) 
      (proxtCharPtrToString $RuleCP $RulePString) 
      (span-left $RulePString "." $N) 
      (substring $RulePString $RuleString 0 $N $_) 
      (store-clause $RuleTerm $_ user $ID) 
      (writeMessage ':- store-clause(') 
      (writeMessage $RuleString) 
      (writeMessage ',-,user,') 
      (writeMessage $ID) 
      (writelnMessage ).) 
      (writelnMessage '% rule added.') 
      (updateEvaluationLabel) 
      (get-clause $ID $H $B $S $L) 
      (addRuleItem $ID $H $B $S $L)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (resultAddRule $Id) 
    ( (get-clause $Id $H $B $S $L) 
      (addRuleItem $Id $H $B $S $L) 
      (writeMessage '% rule ') 
      (number-chars $Id $IdChars) 
      (atom-chars $IdString $IdChars) 
      (writeMessage $IdString) 
      (writelnMessage ' created.')))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (resultAddNewpreds (, $ID $Reflist)) 
    ( (toplevel $Shell) 
      (xmCreateBulletinBoardDialog $Shell Newpreds Nil $NewpredDialog) 
      (recordz newpreddialog 
        (with_self  $ID $NewpredDialog) $_) 
      (xtManageChild $NewpredDialog) 
      (xmCreateFrame $NewpredDialog 'Newpred Frame' Nil $NewpredFrame) 
      (xtManageChild $NewpredFrame) 
      (xmCreateRowColumn $NewpredFrame newpredColumn 
        (:: 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT)) $NewpredColumn) 
      (xtManageChild $NewpredColumn) 
      (xmCreateRowColumn $NewpredColumn titlerow 
        (:: 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN) 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER)) $TitleRow) 
      (xtManageChild $TitleRow) 
      (proxtGetDefaultCharset $DCharset) 
      (proxtStringToCharPtr 'Choose Specialisation' $TitleCP) 
      (xmStringCreate $TitleCP $DCharset $TitleStr) 
      (xmCreateLabelGadget $TitleRow title 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $TitleStr)) $TitleLabel) 
      (xtManageChild $TitleLabel) 
      (xmCreateScrolledWindow $NewpredColumn newpredSW 
        (:: 
          (xmNheight 300) 
          (xmNwidth 300) 
          (xmNlistSizePolicy xmCONSTANT) 
          (xmNscrollBarDisplayPolicy xmSTATIC) 
          (xmNscrollingPolicy xmAUTOMATIC)) $NewpredSW) 
      (xtManageChild $NewpredSW) 
      (xmCreateRowColumn $NewpredSW newpredRC 
        (:: 
          (xmNadjustLast False) 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT) 
          (xmNrowColumnType xmWORK-AREA)) $NewpredRC) 
      (xtGetValues $NewpredRC 
        (:: (xmNbackground $B))) 
      (xtGetValues $NewpredSW 
        (:: (xmNclipWindow $CW))) 
      (xtSetValues $CW 
        (:: (xmNbackground $B))) 
      (xtManageChild $NewpredRC) 
      (addnewpredclauses $Reflist $NewpredRC) 
      (xmCreateRowColumn $NewpredColumn buttonrow 
        (:: 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN) 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER)) $ButtonRow) 
      (xtManageChild $ButtonRow) 
      (xmCreatePushButton $ButtonRow 'Add Rule' Nil $AddRule) 
      (xtManageChild $AddRule) 
      (xtAddCallback $AddRule xmNactivateCallback addselectedRule $_) 
      (xmCreatePushButton $ButtonRow None Nil $None) 
      (xtManageChild $None) 
      (xtAddCallback $None xmNactivateCallback addnorule $_)))
; 
	



  (= 
    (addnewpredclauses  () $_) True)
; 

  (= 
    (addnewpredclauses 
      (Cons  
        (, $NC $Pos $Neg $TR) $R) $Widget) 
    ( (proxtGetDefaultCharset $DCharset) 
      (xxmWriteToString 
        (portray-clause $NC) $DCharset $XmS) 
      (xmCreateLabel $Widget newpredClause 
        (:: 
          (xmNalignment xmALIGNMENT-BEGINNING) 
          (xmNlabelString $XmS) 
          (xmNlabelType xmSTRING)) $Label) 
      (xtManageChild $Label) 
      (recordz newpred 
        (np $Label $NC $Pos $Neg $TR notselected) $Ref) 
      (xtAddEventHandler $Label 
        (:: buttonReleaseMask) False selectnpclause $_) 
      (addnewpredclauses $R $Widget)))
; 




  (= 
    (selectnpclause $Widget $_ $CallData) 
    ( (recorded newpred 
        (np $Widget $NC $Pos $Neg $TR notselected) $Ref) 
      (turnoff-other-selected) 
      (xtGetValues $Widget 
        (:: 
          (xmNbackground $B) 
          (xmNforeground $F))) 
      (xtSetValues $Widget 
        (:: 
          (xmNbackground $F) 
          (xmNforeground $B))) 
      (erase $Ref) 
      (recordz newpred 
        (np $Widget $NC $Pos $Neg $TR selected) $_)))
; 


  (= 
    (selectnpclause $Widget rules $CallData) 
    ( (recorded newpred 
        (np $Widget $NC $Pos $Neg $TR selected) $Ref) 
      (xtGetValues $Widget 
        (:: 
          (xmNbackground $B) 
          (xmNforeground $F))) 
      (xtSetValues $Widget 
        (:: 
          (xmNbackground $F) 
          (xmNforeground $B))) 
      (erase $Ref) 
      (recordz newpred 
        (np $Widget $NC $Pos $Neg $TR notselected) $_)))
; 



  (= 
    (turnoff-other-selected) 
    (mysetof $Ref 
      (^ $Widget 
        (^ $NC 
          (^ $Pos 
            (^ $Neg 
              (^ $TR 
                (^ $B 
                  (^ $F 
                    (, 
                      (recorded newpred 
                        (np $Widget $NC $Pos $Neg $TR selected) $Ref) 
                      (erase $Ref) 
                      (recordz newpred 
                        (np $Widget $NC $Pos $Neg $TR notselected) $_) 
                      (xtGetValues $Widget 
                        (:: 
                          (xmNbackground $B) 
                          (xmNforeground $F))) 
                      (xtSetValues $Widget 
                        (:: 
                          (xmNbackground $F) 
                          (xmNforeground $B))))))))))) $_))
; 


    

  (= 
    (addnorule $_ $_ $_) 
    ( (recorded newpreddialog 
        (with_self  $_ $Widget) $Ref) 
      (erase $Ref) 
      (mysetof $Ref1 
        (^ $X 
          (, 
            (recorded newpred $X $Ref1) 
            (erase $Ref1))) $_) 
      (xtDestroyWidget $Widget)))
; 



  (= 
    (addselectedRule $_ $_ $_) 
    (det-if-then-else 
      (recorded newpred 
        (np $_ $NC $Pos $Neg $TR selected) $_) 
      (, 
        (recorded newpreddialog 
          (with_self  $ID $Widget) $Ref0) 
        (delete-clause $ID) 
        (store-clause $NC $_ newpred $ID) 
        (mysetof $P 
          (^ $PID 
            (, 
              (member $P $Pos) 
              (store-ex $P + $PID))) $_) 
        (mysetof $N 
          (^ $NID 
            (, 
              (member $N $Neg) 
              (store-ex $N - $NID))) $_) 
        (add-symbol  &self 
          (:  kb $TR)) 
        (erase $Ref0) 
        (mysetof $Ref1 
          (^ $X 
            (, 
              (recorded newpred $X $Ref1) 
              (erase $Ref1))) $_) 
        (xtDestroyWidget $Widget) 
        (refresh (:: rules examples))) True))
; 
            



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (resultAddSpec (, $ID $Reflist)) 
    ( (toplevel $Shell) 
      (xmCreateBulletinBoardDialog $Shell Specs Nil $SpecsDialog) 
      (recordz specsdialog 
        (with_self  $ID $SpecsDialog) $_) 
      (xtManageChild $SpecsDialog) 
      (xmCreateFrame $SpecsDialog 'Specs Frame' Nil $SpecsFrame) 
      (xtManageChild $SpecsFrame) 
      (xmCreateRowColumn $SpecsFrame specsColumn 
        (:: 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT)) $SpecsColumn) 
      (xtManageChild $SpecsColumn) 
      (xmCreateRowColumn $SpecsColumn titlerow 
        (:: 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN) 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER)) $TitleRow) 
      (xtManageChild $TitleRow) 
      (proxtGetDefaultCharset $DCharset) 
      (proxtStringToCharPtr 'Choose Specialisation' $TitleCP) 
      (xmStringCreate $TitleCP $DCharset $TitleStr) 
      (xmCreateLabelGadget $TitleRow title 
        (:: 
          (xmNlabelType xmSTRING) 
          (xmNlabelString $TitleStr)) $TitleLabel) 
      (xtManageChild $TitleLabel) 
      (xmCreateScrolledWindow $SpecsColumn specsSW 
        (:: 
          (xmNheight 300) 
          (xmNwidth 300) 
          (xmNlistSizePolicy xmCONSTANT) 
          (xmNscrollBarDisplayPolicy xmSTATIC) 
          (xmNscrollingPolicy xmAUTOMATIC)) $SpecsSW) 
      (xtManageChild $SpecsSW) 
      (xmCreateRowColumn $SpecsSW newpredRC 
        (:: 
          (xmNadjustLast False) 
          (xmNorientation xmVERTICAL) 
          (xmNpacking xmPACK-TIGHT) 
          (xmNrowColumnType xmWORK-AREA)) $SpecsRC) 
      (xtGetValues $SpecsRC 
        (:: (xmNbackground $B))) 
      (xtGetValues $SpecsSW 
        (:: (xmNclipWindow $CW))) 
      (xtSetValues $CW 
        (:: (xmNbackground $B))) 
      (xtManageChild $SpecsRC) 
      (addspecclauses $Reflist $SpecsRC) 
      (xmCreateRowColumn $SpecsColumn buttonrow 
        (:: 
          (xmNorientation xmHORIZONTAL) 
          (xmNpacking xmPACK-COLUMN) 
          (xmNisAligned True) 
          (xmNentryAlignment xmALIGNMENT-CENTER)) $ButtonRow) 
      (xtManageChild $ButtonRow) 
      (xmCreatePushButton $ButtonRow 'Add Rule' Nil $AddRule) 
      (xtManageChild $AddRule) 
      (xtAddCallback $AddRule xmNactivateCallback addselectedSpec $_) 
      (xmCreatePushButton $ButtonRow None Nil $None) 
      (xtManageChild $None) 
      (xtAddCallback $None xmNactivateCallback addnospec $_)))
; 
	



  (= 
    (addspecclauses  () $_) True)
; 

  (= 
    (addspecclauses 
      (Cons  $NC $R) $Widget) 
    ( (proxtGetDefaultCharset $DCharset) 
      (xxmWriteToString 
        (portray-clause $NC) $DCharset $XmS) 
      (xmCreateLabel $Widget specClause 
        (:: 
          (xmNalignment xmALIGNMENT-BEGINNING) 
          (xmNlabelString $XmS) 
          (xmNlabelType xmSTRING)) $Label) 
      (xtManageChild $Label) 
      (recordz spec 
        (np $Label $NC notselected) $Ref) 
      (xtAddEventHandler $Label 
        (:: buttonReleaseMask) False selectspecclause $_) 
      (addspecclauses $R $Widget)))
; 




  (= 
    (selectspecclause $Widget $_ $CallData) 
    ( (recorded spec 
        (np $Widget $NC notselected) $Ref) 
      (turnoff-other-selected-spec) 
      (xtGetValues $Widget 
        (:: 
          (xmNbackground $B) 
          (xmNforeground $F))) 
      (xtSetValues $Widget 
        (:: 
          (xmNbackground $F) 
          (xmNforeground $B))) 
      (erase $Ref) 
      (recordz spec 
        (np $Widget $NC selected) $_)))
; 


  (= 
    (selectspecclause $Widget rules $CallData) 
    ( (recorded spec 
        (np $Widget $NC selected) $Ref) 
      (xtGetValues $Widget 
        (:: 
          (xmNbackground $B) 
          (xmNforeground $F))) 
      (xtSetValues $Widget 
        (:: 
          (xmNbackground $F) 
          (xmNforeground $B))) 
      (erase $Ref) 
      (recordz spec 
        (np $Widget $NC notselected) $_)))
; 



  (= 
    (turnoff-other-selected-spec) 
    (mysetof $Ref 
      (^ $Widget 
        (^ $NC 
          (^ $B 
            (^ $F 
              (, 
                (recorded spec 
                  (np $Widget $NC selected) $Ref) 
                (erase $Ref) 
                (recordz spec 
                  (np $Widget $NC notselected) $_) 
                (xtGetValues $Widget 
                  (:: 
                    (xmNbackground $B) 
                    (xmNforeground $F))) 
                (xtSetValues $Widget 
                  (:: 
                    (xmNbackground $F) 
                    (xmNforeground $B)))))))) $_))
; 


    

  (= 
    (addnospec $_ $_ $_) 
    ( (recorded specsdialog 
        (with_self  $_ $Widget) $Ref) 
      (erase $Ref) 
      (mysetof $Ref1 
        (^ $X 
          (, 
            (recorded spec $X $Ref1) 
            (erase $Ref1))) $_) 
      (xtDestroyWidget $Widget)))
; 



  (= 
    (addselectedSpec $_ $_ $_) 
    (det-if-then-else 
      (recorded spec 
        (np $_ $NC selected) $_) 
      (, 
        (recorded specsdialog 
          (with_self  $ID $Widget) $Ref0) 
        (delete-clause $ID) 
        (store-clause $NC $_ spec $ID) 
        (erase $Ref0) 
        (mysetof $Ref1 
          (^ $X 
            (, 
              (recorded spec $X $Ref1) 
              (erase $Ref1))) $_) 
        (xtDestroyWidget $Widget) 
        (refresh (:: rules))) True))
; 
            

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (resultAddRuleList Nil) 
    (set-det))
; 

  (= 
    (resultAddRuleList (Cons  $Id $IdList)) 
    ( (resultAddRule $Id) (resultAddRuleList $IdList)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (resultSelectRules Nil) 
    ( (unselectAll $_ rules $_) 
      (set-det) 
      (writelnMessage '% resulting rules selected')))
; 

  (= 
    (resultSelectRules (Cons  (with_self  $Id $_) $IdList)) 
    ( (resultSelectRules $IdList) 
      (recorded current 
        (clause $Id $Widget notselected) $_) 
      (selectClause $Widget rules $_)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (resultSelectExamples Nil) 
    ( (unselectAll $_ examples $_) 
      (set-det) 
      (writelnMessage '% resulting examples selected')))
; 

  (= 
    (resultSelectExamples (Cons  $Id $IdList)) 
    ( (resultSelectExamples $IdList) 
      (recorded current 
        (example $Id $Widget notselected) $_) 
      (selectClause $Widget examples $_)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (changeRule $Widget $ClientData $CallData) 
    ( (recorded editor 
        (textWidget $EditorText) $Ref) 
      (recorded editor 
        (editing rules $ID) $_) 
      (delete-clause $ID) 
      (xmTextGetString $EditorText $RuleCP) 
      (xxmStringToTerm $RuleCP $_ $RuleTerm) 
      (store-clause $RuleTerm $_ user $ID) 
      (proxtCharPtrToString $RuleCP $RulePString) 
      (span-left $RulePString "." $N) 
      (substring $RulePString $RuleString 0 $N $_) 
      (writeMessage ':- delete-clause(') 
      (writeMessage $ID) 
      (writelnMessage ).) 
      (writeMessage ':- store-clause(') 
      (writeMessage $RuleString) 
      (writeMessage ',-,user,') 
      (writeMessage $ID) 
      (writelnMessage ).) 
      (writelnMessage '% rule changed.') 
      (get-clause $ID $H $B $S $L) 
      (addRuleItem $ID $H $B $S $L) 
      (updateEvaluationLabel)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (addExample $Widget $ClientData $CallData) 
    ( (recorded editor 
        (textWidget $EditorText) $Ref) 
      (xmTextGetString $EditorText $ExampleCP) 
      (xxmStringToTerm $ExampleCP $_ $ExampleTerm) 
      (store-ex $ExampleTerm ? $ID) 
      (proxtCharPtrToString $ExampleCP $ExamplePString) 
      (span-left $ExamplePString "." $N) 
      (substring $ExamplePString $ExampleString 0 $N $_) 
      (writeMessage ':- store-ex(') 
      (writeMessage $ExampleString) 
      (writeMessage ,?,) 
      (writeMessage $ID) 
      (writelnMessage ).) 
      (writelnMessage '% example added.') 
      (get-example $ID $F $C) 
      (addExampleItem $ID $F $C) 
      (updateEvaluationLabel)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (changeExample $Widget $ClientData $CallData) 
    ( (recorded editor 
        (textWidget $EditorText) $Ref) 
      (recorded editor 
        (editing examples $ID) $_) 
      (get-example $ID $_ $C) 
      (delete-example $ID) 
      (xmTextGetString $EditorText $ExampleCP) 
      (xxmStringToTerm $ExampleCP $_ $ExampleTerm) 
      (proxtCharPtrToString $ExampleCP $ExamplePString) 
      (span-left $ExamplePString "." $N) 
      (substring $ExamplePString $ExampleString 0 $N $_) 
      (store-ex $ExampleTerm $C $ID) 
      (writeMessage ':- delete-example(') 
      (writeMessage $ID) 
      (writelnMessage ).) 
      (writeMessage ':- store-ex(') 
      (writeMessage $ExampleString) 
      (writeMessage ,?,) 
      (writeMessage $ID) 
      (writelnMessage ).) 
      (writelnMessage '% example changed.') 
      (get-example $ID $F $C) 
      (addExampleItem $ID $F $C) 
      (updateEvaluationLabel)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (xxmStringToTerm $S $Charset $T) 
    ( (var $T) 
      (nonvar $S) 
      (proxtCharPtrToString $S $X1) 
      (tell xmTemporary) 
      (write $X1) 
      (nl) 
      (told) 
      (see xmTemporary) 
      (read $T) 
      (seen) 
      (set-det)))
; 


  (= 
    (xxmStringToTerm $XmS $Charset $T) 
    ( (var $XmS) 
      (nonvar $T) 
      (tell xmTemporary) 
      (write $T) 
      (told) 
      (see xmTemporary) 
      (xxmStringRead $XmS $Charset) 
      (seen) 
      (set-det)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (xxmStringRead $S $Charset) 
    ( (stringRead 100 $S1) 
      (atom-chars $AS $S1) 
      (proxtStringToCharPtr $AS $CP) 
      (xmStringCreateLtoR $CP $Charset $XmS) 
      (| 
        (det-if-then 
          (length $S1 100) 
          (xxmStringAppendRead $XmS $S $Charset)) 
        (det-if-then otherwise 
          (= $S $XmS))) 
      (set-det)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (xxmStringAppendRead $S1 $S $Charset) 
    ( (stringRead 100 $S2) 
      (atom-chars $AS $S2) 
      (proxtStringToCharPtr $AS $CP) 
      (xmStringCreateLtoR $CP $Charset $XmS) 
      (xmStringConcat $S1 $XmS $XmS1) 
      (| 
        (det-if-then 
          (length $S2 100) 
          (xxmStringAppendRead $XmS1 $S $Charset)) 
        (det-if-then otherwise 
          (= $S $XmS1))) 
      (set-det)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (stringRead  0 ()) True)
; 

  (= 
    (stringRead $N $S) 
    ( (get0 $Char) (| (det-if-then (=:= $Char -1) (= $S Nil)) (det-if-then otherwise (, (is $M (- $N 1)) (stringRead $M $S1) (= $S (Cons  $Char $S1)))))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (xxmWriteToString $W $Charset $XmS) 
    ( (tell xmTemporary) 
      (call $W) 
      (told) 
      (see xmTemporary) 
      (xxmStringRead $XmS $Charset) 
      (seen) 
      (set-det)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (xxmWriteToCharPtr $W $CP) 
    ( (tell xmTemporary) 
      (call $W) 
      (told) 
      (see xmTemporary) 
      (stringRead 1023 $S) 
      (seen) 
      (atom-chars $AS $S) 
      (proxtStringToCharPtr $AS $CP) 
      (set-det)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (isDepth $D) 
    (| 
      (, 
        (number $D) 
        (> $D 0) 
        (< $D 101)) 
      (, 
        (| 
          (det-if-then 
            (number $D) 
            (number-chars $D $M1)) 
          (det-if-then otherwise 
            (atom-chars $D $M1))) 
        (writeError % ) 
        (append "'" $M1 $M2) 
        (append $M2 "' is not between the range of 1 - 100!" $M3) 
        (atom-chars $Message $M3) 
        (writelnError $Message) 
        (set-det) 
        (fail))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (isExample $Id) 
    (| 
      (get-example $Id $_ $_) 
      (, 
        (| 
          (det-if-then 
            (number $Id) 
            (number-chars $Id $M1)) 
          (det-if-then otherwise 
            (atom-chars $Id $M1))) 
        (writeError % ) 
        (append "'" $M1 $M2) 
        (append $M2 "' is not an example id!" $M3) 
        (atom-chars $Message $M3) 
        (writelnError $Message) 
        (set-det) 
        (fail))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (isRule $Id) 
    (| 
      (get-clause $Id $_ $_ $_ $_) 
      (, 
        (| 
          (det-if-then 
            (number $Id) 
            (number-chars $Id $M1)) 
          (det-if-then otherwise 
            (atom-chars $Id $M1))) 
        (writeError % ) 
        (append "'" $M1 $M2) 
        (append $M2 "' is not a rule id!" $M3) 
        (atom-chars $Message $M3) 
        (writelnError $Message) 
        (set-det) 
        (fail))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (isExOrRule $Id) 
    (| 
      (get-clause $Id $_ $_ $_ $_) 
      (| 
        (get-example $Id $_ $_) 
        (, 
          (| 
            (det-if-then 
              (number $Id) 
              (number-chars $Id $M1)) 
            (det-if-then otherwise 
              (atom-chars $Id $M1))) 
          (writeError % ) 
          (append "'" $M1 $M2) 
          (append $M2 "' is not a rule id nor an example id!" $M3) 
          (atom-chars $Message $M3) 
          (writelnError $Message) 
          (fail)))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (sucheInListe  $A 
      (Cons  $A $_)) True)
; 

  (= 
    (sucheInListe  $_ ()) True)
; 

  (= 
    (sucheInListe $A 
      (Cons  $_ $Rest)) 
    (sucheInListe $A $Rest))
; 



  (= 
    (writeFullstop $X) 
    ( (write $X) (write .)))
; 


  (= 
    (true  $_) True)
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (createEvaluationString $S) 
    (det-if-then-else 
      (evaluated no) 
      (= $S '(not evaluated)') 
      (= $S '    (evaluated)')))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (updateEvaluationLabel) 
    ( (recorded irene $Widget $_) 
      (createEvaluationString $S) 
      (proxtGetDefaultCharset $DCharset) 
      (proxtStringToCharPtr $S $ACP) 
      (xmStringCreate $ACP $DCharset $AXmS) 
      (xtSetValues $Widget 
        (:: (xmNlabelString $AXmS)))))
; 

  
