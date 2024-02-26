;                                           (convert_to_metta_file  group $_444924 metagame/learning/group.pl metagame/learning/group.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================



  (= (do-graph $P_Graph $S_Graph)  
    (sort $P_Graph $EdgeSet)
    (do-vertices $EdgeSet $VertexBag)
    (sort $VertexBag $VertexSet)
    (do-group $VertexSet $EdgeSet $S_Graph))



  (= (do-vertices Nil Nil)  
    (set-det))
  (= (do-vertices (Cons  (^ $A $Z) $Edges) (Cons  $A $Vertices))  
    (do-vertices $Edges $Vertices))



  (= (do-group Nil $_ Nil)  
    (set-det))
  (= (do-group (Cons  $Vertex $Vertices) $EdgeSet (Cons  (- $Vertex $Neibs) $G))  
    (do-group $EdgeSet $Vertex $Neibs $RestEdges)
    (do-group $Vertices $RestEdges $G))


  (= (do-group (Cons  (^ $V $X) $Edges) $V (Cons  $X $Neibs) $RestEdges)  
    (set-det)
    (do-group $Edges $V $Neibs $RestEdges))
  (= (do_group  $Edges $_ () $Edges)  True)


