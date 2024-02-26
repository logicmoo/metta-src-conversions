;                                           (convert-to-metta-file  mygraphs $_412068 metagame/misc/mygraphs.pl metagame/misc/mygraphs.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; File : GRAPHS.PL
;
; Author : R.A.O'Keefe
;
; Updated: 20 March 1984
;
; Purpose: Graph-processing utilities.


  !(public (, (/ p-to-s-graph 2) (/ s-to-p-graph 2) (/ s-to-p-trans 2) (/ p-member 3) (/ s-member 3) (/ p-transpose 2) (/ s-transpose 2) (/ compose 3) (/ top-sort 2) (/ vertices 2) (/ warshall 2)))
; /* The P-representation of a graph is a list of (from-to) vertex pairs, where the pairs can be in any old order. This form is convenient for input/output. The S-representation of a graph is a list of (vertex-neighbours) pairs, where the pairs are in standard order (as produced by keysort) and the neighbours of each vertex are also in standard order (as produced by sort). This form is convenient for many calculations. p_to_s_graph(Pform, Sform) converts a P- to an S- representation. s_to_p_graph(Sform, Pform) converts an S- to a P- representation. warshall(Graph, Closure) takes the transitive closure of a graph in S-form. (NB: this is not the reflexive transitive closure). s_to_p_trans(Sform, Pform) converts Sform to Pform, transposed. p_transpose transposes a graph in P-form, cost O(|E|). s_transpose transposes a graph in S-form, cost O(|V|^2). */

  !(mode (, (vertices + -) (p-to-s-graph + -) (p-to-s-vertices + -) (p-to-s-group + + -) (p-to-s-group + + - -) (s-to-p-graph + -) (s-to-p-graph + + - -) (s-to-p-trans + -) (s-to-p-trans + + - -) (p-member ? ? +) (s-member ? ? +) (p-transpose + -) (s-transpose + -) (s-transpose + - ? -) (transpose-s + + + -) (compose + + -) (compose + + + -) (compose1 + + + -) (compose1 + + + + + + + -) (top-sort + -) (vertices-and-zeros + - ?) (count-edges + + + -) (incr-list + + + -) (select-zeros + + -) (top-sort + - + + +) (decr-list + + + - + -) (warshall + -) (warshall + + -) (warshall + + + -)))



;
; vertices(S_Graph, Vertices)
;
; strips off the neighbours lists of an S-representation to produce
;
; a list of the vertices of the graph. (It is a characteristic of
;
; S-representations that *every* vertex appears, even if it has no
;
; neighbours.)


  (= (vertices Nil Nil)  
    (set-det))
  (= (vertices (Cons  (- $Vertex $Neighbours) $Graph) (Cons  $Vertex $Vertices))  
    (vertices $Graph $Vertices))




  (= (p-to-s-graph $P_Graph $S_Graph)  
    (sort $P_Graph $EdgeSet)
    (p-to-s-vertices $EdgeSet $VertexBag)
    (sort $VertexBag $VertexSet)
    (p-to-s-group $VertexSet $EdgeSet $S_Graph))



  (= (p-to-s-vertices Nil Nil)  
    (set-det))
  (= (p-to-s-vertices (Cons  (- $A $Z) $Edges) (Cons  $A (Cons  $Z $Vertices)))  
    (p-to-s-vertices $Edges $Vertices))



  (= (p-to-s-group Nil $_ Nil)  
    (set-det))
  (= (p-to-s-group (Cons  $Vertex $Vertices) $EdgeSet (Cons  (- $Vertex $Neibs) $G))  
    (p-to-s-group $EdgeSet $Vertex $Neibs $RestEdges)
    (p-to-s-group $Vertices $RestEdges $G))


  (= (p-to-s-group (Cons  (- $V $X) $Edges) $V (Cons  $X $Neibs) $RestEdges)  
    (set-det)
    (p-to-s-group $Edges $V $Neibs $RestEdges))
  (= (p-to-s-group  $Edges $_ () $Edges)  True)




  (= (s-to-p-graph Nil Nil)  
    (set-det))
  (= (s-to-p-graph (Cons  (- $Vertex $Neibs) $G) $P_Graph)  
    (s-to-p-graph $Neibs $Vertex $P_Graph $Rest_P_Graph)
    (s-to-p-graph $G $Rest_P_Graph))


  (= (s-to-p-graph Nil $_ $P_Graph $P_Graph)  
    (set-det))
  (= (s-to-p-graph (Cons  $Neib $Neibs) $Vertex (Cons  (- $Vertex $Neib) $P) $Rest_P)  
    (s-to-p-graph $Neibs $Vertex $P $Rest_P))




  (= (s-to-p-trans Nil Nil)  
    (set-det))
  (= (s-to-p-trans (Cons  (- $Vertex $Neibs) $G) $P_Graph)  
    (s-to-p-trans $Neibs $Vertex $P_Graph $Rest_P_Graph)
    (s-to-p-trans $G $Rest_P_Graph))


  (= (s-to-p-trans Nil $_ $P_Graph $P_Graph)  
    (set-det))
  (= (s-to-p-trans (Cons  $Neib $Neibs) $Vertex (Cons  (- $Neib $Vertex) $P) $Rest_P)  
    (s-to-p-trans $Neibs $Vertex $P $Rest_P))




  (= (warshall $Graph $Closure)  
    (warshall $Graph $Graph $Closure))


  (= (warshall Nil $Closure $Closure)  
    (set-det))
  (= (warshall (Cons  (- $V $_) $G) $E $Closure)  
    (memberchk 
      (- $V $Y) $E)
    (warshall $E $V $Y $NewE)
    (warshall $G $NewE $Closure))
; ; Y := E(v)


  (= (warshall (Cons  (- $X $Neibs) $G) $V $Y (Cons  (- $X $NewNeibs) $NewG))  
    (memberchk $V $Neibs)
    (set-det)
    (ord-union $Neibs $Y $NewNeibs)
    (warshall $G $V $Y $NewG))
  (= (warshall (Cons  (- $X $Neibs) $G) $V $Y (Cons  (- $X $Neibs) $NewG))  
    (set-det)
    (warshall $G $V $Y $NewG))
  (= (warshall  () $_ $_ ())  True)




  (= (p-transpose Nil Nil)  
    (set-det))
  (= (p-transpose (Cons  (- $From $To) $Edges) (Cons  (- $To $From) $Transpose))  
    (p-transpose $Edges $Transpose))




  (= (s-transpose $S_Graph $Transpose)  
    (s-transpose $S_Graph $Base $Base $Transpose))

  (= (s-transpose Nil Nil $Base $Base)  
    (set-det))
  (= (s-transpose (Cons  (- $Vertex $Neibs) $Graph) (Cons  (- $Vertex Nil) $RestBase) $Base $Transpose)  
    (s-transpose $Graph $RestBase $Base $SoFar)
    (transpose-s $SoFar $Neibs $Vertex $Transpose))


  (= (transpose-s (Cons  (- $Neib $Trans) $SoFar) (Cons  $Neib $Neibs) $Vertex (Cons  (- $Neib (Cons  $Vertex $Trans)) $Transpose))  
    (set-det)
    (transpose-s $SoFar $Neibs $Vertex $Transpose))
  (= (transpose-s (Cons  $Head $SoFar) $Neibs $Vertex (Cons  $Head $Transpose))  
    (set-det)
    (transpose-s $SoFar $Neibs $Vertex $Transpose))
  (= (transpose-s  () () $_ ())  True)



;
; p_member(X, Y, P_Graph)
;
; tests whether the edge (X,Y) occurs in the graph. This always
;
; costs O(|E|) time. Here, as in all the operations in this file,
;
; vertex labels are assumed to be ground terms, or at least to be
;
; sufficiently instantiated that no two of them have a common instance.


  (= (p-member $X $Y $P_Graph)  
    (nonvar $X)
    (nonvar $Y)
    (set-det)
    (memberchk 
      (- $X $Y) $P_Graph))
  (= (p-member $X $Y $P_Graph)  
    (member 
      (- $X $Y) $P_Graph))

;
; s_member(X, Y, S_Graph)
;
; tests whether the edge (X,Y) occurs in the graph. If either
;
; X or Y is instantiated, the check is order |V| rather than
;
; order |E|.


  (= (s-member $X $Y $S_Graph)  
    (var $X)
    (var $Y)
    (set-det)
    (member 
      (- $X $Neibs) $S_Graph)
    (member $Y $Neibs))
  (= (s-member $X $Y $S_Graph)  
    (var $X)
    (set-det)
    (member 
      (- $X $Neibs) $S_Graph)
    (memberchk $Y $Neibs))
  (= (s-member $X $Y $S_Graph)  
    (var $Y)
    (set-det)
    (memberchk 
      (- $X $Neibs) $S_Graph)
    (member $Y $Neibs))
  (= (s-member $X $Y $S_Graph)  
    (memberchk 
      (- $X $Neibs) $S_Graph)
    (memberchk $Y $Neibs))


;
; compose(G1, G2, Composition)
;
; calculates the composition of two S-form graphs, which need not
;
; have the same set of vertices.


  (= (compose $G1 $G2 $Composition)  
    (vertices $G1 $V1)
    (vertices $G2 $V2)
    (ord-union $V1 $V2 $V)
    (compose $V $G1 $G2 $Composition))


  (= (compose Nil $_ $_ Nil)  
    (set-det))
  (= (compose (Cons  $Vertex $Vertices) (Cons  (- $Vertex $Neibs) $G1) $G2 (Cons  (- $Vertex $Comp) $Composition))  
    (set-det)
    (compose1 $Neibs $G2 Nil $Comp)
    (compose $Vertices $G1 $G2 $Composition))
  (= (compose (Cons  $Vertex $Vertices) $G1 $G2 (Cons  (- $Vertex Nil) $Composition))  
    (compose $Vertices $G1 $G2 $Composition))



  (= (compose1 (Cons  $V1 $Vs1) (Cons  (- $V2 $N2) $G2) $SoFar $Comp)  
    (compare $Rel $V1 $V2)
    (set-det)
    (compose1 $Rel $V1 $Vs1 $V2 $N2 $G2 $SoFar $Comp))
  (= (compose1  $_ $_ $Comp $Comp)  True)


  (= (compose1 < $_ $Vs1 $V2 $N2 $G2 $SoFar $Comp)  
    (set-det)
    (compose1 $Vs1 
      (Cons  
        (- $V2 $N2) $G2) $SoFar $Comp))
  (= (compose1 > $V1 $Vs1 $_ $_ $G2 $SoFar $Comp)  
    (set-det)
    (compose1 
      (Cons  $V1 $Vs1) $G2 $SoFar $Comp))
  (= (compose1 = $V1 $Vs1 $V1 $N2 $G2 $SoFar $Comp)  
    (ord-union $N2 $SoFar $Next)
    (compose1 $Vs1 $G2 $Next $Comp))


;                                           (error
;                                             (syntax-error  end_of_file_in_block_comment)
;                                             (stream  <stream>(0x5630f7228a00) 0 1 0))

