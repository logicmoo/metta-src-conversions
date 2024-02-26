;                                           (convert_to_metta_file  floyd $_430894 metagame/misc/floyd.pl metagame/misc/floyd.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;; floyd.pl
;
; ;; Barney Pell
;
; ;; University of Cambridge
;
; ;; 1992


  (= (s-to-d-graph Nil Nil)  
    (set-det))
; /* From graphs.pl: The P-representation of a graph is a list of (from-to) vertex pairs, where the pairs can be in any old order. This form is convenient for input/output. The S-representation of a graph is a list of (vertex-neighbours) pairs, where the pairs are in standard order (as produced by keysort) and the neighbours of each vertex are also in standard order (as produced by sort). This form is convenient for many calculations. New addition: The D-representation of a weighted digraph is a list of (vertex-neighbours) pairs, where the pairs are in standard order (as produced by keysort) and the neighbours of each vertex are also in standard order (as produced by sort). Unlike the S-representation, though, each neighbour is a (vertex-weight) pair, which thus weights the edge from non-weighted S-representation of a graph. s_to_d_graph(SForm, Dform) converts an S-rep to a D-rep by giving each edge a weight of 1. d_to_s_graph(SForm, Dform) converts a D-rep to S-rep by dropping the weights. s_floyd(Graph,Closure) computes the reachability matrix for a graph in S-form, where the closure is in D-form. (NB: this is not the reflexive transitive closure). floyd(Graph,Closure) computes the all-pairs shortest-path distance matrix for a weighted digraph in D-form, with the closure also in D-form. (NB: this is not the reflexive transitive closure). */
  (= (s-to-d-graph (Cons  (- $Vertex $Neibs1) $G) (Cons  (- $Vertex $Neibs2) $D))  
    (init-dists $Neibs1 $Neibs2)
    (s-to-d-graph $G $D))


  (= (init_dists  () ())  True)
  (= (init-dists (Cons  $H $T) (Cons  (- $H 1) $Ts))  
    (init-dists $T $Ts))
	

  (= (d-to-s-graph Nil Nil)  
    (set-det))
  (= (d-to-s-graph (Cons  (- $Vertex $Neibs1) $G) (Cons  (- $Vertex $Neibs2) $D))  
    (strip-dists $Neibs1 $Neibs2)
    (d-to-s-graph $G $D))


  (= (strip_dists  () ())  True)
  (= (strip-dists (Cons  (- $H $_) $T) (Cons  $H $Ts))  
    (strip-dists $T $Ts))



  (= (s-floyd $S_Graph $Closure)  
    (s-to-d-graph $S_Graph $Graph)
    (floyd $Graph $Closure))

;
; Replaces all diagonal entries with 0.

  (= (zero-self-d-graph Nil Nil)  
    (set-det))
  (= (zero-self-d-graph (Cons  (- $Vertex $Neibs1) $G) (Cons  (- $Vertex $Neibs2) $D))  
    (ord-min-union 
      (:: (- $Vertex 0)) $Neibs1 $Neibs2)
    (zero-self-d-graph $G $D))


;
; FLOYD(+Graph, -Closure)
;
; =======================
;
; is true when Closure is the all-pairs-shortest-path solution
;
; to the weighted digraph Graph.
;
; 
;
; Graph is a weighted-digraph:
;
; [V_1-Edges_1,...,V_n-Edges_n]
;
; Each Edges is a list of weighted vertices:
;
; EdgesI = [V_j-Weight_ij
;
; 
;
; 
;
; Result is a new weighted-digraph, where W_ij is the
;
; weight of the shortest path from V_i to V_j in the original
;
; graph.
;
; 
;
; This is O(N**3).
;
; This was based on Richard O'Keefe's implementation of warshall/2,
;
; and the description of Floyd's algorithm given in AHO.


  (= (floyd $Graph $Closure)  
    (zero-self-d-graph $Graph $Init)
    (floyd $Init $Init $Closure))

  (= (floyd Nil $Closure $Closure)  
    (set-det))
  (= (floyd (Cons  (- $V $_) $G) $E $Closure)  
    (memberchk 
      (- $V $Y) $E)
    (floyd $E $V $Y $NewE)
    (floyd $G $NewE $Closure))
; ; Y := E(v)

  (= (floyd (Cons  (- $X $Neibs) $G) $V $Y (Cons  (- $X $NewNeibs) $NewG))  
    (memberchk 
      (- $V $VDist) $Neibs)
    (set-det)
    (increment-dists $Y $VDist $YInc)
    (ord-min-union $Neibs $YInc $NewNeibs)
    (floyd $G $V $Y $NewG))
  (= (floyd (Cons  (- $X $Neibs) $G) $V $Y (Cons  (- $X $Neibs) $NewG))  
    (set-det)
    (floyd $G $V $Y $NewG))
  (= (floyd  () $_ $_ ())  True)


  (= (increment_dists  () $_ ())  True)
  (= (increment-dists (Cons  (- $H $D) $T) $N (Cons  (- $H $D1) $T1))  
    (is $D1 
      (+ $D $N))
    (increment-dists $T $N $T1))


;
; ord_min_union(+Set1, +Set2, ?Union)
;
; 
;
; just like ord_union, except our elements are Elt-Weight
;
; pairs instead of just Elts. Then Union is like the ord_union
;
; except when both sets of have two same Elt's with different
;
; weights, the minimum weight is kept.


  (= (ord_min_union  () $Set2 $Set2)  True)
  (= (ord-min-union (Cons  $Head1 $Tail1) $Set2 $Union)  
    (ord-min-union-1 $Set2 $Head1 $Tail1 $Union))


  (= (ord_min_union_1  () $Head1 $Tail1 (Cons  $Head1 $Tail1))  True)
  (= (ord-min-union-1 (Cons  $Head2 $Tail2) $Head1 $Tail1 $Union)  
    (weighted-compare $Order $Head1 $Head2)
    (ord-min-union-1 $Order $Head1 $Tail1 $Head2 $Tail2 $Union))

  (= (ord-min-union-1 < $Head1 $Tail1 $Head2 $Tail2 (Cons  $Head1 $Union))  
    (ord-min-union-1 $Tail1 $Head2 $Tail2 $Union))
  (= (ord-min-union-1 > $Head1 $Tail1 $Head2 $Tail2 (Cons  $Head2 $Union))  
    (ord-min-union-1 $Tail2 $Head1 $Tail1 $Union))
  (= (ord-min-union-1 = (- $Head1 $N1) $Tail1 (- $Head1 $N2) $Tail2 (Cons  (- $Head1 $N) $Union))  
    (min $N1 $N2 $N)
    (ord-min-union $Tail1 $Tail2 $Union))


  (= (weighted-compare $Order (- $Head1 $N1) (- $Head2 $N2))  
    (compare $Order $Head1 $Head2))




  (= (time-floyd $P $N)  
    (random-graph $P $N $G)
    (s-to-d-graph $G $Z)
    (runtime (floyd $Z $ZLast)))
; /* Testing */


