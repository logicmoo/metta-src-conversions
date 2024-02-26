/*************************************************************************** 
   FILE: FOIL.PL
   PROGRAMMER: John Zelle
   DATE: 2/10/92
   DESCRIPTION: 
       A Prolog implementation of the FOIL (First Order Inductive
       Learning) algorithm for learning relational concept definitions.
       For backgound on the algorithm, see :

       Quinlan, J. R., "Learning Logical Definitions from Relations,"
       in Machine Learning, 5, 1990.  
    
       The version presented here is somewhat simplified in that it
       uses a much weaker test to constrain recursive predicates
       (a recursive call must contain vars not found in the head of
       of a clause, and may not introduce any unbound vars), and it
       does not incorporate encoding length restrictions to handle 
       noisy data.  There is also no post-processing of clauses to 
       simplify learned definitions, although this would be relatively 
       easy to add.

       This is a very simple implementation which recomputes tuple
       sets "on the fly".  Don't expect it to run like the wind.
       

   LANGUAGE: Quintus Prolog v. 3.1

   MODIFIED: 3/30/92 (JMZ) Added determinate literals.  See article:

        Quinlan, J. R., "Determinate Literals in Inductive Logic
	Programming," in Proceedings of the Eighth International
	Workshop in Machine Learning, 1991

   INPUT FORM:  The background knowledge for predicate induction
        is represented as "existential" predicates asserted in 
	the module, foil_input.  By existential is is meant that
	the definitions there must be fully constructive to avoid
	instantiation errors when running FOIL.

   EXAMPLE: Definition of "list" from Quinlan's paper.  This is
     an example input file.  After consulting this definition of
     the module foil_input, the definition of list can be learned
     by invoking: foil(list(_)).
----------------------------------------------------------------------
%Background and tuples for learning definition of list. 

% This heading should be on every file used to provide
% test data for FOIL

:- module(foil_input, 
	  [foil_predicates/1, foil_use_negations/1, foil_det_lit_bound/1]).

foil_predicates([ list/1, null/1, components/3 ]).
foil_use_negations(false).        % Don't use negations of foil_predicates
foil_det_lit_bound(0).            % Don't add any determinate literals
                                  % In general, this is a depth limit on
			          %   the search for determinate literals

% Definitions of background predicates
null([]).

components([a], a, []).
components([b, [a], d], b, [[a], d]).
components([[a],d], [a], [d]).
components([d], d, []).
components([e|f], e, f).

list([]).
list([a]).
list([b, [a], d]).
list([[a], d]).
list([d]).
---------------------------------------------------------------------------


***************************************************************************/

:- ensure_loaded(library(occurs)).
:- ensure_loaded(library(basics)).
:- ensure_loaded(library(lists)).
:- ensure_loaded(library(between)).
:- ensure_loaded(library(math)).
:- use_module(library(ordsets),[ord_add_element/3]).

% Run foil to attempt finding a definition for Goal and then print
% out the resulting clauses. e. g. foil(list(_)).
foil(Goal) :-
	foil(Goal,Clauses),
	format("~n~nFOUND DEFINITION:~n~n",[]),
	portray_clauses(Clauses).

% Pretty print a list of clauses.
portray_clauses([]) :- nl.
portray_clauses([H|T]):-
	portray_clause(H),
	portray_clauses(T).

% run foil and print running time stats.
foil_time(Goal) :-
	statistics(runtime, [T0|_]),
	foil(Goal),
	statistics(runtime, [T1|_]),
	T is T1 - T0,
	format("~nRun Time = ~3d sec.~n",[T]).

% Find the positive examples of Goal and construct a set of negative
% examples using a closed world hypothesis.
get_examples(Goal, Pos, Neg) :-
	findall(Goal, foil_input:Goal, Pos),
	create_negatives(Pos, Neg).

% Clauses is the set of clauses defining Goal found by FOIL. Negative examples
% are provided by a closed world assumption.
foil(Goal, Clauses) :-
	get_examples(Goal, Positives, Negatives),
	foil_loop(Positives, Goal, Negatives, [], Clauses).

% FOIL provided explicit negative examples.
foil(Goal, Negatives, Clauses) :-
	findall(Goal, foil_input:Goal, Positives),
	foil_loop(Positives, Goal, Negatives, [], Clauses).

% Each iteration constructs a clause.  Pos is positive examples left to
% be covered, Goal is the concept being defined, Neg is a list of the 
% negative examples and Clauses0 is the list of clauses found in previous
% iterations.
foil_loop(Pos, Goal, Neg, Clauses0, Clauses) :-
	( Pos = [] ->
	      Clauses = Clauses0
	; format("~nUncovered positives adding a clause~n~w~n",[Pos]),
	  extend_clause_loop(Neg, Pos, (Goal :- true), Clause),
	  format("~n~nClause Found: ~n", []),
	  portray_clause(Clause),
	  uncovered_examples(Clause, Pos, Pos1),
	  foil_loop(Pos1, Goal, Neg, [Clause|Clauses0], Clauses)
	).

% Add antecendents to Clause0 until it covers no negatives. Or no more info
% gain is achieved.
extend_clause_loop(Nxs0, Pxs0, Clause0, Clause) :-
	( Nxs0 = [] ->
	      Clause = Clause0
	; format("~n     Current Clause: ~w ~n     --Specializing~n", [Clause0]),
	  format("covered negatives~n~w~n",[Nxs0]),
	  format("covered positives~n~w~n",[Pxs0]),
	  generate_possible_extensions(Clause0, Ls),
	  info_value(Clause0, Pxs0, Nxs0, Info),
	  best_next_clause(Ls, 
			   Nxs0, Pxs0, Clause0, Info, 
			   0, Clause0, 
			   Clause1),
	  ( Clause0 == Clause1 ->
		foil_det_lit_bound(DLB),
		format("No Improvement -- Trying Determinate Literals~n",[]),
		bounded_determinate_literals(DLB, Ls, Clause0, Pxs0, Nxs0, Ds),
		( Ds = [] ->
		      format("No Determinate Literals Found~n",[]),
		      covered_examples(Clause1, Nxs0, Nxs1),
		      format("WARINING--clause covers negatives~n~w~n",[Nxs1]),
		      Clause = Clause1
		; format("Adding Determinate Literals: ~w~n", [Ds]),
		  add_literals(Ds, Clause0, Clause2),
		  covered_examples(Clause2, Nxs0, Nxs1),
		  extend_clause_loop(Nxs1, Pxs0, Clause2, Clause)
		)
	  ; covered_examples(Clause1, Pxs0, Pxs1),
	    covered_examples(Clause1, Nxs0, Nxs1),
	    extend_clause_loop(Nxs1, Pxs1, Clause1, Clause)
	  )
	).
	
% Find the clause which is an extension of Clause by a single literal and
% provides maximum info gain over the original clause.
best_next_clause([], _, _, _, _, _, Clause, Clause).
best_next_clause([L|Ls], 
                 Nxs, Pxs, Clause, Info,
                 Gain0, Best0, 
                 Best) :-
	add_literal(L, Clause, Best1),
	compute_gain(Nxs, Pxs, Info, Best1, Gain1),
%	format("~w ~4f~n", [L, Gain1]),
	( Gain1 > Gain0 ->
	      best_next_clause(Ls, Nxs, Pxs, Clause, Info, Gain1, Best1, Best)
	; Gain1 =:= Gain0 ->
	      choose_tie_clause(Best0, Best1, Best2),
	      best_next_clause(Ls, Nxs, Pxs, Clause, Info, Gain0, Best2, Best)
	; best_next_clause(Ls, Nxs, Pxs, Clause, Info, Gain0, Best0, Best)
	).

choose_tie_clause((A1:-B1), (A2:-B2), C) :-
	variables_in(B1, V1),
	length(V1, N1),
	variables_in(B2, V2),
	length(V2, N2),
	( N2 < N1 -> C = (A2:-B2)  ;  C = (A1:-B1) ).
	      
% For a set of positive and negative examples Pxs and Nxs, compute the
% information gain of Clause over a clause which produces a split having
% Info, as it's "information value" on these examples.
compute_gain(Nxs, Pxs, Info, Clause, Gain) :-
	covered_examples(Clause, Pxs, Retained),
	length(Retained, R),
	( R =:= 0 ->
	      Gain = 0
	; info_value(Clause, Pxs, Nxs, Info1),
	  Gain is R * (Info - Info1)
	).
	
% Compute the information matric for the set of positive and negative
% tuples which result from applying Clause to the examples Pxs and NXs
info_value(Clause, Pxs, Nxs, Value) :-
	tuples(Clause, Pxs, Ptuples),
	length(Ptuples, P),
	( P =:= 0 ->
	      Value = 0
	; tuples(Clause, Nxs, Ntuples),
	  length(Ntuples, N),
	  Temp is P/(P+N),
	  log(Temp, Temp1),
	  Value is Temp1 * -1.442695
	).

% Add a literal to the right end of a clause
add_literal(L, (A :- B), (A :- B1)) :-
	( B = true ->
	      B1 = L
	; B1 = (B,L)
	).

add_literals(Ls, Clause0, Clause) :-
	( Ls = [] ->
	      Clause = Clause0
	; Ls = [L|Ls1],
	  add_literal(L, Clause0, Clause1),
	  add_literals(Ls1, Clause1, Clause)
	).
	

% Construct a list representing the set of variables in Term.
/* Changed from setof definition to correctly handle dterminate
   literals code with clauses containing \+
*/
variables_in(A, Vs) :- variables_in(A, [], Vs).
	
variables_in(A, V0, V) :-
	var(A), !, ord_add_element(V0, A, V).
variables_in(A, V0, V) :-
	ground(A), !, V = V0. 
variables_in(Term, V0, V) :-
	functor(Term, _, N),
	variables_in_args(N, Term, V0, V).

variables_in_args(N, Term, V0, V) :-
	( N =:= 0 ->
	      V = V0
	; arg(N, Term, Arg),
	  variables_in(Arg, V0, V1),
	  N1 is N-1,
	  variables_in_args(N1, Term, V1, V)
	).

% Given a clause and a list of examples, construct the list of tuples
% for the clause.  A tuple is the binding of values to variables such
% that the clause can be used to prove the example.
tuples((A :- B), Xs, Tuples) :-
	variables_in((A :- B), Vars),
	variables_in(A, HeadVars),
	length(HeadVars, N1),
	length(Vars, N2),
	( N1 =:= N2 ->
	      % shortcut - only need 1 proof if no new variables.
	      findall(Vars, (member(A, Xs), \+(\+ foil_input:B)), Tuples)
	; findall(Vars, (member(A,Xs), foil_input:B), Tuples)
	).

% Xs1 are the examples from Xs that can be proved with the clause
covered_examples((A :- B), Xs, Xs1) :-
	findall(A, ( member(A,Xs), \+( \+ foil_input:B ) ), Xs1).

% Xs1 are the examples from Xs that cannot be proved with the clause.
uncovered_examples((A:-B), Xs, Xs1) :-
	findall(A, ( member(A, Xs), \+ foil_input:B ), Xs1 ).

%---------------------------------------------------------------------------
% Ugly code to generate possible literals

generate_possible_extensions((A :- B), Extensions) :-
	variables_in((A :- B), OldVars),
	bagof(L, candidate_literal(A, OldVars, L), Extensions).

possible_unification([], [], []).
possible_unification([H|T], [H|Result], [H|Vars]) :-
	possible_unification(T,Result,Vars).
possible_unification([H|T], [H|T1], Vs) :-
	possible_unification(T, T1, Vs),
	member(V,Vs),
	H = V.

list_of_n_from(Elements, N, List0, List) :-
	( N is 0 ->
	      List = List0
	; N1 is N -1,
	  member(E, Elements),
	  list_of_n_from(Elements, N1, [E|List0], List)
	).

possible_new_vars(true,_,[]).
possible_new_vars(false, N, L) :-
	length(L,N).
possible_new_vars(false, N, L) :-
	N > 0,
	N1 is N-1,
	possible_new_vars(false, N1, L).

bind_vars(Lit, Vars, Index) :-
	( Vars = [] ->
	      true
	; Vars = [H|T],
 	  arg(Index, Lit, H),
	  Index1 is Index + 1,
	  bind_vars(Lit, T, Index1)
	).

recursion_check(G, Pred, Arity, Flag) :-
	( functor(G, Pred, Arity) ->
	      Flag = true
	; Flag = false
	).

candidate_literal(Goal, OldVars, Lit) :-
	foil_predicates(Preds),
	member(Pred/Arity, Preds),
	functor(L, Pred, Arity),
	recursion_check(Goal, Pred, Arity, RecursionFlag),
	MaxNewVars is Arity - 1,
	possible_new_vars(RecursionFlag, MaxNewVars, NewVars),
	length(NewVars, NewVarPositions),
	OldVarPositions is Arity - NewVarPositions,
	list_of_n_from(OldVars, OldVarPositions, [], OldVarSeq),
	recursion_safe(RecursionFlag, Goal, OldVarSeq),
	possible_unification(NewVars, NewVarSeq, _),
	subseq(VarSeq, OldVarSeq, NewVarSeq),
	bind_vars(L, VarSeq, 1),
	( Lit = L ;  foil_use_negations(true), Lit = (\+ L) ).

recursion_safe(RecursionFlag, Goal, OldVarSeq) :-
	( RecursionFlag = true ->
	      \+ (numbervars(Goal, 0, _), ground(OldVarSeq))
	; true
	).
%---------------------------------------------------------------------------
% Closed World Assumption

create_universe(Universe) :-
	setof(Term, term_of_ext_def(Term), Universe).

term_of_ext_def(Term) :-
	foil_predicates(PredSpecs),
	member(Pred/Arity, PredSpecs),
	functor(Goal, Pred, Arity),
	foil_input:Goal,
	between(1, Arity, ArgPos),
	arg(ArgPos, Goal, Term).

create_negatives([P|Ps], Negatives) :-
	functor(P, F, N),
	functor(Template, F, N),
	create_universe(Universe),
	setof(Template,
	      (  arguments_are_members(Template, N, Universe),
	         \+ member(Template, [P|Ps]) ),
	      Negatives).
	      
arguments_are_members(Term, N, Universe) :-
	( N > 0 ->
	      arg(N, Term, Arg),
	      member(Arg, Universe),
	      N1 is N-1,
	      arguments_are_members(Term, N1, Universe)
	; true
	).
	
%---------------------------------------------------------------------------
% Determinate Literals

/* determinate(+Lit, +Vars, +PTuples, +NTuples) -- holds if Lit is a
   determinate literal wrt the bindings for Vars as represented in
   PTuples and NTuples.
*/

determinate(L, Vars, PTuples, NTuples) :-
	binds_new_var(L, Vars),
	determ_cover(PTuples, L, Vars),
	determ_partial_cover(NTuples, L, Vars).

binds_new_var((\+ _),_) :- !, fail.
binds_new_var(L, Vars) :-
	variables_in(L, LVars),
	member(V,LVars),
	\+ contains_var(V, Vars),
	!.

determ_cover([], _, _).
determ_cover([T|Ts], Lit, Vars) :-
	findall(Lit, (Vars = T, foil_input:Lit), [_]),
	determ_cover(Ts, Lit, Vars).

determ_partial_cover([], _, _).
determ_partial_cover([T|Ts], Lit, Vars) :-
	findall(Lit, (Vars=T, foil_input:Lit), Xs),
	(Xs = [] ; Xs = [_]),
	determ_partial_cover(Ts, Lit, Vars).

determinate_literals1(Cands, Body, Vars, PTuples, NTuples, DLits) :-
	bagof(X,
	      ( member(X, Cands), 
	        determinate(X, Vars, PTuples, NTuples),
	        \+( (numbervars(Vars,0,_), ante_memberchk(X,Body)) )
	      ), 
	      DLits).

determinate_literals(Cands, Clause, Pxs, Nxs, DLits) :-
	variables_in(Clause, Vars),
	tuples(Clause, Pxs, PTuples),
	tuples(Clause, Nxs, NTuples),
	Clause = (_:-Body),
	determinate_literals1(Cands, Body, Vars, PTuples, NTuples, DLits).

bounded_determinate_literals(0, _, _, _, _, []) :- !.
bounded_determinate_literals(Bound, Cands, (A:-B), Pxs, Nxs, DLits) :-
	determinate_literals(Cands, (A:-B), Pxs, Nxs, DLits0),
	reachable_antes(Bound, A, DLits0, DLits).

ante_memberchk(A,A) :- !.
ante_memberchk(A, (B,C)) :-
	( ante_memberchk(A,B) ->
	      true
	; ante_memberchk(A,C)
	).

/* reachable_antes(+Bound, +H, +Cands, -Antes) -- Antes is the list of
literals from Cands which can be "connected" to H by some chain of
variables of length <= Bound. */

reachable_antes(Bound, H, Cands, Antes) :-
	variables_in(H, Vs),
	expand_by_var_chain(Bound, Cands, Vs, [], Antes).

expand_by_var_chain(Bound, Cands, Vars, As0, As) :-
	( Bound =:= 0 ->
	      As = As0
	; partition_on_vars(Cands, Vars, Haves, Havenots),
	  ( Haves = [] ->
		As = As0
	  ; append(As0, Haves, As1),
	    variables_in(As1, Vars1),
	    Bound1 is Bound - 1,
	    expand_by_var_chain(Bound1, Havenots, Vars1, As1, As)
	  )
	).

partition_on_vars([], _, [], []).
partition_on_vars([C|Cs], Vars, Hs, Hnots) :-
	( member(V, Vars), contains_var(V, C) ->
	      Hs = [C|Hs1],
	      Hnots = Hnots1
	; Hs = Hs1,
	  Hnots = [C|Hnots1]
	),
	partition_on_vars(Cs, Vars, Hs1, Hnots1).


