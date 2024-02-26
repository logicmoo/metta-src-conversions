%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Builtin Predicates of Prolog Cafe
%
% Mutsunori Banbara (banbara@kobe-u.ac.jp)
% Naoyuki Tamura (tamura@kobe-u.ac.jp)
% Kobe University
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :- op(1150,  fx, (package)).
%package(X):- nb_setval(package,X).

% :-package('TauMachine.builtin_tau').

%:- public system_predicate/1.
%:- multifile(system_predicate/1).
%:- dynamic(system_predicate/1).


/*
system_predicate(nb_setarg(_,_,_)).
system_predicate(setarg(_,_,_)).

system_predicate(nb_get_attr(_,_,_)).
system_predicate(nb_put_attr(_,_,_)).

system_predicate(nb_get_attrs(_,_)).
system_predicate(nb_put_attrs(_,_)).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Control constructs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :- public undo/1.

/*
'$builtin_meta_predicates'(undo, 1, [:]).
'$builtin_meta_predicates'(undo1, 1, [:]).
'$builtin_meta_predicates'(redo_each_call, 3, [:,:,:]).
'$builtin_meta_predicates'(one_of_or_else, 3, [:,:,:]).
*/


%(IF -> THEN; _ELSE) :- call(IF), !, call(THEN).
%(_IF -> _THEN; ELSE) :- call(ELSE).

forall(G1,G):- \+ \+ ( call(G1), \+ \+ call(G2)).

undo(G):- G.

redo_each_call(EachSetup,Call,EachCleanup):-
  setup_call_cleanup(EachSetup,Call,EachCleanup).
  
( IF *-> THEN ; ELSE ) :- 
     AtLeastOnce = dotwo(true), 
     ((call(IF),nb_setarg(1,AtLeastOnce,fail));
       (AtLeastOnce = dotwo(true);Else)).

one_of_or_else(If,Then,Else):-
  ( IF *-> THEN ; ELSE ).



% END
