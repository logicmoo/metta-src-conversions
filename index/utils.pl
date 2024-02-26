/* utilities */
listdiff(L,[],L).
listdiff(L,[H|T],V):-
 remove(H,L,L1),
 listdiff(L1,T,V).

remove(_,[],[]).
remove(H,[H|T],L):-
 remove(H,T,L).
remove(X,[H|T],[H|L]):-
 remove(X,T,L),
 X\=H.

select_two([H|T],H,Y):-
 member(Y,T).
select_two([H|T],X,Y):-
 select_two(T,X,Y).

forall(Goal,Condition):-
 bagof0(Condition,Goal,List),
 forall1(List).

forall1([]).
forall1([H|T]):-
 call(H),
 forall1(T).

bagof0(T,G,L):-
 bagof(T,G,L),!.
bagof0(T,G,[]).

setof0(T,G,L):-
 setof(T,G,L),!.
setof0(T,G,[]).

myassert(X):-
 not call(X),
 assert(X).

add_if(X,Ys,Ys):-
 member(X,Ys),!.
add_if(X,Ys,[X|Ys]).

flatten(Xs,Ys):-
 flatten_dl(Xs,[],Ys).

flatten_dl([],Ys,Ys):-!.
flatten_dl([X|Xs],Ys0,Ys):-!,
 flatten_dl(Xs,Ys0,Ys1),
 flatten_dl(X,Ys1,Ys).
flatten_dl(X,Xs,[X|Xs]).
