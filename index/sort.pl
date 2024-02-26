/* text of window:  sort */
my_sort(Rel,SortAttrs,Tuples,Sorted):-
	rel(Rel,Attrs),
	listdiff(Attrs,SortAttrs,RestAttrs),
	append(SortAttrs,RestAttrs,NewAttrs),
	reorder(Tuples,NewAttrs,NewTuples),
	setof0(T,member(T,NewTuples),TmpSorted),	% quick & dirty
	reorder(TmpSorted,Attrs,Sorted).

reorder([],Attrs,[]).
reorder([T|Ts],Attrs,[NewT|NewTs]):-
	values(Rel,Attrs,Values,T),
	NewT =.. [Rel|Values],
	reorder(Ts,Attrs,NewTs).

splitsort(Ts,Dep,SortedTs):-
	splitsort(Ts,Dep,SortedTs,0,N,0,M).

splitsort(Ts,Dep,SortedTs,(N,M)):-
	splitsort(Ts,Dep,SortedTs,0,N,0,M).

splitsort([],Dep,[],N,N,M,M).
splitsort([Tuple|Tuples],Dep,[Equals|SortedUnEquals],N0,N,M0,M):-
	partition(Tuples,Tuple,Dep,Equals,UnEquals,M0,M1),
	N1 is N0+1,
	!,splitsort(UnEquals,Dep,SortedUnEquals,N1,N,M1,M).

partition([],Tuple,Dep,[Tuple],[],M0,M):-
	M is M0+1.
partition([T|Ts],Tuple,Dep,Es,UnEs,M0,M):-
	horn(Dep,(Head:-(Tuple,Tuple2))),
	( Tuple2 = T	-> Es=[T|Es1],UnEs=UnEs1,M1 is M0+1
	; otherwise 	-> Es=Es1,UnEs=[T|UnEs1],M1 = M0
	),!,partition(Ts,Tuple,Dep,Es1,UnEs1,M1,M).

joinsort(Parts,Dep,NotContr,Contr):-
	joinsort(Parts,Dep,[],NotContr,[],Contr).

joinsort([],Dep,NotContr,NotContr,Contr,Contr).
joinsort([Part|Parts],Dep,NC0,NC,C0,C):-
	contr(Part,[],Dep,Tuples,Answer),
	( Answer = no	-> append(NC0,Part,NC1),C1=C0
	; otherwise	-> NC1=NC0,divide1(Part,[],Dep,DPart),C1=[DPart|C0]
	),!,joinsort(Parts,Dep,NC1,NC,C1,C).
