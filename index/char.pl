/* characterisation */
char(InICs,PosIn,NegIn,OutICs,PosOut,NegOut):-
 char1(InICs,PosIn,NegIn,[],TmpICs,PosOut,NegOut),
 cleanup(TmpICs,[],OutICs).

char1([],P,N,Out,Out,P,N).
char1([IC|ICs],P0,N0,Acc,Out,P,N) :-
 write_debug(['evaluating ',IC]),
 evaluate(P0,N0,IC,Tuples,P1,N1,Answer),
 ( Answer = refine ->	refinements(IC,Tuples,Spec),
 			append(ICs,Spec,NewICs),
			NewAcc=Acc
 ; Answer = keep ->	NewICs=ICs,NewAcc=[IC|Acc]
 ; Answer = ignore(E) -> NewICs=ICs,NewAcc=Acc
 ; Answer = keep(E) ->	NewICs=ICs,insert_ic(Acc,IC,E,NewAcc)
 ),write_debug(['	result: ',Answer]),
 !,char1(NewICs,P1,N1,NewAcc,Out,P,N).

evaluate(P0,N0,IC,Tuples,P,N,Answer) :-
 ( evaluate1(P0,IC,Answer) -> P=P0,N=N0,Tuples=[]
 ; otherwise ->
	% write_debug(['contr...']),
 	contr(P0,N0,IC,T,A),
 	( A = yes ->		Answer = refine,P=P0,N=N0,Tuples=T
 	; A = no ->		Answer = keep,  P=P0,N=N0,Tuples=T
 	; A = possibly ->	queries(P0,N0,T,P1,N1),
				evaluate(P1,N1,IC,Tuples,P,N,Answer)
	)
 ).

contr(P,N,IC,Tuples,Answer) :-
 horn(IC,HornIC),
 incons(HornIC,P,N,Tuples,Answer).

queries(P,N,[],P,N).
queries(P,N,[T|Ts],P1,N1):-
 query(P,N,T,P2,N2),
 queries(P2,N2,Ts,P1,N1).

query(P,N,-T,[T|P],N) :-
 not switched_on(cwa),yesno(['Is ',T,' in the relation? ']),!.
query(P,N,-T,P,[T|N]):-
 not switched_on(cwa).
query(P,N,-T,P,[T|N]):-
 switched_on(cwa),write(-T),nl.
query(P,N,+T,P,N).

cleanup([X|In],Acc,Out):-
 member(IC,In),subsumed(X,IC),!,
 cleanup(In,Acc,Out).
cleanup([X|In],Acc,Out):-
 member(IC,Acc),subsumed(X,IC),!,
 cleanup(In,Acc,Out).
cleanup([X|In],Acc,Out):-
 cleanup(In,[X|Acc],Out).
cleanup([],Out,Out).

incons((Head:-Body),P,N,Tuples,Answer):-
 satisfied(Body,P,N,TuplesB),
 falsified(Head,P,N,TuplesH),!,
 Answer=yes,
 append(TuplesB,TuplesH,Tuples).
incons((Head:-Body),P,N,Tuples,Answer):-
 satisfied(Body,P,N,TuplesB),
 unsatisfied(Head,P,N,TuplesH),!,
 Answer=possibly,
 append(TuplesB,TuplesH,Tuples).
incons((Head:-Body),P,N,[],Answer):-
 Answer=no.

satisfied((A,B),P,N,Tuples):-
 satisfied(A,P,N,TuplesA),
 satisfied(B,P,N,TuplesB),
 append(TuplesA,TuplesB,Tuples).
satisfied((A;B),P,N,Tuples):-
 satisfied(A,P,N,Tuples)
 ; satisfied(B,P,N,Tuples).
satisfied(A,P,N,[+A]):-
 member(A,P).
satisfied(A=A,P,N,[]).	% fds only
satisfied(A,P,N,[]):-
 proc(Rel,Proc),
 exec_proc(A,Proc).

exec_proc(Goal,(P1,P2)):-
 exec_proc(Goal,P1)
 ; exec_proc(Goal,P2).
exec_proc(Goal,(Goal:-Body)):-
 call(Body).

falsified((A,B),P,N,Tuples):-!,
 falsified(A,P,N,Tuples)
 ; falsified(B,P,N,Tuples).
falsified(A,P,N,[-A]):-
 member(A,N).
falsified(A=B,P,N,[]):-	% fds only
 A \= B.

unsatisfied((A,B),P,N,Tuples):-!,
 unsatisfied(A,P,N,Tuples)
 ; unsatisfied(B,P,N,Tuples).
unsatisfied(A,P,N,[-A]):-
 \+ satisfied(A,P,N,_),
 \+ falsified(A,P,N,_).
