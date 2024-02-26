evaluate1(P,IC,Answer) :-
 switched_on(eval),
 dep(IC,Type,Rel,From,To),
 filter(P,Rel,P1,Rest),
 % write_debug(['splitsort...']),
 splitsort(P1,IC,SplitP,NumbersC),
 calc_conf(NumbersC,Conf),
 ( compare(conf(Conf)) -> % write_debug(['divides...']),
 			  divides(SplitP,IC,NewP,NumbersA),
 			  calc_acc(NumbersA,Acc),
 			  ( compare(acc(Acc)) -> Answer=keep(acc(Acc))
 			  ; compare(split(Acc)) -> Answer=keep(split(Acc)) )
 ; otherwise ->		  Answer=ignore(conf(Conf))
 ).

calc_conf((NDivs,NTuples),Confirmation):-
 Confirmation is NTuples/NDivs.

calc_acc(Numbers,Accuracy):-
 calc_acc1(Numbers,0,NTuples,0,NLarge,0,MaxNP),
 Accuracy is 1-((MaxNP-1)*(NTuples-NLarge))/NTuples.

calc_acc1([],NT,NT,NL,NL,MaxNP,MaxNP).
calc_acc1([D|Ds],NT0,NT,NL0,NL,MaxNP0,MaxNP):-
 eval2(D,0,Sum,0,Largest,0,NParts),
 NT1 is NT0+Sum,
 NL1 is NL0+Largest,
 ( NParts>MaxNP0 -> MaxNP1=NParts
 ; otherwise -> MaxNP1=MaxNP0
 ),calc_acc1(Ds,NT1,NT,NL1,NL,MaxNP1,MaxNP).

eval2([],S,S,L,L,M,M).
eval2([N|Ns],S0,S,L0,L,M0,M):-
 S1 is S0+N,
 ( N>L0 -> L1=N
 ; otherwise -> L1=L0
 ),M1 is M0+1,
 eval2(Ns,S1,S,L1,L,M1,M).

insert_ic(L,IC,E,[IC|L]).
