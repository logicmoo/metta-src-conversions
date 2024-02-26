/* text of window:  decomp */
decompose(DisplayIC,PosIn,PosOut,InICs,OutICs):-
 display(IC,DisplayIC),
 contr(PosIn,[],IC,Tuples,Answer),
 ( Answer=no	-> split(IC,PosIn,PosOut,InICs,OutICs)
 ; otherwise	-> divide(PosIn,IC,PosOut,InICs,OutICs)
 ).

split(Dep,PosIn,PosOut,ICs,[Join|ICs]):-
 dep(Dep,Type,Rel,From,To),
 split1(Rel,From,To,PosIn,PosOut,Join).

split1(Rel,From,To,PosIn,PosOut,join(Rel,R1,R2)):-
 append(From,To,In1),
 new_name(In1,Out1,R1),
 rel(Rel,AttrList),
 listdiff(AttrList,To,In2),
 new_name(In2,Out2,R2),
 splits(Rel,R1,R2,Out1,Out2,PosIn,PosTmp),
 remove_dups(PosTmp,PosOut).

splits(Rel,R1,R2,AList1,AList2,[],[]).
splits(Rel,R1,R2,AList1,AList2,[T|Ts],[T1,T2|Rest]):-
 values(Rel,AList1,ValueList1,T),!,
 T1=..[R1|ValueList1],
 values(Rel,AList2,ValueList2,T),
 T2=..[R2|ValueList2],
 splits(Rel,R1,R2,AList1,AList2,Ts,Rest).
splits(Rel,R1,R2,AList1,AList2,[T|Ts],[T|Rest]):-
 % T not in Rel
 splits(Rel,R1,R2,AList1,AList2,Ts,Rest).

new_name(AListIn,AListOut,R):-
 show_list('attributes',AListIn),
 prompt_read('relation name',Answer),
 ( Answer = -	-> concat_atom(AListIn,R),AListOut=AListIn
 ; Answer = +	-> prompt_read('rel(Name,AttrList)',rel(R,AListOut)),
		   permutation(AListOut,AListIn)
 ; otherwise	-> R=Answer,AListOut=AListIn
 ),new_rel(R,AListOut).

exec_rule(DisplayRule,PosIn,PosOut):-
 display(Rule,DisplayRule),
 horn(Rule,(Tuple:-Body)),
 setof0(Tuple,Ts^satisfied(Body,PosIn,[],Ts),PosOut).

divide(PosIn,Dep,PosOut,InICs,[plus(Rel,Names)|OutICs]):-
 dep(Dep,Type,Rel,From,To),
 filter(PosIn,Rel,PosFiltered,Rest),
 splitsort(PosFiltered,Dep,SplitPos),
% divides(SplitPos,Dep,NewPos,Numbers),
 joinsort(SplitPos,Dep,NotContr,Contr),
 find_division(Rel,NotContr,Contr,Numbers,NewPos1),
 new_preds(Rel,NewPos1,[],PosTmp1,[],Names),
 append(PosTmp1,Rest,PosTmp2),
 decomp_again(Names,Dep,PosTmp2,PosOut,InICs,OutICs).

decomp_again([],Dep,Pos,Pos,ICs,ICs).
decomp_again([Name|Names],Dep,PosIn,PosOut,InICs,OutICs):-
 ( yesno(['Decompose ',Name,'? ']) ->
	dep(Dep,Type,R,From,To),
	dep(NewDep,Type,Name,From,To),
	display(NewDep,DDep),
	decompose(DDep,PosIn,PosTmp,InICs,TmpICs)
 ; otherwise -> PosTmp=PosIn,TmpICs=InICs
 ),decomp_again(Names,Dep,PosTmp,PosOut,TmpICs,OutICs).

divides([],Dep,[],[]).
divides([H|T],Dep,[NewH|NewT],[NsH|NsT]):-
 length(H,LH),
 divide1(H,LH,_,[],0,_,Dep,NewH,NsH),
 !,divides(T,Dep,NewT,NsT).

divide1(Pos1,Pos2,IC,PosOut):-
 divide1(Pos1,0,_,Pos2,0,_,IC,PosOut,_).

divide1([],K,K,[],L,L,IC,[],[]):-!.
divide1(Pos1,K0,K,Pos2,L0,L,IC,PosOut,M):-
 contr(Pos1,[],IC,Tuples,Answer),
 ( ( Answer=yes ; Answer=possibly ) ->	Tuples=[+T1,+T2|N],
					remove(T2,Pos1,NewPos1),K1 is K0-1,
					NewPos2=[T2|Pos2],L1 is L0+1,
					PosOut=NewPosOut,M=NewM
 ; Answer=no -> NewPos1=Pos2,K1=L0,
		NewPos2=[],L1=0,
		PosOut=[Pos1|NewPosOut],M=[K0|NewM]
 ),!,divide1(NewPos1,K1,K,NewPos2,L1,L,IC,NewPosOut,NewM).

find_division(Rel,NotContr,Contr,Numbers,NewPos):-
 ( NotContr = [] -> NewPos = Contr
 ; otherwise	 -> NewPos = [[NotContr]|Contr]
 ),show_lists(Rel,[segment,part],NewPos),
 yesno(['Proceed? ']).

new_preds(Rel,[],New,New,Names,Names).
new_preds(Rel,[H|T],New0,New,Names0,Names):-
 new_preds1(Rel,H,New0,New1,Names0,Names1),
 new_preds(Rel,T,New1,New,Names1,Names).

new_preds1(Rel,[],New,New,Names,Names).
new_preds1(Rel,[R|Rs],New0,New,Names0,Names):-
 show_list(Rel,'partial relation',R),
 prompt_read('relation name',N),
 rel(Rel,Attrs),
 new_rel(N,Attrs),
 new_relation(Rel,R,N,NewR),
 append(New0,NewR,New1),
 add_if(N,Names0,Names1),
 new_preds1(Rel,Rs,New1,New,Names1,Names).

new_relation(Rel,R,N,NewR):-
 bagof0(NewT,T^(member(T,R),new_tuple(Rel,T,N,NewT)),NewR).

new_tuple(Rel,T,N,NewT):-
 T=..[Rel|Args],!,
 NewT=..[N|Args].
new_tuple(Rel,T,N,T).	% tuple from other relation
