:- unknown(_,fail).
:- no_style_check(all).

:-compile(library(basics)).
:-compile(library(lists)).
:-compile(library(sets)).
:-compile(library(not)).
% :-compile(library(strings)).

:-dynamic switched_on/1.
:-dynamic level_set/2.

switched_on(cwa).
switched_on(eval).
switched_on(debug).

level_set(acc,+0.8).
level_set(conf,+2).
level_set(split,0.5/0.1).

/* modules */
/*
:-compile(utils),compile(char),compile(eval),compile(hooks),
  compile(object),compile(sort),compile(decomp).
:-compile(inter),compile(commands).
*/
run:-
 commands([],[],[]).


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
:-op(900, xfx, :).
:-op(800, xfx, -->).
:-op(800, xfx, ->->).
:-op(800, xfx, ><).
:-op(800, xfx, <).

/* initialisation */
init_ICs(Rel,ICs):-
 rel(Rel,AList),
 choose_list('Which dependencies',AList,AttrList),
 bagof0(mvd(Rel,[],[A]),member(A,AttrList),IC1),
 bagof0(fd(Rel,[],[A]),member(A,AttrList),IC2),
 append(IC1,IC2,ICs).

/* translation to Horn form */
horn(fd(Rel,FromList,[To]),(A1=A2:-Tuple1,Tuple2)):-
 values(Rel,FromList,ValueList,Tuple1),
 values(Rel,FromList,ValueList,Tuple2),
 value(Rel,To,A1,Tuple1),
 value(Rel,To,A2,Tuple2).
horn(mvd(Rel,FromList,ToList),(Tuple1:-Tuple2,Tuple3)):-
 rel(Rel,AttrList),
 listdiff(AttrList,FromList,List),
 listdiff(List,ToList,RestList),
 values(Rel,FromList,FromValues,Tuple1),
 values(Rel,FromList,FromValues,Tuple2),
 values(Rel,FromList,FromValues,Tuple3),
 values(Rel,ToList,ToValues,Tuple1),
 values(Rel,ToList,ToValues,Tuple2),
 values(Rel,RestList,RestValues,Tuple1),
 values(Rel,RestList,RestValues,Tuple3).
horn(join(Rel,R1,R2),(Tuple1:-Tuple2,Tuple3)):-
 rel(R1,R1Attrs),
 rel(R2,R2Attrs),
 values(Rel,R1Attrs,R1Values,Tuple1),
 values(Rel,R2Attrs,R2Values,Tuple1),
 values(R1,R1Attrs,R1Values,Tuple2),
 values(R2,R2Attrs,R2Values,Tuple3).
horn(plus(Rel,List),(Tuple:-Body)):-
 rel(Rel,RelAttrs),
 values(Rel,RelAttrs,Values,Tuple),
 make_body(List,Values,Body).
horn(proc(Rel),Proc):-
 proc(Rel,Proc).	% works only for one-clause procs

make_body([R],Values,L):-!,
 rel(R,Attrs),
 values(R,Attrs,Values,L).
make_body([R|Rs],Values,(L;Ls)):-
 rel(R,Attrs),
 values(R,Attrs,Values,L),
 make_body(Rs,Values,Ls).

/* refinements on the meta-level */
refinements(fd(Rel,From,[To]),[+T1,+T2],ICs):-!,
 diff(T1,T2,Diff),
 bagof0(fd(Rel,[Attr|From],[To]),fd_spec(To,Diff,Attr),ICs).
refinements(mvd(Rel,From,To),[+T1,+T2,-T3],ICs):-!,
 diff(T1,T3,D13),
 diff(T2,T3,D23),
 bagof0(mvd(Rel,[Attr|From],ST),mvd_spec(Rel,From,To,D13,D23,Attr,ST),ICs).
refinements(IC,Tuples,ICs):-
 display(IC,DisplayIC),
 write(DisplayIC),show_list(' is contradicted by',Tuples),
 ic_spec(IC,ICs).

fd_spec(To,Diff,Attr):-
 member(Attr,Diff),
 Attr\=To.

mvd_spec(Rel,From,To,Diff,_,Attr,ST):-
 member(Attr,Diff),
 remove(Attr,To,ST), ST \= [],
 compl(mvd(Rel,[Attr|From],ST),mvd(Rel,[Attr|From],SCT)),
 SCT \= [].
mvd_spec(Rel,From,To,_,Diff,Attr,ST):-
 member(Attr,Diff),
 remove(Attr,To,ST), ST \= [],
 compl(mvd(Rel,[Attr|From],ST),mvd(Rel,[Attr|From],SCT)),
 SCT \= [].

ic_spec(IC,[R|Rs]):-
 prompt_read('refinement',DR),!,
 display(R,DR),
 ic_spec(IC,Rs).
ic_spec(IC,[]).

/* subsumption test */
subsumed(fd(Rel,From1,To),fd(Rel,From2,To)):-
 subset(From2,From1).
subsumed(mvd(Rel,From1,To),mvd(Rel,From2,To)):- !,
 subset(From2,From1).
subsumed(mvd(Rel,From1,To1),mvd(Rel,From2,To2)):-
 subset(From2,From1),
 compl(mvd(Rel,From1,To1),mvd(Rel,From1,To2)).

compl(mvd(Rel,From,To),mvd(Rel,From,CTo)):-
 rel(Rel,AttrList),
 listdiff(AttrList,From,AL1),
 listdiff(AL1,To,CTo).

/* compression */
compress(In,Out):-
 select(fd(Rel,From,To1),In,In1),
 select(fd(Rel,From,To2),In1,In2),!,
 append(To1,To2,To),
 compress([fd(Rel,From,To)|In2],Out).
compress(ICs,ICs).

/* dependencies only */
dep(fd(Rel,From,To),fd,Rel,From,To).
dep(mvd(Rel,From,To),mvd,Rel,From,To).

/* display form */
display(fd(Rel,From,To),(Rel: From --> To)):-!.
display(mvd(Rel,From,To),(Rel: From ->-> To)):-!.
display(join(Rel,R1,R2),(Rel = R1 >< R2)):-!.
display(plus(Rel,List),(Rel = List)):-!.
display(proc(Rel),(calculated:Rel)):-!.
display(X,X).

/* templates */
template(all,X):-!.
template(fd,fd(Rel,From,To)):-!.
template(mvd,mvd(Rel,From,To)):-!.
template(join,join(Rel,R1,R2)):-!.
template(plus,plus(Rel,List)):-!.
template(proc,proc(Rel)):-!.
template(R,Tuple):-
 rel(R,A),!,
 values(R,A,V,Tuple).
template(X,X).
/* object level <--> meta level */
diff(Tuple1,Tuple2,Diff):-		% old diff
 diff(Tuple1,Tuple2,Rel,Equal,Diff).

diff(Tuple1,Tuple2,Rel,Equal,Diff):-	% new diff (24-2-93)
 Tuple1=..[Rel|Values1],
 Tuple2=..[Rel|Values2],
 rel(Rel,AttrList),
 diff1(Values1,Values2,AttrList,Equal,Diff).

diff1([],[],[],[],[]).			% new diff1 (24-2-93)
diff1([V|Values1],[V|Values2],[A|AttrList],[A|Equal],Diff):-
 diff1(Values1,Values2,AttrList,Equal,Diff).
diff1([V1|Values1],[V2|Values2],[A|AttrList],Equal,[A|Diff]):-
 V1\=V2,
 diff1(Values1,Values2,AttrList,Equal,Diff).

values(_,[],[],_).
values(Rel,[Attr|AttrList],[Value|ValueList],Tuple):-
 value(Rel,Attr,Value,Tuple),
 values(Rel,AttrList,ValueList,Tuple).

value(Rel,Attr,Value,Tuple):-
 rel(Rel,AttrList),
 al2vl(Attr,AttrList,Value,ValueList),
 Tuple=..[Rel|ValueList].

al2vl(_,[],_,[]).
al2vl(Attr,[Attr|AttrList],Value,[Value|ValueList]):-
 al2vl(Attr,AttrList,Value,ValueList).
al2vl(Attr,[A|AttrList],Value,[V|ValueList]):-
 ( Attr\=A ; Value\=V ),
 al2vl(Attr,AttrList,Value,ValueList).
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
/* interaction */
show_lists(Filter,Texts,Lists):-
 show_lists(Filter,Texts,1,[],Lists).

show_lists(Filter,Words,N,Text,[]).
show_lists(Filter,[],N,Text,[H|T]):-
 show_list(Filter,Text,[H|T]).
show_lists(Filter,[Word|Words],1,Text,[H]):-!,
 show_lists(Filter,Words,1,Text,H).
show_lists(Filter,[Word|Words],N,Text,[H|T]):-
 append(Text,[Word,N,' --- '],NewText),
 show_lists(Filter,Words,1,NewText,H),
 N1 is N+1,
 show_lists(Filter,[Word|Words],N1,Text,T).

show_list(Filter,Text,List):-
 filter(List,Filter,Filtered),
 show_list(Text,Filtered).

show_list(Text,[]):- !,
 write_list(['There are no ',Text,'.']),nl.
show_list(Text,List):-
 write_list([Text,':']),
 display_list(List,List1),
 show_items(List1),nl.

choose_list(Text,List,Sel):-
 display_list(List,List1),
 choose_items(1,List1),
 write_list([Text,'? ']),
 read(Ns),
 ( nths(List,Ns,[],Sel) -> true
 ; otherwise		-> write('Wrong number! Try again.'),nl,
			   choose_list(Text,List,Sel)
 ).

nths(List,all,[],List):-!.
nths(In,(A-A),Tmp,Out):-!,
 nths(In,A,Tmp,Out).
nths(In,(A-B),Tmp,Out):-!,
 A<B,A1 is A+1,
 nths(In,A,Tmp,Tmp1),
 nths(In,(A1-B),Tmp1,Out).
nths(In,(N,Ns),Tmp,Out):-!,
 nths(In,N,Tmp,Tmp1),
 nths(In,Ns,Tmp1,Out).
% nths([],1,Tmp,[X|Tmp]):-!,
%  write('Which one? '),read(X).
nths([X|R],1,Tmp,[X|Tmp]):-!.
nths([X|R],N,Tmp,Out):-
 N1 is N-1,
 nths(R,N1,Tmp,Out).

show_items([]):-
 ( switched_on(horn)		-> true
 ; not switched_on(horn)	-> nl
 ).
show_items([H|T]):-
 ( switched_on(horn)		-> portray_clause(H)
 ; not switched_on(horn)	-> nl,write_list(['	',H])
 ),
 show_items(T).

choose_items(N,[]):-
%  nl,write_list(['   ',N,'. Other...']),
 nl.
choose_items(N,[H|T]):-
 nl,write_list(['   ',N,'. ',H]),
 N1 is N+1,
 choose_items(N1,T).

display_list([],[]).
display_list([H|T],[DH|DT]):-
 ( switched_on(horn) ->	displayhorn(H,DH)
 ; otherwise ->		display(H,DH)
 ),
 display_list(T,DT).

displayhorn(X,HX):-
 horn(X,HX),!.
displayhorn(X,X).

prompt_read(Question,Answer):-
 write_list([Question,'? ']),read(Answer),
 ( Answer = stop -> fail
 ; otherwise	 -> true ).

yesno(Question):-
 write_list(Question),read(Answer),
 ( Answer = yes -> true
 ; Answer = no  -> fail
 ; otherwise    -> call(Answer),yesno(Question)
 ).

write_debug(Message):-
 not switched_on(debug).
write_debug(Message):-
 switched_on(debug),
 write('	| '),write_list(Message),nl.

write_list(List):-
 flatten(List,FList),
 write_list1(FList).

write_list1([]).
write_list1([H|T]):-
 write(H),write_list1(T).

quit:-
 abort.
/* commands */
keyword1((save),'   	save in Prolog database').
keyword1((get),'   	get from Prolog database').
keyword1((show),'   	show current').
keyword1((count),'   	count current').
keyword1((del),'   	delete').
keyword1((add),'   	add new').
keyword1((init),'   	initialise constraints').
keyword1((find),'   	find constraints').
keyword1((check),'   	check validity').
keyword1((decomp),'   	decompose relation').
keyword1((comp),'   	compose relations').
keyword1((switch),'   	switch on or off').
keyword1((set),'   	set level').
keyword1((help),'   	get help').

keyword2((pos),'   	positive tuples').
keyword2((neg),'   	negative tuples').
keyword2((ics),'   	integrity constraints').
keyword2((calc),'   	calculated relations').

switch(cwa,'    	Closed-World Assumption').
switch(horn,'    	display in Horn form').
switch(eval,'   	evaluate constraints').
switch(debug,'   	show debugging information').

level(conf,'    	confirmation level of constraint').
level(acc,'     	accuracy of constraint').
level(split,'     	splitting level of constraint').

:-forall(T^keyword1(W,T),op(1200,fx,W)).
:-forall(T^keyword2(W,T),op(1100,fx,W)).
:-forall(T^level(W,T),op(1100,fx,W)).

commands(InICs,PosIn,NegIn):-
 prompt_read('',Command),
 do_command(Command,InICs,PosIn,NegIn,OutICs,PosOut,NegOut),
 !,commands(OutICs,PosOut,NegOut).
commands(InICs,PosIn,NegIn):-
 write('no'),nl,
 !,commands(InICs,PosIn,NegIn).

do_command(save(X),ICs,Pos,Neg,ICs,Pos,Neg):-!,
 save_command(X,ICs,Pos,Neg).
do_command(get(X),InICs,PosIn,NegIn,OutICs,PosOut,NegOut):-!,
 get_command(X,InICs,PosIn,NegIn,OutICs,PosOut,NegOut).
do_command(show(X),ICs,Pos,Neg,ICs,Pos,Neg):-!,
 show_command(X,ICs,Pos,Neg).
do_command(count(X),ICs,Pos,Neg,ICs,Pos,Neg):-!,
 count_command(X,ICs,Pos,Neg).
do_command(del(X),InICs,PosIn,NegIn,OutICs,PosOut,NegOut):-!,
 del_command(X,InICs,PosIn,NegIn,OutICs,PosOut,NegOut).
do_command(add(X),InICs,PosIn,NegIn,OutICs,PosOut,NegOut):-!,
 add_command(X,InICs,PosIn,NegIn,OutICs,PosOut,NegOut).
do_command(init(X),InICs,Pos,Neg,OutICs,Pos,Neg):-!,
 init_command(X,InICs,OutICs).
do_command(find(X),InICs,PosIn,NegIn,OutICs,PosOut,NegOut):-!,
 find_command(X,InICs,PosIn,NegIn,OutICs,PosOut,NegOut).
do_command(check(X),ICs,Pos,Neg,ICs,Pos,Neg):-!,
 check_command(X,ICs,Pos,Neg).
do_command(decomp(IC),InICs,PosIn,Neg,OutICs,PosOut,Neg):-!,
 decompose(IC,PosIn,PosOut,InICs,OutICs).
do_command(comp(Rule),ICs,PosIn,Neg,ICs,PosOut,Neg):-!,
 exec_rule(Rule,PosIn,PosNew),
 append(PosIn,PosNew,PosOut).
do_command(switch(X),ICs,Pos,Neg,ICs,Pos,Neg):-!,
 switch(X).
do_command(set(X),ICs,Pos,Neg,ICs,Pos,Neg):-!,
 set(X).
do_command(help(Topic),ICs,Pos,Neg,ICs,Pos,Neg):-!,
 help_command(Topic).
do_command(Command,ICs,Pos,Neg,ICs,Pos,Neg):-
 call(Command).
do_command(Command,ICs,Pos,Neg,ICs,Pos,Neg):-
 write('?').

save_command(pos(Rel),ICs,Pos,Neg):-
 save_pos(Rel,Pos).
save_command(neg(Rel),ICs,Pos,Neg):-
 save_neg(Rel,Neg).
save_command(ics(T),ICs,Pos,Neg):-
 save_ics(T,ICs).

get_command(pos(Rel),ICs,PosIn,Neg,ICs,PosOut,Neg):-
 get_pos(Rel,Pos),
 append(PosIn,Pos,PosOut).
get_command(neg(Rel),ICs,Pos,NegIn,ICs,Pos,NegOut):-
 get_neg(Rel,Neg),
 append(NegIn,Neg,NegOut).
get_command(ics(T),InICs,Pos,Neg,OutICs,Pos,Neg):-
 get_ics(T,ICs),
 append(InICs,ICs,OutICs).

show_command(all,ICs,Pos,Neg):-!,
 show_list('positive tuples',Pos),
 show_list('negative tuples',Neg),
 show_list('integrity constraints',ICs).
show_command(rel,ICs,Pos,Neg):-!,
 bagof0(R,A^rel(R,A),Rels),
 show_list('relations',Rels).
show_command(Other,ICs,Pos,Neg):-
 get_list(Other,ICs,Pos,Neg,List,Text),!,
 show_list(Text,List).
show_command(Wrong,ICs,Pos,Neg):-
 show_list('choices',['show all','show rel',
                      'show pos all','show pos Rel',
                      'show neg all','show neg Rel',
                      'show ics all','show ics Rel']).

count_command(Filter,ICs,Pos,Neg):-
 get_list(Filter,ICs,Pos,Neg,List,Text),
 length(List,N),
 write_list(['There are ',N,' ',Text,'.']),nl.

del_command(all,ICs,Pos,Neg,[],[],[]).
del_command(ics(F),InICs,Pos,Neg,OutICs,Pos,Neg):-
 filter(InICs,F,Deleted,OutICs).
del_command(pos(Rel),ICs,PosIn,Neg,ICs,PosOut,Neg):-
 filter(PosIn,Rel,Deleted,PosOut).
del_command(neg(Rel),ICs,Pos,NegIn,ICs,Pos,NegOut):-
 filter(NegIn,Rel,Deleted,NegOut).
del_command(rel(R),ICs,PosIn,NegIn,ICs,PosOut,NegOut):-
 retract(rel(R,AList)),
 filter(PosIn,Rel,_,PosOut),
 filter(NegIn,Rel,_,NegOut).
del_command(calc(Rel),InICs,Pos,Neg,OutICs,Pos,Neg):-
 retractall(proc(Rel,Proc)),
 remove(proc(Rel),InICs,OutICs).

add_command(ics(DisplayIC),ICs,Pos,Neg,[IC|ICs],Pos,Neg):-
 display(IC,DisplayIC).
add_command(pos(Tuple),ICs,Pos,Neg,ICs,[Tuple|Pos],Neg).
add_command(neg(Tuple),ICs,Pos,Neg,ICs,Pos,[Tuple|Neg]).
add_command(rel(R,AList),ICs,Pos,Neg,ICs,Pos,Neg):-
 new_rel(R,AList).
add_command(calc(Rel),InICs,PosIn,Neg,[proc(Rel)|OutICs],PosOut,Neg):-
 ask_proc(Rel),
 filter(PosIn,Rel,RelTuples,_),
 setof0(T,(member(T,RelTuples),satisfied(T,[],[],[])),SatTuples),
 listdiff(PosIn,SatTuples,PosOut),
 remove(proc(Rel),InICs,OutICs).
 

init_command(ics(Rel),InICs,OutICs):-
 init_ICs(Rel,ICs),
 append(InICs,ICs,OutICs).

find_command((ics),InICs,PosIn,NegIn,OutICs,PosOut,NegOut):-
 char(InICs,PosIn,NegIn,OutICs,PosOut,NegOut).

check_command(ics(DisplayIC),ICs,Pos,Neg):-
 display(IC,DisplayIC),
 check_ics(IC,Pos,Neg).
check_command(pos(Tuple),ICs,Pos,Neg):-!,
 forall(member(IC,ICs),check_ics(IC,[Tuple|Pos],Neg)).

help_command(commands):-!,
 bagof0(W:T,keyword1(W,T),L),
 show_list('Commands',L).
help_command(filters):-!,
 bagof0(W:T,keyword2(W,T),L),
 show_list('Filters',L).
help_command(switches):-!,
 bagof0(W:T,switch(W,T),L),
 show_list('Switches',L).
help_command(levels):-!,
 bagof0(W:T,level(W,T),L),
 show_list('Levels',L).
help_command(_):-
 help_command(commands),
 help_command(filters),
 help_command(switches),
 help_command(levels).

get_list(pos(Filter),ICs,Pos,Neg,List,'positive tuples'):-
 filter(Pos,Filter,List).
get_list(neg(Filter),ICs,Pos,Neg,List,'negative tuples'):-
 filter(Neg,Filter,List).
get_list(ics(Filter),ICs,Pos,Neg,List,'integrity constraints'):-
 filter(ICs,Filter,Tmp),
 compress(Tmp,List).

filter(In,F,Out):-
 template(F,Template),
 setof0(Template,member(Template,In),Out).

filter(In,F,Out,Rest):-
 filter(In,F,Out),
 listdiff(In,Out,Rest).

new_rel(R,Attrs):-
 rel(R,Attrs),!.
new_rel(R,Attrs):-
 rel(R,A),!,
 write('Error: relation name already in use'),nl,
 fail.
new_rel(R,Attrs):-
 assert(rel(R,Attrs)).

ask_proc(Rel):-
 prompt_read('clause',Clause),
 assert(proc(Rel,Clause)),!,
 ask_proc(Rel).
ask_proc(Rel).

get_pos(Rel,Pos):-
 template(Rel,Tuple),
 bagof0(Tuple,pos_tuple(Tuple),Pos).

get_neg(Rel,Neg):-
 template(Rel,Tuple),
 bagof0(Tuple,neg_tuple(Tuple),Neg).

get_ics(T,ICs):-
 template(T,Templ),
 bagof0(Templ,constraint(Templ),ICs).

save_pos(Rel,Pos):-
 template(Rel,T),
 forall(member(T,Pos),myassert(pos_tuple(T))).

save_neg(Rel,Neg):-
 template(Rel,T),
 forall(member(T,Neg),myassert(neg_tuple(T))).

save_ics(T,ICs):-
 template(T,Templ),
 forall(member(Templ,ICs),myassert(constraint(Templ))).

check_ics(IC,Pos,Neg):-
 display(IC,DisplayIC),
 evaluate(Pos,Neg,IC,Tuples,PosOut,NegOut,Answer),
 write(DisplayIC),
 ( Answer=refine	-> show_list(' is contradicted by',Tuples)
 ; Answer=keep		-> write(' is satisfied'),nl
 ; Answer=keep(E)	-> write_list([' looks promising: ',E]),nl
 ; Answer=ignore(E)	-> write_list([' has low confirmation: ',E]),nl
 ).

switch(X):-
 switch(X,T),
 retract(switched_on(X)),!,
 write_list([X,' is now off.']),nl.
switch(X):-
 switch(X,T),!,
 assert(switched_on(X)),
 write_list([X,' is now on.']),nl.
switch(_):-
 setof0(X,switched_on(X),L1),
 show_list('switches ON',L1),
 setof0(Y,T^(switch(Y,T),not switched_on(Y)),L2),
 show_list('switches OFF',L2).

set(X):-
 X =.. [Level|Rest],
 level(Level,T),!,
 ( Rest=[Value] ->	( retract(level_set(Level,V)),! ; true ),
 			assert(level_set(Level,Value))
 ; Rest=[] ->	get_level(Level,L),write(Level=L),nl
 ).
set(_):-
 setof0(L=V,T^(level(L,T),get_level(L,V)),L),
 show_list('levels',L).

get_level(Level,Value):-
 level(Level,T),
 ( level_set(Level,Value),! ; Value=0 ).

compare(X):-
 X =.. [Level,Value],
 get_level(Level,L),
 ( L= +V ->	Value >= V
 ; L= -V ->	Value =< V
 ; L= V/A ->	Upper is V+A,Value =< Upper,
 		Lower is V-A,Value >= Lower
 ; otherwise ->	write_list(['Wrong level: ',Level=L]),nl,
		break
 ).


:-run.
