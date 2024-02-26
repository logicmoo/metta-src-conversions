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
