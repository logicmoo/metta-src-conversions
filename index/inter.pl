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
