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
