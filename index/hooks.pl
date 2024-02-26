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
