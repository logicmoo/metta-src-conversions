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
:-compile(utils),compile(char),compile(eval),compile(hooks),
  compile(object),compile(sort),compile(decomp).
:-compile(inter),compile(commands).

run:-
 commands([],[],[]).

:-run.
