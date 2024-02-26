:-dynamic rel/2.
:-dynamic pos_tuple/1.
:-dynamic neg_tuple/1.

rel(train,[direction,hour,minutes,stop1]).

% pos_tuple(train(utrecht,8,8,den-bosch)).
% pos_tuple(train(tilburg,8,10,tilburg)).
% pos_tuple(train(maastricht,8,10,weert)).
% pos_tuple(train(utrecht,8,13,eindhoven-bkln)).
% pos_tuple(train(tilburg,8,17,eindhoven-bkln)).
% pos_tuple(train(utrecht,8,25,den-bosch)).
% pos_tuple(train(utrecht,8,31,utrecht)).
% pos_tuple(train(utrecht,8,43,eindhoven-bkln)).
% pos_tuple(train(tilburg,8,47,eindhoven-bkln)).
% pos_tuple(train(utrecht,9,8,den-bosch)).
% pos_tuple(train(tilburg,9,10,tilburg)).
% pos_tuple(train(maastricht,9,10,weert)).
% pos_tuple(train(utrecht,9,13,eindhoven-bkln)).
% pos_tuple(train(tilburg,9,17,eindhoven-bkln)).
% pos_tuple(train(utrecht,9,25,den-bosch)).
% pos_tuple(train(utrecht,9,43,eindhoven-bkln)).
% pos_tuple(train(tilburg,9,47,eindhoven-bkln)).

pos_tuple(train(Direction,Hour,Minutes,Stop1)):-
	normaltrain(Direction,Hour,Minutes,Stop1).
pos_tuple(train(Direction,Hour,Minutes,Stop1)):-
	specialtrain(Direction,Hour,Minutes,Stop1).

normaltrain(Direction,Hour,Minutes,Stop1):-
	hour(Hour),
	fasttrain(Direction,Minutes,Stop1).
normaltrain(Direction,Hour,Minutes,Stop1):-
	hour(Hour),
	slowtrain(Direction,Minutes,Stop1).

fasttrain(Direction,Minutes,Stop1):-
	fasttrain(Direction,Minutes),
	fasttrain_stop1(Direction,Stop1).

slowtrain(Direction,Minutes,Stop1):-
	slowtrain(Direction,Minutes),
	slowtrain_stop1(Direction,Stop1).
slowtrain(Direction,Minutes,Stop1):-
	slowtrain(Direction,Minutes1),
	Minutes is Minutes1+30,
	slowtrain_stop1(Direction,Stop1).

fasttrain(utrecht,08).
fasttrain(tilburg,10).
fasttrain(maastricht,10).
fasttrain(utrecht,25).

fasttrain_stop1(utrecht,den-bosch).
fasttrain_stop1(tilburg,tilburg).
fasttrain_stop1(maastricht,weert).

slowtrain(utrecht,13).
slowtrain(tilburg,17).

slowtrain_stop1(utrecht,eindhoven-bkln).
slowtrain_stop1(tilburg,eindhoven-bkln).

specialtrain(utrecht,8,31,utrecht).

hour(8).
hour(9).
% hour(10).
% hour(11).
