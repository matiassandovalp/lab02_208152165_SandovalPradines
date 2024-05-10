
train(ID, MAKER, RAIL, SPEED, PCARS, [ID, MAKER, RAIL, SPEED, PCARS]):-
	integer(ID),
	string(MAKER),
	string(RAIL),
	integer(SPEED),
	SPEED > 0,
	is_list(PCARS),
	sameModel(PCARS),
	consistency(PCARS).


sameModel([]).
sameModel([CAR|CDR]):-
	pcar(_, _, MODEL, _, CAR),
	same(CDR, MODEL).

same([], _).
same([CAR|CDR], MODEL):-
	pcar(_, _, MODEL, _, CAR),
	sameModel(CDR, MODEL).



%Extrae elementos de la lista excepto el final
init([], []).

init([_], []).

init([X|Xs], [X|Init]) :-
	init(Xs, Init).


consistency([FIRST_CAR|CARS]):-
	pcar(_, _, _, "tr", FIRST_CAR),
	init(CARS, MIDDLE_CARS),
	last(CARS, LAST_CAR),
	consistencyMiddle(MIDDLE_CARS),
	pcar(_, _, _, "tr", LAST_CAR).


consistencyMiddle([]).

consistencyMiddle([CAR|REST]):-
	pcar(_, _, _, "ct", CAR),
	consistency_middle(REST).


trainAddCar(TRAIN, PCAR, POSITION, TRAINOUT):-
	train(ID, MAKER, RAIL, SPEED, PCARS, TRAIN),
	addCar(PCARS, PCAR, POSITION, PCAROUT),
	train(ID, MAKER, RAIL, SPEED, PCAROUT, TRAINOUT).


%recursion que añade un elemento en una posición
addCar(PCARLIST, NEWPCAR, 0, [NEWPCAR|PCARLIST]):-!.

addCar([CAR|CDR], NEWPCAR, POS, [CAR|RES]):-
    POS > 0,
    POS1 is POS - 1,
    addCar(CDR, NEWPCAR, POS1, RES).