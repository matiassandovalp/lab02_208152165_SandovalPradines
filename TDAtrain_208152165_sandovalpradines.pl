:- module('TDAtrain_208152165_sandovalpradines.pl', []).
:- use_module('TDApcar_208152165_sandovalpradines.pl', [pcar/5]).


train(ID, MAKER, RAIL, SPEED, PCARS, [ID, MAKER, RAIL, SPEED, PCARS]):-
	integer(ID),
	string(MAKER),
	string(RAIL),
	integer(SPEED),
	SPEED > 0,
	is_list(PCARS),
	sameModel(PCARS),
	writeln("Todos los trenes tienen el mismo modelo"),
	consistency(PCARS),
	writeln("Los carros son consistentes en su estructura"), !.

getTrainID(TRAIN, ID):-
	train(ID, _, _, _, _, TRAIN), !.
	
getTrainMaker(TRAIN, MAKER):-
	train(_, MAKER, _, _, _, TRAIN), !.

getTrainRailType(TRAIN, RAIL):-
	train(_, _, RAIL, _, _, TRAIN), !.

getTrainSpeed(TRAIN, SPEED):-
	train(_, _, _, SPEED, _, TRAIN), !.

getTrainPcars(TRAIN, PCARLIST):-
	train(_, _, _, _, PCARLIST, TRAIN), !.


sameModel([]):- true, !.

sameModel([CAR|CDR]):-
	pcar(_, _, MODEL, _, CAR),
	same(CDR, MODEL).


same([], _).

same([CAR|CDR], MODEL):-
	pcar(_, _, MODEL, _, CAR),
	same(CDR, MODEL).


%Extrae elementos de la lista excepto el final
init([], []).

init([_], []).

init([X|Xs], [X|Init]) :-
	init(Xs, Init).

consistency([]):- true, !.

consistency([FIRST_CAR|CARS]):-
	pcar(_, _, _, "tr", FIRST_CAR),
	init(CARS, MIDDLE_CARS),
	last(CARS, LAST_CAR),
	consistencyMiddle(MIDDLE_CARS),
	pcar(_, _, _, "tr", LAST_CAR).


consistencyMiddle([]).

consistencyMiddle([CAR|REST]):-
	pcar(_, _, _, "ct", CAR),
	consistencyMiddle(REST).


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
	
/*
pcar(5, 90, "ModelE", "tr", P1), pcar(42, 12, "ModelE", "ct", P2), pcar(123, 1233, "ModelE", "ct", P3), pcar(22222, 1324, "ModelE", "ct", P4), pcar(12, 351, "ModelE", "tr", P5).
pcar(5, 90, "ModelE", "tr", P1), pcar(42, 12, "ModelE", "ct", P2), pcar(123, 1233, "ModelE", "ct", P3), pcar(22222, 1324, "ModelE", "ct", P4), pcar(12, 351, "ModelE", "tr", P5), train(1, "TrainCo", "Express", 200, [P1, P2, P3, P4, P5], Train).
pcar(5, 90, "ModelE", "tr", P1), pcar(42, 12, "ModelE", "ct", P2), pcar(123, 1233, "ModelE", "ct", P3), pcar(22222, 1324, "ModelE", "ct", P4), pcar(12, 351, "ModelE", "tr", P5), train(1, "TrainCo", "Express", 200, [P1, P2, P3, P4, P5], Train), trainAddCar(Train, P3, 1, TRAINPLUS), getTrainPcars(TRAINPLUS, PCARZZ).*/