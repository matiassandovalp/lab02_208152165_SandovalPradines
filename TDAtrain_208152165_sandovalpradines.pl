:- module('TDAtrain_208152165_sandovalpradines.pl', []).
:- use_module('TDApcar_208152165_sandovalpradines.pl', [pcar/5]).

%Dominios: 
%ID = Number
%MAKER = String
%RAIL = String
%SPEED = Positive int
%PCARS = pcar List
%TRAIN = train




%Predicado constructor del TDA train
%Metas principales:train/6
%Metas secundarias:
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

%Predicado que verifica pertenencia a el TDA train
%Metas principales:isTrain/1
%Metas secundarias:
isTrain(Train):-
	train(ID, Maker, Rail, Speed, Pcars, Train),
	number(ID),
	string(Maker),
	string(Rail),
	number(Speed),
	is_list(Pcars).

%Predicados selectores del TDA train
%Metas principales:
%Metas secundarias: 
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

%Predicado que chequea que los trenes sean del mismo modelo de forma recursiva mediante otro predicado
%Metas principales:sameModel/1
%Metas secundarias:
sameModel([]):- true, !.

sameModel([CAR|CDR]):-
	pcar(_, _, MODEL, _, CAR),
	same(CDR, MODEL).

%Predicado que chequea que el modelo de forma recursiva
%Metas principales:same/2
%Metas secundarias:
same([], _).

same([CAR|CDR], MODEL):-
	pcar(_, _, MODEL, _, CAR),
	same(CDR, MODEL).

%Extrae elementos de la lista excepto el final
%Metas principales:init/2
%Metas secundarias:
init([], []).

init([_], []).

init([X|Xs], [X|Init]) :-
	init(Xs, Init).

%Predicado que chequea la consistencia de un tren
%Metas principales:Consistency/1
%Metas secundarias:
consistency([]):- true, !.

consistency([FIRST_CAR|CARS]):-
	pcar(_, _, _, "tr", FIRST_CAR),
	init(CARS, MIDDLE_CARS),
	last(CARS, LAST_CAR),
	consistencyMiddle(MIDDLE_CARS),
	pcar(_, _, _, "tr", LAST_CAR).


%Predicado que chequea la consistencia en los carros de un tren excluyendo los extremos.
%Metas principales:consistencyMiddle/1
%Metas secundarias:
consistencyMiddle([]).

consistencyMiddle([CAR|REST]):-
	pcar(_, _, _, "ct", CAR),
	consistencyMiddle(REST).


%Predicado modificador de TDA train que añade carros a un tren
%Metas principales:trainAddCar/4
%Metas secundarias:
trainAddCar(TRAIN, PCAR, POSITION, TRAINOUT):-
	train(ID, MAKER, RAIL, SPEED, PCARS, TRAIN),
	addCar(PCARS, PCAR, POSITION, PCAROUT),
	train(ID, MAKER, RAIL, SPEED, PCAROUT, TRAINOUT).


%recursion que añade un elemento en una posición
%Metas principales:addCar/4
%Metas secundarias:
addCar(PCARLIST, NEWPCAR, 0, [NEWPCAR|PCARLIST]):-!.

addCar([CAR|CDR], NEWPCAR, POS, [CAR|RES]):-
    POS > 0,
    POS1 is POS - 1,
    addCar(CDR, NEWPCAR, POS1, RES).

%Predicado modificador de TDA train que elimina carros a un tren
%Metas principales:trainRemoveCar/3
%Metas secundarias:
trainRemoveCar(TRAIN, POSITION, TRAINOUT):-
    train(ID, MAKER, RAIL, SPEED, PCARS, TRAIN),
    removeCar(PCARS, POSITION, PCAROUT),
    train(ID, MAKER, RAIL, SPEED, PCAROUT, TRAINOUT).

%Recursion que remueve un elemento en una posicion
%Metas principales:removeCar/3
%Metas secundarias:
removeCar([_|CDR], 0, CDR):- !.

removeCar([CAR|CDR], POS, [CAR|RES]):-
    POS > 0,
    POS1 is POS - 1,
    removeCar(CDR, POS1, RES).

% Predicado que calcula la capacidad máxima de pasajeros del tren
%Metas principales:trainCapacity/2
%Metas secundarias:
trainCapacity(Train, Capacity) :-
    getTrainPcars(Train, Pcars),
    calculateCapacity(Pcars, Capacity).

% Recursion que suma la capacidad de todos los carros
%Metas principales:calculateCapacity/2
%Metas secundarias:
calculateCapacity([], 0).
calculateCapacity([PCAR|Rest], Capacity) :-
    pcar(_, Cap, _, _, PCAR),  % Obtener la capacidad del carro
    calculateCapacity(Rest, RestCapacity),
    Capacity is Cap + RestCapacity.

/*
pcar(5, 90, "ModelE", "tr", P1), pcar(42, 12, "ModelE", "ct", P2), pcar(123, 1233, "ModelE", "ct", P3), pcar(22222, 1324, "ModelE", "ct", P4), pcar(12, 351, "ModelE", "tr", P5).
pcar(5, 90, "ModelE", "tr", P1), pcar(42, 12, "ModelE", "ct", P2), pcar(123, 1233, "ModelE", "ct", P3), pcar(22222, 1324, "ModelE", "ct", P4), pcar(12, 351, "ModelE", "tr", P5), train(1, "TrainCo", "Express", 200, [P1, P2, P3, P4, P5], Train).
pcar(5, 90, "ModelE", "tr", P1), pcar(42, 12, "ModelE", "ct", P2), pcar(123, 1233, "ModelE", "ct", P3), pcar(22222, 1324, "ModelE", "ct", P4), pcar(12, 351, "ModelE", "tr", P5), train(1, "TrainCo", "Express", 200, [P1, P2, P3, P4, P5], Train), trainAddCar(Train, P3, 1, TRAINPLUS), getTrainPcars(TRAINPLUS, PCARZZ).*/