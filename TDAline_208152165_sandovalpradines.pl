:- module('TDAline_208152165_sandovalpradines.pl', []).

:- use_module('TDAsection_208152165_sandovalpradines.pl', [section/5, getPoint1/2, getPoint2/2, getDistance/2, getCost/2]).

line(ID, NAME, RAIL_TYPE, SECTIONS, [ID, NAME, RAIL_TYPE, SECTIONS]):-
	integer(ID),
	string(NAME),
	string(RAIL_TYPE),
	is_list(SECTIONS).


isLine(LINE):-
	is_list(LINE),
	getID(LINE, ID),
	getName(LINE, NAME),
	getRailType(LINE, RAIL_TYPE),
	getSections(LINE, SECTIONS),
	integer(ID),
	string(NAME),
	string(RAIL_TYPE),
	is_list(SECTIONS).

getID(LINE, ID):-
	line(ID, _, _, _, LINE), !.

getName(LINE, NAME):-
	line(_, NAME, _, _, LINE), !.

getRailType(LINE, RAIL_TYPE):-
	line(_, _, RAIL_TYPE, _, LINE), !.

getSections(LINE, SECTIONLIST):-
	line(_, _, _, SECTIONLIST, LINE), !.


%LENGTH, DISTANCE Y COST son variables en todo momento.

lineLength([], 0, 0, 0).

lineLength(LINE, LENGTH, DISTANCE, COST):-
	getSections(LINE, Sections),
	len(Sections,LENGTH),
	dist(Sections, DISTANCE),
	cost(Sections, COST).


len([], 0).

len([_ | CDR], ACUM):-
	len(CDR, ACUM1),
	ACUM is ACUM1 + 2.


dist([], 0).

dist([CAR | CDR], ACUM):-
	dist(CDR, ACUM1),
	getDistance(CAR, ActualDistance),
	ACUM is ACUM1 + ActualDistance.


cost([], 0).

cost([CAR | CDR], ACUM):-
	cost(CDR, ACUM1),
	getCost(CAR, ActualCost),
	ACUM is ACUM1 + ActualCost.


%Llamar a Length desde EST1 Hasta EST2 (Crear predicado que excluya resto de ambas partes).
/*lineSectionLength: TRAYECTO ENTRE 2 ESTACIONES, DISTANCIA Y COSTO 
lineSectionLength(LINE, StationName1, StationName2, PATH, DISTANCE, COST):-
*/


lineAddSection(LINE, SECTION, LINEOUT):-
	getID(LINE, ID),
	getName(LINE, NAME),
	getRailType(LINE, RAIL_TYPE),
	getSections(LINE, SECTIONS),
	\+ member(SECTION, SECTIONS),
	append(SECTIONS, [SECTION], NEWSECTIONS),
	line(ID, NAME, RAIL_TYPE, NEWSECTIONS, LINEOUT).


member(ELEM, [ELEM|_]):-!.

member(ELEM, [_|CDR]):-
	member(ELEM, CDR).


/*
station(2, "Station B", "m", 3, Station2), station(1, "Station A", "r", 5, Station1), station(2, "Station B", "m", 3, Station2), section(Station1, Station2, 25, 40, S1), section(Station2, Station1, 75, 60, S2).
Crear linea y length
station(2, "Station B", "m", 3, Station2), station(1, "Station A", "r", 5, Station1), station(2, "Station B", "m", 3, Station2), section(Station1, Station2, 25, 40, S1), section(Station2, Station1, 75, 60, S2), line(14, "HII", "Monopatin", [S1, S2], L1), lineLength(L1, LEN, DIS, COS).
*/