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
  
continuity(SECT1, SECT2):-
	getPoint2(SECT1, FIRST),
	getPoint1(SECT2, SECOND),
	getStationName(FIRST, FIRSTNAME),
	getStationName(SECOND, SECONDNAME),
	FIRSTNAME = SECONDNAME.
	
consistency(LINE):-
	isTerminal(LINE),
	getSections(LINE, SECTIONLIST),
	pathT(SECTIONLIST).

consistency(LINE):-
	isCircular(LINE),
	getSections(LINE, SECTIONLIST),
	pathC(SECTIONLIST).

%Cuando una linea empieza y termina en estación terminal.
isTerminal(LINE):-
	getSections(LINE, SECTIONS),
	SECTIONS = [],
	true.

isTerminal(LINE):-
	getSections(LINE, SECTIONS),
	SECTIONS \= []
	first(SECTIONS, FIRSTSECTION),
	last(SECTIONS, LASTSECTION),
	getPoint1(FIRSTSECTION, FIRSTSTATION),
	getPoint2(LASTSECTION, LASTSTATION),
	station(_, _, "t", _, FIRSTSTATION),
	station(_, _, "t", _, LASTSTATION).

%Cuando una linea empieza y termina en la misma sección.
isCircular(LINE):-
	getSections(LINE, SECTIONS),
	SECTIONS = [],
	true.

isCircular(LINE):-
	getSections(LINE, SECTIONS),
	SECTIONS \= []
	first(SECTIONS, FIRSTSECTION),
	last(SECTIONS, LASTSECTION),
	getPoint1(FIRSTSECTION, FIRSTSTATION),
	getPoint2(LASTSECTION, LASTSTATION),
	FIRSTSTATION = LASTSTATION.

% Primer elemento de una lista (AUX)
first([H|_], H).

% Ultimo elemento de una lista (AUX)
last([X], X).
last([_|T], X) :-
	last(T, X).

pathT([FIRSTSECTION|SECTIONS]):-
	pathT(SECTIONS, FIRSTSECTION, _).

pathT([], GOALSECTION, GOALSECTION).

pathT([NEXTSECTION|SECTIONS], ACTUALSECTION, GOALSECTION):-
	continuity(ACTUALSECTION, NEXTSECTION),
	pathT(SECTIONS, NEXTSECTION, GOALSECTION).

pathC([FIRSTSECTION|SECTIONS]):-
	append(SECTIONS, [FIRSTSECTION], FULLCIRCLE),
	pathC(FULLCIRCLE, FIRSTSECTION).

pathC([], STARTSECTION, STARTSECTION).

pathC([NEXTSECTION|SECTIONS], ACTUALSECTION, STARTSECTION):-
	continuity(ACTUALSECTION, NEXTSECTION),
	pathC(SECTIONS, NEXTSECTION, STARTSECTION).

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