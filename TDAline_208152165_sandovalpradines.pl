:- module('TDAline_208152165_sandovalpradines.pl', []).

:- use_module('TDAsection_208152165_sandovalpradines.pl', [section/5, getPoint1/2, getPoint2/2, getDistance/2, getCost/2]).
:- use_module('TDAstation_208152165_sandovalpradines.pl', [station/5, getStationName/2]).


line(ID, NAME, RAIL_TYPE, SECTIONS, [ID, NAME, RAIL_TYPE, SECTIONS]):-
	integer(ID),
	string(NAME),
	string(RAIL_TYPE),
	is_list(SECTIONS).
%Comprobacion de camino y caso para lista vacia

isLine(LINE):-
	is_list(LINE),
	getID(LINE, ID),
	getName(LINE, NAME),
	getRailType(LINE, RAIL_TYPE),
	getSections(LINE, SECTIONS),
	integer(ID),
	string(NAME),
	string(RAIL_TYPE),
	is_list(SECTIONS),
	consistency(LINE),
	writeln('La linea ingresada si pertenece al TDA line.').

getID(LINE, ID):-
	line(ID, _, _, _, LINE), !.

getName(LINE, NAME):-
	line(_, NAME, _, _, LINE), !.

getRailType(LINE, RAIL_TYPE):-
	line(_, _, RAIL_TYPE, _, LINE), !.

getSections(LINE, SECTIONLIST):-
	line(_, _, _, SECTIONLIST, LINE), !.
%selectores con pertenencia!!!!

%LENGTH, DISTANCE Y COST son variables en todo momento.
lineLength([], 0, 0, 0).

lineLength(LINE, LENGTH, DISTANCE, COST):-
	getSections(LINE, Sections),
	len(Sections,LENGTH),
	dist(Sections, DISTANCE),
	cost(Sections, COST).


len([], 0).

len([_| CDR], ACUM):-
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





lineSectionLength(Line, StartStation, EndStation, Path, Distance, Cost) :-
	Line = [_, _, _, Sections],
	findStationPath(Sections, StartStation, EndStation, [], Path, Distance, Cost).

findStationPath([], Start, End, Visited, Path, Distance, Cost) :-
	Start == End,
	Path = [],
	Distance = 0,
	Cost = 0.

findStationPath([Section | RestSections], Start, End, Visited, Path, Distance, Cost) :-
	getPoint1(Section, Start),
	getPoint2(Section, Next),
	\+ member(Next, Visited),
	getDistance(Section, D1),
	getCost(Section, C1),
	findStationPath(RestSections, Next, End, [Start | Visited], SubPath, D2, C2),
	Distance is D1 + D2,
	Cost is C1 + C2,
	Path = [Section | SubPath].

findStationPath([_ | RestSections], Start, End, Visited, Path, Distance, Cost) :-
	findStationPath(RestSections, Start, End, Visited, Path, Distance, Cost).




continuity(SECT1, SECT2):-
	getPoint2(SECT1, FIRST),
	getPoint1(SECT2, SECOND),
	getStationName(FIRST, FIRSTNAME),
	getStationName(SECOND, SECONDNAME),
	FIRSTNAME = SECONDNAME.
	
consistency(LINE):-
	isTerminal(LINE),
	getSections(LINE, SECTIONLIST),
	pathT(SECTIONLIST),
	writeln('La linea es consistente con dos estaciones terminales.'), !.

consistency(LINE):-
	isCircular(LINE),
	getSections(LINE, SECTIONLIST),
	pathC(SECTIONLIST),
	writeln('La linea es consistente circularmente.'), !.

%Cuando una linea empieza y termina en estación terminal.
isTerminal(LINE):-
	getSections(LINE, SECTIONS),
	SECTIONS = [],
	true.

isTerminal(LINE):-
	getSections(LINE, SECTIONS),
	SECTIONS \= [],
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
	SECTIONS \= [],
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
	pathC(FULLCIRCLE, FIRSTSECTION, FIRSTSECTION).

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
Crear estaciones y secciones
station(2, "Station B", "m", 3, Station2), station(1, "Station A", "r", 5, Station1), station(2, "Station B", "m", 3, Station2), section(Station1, Station2, 25, 40, S1), section(Station2, Station1, 75, 60, S2).

Crear linea y length
station(2, "Station B", "m", 3, Station2), station(1, "Station A", "r", 5, Station1), station(2, "Station B", "m", 3, Station2), section(Station1, Station2, 25, 40, S1), section(Station2, Station1, 75, 60, S2), line(14, "HII", "Monopatin", [S1, S2], L1), lineLength(L1, LEN, DIS, COS).

---------NO ENTRAN EN SCRIPT DE PRUEBAS-----------------
Chequear consistencia de linea terminal:
station(2, "Station B", "c", 3, Station2), station(1, "Station A", "t", 5, Station1), station(3, "Station C", "t", 3, Station3), section(Station1, Station2, 25, 40, S1), section(Station2, Station3, 75, 60, S2), line(14, "HII", "Monopatin", [S1, S2], L1), consistency(L1).
chequear consistencia de linea circular:
station(2, "Station B", "m", 3, Station2), station(1, "Station A", "r", 5, Station1), station(2, "Station B", "m", 3, Station2), section(Station1, Station2, 25, 40, S1), section(Station2, Station1, 75, 60, S2), line(14, "HII", "Monopatin", [S1, S2], L1), consistency(L1).
---------NO ENTRAN EN SCRIPT DE PRUEBAS-----------------

Length entre dos estaciones
station(1, "Station A", "r", 5, Station1), station(2, "Station B", "m", 3, Station2), station(3, "Station C", "t", 4, Station3), station(4, "Station D", "m", 6, Station4), section(Station1, Station2, 10, 15, S1), section(Station2, Station3, 20, 25, S2), section(Station3, Station4, 30, 35, S3), section(Station4, Station1, 40, 45, S4), line(16, "CircularLine", "Bus", [S1, S2, S3, S4], L3), lineSectionLength(L3, "Station A", "Station C", PATH, DISTANCE, COST).

station(1, "Station A", "r", 5, Station1), station(2, "Station B", "m", 3, Station2), station(3, "Station C", "t", 4, Station3), station(4, "Station D", "m", 6, Station4), section(Station1, Station2, 10, 15, S1), section(Station2, Station3, 20, 25, S2), section(Station3, Station4, 30, 35, S3), section(Station4, Station1, 40, 45, S4), line(16, "CircularLine", "Bus", [S1, S2, S3, S4], L3), lineSectionLength(L3, "Station B", "Station C", PATH, DISTANCE, COST).

Añadir secciones a una line
-No duplicadas
station(2, "Station B", "m", 3, Station2), station(1, "Station A", "r", 5, Station1), station(2, "Station B", "m", 3, Station2), section(Station1, Station2, 25, 40, S1), section(Station2, Station1, 75, 60, S2), line(14, "HII", "Monopatin", [], L1), lineAddSection(L1, S1, L2), lineAddSection(L2, S2, L3).
-Duplicadas
station(2, "Station B", "m", 3, Station2), station(1, "Station A", "r", 5, Station1), station(2, "Station B", "m", 3, Station2), section(Station1, Station2, 25, 40, S1), section(Station2, Station1, 75, 60, S2), line(14, "HII", "Monopatin", [], L1), lineAddSection(L1, S1, L2), lineAddSection(L2, S1, L3).

Pertenencia de line
station(2, "Station B", "m", 3, Station2), station(1, "Station A", "r", 5, Station1), station(2, "Station B", "m", 3, Station2), section(Station1, Station2, 25, 40, S1), section(Station2, Station1, 75, 60, S2), line(14, "HII", "Monopatin", [S1, S2], L1), isLine(L1).


station(1, "Station A", "r", 5, Station1), station(2, "Station B", "m", 3, Station2), station(3, "Station C", "t", 4, Station3), section(Station1, Station2, 25, 40, S1), section(Station2, Station3, 30, 50, S2), section(Station3, Station1, 35, 60, S3), line(14, "HII", "Monopatin", [S1, S2, S3], L1),lineSectionLength(L1, "Station A", "Station C", PATH, DISTANCE, COST).
*/