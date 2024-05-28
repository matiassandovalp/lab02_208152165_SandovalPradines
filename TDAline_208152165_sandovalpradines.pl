:- module('TDAline_208152165_sandovalpradines', []).

:- use_module('TDAsection_208152165_sandovalpradines.pl', [section/5, getPoint1/2, getPoint2/2, getDistance/2, getCost/2]).
:- use_module('TDAstation_208152165_sandovalpradines.pl', [station/5, getStationName/2]).

line(ID, NAME, RAIL_TYPE, SECTIONS, [ID, NAME, RAIL_TYPE, SECTIONS]):-
	integer(ID),
	string(NAME),
	string(RAIL_TYPE),
	is_list(SECTIONS).

isLine(LINE):-
	line(ID,NAME,RAIL_TYPE,SECTIONS,LINE),
	integer(ID),
	string(NAME),
	string(RAIL_TYPE),
	is_list(SECTIONS),
	consistency(LINE),
	writeln('La linea ingresada si pertenece al TDA line.').

myIsLine(LINE):-
	line(ID,NAME,RAIL_TYPE,SECTIONS,LINE),
	integer(ID),
	string(NAME),
	string(RAIL_TYPE),
	is_list(SECTIONS).

getID(LINE, ID):-
	myIsLine(LINE),
	line(ID, _, _, _, LINE), !.

getName(LINE, NAME):-
	myIsLine(LINE),
	line(_, NAME, _, _, LINE), !.

getRailType(LINE, RAIL_TYPE):-
	myIsLine(LINE),
	line(_, _, RAIL_TYPE, _, LINE), !.

getSections(LINE, SECTIONLIST):-
	myIsLine(LINE),
	line(_, _, _, SECTIONLIST, LINE), !.

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

consistency(LINE):-
	isTerminal(LINE),
	getSections(LINE, SECTIONLIST),
	path(SECTIONLIST),
	writeln('La linea es consistente con dos estaciones terminales.'), !.

consistency(LINE):-
	isCircular(LINE),
	getSections(LINE, SECTIONLIST),
	path(SECTIONLIST),
	writeln('La linea es consistente circularmente.'), !.

lineSectionLength(Line, StartStation, EndStation, Path, Distance, Cost) :-
    Line = [_, _, _, Sections],
    findStationPath(Sections, StartStation, EndStation, [], Path, Distance, Cost).

% Base case: if Start equals End, path is empty, distance and cost are zero.
findStationPath(_, Start, End, _, [], 0, 0) :-
    Start == End.

% Recursive case: find the path from Start to End through sections.
findStationPath([Section | RestSections], Start, End, Visited, [Section | Path], Distance, Cost) :-
    getPoint1(Section, StartStat),
    getStationName(StartStat, Start),
    getPoint2(Section, NextStat),
    getStationName(NextStat, Next),
    \+ member(Next, Visited),
    getDistance(Section, D1),
    getCost(Section, C1),
    findStationPath(RestSections, Next, End, [Next | Visited], Path, D2, C2),
    Distance is D1 + D2,
    Cost is C1 + C2.

% General case: skip the current section and continue with the rest.
findStationPath([_ | RestSections], Start, End, Visited, Path, Distance, Cost) :-
    findStationPath(RestSections, Start, End, Visited, Path, Distance, Cost).

% Dummy definition for path/1 to avoid warnings.
path(_).

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

first([H|_], H).

last([X], X).
last([_|T], X) :-
	last(T, X).

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

% Example test cases
/*
Crear estaciones y secciones
station(2, "Station B", "m", 3, Station2), station(1, "Station A", "r", 5, Station1), station(2, "Station B", "m", 3, Station2), section(Station1, Station2, 25, 40, S1), section(Station2, Station1, 75, 60, S2).

Crear linea y length
station(2, "Station B", "m", 3, Station2), station(1, "Station A", "r", 5, Station1), station(2, "Station B", "m", 3, Station2), section(Station1, Station2, 25, 40, S1), section(Station2, Station1, 75, 60, S2), line(14, "HII", "Monopatin", [S1, S2], L1), lineLength(L1, LEN, DIS, COS).

Chequear consistencia de linea terminal:
station(2, "Station B", "c", 3, Station2), station(1, "Station A", "t", 5, Station1), station(3, "Station C", "t", 3, Station3), section(Station1, Station2, 25, 40, S1), section(Station2, Station3, 75, 60, S2), line(14, "HII", "Monopatin", [S1, S2], L1), consistency(L1).
chequear consistencia de linea circular:
station(2, "Station B", "m", 3, Station2), station(1, "Station A", "r", 5, Station1), station(2, "Station B", "m", 3, Station2), section(Station1, Station2, 25, 40, S1), section(Station2, Station1, 75, 60, S2), line(14, "HII", "Monopatin", [S1, S2], L1), consistency(L1).

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
