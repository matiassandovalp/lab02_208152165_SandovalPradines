:- module('TDAline_208152165_sandovalpradines', [line/5, isLine/1, lineLength/4, lineSectionLength/6, lineAddSection/3]).

:- use_module('TDAsection_208152165_sandovalpradines.pl', [section/5, getPoint1/2, getPoint2/2, getDistance/2, getCost/2]).
:- use_module('TDAstation_208152165_sandovalpradines.pl', [station/5, getStationName/2]).

%Dominios
%ID =Int
%NAME = String
%RAIL_TYPE = String
%SECTIONS = Section List
%LINE = Line
%DISTANCE = Int
%COST = Int
%CAR = List
%CDR = List
%ACUM = Int
%Line = Line
%StartStation = String 
%EndStation = String
%Path =SectionList
%Distance = Int
%Cost = Int

%Predicado constructor del TDA line
%Metas principales:line/5
%Metas secundarias:
line(ID, NAME, RAIL_TYPE, SECTIONS, [ID, NAME, RAIL_TYPE, SECTIONS]):-
	integer(ID),
	string(NAME),
	string(RAIL_TYPE),
	is_list(SECTIONS).

%Predicado de pertenencia del TDA line
%Metas principales:isLine/1
%Metas secundarias:
isLine(LINE):-
	line(ID,NAME,RAIL_TYPE,SECTIONS,LINE),
	integer(ID),
	string(NAME),
	string(RAIL_TYPE),
	is_list(SECTIONS),
	consistency(LINE),
	writeln('La linea ingresada si pertenece al TDA line.').

%Predicado de pertenencia del TDA line que excluye la consistencia para evitar errores en los selectores
%Metas principales:myIsLine/1
%Metas secundarias:
myIsLine(LINE):-
	line(ID,NAME,RAIL_TYPE,SECTIONS,LINE),
	integer(ID),
	string(NAME),
	string(RAIL_TYPE),
	is_list(SECTIONS).

%Predicados selectores del TDA line
%Metas principales:
%Metas secundarias:
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

%Predicado que calcula el largo distancia y costo de una linea
%Metas principales:lineLength/4
%Metas secundarias:
lineLength([], 0, 0, 0).

lineLength(LINE, LENGTH, DISTANCE, COST):-
	getSections(LINE, Sections),
	len(Sections,LENGTH),
	dist(Sections, DISTANCE),
	cost(Sections, COST).

%Predicado que calcula recursivamente el largo de una linea
%Metas principales:len/2
%Metas secundarias:
len([], 0).

len([_| CDR], ACUM):-
	len(CDR, ACUM1),
	ACUM is ACUM1 + 2.

%Predicado que calcula recursivamente la distancia de una linea
%Metas principales:dist/2
%Metas secundarias:
dist([], 0).

dist([CAR | CDR], ACUM):-
	dist(CDR, ACUM1),
	getDistance(CAR, ActualDistance),
	ACUM is ACUM1 + ActualDistance.

%Predicado que calcula recursivamente el costo de una linea
%Metas principales:cost/2
%Metas secundarias:
cost([], 0).

cost([CAR | CDR], ACUM):-
	cost(CDR, ACUM1),
	getCost(CAR, ActualCost),
	ACUM is ACUM1 + ActualCost.

%Predicado que verifica la consistencia en las estaciones de una linea
%Metas principales:consistency/1
%Metas secundarias:
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

%Predicado que calcula el camino, distancia y costo de una linea entre dos estaciones
%Metas principales:lineSectionLength/6
%Metas secundarias:
lineSectionLength(Line, StartStation, EndStation, Path, Distance, Cost) :-
    Line = [_, _, _, Sections],
    findStationPath(Sections, StartStation, EndStation, [], Path, Distance, Cost).

%Predicado que encuentra el camino entre dos estaciones y calcula todo lo especificado anteriormente
%Metas principales:findStationPath/7
%Metas secundarias:
findStationPath(_, Start, End, _, [], 0, 0) :-
    Start == End.

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

findStationPath([_ | RestSections], Start, End, Visited, Path, Distance, Cost) :-
    findStationPath(RestSections, Start, End, Visited, Path, Distance, Cost).

%Definición auxiliar de path para evitar errores
%Metas principales:path/1
%Metas secundarias:-
path(_).

%Predicado que verifica si una linea no es circular
%Metas principales:isTerminal/1
%Metas secundarias:
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

%Linea que verifica si una linea es circular
%Metas principales:isCircular(1)
%Metas secundarias:
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

%Predicado que extrae el primer elemento de una lista
%Metas principales:first/2
%Metas secundarias:
first([H|_], H).

%Predicado que extrae el ultimo elemento de una lista
%Metas principales:last/2
%Metas secundarias:
last([X], X).
last([_|T], X) :-
	last(T, X).

%Predicado que añade secciones a una linea
%Metas principales:lineAddSection/3
%Metas secundarias:
lineAddSection(LINE, SECTION, LINEOUT):-
	getID(LINE, ID),
	getName(LINE, NAME),
	getRailType(LINE, RAIL_TYPE),
	getSections(LINE, SECTIONS),
	\+ member(SECTION, SECTIONS),
	append(SECTIONS, [SECTION], NEWSECTIONS),
	line(ID, NAME, RAIL_TYPE, NEWSECTIONS, LINEOUT).

%Predicado que verifica si un elemento pertenece a una lista
%Metas principales:member/2
%Metas secundarias:
member(ELEM, [ELEM|_]):-!.

member(ELEM, [_|CDR]):-
	member(ELEM, CDR).

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
