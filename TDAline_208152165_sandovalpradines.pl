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

len([CAR | CDR], ACUM):-
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

/*lineSectionLength: TRAYECTO ENTRE 2 ESTACIONES, DISTANCIA Y COSTO 
lineSectionLength(LINE, StationName1, StationName2, PATH, DISTANCE, COST):-
*/

lineAddSection(LINE, SECTION, LINEOUT):-
	section(ID, NAME, RAIL, SECTIONS, LINE),
	not(member(SECTION, SECTIONS)),
	append(SECTION, SECTIONS, NEWSECTIONS),
	section(ID, NAME, RAIL, NEWSECTIONS, LINEOUT).
%Verificar id con member y selectores?, llamar a otra funcion que verifique y retorne true si no hay equidades de id?


member(ELEM, [ELEM|_]):-!.

member(ELEM, [CAR|CDR]):-
	member(ELEM, CDR).

idCheck([], Section):-
	true.

idCheck([CAR|CDR], SECTION):-
	true.

 section(st1, st2, 4, 17, section1),
 section(st2, st3, 3, 12, section2).
 
 