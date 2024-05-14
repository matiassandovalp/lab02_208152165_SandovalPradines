:- module('TDAsection_208152165_sandovalpradines.pl', [section/5, getPoint1/2, getPoint2/2, getDistance/2, getCost/2]).

:- use_module('TDAstation_208152165_sandovalpradines.pl', [station/5]).

section(POINT1, POINT2, DISTANCE, COST, [POINT1, POINT2, DISTANCE, COST]):-
	station(_, _, _, _, POINT1),
	station(_, _, _, _, POINT2),
	integer(DISTANCE),
	DISTANCE > 0,
	integer(COST),
	COST >= 0.

getPoint1(SECTION, FIRST_STATION):-
	section(FIRST_STATION, _, _, _, SECTION), !.

getPoint2(SECTION, SECOND_STATION):-
	section(_, SECOND_STATION, _, _, SECTION), !.
	
getDistance(SECTION, DIST):-
    section(_, _, DIST, _, SECTION), !.

getCost(SECTION, COST):-
    section(_, _, _, COST, SECTION), !.

/*
station(2, "elias", "t", 3, st2).
station(3, "miaaaau", "c", 3, st3).
section(st1, st2, 4, 17, Section1).
section(st2, st3, 3, 12, Section2).
getPoint1(section(st1, st2, 4, 17, _), StartPoint).
getPoint2(section(st1, st2, 4, 17, _), EndPoint).
getDistance(section(st1, st2, 4, 17, _), Distance).
getCost(section(st1, st2, 4, 17, _), Cost).
*/