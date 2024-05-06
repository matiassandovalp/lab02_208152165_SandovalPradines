:- module(TDAsection, [section/5, getPoint1/2, getPoint2/2, getDistance/2, getCost/2]).

:- use_module(TDAstation, [station/5]).

section(POINT1, POINT2, DISTANCE, COST, SECTION):-
	station(_, _, _, _, POINT1),
	station(_, _, _, _, POINT2),
	integer(DISTANCE),
	DISTANCE > 0,
	integer(COST),
	COST >= 0,
	SECTION = [POINT1, POINT2, DISTANCE, COST].

getPoint1(SECTION, P1):-
	nth0(0, SECTION, P1).

getPoint2(SECTION, P2):-
	nth0(1, SECTION, P2).
	
getDistance(SECTION, DIST):-
    nth0(2, SECTION, DIST).

getCost(SECTION, COST):-
    nth0(3, SECTION, COST).