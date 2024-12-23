:- module('TDAsection_208152165_sandovalpradines.pl', [section/5, getPoint1/2, getPoint2/2, getDistance/2, getCost/2]).

:- use_module('TDAstation_208152165_sandovalpradines.pl', [station/5]).

% Dominios
% POINT1 = STATION
% POINT2 = STATION
% DISTANCE = entero, DISTANCE > 0
% COST = entero, COST >= 0
% SECTION = [POINT1, POINT2, DISTANCE, COST]

% Metas principales: section/5
% Metas secundarias: -
section(POINT1, POINT2, DISTANCE, COST, [POINT1, POINT2, DISTANCE, COST]):-
    station(_, _, _, _, POINT1),
    station(_, _, _, _, POINT2),
    integer(DISTANCE),
    DISTANCE > 0,
    integer(COST),
    COST >= 0.

% Metas principales: getPoint1/2
% Metas secundarias: -
getPoint1(SECTION, FIRST_STATION):-
    section(FIRST_STATION, _, _, _, SECTION), !.

% Metas principales: getPoint2/2
% Metas secundarias: -
getPoint2(SECTION, SECOND_STATION):-
    section(_, SECOND_STATION, _, _, SECTION), !.
    
% Metas principales: getDistance/2
% Metas secundarias: -
getDistance(SECTION, DIST):-
    section(_, _, DIST, _, SECTION), !.

% Metas principales: getCost/2
% Metas secundarias: -
getCost(SECTION, COST):-
    section(_, _, _, COST, SECTION), !.

/*
station(2, "Station B", "m", 3, Station2), station(1, "Station A", "r", 5, Station1), station(2, "Station B", "m", 3, Station2), section(Station1, Station2, 25, 40, S1).
station(2, "elias", "t", 3, ST2).
station(3, "miaaaau", "c", 3, ST3).
station(2, "elias", "t", 3, ST2), station(3, "miaaaau", "c", 3, ST3), section(ST2, ST3, 3, 12, S1), getPoint1(S1, P1), getPoint2(S1, P2), getDistance(S1, DIST), getCost(S1, COST).
*/
