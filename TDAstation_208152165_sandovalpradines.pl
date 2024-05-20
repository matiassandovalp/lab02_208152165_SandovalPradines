:- module('TDAstation_208152165_sandovalpradines.pl', [station/5]).


stationType("r").
stationType("m").
stationType("c").
stationType("t").

station(ID, NAME, TYPE, STOPTIME, [ID, NAME, TYPE, STOPTIME]):-
	integer(ID),
	string(NAME),
	stationType(TYPE),
	integer(STOPTIME),
	STOPTIME > 0.


getStationID(STATION, ID):-
	station(ID, _, _, _, STATION), !.

getStationName(STATION, NAME):-
	station(_, NAME, _, _, STATION), !.

getStationType(STATION, TYPE):-
	station(_, _, TYPE, _, STATION), !.

getStationStopTime(STATION, STOPTIME):-
	station(_, _, _, STOPTIME, STATION), !.


/*
station(1, "Station A", "r", 5, Station1).
station(2, "Station B", "m", 3, Station2).
station(3, "Station C", "c", 7, Station3).
station(4, "Station D", "t", 10, Station4).
station(4, "Station D", "m", 10, Station4), getStationID(Station4, ID), getStationName(Station4, NAME), getStationType(Station4, TYPE), getStationStopTime(Station4, SST).
*/