:- module(TDAstation, [station/5]).


stationType("r").
stationType("m").
stationType("c").
stationType("t").

station(ID, NAME, TYPE, STOPTIME, STATION):-
	integer(ID),
	string(NAME),
	stationType(TYPE),
	integer(STOPTIME),
	STOPTIME > 0,
	STATION = [ID, NAME, TYPE, STOPTIME].
	
%station(1, "jean", "r", 3, st1).