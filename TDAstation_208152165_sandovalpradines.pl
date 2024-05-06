:- module(TDAstation, [station/5]).


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