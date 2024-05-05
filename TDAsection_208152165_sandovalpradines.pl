:- use_module(TDAstation, [station/5]).

section(POINT1, POINT2, DISTANCE, COST, SECTION):-
	station(_, _, _, _, POINT1),
	station(_, _, _, _, POINT2),
	integer(DISTANCE),
	DISTANCE > 0,
	integer(COST),
	COST >= 0,
	SECTION = [POINT1, POINT2, DISTANCE, COST].


