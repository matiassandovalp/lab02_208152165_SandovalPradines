:- module('TDApcar_208152165_sandovalpradines.pl', [pcar/5]).


carType("tr").
carType("ct").

pcar(ID, CAPACITY, MODEL, TYPE, [ID, CAPACITY, MODEL, TYPE]):-
	integer(ID),
	integer(CAPACITY),
	CAPACITY > 0,
	string(MODEL),
	carType(TYPE).


getPcarID(PCAR, ID):-
	pcar(ID, _, _, _, PCAR).

getPcarCapacity(PCAR, CAPACITY):-
	pcar(_, CAPACITY, _, _, PCAR).

getPcarModel(PCAR, MODEL):-
	pcar(_, _, MODEL, _, PCAR).
	
getPcarType(PCAR, TYPE):-
	pcar(_, _, _, TYPE, PCAR).
