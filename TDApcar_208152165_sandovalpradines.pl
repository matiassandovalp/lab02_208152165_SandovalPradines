
carType("tr").
carType("ct").

pcar(ID, CAPACITY, MODEL, TYPE, [ID, CAPACITY, MODEL, TYPE]):-
	integer(ID),
	integer(CAPACITY),
	CAPACITY > 0,
	string(MODEL),
	carType(TYPE).