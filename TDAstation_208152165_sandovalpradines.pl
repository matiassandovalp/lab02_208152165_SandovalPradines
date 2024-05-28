:- module('TDAstation_208152165_sandovalpradines.pl', [station/5, getStationName/2]).


% Dominios
% ID = entero
% NAME = string
% TYPE = {"r", "m", "c", "t"}
% STOPTIME = entero
% STATION = [ID, NAME, TYPE, STOPTIME]

%Hechos que crean restricciones validas para los stationType
%Metas principales: StationType/1
%Metas secundarias: station/1
stationType("r").
stationType("m").
stationType("c").
stationType("t").

%Función constructora para el TDA station
%Metas principales: station/5
%Metas secundarias: getStationID/2, getStationName/2, getStationType/2, getStationStopTime/2.
station(ID, NAME, TYPE, STOPTIME, [ID, NAME, TYPE, STOPTIME]):-
	integer(ID),
	string(NAME),
	stationType(TYPE),
	integer(STOPTIME),
	STOPTIME > 0.

%Funciones selectoras para el TDA station
%Metas principales: getStationID/2, getStationName/2, getStationType/2, getStationStopTime/2.
%Metas secundarias: -
getStationID(STATION, ID):-
	station(ID, _, _, _, STATION), !.

getStationName(STATION, NAME):-
	station(_, NAME, _, _, STATION), !.

getStationType(STATION, TYPE):-
	station(_, _, TYPE, _, STATION), !.

getStationStopTime(STATION, STOPTIME):-
	station(_, _, _, STOPTIME, STATION), !.


/*
Crear estaciones:
station(1, "Station A", "r", 5, Station1).
station(2, "Station B", "m", 3, Station2).
station(3, "Station C", "c", 7, Station3).
station(4, "Station D", "t", 10, Station4).

---------------NO ENTRA EN SCRIPT DE PRUEBAS-------------
station(4, "Station D", "m", 10, Station4), getStationID(Station4, ID), getStationName(Station4, NAME), getStationType(Station4, TYPE), getStationStopTime(Station4, SST).
*/