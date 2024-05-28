:- module('TDAdriver_208152165_sandovalpradines', [driver/4]).

%Dominio:
%ID = Integer
%NAME =String
%TRAIN_MAKER = String

%Predicado constructor del TDA driver
%Metas principales: driver/4
%Metas secundarias: -
driver(ID, NAME, TRAIN_MAKER, [ID, NAME, TRAIN_MAKER]):-
	integer(ID),
	string(NAME),
	string(TRAIN_MAKER).