:- module('TDAsubway_208152165_sandovalpradines', [subway/3]).

% Dominios
%ID = Number
%Name = String

%Predicado constructor para el TDA subway
%Metas principales: subway/3
%Metas secundarias: -
subway(ID, Name, [ID, Name, [], [], []]):-
	number(ID),
	string(Name).