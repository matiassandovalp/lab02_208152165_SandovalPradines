:- module('TDAsubway_208152165_sandovalpradines', []).

% Dominios
%ID = Number
%Name = String

%Predicado constructor para el TDA subway
%Metas principales: subway/3
%Metas secundarias: -
subway(ID, Name, [ID, Name, [], [], []]):-
	number(ID),
	string(Name).