:- module('ScriptPruebas_208152165_sandovalpradines.pl', []).

:- use_module('TDAstation_208152165_sandovalpradines.pl', [station/5, getStationName/2]).
:- use_module('TDAsection_208152165_sandovalpradines.pl', [section/5, getPoint1/2, getPoint2/2, getDistance/2, getCost/2]).
:- use_module('TDAline_208152165_sandovalpradines', [line/5, isLine/1, lineLength/4, lineSectionLength/6, lineAddSection/3]).
:- use_module('TDApcar_208152165_sandovalpradines.pl', [pcar/5]).
:- use_module('TDAtrain_208152165_sandovalpradines.pl', [train/6, isTrain/1, trainAddCar/4, trainRemoveCar/3, trainCapacity/2]).
:- use_module('TDAdriver_208152165_sandovalpradines', [driver/4]).
:- use_module('TDAsubway_208152165_sandovalpradines', [subway/3]).

