


:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- dynamic location/2.


query_api(_, Result) :- 
    http_get("https://pokeapi.co/api/v2/pokemon/dialga/encounters", Response, []),
    atom_json_dict(Response, Data, []),
    getAllLocations(Data, AllLocations),
    insertLocationIntoKnowledgeBase(AllLocations),
    Result = AllLocations.

getAllLocations([], []).
getAllLocations([OneLocation | T], [OneLocation.location_area.name | ResultsSoFar]) :-
    getAllLocations(T, ResultsSoFar).

insertLocationIntoKnowledgeBase([]).
insertLocationIntoKnowledgeBase([LocationAsString | T]) :- 
    assertz((location(dialga, LocationAsString))),
    insertLocationIntoKnowledgeBase(T).


