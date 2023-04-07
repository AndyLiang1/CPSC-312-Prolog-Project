


:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

query_api(_, Result) :- 
    http_get("https://pokeapi.co/api/v2/pokemon/dialga/encounters", Response, []),
    atom_json_dict(Response, Data, []),
    write(Data),
    getAllLocations(Data, Result).

getAll([], []).
getAll([Object | T], [Object.userId | ResultsSoFar]) :-
    getAll(T, ResultsSoFar).


getAllLocations([], []).
getAllLocations([OneLocation | T], [OneLocation.location_area.name | ResultsSoFar]) :-
    getAllLocations(T, ResultsSoFar).

