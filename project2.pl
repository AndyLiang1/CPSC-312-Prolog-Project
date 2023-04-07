


:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

query_api(_, Result) :- 
    http_get("https://jsonplaceholder.typicode.com/todos", Response, []),
    atom_json_dict(Response, Data, []),
    write(Data),
    getAll(Data, Result).

getAll([], []).
getAll([Object | T], [Value | ResultsSoFar]) :-
    Value = Object.userId,
    getAll(T, ResultsSoFar).


getAllLocations([], []).
getAllLocations([OneLocation | T], [OneLocation.location_area.name | ResultsSoFar]) :-
    getAllLocations(T, ResultsSoFar).

