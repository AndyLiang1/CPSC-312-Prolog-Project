


:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

query_api(_, Result) :- 
    http_get("https://pokeapi.co/api/v2/pokemon/dialga/encounters", Response, []),
    atom_json_dict(Response, Data, []),
    Result = Data.


getAllLocations([], []).
getAllLocations([{"location_area": {"name": V, _}, _} | T], [V | ResultsSoFar]) :- 
    getAllLocations(T, ResultsSoFar).



extractOneProp([], []).
extractOneProp([{"a": V,_} | T], [V | ResultSoFar]) :- 
    extractOneProp(T, ResultSoFar).