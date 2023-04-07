


:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

query_api(URL, Result) :- 
    http_get(URL, Response, []),
    atom_json_dict(Response, Data, []),
    Result = Data.








