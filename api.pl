:- module(api, [getLocationFromKBOrQuery/2, query_all_pokemon_from_type/2]).
:- dynamic location/2, type/2.

query_locations_of_pokemon(Pokemon) :-
    atomics_to_string(["https://pokeapi.co/api/v2/pokemon/", Pokemon, "/encounters"], Request),
    http_get(Request, Response, []),
    atom_json_dict(Response, Data, []),
    getAllLocationsForOnePokemon(Data, AllLocations),
    insertLocationIntoKnowledgeBase(Pokemon, AllLocations).

query_types_of_pokemon(Pokemon) :-
    atomics_to_string(["https://pokeapi.co/api/v2/pokemon/", Pokemon], Request),
    http_get(Request, Response, []),
    atom_json_dict(Response, Data, []),
    getAllTypesForOnePokemon(Data.types, AllTypes),
    insertTypeIntoKnowledgeBase(Pokemon, AllTypes).

query_all_pokemon_from_type(Type, ListOfPokemons) :- 
    atomics_to_string(["https://pokeapi.co/api/v2/type/", Type], Request),
    http_get(Request, Response, []),
    atom_json_dict(Response, Data, []),
    getAllPokemonFromType(Data.pokemon, ListOfPokemons).



getAllPokemonFromType([], []).
getAllPokemonFromType([Pokemon | RestOfList], [Pokemon.pokemon.name | ResultsSoFar]) :- 
    getAllPokemonFromType(RestOfList, ResultsSoFar).

getAllTypesForOnePokemon([], []).
getAllTypesForOnePokemon([Type | T], [Type.type.name | ResultsSoFar]) :-
    getAllTypesForOnePokemon(T, ResultsSoFar).

insertTypeIntoKnowledgeBase(_, []).
insertTypeIntoKnowledgeBase(Pokemon, [TypeAsString | T]) :- 
    type(Pokemon, TypeAsString), !,
    insertLocationIntoKnowledgeBase(Pokemon, T).
insertTypeIntoKnowledgeBase(Pokemon, [TypeAsString | T]) :- 
    assertz(type(Pokemon, TypeAsString)),
    insertTypeIntoKnowledgeBase(Pokemon, T).

getAllLocationsForOnePokemon([], []).
getAllLocationsForOnePokemon([OneLocation | T], [OneLocation.location_area.name | ResultsSoFar]) :-
    getAllLocationsForOnePokemon(T, ResultsSoFar).

insertLocationIntoKnowledgeBase(_, []).
insertLocationIntoKnowledgeBase(Pokemon, [LocationAsString | T]) :- 
    location(Pokemon, LocationAsString), !,
    insertLocationIntoKnowledgeBase(Pokemon, T).
insertLocationIntoKnowledgeBase(Pokemon, [LocationAsString | T]) :- 
    assertz(location(Pokemon, LocationAsString)),
    insertLocationIntoKnowledgeBase(Pokemon, T).

getLocationFromKBOrQuery(Pokemon, Location) :- location(Pokemon, Location).
getLocationFromKBOrQuery(Pokemon, Location) :- not(location(Pokemon, Location)), add_pokemon(Pokemon), location(Pokemon, Location).

not(P) :- P, !, fail ; true.