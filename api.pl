:- module(api, [getLocationFromKBOrQuery/2, getTypeFromKBOrQuery/2, queryAllPokemonFromType/2, queryAllPokemonFromLocation/2, addEncounter/2, getEncounter/1, clearKB/1]).
:- dynamic location/2, type/2, encounter/1.


/* ==== API CALLS === */ 
% Given a pokemon, retreive all locations where it can be found 
queryLocationsOfPokemon(Pokemon) :-
    atomics_to_string(["https://pokeapi.co/api/v2/pokemon/", Pokemon, "/encounters"], Request),
    http_get(Request, Response, []),
    atom_json_dict(Response, Data, []),
    parseLocationsForOnePokemon(Data, AllLocations),
    insertLocationIntoKnowledgeBase(Pokemon, AllLocations),
    insertEncounterIntoKnowledgeBase(Pokemon).

% Given a pokemon, retreive its types
queryTypesOfPokemon(Pokemon) :-
    atomics_to_string(["https://pokeapi.co/api/v2/pokemon/", Pokemon], Request),
    http_get(Request, Response, []),
    atom_json_dict(Response, Data, []),
    parseTypesForOnePokemon(Data.types, AllTypes),
    insertTypeIntoKnowledgeBase(Pokemon, AllTypes),
    insertEncounterIntoKnowledgeBase(Pokemon).

% Given a location, find all pokemons in that location
queryAllPokemonFromLocation(Location, ListOfPokemons) :- 
    atomics_to_string(["https://pokeapi.co/api/v2/location-area/", Location], Request),
    http_get(Request, Response, []),
    atom_json_dict(Response, Data, []),
    parseAllPokemonFromLocation(Data.pokemon_encounters, ListOfPokemons).

% Given a type, find all pokemons of that type
queryAllPokemonFromType(Type, ListOfPokemons) :- 
    atomics_to_string(["https://pokeapi.co/api/v2/type/", Type], Request),
    http_get(Request, Response, []),
    atom_json_dict(Response, Data, []),
    parseAllPokemonFromType(Data.pokemon, ListOfPokemons).

% Validates that Pokemon exists
queryPokemonForEncounter(Pokemon) :-
    atomics_to_string(["https://pokeapi.co/api/v2/pokemon/", Pokemon], Request),
    http_get(Request, _, []).


/* === Check if KB has the info first before using http get === */ 

getLocationFromKBOrQuery(Pokemon, Location) :- location(Pokemon, Location).
getLocationFromKBOrQuery(Pokemon, Location) :- not(location(Pokemon, Location)), addPokemonToKB(Pokemon), location(Pokemon, Location).
getTypeFromKBOrQuery(Pokemon, Type) :- type(Pokemon, Type).
getTypeFromKBOrQuery(Pokemon, Type) :- not(type(Pokemon, Type)), addPokemonToKB(Pokemon), type(Pokemon, Type).
getEncounter(E) :- encounter(E).

addPokemonToKB(Pokemon) :-
    catch(
        (
            queryLocationsOfPokemon(Pokemon),
            queryTypesOfPokemon(Pokemon)
        ), Error, 
        (
            write('An error occurred: '), write(Error),
            fail
        )
    ).
    

addEncounter(L0, Ind) :-
    string_lower(L0, L1),
    encounter(L1),
    string_to_atom("success", Ind), !.
addEncounter(L0, Ind) :-
    string_lower(L0, L1),
    not(encounter(L1)),
    catch(
        (   
            queryPokemonForEncounter(L1),
            insertEncounterIntoKnowledgeBase(L1),
            string_to_atom("success", Ind)
        ),
        Error,
        (   
            write('An error occurred: '), write(Error),
            fail
        )
    ).


not(P) :- P, !, fail ; true.



/* === PARSING API RESPONSES === */

parseLocationsForOnePokemon([], []).
parseLocationsForOnePokemon([OneLocation | T], [OneLocation.location_area.name | ResultsSoFar]) :-
parseLocationsForOnePokemon(T, ResultsSoFar).

parseTypesForOnePokemon([], []).
parseTypesForOnePokemon([Type | T], [Type.type.name | ResultsSoFar]) :-
    parseTypesForOnePokemon(T, ResultsSoFar).

parseAllPokemonFromType([], []).
parseAllPokemonFromType([Pokemon | RestOfList], [Pokemon.pokemon.name | ResultsSoFar]) :- 
    parseAllPokemonFromType(RestOfList, ResultsSoFar).

parseAllPokemonFromLocation([], []). 
parseAllPokemonFromLocation([Pokemon | RestOfList], [Pokemon.pokemon.name | ResultsSoFar]) :- 
    parseAllPokemonFromLocation(RestOfList, ResultsSoFar).


/* Inserts into KB for performance improvement (removes redundant http gets) */
insertTypeIntoKnowledgeBase(_, []).
insertTypeIntoKnowledgeBase(Pokemon, [TypeAsString | T]) :- 
    type(Pokemon, TypeAsString), !,
    insertLocationIntoKnowledgeBase(Pokemon, T).
insertTypeIntoKnowledgeBase(Pokemon, [TypeAsString | T]) :- 
    assertz(type(Pokemon, TypeAsString)),
    insertTypeIntoKnowledgeBase(Pokemon, T).

insertLocationIntoKnowledgeBase(_, []).
insertLocationIntoKnowledgeBase(Pokemon, [LocationAsString | T]) :- 
    location(Pokemon, LocationAsString), !,
    insertLocationIntoKnowledgeBase(Pokemon, T).
insertLocationIntoKnowledgeBase(Pokemon, [LocationAsString | T]) :- 
    assertz(location(Pokemon, LocationAsString)),
    insertLocationIntoKnowledgeBase(Pokemon, T).

insertEncounterIntoKnowledgeBase(L0) :- encounter(L0).
insertEncounterIntoKnowledgeBase(L0) :-
    not(encounter(L0)), 
    assertz(encounter(L0)).

/* === Clears all dynamic predicates === */
clearKB(R) :- 
    retractall(type(_,_)), 
    retractall(location(_,_)), 
    retractall(encounter(_)), 
    string_to_atom("success", R).
