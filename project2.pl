:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- dynamic location/2, type/2.

add_pokemon(Pokemon) :-
    query_locations(Pokemon),
    query_types(Pokemon).

query_locations(Pokemon) :-
    atomics_to_string(["https://pokeapi.co/api/v2/pokemon/", Pokemon, "/encounters"], Request),
    http_get(Request, Response, []),
    atom_json_dict(Response, Data, []),
    getAllLocations(Data, AllLocations),
    insertLocationIntoKnowledgeBase(Pokemon, AllLocations).

query_types(Pokemon) :-
    atomics_to_string(["https://pokeapi.co/api/v2/pokemon/", Pokemon], Request),
    http_get(Request, Response, []),
    atom_json_dict(Response, Data, []),
    getAllTypes(Data.types, AllTypes),
    insertTypeIntoKnowledgeBase(Pokemon, AllTypes).

getAllTypes([], []).
getAllTypes([Type | T], [Type.type.name | ResultsSoFar]) :-
    getAllTypes(T, ResultsSoFar).

insertTypeIntoKnowledgeBase(_, []).
insertTypeIntoKnowledgeBase(Pokemon, [TypeAsString | T]) :- 
    type(Pokemon, TypeAsString), !,
    insertLocationIntoKnowledgeBase(Pokemon, T).
insertTypeIntoKnowledgeBase(Pokemon, [TypeAsString | T]) :- 
    assertz(type(Pokemon, TypeAsString)),
    insertTypeIntoKnowledgeBase(Pokemon, T).

getAllLocations([], []).
getAllLocations([OneLocation | T], [OneLocation.location_area.name | ResultsSoFar]) :-
    getAllLocations(T, ResultsSoFar).

insertLocationIntoKnowledgeBase(_, []).
insertLocationIntoKnowledgeBase(Pokemon, [LocationAsString | T]) :- 
    location(Pokemon, LocationAsString), !,
    insertLocationIntoKnowledgeBase(Pokemon, T).
insertLocationIntoKnowledgeBase(Pokemon, [LocationAsString | T]) :- 
    assertz(location(Pokemon, LocationAsString)),
    insertLocationIntoKnowledgeBase(Pokemon, T).

get_location(Pokemon, Location) :- location(Pokemon, Location).
get_location(Pokemon, Location) :- not(location(Pokemon, Location)), add_pokemon(Pokemon), location(Pokemon, Location).

not(P) :- P, !, fail ; true.

preposition("at").
preposition("in").
preposition("on").

pp([L0| L1], Ind) :-
    preposition(L0),
    noun_phrase(L1, Ind).

% Determiners are ignored and do not provide extra constraints
det(["the" | L], L).
det(L, L).

noun([L], Ind) :- location(Ind, L).

noun_phrase(L0, Ind) :-
    det(L0, L1),
    noun(L1, Ind).

question(["Where", "is", L0], Ind) :-
    string_lower(L0, L1),
    get_location(L1, Ind).

question(["What", "is" | L0], Ind) :-
    pp(L0, Ind).

question(["What", "Pokemon", "is" | L0], Ind) :-
    pp(L0, Ind).

q(Ans) :-
    write("Ask me: "), flush_output(current_output),
    read_line_to_string(user_input, St),
    split_string(St, " ", " ,?.!-", Ln), % ignore punctuation
    question(Ln, Ans).
q(Ans) :-
    write("No more answers\n"),
    write("Would you like to exit? "), flush_output(current_output),
    read_line_to_string(user_input, St0),
    string_lower(St0, St),
    q(St, Ans).

q("yes", _).
q("no", Ans) :- q(Ans).
