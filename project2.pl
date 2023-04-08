:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- dynamic location/2.

combine_request(Pokemon, Request) :-
    string_concat("https://pokeapi.co/api/v2/pokemon/", Pokemon, Request1),
    string_concat(Request1, "/encounters", Request).

query_api(Pokemon, Result) :- 
    combine_request(Pokemon, Request),
    http_get(Request, Response, []),
    atom_json_dict(Response, Data, []),
    getAllLocations(Data, AllLocations),
    insertLocationIntoKnowledgeBase(Pokemon, AllLocations),
    Result = AllLocations.

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

format_answer(Pokemon, Locations, Ans) :-
    strings_concat(Locations, LocString),
    string_concat(Pokemon, " can be found at the ", Ans1),
    string_concat(Ans1, LocString, Ans).

% Concatenates a list of locations to a String
strings_concat([], "").
strings_concat([H, T], Str) :-
    string_concat(" and the ", T, Str0),
    string_concat(H, Str0, Str), !.
strings_concat([H|T], Str) :-
    strings_concat(T, Str0),
    string_concat(", the ", Str0, Str1),
    string_concat(H, Str1, Str).

% ask(Q, A) gives answer A to question Q
ask(Q, A) :-
    question(Q, A).

% Determiners are ignored and do not provide extra constraints
det(["the" | L], L).
det(L, L).

noun([L], Ind) :- location(Ind, L).

noun_phrase(L0, Ind) :-
    det(L0, L1),
    noun(L1, Ind).

question(["Where", "is", L0], Ind) :-
    string_lower(L0, L1),
    query_api(L1, Locations),
    format_answer(L0, Locations, Ind).

question(["What", "is", "at"| L0], Ind) :-
    noun_phrase(L0, Ind).

question(["What", "Pokemon", "is", "at" | L0], Ind) :-
    noun_phrase(L0, Ind).

q(Ans) :-
    write("Ask me: "), flush_output(current_output),
    read_line_to_string(user_input, St),
    split_string(St, " ", " ,?.!-", Ln), % ignore punctuation
    ask(Ln, Ans).
q(Ans) :-
    write("No more answers\n"),
    write("Would you like to exit? "), flush_output(current_output),
    read_line_to_string(user_input, St0),
    string_lower(St0, St),
    q(St, Ans).

q("yes", _).
q("no", Ans) :- q(Ans).

