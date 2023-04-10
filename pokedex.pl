:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(api).

preposition("at").
preposition("in").
preposition("on").

pp([L0| L1], Ind) :-
    preposition(L0),
    noun_phrase(L1, Ind).

pp([L0 | L1], Individual) :- 
    reln([L0 | L1], Individual).

reln([PokemonType, "type"], Individual) :- 
    string_lower(PokemonType, LowerCasePokemonType),
    queryAllPokemonFromType(LowerCasePokemonType, Individual).

reln([Pokemon], Individual) :- 
    string_lower(Pokemon, LowerCasePokemon),
    getTypeFromKBOrQuery(LowerCasePokemon, Individual).

% Determiners are ignored and do not provide extra constraints
det(["the" | L], L).
det(L, L).

noun([L], Result) :- 
    string_lower(L, LowerCaseL),
    queryAllPokemonFromLocation(LowerCaseL, Result).

noun_phrase(L0, Ind) :-
    det(L0, L1),
    noun(L1, Ind).

question(["Where", "is", L0], Ind) :-
    string_lower(L0, L1),
    getLocationFromKBOrQuery(L1, Ind).

question(["What", "is" | L0], Ind) :-
    pp(L0, Ind).

question(["What", "Pokemon", "is" | L0], Ind) :-
    pp(L0, Ind).

question(["What", "pokemon", "is" | L0], Ind) :-
    pp(L0, Ind).

question(["What", "type", "is" | RestOfQuestion], Individual) :- 
    pp(RestOfQuestion, Individual).

q(Ans) :-
    write("Ask me: "), flush_output(current_output),
    read_line_to_string(user_input, St),
    split_string(St, " ", " ,?.!-", Ln), % ignore punctuation
    question(Ln, Ans).
q(Ans) :-
    write("No more answers or invalid query\n"),
    write("Would you like to exit? "), flush_output(current_output),
    read_line_to_string(user_input, St0),
    string_lower(St0, St),
    q(St, Ans).

q("yes", _).
q("no", Ans) :- q(Ans).


% location("dialga", Location)
% q(Ans).
% What type is Dialga?
% What Pokemon is in spear-pillar-area?
% What is at 
