% Builds a query from questions which can then be asked of the knowledge base
:- module(query_question, [question/2]).

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

% a noun_phrase is a determiner followed by a noun
noun_phrase(L0, Ind) :-
    det(L0, L1),
    noun(L1, Ind).

% question(Question, Ind) is true if Ind is an answer to Question 
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

/*
question(["What", "Pokemon", "is", "fire", "type"], Ind).
Ind = ["charmander", "charmeleon", "charizard", "vulpix", "ninetales", "growlithe", "arcanine", "ponyta", "rapidash"|...].
question(["What", "is", "at", "spear-pillar-area"], Ind).
Ind = ["dialga", "palkia"].
question(["Where", "is", "Dialga"], Ind).
Ind = "spear-pillar-area" ;
Ind = "sinjoh-ruins-area" .
*/
