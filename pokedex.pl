:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(api).
:- use_module(query_question).

q(Ans) :-
    write("Ask me: "), flush_output(current_output),
    read_line_to_string(user_input, St),
    split_string(St, " ", " ,?.!-", Ln), % ignore punctuation
    question(Ln, Ans).

start(Ans) :-
    write("\nList of commands:\n"),
    write("e - Enter a new Pokemon encounter.\n"),
    write("q - Ask a question.\n"),
    write("l - List all encountered or searched Pokemon.\n"),
    write("c - Clear history.\n"),
    write("Input: "),
    read_line_to_string(user_input, St),
    handle_command(St, Ans).

start(Ans) :-
    write("No more answers or invalid query\n"),
    write("Would you like to exit? "), flush_output(current_output),
    read_line_to_string(user_input, St0),
    string_lower(St0, St),
    end(St, Ans).

handle_command("e", Ans) :- 
    write("Enter encounter: "),
    read_line_to_string(user_input, St),
    addEncounter(St, Ans).

handle_command("q", Ans) :- q(Ans).
handle_command("l", Ans) :- getEncounter(Ans).
handle_command("c", Ans) :- clearKB(Ans).

end("yes", _).
end("no", Ans) :- start(Ans).


% location("dialga", Location)
% start(Ans).
% q.
% What type is Dialga?
% What Pokemon is in spear-pillar-area?

