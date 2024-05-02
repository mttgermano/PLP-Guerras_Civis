:- include('./../Controllers/Menu.pl').



menu_template("WriteChat",
    [
    "┌───────────────────────────── Guerras Civis ──────────────────────────────┐",
    "│Write your mensage,write exit to stop.                                    │",
    "│                                                                          │",
    "└──────────────────────────────────────────────────────────────────────────┘"]).



write_chat(MenuTemplate) :- 
    cl,
    print_menu(MenuTemplate),
    read_line_to_string(user_input,Input),
    write_to_chat_till_exit,
    menu_template("Start", Menu),
    menu_main(Menu).

write_to_chat_till_exit :-
    write_word(Word),( Word = "exit" ->  true;   write_to_chat_till_exit).

write_word(Word) :-
    read_line_to_string(user_input, Line),
    split_string(Line, " ", "", [Word|_]),
    ( Word = "exit" -> true; append_to_file('chat.txt',Line)).

append_newline(String, StringWithNewline) :-
    atom_concat(String, '.\n', StringWithNewline).

append_to_file(File,Text) :-
     append_newline(Text,LineF),
     open(File, append ,Stream),
     write(Stream,LineF),nl,
     close(Stream).
