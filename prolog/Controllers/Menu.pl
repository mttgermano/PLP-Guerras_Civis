%:- include('Login/Player.pl').

% Menu Template -------------------------------------------------
menu_template("Start",
    [
    "┌───────────────────────────── Guerrras Civis ─────────────────────────────┐",
    "│ > Main                                                                   │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│ [1]  Create Player                                                       │",
    "│ [2]  Login  Player                                                       │",
    "│ [3]  Chat   Room                                                         │",
    "└──────────────────────────────────────────────────────────────────────────┘"]).

menu_template("PlayerCreate",
    [
    "┌───────────────────────────── Guerrras Civis ─────────────────────────────┐",
    "│ > Player Register                                                        │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "└──────────────────────────────────────────────────────────────────────────┘"]).

menu_template("PlayerLogin",
    [
    "┌───────────────────────────── Guerrras Civis ─────────────────────────────┐",
    "│ > Player Login                                                           │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "└──────────────────────────────────────────────────────────────────────────┘"]).

menu_template("Room",
    [
    "┌───────────────────────────── Guerrras Civis ─────────────────────────────┐",
    "│ > Room                                                                   │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│ [1] Create Room                                                          │",
    "│ [2] Login  Room                                                          │",
    "└──────────────────────────────────────────────────────────────────────────┘"]).

menu_template("RoomCreate",
    [
    "┌───────────────────────────── Guerrras Civis ─────────────────────────────┐",
    "│ > Room Register                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "└──────────────────────────────────────────────────────────────────────────┘"]).






menu_template("RoomLogin",
    [
    "┌───────────────────────────── Guerrras Civis ─────────────────────────────┐",
    "│ > Room Login                                                             │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "└──────────────────────────────────────────────────────────────────────────┘"]).

menu_template("RoomWait",
    [
    "┌───────────────────────────── Guerrras Civis ─────────────────────────────┐",
    "│ > Room Wait                                                              │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "│                                                                          │",
    "└──────────────────────────────────────────────────────────────────────────┘"]).



menu_template("RoomChat", MenuTemplate) :-
   open('text.txt', read, Str),
   read_houses(Str, Houses),
   close(Str),
   prepend_pipe_to_strings(Houses,ModifiedList),
   append(["┌───────────────────────────── Guerrras Civis ─────────────────────────────┐"], ModifiedList, MenuWithHeader),
   append(MenuWithHeader, ["└──────────────────────────────────────────────────────────────────────────┘"], MenuTemplate).



% Utils ---------------------------------------------------------
print_menu([]).
print_menu([X|Xs]) :-
    writeln(X),  
    print_menu(Xs).

cl :- (current_prolog_flag(windows, true) -> shell('cls'); shell('clear')).

read_houses(Stream, []):-
  at_end_of_stream(Stream).

read_houses(Stream, [X|L]):-
  \+ at_end_of_stream(Stream),
  read(Stream, X),
  read_houses(Stream, L).

prepend_pipe_to_strings([], []).
prepend_pipe_to_strings([String|Rest], [ModifiedString|ModifiedRest]) :-
    atom_concat('│', String, ModifiedString),
    prepend_pipe_to_strings(Rest, ModifiedRest).




% Menu Principal -----------------------------------------------
menu_main(MenuTemplate) :-
    cl,
    print_menu(MenuTemplate),
    read_line_to_string(user_input, Input),
    switch_menu_main(Input).

% Escolha das acoes
switch_menu_main("1") :- 
    menu_template("PlayerCreate", Menu),
    menu_action("PlayerCreate", Menu).

switch_menu_main("2") :- 
    menu_template("PlayerLogin", Menu),
    menu_action("PlayerLogin", Menu).


switch_menu_main("3") :- 
    menu_template("RoomChat", Menu),
    print_menu(Menu).



% Menu Action ---------------------------------------------------
menu_action(MenuType, MenuTemplate) :-
    cl,
    print_menu(MenuTemplate),
    write("│ Player Name      $ "),
    read_line_to_string(user_input, Input1),
    write("│ Player Password  $ "),
    read_line_to_string(user_input, Input2),
    switch_menu_action(MenuType, Input1, Input2).

% Escolha das acoes
switch_menu_action("PlayerCreate", Pname, Ppassword) :- 
    player_create(Pname, Ppassword),
    menu_template("Room", Menu),
    menu_room(Menu).

switch_menu_action("PlayerLogin",  Pname, Ppassword) :- 
    player_login(Pname, Ppassword),
    menu_template("Room", Menu),
    menu_room(Menu).

player_create(A,B)  :- writeln(5).
player_login(A,B)   :- writeln(5).


% Menu Room -----------------------------------------------------
menu_room(MenuTemplate) :-
    cl,
    print_menu(MenuTemplate),
    read_line_to_string(user_input, Input),
    switch_menu_room(Input).

% Escolha das acoes
switch_menu_room("1") :- 
    menu_template("RoomCreate", Menu),
    menu_room_action("RoomCreate", Menu).

switch_menu_room("2") :- 
    menu_template("RoomLogin", Menu),
    menu_room_action("RoomLogin", Menu).



% Menu Room Action ----------------------------------------------
menu_room_action(MenuType, MenuTemplate) :-
    cl,
    print_menu(MenuTemplate),
    write("│ Room Name      $ "),
    read_line_to_string(user_input, Input1),
    switch_menu_room_action(MenuType, Input1).

% Escolha das acoes
switch_menu_room_action("RoomCreate", Rname) :- 
    room_create(Rname),
    menu_template("RoomWait", Menu),
    menu_room_wait(Menu).

switch_menu_room_action("RoomLogin",  Rname) :- 
    room_login(Rname),
    menu_template("RoomWait", Menu),
    menu_room_wait(Menu).

room_create(A)    :- writeln(5).
room_login(A)     :- writeln(5).


% Menu Room Wait ------------------------------------------------
menu_room_wait(Menu) :-
    writeln(5).
