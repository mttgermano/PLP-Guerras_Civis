:- include('./../Databases/Rooms.pl').

% Menu Template -------------------------------------------------
menu_template("Start",
    [
    "┌───────────────────────────── Guerras Civis ──────────────────────────────┐",
    "│ > Welcome to Guerras Civis!                                              │",
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
    "└──────────────────────────────────────────────────────────────────────────┘"]).

menu_template("PlayerCreate",
    [
    "┌───────────────────────────── Guerras Civis ──────────────────────────────┐",
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
    "┌───────────────────────────── Guerras Civis ──────────────────────────────┐",
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
    "┌───────────────────────────── Guerras Civis ──────────────────────────────┐",
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
    "┌───────────────────────────── Guerras Civis ──────────────────────────────┐",
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
    "┌───────────────────────────── Guerras Civis ──────────────────────────────┐",
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

menu_template("RoomWait", RName, Cpname, Menu) :-
    format(string(MenuLine),
            '│ > Room Wait (~w)                                                       │',
            [RName]),
    format(string(Players),
            '| > Players:                                                               |\n| - ~w', 
            [Cpname]),
    Menu = [
        "┌───────────────────────────── Guerras Civis ──────────────────────────────┐",
        MenuLine,
        "│                                                                          │",
        "│                                                                          │",
        "│                                                                          │",
        Players,
        "│                                                                          │",
        "│                                                                          │",
        "│                                                                          │",
        "│                                                                          │",
        "│                                                                          │",
        "│                                                                          │",
        "│ [1] Start Game                                                           │",
        "│ [2] Atualizar                                                            │",
        "└──────────────────────────────────────────────────────────────────────────┘"
    ].
    

menu_template("RoomWaitNotRoomMaster", RName, Cpname, Menu) :-
    format(string(MenuLine),
            '│ > Room Wait (~w)                                                        │',
            [RName]),
    format(string(Players),
            '| > Players:                                                            |\n ~w', 
            [Cpname]),
    Menu = [
        "┌───────────────────────────── Guerras Civis ──────────────────────────────┐",
        MenuLine,
        "│                                                                          │",
        "│                                                                          │",
        "│                                                                          │",
        "│                                                                          │",
        Players,
        "│                                                                          │",
        "│                                                                          │",
        "│                                                                          │",
        "│                                                                          │",
        "│                                                                          │",
        "│                                                                          │",
        "└──────────────────────────────────────────────────────────────────────────┘"
    ].

% Utils ---------------------------------------------------------
print_menu([]).
print_menu([X|Xs]) :-
    writeln(X),  
    print_menu(Xs), !.

cl :- (current_prolog_flag(windows, true) -> shell('cls'); shell('clear')).


% Menu Principal -----------------------------------------------
menu_main(MenuTemplate) :-
    cl,
    print_menu(MenuTemplate),
    read_line_to_string(user_input, Input),
    switch_menu_main(Input), !.

% Escolha das acoes
switch_menu_main("1") :- 
    menu_template("PlayerCreate", Menu),
    menu_action("PlayerCreate", Menu), !.

switch_menu_main("2") :- 
    menu_template("PlayerLogin", Menu),
    menu_action("PlayerLogin", Menu), !.


% Menu Action ---------------------------------------------------
menu_action(MenuType, MenuTemplate) :-
    cl,
    print_menu(MenuTemplate),
    write("│ Player Name      $ "),
    read_line_to_string(user_input, Input1),
    write("│ Player Password  $ "),
    read_line_to_string(user_input, Input2),
    switch_menu_action(MenuType, Input1, Input2), !.

% Escolha das acoes
switch_menu_action("PlayerCreate", Pname, Ppassword) :- 
    add_player(Pname, Ppassword, false),
    menu_template("Room", Menu),
    menu_room(Menu, Pname), !.

switch_menu_action("PlayerLogin",  Pname, Ppassword) :- 
    (\+player_login(Pname, Ppassword) -> writeln("player não existe"), sleep(3), menu_template("Start", StartMenu), menu_main(StartMenu) ; menu_template("Room", Menu), menu_room(Menu), !).

% Menu Room -----------------------------------------------------
menu_room(MenuTemplate, Cpname) :-  % Current Player Name
    cl,
    print_menu(MenuTemplate),
    read_line_to_string(user_input, Input),
    switch_menu_room(Input, Cpname), !.

% Escolha das acoes
switch_menu_room("1", Cpname) :- 
    menu_template("RoomCreate", Menu),
    menu_room_action("RoomCreate", Menu, Cpname), !.

switch_menu_room("2", Cpname) :- 
    menu_template("RoomLogin", Menu),
    menu_room_action("RoomLogin", Menu, Cpname), !.


% Menu Room Action ----------------------------------------------
menu_room_action(MenuType, MenuTemplate, Cpname) :-
    cl,
    print_menu(MenuTemplate),
    write("│ Room Name      $ "),
    read_line_to_string(user_input, Input),
    switch_menu_room_action(MenuType, Input, Cpname), !.

% Escolha das acoes
switch_menu_room_action("RoomCreate", Rname, Cpname) :- 
    add_room(Rname, Cpname, "asd"),
    menu_template("RoomWait", Rname, Cpname, Menu),
    menu_room_wait(Menu, Cpname), !.

switch_menu_room_action("RoomLogin", Rname, Cpname) :- 
    room_login(Rname, Cpname),
    menu_template("RoomWait", Menu),
    menu_room_wait(Menu, Cpname), !.


% Menu Room Wait ------------------------------------------------
menu_room_wait(Menu, Cpname) :-
    cl,
    print_menu(Menu),
    read_line_to_string(user_input, Input),
    switch_menu_room_wait_start(Input, Menu), !.

% Início do Jogo
switch_menu_room_wait_start("1", _):- 
    menu_template("Game", Menu).
switch_menu_room_wait_start("2", Menu):-
    menu_room_wait(Menu, Cpname), !.