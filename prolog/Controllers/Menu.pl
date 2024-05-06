:- include('./../Databases/Rooms.pl').
:- include('./GameMenu.pl').
:- include('./Utils.pl').
:-

:- discontiguous menu_template/2.
:- discontiguous player/4.

% Menu Template -------------------------------------------------
menu_template("Start",
    [
    "┌───────────────────────────── Guerras Civis ──────────────────────────────┐",
    "│ > Welcome to Guerras Civis                                               │",
    "│                                                                          │",
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
    "│ [3]  Quit                                                                │",
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
    "│ [1]  Go back                                                             │",
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
    "│ [1]  Go back                                                             │",
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
    "│ [1]  Go back                                                             │",
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
    "│ [1]  Go back                                                             │",
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


% Menu Principal -----------------------------------------------
menu_main(MenuTemplate) :-
    cl,
    print_menu(MenuTemplate),
    read_line_to_string(user_input, Input),
    switch_menu_main(Input, MenuTemplate).

% Escolha das acoes
switch_menu_main("1", _) :- 
    menu_template("PlayerCreate", Menu),
    menu_action("PlayerCreate", Menu), !.

switch_menu_main("2", _) :- 
    menu_template("PlayerLogin", Menu),
    menu_action("PlayerLogin", Menu), !.

switch_menu_main("3", _) :- 
    halt.

switch_menu_main(_, Menu):- 
    writeln("> Botão inválido, tente novamente"),
    sleep(2),
    menu_main(Menu).


% Menu Action ---------------------------------------------------
menu_action(MenuType, MenuTemplate) :-
    cl,
    print_menu(MenuTemplate),
    write("│ Player Name      $ "),
    read_line_to_string(user_input, Input1),
    (Input1 = "1"
    -> menu_template("Start", Menu), menu_main(Menu)
    ; write("│ Player Password  $ "),
    read_line_to_string(user_input, Input2),
    switch_menu_action(MenuType, Input1, Input2)).

% Escolha das acoes
switch_menu_action("PlayerCreate", Pname, Ppassword) :- 
    add_player(Pname, Ppassword, false),
    menu_template("Room", Menu),
    menu_room(Menu, Pname).

switch_menu_action("PlayerLogin",  Pname, Ppassword) :- 
    (\+player_login(Pname, Ppassword) 
    -> writeln("> Player não existe!"), sleep(1), switch_menu_main("2", _)
    ; menu_template("Room", Menu), menu_room(Menu)).



% Menu Room -----------------------------------------------------
menu_room(MenuTemplate, Cpname) :-  % Current Player Name
    cl,
    print_menu(MenuTemplate),
    read_line_to_string(user_input, Input),
    switch_menu_room(Input, Cpname, MenuTemplate).

% Escolha das acoes
switch_menu_room("1", Cpname, _) :- 
    menu_template("RoomCreate", Menu),
    menu_room_action("RoomCreate", Menu, Cpname).

switch_menu_room("2", Cpname, _) :- 
    menu_template("RoomLogin", Menu),
    menu_room_action("RoomLogin", Menu, Cpname).

switch_menu_room(_, Cpname, Menu):-
    writeln("Botão inválido, tente novamente"),
    sleep(2),
    menu_room(Menu, Cpname).


% Menu Room Action ----------------------------------------------
menu_room_action(MenuType, MenuTemplate, Cpname) :-
    cl,
    print_menu(MenuTemplate),
    write("│ Room Name      $ "),
    read_line_to_string(user_input, Input),
    (Input = "1" 
    -> menu_template("Room", Menu), menu_room(Menu, Cpname)
    ; switch_menu_room_action(MenuType, Input, Cpname)).

set_rname(NewRname) :-
    retractall(rname(_)),
    assertz(rname(NewRname)).

get_rname(Rname) :-
    rname(Rname).

% Escolha das acoes
switch_menu_room_action("RoomCreate", Rname, Cpname) :- 
    add_room(Rname, Cpname, "asd"),
    set_rname(Rname),
    menu_template("RoomWait", Rname, Cpname, Menu),
    menu_room_wait(Menu, Rname, Cpname).

switch_menu_room_action("RoomLogin", Rname, Cpname) :- 
    (\+room_login(Rname, Cpname)
    -> writeln("> Room não existe!"), sleep(1), switch_menu_room("2", Cpname, "asd")
    ; menu_template("RoomWait", Rname, Cpname, Menu),
    menu_room_wait(Menu, Rname, Cpname)).



% Menu Room Wait ------------------------------------------------
menu_room_wait(Menu, Rname, Cpname) :-
    cl,
    print_menu(Menu),
    read_line_to_string(user_input, Input),
    switch_menu_room_wait_action(Input, Cpname, Menu).

% Iniciar Jogo
switch_menu_room_wait_action("1", Cpname, _):- 
    writeln("Loading Game..."),
    sleep(2),
    get_rname(Rname),
    start_match(Cpname, Rname), !. % Indo para GameMenu.pl

% Atualizar sala
switch_menu_room_wait_action("2", Cpname, Menu):-
    get_rname(Rname),
    menu_room_wait(Menu, Cpname, Rname), !.

switch_menu_room_wait_action(_, Cpname, Menu):-
    get_rname(Rname),
    writeln("Botão inválido, tente novamente"),
    sleep(2),
    menu_room_wait(Menu, Cpname, Rname).