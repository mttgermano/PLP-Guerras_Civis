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

% TODO
% depois que o player fez login, coloca o nome dele abaixo de '> Room',
% para saber qual user está logado
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

% Utils ---------------------------------------------------------
print_menu([]).
print_menu([X|Xs]) :-
    writeln(X),  
    print_menu(Xs).

cl :- (current_prolog_flag(windows, true) -> shell('cls'); shell('clear')).



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
    add_player(Pname, Ppassword, false),
    % TODO
    % colocar switch de verificacao. Se o player ja existe....
    menu_template("Room", Menu),
    menu_room(Menu).


switch_menu_action("PlayerLogin",  Pname, Ppassword) :- 
    player_login(Pname, Ppassword),
    % TODO
    % mesmo caso da linha 158
    menu_template("Room", Menu),
    menu_room(Menu).


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
    add_room(Rname, _, _),
    menu_template("RoomWait", Menu),
    menu_room_wait(Menu).

switch_menu_room_action("RoomLogin",  Rname) :- 
    room_login(Rname, _),
    menu_template("RoomWait", Menu),
    menu_room_wait(Menu).


% Menu Room Wait ------------------------------------------------
menu_room_wait(Menu) :-
    writeln(5).
