%:- include('Login/Player.pl').

% Menu Template -------------------------------------------------
menu_template(Start,
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

menu_template(PlayerCreate,
    [
    "┌───────────────────────────── Guerrras Civis ─────────────────────────────┐",
    "│ > Register                                                               │",
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
    menu_template(PlayerCreate, Menu),
    menu_action(Menu).

switch_menu_main("2") :- 
    menu_template(PlayerLogin, Menu),
    menu_action(Menu).



% tem que pegar os dois dados do user
menu_action(MenuTemplate) :-
    cl,
    print_menu(MenuTemplate),
    write("│ Player Name      $ "),
    read_line_to_string(user_input, Input1),
    write("│ Player Password  $ "),
    read_line_to_string(user_input, Input2),
    switch_menu_action(MenuTemplate, Input1, Input2).

% Escolha das acoes
switch_menu_action(PlayerCreate, Pname, Ppassword) :- 
    player_create(Pname, Ppassword).

switch_menu_action(PlayerLogin,  Pname, Ppassword) :- 
    player_login(Pname, Ppassword).




player_create(A,B) :- write(5).
player_login(A,B) :- write(5).


% Menu Acoes ----------------------------------------------------
