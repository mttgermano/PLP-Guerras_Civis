:- include('./Utils.pl').
:- include('./../Game/GameFunctions.pl').

menu_template("Game", Rname, Players, IsAlive, Role, Round, Period, Menu):- spaces1(X),spaces2(Y),spaces3(Z),
format(string(RoomData), '│ > Room: ~w~w│\n│~w│\n│ > Round: ~w - ~w~w│', [Rname,X,Y,Round,Period,Z]),



    with_output_to(string(PlayerData), print_lists(Players, IsAlive, Role)),
    Menu = [
            "┌───────────────────────────── Guerras Civis ──────────────────────────────┐",
            RoomData,
            "│                                                                          │",
            "│ Players:   IsAlive:    Role:                                             │",
            PlayerData,
            "│                                                                          │",
            "│                                                                          │",
            "│──────────────────────────────────────────────────────────────────────────│",
            "│                                                                          │",
            "│ [1] Realizar Ação                                                        │",
            "│ [2] Enviar Mensagem                                                      │",
            "│                                                                          │",
            "└──────────────────────────────────────────────────────────────────────────┘"].

spaces1(X) :- X = "                                                             ". 
spaces2(X) :- X = "                                                                          ".
spaces3(X) :- X = "                                                 ".

menu_template("RoomChat", MenuTemplate) :-
    %mostar numero n de linhas....
    %manter em loop?
    %ou escolher acao?
    %botao Atualizar chat
    get_rname(Rname),
    get_room_messages(Rname, Messages),
    prepend_pipe_to_strings(Messages, ModifiedList),
    append(["┌───────────────────────────── Guerras Civis ──────────────────────────────┐\n|"], ModifiedList, MenuWithMessages),
    append(MenuWithMessages, ["|\n|──────────────────────────────────────────────────────────────────────────|"], MenuWithButtonHeader),
    append(MenuWithButtonHeader,["|\n| [1] Back Menu\n| [2] Atualizar\n|"], MenuWithButtons),%pode virar um template so.....
    append(MenuWithButtons, ["└──────────────────────────────────────────────────────────────────────────┘"], MenuTemplate).

menu_template("M",
    [
    "┌───────────────────────────── Guerras Civis ──────────────────────────────┐",
    "│ > Os mafiosos venceram!                                                  │",
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
    "│                                                                          │",
    "│                                                                          │",
    "└──────────────────────────────────────────────────────────────────────────┘"]).
menu_template("C",
    [
    "┌───────────────────────────── Guerras Civis ──────────────────────────────┐",
    "│ > Os civis venceram!                                                     │",
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
    "│                                                                          │",
    "│                                                                          │",
    "└──────────────────────────────────────────────────────────────────────────┘"]).

%%adicionar calculo para formatacao dependendo do tamanho da palavra
prepend_pipe_to_strings([], []):- !.
prepend_pipe_to_strings([String|Rest], [ModifiedString|ModifiedRest]) :-
    atom_concat('│', String, ModifiedString),
    prepend_pipe_to_strings(Rest, ModifiedRest).

% Util ---------------------------------------------
print_lists([], [], []):- !.
print_lists([Player|Players], [IsAlive|IsAliveList], [Role|Roles]) :-
    translate_role(Role, R),
    format(atom(PlayerData), "| ~w    ~w      ~w", [Player, IsAlive, R]),
    writeln(PlayerData),
    print_lists(Players, IsAliveList, Roles).

translate_role(-1, "???").
translate_role(1, "Assassino").
translate_role(2, "Aprendiz").
translate_role(3, "Paparazzi").
translate_role(4, "Paralisador").
translate_role(5, "Silenciador").
translate_role(6, "Bruxo").
translate_role(7, "Detetive").
translate_role(8, "Juiz").
translate_role(9, "Policial").
translate_role(10, "Médico").
translate_role(11, "Aldeão").
translate_role(12, "Espírito Vingativo").


start_match(Cpname, Rname):-
    start_game(Rname, Cpname),
    loop_match(Cpname, Rname).

% Início do Jogo / Loop - Vai receber os dados do jogo, chamar o template e esperar escolha
loop_match(Cpname, Rname):-
    get_room_state(Rname, State, Nround),
    measure_state(State, Period),
    (Period = "Civis" ; Period = "Mafiosos" -> 
        menu_template(State, Menu), 
        menu_winner(Menu) 
        ;
        get_players_alive_role(Cpname, Players, Alive, Role),
        menu_template("Game", Rname, Players, Alive, Role, Nround, Period, Menu),
        menu_game(Cpname, Players, Menu)   
    ).

measure_state("A", "Noite").
measure_state("V", "Dia").
measure_state("C", "Civis").
measure_state("M", "Mafiosos").

menu_winner(Menu):-
    cl,
    print_menu(Menu).

menu_game(Cpname, Players, Menu):-
    cl,
    print_menu(Menu),
    read_line_to_string(user_input, Input),
    switch_game_action(Input, Cpname, Players, Menu).

% Ação
switch_game_action("1", Cpname, Players, Menu):-
    write("│ Qual jogador você quer executar sua ação?    $ "),
    read_line_to_string(user_input, ActionTarget), (
        member(ActionTarget, Players)
        ->  writeln("Carregando..."), player_action(Cpname, ActionTarget), sleep(2), menu_game(Cpname, Players, Menu) 
        ;   writeln("Nome incorreto, tente novamente"), sleep(2), menu_game(Cpname, Players, Menu)
        ).
    menu_game(Cpname, Menu).
    
% Chat de mensagem
switch_game_action("2", Cpname, _, _):-
    menu_template("RoomChat", Menu),
    chat_menu(Menu, Cpname).

% Botão inválido
switch_game_action(_, Cpname, Players, Menu):-
    writeln("Botão inválido, tente novamente"),
    sleep(2),
    menu_game(Cpname, Players, Menu). 

chat_menu(Menu, Cpname):-
    cl,
    print_menu(Menu),
    write("| Message  $ "),
    read_line_to_string(user_input, Input),
    switch_chat_menu_action(Input, Cpname, Menu).

% Voltar
switch_chat_menu_action("1", Cpname, _):-
    get_rname(Rname), 
    loop_match(Cpname, Rname), !.

% Atualizar chat
switch_chat_menu_action("2", Cpname, Menu):-
    chat_menu(Menu, Cpname), !.

% Mensagem
switch_chat_menu_action(Input, Cpname, _):-
    atom_concat(Cpname, ': ', MessagePrefix),
    atom_concat(MessagePrefix, Input, Message),
    get_rname(Rname),
    add_message_to_room(Rname, Message),
    switch_game_action("2", Cpname, _, _).
