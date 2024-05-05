:- include('./Utils.pl').
:- include('./../Databases/UserGameData.pl').

menu_template("Game", Rname, Players, IsAlive, Role, Round, State, Menu):- spaces1(X),spaces2(Y),spaces3(Z),spaces4(V),
format(string(RoomData), '│ > Room: ~w~w│\n│~w│\n│ > Round: ~w - ~w~w│\n│~w│', [Rname,X,Y,Round,State,Z,V]),



    with_output_to(string(PlayerData), print_lists(Players, IsAlive, Role)),
    Menu = [
            "┌───────────────────────────── Guerras Civis ──────────────────────────────┐",
            RoomData,
            "│ Players:  IsAlive:  Role:                                                │",
            PlayerData,
            "│                                                                          │",
            "│                                                                          │",
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
spaces4(X) :- X = "                                                                          ".

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


%%adicionar calculo para formatacao dependendo do tamanho da palavra
prepend_pipe_to_strings([], []).
prepend_pipe_to_strings([String|Rest], [ModifiedString|ModifiedRest]) :-
    atom_concat('│', String, ModifiedString),
    prepend_pipe_to_strings(Rest, ModifiedRest).

% Util ---------------------------------------------
print_lists([], [], []):- !.
print_lists([Player|Players], [IsAlive|IsAliveList], [Role|Roles]) :-
    format(atom(PlayerData), "| ~w    ~w         ~w", [Player, IsAlive, Role]),
    writeln(PlayerData),
    print_lists(Players, IsAliveList, Roles).

% Início do Jogo / Loop - Vai receber os dados do jogo e chamar o template
loop_match(Cpname, Rname):-
    Players = ["bot-4323", "bot-3213", "bot-3212", "bot-9873"],
    Alive = ["T", "T", "F", "T"],
    Role = ["???", "Assassino", "Policial", "???"],
    menu_template("Game", Rname, Players, Alive, Role, Round, State, Menu),
    menu_game(Cpname, Players, Menu).

menu_game(Cpname, Players, Menu):-
    cl,
    print_menu(Menu),
    read_line_to_string(user_input, Input),
    switch_game_action(Input, Cpname, Players, Menu).

% Ação
switch_game_action("1", Cpname, Players, Menu):-
    write("│ Qual jogador você quer executar sua ação?    $ "),
    read_line_to_string(user_input, ActionTarget),
    (
        member(ActionTarget, Players) -> 
        writeln("Carregando..."), sleep(2), menu_game(Cpname, Players, Menu) 
        ; writeln("Nome incorreto, tente novamente"), sleep(2), menu_game(Cpname, Players, Menu)
        ).
    % realizar_action(Cpname, ActionTarget), % ABSTRACT
    % writeln("Carregando..."),
    % sleep(2), % Será preciso esperar um tempo para os bots jogarem?
    % menu_game(Cpname, Menu).
    
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

% Ações do chat
switch_chat_menu_action("1", Cpname, _):-
    get_rname(Rname), 
    loop_match(Cpname, Rname), !.

switch_chat_menu_action("2", Cpname, Menu):-
    chat_menu(Menu, Cpname), !.

switch_chat_menu_action(Input, Cpname, _):-
    atom_concat(Cpname, ': ', MessagePrefix),
    atom_concat(MessagePrefix, Input, Message),
    get_rname(Rname),
    add_message_to_room(Rname, Message),
    switch_game_action("2", Cpname, _, _).
