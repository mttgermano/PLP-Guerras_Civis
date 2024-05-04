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

% Util ---------------------------------------------
print_lists([], [], []):- !.
print_lists([Player|Players], [IsAlive|IsAliveList], [Role|Roles]) :-
    format(atom(PlayerData), "| ~w    ~w         ~w", [Player, IsAlive, Role]),
    writeln(PlayerData),
    print_lists(Players, IsAliveList, Roles).

% Início do Jogo / Loop - Vai receber os dados do jogo e chamar o template
start_match(Cpname, Rname):-
    get_players_alive_role(Cpname, Players, Alive, Role),
    menu_template("Game", Rname, Players, Alive, Role, Round, State, Menu),
    menu_game(Cpname, Menu).

menu_game(Cpname, Menu):-
    cl,
    print_menu(Menu),
    read_line_to_string(user_input, Input),
    switch_game_action(Input, Cpname, Menu).

% Ação
switch_game_action("1", Cpname, Menu):-
    write("│ Qual jogador você quer executar sua ação?    $ "),
    read_line_to_string(user_input, ActionTarget), % Validar jogador (provv mesma ideia em login)
    % realizar_action(Cpname, ActionTarget), % ABSTRACT
    writeln("Carregando..."),
    sleep(2), % Será preciso esperar um tempo para os bots jogarem?
    menu_game(Cpname, Menu).
    
% Chat de mensagem
switch_game_action("2", Cpname, Menu).
% Enviar para a interface de mensagens

% Botão inválido
switch_game_action(_, Cpname, Menu):-
    writeln("Botão inválido, tente novamente"),
    sleep(2),
    menu_game(Cpname, Menu).