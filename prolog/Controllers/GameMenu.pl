:- include('./Utils.pl').
menu_template("Game", Rname, Players, IsAlive, Role, Round, State, Menu):- spaces1(X),spaces2(Y),spaces3(Z),spaces4(V),
format(string(RoomData), '│ > Room: ~w~w│\n│~w│\n│ > Round: ~w - ~w~w│\n│~w│ ', [Rname,X,Y,Round,State,Z,V]),



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
    format(atom(PlayerData), "│ ~w    ~w         ~w", [Player, IsAlive, Role]),
    writeln(PlayerData),
    print_lists(Players, IsAliveList, Roles).

start_match(Rname):-
    Players = ["bot-4323", "bot-3213", "bot-3212", "bot-9873"],
    IsAlive = ["T", "T", "F", "T"],
    Role = ["???", "Assassino", "Policial", "???"],
    menu_template("Game", Rname, Players, IsAlive, Role, Round, State, Menu),
    menu_game(Menu).

menu_game(Menu):-
    cl,
    print_menu(Menu).
