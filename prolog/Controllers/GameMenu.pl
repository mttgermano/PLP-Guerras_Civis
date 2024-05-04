:- include('./Utils.pl').
menu_template("Game", Rname, Players, IsAlive, Role, Round, State, Menu):-

    format(string(RoomData), '│ > Room: ~w                                                             │\n│                                                                          │\n│ > Round: ~w - ~w                                                 │\n│                                                                          │ ', [Rname, Round, State]),



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
