menu_template("Game", Rname, Players, IsAlive, Role, Round, State, Menu):-
    format(string(RoomData),
        '| > Room: ~w\n|\n| > Round: ~w - ~w\n|',
        [Rname, Round, State]),
    format(string(GameData),
        '| Players: ~w\n| isAlive: ~w\n|    Role: ~w',
        [Players, IsAlive, Role]),
    Menu = [
        "┌───────────────────────────── Guerras Civis ──────────────────────────────┐",
        RoomData,
        GameData,
        "│                                                                          │",
        "│                                                                          │",
        "│                                                                          │",
        "│                                                                          │",
        "│                                                                          │",
        "│                                                                          │",
        "│                                                                          │",
        "│──────────────────────────────────────────────────────────────────────────│",
        "│ [1] Realizar Ação                                                        │",
        "│ [2] Enviar Mensagem                                                      │",
        "└──────────────────────────────────────────────────────────────────────────┘"
    ].

start_match(Rname):-
    
    menu_template("Game", Rname, Players, IsAlive, Role, Round, State, Menu),
    menu_game(Menu).

menu_game(Menu):-
    cl,
    print_menu(Menu).