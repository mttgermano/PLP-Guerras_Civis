list_alive_players :-
    findall(Name, player(Name, _, true), AlivePlayers),
    list_players_with_numbers(AlivePlayers, 1).

list_players_with_numbers([], _).
list_players_with_numbers([H|T], Index) :-
    format("~w. ~w~n", [Index, H]),
    NextIndex is Index + 1,
    list_players_with_numbers(T, NextIndex).
