:- dynamic user_game_data/8.

user_game_data("PEDRO", 10, true, 0, 0, 0, 0, false).
user_game_data("Djan", 6, true, 0, 0, 1, 0, false).
user_game_data("Matheus", 1, false, 0, 0, 0, 0, false).

add_player(Name, Role) :-
    assertz(user_game_data(Name, Role, true, 0, 0, 0, 0, false)).

delete_player(Name) :-
    retract(user_game_data(Name, _, _, _, _, _, _, _)).

get_role(Name, Role) :-
    user_game_data(Name, Role, _, _, _, _, _, _).

get_player_by_role(Role, Name) :-
    user_game_data(Name, Role, _, _, _, _, _, _).

is_player_alive(Name, Alive) :-
    user_game_data(Name, _, Alive, _, _, _, _, _).

is_role_alive(Role, Alive) :-
    user_game_data(_, Role, Alive, _, _, _, _, _).

get_alive_players([], []).
get_alive_players([Player|Rest], AlivePlayers) :-
    user_game_data(Player, _, true, _, _, _, _, _), 
    get_alive_players(Rest, RemainingAlive),
    AlivePlayers = [Player|RemainingAlive].
get_alive_players([_|Rest], AlivePlayers) :-
    get_alive_players(Rest, AlivePlayers).

is_good(Name, Result) :-
    user_game_data(Name, Value, _, _, _, _, _, _),
    (Value > 6 -> Result = true ; Result = false).

is_paralised(Name, Result) :-
    user_game_data(Name, _, _, _, _, Value, _, _),
    (Value > 0 -> Result = false ; Result = true).

is_silinced(Name, Result) :-
    user_game_data(Name, _, _, _, _, _, Value, _),
    (Value > 0 -> Result = false ; Result = true).

count_good_players([], 0).

count_good_players([Player|Players], Count) :-
    get_role(Player, Value),
    count_good_players(Players, RemainingCount),
    Value > 6,
    Count is RemainingCount + 1.

count_good_players([_|Players], Count) :-
    count_good_players(Players, Count).

is_allowed(Name) :-
    user_game_data(Name, _, true, _, _, LastNum1, LastNum2, _),
    LastNum1 =:= 0,
    LastNum2 =:= 0.
