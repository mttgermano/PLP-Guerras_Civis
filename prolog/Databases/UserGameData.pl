:- use_module(library(random)).
:- dynamic user_game_data/8.
:- include('PlayersKnowledge.pl').
:- include('Players.pl').

% Test
% user_game_data("PEDRO", 10, true, 0, 0, 0, 0, false).
% user_game_data("Djan", 6, true, 0, 0, 1, 0, false).
% user_game_data("Matheus", 1, false, 0, 0, 0, 0, false).
user_game_data("Pedro", 1, false, 0, 0, 0, 0, false).
user_game_data("Djan", 2, true, 0, 0, 0, 0, true). 
user_game_data("Matheus", 3, true, 0, 0, 0, 0, true).
user_game_data("Pedro1", 4, false, 0, 0, 0, 0, false).
user_game_data("Djan1", 5, true, 0, 0, 0, 0, true). 
user_game_data("Matheus1", 6, true, 0, 0, 0, 0, true).
user_game_data("Pedro2", 7, false, 0, 0, 0, 0, false).
user_game_data("Djan2", 8, true, 0, 0, 0, 0, true). 
user_game_data("Matheus2", 9, true, 0, 0, 0, 0, true).
user_game_data("Pedro3", 10, false, 0, 0, 0, 0, false).
user_game_data("Djan3", 11, true, 0, 0, 0, 0, true). 
user_game_data("Matheus3", 12, true, 0, 0, 0, 0, true).

% User Game Data Actions ----------------------------------------
add_user_game_data(Name) :-
    assertz(user_game_data(Name, -1, true, 0, 0, 0, 0, false)).

delete_user_game_data(Name) :-
    retract(user_game_data(Name, _, _, _, _, _, _, _)).

delete_user_game_data_in_room(Room) :-
    get_all_in_room(Room, Players),
    delete_user_game_data_for_players(Players).

delete_user_game_data_for_players([]).
delete_user_game_data_for_players([Player|Rest]) :-
    delete_player(Player),
    delete_user_game_data_for_players(Rest).

assign_roles(Room) :-
    get_all_in_room(Room, Players),
    numlist(1, 12, AllRoles),
    random_permutation(AllRoles, RandomizedRoles),
    assign_roles_to_players(Players, RandomizedRoles).

assign_roles_to_players([], _, _).
assign_roles_to_players([Player|Rest], [Role|RemainingRoles]) :-
    assertz(user_game_data(Player, Role, true, 0, 0, 0, 0, false)),
    assign_roles_to_players(Rest, RemainingRoles).

reset_values(Name) :-
    retract(user_game_data(Name, Role, Status, _, _, _, _, isDeadByCursedWord)),
    assertz(user_game_data(Name, Role, Status, 0, 0, 0, 0, isDeadByCursedWord)).

% User Game Data Utils ------------------------------------------
get_role(Name, Role) :-
    user_game_data(Name, Role, _, _, _, _, _, _).

is_player_alive(Name, Alive) :-
    user_game_data(Name, _, Alive, _, _, _, _, _).

is_role_alive(Role, Names) :-
    member(Name, Names),
    user_game_data(Name, Role, true, _, _, _, _, _).

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

% Player Actions ------------------------------------------------
vote(PlayerName) :-
    retract(user_game_data(PlayerName, X, Y, A, B, C, D, E)),
    NewB is B + 1,
    assertz(user_game_data(PlayerName, X, Y, A, NewB, C, D, E)).

paralise(PlayerName) :-
    retract(user_game_data(PlayerName, X, Y, A, B, C, D, E)),
    NewC is C + 1,
    assertz(user_game_data(PlayerName, X, Y, A, B, NewC, D, E)).

silence(PlayerName) :-
    retract(user_game_data(PlayerName, X, Y, A, B, C, D, E)),
    NewD is D + 1,
    assertz(user_game_data(PlayerName, X, Y, A, B, C, NewD, E)).

kill_vote(PlayerName) :-
    retract(user_game_data(PlayerName, X, Y, A, B, C, D, E)),
    NewA is A + 1,
    assertz(user_game_data(PlayerName, X, Y, NewA, B, C, D, E)).

save_vote(PlayerName) :-
    retract(user_game_data(PlayerName, X, Y, A, B, C, D, E)),
    NewA is A - 1,
    assertz(user_game_data(PlayerName, X, Y, NewA, B, C, D, E)).

get_players_alive_role(Person, Players, Alive, Role) :-
    get_player_room(Person, Room),
    get_all_in_room(Room, Players),
    get_user_game_data(Person, Players, Alive, Role).

get_user_game_data(_, [], [], []).
get_user_game_data(Person, [Player|Rest], [Alive|RestAlive], [Role|RestRole]) :-
    (knows(Person, Player) ->
        get_role(Player, Comp),
        Role = Comp
    ;
        Role = -1
    ),
    is_player_alive(Player, Temp),
    Alive = Temp,
    get_user_game_data(Person, Rest, RestAlive, RestRole).