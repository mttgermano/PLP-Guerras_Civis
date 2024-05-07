:- include('PlayersKnowledge.pl').
:- include('Players.pl').

:- use_module(library(random)).
:- dynamic user_game_data/8.

% Test
% user_game_data("Pedro", 1, true, 0, 0, 0, 0, false).
% user_game_data("Djan", 2, true, 0, 0, 0, 0, false).
% user_game_data("Matheus", 3, true, 0, 0, 0, 0, false).
% user_game_data(Player, Role, Status, KillVote, Vote, Paralize, Silence, IsDeadByCursedWord),

user_game_data("Pedro", 1, false, 0, 0, 0, 0, false).
% user_game_data("Djan", 2, faflse, 0, 0, 0, 0, false).
% user_game_data("Matheus", 3, true, 0, 0, 0, 0, false).

% user_game_data("Pedro1", 4, false, 0, 0, 0, 0, false).
% user_game_data("Djan1", 5, false, 0, 0, 0, 0, false).
% user_game_data("Matheus1", 6, true, 0, 0, 0, 0, false).

% user_game_data("Pedro2", 7, true, 0, 0, 0, 0, false).
% user_game_data("Djan2", 8, false, 0, 0, 0, 0, false).
% user_game_data("Matheus2", 9, true, 0, 0, 0, 0, false).

% user_game_data("Pedro3", 10, false, 0, 0, 0, 0, false).
% user_game_data("Djan3", 11, false, 0, 0, 0, 0, false).
% user_game_data("Matheus3", 12, true, 0, 0, 0, 0, false).


% User Game Data Actions ----------------------------------------
add_user_game_data(Name) :-
    assertz(user_game_data(Name, -1, true, 0, 0, 0, 0, false)).

add_user_game_data(Name, Role) :-
    assertz(user_game_data(Name, Role, true, 0, 0, 0, 0, false)).

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
    assign_roles_to_players(Players, RandomizedRoles),
    maplista(start_knowledge, Players, RandomizedRoles).




maplista(_, [], []).
maplista(Pred, [X|Xs], [Y|Ys]) :-
    call(Pred, X, Y),
    maplista(Pred, Xs, Ys).
    
assign_roles_to_players([], _).
assign_roles_to_players([Player|Rest], [Role|RemainingRoles]) :-
    assertz(user_game_data(Player, Role, true, 0, 0, 0, 0, false)),
    assign_roles_to_players(Rest, RemainingRoles).

reset_values(Name) :-
    retract(user_game_data(Name, Role, Status, _, _, _, _, isDeadByCursedWord)),
    assertz(user_game_data(Name, Role, Status, 0, 0, 0, 0, isDeadByCursedWord)).

get_players_alive_role(Person, Players, Alive, Role) :-
    get_player_room(Person, Room),
    get_all_in_room(Room, Players),
    get_user_game_data(Person, Players, Alive, Role).

get_user_game_data(_, [], [], []).
get_user_game_data(Person, [Player|Rest], [Alive|RestAlive], [Role|RestRole]) :-
    (knows(Person, Player) 
    -> get_role(Player, Comp), Role = Comp
    ; Role = -1),
    is_player_alive(Player, Temp),
    Alive = Temp,
    get_user_game_data(Person, Rest, RestAlive, RestRole).

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
    user_game_data(Name, _, true, _, _, 0, 0, _).

% Player Actions ------------------------------------------------
vote(PlayerName) :-
    retract(user_game_data(PlayerName, X, Y, A, B, C, D, E)),
    NewB is B + 1,
    assertz(user_game_data(PlayerName, X, Y, A, NewB, C, D, E)).

paralise(PlayerName) :-
    retract(user_game_data(PlayerName, X, Y, A, B, C, D, E)),
    NewC is C + 1,
    assertz(user_game_data(PlayerName, X, Y, A, B, NewC, D, E)).

silence_player(PlayerName) :-
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

% KNowledge
% start_knowledge(Rname, Player, Role)

% Assassino
start_knowledge(Cpname, 1) :- 
    get_role(Player1, 2),
    get_role(Player2, 5),
    add_knowledge(Cpname, Cpname),
    add_knowledge(Cpname, Player1),
    add_knowledge(Cpname, Player2).

% Aprendiz
start_knowledge(Cpname, 2) :- 
    get_role(Player1, 1),
    get_role(Player2, 5),
    add_knowledge(Cpname, Cpname),
    add_knowledge(Cpname, Player1),
    add_knowledge(Cpname, Player2).

% Silenciador
start_knowledge(Cpname, 5) :- 
    get_role(Player1, 1),
    get_role(Player2, 2),
    add_knowledge(Cpname, Cpname),
    add_knowledge(Cpname, Player1),
    add_knowledge(Cpname, Player2).


% policial
start_knowledge(Cpname, 9) :- 
    get_role(Player1, 8),
    add_knowledge(Cpname, Cpname),
    add_knowledge(Cpname, Player1).


% juiz
start_knowledge(Cpname, 8) :- 
    get_role(Player1, 10),
    add_knowledge(Cpname, Cpname),
    add_knowledge(Cpname, Player1).

% Base case 
start_knowledge(Cpname, _) :-
    add_knowledge(Cpname, Cpname).
