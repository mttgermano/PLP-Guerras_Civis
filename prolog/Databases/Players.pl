:- dynamic player/4.

:- discontiguous add_player/3.
:- discontiguous delete_player/1.
:- discontiguous change_player_room/2.
:- discontiguous player_login/2.
:- discontiguous get_player_room/2.
:- discontiguous player_exists/1.
:- discontiguous get_all_in_room/2.

% Test 
player("Pedro",     teste, room123, true).
player("Djan",      teste, room123, true). 
player("Matheus",   teste, room123, false).
player("Pedro1",    teste, room123, true).
player("Djan1",     teste, room123, true). 
player("Matheus1",  teste, room123, false).
player("Pedro2",    teste, room123, true).
player("Djan2",     teste, room123, true). 
player("Matheus2",  teste, room123, false).
player("Pedro3",    teste, room123, true).
player("Djan3",     teste, room123, true). 
player("Matheus3",  teste, room123, false).

% Player Actions ------------------------------------------------
add_player(Name, Password, Status) :-
    \+ player(Name, _, _, _),
    assertz(player(Name, Password, _, Status)).

delete_player(Name) :-
    retract(player(Name, _, _, _)),
    write('Player '), write(Name), write(' has been deleted.'), nl.

change_player_room(PlayerName,NewRoom) :-
    retract(player(PlayerName, Password, _, Status)),
    assertz(player(PlayerName, Password, NewRoom, Status)).

player_login(Name, Password) :-
    player(Name, Password, _, _).


% Player Utils --------------------------------------------------
get_player_room(Name, Room) :-
    player(Name, _, Room, _).

player_exists(Name) :-
    player(Name, _, _, _).

get_all_in_room(RoomName, Players) :-
    findall(Player, player(Player, _, RoomName, _), Players).

get_players_in_room(Room, Players) :-
    findall(X, player(X, _, Room, true), Players).

get_bots_in_room(Room, Bots) :-
    findall(X, player(X, _, Room, false), Bots).
